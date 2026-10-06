/* vec.h -- C89 泛型动态数组，单头文件
 *
 * 用法：在**一个** .c 里
 *         #define VEC_IMPLEMENTATION
 *         #include "vec.h"
 *       其余 .c 直接 #include "vec.h"。
 *
 *   int *v = vnew(int, 0);      -- 懒分配，cap=0
 *   int x = 42;
 *   vappend(&v, &x);            -- 注意传 &v：realloc 可能换地址
 *   v[0];  vlen(v);  vcap(v);
 *   vlast(v);  vpop(v);  vclear(v);
 *   vresize(&v, n);             -- len 改成 n，多出来的槽清零
 *   vinsert(&v, i, &x);         -- 在 i 处插入，后面整体右移
 *   vremove(v, i);              -- 删掉 i，后面整体左移（保序，O(n)）
 *   vswap_remove(v, i);         -- 用最后一个顶上（不保序，O(1)）
 *   int *c = vclone(int, v);    -- 深拷贝，容量也一起复制
 *   vfit(&v);                   -- cap 缩到 len（len==0 时整体释放并置 NULL）
 *   vfree(v);                   -- 同时置 NULL
 *   vfree_each(v, fn);          -- 元素自己持有资源时，先 fn(&v[i]) 再 free
 *
 * 相对原 gist 修掉的点：
 *   1. vlen 宏体写死变量名 v，换个名字就编不过
 *   2. 数据区起点没按元素对齐要求调整 -> 对齐要求 > 8 的元素全错位：arm64 上 16 对齐
 *      的原子操作直接 SIGBUS（实测信号 10），x86 上 movaps #GP。
 *      （对齐 <= 8 的原版也活得下去：header 24 字节 + malloc 至少 8 对齐，data 恒 8 对齐。
 *        所以「塞个引用计数节点就崩」是错的，align 4 的 _Atomic int 没事。）
 *      靠运行期 pad 兜底，不靠把 header 补齐：calloc/malloc 只契约
 *      _Alignof(max_align_t)（Darwin arm64 上才 8），header 补齐到 16 也只是碰巧对。
 *   3. void** 类型双关违反 strict aliasing -> 改成在真实类型上回写
 *   4. OOM 静默丢元素 -> voom()，默认 abort，可自己实现替换
 *   5. cap 翻倍溢出 -> 死循环 / 堆分配偏小越界写
 *   6. vfree 不置空
 *   7. realloc 之后 header 可能搬家（pad 变了），元数据必须整体重写
 *
 *   8. 自引用 append（vappend(&v, &v[0])）在 grow 时 e 指向刚被 free 的旧块 -> UAF
 *   9. vlen/vcap 对 NULL 是 UB（vhdr(NULL) 就是 NULL-40）
 *  10. 只有增容没有缩容 -> 补 vfit
 *  11. 元素类型不匹配静默通过（int vec 塞 double）-> vappend 加编译期类型检查
 *
 * 已知取舍：
 *   - 除 vlen/vcap/vfit/vfree 外，其余操作不接受 NULL，传了直接 voom（不静默丢数据）。
 *     vfit/vfree 对 NULL 是安全的 no-op，跟 free(NULL) 一个道理
 *   - C89 下没法按值 push（vappend(&v, i*i) 需要 &(i*i)，不成立）
 *   - vlast/vpop 在 len==0 时是 UB，跟 v[0] 一样，调用方自己保证
 *   - 2x 增长不复用释放块（Rust 也用 2x；要省内存可改 1.5x）
 *   - header 40 字节（stb_ds 才 16），多出的 24 字节买的是超对齐支持
 */

#ifndef VEC_H
#define VEC_H

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

typedef size_t usize;

typedef struct Vh Vh;
struct Vh
{
  usize len;
  usize cap;
  usize esiz; /* 元素大小，grow 靠它跟类型解耦 */
  usize alig; /* 元素对齐要求 */
  usize pad;  /* data - (base + VEC_HDRSZ)，malloc 对齐不够时非 0 */
};

/* 编译器保证 sizeof(struct) 是自身对齐的整数倍，
 * 所以 data - VEC_HDRSZ 当作 Vh* 用一定合法 */
#define VEC_HDRSZ ((usize) sizeof(Vh))

/* C89 版 alignof：sizeof(struct{char;T;}) - sizeof(T) == alignof(T)
 * 别用 offsetof 里现定义结构体 —— clang 判 C23 扩展
 * 单行是刻意的：clang-format 会把宏里的匿名 struct 拆成多行，关掉 */
/* clang-format off */
#define valignof(t) ((usize)(sizeof(struct { char c; t d_; }) - sizeof(t)))
/* clang-format on */

#define vhdr(p) ((Vh *) ((char *) (p) - VEC_HDRSZ))

#define vnew(t, n)      ((t *) valloc_(sizeof(t), valignof(t), (usize) (n)))
#define vfree(p)        ((p) = vfree_((p)))
#define vreserve(pp, n) (*(pp) = vreserve_(*(pp), (usize) (n)))
#define vfit(pp)        (*(pp) = vfit_(*(pp)))

/* 编译期元素类型检查，零运行期开销：sizeof 里的表达式不求值。
 *   - 赋值表达式挡「类型不兼容」：A vec 塞 B
 *   - char[负]     挡「大小不等」  ：int vec 塞 double  */
#define VEC_TYPCHK(pp, e)                                                                          \
  ((void) sizeof((*(pp))[0] = *(e)),                                                               \
   (void) sizeof(char[1 - 2 * (sizeof((*(pp))[0]) != sizeof(*(e)))]))

#define vappend(pp, e)     (*(pp) = vappend_(*(pp), (VEC_TYPCHK(pp, e), (e))))
#define vresize(pp, n)     (*(pp) = vresize_(*(pp), (usize) (n)))
#define vinsert(pp, i, e)  (*(pp) = vinsert_(*(pp), (usize) (i), (VEC_TYPCHK(pp, e), (e))))
#define vremove(p, i)      vremove_((p), (usize) (i))
#define vswap_remove(p, i) vswap_remove_((p), (usize) (i))
#define vclone(t, p)       ((t *) vclone_((p)))
#define vfree_each(p, fn)  ((p) = vfree_each_((p), (fn)))

/* NULL 安全：没 vnew 过的变量也能安全查询 */
#define vlen(p)   ((p) ? vhdr(p)->len : 0)
#define vcap(p)   ((p) ? vhdr(p)->cap : 0)
#define vlast(p)  ((p)[vhdr(p)->len - 1])
#define vpop(p)   ((p)[--vhdr(p)->len])
#define vclear(p) ((void) (vhdr(p)->len = 0))

void  voom(const char *msg) __attribute__((__noreturn__));
void *valloc_(usize esiz, usize alig, usize n);
void *vfree_(void *p);
void *vreserve_(void *p, usize n);
void *vfit_(void *p);
void *vappend_(void *p, const void *e);
void *vresize_(void *p, usize n);
void *vinsert_(void *p, usize i, const void *e);
void  vremove_(void *p, usize i);
void  vswap_remove_(void *p, usize i);
void *vclone_(const void *p);
void *vfree_each_(void *p, void (*fn)(void *));

#ifdef VEC_IMPLEMENTATION

#include <stdio.h>

void
voom(const char *msg)
{
  fprintf(stderr, "vec: %s\n", msg);
  abort();
}

void *
valloc_(usize esiz, usize alig, usize n)
{
  char *base;
  char *data;
  Vh   *h;
  usize pad;

  if (esiz == 0 || alig == 0)
    return NULL;
  if (n > ((((usize) -1) - VEC_HDRSZ - alig) / esiz))
    return NULL;

  /* calloc(1, total) 而不是 calloc(nmemb, size)：后者两参数相乘的溢出行为
   * 是实现定义的。顺带整块清零，数据区拿到手就是零初始化的。
   * 注意只有 valloc_ 能换 calloc —— vreserve_ 要留旧数据，只能是 realloc */
  base = (char *) calloc(1, VEC_HDRSZ + alig - 1 + n * esiz);
  if (!base)
    return NULL;

  data = base + VEC_HDRSZ;
  pad = (alig - ((usize) data % alig)) % alig;
  data += pad;

  h = (Vh *) (data - VEC_HDRSZ);
  h->len = 0;
  h->cap = n;
  h->esiz = esiz;
  h->alig = alig;
  h->pad = pad;

  return data;
}

void *
vfree_(void *p)
{
  Vh *h;

  if (!p)
    return NULL;

  h = vhdr(p);
  free((char *) h - h->pad);

  return NULL;
}

void *
vreserve_(void *p, usize n)
{
  Vh   *h;
  char *base;
  char *nb;
  char *data;
  usize esiz, alig, len, oldpad, cap, pad, room;

  if (!p)
    voom("null vector");

  h = vhdr(p);
  if (h->cap >= n)
    return p;

  /* 元数据必须先抄进局部变量：realloc 之后 h 就悬空了 */
  esiz = h->esiz;
  alig = h->alig;
  len = h->len;
  oldpad = h->pad;
  cap = h->cap;

  room = (((usize) -1) - VEC_HDRSZ - alig) / esiz;
  if (n > room)
    voom("capacity overflow");

  if (cap == 0)
    cap = 8;
  while (cap < n) {
    if (cap > room / 2) { /* 再翻倍要溢出了，直接顶到 n */
      cap = n;
      break;
    }
    cap *= 2;
  }

  base = (char *) h - oldpad;
  nb = (char *) realloc(base, VEC_HDRSZ + alig - 1 + cap * esiz);
  if (!nb)
    voom("out of memory");

  data = nb + VEC_HDRSZ;
  pad = (alig - ((usize) data % alig)) % alig;
  data += pad;

  /* realloc 把旧内容搬到了 nb 开头，源地址在新块内，不是野指针 */
  if (pad != oldpad)
    memmove(data, nb + VEC_HDRSZ + oldpad, len * esiz);

  /* header 可能也搬家了，整体重写，别指望它被 memcpy 过来 */
  h = (Vh *) (data - VEC_HDRSZ);
  h->len = len;
  h->cap = cap;
  h->esiz = esiz;
  h->alig = alig;
  h->pad = pad;

  return data;
}

void *
vfit_(void *p)
{
  Vh   *h;
  char *base;
  char *nb;
  char *data;
  usize esiz, alig, len, oldpad, pad;

  if (!p)
    return p;

  h = vhdr(p);
  esiz = h->esiz;
  alig = h->alig;
  len = h->len;
  oldpad = h->pad;

  if (len == 0) { /* 空的直接整体释放，跟 vfree 一样 */
    free((char *) h - oldpad);
    return NULL;
  }

  if (len == h->cap)
    return p;

  base = (char *) h - oldpad;
  nb = (char *) realloc(base, VEC_HDRSZ + alig - 1 + len * esiz);
  if (!nb)
    voom("out of memory");

  /* 缩小也可能搬家，而且新块的 pad 未必跟旧的一致 */
  data = nb + VEC_HDRSZ;
  pad = (alig - ((usize) data % alig)) % alig;
  data += pad;

  if (pad != oldpad)
    memmove(data, nb + VEC_HDRSZ + oldpad, len * esiz);

  h = (Vh *) (data - VEC_HDRSZ);
  h->len = len;
  h->cap = len;
  h->esiz = esiz;
  h->alig = alig;
  h->pad = pad;

  return data;
}

void *
vappend_(void *p, const void *e)
{
  Vh         *h;
  const char *src = (const char *) e;
  usize       off;
  usize       d;

  if (!p || !e)
    voom("null pointer");

  h = vhdr(p);

  /* 自引用 append：vappend(&v, &v[0])。e 可能落在 vec 内部，
   * 而下面的 vreserve_ 会 realloc，旧块一 free，e 就悬空了。
   * 先记下偏移量，realloc 完再按新基址重算。
   * 用地址差判断，别直接比指针 —— 跨对象的指针关系在标准里是 UB */
  d = (usize) src - (usize) p;
  off = (d < h->len * h->esiz) ? d : (usize) -1;

  if (h->len == h->cap)
    p = vreserve_(p, h->len + 1);

  h = vhdr(p); /* vreserve_ 可能换了地址，重取 */
  if (off != (usize) -1)
    src = (const char *) p + off;

  memcpy((char *) p + h->len * h->esiz, src, h->esiz);
  h->len++;

  return p;
}

void *
vresize_(void *p, usize n)
{
  Vh   *h;
  usize old;

  if (!p)
    voom("null vector");

  h = vhdr(p);
  if (n > h->cap)
    p = vreserve_(p, n);

  h = vhdr(p);
  old = h->len;
  if (n > old) /* 多出来的槽清零，跟 valloc_ 的 calloc 语义对齐 */
    memset((char *) p + old * h->esiz, 0, (n - old) * h->esiz);
  h->len = n;

  return p;
}

void *
vinsert_(void *p, usize i, const void *e)
{
  Vh         *h;
  const char *src = (const char *) e;
  usize       d;
  usize       j;

  if (!p || !e)
    voom("null pointer");

  h = vhdr(p);
  if (i > h->len)
    voom("insert out of range");

  /* 自引用：e 落在 vec 内部。记下它是第几个元素 */
  d = (usize) src - (usize) p;
  if (d < h->len * h->esiz) {
    if (d % h->esiz != 0)
      voom("insert: self-reference must point at an element, not its interior");
    j = d / h->esiz;
  } else {
    j = (usize) -1;
  }

  if (h->len == h->cap)
    p = vreserve_(p, h->len + 1);

  h = vhdr(p); /* vreserve_ 可能换了地址 */

  if (i < h->len)
    memmove((char *) p + (i + 1) * h->esiz, (char *) p + i * h->esiz, (h->len - i) * h->esiz);

  if (j != (usize) -1) {
    /* 让位的时候索引 >= i 的元素整体右移了一位，源也跟着走 */
    if (j >= i)
      j++;
    src = (const char *) p + j * h->esiz;
  }

  memcpy((char *) p + i * h->esiz, src, h->esiz);
  h->len++;

  return p;
}

void
vremove_(void *p, usize i)
{
  Vh *h;

  if (!p)
    voom("null vector");

  h = vhdr(p);
  if (i >= h->len)
    voom("remove out of range");

  if (i + 1 < h->len)
    memmove((char *) p + i * h->esiz, (char *) p + (i + 1) * h->esiz, (h->len - i - 1) * h->esiz);
  h->len--;
}

void
vswap_remove_(void *p, usize i)
{
  Vh *h;

  if (!p)
    voom("null vector");

  h = vhdr(p);
  if (i >= h->len)
    voom("swap_remove out of range");

  /* 拿最后一个元素顶上：O(1)，代价是不保序 */
  if (i + 1 < h->len)
    memcpy((char *) p + i * h->esiz, (char *) p + (h->len - 1) * h->esiz, h->esiz);
  h->len--;
}

void *
vclone_(const void *p)
{
  Vh   *h;
  Vh   *nh;
  char *nd;
  usize esiz, alig, len, cap;

  if (!p)
    return NULL;

  h = vhdr(p);
  esiz = h->esiz;
  alig = h->alig;
  len = h->len;
  cap = h->cap;

  nd = (char *) valloc_(esiz, alig, cap);
  if (!nd)
    voom("out of memory");

  if (len > 0)
    memcpy(nd, p, len * esiz);

  nh = vhdr(nd);
  nh->len = len;

  return nd;
}

void *
vfree_each_(void *p, void (*fn)(void *))
{
  Vh   *h;
  usize i;

  if (!p)
    return NULL;

  h = vhdr(p);
  if (fn) {
    for (i = 0; i < h->len; i++)
      fn((char *) p + i * h->esiz);
  }

  free((char *) h - h->pad);

  return NULL;
}

#endif /* VEC_IMPLEMENTATION */

#endif /* VEC_H */
