#!/bin/sh
# run-tests.sh -- the golden tests.
#
# tests/lex, tests/parse and tests/check split by pass: every lex/ok
#/*.ce must dump exactly its .golden as a token stream (regenerate
# one with: ./cerium -t tests/lex/ok/NN.ce > tests/lex/ok/NN.golden --
# after checking the dump by hand), every parse/ok/*.ce as an AST
# (ditto with -a), every check/ok/*.ce as resolved declarations
# (ditto with -T), and every err/*.ce under any of them must be
# rejected with a diagnostic on stderr and a nonzero exit.
#
# A directory in place of the .ce is a project (12-projects.md):
# every .ce under it, each in the namespace its path spells. The
# project tests live the same way -- check/ok/NN-name/ diffs against
# the NN-name.golden beside it (one (file ...) block per file, -T
# only), check/err/NN-name/ must reject, and tests/run/NN-name/
# compiles to a binary whose exit code the NN-name.expect names. -t
# and -a read one file; a project tests through -T, -s and -c.
#
# tests/run is codegen's: every *.ce compiles to a binary whose
# exit code the matching .expect names; an optional .stdout holds
# the bytes it must print (#[extern(C)] write is how the language
# prints until M3c). A .release beside the source compiles it with
# -r -- the runtime checks out, the wrap a release owns
# (01-types.md) -- and an optional .stderr holds the bytes it must
# write there, a runtime check's panic among them: abort's own exit
# is 134. It needs qbe/qbe built -- a checkout without it skips
# the section rather than failing.
#
# tests/test is the test artifact's (13-testing.md): every entry a
# directory project, -x compiling it to the runner over its #[test]
# fns. The .expect beside the directory names the exit -- 0 every
# one passed, 1 any one failed -- an optional .stdout the report
# lines, an optional .stderr the failures' own words, the panic's
# or the Err's. The report's line order is the table's, the
# compiler's own walk; the .stdout pins it the way a golden pins
# its dump.
set -u
cd "$(dirname "$0")/.."

fail=0
golden() { # $1: the directory, $2: the dump flag
  for f in "$1"/*.ce; do
    [ -e "$f" ] || continue
    g="${f%.ce}.golden"
    if [ ! -f "$g" ]; then
      echo "FAIL $f (no .golden)"
      fail=1
      continue
    fi
    if ./cerium "$2" "$f" 2>/dev/null | diff -u "$g" - >/dev/null; then
      echo "ok   $f"
    else
      echo "FAIL $f"
      ./cerium "$2" "$f" 2>/dev/null | diff -u "$g" - | sed 's/^/     /'
      fail=1
    fi
  done
  for d in "$1"/*/; do # a project: the .golden beside the directory
    [ -d "$d" ] || continue
    g="${d%/}.golden"
    if [ ! -f "$g" ]; then
      echo "FAIL $d (no .golden)"
      fail=1
      continue
    fi
    if ./cerium "$2" "$d" 2>/dev/null | diff -u "$g" - >/dev/null; then
      echo "ok   $d"
    else
      echo "FAIL $d"
      ./cerium "$2" "$d" 2>/dev/null | diff -u "$g" - | sed 's/^/     /'
      fail=1
    fi
  done
}

rejected() { # $1: the directory, $2: the dump flag (-a or -T)
  for f in "$1"/*.ce; do
    [ -e "$f" ] || continue
    if ./cerium "${2:--a}" "$f" >/dev/null 2>&1; then
      echo "FAIL $f (accepted; an error was expected)"
      fail=1
    else
      echo "ok   $f"
    fi
  done
  for d in "$1"/*/; do # a project: -T reads the whole thing
    [ -d "$d" ] || continue
    if ./cerium "${2:--T}" "$d" >/dev/null 2>&1; then
      echo "FAIL $d (accepted; an error was expected)"
      fail=1
    else
      echo "ok   $d"
    fi
  done
}

runthem() { # $1: the directory; an .expect of "!" wants rejection,
  # an optional .stdout holds the bytes the binary must print, an
  # optional .stderr the bytes it must write there, and a .release
  # beside the source compiles it with -r (01-types.md)
  tmp=$(mktemp -d)
  for f in "$1"/*.ce; do
    [ -e "$f" ] || continue
    g="${f%.ce}.expect"
    if [ ! -f "$g" ]; then
      echo "FAIL $f (no .expect)"
      fail=1
      continue
    fi
    exp=$(cat "$g")
    r=""
    [ -f "${f%.ce}.release" ] && r="-r"
    if [ "$exp" = "!" ]; then
      if ./cerium $r -c "$f" -o "$tmp/out" 2>/dev/null; then
        echo "FAIL $f (compiled; a rejection was expected)"
        fail=1
      else
        echo "ok   $f"
      fi
      continue
    fi
    if ! ./cerium $r -c "$f" -o "$tmp/out" 2>"$tmp/err"; then
      echo "FAIL $f (rejected: $(head -1 "$tmp/err"))"
      fail=1
      continue
    fi
    # the exec wrapper matters: dash applies a command's redirects
    # on itself before forking and undoes them only after the wait,
    # so its own signal report (an abort's "Aborted") would land in
    # the very file under cmp -- with the exec there is no parent
    # shell holding the redirects, and the report stays on ours
    sh -c 'exec "$0" >"$1" 2>"$2"' "$tmp/out" "$tmp/stdout" "$tmp/stderr"
    got=$?
    if [ "$got" != "$exp" ]; then
      echo "FAIL $f (exit $got, want $exp)"
      fail=1
      continue
    fi
    s="${f%.ce}.stdout"
    if [ -f "$s" ] && ! cmp -s "$s" "$tmp/stdout"; then
      echo "FAIL $f (stdout $(head -c 40 "$tmp/stdout" | tr '\n' ' ')...)"
      fail=1
      continue
    fi
    s="${f%.ce}.stderr"
    if [ -f "$s" ] && ! cmp -s "$s" "$tmp/stderr"; then
      echo "FAIL $f (stderr $(head -c 40 "$tmp/stderr" | tr '\n' ' ')...)"
      fail=1
      continue
    fi
    s="${f%.ce}.nosym" # patterns (grep regexps, one a line) the
    # binary must not carry: the artifact's own fns the library and
    # the executable hold away (13-testing.md)
    if [ -f "$s" ] && nm "$tmp/out" 2>/dev/null | grep -q -f "$s"; then
      echo "FAIL $f (a symbol the build does not carry: $(nm "$tmp/out" | grep -f "$s" | head -1))"
      fail=1
      continue
    fi
    echo "ok   $f"
  done
  for d in "$1"/*/; do # a project: one binary, the walk's every file
    [ -d "$d" ] || continue
    g="${d%/}.expect"
    if [ ! -f "$g" ]; then
      echo "FAIL $d (no .expect)"
      fail=1
      continue
    fi
    exp=$(cat "$g")
    r=""
    [ -f "${d%/}.release" ] && r="-r"
    if [ "$exp" = "!" ]; then
      if ./cerium $r -c "$d" -o "$tmp/out" 2>/dev/null; then
        echo "FAIL $d (compiled; a rejection was expected)"
        fail=1
      else
        echo "ok   $d"
      fi
      continue
    fi
    if ! ./cerium $r -c "$d" -o "$tmp/out" 2>"$tmp/err"; then
      echo "FAIL $d (rejected: $(head -1 "$tmp/err"))"
      fail=1
      continue
    fi
    # the exec wrapper matters: dash applies a command's redirects
    # on itself before forking and undoes them only after the wait,
    # so its own signal report (an abort's "Aborted") would land in
    # the very file under cmp -- with the exec there is no parent
    # shell holding the redirects, and the report stays on ours
    sh -c 'exec "$0" >"$1" 2>"$2"' "$tmp/out" "$tmp/stdout" "$tmp/stderr"
    got=$?
    if [ "$got" != "$exp" ]; then
      echo "FAIL $d (exit $got, want $exp)"
      fail=1
      continue
    fi
    s="${d%/}.stdout"
    if [ -f "$s" ] && ! cmp -s "$s" "$tmp/stdout"; then
      echo "FAIL $d (stdout $(head -c 40 "$tmp/stdout" | tr '\n' ' ')...)"
      fail=1
      continue
    fi
    s="${d%/}.stderr"
    if [ -f "$s" ] && ! cmp -s "$s" "$tmp/stderr"; then
      echo "FAIL $d (stderr $(head -c 40 "$tmp/stderr" | tr '\n' ' ')...)"
      fail=1
      continue
    fi
    echo "ok   $d"
  done
  rm -rf "$tmp"
}

testthem() { # $1: the directory; every entry a directory project,
  # the .expect its exit, an optional .stdout the report lines, an
  # optional .stderr the failures' own words (13-testing.md).
  #
  # std's rows the tree collects with the project's own (12-
  # projects.md, 13-testing.md): the golden reads the project's
  # alone, the std:: rows stripped and the sum retold over what is
  # left -- std's own pass and fail its own door says, the 00-std
  # project's .expect, and a std row failing fails every exit with
  # it, the whole tree one artifact
  tmp=$(mktemp -d)
  for d in "$1"/*/; do
    [ -d "$d" ] || continue
    g="${d%/}.expect"
    if [ ! -f "$g" ]; then
      echo "FAIL $d (no .expect)"
      fail=1
      continue
    fi
    exp=$(cat "$g")
    if ! ./cerium -x "$d" -o "$tmp/out" 2>"$tmp/err"; then
      echo "FAIL $d (rejected: $(head -1 "$tmp/err"))"
      fail=1
      continue
    fi
    sh -c 'exec "$0" >"$1" 2>"$2"' "$tmp/out" "$tmp/stdout" "$tmp/stderr"
    got=$?
    if [ "$got" != "$exp" ]; then
      echo "FAIL $d (exit $got, want $exp)"
      fail=1
      continue
    fi
    s="${d%/}.stdout"
    if [ -f "$s" ]; then
      awk '!/std::/ { if ($0 ~ /^ok/) p++; else if ($0 ~ /^FAIL/) f++; lines[++n] = $0 }
           END { for (i = 1; i < n; i++) print lines[i]
                 printf "%d passed, %d failed\n", p, f }' \
        "$tmp/stdout" >"$tmp/mine"
      if ! cmp -s "$s" "$tmp/mine"; then
        echo "FAIL $d (stdout $(head -c 40 "$tmp/mine" | tr '\n' ' ')...)"
        fail=1
        continue
      fi
    fi
    s="${d%/}.stderr"
    if [ -f "$s" ] && ! cmp -s "$s" "$tmp/stderr"; then
      echo "FAIL $d (stderr $(head -c 40 "$tmp/stderr" | tr '\n' ' ')...)"
      fail=1
      continue
    fi
    echo "ok   $d"
  done
  rm -rf "$tmp"
}

golden tests/lex/ok -t
golden tests/parse/ok -a
golden tests/check/ok -T
rejected tests/lex/err -t
rejected tests/parse/err -a
rejected tests/check/err -T
if [ -x qbe/qbe ]; then
  runthem tests/run
  testthem tests/test
else
  echo "skipped tests/run and tests/test -- build qbe first: make qbe/qbe"
fi

exit $fail
