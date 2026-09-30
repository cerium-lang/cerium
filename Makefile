.POSIX:
.SUFFIXES: .c .o

# xyz, stage 0. C89, no host framework; src/vec.h is the container layer.
# qbe builds from the submodule; the system cc links whatever qbe emits.

CC      = cc
CFLAGS  = -std=c89 -pedantic -Wall -Wextra -g

SRC     = $(wildcard src/*.c)
OBJ     = $(SRC:.c=.o)
BIN     = xyz

QBE_BIN = qbe/qbe

all: $(BIN)

$(BIN): $(OBJ)
	$(CC) $(OBJ) -o $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJ): $(wildcard src/*.h)

# the prelude's own source, embedded: a table of string literals
# (src/prelude_text.h) joined in the arena, the lexer reading the
# joined text from memory -- std/meta.xyz is parsed like any other
# source, it just never touches the disk
src/prelude.o: src/prelude_text.h
src/prelude_text.h: std/meta.xyz tools/embed.sh
	sh tools/embed.sh std/meta.xyz > $@

$(QBE_BIN):
	@cd qbe >/dev/null 2>&1 || { \
	    echo "qbe/ is empty -- run: git submodule update --init"; exit 1; }
	$(MAKE) -C qbe

test: $(BIN)
	sh tools/run-tests.sh

fmt:
	clang-format -i src/*.c src/*.h

fmt-check:
	clang-format --dry-run --Werror src/*.c src/*.h

hooks:
	git config core.hooksPath .githooks

clean:
	rm -f $(OBJ) $(BIN) src/prelude_text.h

.PHONY: all test fmt fmt-check hooks clean
