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

$(OBJ): src/vec.h src/lex.h src/ast.h src/die.h src/parse.h src/check.h src/sym.h src/type.h

$(QBE_BIN):
	@cd qbe >/dev/null 2>&1 || { \
	    echo "qbe/ is empty -- run: git submodule update --init"; exit 1; }
	$(MAKE) -C qbe

test: $(BIN)
	sh tools/run_tests.sh

fmt:
	clang-format -i src/*.c src/*.h

fmt-check:
	clang-format --dry-run --Werror src/*.c src/*.h

hooks:
	git config core.hooksPath .githooks

clean:
	rm -f $(OBJ) $(BIN)

.PHONY: all test fmt fmt-check hooks clean
