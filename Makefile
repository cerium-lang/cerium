.POSIX:
.SUFFIXES: .c .o

# cerium, stage 0. C89, no host framework; src/vec.h is the container layer.
# qbe builds from the submodule; the system cc links whatever qbe emits.

CC      = cc
CFLAGS  = -std=c89 -pedantic -Wall -Wextra -g

SRC     = $(wildcard src/*.c)
OBJ     = $(SRC:.c=.o)
BIN     = cerium

QBE_BIN = qbe/qbe

all: $(BIN)

$(BIN): $(OBJ)
	$(CC) $(OBJ) -o $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJ): $(wildcard src/*.h)

# phony on purpose: qbe's own Makefile holds the real dependencies, so
# every ask goes in -- a touched source rebuilds, a stale ask costs a
# sub-second stat pass (#141)
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
	rm -f $(OBJ) $(BIN)

.PHONY: all test fmt fmt-check hooks clean $(QBE_BIN)
