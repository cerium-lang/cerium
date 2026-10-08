.POSIX:
.SUFFIXES: .c .o

# cerium, stage 0. C89, no host framework; src/vec.h is the container layer.
# The backend is qbe's, linked in: its objects build from the submodule
# (its own Makefile, its own C99), main.o left out -- src/qbe.c drives the
# passes instead -- and the system cc links whatever they emit.

CC      = cc
CFLAGS  = -std=c89 -pedantic -Wall -Wextra -g

SRC     = $(wildcard src/*.c)
OBJ     = $(SRC:.c=.o)
BIN     = cerium

# qbe's objects, its Makefile's own list with main.o out: the
# driver in src/qbe.c holds what its main.o did. All three
# targets' -- amd64, arm64, rv64 -- one binary every host links
# whole, the native one the config picks at run
QBECOMMON = util parse abi cfg mem ssa alias load copy fold gvn gcm \
            simpl ifopt live spill rega emit
QBEOBJ = $(QBECOMMON:%=qbe/%.o) \
         qbe/amd64/targ.o qbe/amd64/sysv.o qbe/amd64/isel.o \
         qbe/amd64/emit.o qbe/amd64/winabi.o \
         qbe/arm64/targ.o qbe/arm64/abi.o qbe/arm64/isel.o \
         qbe/arm64/emit.o \
         qbe/rv64/targ.o qbe/rv64/abi.o qbe/rv64/isel.o \
         qbe/rv64/emit.o

all: $(BIN)

$(BIN): $(OBJ) qbe/qbe
	$(CC) $(OBJ) $(QBEOBJ) -o $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

# the one cerium file in qbe's own dialect: C99, its headers'
# words (the die macro among them). The generic .c.o rule is
# C89's; this one stands ahead of it
src/qbe.o: src/qbe.c src/qbe.h qbe/all.h qbe/config.h
	$(CC) -std=c99 -Wall -Wextra -g -c src/qbe.c -o $@

$(OBJ): $(wildcard src/*.h)

# the backend's config -- Deftgt, the platform the uname says --
# is that build's own word, written by its Makefile, never checked
# in; the driver below compiles against it, so it stands ahead of
# everything else the backend builds
qbe/config.h:
	@cd qbe >/dev/null 2>&1 || { \
	    echo "qbe/ is empty -- run: git submodule update --init"; exit 1; }
	$(MAKE) -C qbe config.h

# the backend's objects, built by its own Makefile: a touch to
# its sources rebuilds them and the witness binary, and cerium
# relinks after. The config ahead of it keeps the two makes --
# the word's own, the build's whole -- out of each other's way
qbe/qbe: $(wildcard qbe/*.c qbe/*.h) $(wildcard qbe/*/*.c qbe/*/*.h) qbe/config.h
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

.PHONY: all test fmt fmt-check hooks clean
