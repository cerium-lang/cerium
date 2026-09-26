.POSIX:
.SUFFIXES: .c .o

# xyz-cc, stage 0. C89, no host framework; src/vec.h is the container layer.
# qbe builds from the submodule; the system cc links whatever qbe emits.

CC      = cc
CFLAGS  = -std=c89 -pedantic -Wall -Wextra -g

SRC     = $(wildcard src/*.c)
OBJ     = $(SRC:.c=.o)
BIN     = xyz-cc

QBE_BIN = qbe/qbe

all: $(BIN)

$(BIN): $(OBJ)
	$(CC) $(OBJ) -o $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJ): src/vec.h

$(QBE_BIN):
	@cd qbe >/dev/null 2>&1 || { \
	    echo "qbe/ is empty -- run: git submodule update --init"; exit 1; }
	$(MAKE) -C qbe

test: $(BIN) $(QBE_BIN)
	@echo "test: golden tests land with the lexer"

clean:
	rm -f $(OBJ) $(BIN)

.PHONY: all test clean
