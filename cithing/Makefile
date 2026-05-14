CC = gcc
CFLAGS = -O3 -Wall -Wextra -I. -Ijit -g -fPIC
LDFLAGS = -rdynamic

CORE_SRCS = interpreter.c
JIT_CORE_SRCS = jit/core/jit_manager.c \
                jit/core/jit_frontend.c \
                jit/core/ir_builder.c \
                jit/core/reg_alloc.c
JIT_ARCH_SRCS = jit/arch/x86_64/backend.c

SRCS = $(CORE_SRCS) $(JIT_CORE_SRCS) $(JIT_ARCH_SRCS)
OBJS = $(SRCS:.c=.o)
TARGET = libcithing.a

.PHONY: all clean haskell debug

all: $(TARGET) haskell

debug: CFLAGS += -DDEBUG
debug: all

$(TARGET): $(OBJS)
	ar rcs $@ $(OBJS)

haskell: $(TARGET)
	ghc -O3 -threaded Main.hs $(TARGET) -o cithing

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGET) cithing Main.o Main.hi Read.o Read.hi
