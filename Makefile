
#Compiler & flags
CC       := gcc
CFLAGS   := -std=c99 -Wall -Wextra -O2

#Linker flags: link raylib and Windows system libs
LDFLAGS  := -lraylib -lopengl32 -lgdi32 -lwinmm

#Source and object files
SRC      := src/bus.c src/6502core.c src/cpuDemo.c src/main.c
OBJ      := $(SRC:.c=.o)

#Output exe
TARGET   := run.exe

# Default build rule
all: $(TARGET)

$(TARGET): $(OBJ)
	$(CC) $(OBJ) -o $@ $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

#clean build artifacts
clean:
	rm -f $(OBJ) $(TARGET)
