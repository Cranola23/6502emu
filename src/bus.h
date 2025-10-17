#ifndef BUS_H
#define BUS_H

#include <stdint.h>

typedef struct { //Memory structure
    uint8_t ram[64*1024];
} Memory;

typedef struct { //Bus structure
    uint16_t bus;
    Memory *memory;
} Bus;


void BusInit(Bus *bus, Memory *memory);
uint8_t BusRead(Bus *bus, uint16_t address);
void BusWrite(Bus *bus, uint16_t address, uint8_t data);

#endif