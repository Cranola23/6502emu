#include "bus.h"
#include <stdint.h>
#include <string.h>

void BusInit(Bus *bus, Memory *memory){
    bus->memory = memory;
    memset(bus->memory->ram,0,sizeof(bus->memory->ram));
}

uint8_t BusRead(Bus *bus, uint16_t address){
    if(address>=0x0000 && address<=0xFFFF) {
        return bus->memory->ram[address];
    }
}

void BusWrite(Bus *bus,uint16_t address, uint8_t data){
    if(address>=0x0000 && address<=0xFFFF) {
        bus->memory->ram[address] = data;
    }
}
