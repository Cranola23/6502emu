#ifndef CPU_H
#define CPU_H

#include <stdint.h>
#include <stdbool.h>
#include "bus.h"

typedef struct {
    Bus *bus; //pointer struct to bus(to memory)
    uint16_t PC;
    uint8_t SP;
    uint8_t A;
    uint8_t X;
    uint8_t Y;
    uint8_t status;

    //helpers
    uint8_t opcode;
    uint8_t fetched;
    uint16_t address_abs;
    uint16_t address_rel;
    uint8_t cycles;
    uint16_t temp;
} cpu;

typedef enum {
    C = (1<<0), //carry bit
    Z = (1<<1), //zero
    I = (1<<2), //disable interrupt
    D = (1<<3), //decimal
    B = (1<<4), //break
    U = (1<<5), //unused
    V = (1<<6), //overflow
    N = (1<<7), //negative
} statusFlag;

uint8_t cpuGetFlag(cpu *self, statusFlag flag);
void cpuSetFlag(cpu *self, statusFlag flag, bool value);

void cpuInit(cpu *self);
void connectBus(cpu *self, Bus *bus);

uint8_t cpuRead(cpu *self, uint16_t address);
void cpuWrite(cpu *self, uint16_t address, uint8_t data);  

void cpuClock(cpu *self);

void cpuReset(cpu *self); //reset vector
void cpuIrq(cpu *self); //interrupt request signal
void cpuNmi(cpu *self); //non maskable interrupt signal

uint8_t cpuFetch(cpu *self);


//Addressing modes

//Indexed addressing
uint8_t ZPX(cpu *self);
uint8_t ZPY(cpu *self);
uint8_t ABX(cpu *self);
uint8_t ABY(cpu *self);
uint8_t IZX(cpu *self);
uint8_t IZY(cpu *self);

//Other addressing modes
uint8_t IMP(cpu *self); //Implied: targets accumulator
uint8_t ACC(cpu *self);
uint8_t IMM(cpu *self);
uint8_t ZP0(cpu *self);
uint8_t ABS(cpu *self);
uint8_t REL(cpu *self);
uint8_t IND(cpu *self);

//Instruction opcodes
uint8_t ADC(cpu *self);
uint8_t AND(cpu *self);
uint8_t ASL(cpu *self);
uint8_t BCC(cpu *self);
uint8_t BCS(cpu *self);
uint8_t BEQ(cpu *self);
uint8_t BIT(cpu *self);
uint8_t BMI(cpu *self);
uint8_t BNE(cpu *self);
uint8_t BPL(cpu *self);
uint8_t BRK(cpu *self);  
uint8_t BVC(cpu *self);
uint8_t BVS(cpu *self);
uint8_t CLC(cpu *self);
uint8_t CLD(cpu *self);
uint8_t CLI(cpu *self);
uint8_t CLV(cpu *self);
uint8_t CMP(cpu *self);
uint8_t CPX(cpu *self);
uint8_t CPY(cpu *self);

uint8_t DEC(cpu *self);
uint8_t DEX(cpu *self);
uint8_t DEY(cpu *self);
uint8_t EOR(cpu *self);
uint8_t INC(cpu *self);
uint8_t INX(cpu *self);
uint8_t INY(cpu *self);
uint8_t JMP(cpu *self);
uint8_t JSR(cpu *self);
uint8_t LDA(cpu *self);
uint8_t LDX(cpu *self);
uint8_t LDY(cpu *self);
uint8_t LSR(cpu *self);
uint8_t NOP(cpu *self);
uint8_t ORA(cpu *self);
uint8_t PHA(cpu *self);
uint8_t PHP(cpu *self);
uint8_t PLA(cpu *self);
uint8_t PLP(cpu *self);

uint8_t ROL(cpu *self);
uint8_t ROR(cpu *self);
uint8_t RTI(cpu *self);
uint8_t RTS(cpu *self);
uint8_t SBC(cpu *self);
uint8_t SEC(cpu *self);
uint8_t SED(cpu *self);
uint8_t SEI(cpu *self);
uint8_t STA(cpu *self);
uint8_t STX(cpu *self);
uint8_t STY(cpu *self);
uint8_t TAX(cpu *self);
uint8_t TAY(cpu *self);
uint8_t TSX(cpu *self);
uint8_t TXA(cpu *self);
uint8_t TXS(cpu *self);
uint8_t TYA(cpu *self);

uint8_t XXX(cpu *self);

#endif