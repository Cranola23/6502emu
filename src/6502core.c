#include "6502core.h"
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define ABS_SP(SP) ((0x0100) + (SP)) //Stack + offset(SP)
#define HEX_CHARS "0123456789ABCDEF"

typedef struct {
    char name[4];
    uint8_t (*opc)(cpu *self);
    uint8_t (*addrmode)(cpu *self);
    uint8_t cycles;
} cpuInstruction;

static cpuInstruction LOOKUP [16*16]= {
    { "BRK", BRK, IMP, 7 },{ "ORA", ORA, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 3 },{ "ORA", ORA, ZP0, 3 },{ "ASL", ASL, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PHP", PHP, IMP, 3 },{ "ORA", ORA, IMM, 2 },{ "ASL", ASL, IMP, 2 },{ "???", XXX, IMP, 2 },{ "???", NOP, IMP, 4 },{ "ORA", ORA, ABS, 4 },{ "ASL", ASL, ABS, 6 },{ "???", XXX, IMP, 6 },
    { "BPL", BPL, REL, 2 },{ "ORA", ORA, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "ORA", ORA, ZPX, 4 },{ "ASL", ASL, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "CLC", CLC, IMP, 2 },{ "ORA", ORA, ABY, 4 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "ORA", ORA, ABX, 4 },{ "ASL", ASL, ABX, 7 },{ "???", XXX, IMP, 7 },
    { "JSR", JSR, ABS, 6 },{ "AND", AND, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "BIT", BIT, ZP0, 3 },{ "AND", AND, ZP0, 3 },{ "ROL", ROL, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PLP", PLP, IMP, 4 },{ "AND", AND, IMM, 2 },{ "ROL", ROL, IMP, 2 },{ "???", XXX, IMP, 2 },{ "BIT", BIT, ABS, 4 },{ "AND", AND, ABS, 4 },{ "ROL", ROL, ABS, 6 },{ "???", XXX, IMP, 6 },
    { "BMI", BMI, REL, 2 },{ "AND", AND, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "AND", AND, ZPX, 4 },{ "ROL", ROL, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "SEC", SEC, IMP, 2 },{ "AND", AND, ABY, 4 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "AND", AND, ABX, 4 },{ "ROL", ROL, ABX, 7 },{ "???", XXX, IMP, 7 },
    { "RTI", RTI, IMP, 6 },{ "EOR", EOR, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 3 },{ "EOR", EOR, ZP0, 3 },{ "LSR", LSR, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PHA", PHA, IMP, 3 },{ "EOR", EOR, IMM, 2 },{ "LSR", LSR, IMP, 2 },{ "???", XXX, IMP, 2 },{ "JMP", JMP, ABS, 3 },{ "EOR", EOR, ABS, 4 },{ "LSR", LSR, ABS, 6 },{ "???", XXX, IMP, 6 },
    { "BVC", BVC, REL, 2 },{ "EOR", EOR, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "EOR", EOR, ZPX, 4 },{ "LSR", LSR, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "CLI", CLI, IMP, 2 },{ "EOR", EOR, ABY, 4 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "EOR", EOR, ABX, 4 },{ "LSR", LSR, ABX, 7 },{ "???", XXX, IMP, 7 },
    { "RTS", RTS, IMP, 6 },{ "ADC", ADC, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 3 },{ "ADC", ADC, ZP0, 3 },{ "ROR", ROR, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PLA", PLA, IMP, 4 },{ "ADC", ADC, IMM, 2 },{ "ROR", ROR, IMP, 2 },{ "???", XXX, IMP, 2 },{ "JMP", JMP, IND, 5 },{ "ADC", ADC, ABS, 4 },{ "ROR", ROR, ABS, 6 },{ "???", XXX, IMP, 6 },
    { "BVS", BVS, REL, 2 },{ "ADC", ADC, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "ADC", ADC, ZPX, 4 },{ "ROR", ROR, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "SEI", SEI, IMP, 2 },{ "ADC", ADC, ABY, 4 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "ADC", ADC, ABX, 4 },{ "ROR", ROR, ABX, 7 },{ "???", XXX, IMP, 7 },
    { "???", NOP, IMP, 2 },{ "STA", STA, IZX, 6 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 6 },{ "STY", STY, ZP0, 3 },{ "STA", STA, ZP0, 3 },{ "STX", STX, ZP0, 3 },{ "???", XXX, IMP, 3 },{ "DEY", DEY, IMP, 2 },{ "???", NOP, IMP, 2 },{ "TXA", TXA, IMP, 2 },{ "???", XXX, IMP, 2 },{ "STY", STY, ABS, 4 },{ "STA", STA, ABS, 4 },{ "STX", STX, ABS, 4 },{ "???", XXX, IMP, 4 },
    { "BCC", BCC, REL, 2 },{ "STA", STA, IZY, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 6 },{ "STY", STY, ZPX, 4 },{ "STA", STA, ZPX, 4 },{ "STX", STX, ZPY, 4 },{ "???", XXX, IMP, 4 },{ "TYA", TYA, IMP, 2 },{ "STA", STA, ABY, 5 },{ "TXS", TXS, IMP, 2 },{ "???", XXX, IMP, 5 },{ "???", NOP, IMP, 5 },{ "STA", STA, ABX, 5 },{ "???", XXX, IMP, 5 },{ "???", XXX, IMP, 5 },
    { "LDY", LDY, IMM, 2 },{ "LDA", LDA, IZX, 6 },{ "LDX", LDX, IMM, 2 },{ "???", XXX, IMP, 6 },{ "LDY", LDY, ZP0, 3 },{ "LDA", LDA, ZP0, 3 },{ "LDX", LDX, ZP0, 3 },{ "???", XXX, IMP, 3 },{ "TAY", TAY, IMP, 2 },{ "LDA", LDA, IMM, 2 },{ "TAX", TAX, IMP, 2 },{ "???", XXX, IMP, 2 },{ "LDY", LDY, ABS, 4 },{ "LDA", LDA, ABS, 4 },{ "LDX", LDX, ABS, 4 },{ "???", XXX, IMP, 4 },
    { "BCS", BCS, REL, 2 },{ "LDA", LDA, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 5 },{ "LDY", LDY, ZPX, 4 },{ "LDA", LDA, ZPX, 4 },{ "LDX", LDX, ZPY, 4 },{ "???", XXX, IMP, 4 },{ "CLV", CLV, IMP, 2 },{ "LDA", LDA, ABY, 4 },{ "TSX", TSX, IMP, 2 },{ "???", XXX, IMP, 4 },{ "LDY", LDY, ABX, 4 },{ "LDA", LDA, ABX, 4 },{ "LDX", LDX, ABY, 4 },{ "???", XXX, IMP, 4 },
    { "CPY", CPY, IMM, 2 },{ "CMP", CMP, IZX, 6 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 8 },{ "CPY", CPY, ZP0, 3 },{ "CMP", CMP, ZP0, 3 },{ "DEC", DEC, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "INY", INY, IMP, 2 },{ "CMP", CMP, IMM, 2 },{ "DEX", DEX, IMP, 2 },{ "???", XXX, IMP, 2 },{ "CPY", CPY, ABS, 4 },{ "CMP", CMP, ABS, 4 },{ "DEC", DEC, ABS, 6 },{ "???", XXX, IMP, 6 },
    { "BNE", BNE, REL, 2 },{ "CMP", CMP, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "CMP", CMP, ZPX, 4 },{ "DEC", DEC, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "CLD", CLD, IMP, 2 },{ "CMP", CMP, ABY, 4 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "CMP", CMP, ABX, 4 },{ "DEC", DEC, ABX, 7 },{ "???", XXX, IMP, 7 },
    { "CPX", CPX, IMM, 2 },{ "SBC", SBC, IZX, 6 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 8 },{ "CPX", CPX, ZP0, 3 },{ "SBC", SBC, ZP0, 3 },{ "INC", INC, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "INX", INX, IMP, 2 },{ "SBC", SBC, IMM, 2 },{ "NOP", NOP, IMP, 2 },{ "???", SBC, IMP, 2 },{ "CPX", CPX, ABS, 4 },{ "SBC", SBC, ABS, 4 },{ "INC", INC, ABS, 6 },{ "???", XXX, IMP, 6 },
    { "BEQ", BEQ, REL, 2 },{ "SBC", SBC, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "SBC", SBC, ZPX, 4 },{ "INC", INC, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "SED", SED, IMP, 2 },{ "SBC", SBC, ABY, 4 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "SBC", SBC, ABX, 4 },{ "INC", INC, ABX, 7 },{ "???", XXX, IMP, 7 },
};

void cpuInit(cpu *self){
    self->A=0;
    self->X=0;
    self->Y=0;
    self->PC=0x00;
    self->SP=0x00;

    self->fetched=0x00;
    self->address_abs=0x00;
    self->address_rel=0x00;
    self->cycles=0;
    self->opcode=0x00;
}

void connectBus(cpu *self, Bus *bus){
    self->bus = bus;
}

uint8_t cpuRead(cpu *self, uint16_t address){
    return BusRead(self->bus, address);
}

void cpuWrite(cpu *self, uint16_t address, uint8_t data){
    BusWrite(self->bus, address, data);
}

uint8_t cpuGetFlag(cpu *self, statusFlag flag){
    return ((self->status & flag) > 0) ? 1 : 0;
}

void cpuSetFlag(cpu *self, statusFlag flag, bool value){
    if(value){
        self->status |= flag; //set bit
    }
    else{
        self->status &= ~flag;
    }
}

void cpuClock(cpu *self){
    if(self->cycles == 0){
        self->opcode = cpuRead(self, self->PC);
        self->PC++;

        cpuInstruction currentInstr = LOOKUP[self->opcode];
        self->cycles = currentInstr.cycles;
        uint8_t addressmode = currentInstr.addrmode(self);
        uint8_t operation = currentInstr.opc(self);

        self->cycles += (addressmode & operation); //extra cycle when addressmode and operation return 1 
    }
    self->cycles--;
}

void cpuReset(cpu *self){
    self->address_abs = 0xFFFC; //address to set PC to
    uint16_t low = cpuRead(self, self->address_abs);
    uint16_t high = cpuRead(self, self->address_abs + 1);
    self->PC = (high << 8) | low;
    
    self->A = 0x00;
    self->X = 0x00;
    self->Y = 0x00;
    self->status = 0x00 | U;
    self->SP = 0xFD; //SP offset resets since 0xFF,0xFE,0xFD goes to reset vector and status

    self->address_rel = 0x0000;
    self->address_abs = 0x0000;
    self->fetched = 0x00;

    self->cycles = 8;
}

void cpuIrq(cpu *self){
    if(cpuGetFlag(self, I) == 0){
        cpuWrite(self, ABS_SP(self->SP--), (self->PC >> 8) & 0x00FF); //push high byte
        cpuWrite(self, ABS_SP(self->SP), self->PC & 0x00FF); //push low byte

        cpuSetFlag(self, U, 1);
        cpuSetFlag(self, B, 0);
        cpuSetFlag(self, I, 1);
        cpuWrite(self, ABS_SP(self->SP--), self->status); //push status

        self->address_abs = 0xFFFE;
        uint16_t low = cpuRead(self, self->address_abs); 
        uint16_t high = cpuRead(self, self->address_abs + 1);
        self->PC = (high << 8) | low; //load PC from IRQ address

        self->cycles = 7;
    }
}

void cpuNmi(cpu *self){
    cpuWrite(self, ABS_SP(self->SP--), (self->PC >> 8) & 0x00FF); //push high byte
    cpuWrite(self, ABS_SP(self->SP), self->PC & 0x00FF); //push low byte

    cpuSetFlag(self, U, 1);
    cpuSetFlag(self, B, 0);
    cpuSetFlag(self, I, 1);
    cpuWrite(self, ABS_SP(self->SP--), self->status); //push status

    self->address_abs = 0xFFFA;
    uint16_t low = cpuRead(self, self->address_abs); 
    uint16_t high = cpuRead(self, self->address_abs + 1);
    self->PC = (high << 8) | low; //load PC from NMI address 

    self->cycles = 8;
}

uint8_t cpuFetch(cpu *self){
    if(LOOKUP[self->opcode].addrmode != IMP){
        self->fetched = cpuRead(self, self->address_abs);
    }    
    return self->fetched;
}


//Addressing Modes
//Indexed Addressing
uint8_t ZPX(cpu *self){
    self->address_abs = cpuRead(self, self->PC++) + self->X;
    self->address_abs &= 0x00FF;
    return 0;
}

uint8_t ZPY(cpu *self){
    self->address_abs = cpuRead(self, self->PC++) + self->Y;
    self->address_abs &= 0x00FF;
    return 0;
}

uint8_t ABX(cpu *self){
    uint16_t lo = cpuRead(self, self->PC++);
    uint16_t hi = cpuRead(self, self->PC++);
    self->address_abs = ((hi<<8) | lo) + self->X;

    //checking if high byte of absadress is diff from original page
    if((self->address_abs & 0xFF00) != (hi<<8)){
        return 1; //extra cycle
    } 
    return 0;
}

uint8_t ABY(cpu *self){
    uint16_t lo = cpuRead(self, self->PC++);
    uint16_t hi = cpuRead(self, self->PC++);
    self->address_abs = ((hi<<8) | lo) + self->Y;

    //checking if high byte of absadress is diff from original page
    if((self->address_abs & 0xFF00) != (hi<<8)){
        return 1; //extra cycle
    } 
    return 0;
}

uint8_t IZX(cpu *self){ //add X offset in zero page
    uint16_t t = cpuRead(self, self->PC++);  //read zero page pointer
    uint16_t lo = ((uint16_t)(t + (uint16_t)self->X) & 0x00FF);
    uint16_t hi = ((uint16_t)(t + (uint16_t)self->X + 1) & 0x00FF);
    self->address_abs = (hi << 8) | lo;
    return 0;
}

uint8_t IZY(cpu *self){  //add Y offset after base address so an extra cycle
    uint16_t t = cpuRead(self, self->PC++);
    uint16_t lo = cpuRead(self, (t & 0x00FF));
    uint16_t hi = cpuRead(self, ((t+1) & 0x00FF));
    self->address_abs = ((hi << 8) | lo) + self->Y;

    if((self->address_abs & 0x00FF) != (hi<<8)){
        return 1;
    }
    return 0;
}

//Other Addressing
uint8_t IMP(cpu *self){
    self->fetched = self->A;
    return 0;
}

uint8_t ACC(cpu *self){
    return 0;
}

uint8_t IMM(cpu *self){
    self->address_abs = self->PC++;
    return 0;
}

uint8_t ZP0(cpu *self){
    self->address_abs = cpuRead(self, self->PC);
    self->address_abs &= 0x00FF; 
    self->PC++;
    return 0;
}

uint8_t ABS(cpu *self){
    uint16_t low = cpuRead(self, self->PC++);
    uint16_t high = cpuRead(self, self->PC++);

    self->address_abs = (high<<8) | low;
    return 0;
}

uint8_t REL(cpu *self){
    self->address_rel = cpuRead(self, self->PC++);
    if(self->address_rel & 0x80){
        self->address_rel |= 0xFF00;
    }
    return 0;
}

uint8_t IND(cpu *self){
    uint16_t lo_ptr = cpuRead(self, self->PC++);
    uint16_t hi_ptr = cpuRead(self, self->PC++);
    uint16_t ptr = (hi_ptr<<8) | lo_ptr;
    //emulating 6502 jump bug where if low byte 0xFF then it wraps around the same page
    if(lo_ptr==0x00FF){ 
        self->address_abs = (cpuRead(self, ptr & 0xFF00)<<8) | cpuRead(self, ptr+0);
    }
    else{
        self->address_abs = (cpuRead(self, ptr+0)<<8) | cpuRead(self, ptr+1);
    }
    return 0;
}

//OP codes

uint8_t ADC(cpu *self){
    cpuFetch(self);
    self->temp = (uint16_t)self->A + (uint16_t)self->fetched + (uint16_t)cpuGetFlag(self, C); //addition A=A+M+C
    cpuSetFlag(self, C, self->temp > 255);
    cpuSetFlag(self, Z, (self->temp & 0x00FF) == 0);
    cpuSetFlag(self, V, ((uint16_t)self->A ^ (uint16_t)self->temp) & (~((uint16_t)self->A ^ (uint16_t)self->fetched)) & 0x0080); //overflow
    cpuSetFlag(self, N, (self->temp & 0x80));
    self->A = self->temp & 0x00FF;
    return 1;
}

uint8_t AND(cpu *self){
    cpuFetch(self);
    self->A = self->A & self->fetched;
    cpuSetFlag(self, Z, 0x00);
    cpuSetFlag(self, N, self->A & 0x80); //N is set if bit 7 is set
    return 1; 
}

uint8_t ASL(cpu *self){
    cpuFetch(self);
    self->temp = (uint16_t)self->fetched << 1;
    cpuSetFlag(self, C, (self->temp & 0xFF00) > 0);
    cpuSetFlag(self, Z, (self->temp & 0x00FF) == 0x00);
    cpuSetFlag(self, N, (self->temp & 0x80));

    if(LOOKUP[self->opcode].addrmode(self) == IMP){
        self->A = self->temp & 0x00FF;
    }
    else{
        cpuWrite(self, self->address_abs, self->temp & 0x00FF); 
    }
    return 0;
}

uint8_t BCC(cpu *self){
    if(cpuGetFlag(self,C) == 0){
        self->cycles++;
        self->address_abs = self->PC + self->address_rel;
        if((self->address_abs & 0xFF00) != (self->PC & 0xFF00)){ //not the same page
            self->cycles++;
        }
        self->PC = self->address_abs;
    }
    return 0;
}

uint8_t BCS(cpu *self){
    if(cpuGetFlag(self,C) == 1){
        self->cycles++;
        self->address_abs = self->PC + self->address_rel;
        if((self->address_abs & 0xFF00) != (self->PC & 0xFF00)){ //not the same page
            self->cycles++;
        }
        self->PC = self->address_abs;
    }
    return 0;
}

uint8_t BEQ(cpu *self){
    if(cpuGetFlag(self,Z) == 1){
        self->cycles++;
        self->address_abs = self->PC + self->address_rel;
        if((self->address_abs & 0xFF00) != (self->PC & 0xFF00)){ //not the same page
            self->cycles++;
        }
        self->PC = self->address_abs;
    }
    return 0;
}

uint8_t BIT(cpu *self){
    cpuFetch(self);
    self->temp = self->A & self->fetched;
    cpuSetFlag(self, Z, (self->temp & 0x00FF) == 0);
    cpuSetFlag(self, V, (self->fetched & 0x40));
    cpuSetFlag(self, N, (self->fetched & 0x80));
    return 0;
}

uint8_t BMI(cpu *self){
    if(cpuGetFlag(self,N) == 1){
        self->cycles++;
        self->address_abs = self->PC + self->address_rel;
        if((self->address_abs & 0xFF00) != (self->PC & 0xFF00)){ //not the same page
            self->cycles++;
        }
        self->PC = self->address_abs;
    }
    return 0;
}

uint8_t BNE(cpu *self){
    if(cpuGetFlag(self,Z) == 0){
        self->cycles++;
        self->address_abs = self->PC + self->address_rel;
        if((self->address_abs & 0xFF00) != (self->PC & 0xFF00)){ //not the same page
            self->cycles++;
        }
        self->PC = self->address_abs;
    }
    return 0;
}

uint8_t BPL(cpu *self){
    if(cpuGetFlag(self,N) == 0){
        self->cycles++;
        self->address_abs = self->PC + self->address_rel;
        if((self->address_abs & 0xFF00) != (self->PC & 0xFF00)){ //not the same page
            self->cycles++;
        }
        self->PC = self->address_abs;
    }
    return 0;
}

uint8_t BRK(cpu *self){
    self->PC++;
    cpuSetFlag(self, I, 1);
    cpuWrite(self, ABS_SP(self->SP--), ((self->PC >> 8) & 0x00FF)); //high byte
    cpuWrite(self, (self->SP--), (self->PC & 0x00FF)); //low byte

    cpuSetFlag(self, B, 1);
    cpuWrite(self, (self->SP--), (self->status));
    cpuSetFlag(self, B, 1);

    self->PC = (uint16_t)cpuRead(self, 0xFFFE) | (uint16_t)cpuRead(self, 0xFFFF) << 8;
    return 0;
}

uint8_t BVC(cpu *self){
    if(cpuGetFlag(self,V) == 0){
        self->cycles++;
        self->address_abs = self->PC + self->address_rel;
        if((self->address_abs & 0xFF00) != (self->PC & 0xFF00)){ //not the same page
            self->cycles++;
        }
        self->PC = self->address_abs;
    }
    return 0;
}

uint8_t BVS(cpu *self){
    if(cpuGetFlag(self,V) == 1){
        self->cycles++;
        self->address_abs = self->PC + self->address_rel;
        if((self->address_abs & 0xFF00) != (self->PC & 0xFF00)){ //not the same page
            self->cycles++;
        }
        self->PC = self->address_abs;
    }
    return 0;
}

uint8_t CLC(cpu *self){
    cpuSetFlag(self, C, 0);
    return 0;
}

uint8_t CLD(cpu *self){
    cpuSetFlag(self, D, 0);
    return 0;
}

uint8_t CLI(cpu *self){
    cpuSetFlag(self, I, 0);
    return 0;
}

uint8_t CLV(cpu *self){
    cpuSetFlag(self, V, 0);
    return 0;
}

uint8_t CMP(cpu *self){
    cpuFetch(self);
    self->temp = (uint16_t)self->A - (uint16_t)self->fetched;
    cpuSetFlag(self, C, (self->A>=self->fetched));
    cpuSetFlag(self, Z, ((self->temp & 0x00FF) == 0x0000));
    cpuSetFlag(self, N, (self->temp & 0x0080));
    return 1;
}

uint8_t CPX(cpu *self){
    cpuFetch(self);
    self->temp = (uint16_t)self->X - (uint16_t)self->fetched;
    cpuSetFlag(self, C, (self->X>=self->fetched));
    cpuSetFlag(self, Z, ((self->temp & 0x00FF) == 0x0000));
    cpuSetFlag(self, N, (self->temp & 0x0080));
    return 0;
}

uint8_t CPY(cpu *self){
    cpuFetch(self);
    self->temp = (uint16_t)self->Y - (uint16_t)self->fetched;
    cpuSetFlag(self, C, (self->Y>=self->fetched));
    cpuSetFlag(self, Z, ((self->temp & 0x00FF) == 0x0000));
    cpuSetFlag(self, N, (self->temp & 0x0080));
    return 0;
}

uint8_t DEC(cpu *self){
    cpuFetch(self);
    self->temp = (uint16_t)self->fetched - 0x0001;
    cpuWrite(self, self->address_abs, (self->temp & 0x00FF));
    cpuSetFlag(self, Z, ((self->temp & 0x00FF) == 0x0000));
    cpuSetFlag(self, N, (self->temp & 0x0080));
    return 0;
}

uint8_t DEX(cpu *self){
    self->X--;
    cpuSetFlag(self, Z, (self->X == 0x00));
    cpuSetFlag(self, N, (self->X & 0x80));
    return 0;
}

uint8_t DEY(cpu *self){
    self->Y--;
    cpuSetFlag(self, Z, (self->Y == 0x00));
    cpuSetFlag(self, N, (self->Y & 0x80));
    return 0;
}

uint8_t EOR(cpu *self){
    cpuFetch(self);
    self->A ^= self->fetched;
    cpuSetFlag(self, Z, self->A == 0x00);
    cpuSetFlag(self, N, self->A & 0x80);
    return 1;
}

uint8_t INC(cpu *self){
    cpuFetch(self);
    self->temp = (uint16_t)self->fetched + 0x0001;
    cpuWrite(self, self->address_abs, (self->temp & 0x00FF));
    cpuSetFlag(self, Z, ((self->temp & 0x00FF) == 0x0000));
    cpuSetFlag(self, N, (self->temp & 0x0080));
    return 0;
}

uint8_t INX(cpu *self){
    self->X++;
    cpuSetFlag(self, Z, (self->X == 0x00));
    cpuSetFlag(self, N, (self->X & 0x80));
    return 0;
}

uint8_t INY(cpu *self){
    self->Y++;
    cpuSetFlag(self, Z, (self->Y == 0x00));
    cpuSetFlag(self, N, (self->Y & 0x80));
    return 0;
}

uint8_t JMP(cpu *self){
    self->PC = self->address_abs;
    return 0;
}

uint8_t JSR(cpu *self){
    --self->PC; //address - 1 of the return point
    cpuWrite(self, ABS_SP(self->SP--), (self->PC >> 8) & 0x00FF); //high byte
    cpuWrite(self, ABS_SP(self->SP), self->PC & 0x00FF); //low byte
    self->PC = self->address_abs;
    return 0;
}

uint8_t LDA(cpu *self){
    cpuFetch(self);
    self->A = self->fetched;
    cpuSetFlag(self, Z, (self->A == 0x00));
    cpuSetFlag(self, N, (self->A & 0x80));
    return 1;
}

uint8_t LDX(cpu *self){
    cpuFetch(self);
    self->X = self->fetched;
    cpuSetFlag(self, Z, (self->X == 0x00));
    cpuSetFlag(self, N, (self->X & 0x80));
    return 1;
}

uint8_t LDY(cpu *self){
    cpuFetch(self);
    self->Y = self->fetched;
    cpuSetFlag(self, Z, (self->Y == 0x00));
    cpuSetFlag(self, N, (self->Y & 0x80));
    return 1;
}

uint8_t LSR(cpu *self){
    cpuFetch(self);
    cpuSetFlag(self, C, (self->fetched & 0x0001));
    self->temp = self->fetched >> 1; 
    cpuSetFlag(self, Z, ((self->temp & 0x00FF) == 0));
    cpuSetFlag(self, N, (self->temp & 0x0080));

    if(LOOKUP[self->opcode].addrmode == IMP){
        self->A = self->temp & 0x00FF;
    }
    else{
        cpuWrite(self, self->address_abs, (self->temp & 0x00FF));
    }
    return 0;
}

uint8_t NOP(cpu *self){ 
    switch (self->opcode){
        case 0x1C:
        case 0x3C:
        case 0x5C:
        case 0x7C:
        case 0xDC:
        case 0xFC:
            return 1;
        default:
            break;
    }
	return 0;
}

uint8_t ORA(cpu *self){
    cpuFetch(self);
    self->A |= self->fetched;
    cpuSetFlag(self, Z, (self->A = 0x00));
    cpuSetFlag(self, N, (self->A & 0x80));
    return 1;
}

uint8_t PHA(cpu *self){
    cpuWrite(self, ABS_SP(self->SP--), self->A);
    return 0;
}

uint8_t PHP(cpu *self){
    cpuWrite(self, ABS_SP(self->SP--), self->status|B|U); //break flag set to 1
    cpuSetFlag(self, B, 0);
    cpuSetFlag(self, U, 0);
    return 0;
}

uint8_t PLA(cpu *self){
    self->SP++;
    self->A = cpuRead(self, ABS_SP(self->SP));
    cpuSetFlag(self, Z, (self->A == 0x00));
    cpuSetFlag(self, N, (self->A & 0x80));
    return 0;
}

uint8_t PLP(cpu *self){
    self->status = cpuRead(self, ABS_SP(++self->SP));
    cpuSetFlag(self, U, 1);
    return 0;
}

uint8_t ROL(cpu *self){
    cpuFetch(self);
    self->temp = (uint16_t)self->fetched << 1 | cpuGetFlag(self, C); // C << value << C (C is bit 0)
    cpuSetFlag(self, C, ((self->temp & 0x0100)!= 0));
    cpuSetFlag(self, Z, ((self->temp & 0x00FF) == 0x0000));
    cpuSetFlag(self, N, (self->temp & 0x0080));

    if(LOOKUP[self->opcode].addrmode == IMP){
        self->A = self->temp & 0x00FF;
    }
    else{
        cpuWrite(self, self->address_abs, self->temp & 0x00FF);
    }
    return 0;
}

uint8_t ROR(cpu *self){
    cpuFetch(self);
    self->temp = (uint16_t)cpuGetFlag(self, C) << 7 | (self->fetched >> 1);
    cpuSetFlag(self, C, (self->fetched & 0x0001));
    cpuSetFlag(self, Z, ((self->A & 0x00FF) == 0x00));
    cpuSetFlag(self, N, (self->temp & 0x0080));

    if(LOOKUP[self->opcode].addrmode == IMP){
        self->A = self->temp & 0x00FF;
    }
    else{
        cpuWrite(self, self->address_abs, self->temp & 0x00FF);
    }
    return 0;
}

uint8_t RTI(cpu *self){
    self->status = cpuRead(self, ABS_SP(++self->SP));
    cpuSetFlag(self, B, ~cpuGetFlag(self, B));
    cpuSetFlag(self, U, ~cpuGetFlag(self, U));
    
    uint16_t low = cpuRead(self, ABS_SP(++self->SP));
    uint16_t high = cpuRead(self, ABS_SP(++self->SP));
    self->PC = (high << 8) | low;
    return 0;
}

uint8_t RTS(cpu *self){
    uint16_t low = cpuRead(self, ABS_SP(++self->SP));
    uint16_t high = cpuRead(self, ABS_SP(++self->SP));
    self->PC = (high << 8) | low;
    self->PC++;
    return 0;
}

uint8_t SBC(cpu *self){
    cpuFetch(self);
    uint16_t inv = (uint16_t)(self->fetched) ^ 0x00FF;
    self->temp = (uint16_t)self->A + inv + (uint16_t)cpuGetFlag(self, C); //subtract A=A+(-M)+C+1 (-M+1 is 2s complement)
    cpuSetFlag(self, C, self->temp & 0xFF00);
    cpuSetFlag(self, Z, (self->temp & 0x00FF) == 0);
    cpuSetFlag(self, V, ((uint16_t)self->A ^ self->temp) & (self->temp ^ inv) & 0x0080); //overflow
    cpuSetFlag(self, N, (self->temp & 0x0080));   
    self->A = self->temp & 0x00FF;
    return 1;
}

uint8_t SEC(cpu *self){
    cpuSetFlag(self, C, 1);
    return 0;
}

uint8_t SED(cpu *self){
    cpuSetFlag(self, D, 1);
    return 0;
}

uint8_t SEI(cpu *self){
    cpuSetFlag(self, I, 1);
    return 0;
}

uint8_t STA(cpu *self){
    cpuWrite(self, self->address_abs, self->A);
    return 0;
}

uint8_t STX(cpu *self){
    cpuWrite(self, self->address_abs, self->X);
    return 0;
}

uint8_t STY(cpu *self){
    cpuWrite(self, self->address_abs, self->Y);
    return 0;
}

uint8_t TAX(cpu *self){
    self->X = self->A;
    cpuSetFlag(self, Z, self->X == 0);
    cpuSetFlag(self, N, self->X & 0x80);
    return 0;
}

uint8_t TAY(cpu *self){
    self->Y = self->A;
    cpuSetFlag(self, Z, self->Y == 0);
    cpuSetFlag(self, N, self->Y & 0x80);
    return 0;
}

uint8_t TSX(cpu *self){
    self->X = self->SP;
    cpuSetFlag(self, Z, self->X == 0);
    cpuSetFlag(self, N, self->X & 0x80);
    return 0;
}

uint8_t TXA(cpu *self){
    self->A = self->X;
    cpuSetFlag(self, Z, self->A == 0);
    cpuSetFlag(self, N, self->A & 0x80);
    return 0;
}

uint8_t TXS(cpu *self){
    self->SP = self->X;
    return 0;
}

uint8_t TYA(cpu *self){
    self->A = self->Y;
    cpuSetFlag(self, Z, self->A == 0);
    cpuSetFlag(self, N, self->A & 0x80);
    return 0;
}

uint8_t XXX(cpu *self){
    return 0;
}

static char* hex(uint32_t n, uint8_t d, char* dst){ //tool to convert to hex string
    memset(dst,0,d+1);     //strset(dst, 0);
    dst[d]='\0';
    int i;
    for(i=d-1;i>=0;i--,n >>= 4){
        dst[i]=HEX_CHARS[n & 0xF];
    }
    return dst;
}

//Disassembly function; uses cpu internal operation 
void cpuDisassemble(cpu *self, uint16_t nStart, uint16_t nStop, char *mapLines[0xFFFF]){
    uint32_t addr = nStart;
    uint8_t value = 0x00, low = 0x00, high = 0x00;
    uint16_t line_addr;
    char* sInstr = (char*)calloc(1024, 1); 
    char hex_buff[16];

    while(addr < nStop){
        memset(sInstr,0,1024);
        line_addr = addr;

        //Instruction address prefix
        strcat(sInstr, "$");
        strcat(sInstr, hex(addr, 4, hex_buff));
        strcat(sInstr, ": ");

        //Read Instruction; human readable
        uint8_t opcode = cpuRead(self, addr);
        addr++;
        strcat(sInstr, LOOKUP[opcode].name);
        strcat(sInstr, " ");

        //Read Address Modes
        if (LOOKUP[opcode].addrmode == IMP)
		{
            strcat(sInstr, " {IMP}");
		}
		else if (LOOKUP[opcode].addrmode == IMM)
		{
			value = cpuRead(self, addr); addr++;
            strcat(sInstr, "#$");
            strcat(sInstr, hex(value, 2, hex_buff));
            strcat(sInstr, " {IMM}");
		}
		else if (LOOKUP[opcode].addrmode == ZP0)
		{
			low = cpuRead(self, addr); addr++;
			high = 0x00;
            strcat(sInstr, "$");
            strcat(sInstr, hex(low, 2, hex_buff));
            strcat(sInstr, " {ZP0}");
		}
		else if (LOOKUP[opcode].addrmode == ZPX)
		{
			low = cpuRead(self, addr); addr++;
			high = 0x00;
            strcat(sInstr, "$");
            strcat(sInstr, hex(low, 2, hex_buff));
            strcat(sInstr, ", X {ZPX}");
		}
		else if (LOOKUP[opcode].addrmode == ZPY)
		{
			low = cpuRead(self, addr); addr++;
			high = 0x00;
            strcat(sInstr, "$");
            strcat(sInstr, hex(low, 2, hex_buff));
            strcat(sInstr, ", Y {ZPY}");
		}
		else if (LOOKUP[opcode].addrmode == IZX)
		{
			low = cpuRead(self, addr); addr++;
			high = 0x00;
            strcat(sInstr, "($");
            strcat(sInstr, hex(low, 2, hex_buff));
            strcat(sInstr, ", X) {IZX}");					
		}
		else if (LOOKUP[opcode].addrmode == IZY)
		{
			low = cpuRead(self, addr); addr++;
			high = 0x00;
            strcat(sInstr, "($");
            strcat(sInstr, hex(low, 2, hex_buff));
            strcat(sInstr, "), Y {IZY}");	
		}
		else if (LOOKUP[opcode].addrmode == ABS)
		{
			low = cpuRead(self, addr); addr++;
			high = cpuRead(self, addr); addr++;
            strcat(sInstr, "$");
            strcat(sInstr, hex((uint16_t)(high << 8) | low, 4, hex_buff));
            strcat(sInstr, " {ABS}");
		}
		else if (LOOKUP[opcode].addrmode == ABX)
		{
			low = cpuRead(self, addr); addr++;
			high = cpuRead(self, addr); addr++;
            strcat(sInstr, "$");
            strcat(sInstr, hex((uint16_t)(high << 8) | low, 4, hex_buff));
            strcat(sInstr, ", X {ABX}");
		}
		else if (LOOKUP[opcode].addrmode == ABY)
		{
			low = cpuRead(self, addr); addr++;
			high = cpuRead(self, addr); addr++;
            strcat(sInstr, "$");
            strcat(sInstr, hex((uint16_t)(high << 8) | low, 4, hex_buff));
            strcat(sInstr, ", Y {ABY}");
		}
		else if (LOOKUP[opcode].addrmode == IND)
		{
			low = cpuRead(self, addr); addr++;
			high = cpuRead(self, addr); addr++;
            strcat(sInstr, "($");
            strcat(sInstr, hex((uint16_t)(high << 8) | low, 4, hex_buff));
            strcat(sInstr, ") {IND}");
		}
		else if (LOOKUP[opcode].addrmode == REL)
		{
			value = cpuRead(self, addr); addr++;
            strcat(sInstr, "$");
            strcat(sInstr, hex(value, 2, hex_buff));
            strcat(sInstr, " [$");
            strcat(sInstr, hex(addr + (uint8_t)value, 4, hex_buff));
            strcat(sInstr, "] {REL}");
		}

		mapLines[line_addr] = strdup(sInstr);
    }
}