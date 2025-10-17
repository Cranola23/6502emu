#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "raylib.h"

#include "cpuDemo.h"
#include "6502core.h"  //cpu struct + cpuReset, cpuClock, cpuRead, etc.
#include "bus.h"       //BusInit(Bus*, Memory*), cpuConnectBus, Memory, Bus structs

#define HEX_CHARS "0123456789ABCDEF"

static char* hex(uint32_t n, uint8_t d, char* dst){ //tool to convert to hex string
    memset(dst,0,d+1);     
    dst[d]='\0';
    int i;
    for(i=d-1;i>=0;i--,n >>= 4){
        dst[i]=HEX_CHARS[n & 0xF];
    }
    return dst;
}

//Draw ram addressess (row*col)
void drawRam(cpu *self, int x, int y, uint16_t nAddr, int nRows, int nColumns){
    int nRamX=x, nRamY=y;
    char hex_buff[16];
    char sOffset[1024];

    for(int row=0; row<nRows; row++){
        memset(sOffset,0,1024);
        strcat(sOffset, "$");
        strcat(sOffset, hex(nAddr, 4, hex_buff)); //address
        strcat(sOffset, ":");
        for(int col=0; col<nColumns; col++){
            strcat(sOffset, " ");
            strcat(sOffset, hex(cpuRead(self, nAddr), 2, hex_buff));
            nAddr++;
        }
        DrawText(sOffset, nRamX, nRamY, 4, BLACK);
        nRamY+=10;
    }
}

void drawCpu(cpu *self, int x, int y){
    char hex_buff[16];
    DrawText("STATUS: ", x, y, 4, WHITE);
    DrawText("N", x + 64, y, 4, self->status & N ? GREEN : RED);
    DrawText("V", x + 80, y, 4, self->status & V ? GREEN : RED);
    DrawText("-", x + 96, y, 4, self->status & U ? GREEN : RED);
    DrawText("B", x + 112, y, 4, self->status & B ? GREEN : RED);
    DrawText("D", x + 128, y, 4, self->status & D ? GREEN : RED);
    DrawText("I", x + 144, y, 4, self->status & I ? GREEN : RED);
    DrawText("Z", x + 160, y, 4, self->status & Z ? GREEN : RED);
    DrawText("C", x + 178, y, 4, self->status & C ? GREEN : RED);

    char pc[1024];
    char a[1024];
    char xr[1024];
    char yr[1024];
    char sp[1024];

    strcpy(pc, "");
    strcpy(a, "");
    strcpy(xr, "");
    strcpy(yr, "");
    strcpy(sp, "");
    
    strcat(pc, "PC: $");
    strcat(pc, hex(self->PC,4,hex_buff));
    
    strcat(a, "A: $");
    strcat(a, hex(self->A,4,hex_buff));
    strcat(a, " [");
    char a_str[256];
    sprintf(a_str, "%d", self->A); //converting Acc(not hex val) to decimal to concat to str buffer
    strcat(a, a_str);
    strcat(a, "]");

    strcat(xr, "X: $");
    strcat(xr, hex(self->X,4,hex_buff));
    strcat(xr, " [");
    char xr_str[256];
    sprintf(xr_str, "%d", self->X); 
    strcat(xr, xr_str);
    strcat(xr, "]");
    
    strcat(yr, "Y: $");
    strcat(yr, hex(self->Y,4,hex_buff));
    strcat(yr, " [");
    char yr_str[256];
    sprintf(yr_str, "%d", self->Y); 
    strcat(yr, yr_str);
    strcat(yr, "]");

    strcat(sp, "SP: $");
    strcat(sp, hex(self->SP,4,hex_buff));
        
    DrawText(pc, x , y + 10, 4, WHITE);
    DrawText(a, x , y + 20, 4, WHITE);
    DrawText(xr, x , y + 30, 4, WHITE);
    DrawText(yr, x , y + 40, 4, WHITE);
    DrawText(sp, x , y + 50, 4, WHITE);
}

void setupDemo(cpu *self, Bus *b, Memory *m){
    BusInit(b,m);
    cpuInit(self);
    connectBus(self,b);
    
    	// Load Program (assembled at https://www.masswerk.at/6502/assembler.html)
		/*
			*=$8000
			LDX #10
			STX $0000
			LDX #3
			STX $0001
			LDY $0000
			LDA #0
			CLC
			loop
			ADC $0001
			DEY
			BNE loop
			STA $0002
			NOP
			NOP
			NOP
		*/

    uint8_t prog[] = {
        0xA2,0x0A,0x8E,0x00,0x00,0xA2,0x03,0x8E,0x01,0x00,
        0xAC,0x00,0x00,0xA9,0x00,0x18,0x6D,0x01,0x00,0x88,
        0xD0,0xFA,0x8D,0x02,0x00,0xEA,0xEA,0xEA
    };
    memcpy(&m->ram[0x8000], prog, sizeof prog);

    m->ram[0xFFFC]=0x00;
    m->ram[0xFFFD]=0x80;
    cpuReset(self);
}