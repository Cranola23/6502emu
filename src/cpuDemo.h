#ifndef CPUDEMO
#define CPUDEMO
#include "6502core.h"

void drawRam(cpu *self, int x, int y, uint16_t nAddr, int nRows, int nColumns);
void drawCpu(cpu *self, int x, int y);
void setupDemo(cpu *self, Bus *b, Memory *m);

#endif