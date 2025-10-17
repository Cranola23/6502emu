#include "bus.h"
#include "6502core.h"
#include "cpuDemo.h"
#include "raylib.h"


int main(void){
    Memory mem;
    Bus bus;
    cpu proc;

    InitWindow(680,480,"6502 CPU Demo");
    SetTargetFPS(60);

    setupDemo(&proc,&bus,&mem);

    while (!WindowShouldClose())
    {
        if(IsKeyPressed(KEY_SPACE)){
            do{cpuClock(&proc);} while(proc.cycles>0);
        }
        if(IsKeyPressed(KEY_R)){cpuReset(&proc);}
    
        BeginDrawing();
        ClearBackground(BLUE);

        drawRam(&proc, 2,2,0x0000,16,16);
        drawRam(&proc,2,220,0x8000,16,16);
        drawCpu(&proc, 420, 2);

        DrawText("SPACE = Next Cycle  R = Reset",10,450,10,YELLOW);
        EndDrawing();
    }
    CloseWindow();
    return 0;
    
}