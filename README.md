# Ricoh 2A03 Implemented in C
The Ricoh 2A03 is the one that was used in the NES (Nintendo Entertainment System) which is based on the 8-bit MOS 6502 cpu. 
This is a project for a 6502 cpu emulator written in pure C which aims to help me learn about emulation and improve my understanding of the C language.

## Usage
Requires [Raylib](https://www.raylib.com/) libraries installed.
`git clone` the repository and run `make` and `./run.exe` after building. This really just works on windows for now. 

To run custom source code on the emulator, load the object code into memory by copying it into `prog[]` at ram location `0x8000` in `cpuDemo.c` and rebuilding with `make`.
For assembling your custom program, you can use an online assembler such as [6502 Assembler by Masswerk](https://www.masswerk.at/6502/assembler.html).

## Credits
I am very grateful to the wonderful people behind [Nesdev Wiki](https://www.nesdev.org/wiki/Nesdev_Wiki) for their exhaustive collection of documentation and resources that have been instrumental in the development of this project and [OneLoneCoder](https://onelonecoder.com/) for his tutorials and content which have helped me sharpen my skills in C.