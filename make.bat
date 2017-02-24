del minesweeper.exe
NASM -fobj minesweeper.asm -o minesweeper.obj
rc resources.rc
alink -oPE -subsys gui minesweeper.obj resources.res
minesweeper.exe
