; This is an assembly language implementation of Minesweeper game. This
; file was created as a school project for IPA course at FIT BUT.
;
; Miloslav Číž, 2013


;=======================================================================

bits 32

; include:
%include 'win32n.inc'     ; WinApi declarations
%include 'general.mac'    ; general macros
%include 'rw32.inc'       ; input/output library

; dll function import:

dllimport DeleteDC,gdi32.dll
dllimport Rectangle,gdi32.dll
dllimport GetModuleHandle,kernel32.dll,GetModuleHandleA
dllimport GetCommandLine,kernel32.dll,GetCommandLineA
dllimport GetLastError,kernel32.dll
dllimport ExitProcess,kernel32.dll
dllimport GetSystemTime,kernel32.dll
dllimport CreateCompatibleDC,Gdi32.dll
dllimport SelectObject,gdi32.dll
dllimport GetObject,gdi32.dll,GetObjectA
dllimport ShowWindow,user32.dll
dllimport ShowCursor,user32.dll
dllimport GetDC,user32.dll
dllimport InvalidateRect,user32.dll
dllimport UpdateWindow,user32.dll
dllimport TranslateMessage,user32.dll
dllimport RegisterClassEx,user32.dll, RegisterClassExA
dllimport LoadIcon,user32.dll,LoadIconA
dllimport LoadCursor,user32.dll,LoadCursorA
dllimport CreateWindowEx,user32.dll,CreateWindowExA
dllimport GetMessage,user32.dll,GetMessageA
dllimport PeekMessage,user32.dll,PeekMessageA
dllimport DispatchMessage,user32.dll,DispatchMessageA
dllimport PostQuitMessage,user32.dll
dllimport MessageBox,user32.dll,MessageBoxA
dllimport DefWindowProc,user32.dll,DefWindowProcA
dllimport LoadMenu,user32.dll,LoadMenuA
dllimport BeginPaint,user32.dll
dllimport EndPaint,user32.dll
dllimport TextOut,gdi32.dll,TextOutA
dllimport BitBlt,gdi32.dll
dllimport DeleteObject,gdi32.dll
dllimport LoadImage,user32.dll,LoadImageA
dllimport SetWindowPos,user32.dll
dllimport MoveWindow,user32.dll
dllimport GetCursor,user32.dll
dllimport SetCursor,user32.dll
dllimport GetSystemMetrics,user32.dll
dllimport LoadCursor,user32.dll,LoadCursorA
dllimport SetTimer,user32.dll
dllimport KillTimer,user32.dll
dllimport PlaySound,winmm.dll,PlaySoundA
dllimport GetWindowRect,user32.dll

;=======================================================================

; DATA SEGMENT:

[section .data class=DATA use32 align=16]

INFO_BAR_HEIGHT  equ    30                   ; upper info bar height

mapWidth     dd 10      ; map width in squares
mapHeight    dd 10      ; map height in squares
mapArray     resb 10000 ; 2D map array (saved by rows), value semantics:
                        ;   7 6 5 4 3 2 1 0
                        ;
                        ;   2 MSB (bits 7 and 6):
                        ;     00 = revealed
                        ;     01 = hidden
                        ;     10 = marked as mine
                        ;     11 = marked with question mark
                        ;
                        ;   3 LSB (bits 3, 2, 1 and 0):
                        ;     number 0 - 8 = number of mines in the
                        ;                    neighbourhood
                        ;     1111 = mine

hWnd         dd 0       ; window handle
dwWndWidth   dd 300     ; window width
dwWndHeight  dd 200     ; window height
hHDC         dd 0       ; device context handle
hMHDC        dd 0       ; memory device context handle
hBitmapOld   dd 0       ; helper handle
hMenu        dd 0       ; menu handle
hIcon        dd 0       ; application icon

randomValue  dd 0       ; randomly generated value
helperX      db 0
helperY      db 0
helperA      db 0
helperB      db 0
helperC      db 0
gameOver     db 0       ; whether the game is over (1) or not (0)
windowWidth  dd 0       ; window width in pixels
windowHeight dd 0       ; window height in pixels
timerId      dd 0       ; id of the timer object
timerCount   dd 0       ; increments each second
mineCount    dd 5       ; number of mines left to find
helper       dd 0       ; a helper variable
gameTime     dd 0xFFFFFFFF     ; game time counter, 0xFFFFFFFF = stopped

neighbourhood:          ; 8-square neighbourhood offsets (x,y)
             db -1      ; upper left
             db -1
             db 0       ; upper
             db -1
             db 1       ; upper right
             db -1
             db -1      ; left
             db 0
             db 1       ; right
             db 0
             db -1      ; bottom left
             db 1
             db 0       ; bottom
             db 1
             db 1       ; bottom right
             db 1

message:     resb MSG_size

                               ; resource paths:

pathBitmapMine               db "images/mine.bmp",0
pathBitmapSquareHidden       db "images/square_hidden.bmp",0
pathBitmapSquareUnknown      db "images/square_unknown.bmp",0
pathBitmapSquareMine1        db "images/square_mine_1.bmp",0
pathBitmapSquareMine2        db "images/square_mine_2.bmp",0
pathBitmapSquareRevealed0    db "images/square_empty_0.bmp",0
pathBitmapSquareRevealed1    db "images/square_empty_1.bmp",0
pathBitmapSquareRevealed2    db "images/square_empty_2.bmp",0
pathBitmapSquareRevealed3    db "images/square_empty_3.bmp",0
pathBitmapSquareRevealed4    db "images/square_empty_4.bmp",0
pathBitmapSquareRevealed5    db "images/square_empty_5.bmp",0
pathBitmapSquareRevealed6    db "images/square_empty_6.bmp",0
pathBitmapSquareRevealed7    db "images/square_empty_7.bmp",0
pathBitmapSquareRevealed8    db "images/square_empty_8.bmp",0
pathSoundExplosion           db "sounds/explosion.wav",0

string szWndClassName,       "minesweeper window"   ; window class name
string szWndCaption,         "Assembly Minesweeper" ; window caption

string menuName,             "MinesweeperMenu"
string textTime,             "time: "
string textMines,            "mines left: "
string timeString,           "00:00"
string mineString,           "99"
string textLost,             "You have lost."
string textWon,              "You have won."
string textMessage,          "game message"
string textNewGame,          "Do you want an easy game?"
string textAbout,            "Assembly Minesweeper v 1.0, (c) Miloslav Ciz 2013"
string textHelp,             "Reveal all squares that don't have mines on them, if you click a square containing a mine, you lose. Each empty square that has been revealed is marked with a number that says how many mines there are in its 8-square neighbourhood (no number means 0). You can lose with your first move. The right mouse button can help you mark the squares. Good luck."

hBitmapMine                  dd   0 ; bitmap handle - mine
hBitmapSquareHidden          dd   0 ; bitmap handle - hidden square
hBitmapSquareUnknown         dd   0 ; bitmap handle - '?' marked square
hBitmapSquareMine1           dd   0 ; bitmap handle - square marked with mine (frame 0)
hBitmapSquareMine2           dd   0 ; bitmap handle - square marked with mine (frame 1)
hBitmapSquareRevealed        resd 9 ; 9 bitmap handles - revealed squares with numbers on them
hBitmapHelper                dd   0 ; helper handle to bitmap

WndClass:                      ;WndClass struct initialisation:
    istruc WNDCLASSEX
      at WNDCLASSEX.cbSize,             dd WNDCLASSEX_size
      at WNDCLASSEX.style,              dd CS_VREDRAW + CS_HREDRAW
      at WNDCLASSEX.lpfnWndProc,        dd WndProc
      at WNDCLASSEX.cbClsExtra,         dd 0
      at WNDCLASSEX.cbWndExtra,         dd 0
      at WNDCLASSEX.hInstance,          dd NULL
      at WNDCLASSEX.hIcon,              dd NULL
      at WNDCLASSEX.hCursor,            dd NULL
      at WNDCLASSEX.hbrBackground,      dd NULL
      at WNDCLASSEX.lpszMenuName,       dd NULL
      at WNDCLASSEX.lpszClassName,      dd szWndClassName
      at WNDCLASSEX.hIconSm,            dd NULL
    iend

systemTime:                    ; system time structure:
    istruc SYSTEMTIME
    iend

paintStructure:                ; for painting
  istruc PAINTSTRUCT
  iend

bitmapStruct:                  ; also for painting
  istruc BITMAP
  iend

rectangle:                     ; for retrieving window coordinations
  istruc RECT
  iend

;=======================================================================

; CODE SEGMENT:

[section .code use32 class=CODE]

  ; the program starts here:
  prologue                    ; initial instructions

  invoke GetSystemTime,systemTime   ; get the system time

  mov eax,[systemTime + SYSTEMTIME.wMilliseconds]
  shl eax,16
  mov eax,[systemTime + SYSTEMTIME.wSecond]
  mov [randomValue],eax       ; set the seed for random number generator

  mov esi,szWndClassName
  invoke GetModuleHandle,NULL                  ; get module handle
  mov [hInstance],eax                          ; eax <- module handle
  mov [WndClass + WNDCLASSEX.hInstance],eax    ; WndClass.hInstance <- module handle

  ; set the cursor for the window class

  invoke LoadCursor,NULL,IDC_ARROW
  mov [WndClass + WNDCLASSEX.hCursor],eax

  ; load images:

  invoke LoadImage,[hInstance],pathBitmapMine,IMAGE_BITMAP,0,0,16
  mov [hBitmapMine],eax
  invoke LoadImage,[hInstance],pathBitmapSquareHidden,IMAGE_BITMAP,0,0,16
  mov [hBitmapSquareHidden],eax
  invoke LoadImage,[hInstance],pathBitmapSquareUnknown,IMAGE_BITMAP,0,0,16
  mov [hBitmapSquareUnknown],eax
  invoke LoadImage,[hInstance],pathBitmapSquareMine1,IMAGE_BITMAP,0,0,16
  mov [hBitmapSquareMine1],eax
  invoke LoadImage,[hInstance],pathBitmapSquareMine2,IMAGE_BITMAP,0,0,16
  mov [hBitmapSquareMine2],eax
  invoke LoadImage,[hInstance],pathBitmapSquareRevealed0,IMAGE_BITMAP,0,0,16
  mov [hBitmapSquareRevealed],eax
  invoke LoadImage,[hInstance],pathBitmapSquareRevealed1,IMAGE_BITMAP,0,0,16
  mov [hBitmapSquareRevealed + 4],eax
  invoke LoadImage,[hInstance],pathBitmapSquareRevealed2,IMAGE_BITMAP,0,0,16
  mov [hBitmapSquareRevealed + 8],eax
  invoke LoadImage,[hInstance],pathBitmapSquareRevealed3,IMAGE_BITMAP,0,0,16
  mov [hBitmapSquareRevealed + 12],eax
  invoke LoadImage,[hInstance],pathBitmapSquareRevealed4,IMAGE_BITMAP,0,0,16
  mov [hBitmapSquareRevealed + 16],eax
  invoke LoadImage,[hInstance],pathBitmapSquareRevealed5,IMAGE_BITMAP,0,0,16
  mov [hBitmapSquareRevealed + 20],eax
  invoke LoadImage,[hInstance],pathBitmapSquareRevealed6,IMAGE_BITMAP,0,0,16
  mov [hBitmapSquareRevealed + 24],eax
  invoke LoadImage,[hInstance],pathBitmapSquareRevealed7,IMAGE_BITMAP,0,0,16
  mov [hBitmapSquareRevealed + 28],eax
  invoke LoadImage,[hInstance],pathBitmapSquareRevealed8,IMAGE_BITMAP,0,0,16
  mov [hBitmapSquareRevealed + 32],eax

  invoke LoadMenu,[hInstance],menuName         ; load the menu
  mov [hMenu],eax

  invoke LoadIcon,NULL,32518                   ; load the icon (standard Windows shield)
  mov [hIcon],eax
  mov [WndClass + WNDCLASSEX.hIcon],eax

  invoke RegisterClassEx,WndClass              ; register window class
                                               ; create the window:
  invoke CreateWindowEx,0,szWndClassName,szWndCaption,WS_OVERLAPPED + WS_CAPTION + WS_SYSMENU + WS_MINIMIZEBOX,50,50,200,200,NULL,[hMenu],[hInstance],NULL

  mov [hWnd],eax                               ; eax <- window handle
  invoke ShowWindow,eax,SW_SHOWDEFAULT         ; show the window
  invoke UpdateWindow,[hWnd]                   ; redraw the window




  call newGame,1
  invoke GetWindowRect,[hWnd],rectangle
  invoke MoveWindow,[hWnd],[rectangle + RECT.left],[rectangle + RECT.top],[windowWidth],[windowHeight],1


  call updateMineString                  ; update the mine string

  invoke SetTimer,[hWnd],1,1000,NULL  ; set the timer
  mov [timerId],eax

;----------------------------
; Windows message loop:
messageLoop:
  invoke GetMessage,message,NULL,0,0  ; get the message

  cmp eax,0                           ; if zero was returned, go to end
  je .Finish

  invoke TranslateMessage,message     ; translate the message
  invoke DispatchMessage,message      ; send the message to processing

  jmp messageLoop                     ; loop
;----------------------------

.Finish:                              ; end of program

  ; free the resources:

  invoke DeleteObject,[hBitmapMine]
  invoke DeleteObject,[hBitmapSquareHidden]
  invoke DeleteObject,[hBitmapSquareUnknown]
  invoke DeleteObject,[hBitmapSquareMine1]
  invoke DeleteObject,[hBitmapSquareMine2]
  invoke DeleteObject,[hBitmapSquareRevealed]
  invoke DeleteObject,[hBitmapSquareRevealed + 1]
  invoke DeleteObject,[hBitmapSquareRevealed + 2]
  invoke DeleteObject,[hBitmapSquareRevealed + 3]
  invoke DeleteObject,[hBitmapSquareRevealed + 4]
  invoke DeleteObject,[hBitmapSquareRevealed + 5]
  invoke DeleteObject,[hBitmapSquareRevealed + 6]
  invoke DeleteObject,[hBitmapSquareRevealed + 7]
  invoke DeleteObject,[hBitmapSquareRevealed + 8]
  invoke KillTimer,[hWnd],[timerId]

  epilogue                            ; final instructions

;-----------------------------------------------------------------------

  function WndProc,hWnd,wMsg,wParam,lParam

  ; This function processes window messages.
  ; changes: eax, ebx, ecx

  begin

  cmp dword [wMsg],WM_PAINT      ; repaint the window
  je paintWindow

  cmp dword [wMsg],WM_LBUTTONUP
  je leftClick                   ; left button click

  cmp dword [wMsg],WM_RBUTTONUP
  je rightClick                  ; right button click

  cmp dword [wMsg],WM_TIMER      ; timer notification
  je timerEvent

  cmp dword [wMsg],WM_CLOSE      ; close the window
  je closeProgram

  cmp dword [wMsg],WM_DESTROY    ; the window is being destroyed
  je closeProgram

  cmp dword [wMsg], WM_COMMAND   ; menu item clicked
  je menuItem

  ; other message - pass to default processing function:
  invoke DefWindowProc,[hWnd],[wMsg],[wParam],[lParam]
  return eax      ; return the DefWindowProc return value

menuItem:

  mov eax,dword [wParam]
  cmp eax,1       ; MENU_NEW_GAME
  jne menuNext1

  invoke MessageBox,NULL,textNewGame,textMessage,MB_YESNOCANCEL ; display the game difficulty dialog

  cmp eax,6       ; "yes" selected
  jne menuSkip
  call newGame,0  ; easy game
  invoke GetWindowRect,[hWnd],rectangle
  invoke MoveWindow,[hWnd],[rectangle + RECT.left],[rectangle + RECT.top],[windowWidth],[windowHeight],1
  invoke InvalidateRect,[hWnd],NULL,1   ; repaint
menuSkip:
  cmp eax,7       ; "no" selected
  jne menuSkip2
  call newGame,1  ; hard game
  invoke GetWindowRect,[hWnd],rectangle
  invoke MoveWindow,[hWnd],[rectangle + RECT.left],[rectangle + RECT.top],[windowWidth],[windowHeight],1
  invoke InvalidateRect,[hWnd],NULL,1   ; repaint
menuSkip2:
  return 0

menuNext1:
  cmp eax,2       ; MENU_EXIT
  jne menuNext2
  jmp closeProgram

menuNext2:
  cmp eax,4       ; MENU_ABOUT
  jne menuNext3
  invoke MessageBox,NULL,textAbout,textMessage,MB_OK
  return 0;

menuNext3:
  cmp eax,3       ; MENU_HELP
  jne menuNext4
  invoke MessageBox,NULL,textHelp,textMessage,MB_OK
  return 0;

menuNext4:
  return 0

timerEvent:
  mov eax,[timerCount]  ; increment the time count
  inc eax
  mov [timerCount],eax

  call updateTimeString

  mov eax,[gameTime]    ; increment game time (if not 0xFFFF or the game is over)

  cmp eax,0xFFFFFFFF    ; 0xFFFFFFFF = stopped
  je dontIncrement
  mov bl,[gameOver]
  cmp bl,1
  je dontIncrement
  inc eax
  mov [gameTime],eax
dontIncrement:
  invoke InvalidateRect,[hWnd],NULL,1   ; repaint
  return 0

leftClick:
  mov al,[gameOver]                     ; don't handle clicks if the game is over
  cmp al,1
  jne leftClickContinue
  return 0

leftClickContinue:
  call mouseToMap,[lParam]
  call handleLeftClick,eax
  invoke InvalidateRect,[hWnd],NULL,1   ; repaint
  return 0

rightClick:
  mov al,[gameOver]                     ; don't handle clicks if the game is over
  cmp al,1
  jne rightClickContinue
  return 0

rightClickContinue:

  call mouseToMap,[lParam]
  call handleRightClick,eax
  invoke InvalidateRect,[hWnd],NULL,1   ; repaint
  call updateMineString                 ; update the mine count string
  return 0

paintWindow:                         ; repaint the window

  invoke BeginPaint,[hWnd],paintStructure
  mov [hHDC],eax                     ; save the device context
  invoke CreateCompatibleDC,[hHDC]   ; create the memory DC
  mov [hMHDC],eax                    ; save its handle
  invoke SelectObject,[hMHDC],[hBitmapMine]  ; get the old bitmap handle
  mov [hBitmapOld],eax                       ; save the old bitmap handle

  invoke Rectangle,[hHDC],0,0,[windowWidth],INFO_BAR_HEIGHT   ; draw the upper info bar

  invoke TextOut,[hHDC],5,5,textTime,lenof.textTime  ; draw the text on the bar
  invoke TextOut,[hHDC],50,5,timeString,lenof.timeString
  invoke TextOut,[hHDC],150,5,textMines,lenof.textMines
  invoke TextOut,[hHDC],230,5,mineString,lenof.mineString

  ;----------------         draw loop

  mov [helperX],byte -1     ; helperX := -1
  mov [helperY],byte -1     ; helperY := -1

drawLoopY:
  mov al,[helperY]
  inc al
  mov [helperY],al          ; helperY++

drawLoopX:                  ; draw the map in loop
  mov al,[helperX]
  inc al
  mov [helperX],al          ; helperX++

  mov bl,[helperX]
  mov bh,[helperY]
  call selectBitmapForSquare,ebx

  mov [hBitmapHelper],eax

  invoke SelectObject,[hMHDC],[hBitmapHelper]  ; set the bitmap
  invoke GetObject,[hBitmapHelper],BITMAP_size,bitmapStruct ; get the information about the bitmap

  mov eax,0
  mov al,[helperX]
  mul word [bitmapStruct + BITMAP.bmWidth]
  mov ecx,eax                               ; ecx := x * bitmap width
  mov eax,0
  mov al,[helperY]
  mul word [bitmapStruct + BITMAP.bmHeight]
  add eax,INFO_BAR_HEIGHT                   ; eax := y * bitmap width + INFO_BAR_HEIGHT

  mov [bitmapStruct + BITMAP.bmWidth],word 43    ; set the resolution manually, GetObject doesn't work on some systems
  mov [bitmapStruct + BITMAP.bmHeight],word 43

  invoke BitBlt,[hHDC],ecx,eax,[bitmapStruct + BITMAP.bmWidth],[bitmapStruct + BITMAP.bmHeight],[hMHDC],0,0,SRCCOPY ; draw

  mov al,[helperX]
  cmp al,[mapWidth]
  jne drawLoopX             ; helperX == 0 ?

  mov al,byte -1
  mov [helperX],al          ; reset the column count

  mov al,[helperY]
  cmp al,[mapHeight]
  jne drawLoopY             ; helperY == 0 ?

  ;----------------

  invoke SelectObject,[hMHDC],[hBitmapOld]     ; set the old bitmap back
  invoke DeleteDC,[hMHDC]                      ; free the memory DC
  invoke EndPaint,[hWnd],paintStructure

  return 0


closeProgram:

  invoke PostQuitMessage,0       ; send the message to quit the process
  return 0

  end
;-----------------------------------------------------------------------

  function indexOffset2D,xIndex,yIndex,arrayWidth,arrayHeight

  ; Converts 2D array index [xIndex,yIndex] to a single index for an
  ; array of arrayWidth times arrayHeight saved by rows. The index is
  ; returned in eax. All operands are 32 bit, but only lower 16 bits
  ; are are considered, all numbers are unsigned.
  ; changes: eax, ebx

  begin

  mov eax,[yIndex]
  mov ebx,[arrayWidth]
  mul bx                 ; bx <- yIndex * arrayWidth
  shl eax,16             ; set 16 high bits of eax to zero
  shr eax,16
  add eax,[xIndex]       ; bx <- bx + xIndex

  return eax
  end

;-----------------------------------------------------------------------

  function placeMines,numberOfMines,arrayAddress,arrayLength

  ; This function fills given array of bytes with given length with
  ; zeroes, then places numberOfMines mines randomly within the array
  ; and initialises all squares (bytes) to be hidden. Number of mines
  ; must be <= array length.
  ; changes: eax, ebx, ecx, edx

  begin

  mov ecx,[arrayLength]
  mov eax,[arrayAddress]

arrayLoop:            ; fill the array with 01000000b (0 mines, hidden)
  mov [eax],byte 01000000b
  inc eax                   ; next index
  loop arrayLoop            ; loop


  mov ecx,0                 ; number of mines places

mineLoop:                   ; place mines randomly
  call randomNumber         ; get random number
  mov edx,0

  div dword [arrayLength]   ; edx := radnom mod arrayLength

  mov eax,[arrayAddress]
  add eax,edx               ; eax := address of mapArray[edx]
  mov bl,byte [eax]         ; bl := mapArray[edx]

  cmp bl,01001111b          ; bl = 01000111b ? (is there a mine?)

  je mineLoop               ; mine already there => generate another

                            ; no mine here => place it
  mov [eax],byte 01001111b  ; mine + hidden square
  inc ecx                   ; increment number of mines placed

  cmp ecx,[numberOfMines]   ; have we placed all mines already?
  jne mineLoop              ; if not, then loop again

  return eax
  end

;-----------------------------------------------------------------------

  function countMines,addressOfArray,WidthOfArray,HeightOfArray

  ; The function counts number of mines in 8 square neighbourhood for
  ; each empty (containing no mine) square in given two-dimensional
  ; array and saves the value to that square.
  ; changes: eax, ebx, ecx, edx

  begin

  mov eax,[HeightOfArray]
  mov [helperB],al      ; y count

mineCountLoopY:   ; for each array line

  mov al,[helperB]        ; y count --
  dec al
  mov [helperB],al

  mov eax,[WidthOfArray]
  mov [helperA],al      ; x count

mineCountLoopX:   ; for each array column

  mov al,[helperA]        ; x count --
  dec al
  mov [helperA],al

  mov [helper],byte 0   ; number of mines
  mov [helperC],byte 8  ; check the 8-square neighbourhood for mines

mineCountLoopN:
  mov al,[helperC]        ; neighbourhood count --
  dec al
  mov [helperC],al

  mov al,[helperC]        ; al := neighbourhood count
  mov ah,2
  mul ah                  ; multiply by 2 because we store by 2 bytes
  mov edx,0
  mov dl,al
  mov ax,word [neighbourhood + edx]   ; get the neighbourhood offset

  add al,[helperA]           ; add the offset to current position
  add ah,[helperB]

  mov [helperX],al           ; helperX := x + offset_x
  mov [helperY],ah           ; helperY := y + offset_y

  mov ebx,0                  ; expand to dword in eax and ebx
  mov bl,[helperY]
  mov eax,0
  mov al,[helperX]

  call checkRange,eax,ebx    ; check the array range

  cmp eax,0  ; outside of the array => don't check
  je cmSkip

  mov eax,0                  ; expand to dword in eax and ebx
  mov al,[helperX]
  mov ebx,0
  mov bl,[helperY]

  call indexOffset2D,eax,ebx,[mapWidth],[mapHeight]

  mov al,[mapArray + eax] ; al = mapArray[helperX][helperY]

  and al,00001111b        ; mask out the lower 4 bits
  cmp al,00001111b        ; is there a mine?
  jne cmSkip

  mov al,[helper]         ; number of mines ++
  inc al
  mov [helper],al

cmSkip:

  mov al,[helperC]        ; al := neighbourhood count
  cmp al,0
  jne mineCountLoopN      ; loop in neighbourhood search

                          ; write the number of mines to the square
  mov eax,0               ; expand the coordinations to dword
  mov ebx,0
  mov al,[helperA]
  mov bl,[helperB]

  call indexOffset2D,eax,ebx,[mapWidth],[mapHeight]

  mov cl,[mapArray + eax] ; cl := mapArray[x][y]
  mov ch,cl
  and ch,00001111b
  cmp ch,00001111b
  je cmDontWrite          ; mine on the square => don't write here
  and cl,11110000b
  or  cl,[helper]
  mov [mapArray + eax],cl

cmDontWrite:

  mov al,[helperA]        ; x count == 0 ?
  cmp al,0
  jne mineCountLoopX      ; loop in x

  mov al,[helperB]        ; y count == 0 ?
  cmp al,0
  jne mineCountLoopY      ; loop in y

  return eax
  end

;-----------------------------------------------------------------------

  function handleLeftClick,coordinationsL

  ; Handles left click on a map square, the coordinations are expected
  ; in format: bits [0:7] - x square coordination, bits [8 - 15] - y
  ; square coordination, other bits are ignored
  ; changes: eax, ebx, ecx, edx

  begin

  mov eax,[coordinationsL]

  mov ebx,0
  mov ecx,0

  mov bl,al
  mov cl,ah

  call checkRange,ebx,ecx
  cmp eax,0
  je hlcNotInRange

  call indexOffset2D,ebx,ecx,[mapWidth],[mapHeight]

  mov bl,[mapArray + eax]   ; bl := mapArray[x][y]
  mov bh,bl                 ; bh := bl

  and bh,11000000b
  cmp bh,01000000b          ; is the square hidden?
  jne hlcNotInRange

  and bl,00111111b          ; set the square to "revealed"
  mov [mapArray + eax],bl   ; write the value back

  push ebx                  ; keep the ebx content, the next function messes it up

  call checkVictory         ; check the victory
  cmp eax,1
  jne hlcNoWin
                            ; victory here
  mov [gameOver],byte 1     ; end the game
  invoke MessageBox,NULL,textWon,textMessage,NULL    ; display lose message

hlcNoWin:

  pop ebx

  and bl,00001111b          ; if the square has mine count of 0
  cmp bl,0
  jne hlcNext1
                            ; reveal all neighbour squares automatically
  push dword 8              ; 8 square neighbourhood count on stack
hlcLoop:
  pop eax
  dec eax
  push eax                  ; neighbourhood count --

  mov eax,[coordinationsL]
  mov ecx,0
  mov ebx,0
  mov cl,al                 ; cl := current x
  mov ch,ah                 ; ch := current y

  mov ebx,[coordinationsL]  ; bl := current x; bh := current y
  pop eax                   ; eax := neighbourhood count
  push eax                  ; keep the value on the stack
  mov edx,2
  mul dl                    ; al := neighbourhood count * 2 (array stored by 2 bytes)
  mov edx,[neighbourhood + eax] ; edx := neighbourhood[neighbourhood count]
  add dl,bl                 ; add neighbourhood offset for x
  add dh,bh                 ; add neighbourhood offset for y
                            ; now in edx are modified coordinations (with neighbourhood offset added)
  mov eax,0                 ; expand the coordinations to dword
  mov al,dl
  mov ebx,0
  mov bl,dh

  call checkRange,eax,ebx
  cmp eax,0
  je hlcSkip

  call handleLeftClick,edx  ; recursively call the function to reveal all neighbourhood squares

hlcSkip:

  pop eax
  push eax
  cmp eax,0
  jne hlcLoop

  pop eax
  jmp hlcNext2

hlcNext1:

  cmp bl,00001111b          ; is there a mine on the clicked square?
  jne hlcNext2
                            ; game over here:
  mov [gameOver],byte 1     ; end the game
  call revealAll            ; reveal all squares
  invoke PlaySound,pathSoundExplosion,NULL,SND_FILENAME + SND_ASYNC ; play the explosion sound
  invoke MessageBox,NULL,textLost,textMessage,NULL    ; display lose message

hlcNext2:

  mov eax,[gameTime]
  cmp eax,0xFFFFFFFF
  jne hlcNotInRange

  mov [gameTime],word 0     ; first move done => start the time count

hlcNotInRange:

  return eax

  end

;-----------------------------------------------------------------------

  function handleRightClick,coordinationsR

  ; Handles right click on a map square, the coordinations are expected
  ; in format: bits [0:7] - x square coordination, bits [8 - 15] - y
  ; square coordination, other bits are ignored
  ; changes: eax, ebx, ecx

  begin

  mov eax,[coordinationsR]

  mov ebx,0
  mov ecx,0

  mov bl,al
  mov cl,ah

  call checkRange,ebx,ecx
  cmp eax,0
  je hrcEnd

  call indexOffset2D,ebx,ecx,[mapWidth],[mapHeight]

  mov bl,[mapArray + eax]   ; bl := mapArray[x][y]
  mov bh,bl                 ; bh := bl

  and bl,11000000b          ; mask out the highest 2 bits

  cmp bl,00000000b      ; revealed?
  je hrcEnd2
  cmp bl,01000000b      ; hidden?
  jne hrcNext1

  mov ecx,[mineCount]   ; update the mine counter
  cmp ecx,0
  je hrcEnd
  dec ecx
  mov [mineCount],ecx

  and bh,00111111b
  or  bh,10000000b      ; mark the square with mine

  jmp hrcEnd
hrcNext1:
  cmp bl,10000000b      ; marked as mine?
  jne hrcNext2
  and bh,00111111b
  or  bh,11000000b      ; mark the square with question mark

  mov ecx,[mineCount]   ; update the mine counter
  inc ecx
  mov [mineCount],ecx

  jmp hrcEnd
hrcNext2:
  and bh,00111111b      ; marked with question mark here
  or  bh,01000000b      ; unmark the square
hrcEnd:

  mov [mapArray + eax],bh ; write the value back
hrcEnd2:

  return eax
  end

;-----------------------------------------------------------------------

  function randomNumber

  ; Returns a random combination of zeroes and ones in eax. The value
  ; depends on the number in seed.
  ; changes: eax, edx

  begin

  ; congruent generator:

  mov eax,[randomValue]
  mov edx,413             ; some random number
  mul edx
  add eax,8944393         ; another random value

  mov [randomValue],eax

  return eax

  end

;-----------------------------------------------------------------------

  function mouseToMap,mouseCoordinations

  ; Converts mouse coordinations to game map coordinations, the result
  ; is returned in format: al = map x, ah = map y, the mouse
  ; coordinations are expected in format: lower 16 bits - mouse x,
  ; upper 16 bits - mouse y
  ; changes: eax, ebx, ecx

  begin

  mov ebx,0
  mov ecx,43            ; tile resolution

  mov eax,[mouseCoordinations]

  div cl
  mov bl,al             ; bl := mouse_x / 43

  shr eax,16
  cmp eax,INFO_BAR_HEIGHT
  jl mtmSkip

  sub eax,INFO_BAR_HEIGHT
  div cl
  mov bh,al             ; bh := (mouse_y - INFO_BAR_HEIGHT) / 43
  jmp mtmSkip2

mtmSkip:
  mov bh,0
mtmSkip2:

  mov eax,ebx

  return eax

  end

;-----------------------------------------------------------------------

  function selectBitmapForSquare,squareCoords

  ; Returns (in eax) a bitmap handle representing given map square, the
  ; coordinations are expected in format: bits [0:7] - x coordination of
  ; the square, bits [8:15] - y coordinations of the square, other bits
  ; are ignored.
  ; changes: eax, ebx, ecx

  begin

  mov eax,[squareCoords]

  mov ebx,0
  mov ecx,0

  mov bl,al   ; bl := x
  mov cl,ah   ; cl := y

  call indexOffset2D,ebx,ecx,[mapWidth],[mapHeight]

  mov bl,[mapArray + eax]   ; bl := mapArray[x][y]

  mov eax,ebx

  and al,11000000b
  cmp al,01000000b                 ; hidden? (01)
  jne sbfsNext1
  mov eax,[hBitmapSquareHidden]
  jmp sbfsEnd
sbfsNext1:
  cmp al,10000000b                 ; marked as mine? (10)
  jne sbfsNext2
  mov ecx,[timerCount]             ; decide the animation frame
  and cl,00000001b
  cmp cl,0
  jne sbfsFrame2
  mov eax,[hBitmapSquareMine1]
  jmp sbfsEnd
sbfsFrame2:
  mov eax,[hBitmapSquareMine2]
  jmp sbfsEnd
sbfsNext2:
  cmp al,11000000b                 ; marked with question mark? (11)
  jne sbfsNext3
  mov eax,[hBitmapSquareUnknown]
  jmp sbfsEnd
sbfsNext3:                         ; revealed (00)
  and bl,00001111b                 ; see the number of mines

  cmp bl,00001111b                 ; mine?
  je sbfsMine

  mov eax,0
  mov ecx,4                        ; to multiply by 4 bytes
  mov al,bl
  mul cl                           ; eax := mines * 4 (address offset)
  mov eax,[hBitmapSquareRevealed + eax]

  jmp sbfsEnd

sbfsMine:
  mov eax,[hBitmapMine]

sbfsEnd:

  return eax

  end

;-----------------------------------------------------------------------

  function checkRange,coordinationX,coordinationY

  ; Checks if the specified coordinations are in the range of the map
  ; array, i.e. x in <0,mapWidth> and y in <0,mapHeight>, the result
  ; is returned in eax (1 = the coordinations are in range, 0
  ; otherwise).
  ; changes: eax

  begin

  mov eax,[coordinationX]
  cmp eax,[mapWidth]        ; coordinationX > mapWidth?
  jge crNotInRange
  mov eax,[coordinationY]
  cmp eax,[mapHeight]       ; coordinationY > mapHeight?
  jge crNotInRange

  mov eax,1                 ; coordinations in range here

  return eax

crNotInRange:
  mov eax,0
  return eax

  end

;-----------------------------------------------------------------------

  function updateTimeString

  ; Updates the global time string depending on global timer count
  ; variable.
  ; changes: eax, ecx

  begin

  mov eax,[gameTime]

  cmp eax,0xFFFFFFFF
  je utsEnd            ; the timer is stopped, don't update

  mov ecx,60           ; seconds in minute
  div cl               ; al := minutes ; ah := seconds
  mov ebx,eax

  mov ecx,10           ; for dividing by 10
  mov ah,0             ; ax := minutes
  div cl               ; al := minutes div 10; ah := minutes mod 10

  add al,'0'           ; make the numbers characters
  add ah,'0'

  mov [timeString + 1],ah
  mov [timeString],al

  mov ax,bx            ; now handle seconds
  shr ax,8
  div cl               ; al := seconds div 10; ah := seconds mod 10

  add al,'0'           ; make the numbers characters
  add ah,'0'

  mov [timeString + 4],ah
  mov [timeString + 3],al

utsEnd:

  return eax

  end

;-----------------------------------------------------------------------

  function updateMineString

  ; Updates the global mine count string depending on global mine count
  ; variable.
  ; changes: eax, ebx

  begin

  mov eax,[mineCount]

  mov ebx,10           ; to divide by 10
  div bl               ; al := mineCount div 10; ah := mineCount mod 10

  add al,'0'           ; make numbers character
  add ah,'0'           ; make numbers character

  cmp al,'0'           ; replace the possible leading zero with space
  jne umsSkip
  mov al,' '

umsSkip:
  mov [mineString + 1],ah  ; write the string
  mov [mineString],al

  return eax
  end

;-----------------------------------------------------------------------

  function revealAll

  ; Reveals all game map squares.
  ; changes: eax, ebx

  begin

  mov eax,[mapWidth]
  mov ebx,[mapHeight]
  mul al                  ; ax := mapWidth * mapHeight
  mov ebx,0
  mov bx,ax
  mov eax,ebx             ; eax = index

raLoop:                   ; mark all squares as revealed
  dec eax                 ; position--

  mov bl,[mapArray + eax] ; bl := mapArray[index]
  and bl,00111111b        ; mark as revealed
  mov [mapArray + eax],bl

  cmp eax,0
  jne raLoop

  return eax
  end

;-----------------------------------------------------------------------

  function checkVictory

  ; Checks if the game was won (i.e. all empty squares are revealed).
  ; The result is returned in eax as 1 (victory) or 0 (no victory).
  ; changes: eax, ebx

  begin

  mov eax,[mapWidth]
  mov ebx,[mapHeight]
  mul al                  ; ax := mapWidth * mapHeight
  mov ebx,0
  mov bx,ax
  mov eax,ebx             ; eax = index

cvLoop:                   ; check all squares
  dec eax                 ; position--

  mov bl,[mapArray + eax] ; bl := mapArray[index]
  mov bh,bl
  and bl,00001111b        ; mask out the lower 4 bits
  cmp bl,00001111b        ; is there a mine?
  je cvSkip               ; mine => continue
  and bh,11000000b        ; mask out the upper 2 bits
  cmp bh,0                ; is the square revealed?
  je cvSkip               ; if so, continue checking
  return 0                ; unrevealed square with a mine => no victory here
cvSkip:

  cmp eax,0
  jne cvLoop

  return 1                ; victory here
  end

;-----------------------------------------------------------------------

  function newGame,difficulty

  ; Initialises a new game with one of two difficulties (0 or 1). It
  ; sets up the map array and variables such as window size etc.
  ; changes: eax, ebx, ecx

  begin

  cmp [difficulty], dword 1   ; set the difficulty parameters
  je ngSkip
  mov [mineCount], dword 10   ; 0 = 9 x 9, 10 mines
  mov [mapWidth], dword 9
  mov [mapHeight], dword 9
  jmp ngSkip2
ngSkip:
  mov [mineCount], dword 30   ; 1 = 12 x 12, 30 mines
  mov [mapWidth], dword 12
  mov [mapHeight], dword 12
  jmp ngSkip2
ngSkip2:

  mov eax,[mapWidth]
  mov ebx,[mapHeight]
  mul bx                      ; eax := mapWidth * mapHeight

  call placeMines,[mineCount],mapArray,eax   ; initialise the map
  call countMines,mapArray,[mapWidth],[mapHeight]

  mov [gameOver],byte 0
  mov [gameTime],dword 0
  call updateTimeString                      ; redraw the timer string
  call updateMineString                      ; redraw the mine string
  mov [gameTime],dword 0xFFFFFFFF            ; reset the timer

                         ; set the window size variables:
  mov ecx,43             ; tile size in pixels
  mov eax,[mapWidth]
  mul cx
  mov [windowWidth],eax
  mov eax,[mapHeight]
  mul cx
  add eax,INFO_BAR_HEIGHT                ; add the info bar height to total height
  mov ebx,eax                            ; ebx := total height

  invoke GetSystemMetrics,SM_CYCAPTION   ; get the title bar size

  add ebx,eax                            ; and add it to height

  invoke GetSystemMetrics,SM_CYMENU      ; get the menu height

  add ebx,eax                            ; and add it to height

  mov [windowHeight],ebx



  return eax
  end

;-----------------------------------------------------------------------
