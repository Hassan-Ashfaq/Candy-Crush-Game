; *********************************************************************************************
; Created by:
; M.Abdullah Bilal (19I-0616)
; Hassan Ashfaq (19I-1708)
; AI-K
; *********************************************************************************************

; *********************************************************************************************
; CLEARS SCREEN TO DEFAULT COLOR

SCREENSETUP MACRO
    MOV AH, 00H ; Display Mode Function
    MOV AL, 014 ; To select screen mode
    INT 10H

    MOV AH, 0BH
    MOV BH, 00
    MOV BL, 08H
    INT 10H

ENDM

CLEARSCREEN MACRO

    MOV AH, 06
    MOV AL, 00
    MOV BH, 08H
    MOV CX, 0000H
    MOV DX, 0FFFFH
    INT 10H

    MOV AH, 0BH
    MOV BH, 00
    MOV BL, 08H
    INT 10H

ENDM

; *********************************************************************************************
; REQUIRES THE MESSAGE AS A STRING ALONG WITH THE X AND Y COORDINATES FOR WHERE TO PRINT ON THE SCREEN.

MESSAGEDISPLAY MACRO MESSAGE, X8, Y8

    ; SET CURSOR POSITION
    MOV AH, 02 ; FOR MODE
    MOV BH, 00 ; PAGE NUMBER
    MOV DH, X8 ; POSITION OF CURSOR(X-AXIS)
    MOV DL, Y8 ; POSITION OF CURSOR(Y-AXIS)
    INT 10H
    
    ; MESSAGE DISPLAY
    MOV AH, 09H
    MOV DX, OFFSET MESSAGE
    INT 21H

ENDM


; *********************************************************************************************
; CREATES NEW FILE AND SAVES OUTPUT TO IT
FILECREATION MACRO FILENAME, OUTPUT, BYTES

    ; Creating File
    ;MOV AH, 3CH
    ;MOV CL, 00
    ;MOV DX, OFFSET FILENAME
    ;INT 21H
    ;MOV [FHANDLE], AX

    ; Loading file
    MOV DX, OFFSET FILENAME
    MOV AL, 02
    MOV AH, 3DH
    INT 21H
    MOV [FHANDLE], AX

    ; Moving pointer to end of file
    MOV CX, 0
    MOV AH, 42H           ; service routine to move file pointer.
    MOV AL, 02H           ; service routine to move pointer to the end of file.
    INT 21H

    ; Writing to file
    MOV CX, [BYTES] ; SPECIFYING HOW MANY BYTES TO WRITE TO FILE
    MOV BX, AX ; As AH is to be used and file handler was stored in AX. We save file handler in BX
    MOV DX, OFFSET OUTPUT
    MOV AH, 40H
    INT 21H

    ; CLOSING FILE
    MOV AH, 3EH
    INT 21H

ENDM

; *********************************************************************************************
; TAKES INPUT AND SAVES TO FILE
NAMEINPUT MACRO BUFFER, X8, Y8
MOV CX, 00H
MOV DI, 00H

INPUT:


    ; SET CURSOR POSITION FOR INPUT
    MOV AH, 02H ; FOR MODE
    MOV BH, 00H ; PAGE NUMBER
    MOV DH, X8 ; POSITION OF CURSOR(X-AXIS)
    MOV DL, Y8 ; POSITION OF CURSOR(Y-AXIS)
    INT 10H

    MOV AH, 08H ; Taking characters input without echo
    INT 21H
    
    CMP AL, 13D ; If Enter is pressed
    JE END_INPUT
    
    CMP CX, 20D ; Maximum 20 characters are allowed
    JE INPUTLIMIT
    
    MOV BUFFER[DI], AL  ; Storing character in string
    
    ; SET CURSOR POSITION FOR WRITING ON SCREEN
    MOV AH, 02H ; FOR MODE
    MOV BH, 00H ; PAGE NUMBER
    MOV DH, X8 ; POSITION OF CURSOR(X-AXIS)
    MOV DL, Y8 ; POSITION OF CURSOR(Y-AXIS)
    INT 10H

    MOV DL, BUFFER[DI]
    MOV AH, 02
    INT 21H
    
    INC [Y8] ; Incrementing column for printing next character
    INC DI ; Incrementing string index
    INC CX ; Incrementing count of characters
    
JMP INPUT

INPUTLIMIT:

    MOV [X8], 11  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 20 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY NAMELIMITMESSAGE, X8, Y8

    ; WAIT FOR ANY KEY BEFORE CONTINUING
    MOV AH,10H
    INT 16H

END_INPUT:
    MOV DL, 10D
    MOV BUFFER[DI], AL  ; Storing new line character in string
    MOV [BYTES], 20D
    FILECREATION FILENAME, BUFFER, BYTES
ENDM
; *********************************************************************************************
; CONTAINS ALL CODE REQUIRED ON THE INITIAL PAGE

INITPAGE MACRO

    CLEARSCREEN
    
    ; display game name
    MOV [X8], 2  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 14 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY GAMENAME, X8, Y8

    ; message for name input
    MOV [X8], 10  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 2 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY NAMEMESSAGE, X8, Y8

    ; created by message display
    MOV [X8], 18  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 2 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY CREATORMESSAGE, X8, Y8

    ; creator1 name display
    MOV [X8], 20  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 10 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY CREATOR1, X8, Y8

    ; creator2 name display
    MOV [X8], 22  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 10 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY CREATOR2, X8, Y8

    ; TAKING INPUT FROM USER
    MOV [X8], 10  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 14 ; POSITION OF CURSOR(Y-AXIS)
    NAMEINPUT NAMEVAL, X8, Y8

ENDM

; *********************************************************************************************
; CONTAINS ALL CODE REQUIRED ON THE RULES PAGE

DISPLAYRULES MACRO

    CLEARSCREEN
    
    MOV [X8], 2  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 2 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY RULESMESSAGE, X8, Y8

    MOV [X8], 6  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 2 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY RULE1, X8, Y8

    MOV [X8], 7  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 2 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY RULE11, X8, Y8

    MOV [X8], 9  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 2 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY RULE2, X8, Y8

    MOV [X8], 10  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 2 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY RULE21, X8, Y8

    MOV [X8], 12  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 2 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY RULE3, X8, Y8

    MOV [X8], 13  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 2 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY RULE31, X8, Y8

    MOV [X8], 20  ; POSITION OF CURSOR(X-AXIS)
    MOV [Y8], 6 ; POSITION OF CURSOR(Y-AXIS)
    MESSAGEDISPLAY FINALMESSAGE, X8, Y8

    ; WAIT FOR ANY KEY BEFORE CONTINUING
    MOV AH,10H
    INT 16H

ENDM

; *********************************************************************************************
; DRAWS BOARD
DRAWLVL MACRO LEVEL
    
    .IF(LEVEL == 1)
        mov si, offset Level_1_Grid
    .ELSEIF (LEVEL == 2)
        mov si, offset Level_2_Grid
    .ELSE
        mov si, offset Level_3_Grid
    .ENDIF
    MOV [shapes_vertical], 0
    MOV [shapes_horizontal], 0
    MOV [X], 75
    MOV [Y], 30
    .while shapes_vertical < 7
        .while shapes_horizontal < 7
            mov bl,[si]
            push x 
            push y
            .if bl == 0  ; BOMB
                Draw_Bomb x,y,dp1,dp2
            .elseif bl == 1
                Draw_Triangle x,y,dp1,dp2
            .elseif bl == 2
                Draw_Plus x,y,dp1,dp2
            .elseif bl == 3
                Draw_Square x,y,dp1,dp2
            .elseif bl == 4
                Draw_Inverse_Triangle x,y,dp1,dp2
            .elseif bl == 5
                Draw_T x,y,dp1,dp2
            .elseif bl == 6  ; Block Tile
                Draw_Block x,y,dp1,dp2
            .endif
            pop y 
            pop x
            mov dp1,0
            mov dp2,0
            add x,70
            inc si
            inc shapes_horizontal
        .endw
        mov shapes_horizontal,0
        mov x,75
        add y,20
        inc shapes_vertical
    .endw

    MOV [X8], 01D
    MOV [Y8], 11D
    MESSAGEDISPLAY NAMEVAL, X8, Y8

ENDM
; *********************************************************************************************
; MACRO SECTION ENDS

Grid_Level_1 Macro x, y
    CLEARSCREEN
    ; Drawing Horizontal Lines
    .while y<=170
        .while x<=565
            mov bh, 0
            mov cx, x 
            mov dx, y
            mov ah, 0Ch
            mov al, 01h
            int 10h
            inc x  ; Incrementing x to make pixel
        .endw
        mov x, 75  ; Moving X back to 10 to Draw Next Line
        add y, 20  ; To Seperate Each Horizontal Lines By 25px
    .endw
    
    mov x, 75
    mov y, 30
    
    ; Drawing Vertical Lines
    .while y<=170
        .while x<=630
            mov bh, 0
            mov cx, x
            mov dx, y
            mov ah, 0Ch
            mov al, 01h
            int 10h
            add x, 70  ; To Seperate Each Vertical Lines By 80px
        .endw
        mov x, 75 ; Moving X back to 80 to Draw Next Line
        inc y ; Incrementing Y1 to Draw a Vetical Line
    .endw
endm
;================================================================
;==============Draw Block====================
Draw_Block Macro X, Y, X_range, Y_range
    .while Y_range <= 20  
        push X
        push X_range
        ; Inner Loop till X_range < 20 (To Draw 20 Pixel)
        .while X_range <= 70 
            mov bh,0
            mov cx, X
            mov dx, Y
            mov al, 07h
            mov ah, 0ch
            int 10h
            inc X      
            inc X_range
        .endw
        inc Y_range
        pop X_range
        pop X
        inc Y
    .endw
endm
;===============Draw Bomb======================
Draw_Bomb Macro X, Y, X_range, Y_range
    push X
    push Y
    push X_range
    push Y_range
    Draw_Triangle X,Y,X_range,Y_range
    pop Y_range
    pop X_range
    pop Y
    pop X
    Draw_Inverse_Triangle X, Y, X_range, Y_range
endm
;============Draw Square===============
Draw_Square Macro X, Y, X_range, Y_range
    ; Adding In X & Y to Draw Shape in Center
    add X, 25
    add Y, 5
    ; Rendering Pixel By Pixel using C++ Logic ( Two For Loop )
    .while Y_range <= 10  ; Outer Loop till Y_range < 10 (To Draw 10 Pixel)
        push X
        push X_range
        .while X_range <= 20 ; Inner Loop till X_range < 20 (To Draw 20 Pixel)
            mov bh,0
            mov cx, X
            mov dx, Y
            mov al, 0Bh
            mov ah, 0ch
            int 10h
            inc X      
            inc X_range
        .endw
        inc Y_range
        pop X_range
        pop X
        inc Y
    .endw
ENDM

;==========Draw Triangle============
Draw_Triangle Macro X, Y, X_range, Y_range
    ; Adding In X & Y to Draw Shape in Center
    add X, 35
    add Y, 5
    ; Drawing Triangle Pixel by Pixel using C++ logic (Three For Loop)
    .while Y_range <=10 ; Outer Loop till Y_range < 10 (To Draw 10 Pixel)
        mov bx,Y_range
        mov X_range,bx
        ; Inner-I Loop till X_range > 0 (To Draw Right Side of Triangle Pixel by Pixel)
        .while X_range > 0 
            mov bh,0
            mov cx, X
            mov dx, Y
            mov al, 0Eh
            mov ah, 0ch
            int 10h
            inc X
            dec X_range
        .endw
        sub X,bx 
        mov bx,Y_range
        mov X_range, bx
        ; Inner-II Loop till X_range > 0 (To Draw Left Side of Triangle Pixel by Pixel)
        .while X_range > 0
            mov bh,0
            mov cx, X
            mov dx, Y
            mov al, 0Eh
            mov ah, 0ch
            int 10h
            dec X
            dec X_range
        .endw
        inc Y   ; Incrementing Y to Draw Simple Triangle
        add X,bx
        inc Y_range
    .endw
ENDM

;===========Draw Inverse Triangle===========
Draw_Inverse_Triangle Macro X, Y, X_range, Y_range
    ; Adding In X & Y to Draw Shape in Center
    add X,35
    add Y,15
    ; Drawing Triangle Pixel by Pixel using C++ logic (Three For Loop)
    .while Y_range <=10
        mov bx,Y_range
        mov X_range,bx
        ; Inner-I Loop till X_range > 0 (To Draw Right Side of Triangle Pixel by Pixel)
        .while X_range > 0
            mov bh,0
            mov cx, X
            mov dx, Y
            mov al, 04h
            mov ah, 0ch
            int 10h
            inc X
            dec X_range
        .endw
        sub X,bx
        mov bx,Y_range
        ; Inner-II Loop till X_range > 0 (To Draw Left Side of Triangle Pixel by Pixel)
        mov X_range,bx
        .while X_range > 0
            mov bh,0
            mov cx, X
            mov dx, Y
            mov al, 04h
            mov ah, 0ch
            int 10h
            dec X
            dec X_range
        .endw
        dec Y    ; Decrementing Y to Draw Inverse Triangle
        add X,bx
        inc Y_range
    .endw
ENDM

;=========Draw Plus=============
Draw_Plus Macro X, Y, X_range, Y_range
    ; Adding In X & Y to Draw Shape in Center
    add X,30
    add Y,3
    push X
    push Y
    push X_range
    push Y_range
    
    ; Drawing First part of Plus Sign Vertical Bar
    ; Drawing Pixel by Pixel C++ Logic (Two For Loop)
    .while Y_range <=14
        .while X_range <=8
            mov bh,0
            mov cx, X
            mov dx, Y
            mov al, 05h
            mov ah, 0ch
            int 10h
            inc X
            inc X_range
        .endw
        mov X_range,0
        inc Y
        sub X,9
        inc Y_range
    .endw

    pop Y_range
    pop X_range
    pop Y
    pop X
    sub X, 10
    add Y, 4

    ; Drawing Second part of Plus Sign Horizontal Bar
    ; Drawing Pixel by Pixel C++ Logic (Two For Loop)
    .while Y_range <=5
        .while X_range <=28
            mov bh,0
            mov cx, X
            mov dx, Y
            mov al, 05h
            mov ah, 0ch
            int 10h
            inc X
            inc X_range
        .endw
        mov X_range,0
        inc Y
        sub X,29
        inc Y_range
    .endw
ENDM

;=========Draw T=============
Draw_T Macro X, Y, X_range, Y_range
    add X,30
    add Y,3
    ; Putting Values to Stack for Later Use
    push X
    push Y
    push X_range
    push Y_range

    ; Drawing First part of T Sign Vertical Bar
    ; Drawing Pixel by Pixel C++ Logic (Two For Loop)
    .while Y_range <=14
        .while X_range <=8
            mov bh,0
            mov cx, X
            mov dx, Y
            mov al, 0Ch
            mov ah, 0ch
            int 10h
            inc X
            inc X_range
        .endw
        mov X_range,0
        inc Y
        sub X,9
        inc Y_range
    .endw

    ; Potting Values
    pop Y_range
    pop X_range
    pop Y
    pop X
    sub X, 10

    ; Drawing Second part of T Sign Horizontal Bar
    ; Drawing Pixel by Pixel C++ Logic (Two For Loop)
    .while Y_range <=5
        .while X_range <=28
            mov bh,0
            mov cx, X
            mov dx, Y
            mov al, 0Ch
            mov ah, 0ch
            int 10h
            inc X
            inc X_range
        .endw
        mov X_range,0
        inc Y
        sub X,29
        inc Y_range
    .endw
ENDM
;======================
; *********************************************************************************************
; CONTAINS CODE TO CALCULATE ADDRESS OF CANDY
CALCULATEADDRESS MACRO X, Y, INDEX

    MOV [INDEX], 00
    ; GETTING VALUE WITHOUT OFFSET(MAKING START OF GRID AS ORIGIN)
    SUB [X], 75
    SUB [Y], 30

    ; DIVIDING Y BY 20 (DISTANCE BETWEEN INDEXES)
    MOV AX, [Y]
    MOV BL, 20D
    DIV BL ; Y-AXIS ANSWER IN AL
    MOV BL, 7 ; EACH ROW HAS 7 CANDIES
    MUL BL ; ANSWER STORED IN AX
    ADD [INDEX], AX

    ; DIVIDING X BY 70 (DISTANCE BETWEEN INDEXES)
    MOV AX, [X]
    MOV BL, 70D
    DIV BL ; X-AXIS ANSWER IN AL
    MOV AH, 00
    ADD [INDEX], AX

ENDM

; *********************************************************************************************

; CONTAINS CODE TO MODIFY ARRAY.
MODIFYLVL1GRID MACRO INDEX1, INDEX2, LEVEL
    PUSH CX
    PUSH DX
    MOV SI, OFFSET LEVEL
    MOV DI, OFFSET LEVEL
    MOV BX, [INDEX1]
    ADD SI, BX ; CONTAINS ADDRESS OF INDEX1
    MOV BX, [INDEX2]
    ADD DI, BX ; CONTAINS ADDRESS OF INDEX2
    MOV CX, [INDEX1]
    INC CX
    MOV DL, [DI] ; MOVING VALUE IN GRID IN DX
    MOV AX, 0
    .IF(CX == INDEX2) ; CHECKING NEXT CHARACTER
        .IF(DL != 6) ; CHECKING IF NEXT POSITION IS A TILE
            MOV Valide_Check, 1
        .ENDIF
    .ENDIF
    SUB CX, 2
    .IF(CX == INDEX2) ; CHECKING PREVIOUS CHARACTER
        .IF(DL != 6) ; CHECKING IF BEFORE POSITION IS A TILE
            MOV Valide_Check, 1
        .ENDIF
    .ENDIF
    INC CX
    ADD CX, 7
    .IF(CX == INDEX2) ; CHECKING BELOW CHARACTER
        .IF(DL != 6) ; CHECKING IF BELOW POSITION IS A TILE
            MOV Valide_Check, 1
        .ENDIF
    .ENDIF
    SUB CX, 14
    .IF(CX == INDEX2) ; CHECKING ABOVE CHARACTER
        .IF(DL != 6) ; CHECKING IF ABOVE POSITION IS A TILE
            MOV Valide_Check, 1
        .ENDIF
    .ENDIF
    POP DX
    POP CX
ENDM
; *********************************************************************************************
; CODE TO CHECK WHETHER SWAP SHOULD BE MADE OR NOT
CHECKSWAP MACRO X1, Y1, X2, Y2
    
    MOV AX, 00 ; WILL BE USED AS FLAG
    MOV BX, [X1]
    ADD BX, 105D
    MOV CX, [X1]
    SUB CX, 105D
    .IF([X2] < BX && [X2] > CX)
        MOV AX, 01 ; SWAP
    .ENDIF
    MOV BX, [Y1]
    ADD BX, 30D
    MOV CX, [Y1]
    SUB CX, 30D
    .IF([Y2] < BX && [Y2] > CX)
        MOV AX, 01 ; SWAP
    .ENDIF

ENDM