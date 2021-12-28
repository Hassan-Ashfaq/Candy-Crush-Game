INCLUDE Project.INC

.MODEL SMALL
.STACK 100H

.DATA
; ******** SHARED DATA ************************************************************************
	X8 DB 0
	Y8 DB 0
	MOVES DW 0
; *********************************************************************************************

; ******** Data for start Page ****************************************************************
	GAMENAME DB 'CANDY CRUSH!!!$'
	CREATORMESSAGE DB 'CREATED BY:$'
	CREATOR1 DB 'Abdullah Bilal(19I-0616)$'
	CREATOR2 DB 'Hassan Ashfaq(19I-1708)$'
	NAMEMESSAGE DB 'INPUT NAME: $'
	NAMELIMITMESSAGE DB 'MAXIMUM CHARACTER LIMIT FOR NAME REACHED$'
	NAMEVAL DB 20 DUP(0), '$'
; *********************************************************************************************

; ******** Data for Rules Page ****************************************************************
	RULESMESSAGE DB 'Rules for the game:$'
	RULE1 DB '- Match 3 Candies in a row to crush$'
	RULE11 DB 'them.$'
	RULE2 DB '- Upon crushing candies, points$'
	RULE21 DB 'will be awarded.$'
	RULE3 DB '- If candies do not match, they$'
	RULE31 DB 'will not be crushed.$'
	FINALMESSAGE DB 'PRESS ANY KEY TO START GAME$'
; *********************************************************************************************

; ******** Data for File Handling *************************************************************
	FILENAME DB 'SCORES.txt', 0
	FHANDLE DW 0
	BYTES DW 0
; *********************************************************************************************
; ******** Data for Cursor Movement ***********************************************************
	X_coord1 DW 0
	Y_coord1 DW 0
	X_coord2 DW 0
	Y_coord2 DW 0
; *********************************************************************************************
; ******** Data for Swapping Candies **********************************************************
	INDEX1 DW 0
	INDEX2 DW 0
; *********************************************************************************************
; To Draw Grid
	x dw 75
	y dw 30
	; dp1 & dp2 help us to render shapes
	dp1 dw 0
	dp2 dw 0
	Move_Label db 'Move: $'
	Score_Label db 'Score: $'
	Level_1_Grid db 3,4,3,3,1,2,5 
           		db 3,4,2,5,4,4,1
            	db 1,1,4,5,2,1,4
           		db 2,5,0,3,1,2,4
           		db 5,4,4,1,5,4,2
           		db 3,1,3,5,2,2,4
           		db 2,3,3,1,2,4,5
	Level_1_Grid_copy db 3,4,3,3,1,2,5 
           		db 3,4,2,5,4,4,1
            	db 1,1,4,5,2,1,4
           		db 2,5,5,3,1,2,4
           		db 5,4,4,1,5,4,2
           		db 3,1,3,5,2,2,4
           		db 2,3,3,1,2,4,5
	Level_1_Score dw 0
	Level_1_Label db 'Level 1 $'

	Level_2_Grid db 6,1,3,6,1,3,6 
           		db 6,2,2,5,4,2,6
            	db 1,1,4,5,2,1,4
           		db 6,5,5,3,1,2,6
           		db 5,4,4,1,5,4,2
           		db 6,2,3,5,2,2,6
           		db 6,3,3,6,2,1,6
	Level_2_Grid_copy db 6,1,3,6,1,3,6 
           		db 6,2,2,5,4,2,6
            	db 1,1,4,5,2,1,4
           		db 6,5,5,3,1,2,6
           		db 5,4,4,1,5,4,2
           		db 6,2,3,5,2,2,6
           		db 6,3,3,6,2,1,6
	Level_2_Score dw 0
	Level_2_Label db 'Level 2 $'
				   
	Level_3_Grid db 3,1,3,6,1,3,3 
           	    db 2,3,1,6,3,2,1
            	db 1,1,4,6,2,1,4
           		db 6,6,6,6,6,6,6
           		db 2,4,4,6,5,1,2
           		db 4,2,3,6,2,2,5
           		db 3,2,3,6,2,1,2
	Level_3_Grid_copy db 3,1,3,6,1,3,3 
           	    db 2,3,1,6,3,2,1
            	db 1,1,4,6,2,1,4
           		db 6,6,6,6,6,6,6
           		db 2,4,4,6,5,1,2
           		db 4,2,3,6,2,2,5
           		db 3,2,3,6,2,1,2
	Level_3_Score dw 0
	Level_3_Label db 'Level 3 $'

	Invalid db 'Move Not Valid! $'
	Valid db 'Swap Done $' 
	IsSwaped db 2

	Counting_No_Pushed dw 0

    LEVEL DB 01
	shapes_horizontal dw 0
	shapes_vertical dw 0

	; Condition Checkers
	Valide_Check db 0
	Final_Check db 0
	
	; Helper to COrrect Swap
	temp db 0
	Next1 db 0
	Next2 db 0
; *********************************************************************************************
; ******** Data for Destroying Candies ********************************************************

	ARR DB 1,0,3,2,4,1,5,0,1,3,2,4,3,5,1 

; *********************************************************************************************

	temp_var dw 0
	level_alert db 'Level Passed!$'
	level_warning db 'Level Failed!$'
.CODE

MAIN PROC	
	MOV AX, @DATA
	MOV DS, AX

	SCREENSETUP
	INITPAGE
	DISPLAYRULES

	;=================LEVEL 1======================
	Level_1_Replay:
	call Level_1
	;=================Level-1 End Message===========
	CLEARSCREEN
	mov ax, Level_1_Score
	mov temp_var, ax
	call Level_Alert_Checker
	.if temp_var >0
		call Replay
		.if temp_var == 15
			CLEARSCREEN
			mov x, 75
			mov y, 30
			mov dp1, 0
			mov dp2, 0
			mov temp_var, 0
			mov Level_1_Score, 0

			mov si, offset Level_1_Grid
			mov di, offset Level_1_Grid_copy
			call Re_Fill_Board
			mov LEVEL, 1
			jmp Level_1_Replay
		.endif
	.endif
	mov temp_var, 0
	;======================================================

	Level_2_Replay:
	mov x, 75
	mov y, 30
	mov dp1, 0
	mov dp2, 0
	mov Valide_Check, 0
	mov Final_Check, 0
	call Level_2
	;=================Level-1 End Message===========
	CLEARSCREEN
	mov ax, Level_2_Score
	mov temp_var, ax
	call Level_Alert_Checker
	.if temp_var >0
		call Replay
		.if temp_var == 15
			CLEARSCREEN
			mov x, 75
			mov y, 30
			mov dp1, 0
			mov dp2, 0
			mov temp_var, 0
			mov Level_2_Score, 0

			mov si, offset Level_2_Grid
			mov di, offset Level_2_Grid_copy
			call Re_Fill_Board
			mov LEVEL, 2
			jmp Level_2_Replay
		.endif
	.endif
	mov temp_var, 0

	;======================================================
	Level_3_Replay:
	mov x, 75
	mov y, 30
	mov dp1, 0
	mov dp2, 0
	mov Valide_Check, 0
	mov Final_Check, 0
	call Level_3

	;=================Level-1 End Message===========
	CLEARSCREEN
	mov ax, Level_3_Score
	mov temp_var, ax
	call Level_Alert_Checker
	.if temp_var >0
		call Replay
		.if temp_var == 15
			CLEARSCREEN
			mov x, 75
			mov y, 30
			mov dp1, 0
			mov dp2, 0
			mov temp_var, 0
			mov Level_3_Score, 0
			mov si, offset Level_3_Grid
			mov di, offset Level_3_Grid_copy
			call Re_Fill_Board
			mov LEVEL, 3
			jmp Level_3_Replay
		.endif
	.endif
	mov temp_var, 0

	;======================================================
MAIN ENDP

Re_Fill_Board proc
	mov cx, 49
	L1:
		mov bl, [di]
		mov [si], bl
		inc si
		inc di
		loop L1
	ret
Re_Fill_Board endp

Replay proc
	mov ah,01h
    int 21h
    .if al == 13
        mov temp_var, 15  
    .elseif al == 27
        mov ah, 4CH
		int 21h
    .endif
	ret
Replay endp

Level_Alert_Checker proc
	.if temp_var>=80
		mov X8, 10
		mov Y8, 35
		MESSAGEDISPLAY level_alert, X8, Y8
	.elseif temp_var<80
		mov X8, 10
		mov Y8, 35
		MESSAGEDISPLAY level_warning, X8, Y8
	.endif
	ret
Level_Alert_Checker endp

Displaying_Score proc
    mov bl, 10
	L1:
        cmp al, 0
        je disp
        div bl
        mov cl, ah
        mov ch, 0
        push cx
        mov ah, 0
        inc Counting_No_Pushed
        jmp L1

    disp:
        cmp Counting_No_Pushed, 0
        je Return
        pop dx
        add dx, 48
        mov ah, 02h
        int 21h
        dec Counting_No_Pushed
        jmp disp
    Return:
        ret
Displaying_Score endp

Level_1 proc
	CLEARSCREEN
    MOV MOVES, 15 ; NUMBER OF MOVES IN THIS LEVEL
    push x 
    push y
    Grid_Level_1 x, y
    pop y 
    pop x
    
    MOV AX, 01H ; Displays the mouse cursor
    INT 33H

    MOV CX,00 ; Setting X-Axis Position of the mouse
    MOV DX,20 ; Setting Y-Axis Position of the mouse
    MOV AX, 04 ; Setting the start position of the mouse
    INT 33H

    DRAWLVL LEVEL
	call Level_1_Score_Printing

    L11:
    INPWAIT11:
        
        MOV AX, 05 ; GET BUTTON INFORMATION
        MOV BX, 00 ; BUTTON ID
        INT 33H

        TEST AX, 1 ; CHECKING IF LEFT BUTTON IS PRESSES, 1 FOR CHECKING IF YES, 0 FOR NO.
        JZ INPWAIT11 ; IF NOT PRESSES, WAIT.
        MOV X_coord1, CX ; IF PRESSED, GET COORDINATES.
        MOV Y_coord1, DX

        MOV CX,500

        lOOP11:
            PUSH CX
            MOV CX, 500
            LOOP21:
                DEC DX
                CMP DX,0
            LOOP LOOP21
            POP CX
        LOOP LOOP11

    INPWAIT21:
        MOV AX, 05 ; GET BUTTON INFORMATION
        MOV BX, 00 ; BUTTON ID
        INT 33H

        TEST AX, 1 ; CHECKING IF LEFT BUTTON IS PRESSES, 1 FOR CHECKING IF YES, 0 FOR NO.
        JZ INPWAIT21 ; IF NOT PRESSES, WAIT.
        MOV X_coord2, CX ; IF PRESSED, GET COORDINATES.
        MOV Y_coord2, DX

        CHECKSWAP X_coord1, Y_coord1, X_coord2, Y_coord2 ; RETURNS 1 IN AX IF SWAP IS NEEDED
        .IF(AX == 1)
            CLEARSCREEN ; CLEAR SCREEN
            CALCULATEADDRESS X_coord1, Y_coord1, INDEX1 ; CALCULATING ADDRESS IN ARRAY
            CALCULATEADDRESS X_coord2, Y_coord2, INDEX2 ; CALCULATING ADDRESS IN ARRAY
            ;===========BOMB=========================
                MOV SI, OFFSET Level_1_Grid
                MOV DI, OFFSET Level_1_Grid
                MOV BX, INDEX1
                ADD SI, BX ; CONTAINS ADDRESS OF INDEX1

                MOV BX,  INDEX2
                ADD DI, BX ; CONTAINS ADDRESS OF INDEX2
                MOV BL, [DI]
                MOV AL, [SI]

                .IF (AL == 0)
                    MOV BL, [DI]
                    MOV DI, OFFSET Level_1_Grid
                    MOV CX, 49
                    MOV SI, OFFSET ARR
                    BOMBLOOP1:
                        .IF([DI] == BL)
                            MOV AL, [SI]
                            MOV [DI], AL
                        .ENDIF
                    LOOP BOMBLOOP1
                .ELSEIF (BL == 0)
                    MOV BL, [SI]
                    MOV SI, OFFSET Level_1_Grid
                    MOV CX, 49
                    MOV SI, OFFSET ARR
                    BOMBLOOP2:
                        .IF([SI] == BL)
                            MOV AL, [DI]
                            MOV [SI], AL
                        .ENDIF
                    LOOP BOMBLOOP2
                .ENDIF
            MODIFYLVL1GRID INDEX1, INDEX2, Level_1_Grid ; MAKE CHANGES IN ARRAY.
            .if Valide_Check == 1
                call Validate_Move_Level_1
				.if Final_Check != 0 
					MOV SI, OFFSET Level_1_Grid
					MOV DI, OFFSET Level_1_Grid
					MOV BX, INDEX1
					ADD SI, BX ; CONTAINS ADDRESS OF INDEX1

					MOV BX,  INDEX2
					ADD DI, BX ; CONTAINS ADDRESS OF INDEX2

					MOV AL, [SI]
					MOV BL, [DI]
					MOV [SI], BL
					MOV [DI], AL

					PUSH AX
					PUSH BX
					PUSH CX
					PUSH DX
					PUSH SI
					PUSH DI

					MOV AH, 00h
   					INT 1AH

   					mov  ax, dx
				    xor  dx, dx
				    mov  cx, 13    
				    div  cx

				    MOV SI, OFFSET ARR
			    	MOV AL, AH
			    	MOV AH, 00
			    	ADD SI, AX
				    
				    .IF (Final_Check == 1)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	INC DI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC DI
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 2)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	DEC DI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	DEC DI
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 3)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	DEC DI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC DI
				    	INC DI
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 4)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	ADD DI, 7
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	ADD DI, 7
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 5)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	SUB DI, 7
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	SUB DI, 7
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 6)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	SUB DI, 7
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	ADD DI, 14
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ENDIF

				    POP DI
				    POP SI
				    POP DX
				    POP CX
				    POP BX
				    POP AX

					mov Valide_Check, 0
					mov Final_Check, 0
					add Level_1_Score, 10
					mov IsSwaped, 1
				.else
					mov IsSwaped, 0
				.endif
			.else
				mov IsSwaped, 0
            .endif
            DEC MOVES
            MOV [X], 75 ; RESETTING VALUES TO DRAW GRID.
            MOV [Y], 30
        .ENDIF
        MOV CX,500

        MOV AX, 02H ; HIDES the mouse cursor
        INT 33H
        
        Grid_Level_1 x, y ; DRAWING GRID
        DRAWLVL LEVEL ; DRAWING CANDIES.
		call Level_1_Score_Printing
		mov IsSwaped, 2
        MOV AX, 01H ; Displays the mouse cursor
        INT 33H

        CMP MOVES, 0
    JNE L11
    INC LEVEL
    MOV BYTES, 2
    FILECREATION FILENAME, Level_1_Score, BYTES
	ret
Level_1 endp

Level_1_Score_Printing proc
	.if IsSwaped==1
		mov X8, 22
		mov Y8, 10
		MESSAGEDISPLAY Valid, X8, Y8
	.elseif IsSwaped==0
		mov X8, 22
		mov Y8, 10
		MESSAGEDISPLAY Invalid, X8, Y8
	.endif 

	mov X8, 1
	mov Y8, 35
	MESSAGEDISPLAY Level_1_Label, X8, Y8

	mov X8, 1
	mov Y8, 65
	MESSAGEDISPLAY Score_Label, X8, Y8
	MOV AH,02H
	MOV BX,0
	MOV DH, 1 ;Printing Location Row
	MOV DL, 72 ;Printing Location Col
	INT 10H
	
	mov ax, 0
	mov bx, 0
	mov cx, 0
	mov dx, 0
	mov Counting_No_Pushed, 0

	mov ax, Level_1_Score
	call Displaying_Score

	mov X8, 22
	mov Y8, 35
	MESSAGEDISPLAY Move_Label, X8, Y8

	MOV AH,02H
	MOV BX,0
	MOV DH, 22 ;Printing Location Row
	MOV DL, 43 ;Printing Location Col
	INT 10H
	
	mov ax, 0
	mov bx, 0
	mov cx, 0
	mov dx, 0
	mov Counting_No_Pushed, 0

	mov ax, MOVES
	call Displaying_Score
	ret 
Level_1_Score_Printing endp

Level_2 proc

    CLEARSCREEN
    MOV MOVES, 15 ; NUMBER OF MOVES IN THIS LEVEL

    push x 
    push y
    Grid_Level_1 x, y
    pop y 
    pop x
    
    MOV AX, 01H ; Displays the mouse cursor
    INT 33H

    MOV CX,00 ; Setting X-Axis Position of the mouse
    MOV DX,20 ; Setting Y-Axis Position of the mouse
    MOV AX, 04 ; Setting the start position of the mouse
    INT 33H

    DRAWLVL LEVEL
	call Level_2_Score_Printing

    L12:
    INPWAIT12:
        
        MOV AX, 05 ; GET BUTTON INFORMATION
        MOV BX, 00 ; BUTTON ID
        INT 33H

        TEST AX, 1 ; CHECKING IF LEFT BUTTON IS PRESSES, 1 FOR CHECKING IF YES, 0 FOR NO.
        JZ INPWAIT12 ; IF NOT PRESSES, WAIT.
        MOV X_coord1, CX ; IF PRESSED, GET COORDINATES.
        MOV Y_coord1, DX

        MOV CX,500

        lOOP12:
            PUSH CX
            MOV CX, 500
            LOOP22:
                DEC DX
                CMP DX,0
            LOOP LOOP22
            POP CX
        LOOP LOOP12


    INPWAIT22:
        MOV AX, 05 ; GET BUTTON INFORMATION
        MOV BX, 00 ; BUTTON ID
        INT 33H

        TEST AX, 1 ; CHECKING IF LEFT BUTTON IS PRESSES, 1 FOR CHECKING IF YES, 0 FOR NO.
        JZ INPWAIT22 ; IF NOT PRESSES, WAIT.
        MOV X_coord2, CX ; IF PRESSED, GET COORDINATES.
        MOV Y_coord2, DX

        CHECKSWAP X_coord1, Y_coord1, X_coord2, Y_coord2 ; RETURNS 1 IN AX IF SWAP IS NEEDED
        .IF(AX == 1)
            CLEARSCREEN ; CLEAR SCREEN
            CALCULATEADDRESS X_coord1, Y_coord1, INDEX1 ; CALCULATING ADDRESS IN ARRAY
            CALCULATEADDRESS X_coord2, Y_coord2, INDEX2 ; CALCULATING ADDRESS IN ARRAY
            MODIFYLVL1GRID INDEX1, INDEX2, Level_2_Grid ; MAKE CHANGES IN ARRAY.
			.if Valide_Check == 1
                call Validate_Move_Level_2
				.if Final_Check != 0 
					MOV SI, OFFSET Level_2_Grid
					MOV DI, OFFSET Level_2_Grid
					MOV BX, INDEX1
					ADD SI, BX ; CONTAINS ADDRESS OF INDEX1

					MOV BX,  INDEX2
					ADD DI, BX ; CONTAINS ADDRESS OF INDEX2

					MOV AL, [SI]
					MOV BL, [DI]
					MOV [SI], BL
					MOV [DI], AL


					MOV AH, 00h
   					INT 1AH

   					mov  ax, dx
				    xor  dx, dx
				    mov  cx, 13    
				    div  cx

				    MOV SI, OFFSET ARR
			    	MOV AL, AH
			    	MOV AH, 00
			    	ADD SI, AX
				    
				    .IF (Final_Check == 1)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	INC DI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC DI
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 2)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	DEC DI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	DEC DI
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 3)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	DEC DI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC DI
				    	INC DI
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 4)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	ADD DI, 7
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	ADD DI, 7
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 5)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	SUB DI, 7
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	SUB DI, 7
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 6)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	SUB DI, 7
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	ADD DI, 14
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ENDIF

					mov Valide_Check, 0
					mov Final_Check, 0
					add Level_2_Score, 10
					mov IsSwaped, 1
				.else
					mov IsSwaped, 0
				.endif
			.else
				mov IsSwaped, 0
            .endif
            MOV [X], 75 ; RESETTING VALUES TO DRAW GRID.
            MOV [Y], 30
            DEC MOVES
        .ENDIF
        MOV CX,500

        MOV AX, 02H ; HIDES the mouse cursor
        INT 33H
        
        Grid_Level_1 x, y ; DRAWING GRID
        DRAWLVL LEVEL ; DRAWING CANDIES.
		call Level_2_Score_Printing
		mov IsSwaped, 2
        MOV AX, 01H ; Displays the mouse cursor
        INT 33H

        CMP MOVES, 0
    JNE L12
    INC LEVEL
    FILECREATION FILENAME, Level_2_Score, BYTES
    RET
Level_2 endp

Level_2_Score_Printing proc
	.if IsSwaped==1
		mov X8, 22
		mov Y8, 10
		MESSAGEDISPLAY Valid, X8, Y8
	.elseif IsSwaped==0
		mov X8, 22
		mov Y8, 10
		MESSAGEDISPLAY Invalid, X8, Y8
	.endif 

	mov X8, 1
	mov Y8, 35
	MESSAGEDISPLAY Level_2_Label, X8, Y8

	mov X8, 1
	mov Y8, 65
	MESSAGEDISPLAY Score_Label, X8, Y8
	MOV AH,02H
	MOV BX,0
	MOV DH, 1 ;Printing Location Row
	MOV DL, 72 ;Printing Location Col
	INT 10H
	
	mov ax, 0
	mov bx, 0
	mov cx, 0
	mov dx, 0
	mov Counting_No_Pushed, 0

	mov ax, Level_2_Score
	call Displaying_Score

	mov X8, 22
	mov Y8, 35
	MESSAGEDISPLAY Move_Label, X8, Y8

	MOV AH,02H
	MOV BX,0
	MOV DH, 22 ;Printing Location Row
	MOV DL, 43 ;Printing Location Col
	INT 10H
	
	mov ax, 0
	mov bx, 0
	mov cx, 0
	mov dx, 0
	mov Counting_No_Pushed, 0

	mov ax, MOVES
	call Displaying_Score
	ret 
Level_2_Score_Printing endp

Level_3 proc

    CLEARSCREEN

    MOV MOVES, 15 ; NUMBER OF MOVES IN THIS LEVEL

    push x 
    push y
    Grid_Level_1 x, y
    pop y 
    pop x
    
    MOV AX, 01H ; Displays the mouse cursor
    INT 33H

    MOV CX,00 ; Setting X-Axis Position of the mouse
    MOV DX,20 ; Setting Y-Axis Position of the mouse
    MOV AX, 04 ; Setting the start position of the mouse
    INT 33H

    DRAWLVL LEVEL
	call Level_3_Score_Printing

    L13:
    INPWAIT13:
        
        MOV AX, 05 ; GET BUTTON INFORMATION
        MOV BX, 00 ; BUTTON ID
        INT 33H

        TEST AX, 1 ; CHECKING IF LEFT BUTTON IS PRESSES, 1 FOR CHECKING IF YES, 0 FOR NO.
        JZ INPWAIT13 ; IF NOT PRESSES, WAIT.
        MOV X_coord1, CX ; IF PRESSED, GET COORDINATES.
        MOV Y_coord1, DX

        MOV CX,500

        lOOP13:
            PUSH CX
            MOV CX, 500
            LOOP23:
                DEC DX
                CMP DX,0
            LOOP LOOP23
            POP CX
        LOOP LOOP13


    INPWAIT23:
        MOV AX, 05 ; GET BUTTON INFORMATION
        MOV BX, 00 ; BUTTON ID
        INT 33H

        TEST AX, 1 ; CHECKING IF LEFT BUTTON IS PRESSES, 1 FOR CHECKING IF YES, 0 FOR NO.
        JZ INPWAIT23 ; IF NOT PRESSES, WAIT.
        MOV X_coord2, CX ; IF PRESSED, GET COORDINATES.
        MOV Y_coord2, DX


        CHECKSWAP X_coord1, Y_coord1, X_coord2, Y_coord2 ; RETURNS 1 IN AX IF SWAP IS NEEDED
        .IF(AX == 1)
            CLEARSCREEN ; CLEAR SCREEN
            CALCULATEADDRESS X_coord1, Y_coord1, INDEX1 ; CALCULATING ADDRESS IN ARRAY
            CALCULATEADDRESS X_coord2, Y_coord2, INDEX2 ; CALCULATING ADDRESS IN ARRAY
            MODIFYLVL1GRID INDEX1, INDEX2, Level_3_Grid ; MAKE CHANGES IN ARRAY.
			.if Valide_Check == 1
                call Validate_Move_Level_3
				.if Final_Check != 0
					MOV SI, OFFSET Level_3_Grid
					MOV DI, OFFSET Level_3_Grid
					MOV BX, INDEX1
					ADD SI, BX ; CONTAINS ADDRESS OF INDEX1

					MOV BX,  INDEX2
					ADD DI, BX ; CONTAINS ADDRESS OF INDEX2

					MOV AL, [SI]
					MOV BL, [DI]
					MOV [SI], BL
					MOV [DI], AL

					MOV AH, 00h
   					INT 1AH

   					mov  ax, dx
				    xor  dx, dx
				    mov  cx, 13    
				    div  cx

				    MOV SI, OFFSET ARR
			    	MOV AL, AH
			    	MOV AH, 00
			    	ADD SI, AX
				    
				    .IF (Final_Check == 1)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	INC DI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC DI
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 2)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	DEC DI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	DEC DI
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 3)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	DEC DI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC DI
				    	INC DI
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 4)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	ADD DI, 7
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	ADD DI, 7
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 5)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	SUB DI, 7
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	SUB DI, 7
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ELSEIF (Final_Check == 6)
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	INC SI
				    	SUB DI, 7
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    	ADD DI, 14
				    	INC SI
				    	MOV AL, [SI]
				    	MOV [DI], AL
				    .ENDIF


					mov Valide_Check, 0
					mov Final_Check, 0
					add Level_3_Score, 10
					mov IsSwaped, 1
				.else
					mov IsSwaped, 0
				.endif
			.else
				mov IsSwaped, 0
            .endif
            MOV [X], 75 ; RESETTING VALUES TO DRAW GRID.
            MOV [Y], 30
            DEC MOVES
        .ENDIF
        MOV CX,500

        MOV AX, 02H ; HIDES the mouse cursor
        INT 33H
        
        Grid_Level_1 x, y ; DRAWING GRID
        DRAWLVL LEVEL ; DRAWING CANDIES.
		call Level_3_Score_Printing
		mov IsSwaped, 2
        MOV AX, 01H ; Displays the mouse cursor
        INT 33H

        CMP MOVES, 0
    JNE L13
    FILECREATION FILENAME, Level_3_Score, BYTES
    RET
Level_3 endp

Level_3_Score_Printing proc

	.if IsSwaped==1
		mov X8, 22
		mov Y8, 10
		MESSAGEDISPLAY Valid, X8, Y8
	.elseif IsSwaped==0
		mov X8, 22
		mov Y8, 10
		MESSAGEDISPLAY Invalid, X8, Y8
	.endif 

	mov X8, 1
	mov Y8, 35
	MESSAGEDISPLAY Level_3_Label, X8, Y8

	mov X8, 1
	mov Y8, 65
	MESSAGEDISPLAY Score_Label, X8, Y8
	MOV AH,02H
	MOV BX,0
	MOV DH, 1 ;Printing Location Row
	MOV DL, 72 ;Printing Location Col
	INT 10H
	
	mov ax, 0
	mov bx, 0
	mov cx, 0
	mov dx, 0
	mov Counting_No_Pushed, 0

	mov ax, Level_3_Score
	call Displaying_Score

	mov X8, 22
	mov Y8, 35
	MESSAGEDISPLAY Move_Label, X8, Y8

	MOV AH,02H
	MOV BX,0
	MOV DH, 22 ;Printing Location Row
	MOV DL, 43 ;Printing Location Col
	INT 10H
	
	mov ax, 0
	mov bx, 0
	mov cx, 0
	mov dx, 0
	mov Counting_No_Pushed, 0

	mov ax, MOVES
	call Displaying_Score
	ret 
Level_3_Score_Printing endp

Validate_Move_Level_1 proc
;Horizontal Checks============================================
  	mov si, offset Level_1_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di, offset Level_1_Grid
	add di, BX

	inc di
	mov al, [di]
	mov Next1, al

	inc di
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al ; SAME AS ONE ON 2 RIGHT
		.if temp == bl ; SAME AS ONE ON 1 RIGHT
			mov Final_Check, 1 ; NEXT 2 ON THE RIGHT ARE THE SAME.
		.else
			jmp Not_Valid 
		.endif
		jmp Not_Valid
	.endif
	Not_Valid:
	;==========================================================
	mov si,offset Level_1_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_1_Grid
	add di, BX

	dec di
	mov al, [di]
	mov Next1, al

	dec di
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 2 ; PREVIOUS 2 ON THE LEFT ARE THE SAME
		.else
			jmp Not_Valid1
		.endif
		jmp Not_Valid1
	.endif
	Not_Valid1:
	;==========================================================
	mov si,offset Level_1_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_1_Grid
	add di, BX

	dec di
	mov al, [di]
	mov Next1, al
	inc di
	inc di
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 3 ; BOTH 1 LEFT AND 1 RIGHT ARE THE SAME.
		.else
			jmp Not_Valid2
		.endif
		jmp Not_Valid2
	.endif
	Not_Valid2:
;Vertical Checks============================================
	mov si,offset Level_1_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_1_Grid
	add di, BX

	add di, 7
	mov al, [di]
	mov Next1, al

	add di, 7
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 4 ; BELOW 2 ARE SAME.
		.else
			jmp Not_Valid3
		.endif
		jmp Not_Valid3
	.endif
	Not_Valid3:
	;=================================
	mov si,offset Level_1_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_1_Grid
	add di, BX

	sub di, 7
	mov al, [di]
	mov Next1, al

	sub di, 7
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 5 ; ABOVE 2 ARE SAME.
		.else
			jmp Not_Valid4
		.endif
		jmp Not_Valid4
	.endif
	Not_Valid4:
	;===================================
	mov si,offset Level_1_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_1_Grid
	add di, BX

	add di, 7
	mov al, [di]
	mov Next1, al

	sub di, 14
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 6 ; ABOVE AND BELOW ARE SAME.
		.else
			jmp Not_Valid5
		.endif
		jmp Not_Valid5
	.endif
	Not_Valid5:
   	ret 
Validate_Move_Level_1 ENDP

Validate_Move_Level_2 proc
;Horizontal Checks============================================
  	mov si, offset Level_2_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di, offset Level_2_Grid
	add di, BX

	inc di
	mov al, [di]
	mov Next1, al

	inc di
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 1 ; NEXT 2
		.else
			jmp Not_Valid 
		.endif
		jmp Not_Valid
	.endif
	Not_Valid:
	;==========================================================
	mov si,offset Level_2_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_2_Grid
	add di, BX

	dec di
	mov al, [di]
	mov Next1, al

	dec di
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 2 ; PREVIOUS 2
		.else
			jmp Not_Valid1
		.endif
		jmp Not_Valid1
	.endif
	Not_Valid1:
	;==========================================================
	mov si,offset Level_2_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_2_Grid
	add di, BX

	dec di
	mov al, [di]
	mov Next1, al
	inc di
	inc di
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 3 ; NEXT AND PREVIOUS
		.else
			jmp Not_Valid2
		.endif
		jmp Not_Valid2
	.endif
	Not_Valid2:
;Vertical Checks============================================
	mov si,offset Level_2_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_2_Grid
	add di, BX

	add di, 7
	mov al, [di]
	mov Next1, al

	add di, 7
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 4 ; BELOW 2
		.else
			jmp Not_Valid3
		.endif
		jmp Not_Valid3
	.endif
	Not_Valid3:
	;=================================
	mov si,offset Level_2_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_2_Grid
	add di, BX

	sub di, 7
	mov al, [di]
	mov Next1, al

	sub di, 7
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 5 ; ABOVE 2
		.else
			jmp Not_Valid4
		.endif
		jmp Not_Valid4
	.endif
	Not_Valid4:
	;===================================
	mov si,offset Level_2_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_2_Grid
	add di, BX

	add di, 7
	mov al, [di]
	mov Next1, al

	sub di, 14
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 6 ; ANOVE AND BELOW
		.else
			jmp Not_Valid5
		.endif
		jmp Not_Valid5
	.endif
	Not_Valid5:
   	ret 
Validate_Move_Level_2 ENDP


Validate_Move_Level_3 proc
;Horizontal Checks============================================
  	mov si, offset Level_3_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di, offset Level_3_Grid
	add di, BX

	inc di
	mov al, [di]
	mov Next1, al

	inc di
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 1 ; NEXT 2
		.else
			jmp Not_Valid 
		.endif
		jmp Not_Valid
	.endif
	Not_Valid:
	;==========================================================
	mov si,offset Level_3_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_3_Grid
	add di, BX

	dec di
	mov al, [di]
	mov Next1, al

	dec di
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 2 ; PREVIOUS 2
		.else
			jmp Not_Valid1
		.endif
		jmp Not_Valid1
	.endif
	Not_Valid1:
	;==========================================================
	mov si,offset Level_3_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_3_Grid
	add di, BX

	dec di
	mov al, [di]
	mov Next1, al
	inc di
	inc di
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 3 ; NEXT AND PREVIOUS
		.else
			jmp Not_Valid2
		.endif
		jmp Not_Valid2
	.endif
	Not_Valid2:
;Vertical Checks============================================
	mov si,offset Level_3_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_3_Grid
	add di, BX

	add di, 7
	mov al, [di]
	mov Next1, al

	add di, 7
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 4 ; BELOW 2
		.else
			jmp Not_Valid3
		.endif
		jmp Not_Valid3
	.endif
	Not_Valid3:
	;=================================
	mov si,offset Level_3_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_3_Grid
	add di, BX

	sub di, 7
	mov al, [di]
	mov Next1, al

	sub di, 7
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 5 ; ABOVE 2
		.else
			jmp Not_Valid4
		.endif
		jmp Not_Valid4
	.endif
	Not_Valid4:
	;===================================
	mov si,offset Level_3_Grid
	mov bx, INDEX1
	add si, bx
	
	mov al, [si]
	mov temp, al

	mov bx, INDEX2
	mov di,offset Level_3_Grid
	add di, BX

	add di, 7
	mov al, [di]
	mov Next1, al

	sub di, 14
	mov al, [di]
	mov Next2, al

	mov al,Next1
	mov bl,Next2
	.if temp == al
		.if temp == bl
			mov Final_Check, 6 ; BELOW AND ABOVE
		.else
			jmp Not_Valid5
		.endif
		jmp Not_Valid5
	.endif
	Not_Valid5:
   	ret 
Validate_Move_Level_3 ENDP
END MAIN