; Z80TYPE.ASM - DETERMINE AND PRINT Z80 CPU TYPE
; WRITTEN BY SERGEY KISELEV <SKISELEV@GMAIL.COM>
;
; RUNS ON CP/M SYSTEMS WITH ZILOG Z80 AND COMPATIBLE PROCESSORS
; USES SIO INTERRUPT VECTOR REGISTER FOR TESTS
;
; BUILDING AND RUNNIG STEPS:
; A>ASM Z80TYPE
; A>LOAD Z80TYPE
; A>Z80TYPE
;==============================================================================
; Some changes added by Zegar. <S.ZEGARLINSKI@GMAIL.COM>
; 14/05/2024
; Program changed for non-CP/M computer.
; It should work on any Z80 with 8251A (0E4H) and 8255 (0E0H).
; Tested on CA80 with MIK1.
; Use to build: https://github.com/sbprojects/sbasm3
;==============================================================================

;*********************************************************************
        .cr z80                     
        .tf Z80testCA80.hex,int   
        .lf Z80testCA80.lst
        .sf Z80testCA80.sym       
;        .in ca80.inc
;*********************************************************************
        .sm code           ; 
        .or $C000          ; RAM IN ALL OF CA80
;**************************************************************************
DATA_8251    .EQ 0E4H    ;Data register 8251A                             *
CONTR_8251   .EQ 0E5H    ;Control register 8251A                          *
CONTR_8255   .EQ 0E3H    ;Control register 8255                           *
;**************************************************************************

;BDOS	EQU	5
;WRCHR	EQU	2
;WRSTR	EQU	9
; SIO CHANNEL B COMMAND PORT - RC2014/SC DEFAULT
;
; Costants definitions
;
eos             .equ    $00             ; End of string
cr              .equ    $0d             ; Carriage return
lf              .equ    $0a             ; Line feed
space           .equ    $20             ; Space
SIOBC	        .EQ 	0E0H            ; PA2 (Intel 8255) CA80
;
;
                CALL    INIT_8255       ; PA INSTEAD SIOBC
                CALL    INIT_8251       ; UART INSTEAD BDOS
                CALL    INIT_BUFFER     ; CIRCULAR BUFFER FOR UART

;	ORG	0100H

	LD	DE,MSGSIGNIN
;	MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS
	
; CHECK FOR U880

	LD	HL,0FFFFH
	LD	BC,00100H+SIOBC	; USE SIO CHANNEL B COMMAND PORT FOR TESTS

	; DI
	; MVI	A,02H		; SET SIO CHANNEL B REGISTER POINTER
	; 			; TO REGISTER 2 - INTERRUPT VECTOR REGISTER
	; OUT	SIOBC
	;IN	A,(SIOBC)		; READ THE CURRENT INTERRUPT VECTOR
	SCF
	.DB	0EDH,0A3H	; Z80 OUTI INSTRUCTION
	;PUSH	AF		; SAVE THE ORIGINAL VECTOR ON THE STACK
	; MVI	A,02H		; SET SIO CHANNEL B REGISTER POINTER
	; 			; TO REGISTER 2 - INTERRUPT VECTOR REGISTER
	;OUT	(SIOBC),A
	;POP	AF		; RESTORE THE ORIGINAL INTERRUPT VECTOR
	;OUT	(SIOBC),A		; WRITE IT TO THE SIO
	; EI
	JR  NC,Z80

; U880 DETECTED

	LD	HL,ISMME
	LD	(HL),1		; SET MANUFACTURER MME FLAG
	LD	DE,MSGU880
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS
	JR	CHKCMOS
	
Z80:
	LD	DE,MSGZ80
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS

CHKCMOS:
	LD	DE,MSGFAMILY
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS
; NMOS/CMOS CPU DETECTION ALGORITHM:
; 1. DISABLE INTERRUPTS
; 2. READ AND SAVE SIO CHANNEL B INTERRUPT VECTOR
; 3. MODIFY SIO CHANNEL B INTERRUPT VECTOR USING OUT (C),<0|0FFH>
;    (DB 0EDH, 071H) UNDOCMENTED INSTRUCTION:
;      ON AN NMOS CPU: OUT (C),0
;      ON A CMOS CPU: OUT (C),0FFH
; 4. READ AND SAVE SIO CHANNEL B INTERRUPT VECTOR
; 5. RESTORE SIO CHANNEL B INTERRUPT VECTOR
; 6. SET SIO REGISTER POINTER TO 0
; 7. ENABLE INTERRUPTS
; 8. CHECK THE VALUE READ BACK IN STEP 4
;      0 - NMOS CPU
;      0FFH - CMOS CPU
	; DI
	; MVI	A,02H		; SET SIO CHANNEL B REGISTER POINTER
	; 			; TO REGISTER 2 - INTERRUPT VECTOR REGISTER
	; OUT	SIOBC
	; IN	A,(SIOBC)		; READ THE CURRENT INTERRUPT VECTOR
	; LD	B,A		; SAVE THE ORIGINAL VECTOR TO REGISTER B
	LD	C,SIOBC
	.DB	0EDH, 071H	; UNDOCUMENTED OUT (C),<0|0FFH> INSTRUCTION
				; WRITE 0 OR FF TO THE SIO INTERRUPT VECTOR
	; MVI	A,02H		; SET SIO CHANNEL B REGISTER POINTER
	; 			; TO REGISTER 2 - INTERRUPT VECTOR REGISTER
	; OUT	(SIOBC),A
	IN	A,(SIOBC)		; READ THE NEW INTERRUPT VECTOR
	;LD	C,A		; SAVE THE NEW VECTOR TO REGISTER B
	; MVI	A,02H		; SET SIO CHANNEL B REGISTER POINTER
	; 			; TO REGISTER 2 - INTERRUPT VECTOR REGISTER
	; OUT	(SIOBC),A
	; LD	A,B		; RESTORE THE ORIGINAL INTERRUPT VECTOR
	; OUT	(SIOBC),A		; WRITE IT TO THE SIO
	;EI
	;LD	A,C		; VALUE WRITTEN BY OUT (C),<0|0FFH> INSTRUCTION
	CP	00H		; IS IT ZERO?
	JR  NZ,CMOS

; NMOS DETECTED
	
	LD	DE,MSGNMOS
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS

; CHECK MANUFACTURER FOR NMOS CPUS
	
	LD	DE,MSGVENDOR
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS

; CHECK IF THE MANUFACTURER IS MME

	LD	HL,ISMME
	LD	A,(HL)
	CP	1
	JR  NZ,NOTMME
	
	LD	DE,MSGMME
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS
	JR	DONE

NOTMME:
	LD	DE,MSGNOTMME
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS
	JR	MANUFACTURER    ; DONE !!! CHANGED FOR TOSHIBA
	
CMOS:
	
	LD	DE,MSGCMOS
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS

; CHECK MANUFACTURER FOR CMOS CPUS
MANUFACTURER:
	LD	DE,MSGVENDOR
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS

; TEST SCF (STC) WHEN PREVIOUS INSTRUCTION DOESN'T MODIFY FLAGS
; POP AF (POP PSW) IS NOT TREATED AS FLAG MODIFYING INSTRUCTION

	LD	DE,2800H		; SET 'A' REGISTER BITS 3 AND 5
	CALL	TESTSCF
	CP	29H		; FLAGS ON ZILOG CPU
	JR  Z,ZILOG
	CP	09H		; FLAGS ON TOSHIBA (AND SOME NEC?) CPUS
	JR  Z,TOSHIBA
	CP	01H		; FLAGS ON NEC CPU
	JR  Z,NEC

; UNRECOGNIZED CPU

	CALL	PRINTHEX	; PRINT FLAGS

	LD	DE,MSGUNKNOWN
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS
	JR	DONE
		
ZILOG:
	LD	DE,MSGZILOG
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS
	JR	DONE

TOSHIBA:
	LD	DE,MSGTOSHIBA
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS
	JR	DONE
	
NEC:
	LD	DE,MSGNEC
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS
	
DONE:
	CALL	TESTFLAGS	; TEST HOW FLAGS SCF AFFECTS FLAGS
	CALL	FLUSH_TX
    RST     30H         ; MONITOR CA80
;	RET			; RETURN TO CP/M

;-------------------------------------------------------------------------
; TESTFLAGS - TEST HOW SCF INSTRUCTION AFFECTS YF AND XF FLAGS
; NOTE: YF IS FLAGS.5 AND XF IS FLAGS.3
; INPUT:
;	NONE
; OUTPUT:
;	PRINTED ON CONSOLE
;-------------------------------------------------------------------------	
TESTFLAGS:
	LD	DE,MSGFLAGS
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS
	LD	D,00H
LOOP1:
	LD	E,00H
LOOP2:
	CALL	TESTSCF
	LD	HL,FLAGCOUNT	; POINT TO FLAG COUNTERS
	LD	C,8		; NUMBER OF BITS / FLAGS
LOOP3:
	RRCA			; LOWER BIT TO C
	JR  NC,LOOP4

	INC	(HL)		; INCREMENT COUNTER IF FLAG IS SET
	JR  NZ,LOOP4		; NO OVERFLOW
	INC	HL		; MOVE TO THE HIGH BIT
	INC	(HL)		; INCREMENT HIGHER BIT
	JR	LOOP5		; ALREADY INCREMENTED HL BY 1, SKIP NEXT INX H
LOOP4:
	INC	HL		; MOVE TO THE NEXT COUNTER
LOOP5:
	INC	HL
	DEC	C		; DECREMENT LOOP COUNTER
	JR  NZ,LOOP3		; LOOP COUNTER NOT ZERO - NEXT BIT
	INC	E		; INCREMENT DE
	JR  NZ,LOOP2
	INC	D
	JR  NZ,LOOP1
	
; PRINT VALUES
	LD	C,16		; 16 DIGITS
	LD	HL,FLAGCOUNT+15	; POINT AT THE MSB
LOOP6:
	LD	A,(HL)
	CALL 	PRINTHEX
	DEC	HL
	DEC	C
	JR  NZ,LOOP6		; PRINT NEXT DIGIT
	LD	DE,MSGCRLF
	;MVI	C,WRSTR
	CALL	WRSTR                       ; CALL  BDOS
	RET
	
;-------------------------------------------------------------------------
; TESTSCF - TEST HOW SCF INSTRUCTION AFFECTS YF AND XF FLAGS
; NOTE: YF IS FLAGS.5 AND XF IS FLAGS.3
; INPUT:
;	D - ACCUMULATOR VALUE BEFORE SCF
;	E - FLAGS VALUE BEFORE SCF
; OUTPUT:
;	A.0 - XF FLAG VALUE AFTER SCF
;	A.1 - YF FLAG VALUE AFTER SCF
;-------------------------------------------------------------------------
TESTSCF:
	PUSH	DE
	;DI
	LD	A,0FEH
	AND	D
	LD	D,A
	LD	A,0FEH
	AND	E
	LD	E,A
	PUSH	DE		; PUSH DE TO THE STACK
	POP	AF		; POP A AND FLAGS FROM THE STACK (DE)
	SCF			; SET CF FLAG, DEPENDING ON THE CPU TYPE THIS
				; ALSO MIGHT CHANGE YF AND XF FLAGS
	PUSH	AF		; STORE A AND F
	POP	DE		; NEW FLAGS IN E
	;EI
;	XRA	A		; ZERO A
;	DB	0CBH,5BH	; BIT 3,E - TEST IF XF IS SET
;	JNZ	TESTYF		; NOT SET
;	DB	0CBH,0C7H	; SET 0,A - SET BIT A.0
;TESTYF:
;	DB	0CBH,6BH	; BIT 5,E - TEST IF YF IS SET
;	JNZ	TESTSCF1
;	DB	0CBH,0CFH	; SET 1,A - SET BIT A.1
;TESTSCF1:
	LD	A,E
	POP	DE
	RET

;-------------------------------------------------------------------------
; PRINTHEX - PRINT BYTE IN HEXADECIMAL FORMAT
; INPUT:
;	A - BYTE TO PRINT
; OUTPUT:
;	NONE
;-------------------------------------------------------------------------
PRINTHEX:
	PUSH	BC
	PUSH	DE
	PUSH	HL
	PUSH	AF		; SAVE PRINTED VALUE ON THE STACK
	RRCA			; ROTATE HIGHER 4 BITS TO LOWER 4 BITS
	RRCA
	RRCA
	RRCA
	CALL	PRINTDIGIT	; PRINT HIGHER 4 BITS
	POP	    AF		; RESTORE PRINTED VALUE
	PUSH	AF		; PUSH IT TO THE STACK AGAIN
	CALL	PRINTDIGIT	; PRINT LOWER 4 BITS
	POP	AF	
	POP	HL
	POP	DE
	POP	BC
	RET

;-------------------------------------------------------------------------	
; PRINTDIGIT - PRINT DIGIT IN HEXADECIMAL FORMAT
; INPUT:
;	A - DIGIT TO PRINT, LOWER 4 BITS 
; OUTPUT:
;	NONE
; TRASHES REGISTERS A, FLAGS, BC, DE, HL
;-------------------------------------------------------------------------	
PRINTDIGIT:
	AND	0FH		; ISOLATE LOWER 4 BITS
	ADD	'0'		; CONVERT TO ASCII
	CP	':'		; GREATER THAN '9'?
	JR  C,PRINTIT
	ADD	7       ; CONVERT A-F TO ASCII
	
PRINTIT:
	;LD	E,A
	;MVI	C,WRCHR
	CALL	putc                       ; CALL  BDOS
	RET

FLAGCOUNT:	.DW	0,0,0,0,0,0,0,0
ISMME:		.DB	0
MSGSIGNIN:	.DB	'Z80 Processor Type Detection (C) 2024 Sergey Kiselev',CR,LF
		    .DB	'Processor family: ',EOS
MSGFAMILY:	.DB	'Logic family:     ',EOS
MSGVENDOR:	.DB	'Manufacturer:     ',EOS
MSGFLAGS:	.DB	'SCF flags test:   ',EOS
MSGU880:	.DB	'U880',CR,LF,EOS
MSGZ80:		.DB	'Z80',CR,LF,EOS
MSGNMOS:	.DB	'NMOS',CR,LF,EOS
MSGCMOS:	.DB	'CMOS',CR,LF,EOS
MSGMME:		.DB	'MME or Thesys',CR,LF,EOS
MSGNOTMME:	.DB	'Zilog or non-MME/Thesys clone',CR,LF,EOS
MSGZILOG:	.DB	'Zilog or SGS/ST',CR,LF,EOS
MSGTOSHIBA:	.DB	'Toshiba or NEC',CR,LF,EOS
MSGVM1:		.DB	'VM1',CR,LF,EOS
MSGNEC:		.DB	'NEC',CR,LF,EOS
MSGUNKNOWN:	.DB	' Unknown'
MSGCRLF:	.DB	CR,LF,EOS

;	END
;************************************************************************
; ADDED BY ZEGAR.
;************************************************************************
;------------------------------------------------------------------------------
;---
;--- I/O subroutines
;---
;------------------------------------------------------------------------------
WRSTR:
                EX DE,HL
                CALL puts
                EX DE,HL
                CALL FLUSH_TX
                RET
;------------------------------------------------------------------------------
;---
;--- String subroutines
;---
;------------------------------------------------------------------------------

;
; Send a string to the serial line, HL contains the pointer to the string:
;
puts            push    af
;                push    hl
puts_loop       ld      a, (hl)
                cp      eos             ; End of string reached?
                jr      z, puts_end     ; Yes
                call    putc
                inc     hl              ; Increment character pointer
                jr      puts_loop       ; Transmit next character
puts_end        
;                pop     hl
                pop     af
                ret 
;
; Send a single character to the serial line (A contains the character):
;
putc            
                LD      (SAVE_CHAR),A   ; instead of PUSH AF
                CALL    CHECK_TX        ; try to send char from buffer
                CALL    write_buffer    ; put new char in buffer
                RET
;
; Wait for a single incoming character on the serial line
; and read it, result is in A:
;
getc    
                CALL    CHECK_TX        ; try to send char from buffer
                CALL    READ_CHAR       ; is new char?
                JR      Z,GETC          ; repeat if not
                RET                     ; in A new char

;************************************************************************
;*              I8255 INIT                                              *
;*              CA80 USER PORT                                          *
;************************************************************************
INIT_8255:
    LD A,8AH            ;PA OUT, PB, PC IN
    OUT (CONTR_8255),A
    RET
;************************************************************************
;*              I8251A INIT                                             *
;*      SEE RADIOELEKTRONIK 1/1994                                      *
;************************************************************************
INIT_8251:
	XOR	A
	OUT	(CONTR_8251),A
	OUT	(CONTR_8251),A
	OUT	(CONTR_8251),A
	LD	A,40H		    ;RESET
	OUT	(CONTR_8251),A
	LD	A,4EH		    ;8 BIT, 1 STOP, X16
	OUT	(CONTR_8251),A
	IN	A,(DATA_8251)   ;FLUSH
	IN	A,(DATA_8251)
	LD	A,07H		    ;RST=1, DTR=0, Rx Tx ON
	OUT	(CONTR_8251),A
	RET

;************************************************************************
;*              I8251A READ CHAR                                        *
;************************************************************************
READ_CHAR:              
	IN	A,(CONTR_8251)
	AND	02H             ; Rx ready? 
	RET	Z               ; return if not
	IN	A,(DATA_8251)   ; read new char
	RET

;************************************************************************
;*              I8251A SEND CHAR                                        *
;************************************************************************
; SEND_CHAR:
; 	LD	(SAVE_CHAR),A
; SEND1:
; 	IN	A,(CONTR_8251)
; 	AND	01H
; 	RET	Z
; 	LD	A,(SAVE_CHAR)
; 	OUT	(DATA_8251),A
; 	RET

; Z80 Ring Buffer with Empty/Full Check Example

; Constants
;BUFFER_SIZE  equ 16    ; Define the size of the buffer
BUFFER_START .equ 0FBH   ; Start address of the buffer in memory

; Buffer initialization
init_buffer:
    XOR     A            ; Initialize the write and read pointers
    LD      IX,write_ptr
    LD      (IX+0),A      ; write_ptr
    LD      (IX+1),A      ; read_ptr
    ret

CHECK_TX:
    IN	    A,(CONTR_8251)
    AND	    01H
    RET	    Z               ; return if Tx not ready
    CALL    read_buffer
    OR      A
    RET     Z               ; return if buffer is empty
    OUT	    (DATA_8251),A   ; send char
	RET

FLUSH_TX:
    CALL    is_buffer_empty
    RET     Z               ; return if buffer is empty
    CALL    CHECK_TX        ; try to send char from buffer
    JR      FLUSH_TX        ; repeat

; Check if the buffer is empty
is_buffer_empty:
    LD      A,(IX+0)      ; write_ptr
    CP      (IX+1)        ; read_ptr
    ret                   ; Zero flag is set if buffer is empty

; Check if the buffer is full
is_buffer_full:
    LD      A,(IX+0)      ; Get the current write pointer
    inc     a             ; Move to the next position
    CP      (IX+1)        ; read_ptr
    ret                   ; Zero flag is set if buffer is full

; Write data to the buffer with full check
write_buffer:
    call    is_buffer_full ; Check if the buffer is full
    RET     Z           ; buffer_full   ; If the Zero flag is set, the buffer is full

    ; Write data (assuming SAVE_CHAR holds the data to write)
    PUSH    HL
    ld      H, BUFFER_START
    LD      L,(IX+0)        ; Get the current write pointer
    LD      A,(SAVE_CHAR)   ; put new char in buffer
    ld      (hl), a         ; Write the data
    POP     HL
    ; Increment the write pointer
    INC     (IX+0)          ; Move to the next position
    ret

buffer_full:
    ; Handle the error case (e.g., return without writing)
    ;ret

; Read data from the buffer with empty check
read_buffer:
    call    is_buffer_empty     ; Check if the buffer is empty
    JR      Z, buffer_empty     ; If the Zero flag is set, the buffer is empty

    ; Read data
    PUSH    HL
    ld      H, BUFFER_START
    LD      L,(IX+1)            ; Get the current read pointer
    ld      A,(hl)              ; Read the data
    POP     HL
    ; Increment the read pointer
    INC     (IX+1)              ; Move to the next position
    ret

buffer_empty:
    ; Handle the empty case (e.g., return without reading)
    XOR     A
    ret

; Variables
write_ptr:   .db 0      ; Write pointer (offset from BUFFER_START)
read_ptr:    .db 0      ; Read pointer (offset from BUFFER_START)
SAVE_CHAR:
    .DB 0FFH

; FOR CAFL - MASS STORAGE CA80
   ;################################################
   ; po ostatnim bajcie naszego programu wpisujemy 2 x AAAA
   ;.db 0AAh, 0AAh, 0AAh, 0AAh ; po tym markerze /2x AAAA/ nazwa programu
   ;################################################
 .db 0AAh, 0AAh, 0AAh, 0AAh ; marker nazwy
 .db "Z80 TEST CA80 T&N"    ; nazwa programu, max 16 znaków /dla LCD 4x 20 znakow w linii/
 .db 0FFH                   ; koniec tekstu

; koniec zabawy. :-)

                .end
