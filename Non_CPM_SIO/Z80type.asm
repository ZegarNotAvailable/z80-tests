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
; 20/05/2024
; Program changed for non-CP/M computer.
; It should work on any Z80 with Z80 SIO (0E4H).
; Tested on CA80 with Universal Z80 SIO Module by T. Pycio.
; Use to build: https://github.com/sbprojects/sbasm3
;==============================================================================

;*********************************************************************
        .cr z80                     
        .tf Z80testSIO.hex,int   
        .lf Z80testSIO.lst
        .sf Z80testSIO.sym       
;        .in ca80.inc
;*********************************************************************
        .sm code           ; 
        .or $C000          ; RAM IN ALL OF CA80
;**************************************************************************
CHA_DATA     .EQ 84H    ;Data register on channel A                      *
CHB_DATA     .EQ 85H    ;Data register on channel B                      *
CHA_CNTR     .EQ 86H    ;Control registers on channel A                  *
CHB_CNTR     .EQ 87H    ;Control registers on channel B                  *
CONTR_8255   .EQ 0E3H    ;Control register 8255                           *
;**************************************************************************


;BDOS	EQU	5
WRCHR	.EQU	2
WRSTR	.EQU	9
;CMDLINE	EQU	80H		; CP/M command line offset
; SIO CHANNEL B COMMAND PORT - RC2014/SC DEFAULT
SIOBC	.EQU	0E0H

;	ORG	0100H
EOS             .equ    $00             ; End of string
CR              .equ    $0d             ; Carriage return
LF              .equ    $0a             ; Line feed
;
;
    CALL    INIT_8255       ; PA INSTEAD SIOBC
	CALL	SIO_INIT	    ; UART INSTEAD BDOS
    CALL	INIT_BUFFER     ; CIRCULAR BUFFER FOR UART

	LD	DE,MSGSIGNIN
	CALL	PRINTSTR
	
; 	LD	HL,CMDLINE
; 	LD	A,(HL)		; get the number of characters
; 	CP	0
; 	JR	Z,NOARGS

; ARGS1:
; 	INC	HL
; 	LD	A,(HL)
; 	CP	20H
; 	JR	NZ,ARGS2
; 	DEC	C
; 	JR	NZ,ARGS1

; ARGS2:
; 	CP	'/'
; 	JR	NZ,ARGSERR
; 	INC	HL
; 	DEC	C
; 	JR	Z,ARGSERR
; 	LD	A,(HL)
; 	CP	'D'
; 	JR	Z,ARGDEBUG
; 	JR	ARGSERR
	
; ARGSERR:
; 	LD	DE,MSGUSAGE
; 	CALL	PRINTSTR
; 	RET

; ARGDEBUG:
; 	LD	A,1
; 	LD	HL,DEBUG
; 	LD	(HL),A
	
NOARGS:
	CALL	TESTCMOS
	LD	HL,ISCMOS
	LD	(HL),A		; store result to ISCMOS
		
	CALL	TESTU880
	LD	HL,ISU880
	LD	(HL),A		; store result to ISU880
	
	CALL	TESTXY
	LD	HL,XYRESULT
	LD	(HL),A

;-------------------------------------------------------------------------
; Debug
	; LD		HL,DEBUG
	; LD		A,(HL)
	; CP		0
	; JR		Z,DETECTCPU
	

	LD	HL,ISCMOS
	LD	A,(HL)
	LD	DE,MSGRAWCMOS	; display CMOS test result
	CALL	PRINTSTR
	CALL	PRINTHEX
	
	LD	HL,ISU880
	LD	A,(HL)		; store result to ISU880	
	LD	DE,MSGRAWU880	; display U880 test result
	CALL	PRINTSTR
	CALL	PRINTHEX

	LD	HL,XYRESULT
	LD	A,(HL)
	LD	DE,MSGRAWXY	; display XF/YF flags test result
	CALL	PRINTSTR
	CALL	PRINTHEX

	LD	DE,MSGCRLF
	CALL	PRINTSTR
	
	CALL	TESTFLAGS	; TEST HOW FLAGS SCF AFFECTS FLAGS

;-------------------------------------------------------------------------
; CPU detection logic
DETECTCPU:

	LD	DE,MSGCPUTYPE
	CALL	PRINTSTR

; check for U880 CPU

	LD	HL,ISU880
	LD	A,(HL)
	CP	0			; Is it a U880?
	JR	Z,CHECKZ80	; check Z80 flavor
	
	LD	HL,XYRESULT
	LD	A,(HL)
	CP	0FFH		; does it always set XF/YF?
	LD	DE,MSGU880NEW
	JR	Z,DONE		; jump if a new U880/Thesys Z80
	LD	DE,MSGU880OLD
	JR	DONE

; check for Z80 type
CHECKZ80:

	LD	HL,ISCMOS
	LD	A,(HL)
	CP	0		; Is it a NMOS CPU?
	JR	NZ,CHECKCMOS	; check CMOS Z80 flavor

; check for Sharp LH5080A
	LD	HL,XYRESULT
	LD	A,(HL)
	CP	30H
	JR	Z,SHARPLH5080A
	CP	0FFH		; does it always set XF/YF?
	JR	Z,NMOSZ80
	CP	0FDH		; does it sometimes not set XF when FLAGS.3=1?
	JR	Z,NECU780C
	CP	0F4H
	JR	Z,KR1858VM1
	LD	DE,MSGNMOSUNKNOWN
	JR	DONE

SHARPLH5080A:
	LD	DE,MSGSHARPLH5080A
	JR	DONE
	
NMOSZ80:
	LD	DE,MSGNMOSZ80
	JR	DONE
	
NECU780C:
	LD	DE,MSGNECD780C
	JR	DONE

KR1858VM1:
	LD	DE,MSGKR1858VM1
	JR	DONE

CHECKCMOS:
	LD	HL,XYRESULT
	LD	A,(HL)
	CP	0FFH		; does it always set XF/YF?
	JR	Z,CMOSZ80
	CP	3FH		; does it never set YF when A.5=1?
	JR	Z,TOSHIBA

; test for NEC D70008AC. These CPUs seem to behave as following:
; A.5=1 & F.5=0 => YF=1
; A.3=1 & F.3=0 => XF is not set at all, or only sometimes is set
; A.5=0 & F.5=1 => YF is sometimes set
; A.3=0 & F.3=1 => XF is sometimes set
; Note: All of 3 D70008AC that I have behave a bit differently here
;       this might need to be updated when more tests are done
	CP	20H		; YF is often set when A.5=1?
	JR	NC,CMOSUNKNOWN	; XYRESULT > 1Fh, not a NEC...
	AND	0FH		; F.5=1 & A.5=0 and F.3=1 & A.3=0 results
	CP	03H		; F.5=1 & A.5=0 never result in YF set?
	JR	C,CMOSUNKNOWN
	AND	03H		; F.3=1 & A.3=0 results
	JR	NZ,NEC

CMOSUNKNOWN:	
	LD	DE,MSGCMOSUNKNOWN
	JR	DONE
	
CMOSZ80:
	LD	DE,MSGCMOSZ80
	JR	DONE

TOSHIBA:
	LD	DE,MSGTOSHIBA
	JR	DONE

NEC:
	LD	DE,MSGNECD70008AC
	JR	DONE

DONE:
	CALL	PRINTSTR
	LD	DE,MSGCRLF
	CALL	PRINTSTR
;	RET					; RETURN TO CP/M
	LD	DE,MSDONE
	CALL	PRINTSTR
	LD	DE,MSGCRLF
	CALL	PRINTSTR
	RST	30H         ; MONITOR CA80
	
;-------------------------------------------------------------------------
; TESTCMOS - Test if the CPU is a CMOS variety according to OUT (C),0 test
; Note: CMOS Sharp LH5080A is reported as NMOS
; Input:
;	None
; Output:
;	A = 00 - NMOS
;	A = FF - CMOS
;------------------------------------------------------------------------- 
TESTCMOS:
; NMOS/CMOS CPU DETECTION ALGORITHM:
; 1. DISABLE INTERRUPTS
; 2. READ AND SAVE SIO CHANNEL B INTERRUPT VECTOR
; 3. MODIFY SIO CHANNEL B INTERRUPT VECTOR USING OUT (C),<0|0FFH>
;    (.DB 0EDH, 071H) UNDOCMENTED INSTRUCTION:
;      ON AN NMOS CPU: OUT (C),0
;      ON A CMOS CPU: OUT (C),0FFH
; 4. READ AND SAVE SIO CHANNEL B INTERRUPT VECTOR
; 5. RESTORE SIO CHANNEL B INTERRUPT VECTOR
; 6. SET SIO REGISTER POINTER TO 0
; 7. ENABLE INTERRUPTS
; 8. CHECK THE VALUE READ BACK IN STEP 4
;      0 - NMOS CPU
;      0FFH - CMOS CPU
;	DI
	; LD	A,02H			; SET SIO CHANNEL B REGISTER POINTER
	; 					; TO REGISTER 2 - INTERRUPT VECTOR REGISTER
	; OUT	(SIOBC),A
	; IN	A,(SIOBC)		; READ THE CURRENT INTERRUPT VECTOR
	; LD	B,A				; SAVE THE ORIGINAL VECTOR TO REGISTER B
	LD	C,SIOBC
	.DB	0EDH, 071H		; UNDOCUMENTED OUT (C),<0|0FFH> INSTRUCTION
						; WRITE 0 OR FF TO THE SIO INTERRUPT VECTOR
	; LD	A,02H			; SET SIO CHANNEL B REGISTER POINTER
	; 					; TO REGISTER 2 - INTERRUPT VECTOR REGISTER
	; OUT	(SIOBC),A
	IN	A,(SIOBC)		; READ THE NEW INTERRUPT VECTOR
	; LD	C,A				; SAVE THE NEW VECTOR TO REGISTER B
	; LD	A,02H			; SET SIO CHANNEL B REGISTER POINTER
	; 					; TO REGISTER 2 - INTERRUPT VECTOR REGISTER
	; OUT	(SIOBC),A
	; LD	A,B				; RESTORE THE ORIGINAL INTERRUPT VECTOR
	; OUT	(SIOBC),A		; WRITE IT TO THE SIO
;	EI
	; LD	A,C				; VALUE WRITTEN BY OUT (C),<0|0FFH> INSTRUCTION
	RET

;-------------------------------------------------------------------------
; TESTU880 - Check if the CPU is MME U880 or Thesys Z80
; Input:
;	None
; Output:
;	A = 0 - Non-U880
;	A = 1 - U880
;-------------------------------------------------------------------------
TESTU880:
	LD	HL,0FFFFH
	LD	BC,00100H+SIOBC	; USE SIO CHANNEL B COMMAND PORT FOR TESTS

;	DI
	; LD	A,02H		; SET SIO CHANNEL B REGISTER POINTER
	; 				; TO REGISTER 2 - INTERRUPT VECTOR REGISTER
	; OUT	(SIOBC),A
	; IN	A,(SIOBC)	; READ THE CURRENT INTERRUPT VECTOR
	SCF
	.DB	0EDH,0A3H	; Z80 OUTI INSTRUCTION
	; PUSH	AF		; SAVE THE ORIGINAL VECTOR ON THE STACK
	; LD	A,02H		; SET SIO CHANNEL B REGISTER POINTER
	; 				; TO REGISTER 2 - INTERRUPT VECTOR REGISTER
	; OUT	(SIOBC),A
	; POP	AF			; RESTORE THE ORIGINAL INTERRUPT VECTOR
	; OUT	(SIOBC),A	; WRITE IT TO THE SIO
;	EI
	LD	A,1			; Assume it is a U880, set A = 1
	JR	C,TESTU880DONE	; It is a U880, exit
	XOR	A			; Not a U880, set A = 00

TESTU880DONE:
	RET

;-------------------------------------------------------------------------
; TESTXY - Tests how SCF (SCF) instruction affects FLAGS.5 (YF) and FLAGS.3 (XF)
; Input:
;	None
; Output:
;	A[7:6] - YF result of F = 0, A = C | 0x20 & 0xF7
;	A[5:4] - XF result of F = 0, A = C | 0x08 & 0xDF
;	A[3:2] - YF result of F = C | 0x20 & 0xF7, A = 0
;	A[1:0] - XF result of F = C | 0x08 & 0xDF, A = 0
;	Where the result bits set as follows:
;	00 - YF/XF always set as 0
;	11 - YF/XF always set as 1
;	01 - YF/XF most of the time set as 0
;	10 - YF/XF most of the time set as 1
;-------------------------------------------------------------------------
TESTXY:
	LD	C,0FFH		; loop counter
	
TESTXY1:
	LD	HL,XFYFCOUNT	; results stored here

; check F = 0, A = C | 0x20 & 0xF7
	LD	E,00H		; FLAGS = 0
	LD	A,C
	OR	020H		; A.5 = 1
	AND	0F7H		; A.3 = 0
	LD	D,A			; A = C | 0x20 & 0xF7
	PUSH	DE		; PUSH DE TO THE STACK
	POP		AF		; POP A AND FLAGS FROM THE STACK (DE)
	SCF				; SET CF FLAG, DEPENDING ON THE CPU TYPE THIS
					; ALSO MIGHT CHANGE YF AND XF FLAGS
	CALL	STOREYCOUNT

; check F = 0, A = C | 0x08 & 0xDF
	LD	E,00H		; FLAGS = 0
	LD	A,C
	OR	08H			; A.3 = 1
	AND	0DFH		; A.5 = 0
	LD	D,A			; A = C | 0x08 & 0xDF
	PUSH	DE		; PUSH DE TO THE STACK
	POP	AF			; POP A AND FLAGS FROM THE STACK (DE)
	SCF				; SET CF FLAG, DEPENDING ON THE CPU TYPE THIS
					; ALSO MIGHT CHANGE YF AND XF FLAGS
	CALL	STOREXCOUNT

; check F = C | 0x20 & 0xF7, A = 0
	LD	A,C
	OR	020H		; FLAGS.5 = 1
	AND	0F7H		; FLAGS.3 = 0
	LD	E,A			; FLAGS = C | 0x20 & 0xF7
	LD	D,00H		; A = 0
	PUSH	DE		; PUSH DE TO THE STACK
	POP	AF			; POP A AND FLAGS FROM THE STACK (DE)
	SCF				; SET CF FLAG, DEPENDING ON THE CPU TYPE THIS
					; ALSO MIGHT CHANGE YF AND XF FLAGS
	CALL	STOREYCOUNT

; check F = C | 0x08 & 0xDF, A = 0
	LD	A,C
	OR	08H			; FLAGS.3 = 1
	AND	0DFH		; FLAGS.5 = 0
	LD	E,A			; FLAGS = C | 0x08 & 0xDF
	LD	D,00H		; A = 0
	PUSH	DE		; PUSH DE TO THE STACK
	POP	AF			; POP A AND FLAGS FROM THE STACK (DE)
	SCF				; SET CF FLAG, DEPENDING ON THE CPU TYPE THIS
					; ALSO MIGHT CHANGE YF AND XF FLAGS
	CALL	STOREXCOUNT

	DEC	C
	JR	NZ,TESTXY1
	
	LD	C,4			; iteration count - number of bytes
	LD	HL,XFYFCOUNT	; counters

TESTXY2:
	RLA
	RLA
	AND	0FCH		; zero two least significant bits
	LD	B,A		; store A to B
	LD	A,(HL)
	CP	7FH
	JR	TESTXY3		; jump if the count is 0x80 or more
	CP	0
	JR	Z,TESTXY5		; the count is 0 leave bits at 0
	LD	A,1		; the count is between 1 and 0x7F, set result bits to 01
	JR	TESTXY5
TESTXY3:
	CP	0FFH
	LD	A,2		; the count is between 0x80 and 0xFE, set result bits to 10
	JR	NZ,TESTXY4
	LD	A,3		; the count is 0xFF, set result bits to 11
	JR	TESTXY5
TESTXY4:
	LD	A,1		; the count is 0x7F or less, set result bits to 01
TESTXY5:
	OR	B
	INC	HL
	DEC	C
	JR	NZ,TESTXY2
	RET

;-------------------------------------------------------------------------
; STOREXCOUNT - Isolates and stores XF to the byte counter at (HL)
; Input:
;	FLAGS	- flags
;	HL	- pointer to the counters
; Output:
;	HL	- incremented by 1 (points to the next counter)
; Trashes A and DE
;-------------------------------------------------------------------------
STOREXCOUNT:
	PUSH	AF		; transfer flags
	POP	DE			; to E register
	LD	A,E
	AND	08H			; isolate XF
	JR	Z,STOREXDONE
	INC	(HL)		; increment the XF counter (HL)
STOREXDONE:
	INC	HL			; point to the next entry
	RET

;-------------------------------------------------------------------------
; STOREYCOUNT - Isolates and stores YF to the byte counter at (HL)
; Input:
;	FLAGS	- flags
;	HL	- pointer to the counters
; Output:
;	HL	- incremented by 1 (points to the next counter)
; Trashes A and DE
;-------------------------------------------------------------------------
STOREYCOUNT:
	PUSH	AF		; transfer flags
	POP	DE			; to E register
	LD	A,E
	AND	20H			; isolate YF
	JR	Z,STOREYDONE
	INC	(HL)		; increment the YF counter (HL)
STOREYDONE:
	INC	HL			; point to the next entry
	RET
	
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
	LD	C,WRSTR
	CALL	BDOS
	LD	D,00H
TFLOOP1:
	LD	E,00H
TFLOOP2:
	PUSH	DE
;	DI
	PUSH	DE			; PUSH DE TO THE STACK
	POP	AF				; POP A AND FLAGS FROM THE STACK (DE)
	CCF					; SET CF FLAG, DEPENDING ON THE CPU TYPE THIS
						; ALSO MIGHT CHANGE YF AND XF FLAGS
	PUSH	AF			; STORE A AND F
	POP	DE				; NEW FLAGS IN E
;	EI
	LD	A,E				; FLAGS TO ACCUMULATOR
	POP	DE
	JR	CONT

PRINTFLAGS:
	CALL	PRINTHEX	; PRINT ACCUMULATOR
	LD	A,E				; FLAGS TO ACCUMULATOR
	POP	DE
	PUSH	AF
	LD	A,D				; PRINT ORIGINAL ACCUMULATOR(FLAGS)
	CALL	PRINTHEX
	POP	AF
	CALL	PRINTHEX	; PRINT NEW FLAGS
	PUSH	DE
	LD	DE,MSGCRLF
	CALL	PRINTSTR
	POP	DE
CONT:

	LD	HL,XFCOUNT	; POINT TO XF COUNTER
	RRCA			; BIT 3 TO CF
	RRCA
	RRCA
	RRCA
	JR	NC,TFLOOP4
	INC	(HL)			; INCREMENT COUNTER IF FLAG IS SET
	JR	NZ,TFLOOP4		; NO OVERFLOW
	INC	HL				; MOVE TO THE HIGH BIT
	INC	(HL)			; INCREMENT HIGHER BIT

TFLOOP4:
	LD	HL,YFCOUNT		; POINT TO YF COUNTER
	RRCA				; BIT 5 TO CF
	RRCA
	JR	NC,TFLOOP5
	INC	(HL)			; INCREMENT COUNTER IF FLAG IS SET
	JR	NZ,TFLOOP5		; NO OVERFLOW
	INC	HL				; MOVE TO THE HIGH BIT
	INC	(HL)			; INCREMENT HIGHER BIT
TFLOOP5:
	INC	E
	JR	NZ,TFLOOP2
	INC	D				; INCREMENT D
	JR	NZ,TFLOOP1

; PRINT VALUES
	LD	C,4				; 4 BYTES
	LD	HL,YFCOUNT+1	; POINT AT THE MSB
TFLOOP6:
	LD	A,(HL)
	CALL 	PRINTHEX
	DEC	HL
	DEC	C
	JR	NZ,TFLOOP6		; PRINT NEXT DIGIT

	LD	DE,MSGCRLF
	LD	C,WRSTR
	CALL	BDOS
	RET

; PRINT VALUES
	LD	HL,YFCOUNT+1	; MSB OF YF COUNT
	LD	A,(HL)
	CALL 	PRINTHEX
	DEC	HL				; LSB OF YF COUNT
	LD	A,(HL)
	CALL 	PRINTHEX
	LD	HL,XFCOUNT+1	; MSB OF XF COUNT
	LD	A,(HL)
	CALL 	PRINTHEX
	DEC	HL				; LSB OF XF COUNT
	LD	A,(HL)
	CALL 	PRINTHEX

	LD	DE,MSGCRLF
	LD	C,WRSTR
	CALL	BDOS
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
	PUSH	AF			; SAVE PRINTED VALUE ON THE STACK
	RRCA				; ROTATE HIGHER 4 BITS TO LOWER 4 BITS
	RRCA
	RRCA
	RRCA
	CALL	PRINTDIGIT	; PRINT HIGHER 4 BITS
	POP	AF				; RESTORE PRINTED VALUE
	PUSH	AF			; PUSH IT TO THE STACK AGAIN
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
	ADD	A,'0'		; CONVERT TO ASCII
	CP	':'		; GREATER THAN '9'?
	JR	C,PRINTIT
	ADD	A,7		; CONVERT A-F TO ASCII
	
PRINTIT:
	LD	E,A
	LD	C,WRCHR
	CALL	BDOS
	RET

;-------------------------------------------------------------------------
; PRINTSTR - Print string
; INPUT:
;	D - address of the string to print
; OUTPUT:
;	None
; Note: String must be terminated with a dollar sign
;-------------------------------------------------------------------------
PRINTSTR:
	PUSH	AF
	PUSH	BC
	PUSH	DE
	PUSH	HL
	LD		C,WRSTR
	CALL	BDOS
	POP		HL
	POP		DE
	POP		BC
	POP		AF
	RET

DEBUG		.DB	0
ISCMOS		.DB	0
ISU880		.DB	0
XYRESULT	.DB	0
XFYFCOUNT	.DB	0,0,0,0
XFCOUNT		.DW	0
YFCOUNT		.DW	0
MSGSIGNIN	.DB	'Z80 Processor Type Detection (C) 2024 Sergey Kiselev'
MSGCRLF		.DB	CR,LF,EOS
;MSGUSAGE	.DB	'Invalid argument. Usage: z80type [/D]',0DH,0AH,EOS
MSGRAWCMOS	.DB	'Raw results:       CMOS: ',EOS
MSGFLAGS	.DB	'XF/YF flags test:  ',EOS
MSGRAWU880	.DB	' U880: ',EOS
MSGRAWXY	.DB	' XF/YF: ',EOS
MSGCPUTYPE	.DB	'Detected CPU type: ',EOS
MSGU880NEW	.DB	'Newer MME U880, Thesys Z80, Microelectronica MMN 80CPU',EOS
MSGU880OLD	.DB	'Older MME U880',EOS
MSGSHARPLH5080A	.DB	'Sharp LH5080A',EOS
MSGNMOSZ80	.DB	'Zilog Z80, Zilog Z08400 or similar NMOS CPU',CR,LF
		.DB      '                   '
		.DB	'Mostek MK3880N, SGS/ST Z8400, Sharp LH0080A, KR1858VM1',EOS
MSGNECD780C	.DB	'NEC D780C, GoldStar Z8400, possibly KR1858VM1',EOS
MSGKR1858VM1	.DB	'Overclocked KR1858VM1',EOS
MSGNMOSUNKNOWN	.DB	'Unknown NMOS Z80 clone',EOS
MSGCMOSZ80	.DB	'Zilog Z84C00',EOS
MSGTOSHIBA	.DB	'Toshiba TMPZ84C00AP, ST Z84C00AB',EOS
MSGNECD70008AC	.DB	'NEC D70008AC',EOS
MSGCMOSUNKNOWN	.DB	'Unknown CMOS Z80 clone',EOS
MSDONE:	.DB	'Done.',EOS

;------------------------------------------------------------------------------
;---
;--- BDOS SUBSTITUTE subroutine
;---
;------------------------------------------------------------------------------
BDOS:
	PUSH	BC
	PUSH	DE
	PUSH	HL
	PUSH	AF
	LD		A,C
	CP		WRSTR
	JR		Z,STRING	; PRINT STRING
	CP		WRCHR
	JR		NZ,EXIT	; WRONG VALUE
	LD		A,E			; PRINT CHAR
	CALL	PUTC
	JR		EXIT
STRING:
	EX		DE,HL
	CALL	PUTS
	CALL	FLUSH_TX
EXIT:	
	POP		AF
	POP		HL
	POP		DE
	POP		BC
	RET

;------------------------------------------------------------------------------
;---
;--- String subroutine
;---
;------------------------------------------------------------------------------

;
; Send a string to the serial line, HL contains the pointer to the string:
;
PUTS:       LD   	A,(HL)
            CP		EOS             ; End of string reached?
            RET		Z				; Yes
            CALL	PUTC
            INC		HL              ; Increment character pointer
            JR		PUTS		    ; Transmit next character

;------------------------------------------------------------------------------
;---
;--- I/O subroutines
;---
;------------------------------------------------------------------------------

;
; Send a single character to the serial line (A contains the character):
;
PUTC:            
                LD      (SAVE_CHAR),A   ; instead of PUSH AF
                CALL    CHECK_TX        ; try to send char from buffer
                CALL    write_buffer    ; put new char in buffer
                RET
;
; Wait for a single incoming character on the serial line
; and read it, result is in A:
;
GETC:    
                CALL    CHECK_TX        ; try to send char from buffer
                CALL    READ_CHAR       ; is new char?
                RET     C               ; in A new char
                JR      GETC            ; repeat if not

;************************************************************************
;*              I8255 INIT                                              *
;*              CA80 USER PORT                                          *
;************************************************************************
INIT_8255:
    LD A,8AH            ;PA OUT, PB, PC IN
    OUT (CONTR_8255),A
    RET
;************************************************************************                
;*************************************************************************
;*              Z80 SIO INIT                                             *
;*************************************************************************
SIO_INIT:
				LD C,CHA_CNTR       ;INIT CHANNEL A
				CALL SIO_INI
				LD C,CHB_CNTR       ;INIT CHANNEL B
SIO_INI:
				LD B,7              ;LENGHT OF SIO_INIT_TABLE
				LD HL,SIO_INIT_TABLE
				OTIR                ;WRITE TO ALL REGS
				RET

SIO_INIT_TABLE:
    .DB 18h             ;RESET CHANNEL
    .DB 04h             ;REG4
    .DB 0C4H            ;x64 clock, 1 stop bit, no parity (7,3728MHz -> 115200 baud)
    .DB 03H             ;REG3
    .DB 0C1H            ;Set receive config to 8 bits, RX ENABLE
    .DB 05h             ;REG5
    .DB 68h             ;Transmitter configuration set to 8 bits, TX ENABLE
     

;*************************************************************************
;*              Z80 SIO READ CHAR                                        *
;*************************************************************************
READ_CHAR:              
				XOR A
				OUT (CHB_CNTR),A    ;TEST RX
				IN A,(CHB_CNTR)     ;READ REG0
				RRA                 ;RX CHAR AVAILABLE -> CY
				RET NC              ;RX NOT AVAILABLE
				IN A,(CHB_DATA)     ;READ CHAR
				RET                 ;IF CY=1 A=NEW CHAR

;*************************************************************************
;*              Z80 SIO SEND CHAR (IF ANY)                               *
;*************************************************************************
CHECK_TX:
				XOR     A
				OUT     (CHB_CNTR),A    ;TEST RX
				IN      A,(CHB_CNTR)    ;READ REG0
				BIT     2,A             ;TEST TRANSMIT BUFFER EMPTY
				RET	    Z               ; return if Tx not ready
				CALL    read_buffer
				OR      A
				RET     Z               ; return if buffer is empty
				OUT	    (CHB_DATA),A    ; send char
				RET

;*************************************************************************
; Z80 Ring Buffer with Empty/Full Check Example
;*************************************************************************

; Constants
BUFFER_START .equ 0FBH   ; Start address of the buffer in memory

; Buffer initialization
init_buffer:
				XOR     A            ; Initialize the write and read pointers
				LD      IX,write_ptr
				LD      (IX+0),A      ; write_ptr
				LD      (IX+1),A      ; read_ptr
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
				RET                   ; Zero flag is set if buffer is empty

; Check if the buffer is full
is_buffer_full:
				LD      A,(IX+0)      ; Get the current write pointer
				INC     A             ; Move to the next position
				CP      (IX+1)        ; read_ptr
				RET                   ; Zero flag is set if buffer is full

; Write data to the buffer with full check
write_buffer:
				CALL    is_buffer_full ; Check if the buffer is full
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
				RET

buffer_full:
    ; Handle the error case (e.g., return without writing)
    ;ret

; Read data from the buffer with empty check
read_buffer:
				CALL    is_buffer_empty     ; Check if the buffer is empty
				JR      Z, buffer_empty     ; If the Zero flag is set, the buffer is empty

    ; Read data
				PUSH    HL
				LD      H, BUFFER_START
				LD      L,(IX+1)            ; Get the current read pointer
				LD      A,(hl)              ; Read the data
				POP     HL
    ; Increment the read pointer
				INC     (IX+1)              ; Move to the next position
				RET

buffer_empty:
    ; Handle the empty case (e.g., return without reading)
				XOR     A
				RET

   ;################################################
   ;##   po ostatnim bajcie naszego programu wpisujemy 2 x AAAA
   ;.db 0AAh, 0AAh, 0AAh, 0AAh ; po tym markerze /2x AAAA/ nazwa programu
   ;################################################
 .db 0AAh, 0AAh, 0AAh, 0AAh ; marker nazwy
 .db "Z80 CPU test SIO"       ; nazwa programu, max 16 znaków /dla LCD 4x 20 znakow w linii/
 .db 0FFH                   ; koniec tekstu

; Variables
write_ptr:   	.db 0      ; Write pointer (offset from BUFFER_START)
read_ptr:    	.db 0      ; Read pointer (offset from BUFFER_START)
SAVE_CHAR:		.DB 0FFH
; koniec zabawy. :-)

	.END