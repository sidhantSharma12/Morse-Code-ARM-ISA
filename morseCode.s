THUMB 		; Declare THUMB instruction set 
                AREA 		My_code, CODE, READONLY 	; 
                EXPORT 		__MAIN 		; Label __MAIN is used externally q
		ENTRY 
__MAIN
; The following lines are similar to Lab-1 but use an address, in r4, to make it easier.
; Note that one still needs to use the offsets of 0x20 and 0x40 to access the ports
;
; Turn off all LEDs 
			MOV 		R2, #0xC000
			MOV 		R3, #0xB0000000	
			MOV 		R4, #0x0
			MOVT 		R4, #0x2009
			ADD 		R4, R4, R2 		; 0x2009C000 - the base address for dealing with the ports
			STR 		R3, [R4, #0x20]		; Turn off the three LEDs on port 1
			MOV 		R3, #0x0000007C
			STR 		R3, [R4, #0x40] 	; Turn off five LEDs on port 2 


ResetLUT
		LDR         R5, =InputLUT            ; assign R5 to the address at label LUT


		MOV R7, #5							;counter for characters
	
NextChar	BL		LED_OFF									;between characters
			BL		DELAYtwo				;add delay between characters
			LDRB        R0, [R5] 			; Read a character
			ADD         R5, #1              ; point to next value for number of delays, jump by 1 byte
			B		ProcessChar				; If we have a character process it


ProcessChar	BL		CHAR2MORSE				; convert ASCII to Morse pattern in R1		




findOneLOOP	LSLS R0, R0, #1 					;shift bits left, loop to find the first one in the binary morse string
	
			BCC findOneLOOP					;branch if zero/return when 1 is found
			
			BL LED_ON						;turn on (its always 1 here) 
	
processLOOP	LSLS R0, R0, #1 					; shift left by one, loop until the word is 0
			
			BLCS LED_ON 						;if carry bit is one, branch to LED_ON
			BLCC LED_OFF						;if carry bit is zero, branch to LED_OFF
		
			;delay is always run
		
			
			TEQ 	R0, #0 					;check if the character's bits are all zero
			BNE processLOOP 				;if not zero branch back
	
	SUBS R7, R7, #1 						;subtract one to check if string finished
	
	BGT  	NextChar 						;if string didnt finish, branch back to next char
	
	BL      LED_OFF
	BL		DELAYthree						;end of string
	B 		ResetLUT						;always branch back at the end of the string


; Subroutines
;
;			convert ASCII character to Morse pattern
;			pass ASCII character in R0, output in R1
;			index into MorseLuT must be by steps of 2 bytes


CHAR2MORSE	STMFD	R13!,{R14}			; push Link Register (return address) on stack
			SUB 	R0, R0, #0x41		;calculates the offset 		  ; ******************************************************************************************************
			LSL		R0, #1				;times by two cuz WHAATT THE FUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUCCCCCKK
			LDR 	R1 , =MorseLUT		;get the LUT adress
			LDRH	R0, [R1, R0]		;Load R0 with the LUT(offset) ; ******************************************************************************************************
	
			LDMFD		R13!,{R15}		; restore LR to R15 the Program Counter to return


LED_ON 	   	STMFD		R13!,{R3, R14}		; preserve R3 and R4 on the R13 stack
			MOV 		R3, #0xA0000000	
			STR 		R3, [R4, #0x20]	
			BL DELAYone 					;Delay by 1 unit
			
			LDMFD		R13!,{R3, R15}


LED_OFF	   	STMFD		R13!,{R3, R14}	; push R3 and Link Register (return address) on stack
			MOV 		R3, #0xB0000000	


			STR 		R3, [R4, #0x20]
			BL DELAYone 					;Delay by 1 unit
			LDMFD		R13!,{R3, R15}	; restore R3, R4 and LR to R15 the Program Counter to return


;	Delay unit is 500ms


DELAYone	STMFD		R13!,{R2, R14}
				MOVT 		R2, #0x7	;1 unit of delay time
delayLoop1		SUBS		R2, #1		;decrement counter
				BNE			delayLoop1
				LDMFD R13!,{R2, R15}
				
DELAYtwo		STMFD		R13!,{R2, R14} 	
				MOVT 		R2, #0xE 	;2 units of delay time
delayLoop2		SUBS		R2, #1		;decrement counter
				BNE			delayLoop2
				LDMFD R13!,{R2, R15}				
	
DELAYthree	STMFD		R13!,{R2, R14} 	
				MOVT 		R2, #0x15 	;3 units of delay time
delayLoop3		SUBS		R2, #1		;decrement counter
				BNE			delayLoop3
				LDMFD R13!,{R2, R15}
				
;
; Data used in the program
; DCB is Define Constant Byte size
; DCW is Define Constant Word (16-bit) size
; EQU is EQUate or assign a value.  This takes no memory but instead of typing the same address in many places one can just use an EQU
;
		ALIGN				; make sure things fall on word addresses


; One way to provide a data to convert to Morse code is to use a string in memory.
; Simply read bytes of the string until the NULL or "0" is hit.  This makes it very easy to loop until done.
;
InputLUT	DCB		"BIRD", 0	; strings must be stored, and read, as BYTES


		ALIGN				; make sure things fall on word addresses
MorseLUT 
		DCW 	0x17, 0x1D5, 0x75D, 0x75 	; A, B, C, D
		DCW 	0x1, 0x15D, 0x1DD, 0x55 	; E, F, G, H
		DCW 	0x5, 0x1777, 0x1D7, 0x175 	; I, J, K, L
		DCW 	0x77, 0x1D, 0x777, 0x5DD 	; M, N, O, P
		DCW 	0x1DD7, 0x5D, 0x15, 0x7 	; Q, R, S, T
		DCW 	0x57, 0x157, 0x177, 0x757 	; U, V, W, X
		DCW 	0x1D77, 0x775 			; Y, Z


; One can also define an address using the EQUate directive
;
LED_PORT_ADR	EQU	0x2009c000	; Base address of the memory that controls I/O like LEDs


		END 
