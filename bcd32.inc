;******************************************************************
; Convert 32-bit binary number at <bin> into a bcd number
; at <bcd>. Uses Mike Keitz's procedure for handling bcd 
; adjust; Modified Microchip AN526 for 32-bits.
; rough guess is that about 2200 instructions will be executed

b2bcd	movlw	.32		; 32-bits
	movwf	ii		; make cycle counter
	clrf	bcd		; clear result area
	clrf	bcd+1
	clrf	bcd+2
	clrf	bcd+3
	clrf	bcd+4
	
b2bcd2	movlw	bcd		; make pointer
	movwf	FSR
	movlw	.5
	movwf	cnt

; Mike's routine:

b2bcd3	movlw	0x33		
	addwf	INDF,f		; add to both nybbles
	btfsc	INDF,3		; test if low result > 7
	andlw	0xf0		; low result >7 so take the 3 out
	btfsc	INDF,7		; test if high result > 7
	andlw	0x0f		; high result > 7 so ok
	subwf	INDF,f		; any results <= 7, subtract back
	incf	FSR,f		; point to next
	decfsz	cnt
	goto	b2bcd3
	
	rlf	bin+0,f		; get another bit
	rlf	bin+1,f
	rlf	bin+2,f
	rlf	bin+3,f
	rlf	bcd+0,f		; put it into bcd
	rlf	bcd+1,f
	rlf	bcd+2,f
	rlf	bcd+3,f
	rlf	bcd+4,f
	decfsz	ii,f		; all done?
	goto	b2bcd2		; no, loop
	return			; yes