; 16bit by 16bit unsigned multiply
;  by Martin Sturm 2010
; tested
;
; aH:aL * bH:bL --> r4:r3:r2:r1
;
; t1 is a temporary variable
; if modifying a or b is ok, aL, bL, or bH can be used as t1
;
; 140 instructions, 140 cycles
;

; helper macro
mmac MACRO A,bit, u2,u1
    BTFSC	A,bit
    ADDWF	u2,F
    RRF	u2,F
    RRF	u1,F
    ENDM

MULT_16x16_FASTEST MACRO aH,aL, bH,bL, r4,r3,r2,r1, t1

	; 16x8 multiply  aH:aL * bL -> r3:r4:r1
    CLRF	r3
    CLRF	r1
    CLRC
    MOVFW	bL
	mmac	aL,0, r3,r1
	mmac	aL,1, r3,r1
	mmac	aL,2, r3,r1
	mmac	aL,3, r3,r1
	mmac	aL,4, r3,r1
	mmac	aL,5, r3,r1
	mmac	aL,6, r3,r1
	mmac	aL,7, r3,r1
    CLRF	r4
	; carry already clear from last RRF of mmac above
	; bL still in W
	mmac	aH,0, r3,r4
	mmac	aH,1, r3,r4
	mmac	aH,2, r3,r4
	mmac	aH,3, r3,r4
	mmac	aH,4, r3,r4
	mmac	aH,5, r3,r4
	mmac	aH,6, r3,r4
	mmac	aH,7, r3,r4
	
	; 16x8 multiply  aH:aL * bH -> r4:t1:r2
	;  middle byte from previous 16x8 multiplication starts in r4
	;  but ends in r2
    CLRF	r2
    MOVFW	bH
	; carry already clear from last RRF of mmac above
	mmac	aL,0, r4,r2
	mmac	aL,1, r4,r2
	mmac	aL,2, r4,r2
	mmac	aL,3, r4,r2
	mmac	aL,4, r4,r2
	mmac	aL,5, r4,r2
	mmac	aL,6, r4,r2
	mmac	aL,7, r4,r2
	; W still holds bH
    CLRF	t1	; bH,bL, or aL may be used for t1
	; carry already clear from last RRF of mmac above
	mmac	aH,0, r4,t1
	mmac	aH,1, r4,t1
	mmac	aH,2, r4,t1
	mmac	aH,3, r4,t1
	mmac	aH,4, r4,t1
	mmac	aH,5, r4,t1
	mmac	aH,6, r4,t1
	mmac	aH,7, r4,t1
	
	; add middle byte aH:aL * bH to upper byte of aH:aL * bL
    MOVFW	t1
    ADDWF	r3,F
    SKPNC
    INCF	r4,F

	ENDM