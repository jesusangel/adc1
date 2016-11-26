;16Bits_BCD: Esta rutina convierte un número binario de 16 bits situado en Cont_H y
;Cont_L y, lo convierte en 5 dígitos BCD que se depositan en las variables BCD_0, BCD_1
;y BCD_2, siendo esta última la de menos peso.
;Está presentada en la nota de aplicación AN544 de MICROCHIP y adaptada por MSE

Bits16_BCD	
    bcf	    STATUS,C
    clrf    Contador	
    bsf	    Contador,4		;Carga el contador con 16		
    clrf    BCD_0
    clrf    BCD_1
    clrf    BCD_2		;Puesta a 0 inicial

Loop_16		
    rlf		Byte_L,F
    rlf		Byte_H,F
    rlf		BCD_2,F
    rlf		BCD_1,F
    rlf		BCD_0,F		;Desplaza a izda. (multiplica por 2)
    decfsz	Contador,F
    goto	Ajuste
    return

Ajuste		
    movlw	BCD_2
    movwf	FSR			;Inicia el índice
    call	Ajuste_BCD		;Ajusta el primer byte
    incf	FSR,F
    call	Ajuste_BCD		;Ajusta el segundo byte
    incf	FSR,F
    call	Ajuste_BCD
    goto	Loop_16

Ajuste_BCD	
    movf	INDF,W		
    addlw	0x03
    movwf	Temporal	
    btfsc	Temporal,3		;Mayor de 7 el nibble de menos peso ??
    movwf	INDF			;Si, lo acumula
    movf	INDF,W		
    addlw	0x30
    movwf	Temporal
    btfsc	Temporal,7		;Mayor de 7 el nibble de menos peso ??
    movwf	INDF			;Si, lo acumula
    return