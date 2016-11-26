    #include <p16F876A.inc>
    errorlevel -302         ; no bank warnings

; CONFIG
 __config 0xFF79
;    __CONFIG _FOSC_XT & _WDTE_OFF & _PWRTE_OFF & _BOREN_ON & _LVP_OFF & _CPD_OFF & _WRT_OFF & _CP_OFF
 
#define res .48828	; 4.8828 mV

Lcd_var	    EQU 0x20
Byte_L	    EQU 0x22
Byte_H	    EQU 0x23
BCD_2	    EQU 0x24
BCD_1	    EQU 0x25
BCD_0	    EQU	0x26
Contador    EQU	0x27
Temporal    EQU 0x28
DELAY	    EQU 0x29
F1L	    EQU	0x30	    ; LSB del primer factor de la multiplicación
F1H	    EQU	0x31	    ; MSB del primer factor de la multiplicación
F2L	    EQU	0x32	    ; LSB del segundo factor de la multiplicación
F2H	    EQU	0x33	    ; MSB del segundo factor de la multiplicación
;Registros para guardar resultado de multiplicacion de 32 bit's
p1	    EQU	0x34	    ; LSB del producto F1*F2
p2	    EQU	0x35	    ; Segundo byte del producto F1*F2
p3	    EQU	0x36	    ; Tercer byte del producto F1*F2
p4	    EQU	0x37	    ; MSB del producto F1*F2
Flag 	    EQU	0x38	    ;Registro temporal de multiplicacion
t1	    EQU	0x39
PERIODO	    EQU .255	    ; Periodo del pulso PWM
bin	    EQU	0x34	    ; 4 bytes a convertir a BCD
bcd	    EQU	0x44	    ; 5 bytes resultado de la conversión 32b2BCD
ii	    EQU	0x49
cnt	    EQU	0x50
 
RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    START                   ; go to beginning of program

    ORG 0x04
    goto INTER
    
    ORG 0x05
    
	#include "lcd_cxx.inc"
	#include "bcd16.inc"		
	#include "bcd32.inc"
	#include "MULT_16x16_FASTEST.inc"
	
loadd	macro BH, BL
	local	p
p = res / .256
	movlw	p
	movwf	BH
	movlw	res - p * .256
	movwf	BL
	endm

MAIN_PROG CODE                      ; let linker place main program
 
; Muestra por el LCD los diez dígitos situados en las variables bcd4-0
; Los dígitos BCD serán: AB CD EF GH IJ (MSB=A, LSB=J)
MOSTRARV
	bcf	STATUS, RP0	    ; Banco 0
	movlw	0x80
	call	LCD_REG
	swapf	bcd+3, w	    ; Cargamos en W uds y decenas de V
	andlw	0x0f		    ; Primero mostramos las unidades
	iorlw	0x30		    ; Lo convertimos a ASCII: 0x30, ..., 0x39
	call	LCD_DATO	    ; Lo sacamos por la pantalla
	movlw	0x2E		    ; Ponemos el punto decimal
	call	LCD_DATO	    ; en la pantalla
	movf	bcd+3, w	    ; Cargamos en W uds y decenas
	andlw	0x0f		    ; Ahora cogemos las decenas de V
	iorlw	0x30		    ; Lo convertimos a ASCII: 0x30, ..., 0x39
	call	LCD_DATO	    ; Y lo sacamos por la pantalla	
	swapf	bcd+2, w	    ; Seguimos con centenas y milésimas
	andlw	0x0f		    
	iorlw	0x30		    
	call	LCD_DATO	    
	movf	bcd+2, w
	andlw	0x0f		    
	iorlw	0x30		    
	call	LCD_DATO	    
	movlw	0x20		    ; Espacio
	call	LCD_DATO
	movlw	0x56		    ; V
	call	LCD_DATO
	return			    ; Devolvemos el control


; Muestra por el LCD los diez dígitos situados en las variables bcd4-0
; Los dígitos BCD serán: AB CD EF GH IJ (MSB=A, LSB=J)
VISUALIZAR32
	bcf	STATUS, RP0	    ; Banco 0
	movlw	0x80
	call	LCD_REG
	movlw	.5		    ; Vamos a procesar 5 bytes
	movwf	Contador	    ; con este contador
	movlw	bcd+4		    ; Primero sacamos el dígito más alto (A)
	movwf	FSR		    ; Direccionamiento indirecto: &bcd+4
VISU_LOOP32
	swapf	INDF, W		    ; Intercambiamos digitos BCD: AB->BA
	andlw	0x0f		    ; Nos quedamos con el de la drcha (A)
	iorlw	0x30		    ; Lo convertimos a ASCII: 0x30, ..., 0x39
	call	LCD_DATO	    ; Y lo sacamos por la pantalla
	movf	INDF, W		    ; Procedemos igual con el otro dígito (B)
	andlw	0x0f
	iorlw	0x30
	call	LCD_DATO
	decf	FSR, F		    ; Seguimos con el siguiente byte
	decfsz	Contador, F	    ; Si aún no hemos mostrado los 5 bytes
	goto	VISU_LOOP32	    ; Continuamos con el siguiente byte
	return			    ; else devolvemos el control
	
; Muestra por el LCD los cinco dígitos situados en las variables BCD2-0
VISUALIZAR
	bcf	STATUS, RP0
	movlw	0x80
	call	LCD_REG
	movlw	.3
	movwf	Contador
	movlw	BCD_0
	movwf	FSR
VISU_LOOP
	swapf	INDF, W
	andlw	0x0f
	iorlw	0x30
	call	LCD_DATO
	movf	INDF, W
	andlw	0x0f
	iorlw	0x30
	call	LCD_DATO
	decf	FSR, F
	decfsz	Contador, F
	goto	VISU_LOOP
	return

; INITAD, inicializa y configura el hardware A/D
; Selecciona ch0, 8*Tosc, A/D int y enciende el A/D
INITAD
	bsf	STATUS, RP0		; Select bank 1
	bcf	STATUS, RP1
	movlw   b'10001110'		; Right justify, select AN0 
	movwf   ADCON1			; as analog input
	bsf	PIE1, ADIE		; enable a/d int.
	bcf	STATUS, RP0             ; Select bank 0	
	movlw   b'01000001'	        ; 8*Tosc, Ch 0 (RA0/AN0), go=0, AD on
	movwf   ADCON0
	return				;

WAIT20us	
	bcf	STATUS, RP0
	movlw	~.20			; Wait 20us for sample on ch0
	movwf	TMR0
	bcf	INTCON, T0IF		; Borramos flag T0IF
	bsf	INTCON, T0IE		; Habilitamos int TMR0
	bsf	INTCON, GIE		; Activo interrupciones
	return

; Carga el valor de 10 bits resultado de la conversión A/D situado en los
; registros ADRESH y ADRESL (justificado a la derecha) en el registro CCP2L
; (los bits) más significativos y en los bits 5:6 del CCP2CON los menos signi-
; ficativos. Por ejemplo, si ADRESH-ADRESL = 0000 00ji hgfe dcba entonces
; CCP2L<7:0> = jihg fedc
; CCP2CON<6:5> = ba
ADCTOCCP2
	bcf	STATUS, C		; Borramos acarreo
	bcf	STATUS, RP0		; Banco 0
	bsf	PORTC, 5
	movlw	b'00000011'		; Borramos los 6 bits más significativos
	andwf	Byte_H, F		; de Byte_H: 0000 00ji
	rrf	Byte_H, F		; Movemos los bits más significativos
	rrf	Byte_L, F		; del resultado de la conversión A/D
	rrf	Byte_H, F		; al registro ADRESL: jihg fedc.
	rrf	Byte_L, F		; ADRESL: jihg fedc
	movfw	Byte_L			; Cargamos ADRESL en CCPR2L	
	movwf	CCPR2L			; (bits más significativos de la conv)
	bsf	PORTC, 4
	rrf	Byte_H, F		; ADRESH: ba0 0000
	rrf	Byte_H, F		; ADRESH: 0ba0 0000
	rrf	Byte_H, F		; ADRESH: 00ba 0000
	movfw	Byte_H
	iorwf	CCP2CON, F		; CCP2CON: xxab xxxx
	bsf	PORTC, 3
	return

TMR0INT
	bsf	STATUS, RP0		; Bank 1
	bsf	PIE1, ADIE		; Elable A/D interrupts
	bcf	STATUS, RP0		; Bank 0
	movlw	b'1100000'		; GIE, PIE, resto off
	movwf	INTCON
	bcf	PIR1, ADIF		; Borramos flag ADIF
	
	bsf	ADCON0, GO		; Start AD conversion
					; The ADIF bit will be set and the GO/DONE
 					; bit is cleared upon completion of the
					; A/D Conversion.
	return	

LEEADC	
	bcf	PIR1, ADIF		; Borramos flag ADIF
	btfsc	ADCON0, GO		; Si GO=0 salta, else
	goto	WAIT20us		; seguimos esperando
	bsf	STATUS, RP0		; Banco 1
	bcf	STATUS, RP1			
	movf	ADRESL, W		; Leemos bits menos peso del A/D
	bcf	STATUS, RP0		; Banco 0
	movwf	Byte_L			; Repetimos para convertir a BCD
	movwf	F2L
	movf	ADRESH, W		; Leemos bits más peso del A/D
	movwf	Byte_H			; Los cargamos para convertir a BCD
	movwf	F2H
	loadd	F1H, F1L
	MULT_16x16_FASTEST F2H,F2L, F1H,F1L, p4,p3,p2,p1,t1
	call	b2bcd			; Codificamos la multiplicación a BCD
	call	MOSTRARV		; Mostramos el resultado en el LCD
	;call	Bits16_BCD		; Codificamos valor convertido a BCD
	;call	VISUALIZAR
	bsf	STATUS, RP0		; Banco 1
	bcf	STATUS, RP1			
	movf	ADRESL, W		; Leemos bits menos peso del A/D
	bcf	STATUS, RP0		; Banco 0
	movwf	Byte_L			; Repetimos para actualizar PWM
	movf	ADRESH, W		; Leemos bits más peso del A/D
	movwf	Byte_H			; Los cargamos para actualizar PWM
	bsf	PORTC, 7		; Iluminamos led 7
	call	ADCTOCCP2
	bsf	PORTC, 6		; Iluminamos led 6
	return
	
INTER
	bcf	STATUS, RP0		; Banco 0
	bcf	STATUS, RP1		;
	btfsc   INTCON,T0IF		; Si T0IF = 0 salta, else
   	call    TMR0INT			; tratar int TMR0
	btfsc	PIR1, ADIF		; Si ADIF = 0 salta, else
	call	LEEADC			; tratar int ADIF
	btfss	ADCON0, GO		; Si GO = 1, salta, else
	call	WAIT20us		; Esperar e iniciar otra adquisición
	retfie

START
	clrf	PORTA			; Borramos salidas
	clrf	PORTB			; Borramos salidas
	clrf	PORTC			; Borramos salidas
	clrf	CCP2CON			; Apagamos módulo CCP2
	clrf	CCPR2L			; Duty cycle 0%
	clrf	TMR2			; Apagamos TMR2
	bsf	STATUS, RP0		; Banco 1
	bcf	STATUS, RP1		;
	movlw   b'10001110'		; AN0 E/S analogica
	movwf   ADCON1			; AN1:5 E/S digitales
	bcf	OPTION_REG, T0CS	; TMR0 temporizador	
	movlw	b'00000001'		; Set AN0 as input
	movwf	TRISA			; AN1-5 as outputs
	clrf	TRISB			; PortB digital 
	clrf	TRISC			; PortC salidas (RC1 -> PWM)
	movfw	PERIODO-1		; Carga el registro
	movwf	PR2			; de periodo
	call	INITAD
	bcf	STATUS, RP0		; Banco 0
	movlw	b'00001100'		; Configuración de CCP2
	movwf	CCP2CON			; en modo PWM
	movlw	b'0000110'		; Empezamos a contar con TMR2 con
	BSF	T2CON, TMR2ON 		; predivisor 1:16 y postdivisor 1:1
	call	UP_LCD
	call	LCD_INI
	movlw	b'00001100'
	call	LCD_REG
	call	WAIT20us		; Esperamos para adquirir la lectura

INFLOOP
	clrwdt				; Refresca el perro guardián
	nop
	nop
	nop
	goto	INFLOOP

	END


