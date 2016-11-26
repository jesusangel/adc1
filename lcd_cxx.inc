
;                               LCD_CXX.INC
;
;						Autor: Mikel Etxebarria
;			(c) Ingenier�a de Microsistemas Programados S.L.
;						www.microcontroladores.com
;								Bilbao 2006
;						
;El conjunto de rutinas que se presentan a continuaci�n permiten realizar las tareas b�sicas 
;de control del m�dulo de visualizaci�n LCD. Este fichero se debe incluir en los futuros
;programas fuente mediante la directiva INCLUDE
;
;Las rutinas est�n adaptadas a las velocidades m�s lentas de los nuevos m�dulo LCD, en las cuales 
;el tiempo de activaci�n (Tc) de la se�al ENABLE es de unos 40 uS. La velocidad de trabajo del 
;PIC se supone de 4MHz y se realiza un interfase de 8 bits
;
;Se supone que RA1, RA2 y RA3 est�n conectadas con las se�ales RS, R/W y E respectivamente.
;RB0-RB7 est�n conectadas a las l�neas de datos

#define ENABLE       bsf PORTA,3        ;Activa se�al E
#define DISABLE      bcf PORTA,3        ;Desactiva se�al E
#define LEER         bsf PORTA,2        ;Pone LCD en Modo RD
#define ESCRIBIR     bcf PORTA,2        ;Pone LCD en Modo WR
#define OFF_COMANDO  bcf PORTA,1        ;Desactiva RS (modo comando)
#define ON_COMANDO   bsf PORTA,1        ;Activa RS (modo dato

    CBLOCK	Lcd_var		;Inicio de las variables. Ser� la primera						
    Lcd_Temp_1			;direcc�n libre disponible
    Lcd_Temp_2
    ENDC											

;*************************************************************************
;UP_LCD: Configuraci�n PIC para el LCD.
;
UP_LCD          bsf     STATUS,RP0      ;Banco 1
                clrf    PORTB           ;RB <0-7> salidas digitales
                movlw	b'11110001'
				movwf	PORTA	      	;Puerta A salidas
                bcf     STATUS,RP0      ;Banco 0
                OFF_COMANDO             ;RS=0
                DISABLE                 ;E=0
                return
;
;**************************************************************************
;LCD_BUSY: Lectura del Flag Busy y la direcci�n.
;
LCD_BUSY        LEER                    ;Pone el LCD en Modo RD
                bsf     STATUS,RP0           
                movlw   H'FF'
                movwf   PORTB           ;Puerta B como entrada
                bcf     STATUS,RP0      ;Selecciona el banco 0
                ENABLE                  ;Activa el LCD
                nop
LCD_BUSY_1		btfsc   PORTB,7         ;Chequea bit de Busy
                goto    LCD_BUSY_1		;
                DISABLE                 ;Desactiva LCD
                bsf     STATUS,RP0                               
                clrf    PORTB           ;Puerta B salida
                bcf     STATUS,RP0                              
                ESCRIBIR                ;Pone LCD en modo WR
                return

;**************************************************************************
;LCD_E: Pulso de Enable. En los nuevos LCD's esta se�al debe estar a "0" unos 40uS 
;antes de volver a ponerse a "1".
;
LCD_E           ENABLE                  ;Activa E
				nop
                DISABLE                 ;Desactiva E
				movlw	.14
				movwf	Lcd_Temp_1
LCD_E_1			decfsz	Lcd_Temp_1,F	;Pierde unos 40 uS para la constante de tiempo Tc
				goto	LCD_E_1			;de los nuevos m�dulos LCD de winteck
				return
;
;**************************************************************************
;LCD_DATO: Escritura de datos en DDRAM o CGRAM. Env�a el dato presente en el W
;
LCD_DATO        OFF_COMANDO             ;Desactiva RS (modo comando)
                movwf   PORTB           ;Valor ASCII a sacar por portb
                call    LCD_BUSY        ;Espera a que se libere el LCD
                ON_COMANDO              ;Activa RS (modo dato).
                goto    LCD_E           ;Genera pulso de E

;**************************************************************************
;LCD_REG: Escritura de comandos en el LCD. Env�a el comando presente en el W

LCD_REG         OFF_COMANDO             ;Desactiva RS (modo comando)
                movwf   PORTB           ;C�digo de comando.
                call    LCD_BUSY        ;LCD libre?.
                goto    LCD_E           ;SI.Genera pulso de E.

;***************************************************************************
;LCD_INI: inicializaci�n del LCD enviando el comando "Function Set" 3 veces consecutivas
;con un intervalo de unos 5 mS. El LCD queda borrado y el cursor en la primera posici�n

LCD_INI			movlw	b'00111000'
				call	LCD_REG			;C�digo de instrucci�n
				call	LCD_DELAY		;Temporiza
				movlw	b'00111000'
				call	LCD_REG			;C�digo de instrucci�n
				call	LCD_DELAY		;Temporiza
				movlw	b'00111000'
				call	LCD_REG			;C�digo de instrucci�n
				call	LCD_DELAY		;Temporiza
				movlw   b'00000001'     ;Borra LCD y Home.
                call    LCD_REG
                return

;*****************************************************************************
;LCD_DELAY: Rutina de temporizaci�n de unos 5 mS a 4MHz. Se emplean las variables Lcd_Temp_1
;y LCD_Temp_2 en lugar del TMR0. Este queda libre para las aplicaciones del usuario

LCD_DELAY:		clrwdt
                movlw	.7
                movwf	Lcd_Temp_1
                clrf	Lcd_Temp_2
LCD_DELAY_1:	decfsz	Lcd_Temp_2,F
				goto	LCD_DELAY_1
				decfsz	Lcd_Temp_1,F
				goto	LCD_DELAY_1
				return
            
