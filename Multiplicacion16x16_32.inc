;************************************************************
;*** Rutina de multiplicacion de 16bits x 16bits = 32bits ***
;************************************************************

;<Factor1b;Factor1a>x<Factor2b;Factor2a>=<Resultado_d;Resultado_c;Resultado_b;Resultado_a>
; No me ha funcionado bien

;RUTINA:
Multiplicacion
	clrf	Resultado_a
	clrf	Resultado_b
	clrf 	Resultado_c
	clrf 	Resultado_d
	bsf 	Resultado_a,7
	clrf 	Flag
	btfss 	Factor1b,7
	goto 	mult_1
	comf 	Factor1b,f
	comf 	Factor1a,f
	incf 	Factor1a,f
	btfsc 	STATUS,Z
	incf 	Factor1b,f
	incf 	Flag,f
mult_1
	btfss 	Factor2b,7
	goto 	mult_2
	comf 	Factor2b,f
	comf	Factor2a,f
	incf	Factor2a,f
	btfsc 	STATUS,Z
	incf 	Factor2b,f
	incf 	Flag
mult_2	
	rrf 	Factor1b,f
	rrf 	Factor1a,f
	btfss 	STATUS,C
	goto 	mult_3
	movf 	Factor2a,w
	addwf 	Resultado_b,f
	movf 	Factor2b,w
	btfsc 	STATUS,C
	incfsz 	Factor2b,w
	addwf 	Resultado_c,f
	btfsc 	STATUS,C
	incf 	Resultado_d,f
	bcf 	STATUS,C
mult_3
	btfss 	Factor1a,7
	goto 	mult_4
	movf 	Factor2a,w
	addwf 	Resultado_c,f
	btfsc 	STATUS,C
	incf 	Resultado_d,f
	movf	Factor2b,w
	addwf 	Resultado_d,f
mult_4	
	rrf 	Resultado_d,f
	rrf 	Resultado_c,f
	rrf 	Resultado_b,f
	rrf 	Resultado_a,f
	btfss 	STATUS,C
	goto 	mult_2
	btfss 	Flag,0
	goto 	Salir
	comf 	Resultado_d,f
	comf 	Resultado_c,f
	comf 	Resultado_b,f
	comf 	Resultado_a,f
	incf 	Resultado_a,f
	btfsc 	STATUS,Z
	incf 	Resultado_b,f
	btfsc 	STATUS,Z
	incf 	Resultado_c,f
	btfsc 	STATUS,Z
	incf 	Resultado_d,f
Salir
	return
