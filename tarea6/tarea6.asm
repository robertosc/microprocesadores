;##############################################################################
;                                 Tarea #6
;   Fecha: 21 de enero del 2021
;   Autor: Roberto Sánchez y Luis guillermo Ramírez
;
;   Descripcion:
;##############################################################################
#include registers.inc


EOM:            equ $00 ;end of message
CR:             equ $0D ;carriage return
LF:             equ $0A ;line feed
NP:             equ $0C ;new page
SUB:            equ $1A ;control substitute
;------------------------------------------------------------------------------
;     Declaracion de las estructuras de datos y vectores de interrupcion
;------------------------------------------------------------------------------
;Vectores de interrupcion:
                org $FFD2
                dw ATD0_ISR ;direccion de la subrutina de servicio a interrupcion ATD0.
                org $FFD4
                dw SCI_ISR ;direccion de la subrutina de servicio a interrupcion SCI1.
                org $FFF0
                dw RTI_ISR  ;direccion de la subrutina de servicio a interrupcion RTI.

;Estructuras de datos:
                org $1010
NIVEL_PROM:     ds 2 ;almacena el promedio de las 6 mediciones a 10 bits del ATD.
NIVEL:          ds 1 ;almacena el valor redondeado a 8 bits de NIVEL_PROM en metros.
VOLUMEN:        ds 1 ;almacena el volumen del tanque en metros cubicos (m3).
CONT_RTI:       ds 1 ;permite generar la cadencia de 1 seg en el RTI para la transmision.

BCD_H:          ds 1 ;miles y centenas en BCD
BCD_L:          ds 1 ;decenas y unidades en BCD
LOW:            ds 1 ;variable temporal
Puntero:      ds 2 ;con este puntero se maneja la impresion caracter por caracter.
BANDERAS:          ds 1 ; X:X:X:X:X:MSG_SEL:MSG:DONE

;Mensajes:
MENSAJE:        db SUB
                fcc "                         UNIVERSIDAD DE COSTA RICA"
                db CR,LF
                fcc "                      ESCUELA DE INGENIERIA ELECTRICA"
                db CR,LF
                fcc "                             MICROPROCESADORES"
                db CR,LF
                fcc "                                   IE0623"
                db CR,LF,CR,LF
                fcc "                    VOLUMEN CALCULADO: "
CENTENAS:       ds 1
DECENAS:        ds 1
UNIDADES:       ds 1
                fcc " m3."
                db CR,LF,EOM

ALARMA_BAJO:      db CR,LF,CR,LF
                fcc "                    Alarma: El Nivel esta Bajo."
                db EOM

FULL:       fcc "                    Tanque vaciando, Bomba Apagada."
                db EOM
;*******************************************************************************
;                             Programa principal
;*******************************************************************************
;------------------------------------------------------------------------------
;                          Configuracion del hardware
;------------------------------------------------------------------------------
    ORG $2000
;Configuracion de la salida SAL: LED en puerto PORTB0.
    MOVB #$01,DDRB ;se configura PORTB0 como salida
    bset DDRJ,$02

;Configuracion del ATD0:
    MOVB #$C2,ATD0CTL2 ;habilita convertidor ATD0, borrado rapido de banderas y las interrupciones.
    LDAA #160

CONFIG_ATD:
    DBNE A,CONFIG_ATD ;10 us, tiempo de ATD.

    MOVB #$30,ATD0CTL3 ;ciclo de 6 conversiones
    MOVB #$10,ATD0CTL4 ;f_sample = 700 kHz con preescalador 16
    MOVB #$80,ATD0CTL5 ;justifica a la derecha

;Configuracion del RTI:
    MOVB #$54,RTICTL
    BSET CRGINT,#$80

;Configuracion del Serial Communication Interface (SCI1):
    MOVW #39,SC1BDH 		;Baud Rate = 38400
    MOVB #%00000000,SC1CR1 	;M = 0, ParityEnable = 0
    MOVB #%00001000,SC1CR2 	;TE = 1

    CLI
;------------------------------------------------------------------------------
;                       Inicializacion de variables
;------------------------------------------------------------------------------
    LDS #$4000  ;inicializa el stack
    MOVB #100,CONT_RTI
    CLR BANDERAS
;------------------------------------------------------------------------------
MAIN:
    JSR CALCULO_NIVEL
    JSR BIN_BCD
    JSR BCD_ASCII
    BRA MAIN
;*******************************************************************************


;------------------------------------------------------------------------------
CALCULO_NIVEL:
        Ldd #1023 ;Maximo numero de 10 bits
        Ldx #255   ; Maximo numero de 8 bits
        Idiv  ;x=4   ;Obtenemos la razon entre estas dos escalas (4)
        Ldd NIVEL_PROM
        Idiv ;x=NIVEL_PROM/4
        Xgdx ;D=NIVEL_PROM/4
        Ldaa #20  ;Maximo de nivel medido
        Mul
        Ldx #255
        Idiv  ;Con eso se obtiene el valor nivel en en X
        Xgdx  ;Se pasa a D
        Cmpb #15 ;Maxima altura permitida es 15
        Bls get_V
        Ldab #15

get_V:
        Stab NIVEL
        ldaa #7 ;Area real es 7.07 pero se redondea
        Mul ;V= A*H
        Stab VOLUMEN

        ;Realizamos las comparaciones de nivel
        cmpb #95  ;V>=95
        bhs LED_OFF

        cmpb #16 ;V<=95
        bls LED_ON

        cmpb #32 ;95>=V>=32
        bhs NO_PRINT
        
        
        ;32>=V>=16
        brclr BANDERAS,$04,PRINT
        bra NO_PRINT

LED_ON:
        movb #$00,PTJ        ;habilita led
	bset PORTB %00000001 ;enciende la bomba
        bclr BANDERAS %00000100 ;MSG_SEL <--- 0 para indicar que el mensaje a imprimir es el de alarma
        bra PRINT

LED_OFF:
	movb #$02,PTJ ;deshabilida leds
        bclr PORTB %00000001 ;se apaga la bomba
        bset BANDERAS %00000100 ;MSG_SEL <--- 1 para indicar que el mensaje a imprimir es el de tanque lleno
PRINT:
        bset BANDERAS %00000010 ;MSG <-- 1 para indicar que hay que imprimir otro mensaje
         rts

NO_PRINT:
        bclr BANDERAS %00000010 ;MSG <-- 0 para indicar que NO hay que imprimir otro mensaje
        rts

;------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

BIN_BCD:
                ldy #15
                clra
                ldab VOLUMEN

                clr BCD_H
                clr BCD_L

LOOP_BINBCD:
                lsld       ;se realiza el desplazamiento
                rol BCD_L
                rol BCD_H
                pshd       ;guardamos d en pila para usar a y b
                ldaa #$0F
                anda BCD_L
                cmpa #5
                blo UNIDADES_CONV
                adda #3
UNIDADES_CONV:
                staa LOW
                ldaa #$F0 ;mascara
                anda BCD_L ;aplica mascara
                cmpa #$50  ;compara con 5
                blo DECENAS_CONV
                adda #$30
DECENAS_CONV:
                adda LOW
                staa BCD_L
                ldaa #$0F ;mascara
                anda BCD_H ;se obtienen las centenas
                cmpa #5
                blo CENTENAS_CONV
                adda #3
CENTENAS_CONV:
                staa LOW
                ldaa #$F0 ;mascara
                anda BCD_H ;se obtienen los miles
                cmpa #$50
                blo MILES_CONV
                adda #$30
MILES_CONV:
                adda LOW
                staa BCD_H
                dey
                puld          ;recupera d
                bne LOOP_BINBCD
                lsld
                rol BCD_L
                rol BCD_H
                rts

;-------------------------------------------------------------------------------
BCD_ASCII:
            ldaa #$0F ;mascara
            anda BCD_L ;unidades
            adda #$30 ;conversion, basat con sumar $30
            staa UNIDADES
            ldab #$F0 ;mascara
            andb BCD_L ;decenas
            lsrb
            lsrb
            lsrb
            lsrb ;desplazamos
            addb #$30
            stab DECENAS
            ldaa #$0F
            anda BCD_H ;centenas
            adda #$30
            staa CENTENAS
            rts

;------------------------------------------------------------------------------
ATD0_ISR:
    Ldd ADR00H
    Addd ADR01H
    Addd ADR02H
    Addd ADR03H
    Addd ADR04H
    Addd ADR05H
    ;Tenemos en RR1 la suma de los 6 numeros
    Ldx #6
    Idiv ;X = D/X para el promedio
    Stx NIVEL_PROM ;Dado por el enunciado para guardar el promedio
    Movb #$80,ATD0CTL5 ;escribimos para ques se repita el proceso
    Rti
;------------------------------------------------------------------------------

RTI_ISR:
    Bset CRGFLG %10000000  ;reinicia la bandera de interrupcion
    Tst CONT_RTI
    Beq REFRESCAR
    Dec CONT_RTI
    Bra FIN_RTI
REFRESCAR:
    Movb #100,CONT_RTI ;Volvemos al contador con 100
    Bset SC1CR2 %01000000 ; TCIE = 1
    Movw #MENSAJE,Puntero ;Puntero al inicio del mensaje
    ;Iniciamos el proceso de transmision
    Ldaa SC1SR1
    Movb #NP,SC1DRL ;Transmitimos un new page solo para empezar el proceso

FIN_RTI:
    Rti

;------------------------------------------------------------------------------
SCI_ISR:

    ldx Puntero         ;Puntero para imprimir
    ldaa 1,X+             ; dato a imprimir
    cmpa #EOM                 ; revisa que o sea final
    beq DET_MSG 	;final
    ldab SC1SR1 ;inicia transmision
    staa SC1DRL
    stx Puntero 	;puntero de impresion.
    bra FIN_SCI 	;se termina la subrutina y se espera a que vuelva a interrumpir luego de transmitir el byte.

DET_MSG:

    brset BANDERAS,$01,SCI_OFF ;Revisa si ya terminó de imprimir
    brclr BANDERAS,$02,SCI_OFF
    brclr BANDERAS,$04,ALARMA ;Se debe imprimir es el de alarma.

    bset BANDERAS,$01 ;Pone bandera de final
    movw #FULL,Puntero ;Tanque lleno

    bra FIN_SCI

ALARMA:
    movb #$01,PORTB;enciende el led
    movw #ALARMA_BAJO,Puntero ;se carga el mensaje de alarma
    bset BANDERAS $01 ;Pone bandera de final

    bra FIN_SCI

SCI_OFF:
    bclr SC1CR2,$40 ;Se deshabilita la interrupcion SPI. TCIE = 0.
    bclr BANDERAS,$01 ;No terminada

FIN_SCI:
    rti

