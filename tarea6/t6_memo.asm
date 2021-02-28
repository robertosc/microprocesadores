
#include registers.inc

EOM:            EQU $00 ;end of message
CR:             EQU $0D ;carriage return
LF:             EQU $0A ;line feed
NP:             EQU $0C ;new page
SUB:            EQU $1A ;control substitute

;Definicion de vectores:
                Org $FFD2
                Dw ATD0_ISR ;direccion de la subrutina de servicio a interrupcion ATD0.
                ;Org $FFD4
                ;Dw SCI_ISR ;direccion de la subrutina de servicio a interrupcion SCI1.
                ;Org $FFF0
                ;Dw RTI_ISR  ;direccion de la subrutina de servicio a interrupcion RTI.

;Estructuras de datos:
               Org $1010

NIVEL_PROM: Ds 2
NIVEL:      Ds 1
VOLUMEN:    Ds 1


;Mensajes:
MENSAJE:        Db SUB
                Fcc "                         UNIVERSIDAD DE COSTA RICA"
                Db CR,LF
                Fcc "                      ESCUELA DE INGENIERIA ELECTRICA"
                Db CR,LF
                Fcc "                             MICROPROCESADORES"
                Db CR,LF
                Fcc "                                   IE0623"
                Db CR,LF,CR,LF
                Fcc "                            VOLUMEN CALCULADO: "
CENTENAS:       Ds 1
DECENAS:        Ds 1
UNIDADES:       Ds 1
                Fcc " m3."
                Db CR,LF,EOM

MSG_ALARM:      Db CR,LF,CR,LF
                Fcc "                    Alarma: El Nivel esta Bajo."
                Db EOM

MSG_FULL:       Fcc "                    Tanque vaciando, Bomba Apagada."
                Db EOM
;*******************************************************************************
;                             Programa principal
;*******************************************************************************
;------------------------------------------------------------------------------
;                          Configuracion del hardware
;------------------------------------------------------------------------------
    Org $2000


    ;Configuracion del ATD0:
    Movb #$C2,ATD0CTL2 ;habilita convertidor ATD0, borrado rapido de banderas y las interrupciones.
    Ldaa #160
CONFIG_ATD:
    Dbne A,CONFIG_ATD ;3ciclosCLK * 160cuentas * (1/48 MHz) = 10 us. Tiempo requerido para que inicie el ATD.
    Movb #$30,ATD0CTL3 ;ciclo de 6 conversiones
    Movb #$10,ATD0CTL4 ;conversion a 10 bits, tiempo final de muestreo es 2 periodos, f_sample = 700 kHz (PRS~16)
    Movb #$80,ATD0CTL5 ;justifica a la derecha. Sin signo. No multiplexacion. Canal 7 es el del pot. Escritura inicia ciclo.




    Cli
;------------------------------------------------------------------------------
;                       Inicializacion de variables
;------------------------------------------------------------------------------
    LDS #$4000  ;inicializa el stack
    Ldx #0
    ;MOVB #100,CONT_RTI
    ;CLR FLAGS
;------------------------------------------------------------------------------
MAIN_LOOP:
    ;JSR CALCULO
    ;JSR BIN_BCD_9999
    ;JSR BCD_ASCII_999
    ;JSR DETERMINE_FLAGS
    BRA MAIN_LOOP
;*******************************************************************************


;------------------------------------------------------------------------------
;   Subrutina ATD0_ISR: luego de que el ATD realiza las 6 mediciones al canal 0
;     esta subruina calcula el promedio de esas mediciones y lo guarda en la
;     variable NIVEL_PROM. Ademas inicia el siguiente ciclo de conversion.
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

CALCULO:
        Ldd #1023
        Ldx #255
        Idiv  ;x=4
        Ldd NIVEL_PROM
        Idiv ;x=NIVEL_PROM/4
        Xgdx ;A=0, B= NIVEL_PROM/4
        Ldaa #20
        Mul
        Ldx #255
        Idiv
        Cpx #15 ;Maxima altura
        Bls get_V
        Ldx #15

get_V:  Xgdx  ;Tenemos en B el valor del nivel
        Stab NIVEL
        ldaa #7 ;Area real es 7.07 pero se redondea
        Mul
        Std VOLUMEN
        Rts
