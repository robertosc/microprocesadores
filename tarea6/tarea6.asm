
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
BCD_H:          ds 1
BCD_L:          ds 1
LOW:            ds 1
VOLUMEN:        ds 1
NIVEL:          ds 1
BANDERAS:       ds 1
;vict
CONT_RTI:       ds 1
PRINT_PTR:      DS 2

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
    JSR CALCULO
    JSR BIN_BCD
    JSR BCD_ASCII
    JSR DETERMINAR_NIVEL_ESTADO
    BRA MAIN_LOOP
;*******************************************************************************

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

BIN_BCD:
                ldx #15
                     clra
                    ldab VOLUMEN

                    clr BCD_H
                    clr BCD_L
            
LOOP_BINBCD:
                    lsld       ;se realiza el desplazamiento
                rol BCD_L
                    rol BCD_H
                     pshd
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
                        puld
                dex
                bne LOOP_BINBCD
                lsld
                    rol BCD_L
                    rol BCD_H
                     rts

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
    
    
DETERMINAR_NIVEL_ESTADO:
        ldaa VOLUMEN
        cmpa #95
        bhs LED_OFF
        
        cmpa #16
        bls LED_ON
        
        cmpa #32
        bhs NO_PRINT
        
        brclr BANDERAS,$04,PRINT
        bra NO_PRINT

LED_ON:
        BSET PORTB %00000001 ;enciende la bomba
        BCLR BANDERAS %00000100 ;MSG_SEL <--- 0 para indicar que el mensaje a imprimir es el de alarma
        BRA PRINT

LED_OFF:
        BCLR PORTB %00000001 ;se apaga la bomba
        BSET BANDERAS %00000100 ;MSG_SEL <--- 1 para indicar que el mensaje a imprimir es el de tanque lleno
PRINT:
        BSET BANDERAS %00000010 ;MSG <-- 1 para indicar que hay que imprimir otro mensaje
         rts

NO_PRINT:
        BCLR BANDERAS %00000010 ;MSG <-- 0 para indicar que NO hay que imprimir otro mensaje
        rts



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

RTI_ISR:
    BSET CRGFLG %10000000  ;reinicia la bandera de interrupcion
    TST CONT_RTI
    BEQ REFRESCAR
    DEC CONT_RTI
    BRA FIN_RTI
REFRESCAR:
    MOVW #MENSAJE,PRINT_PTR ;se carga el inicio del mensaje al puntero de impresion.
    BSET SC1CR2 %01000000 ;Interrupciones de transmisor: TCIE = 1
    LDAA SC1SR1 ;primer paso para iniciar transmision
    MOVB #NP,SC1DRL ;se escribe el dato a transmitir para iniciar la transmision.
    MOVB #100,CONT_RTI ;se recarga el contador para la cadencia de 1 Hz.
FIN_RTI:
    RTI
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;   Subrutina SCI_ISR: esta subrutina realiza la transmision, 1 byte por cada
;     interrupcion, a traves de la interfaz SC1. Cuando termina de transimitir
;     todo el mensaje se deshabilita la interrupcion.
;------------------------------------------------------------------------------
SCI_ISR:
    LDX PRINT_PTR ;se carga el puntero de impresion de bytes
    LDAA 1,X+ ;se obtiene el caracter que se debe imprimir.
    CMPA #EOM ;se comprueba que el caracter no sea el final de la hilera de caracteres
    BEQ MENSAJES ;si es el final, se deben revisar los casos especiales de impresion
    LDAB SC1SR1 ;primer paso para iniciar transmision
    STAA SC1DRL ;se escribe el dato a transmitir para iniciar la transmision.
    STX PRINT_PTR ;se almacena el puntero de impresion.
    BRA FIN_SCI ;se termina la subrutina y se espera a que vuelva a interrumpir luego de transmitir el byte.
MENSAJES:
    BRSET BANDERAS %00000001 SCI_OFF ;Si DONE = 1 no hay nada mas por imprimir y se debe desahibilitar la interrupcion SCI
    BRCLR BANDERAS %00000010 SCI_OFF ;Si MSG = 0 es porque no hay que imprimir ningun mensaje adicional.
    BRCLR BANDERAS %00000100 PRINT_ALARM ;Si MSG_SEL = 0 el mensaje que se debe imprimir es el de alarma.

    MOVW #MSG_FULL,PRINT_PTR ;se carga el mensaje de tanque lleno
    BSET BANDERAS %00000001 ;DONE <-- 1 para que en el proximo caracter EOM se termine el refrescamiento de la pantalla.
    BRA FIN_SCI

PRINT_ALARM:
    MOVW #MSG_ALARM,PRINT_PTR ;se carga el mensaje de alarma
    BSET BANDERAS %00000001 ;DONE <-- 1 para que en el proximo caracter EOM se termine el refrescamiento de la pantalla.
    BRA FIN_SCI

SCI_OFF:
    BCLR SC1CR2 %01000000 ;Se deshabilita la interrupcion SPI. TCIE = 0.
    BCLR BANDERAS %00000001 ;DONE <-- 0 para que la bandera quede lista para el proximo refrescamiento

FIN_SCI:
    RTI
;------------------------------------------------------------------------------
