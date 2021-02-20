;##############################################################################
;                                 Tarea #6
;   Fecha: 13 de noviembre del 2020.
;   Autor: Victor Manuel Yeom Song
;
;   Descripcion: este programa simula el control de nivel de un tanque. Realiza
;     mediciones de una entrada analogica y la convierte a digital, esta entrada
;     representa el nivel. A partir del nivel se obtiene el volumen y con el
;     volumen se enciende y apaga un led segun el estado del tanque, para
;     simular el encendido y apagado de la bomba de llenado del tanque. Adicionalmente,
;     el volumen del tanque junto con otros mensajes son transmitidos a una terminal
;     (que debe ser configurada por aparte) en la computadora a traves de la
;     interfaz de comunicacion serial SC1.
;##############################################################################
#include registers.inc

HMAX:           EQU 15 ;metros
AREA_M2_x10:    EQU 71 ;metros cuadrados escalados por 10 (el area de la base es 7.06 m2)
PLENA_ESCALA:   EQU 20 ;metros
CUANTOS_10:     EQU 1024 ;conversiones a 10 bits --> 2^10 cuantos.
CUANTOS_8:      EQU 256 ;conversiones a 8 bits --> 2^8 cuantos.
PORCIENTO_15:   EQU 16 ;15% del volumen maximo 105.9
PORCIENTO_30:   EQU 32 ;30% del volumen maximo 105.9
PORCIENTO_90:   EQU 95 ;90% del volumen maximo 105.9

EOM:            EQU $00 ;end of message
CR:             EQU $0D ;carriage return
LF:             EQU $0A ;line feed
NP:             EQU $0C ;new page
SUB:            EQU $1A ;control substitute
;------------------------------------------------------------------------------
;     Declaracion de las estructuras de datos y vectores de interrupcion
;------------------------------------------------------------------------------
;Vectores de interrupcion:
                ORG $FFD2
                DW ATD0_ISR ;direccion de la subrutina de servicio a interrupcion ATD0.
                ORG $FFD4
                DW SCI_ISR ;direccion de la subrutina de servicio a interrupcion SCI1.
                ORG $FFF0
                DW RTI_ISR  ;direccion de la subrutina de servicio a interrupcion RTI.

;Estructuras de datos:
                ORG $1010
NIVEL_PROM:     DS 2 ;almacena el promedio de las 6 mediciones a 10 bits del ATD.
NIVEL:          DS 1 ;almacena el valor redondeado a 8 bits de NIVEL_PROM en metros.
VOLUMEN:        DS 1 ;almacena el volumen del tanque en metros cubicos (m3).
CONT_RTI:       DS 1 ;permite generar la cadencia de 1 seg en el RTI para la transmision.

BCD_H:          DS 1 ;miles y centenas en BCD
BCD_L:          DS 1 ;decenas y unidades en BCD
LOW:            DS 1 ;variable temporal
PRINT_PTR:      DS 2 ;con este puntero se maneja la impresion caracter por caracter.
FLAGS:          DS 1 ; X:X:X:X:X:MSG_SEL:MSG:DONE

;Mensajes:
MENSAJE:        DB SUB
                FCC "                         UNIVERSIDAD DE COSTA RICA"
                DB CR,LF
                FCC "                      ESCUELA DE INGENIERIA ELECTRICA"
                DB CR,LF
                FCC "                             MICROPROCESADORES"
                DB CR,LF
                FCC "                                   IE0623"
                DB CR,LF,CR,LF
                FCC "                    VOLUMEN CALCULADO: "
CENTENAS:       DS 1
DECENAS:        DS 1
UNIDADES:       DS 1
                FCC " m3."
                DB CR,LF,EOM

MSG_ALARM:      DB CR,LF,CR,LF
                FCC "                    Alarma: El Nivel esta Bajo."
                DB EOM

MSG_FULL:       FCC "                    Tanque vaciando, Bomba Apagada."
                DB EOM
;*******************************************************************************
;                             Programa principal
;*******************************************************************************
;------------------------------------------------------------------------------
;                          Configuracion del hardware
;------------------------------------------------------------------------------
    ORG $2000
;Configuracion de la salida SAL: LED en puerto PORTB0.
    MOVB #$01,DDRB ;se configura PORTB0 como salida

;Configuracion del ATD0:
    MOVB #$C2,ATD0CTL2 ;habilita convertidor ATD0, borrado rapido de banderas y las interrupciones.
    LDAA #160
CONFIG_ATD:
    DBNE A,CONFIG_ATD ;3ciclosCLK * 160cuentas * (1/48 MHz) = 10 us. Tiempo requerido para que inicie el ATD.
    MOVB #$30,ATD0CTL3 ;ciclo de 6 conversiones
    MOVB #$10,ATD0CTL4 ;conversion a 10 bits, tiempo final de muestreo es 2 periodos, f_sample = 700 kHz (PRS~16)
    MOVB #$80,ATD0CTL5 ;justifica a la derecha. Sin signo. No multiplexacion. Canal 7 es el del pot. Escritura inicia ciclo.

;Configuracion del RTI:
    MOVB #$54,RTICTL
    BSET CRGINT %10000000

;Configuracion del Serial Communication Interface (SCI1):
    MOVW #39,SC1BDH ;Baud Rate = 38400
    MOVB #%00000000,SC1CR1 ;M = 0, ParityEnable = 0
    MOVB #%00001000,SC1CR2 ;TE = 1

    CLI
;------------------------------------------------------------------------------
;                       Inicializacion de variables
;------------------------------------------------------------------------------
    LDS #$4000  ;inicializa el stack
    MOVB #100,CONT_RTI
    CLR FLAGS
;------------------------------------------------------------------------------
MAIN_LOOP:
    JSR CALCULO
    JSR BIN_BCD_9999
    JSR BCD_ASCII_999
    JSR DETERMINE_FLAGS
    BRA MAIN_LOOP
;*******************************************************************************


;------------------------------------------------------------------------------
;   Subrutina CALCULO: la variable NIVEL_PROM contiene la medicion del tanque a
;     una resolucion de 10 bits, esta subrutina convierte este valor a una escala
;     con resolucion de 8 bits y lo guarda en la variable NIVEL. Ademas, a partir
;     del NIVEL se obtiene el volumen del tanque en metros cubicos y se guarda
;     en la variable VOLUMEN.
;------------------------------------------------------------------------------
CALCULO:
    LDD #CUANTOS_10-1
    LDX #CUANTOS_8-1
    IDIV ;X = D/X = 1023/255 ~ 4.   Los cuantos a 8 bits valen 4 veces mas en escala que los cuantos a 10 bits.
    LDD NIVEL_PROM
    IDIV ;X = D/X = NIVEL_PROM/4. Se convierte la medicion a 10 bits en su valor a 8 bits.
    TFR X,D
    LDAA #PLENA_ESCALA
    MUL ;D = A*B = PLENA_ESCALA*NIVEL
    LDX #CUANTOS_8-1
    IDIV ;X = D/X = NIVEL*[PLENA_ESCALA/(CUANTOS_8-1)] = NIVEL*[RESOLUCION] = ALTURA en metros.
    TFR X,D
    CMPB #HMAX
    BLS CALC_VOL
    LDAB #HMAX ;este programa simula el sensor del tanque con el potenciometro, por lo  que si se podria tener la medicion maxima de 20 metros. Como el tanque es de 15 metros maximo se fuerza que este sea el nivel si la medicion es mayor.
CALC_VOL:
    STAB NIVEL ;se almacena el valor en metros de la medicion del nivel, a una resolucion de 8 bits.
    LDAA #AREA_M2_x10 ;Se carga el valor del area, precalculado a mano pues es constate.
    MUL ;D = A*B = AREA_M2_x10*NIVEL = VOLUMEN*10
    LDX #10
    IDIV ;X = D/X = VOLUMEN*10/10 = VOLUMEN
    TFR X,D
    STAB VOLUMEN
    RTS
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;   Subrutina BIN_BCD_9999: esta subrutina convierte un valor binario entre 0 y 9999
;     a BCD. Cada digitio en BCD requiere un nibble, por lo que se usan las variables
;     BCD_H y BCD_L para almacenar cada uno de ellos. La variable a convertir es VOLUMEN.
;------------------------------------------------------------------------------
BIN_BCD_9999:
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
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;   Subrutina BCD_ACII_999: esta subrutina toma un numero en BCD de tres digitos
;     y convierte cada uno de sus digitos en su correspondiente codificacion ASCII.
;     Se guardan en las variables CENTENAS, DECENAS y UNIDADES.
;------------------------------------------------------------------------------
BCD_ASCII_999:
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

;------------------------------------------------------------------------------
;   Subrutina DETERMINE_FLAGS: a partir del VOLUMEN calculado, esta subrutina
;     activa el LED cuando se esta por debajo del 15% y lo desactiva cuando
;     se esta por encima de 90%. Ademas modifica las banderas MSG y MSG_SEL para
;     que cuando se refresque la terminal se impriman los mensajes de alarma y
;     tanque lleno cuando corresponda.
;------------------------------------------------------------------------------
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

;------------------------------------------------------------------------------
;   Subrutina RTI_ISR: el canal 5 del output compare interrumpe a un periodo
;     de 10 ms y por medio de la variable CONT_RTI se logra una cadencia de 1 Hz
;     en la cual se refresca la informacion de la terminal. Para hacer esto,
;     cuando el contador CONT_RTI (precargado con 100) alcanza un valor de 0, se
;     habilita la interrupcion del SCI1 y se inicia una transmision.
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
    BRSET FLAGS %00000001 SCI_OFF ;Si DONE = 1 no hay nada mas por imprimir y se debe desahibilitar la interrupcion SCI
    BRCLR FLAGS %00000010 SCI_OFF ;Si MSG = 0 es porque no hay que imprimir ningun mensaje adicional.
    BRCLR FLAGS %00000100 PRINT_ALARM ;Si MSG_SEL = 0 el mensaje que se debe imprimir es el de alarma.

    MOVW #MSG_FULL,PRINT_PTR ;se carga el mensaje de tanque lleno
    BSET FLAGS %00000001 ;DONE <-- 1 para que en el proximo caracter EOM se termine el refrescamiento de la pantalla.
    BRA FIN_SCI

PRINT_ALARM:
    MOVW #MSG_ALARM,PRINT_PTR ;se carga el mensaje de alarma
    BSET FLAGS %00000001 ;DONE <-- 1 para que en el proximo caracter EOM se termine el refrescamiento de la pantalla.
    BRA FIN_SCI

SCI_OFF:
    BCLR SC1CR2 %01000000 ;Se deshabilita la interrupcion SPI. TCIE = 0.
    BCLR FLAGS %00000001 ;DONE <-- 0 para que la bandera quede lista para el proximo refrescamiento

FIN_SCI:
    RTI
;------------------------------------------------------------------------------
