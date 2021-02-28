;##############################################################################
;                          Proyecto Final: Runmeter623
;   Fecha: 30 de noviembre del 2020.
;   Autor: Victor Yeom Song
;
;   Descripcion: El siguiente codigo para la tarjeta de entrenamiento dragon12 corresponde a un sistema de medicion de velocidad
;   y despliegue de informacion en un velodromo. Cuenta con 4 modos; el modo config para configurar la cantidad de vueltas que
;   se desean medir, el modo libre para tener al sistema en un tipo de modo ocioso, el modo competencia que mide la velocidad y
;   la cantidad de vueltas realizadas por un ciclista y el modo resumen que le muestra al ciclista su rendimiento en forma de la
;   velocidad promedio en la cantidad de vueltas medida. Todo el intercambio de informacion con el ciclista ocurre por medio del
;   despliegue de datos en la pantalla LCD y la pantalla de 7 segmentos, as? como el teclado matricial en el caso del modo config.
;
;##############################################################################
#include registers.inc

;------------------------------------------------------------------------------
;     Declaracion de las estructuras de datos y vectores de interrupcion
;------------------------------------------------------------------------------
;Vectores de interrupcion:
                ORG $3E52   ;direccion del vector de interrupcion ATD0.
                DW ATD_ISR  ;direccion de la subrutina de servicio a interrupcion ATD0.
                ORG $3E70   ;direccion del vector de interrupcion RTI.
                DW RTI_ISR  ;direccion de la subrutina de servicio a interrupcion RTI.
                ORG $3E4C   ;direccion del vector de interrupcion PTH.
                DW CALCULAR ;direccion de la subrutina de servicio a interrupcion PTH.
                ORG $3E66   ;direccion del vector de interrupcion OC4.
                DW OC4_ISR  ;direccion de la subrutina de servicio a interrupcion OC4.
                ORG $3E5E   ;direccion del vector de interrupcion TCNT.
                DW TCNT_ISR ;direccion de la subrutina de servicio a interrupcion TCNT.


;Estructuras de datos:
                ORG $1000
                        ;COMPE:X:X:
Banderas:       DS 1  ;Tiene el formato: COMPE:COMPE_STATE:X:CALC_TICKS:PANT_FLG:ARRAY_OK:TCL_LEIDA:TCL_LISTA.
                      ;MOD_PREV_H y MOD_PREV_L indican el modo de funcionamiento previo al que se utiliza, usado para funcionamiento de los modos competencia y libre
                      ;PANT_FLG indica el estado de las pantallas a utilizar por PANT_CTRL
                      ;ARRAY_OK indica que se presiono la tecla Enter y que en el arreglo ya se tienen todos los valores leidos.
                      ;TCL_LEIDA indica que ya se habia tenido una lectura del teclado y que se estaba esperando a que se diera la supresion de rebotes.
                      ;TCL_LISTA indica que luego de la supresion de rebotes se confirmo que si se presiono una tecla.

NumVueltas:     ds 1
ValorVueltas:   ds 1

MAX_TCL:        db 2  ;Maximo numero de teclas leidas
Tecla:          ds 1  ;Variable que almacena la tecla leida
Tecla_IN:       ds 1  ;Valor temporal tomado por el teclado
Cont_Reb:       ds 1  ;Contador para eliminar los probelemas de rebotes
Cont_TCL:       ds 1  ;Indice para el array de teclas
Patron:         ds 1  ;Indice para MUX TECLADO
Num_Array:      ds 2  ;Todas las teclas guardadas

BRILLO:         ds 1  ;Brillo elegido por el usuario de la pantalla
POT:            ds 1

TICK_EN:        ds 2
TICK_DIS:       ds 2

Veloc:          ds 1
Vueltas:        ds 1
VelProm:        ds 1

TICK_MED:       ds 2

BIN1:           ds 1  ;variable en binario de CantPQ y CUENTA
BIN2:           ds 1  ;variable en binario de AcmPQ
BCD1:           ds 1  ;Mismo valor que BIN1 pero en BCD
BCD2:           ds 1  ;Mismo valor que BIN2 pero en BCD

BCD_L:          ds 1
BCD_H:          ds 1
TEMP:           ds 1
LOW:            ds 1

DISP1:          ds 1  ;BCD2 para display de 7 segmentos (primer byte)
DISP2:          ds 1  ;BCD2 para display de 7 segmentos (segundo byte)
DISP3:          ds 1  ;Mismo caso que disp 1 y disp 2 pero para BCD1 (3 Y 4 RESPECTIVAMENTE)
DISP4:          ds 1

LEDS:           ds 1  ;PB1 para modo CONFIG, PB0 a modo RUN.
CONT_DIG:       ds 1  ;Habilitador de pantalla
CONT_TICKS:     ds 1  ;contador para el Output Compare
DT:             ds 1  ;ciclo de trabajo. DT = N-K
CONT_7SEG:      ds 2  ;contador de ticks de OC4

CONT_200:       ds 1

Cont_Delay:     ds 1
D2ms:           db 100  ;2 milisegundos
D260us:         db 12  ;modificar    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
D40us:          db 2  ;40 microsegundos
CLEAR_LCD:      db $01  ;comando para limpiar el LCD
ADD_L1:         db $80  ;direccion inicio de linea 1
ADD_L2:         db $C0  ;direccion inicio de linea 2

TICKS_TIME:     DS 2  ;Variable WORD utilizada para medir la cantidad de ticks que deben pasar para recorrer 100 m
SAVE_MED:       DS 2
                ORG $1040
Teclas:         DB $01,$02,$03,$04,$05,$06,$07,$08,$09,$0B,$00,$0E ;valores de las teclas

                ORG $1050
SEGMENT:        DB $3F,$06,$5B,$4F,$66,$6D,$7D,$07,$7F,$6F,$40,$00 ;patrones para el display de 7 segmentos de los digitos

                ORG $1060
iniDsp:         DB 4,FUNCTION_SET,FUNCTION_SET,ENTRY_MODE_SET,DISPLAY_ON

;LCD:
FUNCTION_SET:   EQU $28
ENTRY_MODE_SET: EQU $06
DISPLAY_ON:     EQU $0C
CLEAR_DISPLAY:  EQU $01
RETURN_HOME:    EQU $02
DDRAM_ADDR1:    EQU $80
DDRAM_ADDR2:    EQU $C0
EOM:            EQU $00

                ORG $1070 ;mensajes
MSG_LIBRE1:     fcc "  RunMeter 623  "
                db EOM
MSG_LIBRE2:     fcc "   MODO LIBRE   "
                db EOM

MSG_COMPE1:            fcc "M.COMPETENCIA"
                db EOM
MSG_COMPE2:     fcc "VUELTA    VELOC"
                db EOM

MSG_CONFIG1:    fcc "  MODO CONFIG   "
                db EOM
MSG_CONFIG2:    fcc "  NUM VUELTAS   "
                db EOM

MSG_ESPERA:     fcc "  Esperando...  "
                db EOM

MSG_CALCULANDO: fcc "  CALCULANDO... "
                db EOM

MSG_ALERTA1:    fcc "** VELOCIDAD **"
                db EOM

MSG_ALERTA2:    fcc "*FUERA DE RANGO*"
                db EOM

MSG_RESUMEN:    fcc "  MODO RESUMEN  "
                db EOM


;*******************************************************************************
;                             Programa principal
;*******************************************************************************
;------------------------------------------------------------------------------
;                          Configuracion del hardware
;------------------------------------------------------------------------------
    ORG $2000
;Configuracion RTI:
    BSET CRGINT %10000000 ;se habilita RTI
    MOVB #$31,RTICTL      ;periodo de 1.024 ms

;Configuracion keywakeup en puerto H:
    BCLR PIEH,$0F   ;se deshabilita keywakeup en PH0 y PH3.
    MOVB #$00,PPSH ;las interrupciones deben ocurrir en el flanco decreciente.

;Configuracion PH7-PH6 como entrada de proposito general por polling: (Dipswitch)
    BCLR DDRH,$C0

;Configuracion del teclado en puerto A:
    MOVB #$F0,DDRA        ;parte alta de A como salida y parte baja como entrada
    BSET PUCR %00000001   ;resistencias de pull-up en puerto A. Son necesarias para que haya un 1 en el PAD cuando no se presiona ningun boton del teclado.

;Configuracion del modulo de Timer como Output Compare en el Canal 4:
    BSET TSCR1 %10000000 ;Habilita modulo temporizacion
    BSET TSCR2 %00000100 ;Preescalador magnitud 16
    BSET TIOS %00010000 ;Habilida canal 4
    BSET TIE %00010000 ;Interrup en canal 4
    BCLR TCTL1 3 ;revisar

;Configuracion de los displays de 7 segmentos y los LEDS.
    MOVB #$FF,DDRB ;puerto core B se configura como salida de proposito general. (LEDS y SEGMENTOS)
    MOVB #$0F,DDRP ;parte baja de puerto P se configura como salida de proposito general. (~Habilitador Segmentos)
    BSET DDRJ %00000010 ;se configura bit 1 del puerto J como salida de proposito general . (~Habilitador LEDS)

;Configuracion de pantalla LCD
    MOVB #$FF,DDRK ;todos los pines del puerto K se configura como salida para controlar la LCD.

;Configuracion del ATD
    MOVB #$30,ATD0CTL3
    MOVB #$B3,ATD0CTL4
    MOVB #$87,ATD0CTL5

    CLI        ;habilita interrupciones mascarables.
    LDS #$3BFF  ;inicializa el stack
;------------------------------------------------------------------------------
;                       Inicializacion de variables
;------------------------------------------------------------------------------
;Teclado matricial:
    MOVB #$FF,Tecla
    MOVB #$FF,Tecla_IN
    MOVB #$FF,Num_Array
    CLR Cont_Reb
    CLR Cont_TCL
    CLR Patron
    LDAA MAX_TCL
    LDX #NUM_ARRAY-1


BORRAR_ARRAY:
    movb #$FF,A,X
    dbne A,BORRAR_ARRAY

;Displays de 7 segmentos y LEDS:
    clr CONT_7SEG
    clr CONT_TICKS
    clr CONT_DIG
    movb #0,BRILLO
    movb #$02,LEDS
    clr BCD1
    clr BCD2
    movb SEGMENT,DISP3 ;
    movb SEGMENT,DISP4 ;

;Programa:
    CLR VUELTAS
    CLR ValorVueltas
    CLR NumVueltas
    CLR VELOC
    CLR VELPROM
    CLR Banderas
    CLR Cont_Reb
    CLR Cont_TCL
    MOVW #$0000,TICKS_TIME
    MOVW #$0000,TICK_EN
    MOVW #$0000,TICK_DIS

    BSET TIE,$10 ;se habilitan las interrupciones por output compare en canal 4
    BSET TSCR1,$80 ;se habilita el modulo de timer
    LDD TCNT
    ADDD #30
    STD TC4 ;se carga el valor inicial para interrupcion de OC4
    BSET CRGINT,$80 ;se habilitan las interrupciones RTI
    ;se habilitan las interrupciones por ATD0
    MOVB #$C2,ATD0CTL2
    LDAA #160
CONFIG_ATD:
    DBNE A,CONFIG_ATD ;3 ciclos del reloj * 160 * (1/48MHz) = 10 us. Tiempo de inicio del ATD

;Conf LCD
                ldx #iniDsp
                inx
                clrb

INITIALIZE_LCD:
                ldaa b,x                ;cargamos inicio de inicio de display
                jsr Send_Command        ;carga comando
                movb D40us,Cont_Delay   ;delay
                jsr Delay
                incb                    ;aumenta
                cmpb iniDsp             ;vemos si recorrimos todo
                bne INITIALIZE_LCD
                ldaa CLEAR_LCD          ;limpia
                jsr Send_Command
                movb D2ms,Cont_Delay    ;delay
                jsr Delay


;------------------------------------------------------------------------------
MAIN:
FIRST_CONFIG:
                movb #$BB,BIN2           ;apaga pantallas 7seg
                movb #$BB,BIN1
                jsr MODO_CONFIG ;Se mantiene hasta que se ingrese un numero de vueltas
                tst NumVueltas
                beq FIRST_CONFIG
                bra DIP_SWITCH


LLAMAR_LIBRE:
                jsr MODO_LIBRE
                bra DIP_SWITCH


LLAMAR_COMPE:
                jsr MODO_COMP     ;se ejecuta el modo competicion
                bra DIP_SWITCH  ;se vuelve a leer el modo de operacion


LLAMAR_RESUMEN:
                jsr MODO_RESUM       ;se ejecuta el modo resumen
                bra DIP_SWITCH     ;se vuelve a leer el modo de operacion


LLAMAR_CONFIF:
                jsr MODO_CONFIG      ;se ejecuta el modo config
                bra DIP_SWITCH     ;se vuelve a leer el modo de operacion




;------------------------------------------------------------------------------
;       Subrutinas de interrupciones
;------------------------------------------------------------------------------

DIP_SWITCH:
                brclr PTIH,#$C0,LLAMAR_LIBRE
                brset CRGINT,$80,CONTINUE_CHECK
                bset TIE,$10 ;se habilitan las interrupciones por output compare en canal 4
                bset CRGINT,$80 ;se habilitan las interrupciones RTI
CONTINUE_CHECK:
                brset PTIH,#$40,CONF_COMP
                bra LLAMAR_RESUMEN
CONF_COMP:
                brclr PTIH,#$80,LLAMAR_CONFIF
                bra LLAMAR_COMPE
    
;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion PTH CALCULAR: Subrutina utilizada para la lectura de los sensores y subsecuentes
;      calculos de velocidad, velocidad promedio, cantidad de vueltas y ticks necesarios para recorrer 100 m.
;------------------------------------------------------------------------------


CALCULAR:
                brset PIFH,$01,PH0_ISR          ; se revisa cual interrupcion es
                brset PIFH,$08,PH3_ISR
;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion PTH0: Subrutina de atencion a la interrupcion por key wakeup en PH0. Simula el segundo sensor
;      del velodromo y determina si la velocidad medida esta en el rango de velocidades valido, ademas de actualizar la velocidad promedio
;      en el caso de que lo este.
;      INPUTS: Cont_Reb, TICK_MED
;      OUTPUTS: VELOC, VELPROM, VUELTAS, VELOCIDAD_VAL, CANT_VUELTAS_MAXIMA
;      Formula para calcular la velocidad: VELOC = 9064/TICK_MED
;      Formula para calcular/actualizar la velocidad promedio: VELPROM = (VELPROM*(VUELTAS-1) + VELOC)/VUELTAS
;------------------------------------------------------------------------------

PH0_ISR:
                bset PIFH,$01 ;se limpia la bandera
                tst Cont_Reb ;se revisa si se terminaron los rebotes
                bne FIN_PH0

                movb #100,Cont_Reb ;se recarga cont_reb para control de rebotes
                ldx TICK_MED ;se lee la cantidad de ticks medidos

                movw TICK_MED,SAVE_MED

                cpx #129 ;129 ticks para 35 km/h
                bhi VELOCID_INVALID ;si es mayor a esto, la velocidad es menor a 35 km/h
                cpx #48 ;48 ticks para 95 km/h
                blo VELOCID_INVALID ;si es menor a esto, la velocidad es mayor a 95 km/h


                stx TICKS_TIME ;se guarda la cantidad de ticks necesarios para recorrer 55 m
                ldd #4532 ;D = 4532
                idiv ;X = (D/X) = 4532/TICKS = VELOC
                tfr X,D ;D = VELOC
                stab VELOC ;la velocidad siempre es menor a 1 byte, por lo que se guarda en VELOC

                inc VUELTAS
                ldaa VELOC    ; se recarga veloc porque se necesita en a
                ldab VELPROM
                sba

                tfr a,b
                clra
                tfr d,y
                ldab VUELTAS
                tfr d,x
                tfr y,d

                idiv ;queda en x

                tfr x,d
                addd VELPROM
                stab VELPROM

                BRA FIN_PH0

VELOCID_INVALID:
                bset VELOC,$FF ;la velocidad medida es invalida, por lo que se desactiva la bandera de velocidad valida
                bra FIN_PH0

FIN_PH0:
                rti

;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion PTH3: Subrutina de atencion a la interrupcion por key wakeup en PH3. Simula el primer sensor
;      del velodromo y se encarga de borrar TICK_MED, asi como indicar que se debe cargar el Mensaje Cargando en la pantalla LCD
;      INPUTS: Cont_Reb
;      OUTPUTS: TICK_MED, DISPLAY_CALC
;------------------------------------------------------------------------------
PH3_ISR:
                bset PIFH,$08
                tst Cont_Reb
                bne FIN_PH3
                clr SAVE_MED
                BSET Banderas,$40 ;se levanta DISPLAY_CALC
                movb #100,Cont_Reb
                movw #$0000,TICK_MED ;se borra TICK_MED

FIN_PH3:
                rti



;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion TCNT: Subrutina de atencion por timer overflow. Se encarga de incrementar TICK_MED y
;      decrementar TICK_DIS y TICK_EN, esto con el objetivo de calcular la velocidad y marcar los tiempos en los que se deben actualizar
;      las pantallas. Usa prescaler de 8, el tiempo de tick es dado por Ttick = 8*2^(16) / 24MHz, que es aproximadamente 21.8 ms
;      OUTPUTS: TICK_MED, TICK_EN, TICK_DIS, PANT_FLG
;------------------------------------------------------------------------------

TCNT_ISR:
                bset TFLG2,$80 ;activa la interrupcion
                
                ldd TICK_EN ;Se ve si es 0 para cargar en pantalla
                cpd #0
                beq TICKEN_ZERO
                cpd #$FFFF ;Termina
                beq TICKMED_FULL
                subd #1 ;si no se cumple ninguna las dos condiciones revisadas, se decrementa y guarda el valor
                std TICK_EN
                bra TICKMED_FULL

TICKEN_ZERO:
                bset Banderas,$08 ;se levanta PANT_FLG
                ldy TICK_EN
                bset TICK_EN,$FF
                bset 1,Y,$FF
                
TICKMED_FULL:
                ldd TICK_MED ;Si llega a FF se reinicia a 0
                cpd #$FFFF
                beq TICKDIS_REV
                addd #1 ;si no es $FFFF, se incrementa y se guarda el valor
                std TICK_MED

TICKDIS_REV:
                ldd TICK_DIS ;se carga TICK_DIS y se ve si es 0. Si fuera 0, se debe desactivar PANT_FLG
                cpd #0       ; no se puede usar tst, es un word
                beq TICKDIS_ZERO
                cpd #$FFFF ;se ve si es $FFFF, lo cual significa que ya se llego a 0 y no se ha cargado un nuevo valor
                beq FIN_TCNT
                subd #1 ;si no se cumple ninguna las dos condiciones revisadas, se decrementa y guarda el valor
                std TICK_DIS
                bra FIN_TCNT

TICKDIS_ZERO:
                bclr Banderas,$08 ;se desactiva PANT_FLG
                ldy TICK_DIS
                bset TICK_DIS,$FF
                bset 1,Y,$FF

FIN_TCNT:
                rti

;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion ATD0: Subrutina utilizada para la conversion analogica digital del potenciometro
;      de la tarjeta dragon 12, utilizado para controlar el brillo de los leds y las pantallas de 7 segmentos.
;      Se toman 6 mediciones y se calcula el promedio.
;      INPUTS: ADR00H,ADR01H,ADR02H,ADR03H,ADR04H,ADR05H
;      OUTPUTS: BRILLO, DT
;------------------------------------------------------------------------------

ATD_ISR:
                ldd ADR00H   ;Se hace en d para sumar words
                addd ADR01H
                addd ADR02H
                addd ADR03H
                addd ADR04H
                addd ADR05H
                ldx #6
                idiv
                tfr X,D
                stab POT ;Guardar el promedio
                ldaa #20
                mul
                ldx #255
                idiv
                tfr X,D
                stab BRILLO

                RTI

;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion RTI: Esta subrutina descuenta contadores
;     siempre y cuando no sean cero. Los ticks del RTI duran 1.024 ms, por lo
;     que si se cargan variables con X valor se pueden contar aproximadamente
;     X milisegundos. Cont_Reb tiene un valor maximo de 10; se utiliza para
;     suprimir rebotes contando ~10ms. Tambien lleva el tiempo para iniciar ciclos de conversion del ATD
;     INPUTS: Cont_Reb, CONT_200
;     OUTPUTS: Cont_Reb, CONT_200
;------------------------------------------------------------------------------
RTI_ISR:        bset CRGFLG %10000000                 ;borra bandera de interrupcion RTI
                tst Cont_Reb
                beq TIMER                       ;si llegaron los rebotes a 0, se termina la rubrutina
                dec Cont_Reb

                                 ;Solo se decrementa si TIMER CUENTA no es cero

TIMER:          tst CONT_200
                bne NO_RESET
                ;Caso en que es cero y se reinicia
                Movb #200,CONT_200 ;Volvemos al contador con 200
                Movb #$87,ATD0CTL5
                Bra FIN_RTI


NO_RESET:
                dec CONT_200                          ;Decrementamos el contador de rebotes si aun no ha llegado a cero


FIN_RTI:
                Rti

;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion por output compare en el canal 4:
;     Descuenta Cont_Delay, refresca cada 100 ms (5000ticks) los valores de
;     DISP1-DISP4, multiplexa el bus del puerto B para mostrar informacion en
;     los displays de 7 segmentos y los LEDS, y todo con un ciclo de trabajo
;     variable que depende de DT.
;     INPUTS: DT, Cont_Delay, LEDS, CONT_TICKS, DISP1-DISP4
;     OUTPUTS: Cont_Delay, DISP1-DISP4
;------------------------------------------------------------------------------
OC4_ISR:
                ldaa Cont_Delay                 ;Revisamos Cont_Delay para ver si hay que restarle
                cmpa #0
                beq CONTROL_PANTALLA            ; si ya es 0, pasamos a siguiente func
                dec Cont_Delay                  ; decrementa

CONTROL_PANTALLA:
                ldx CONT_7SEG                   ; carga contador de 7seg
                dex                             ; le decrementa
                stx CONT_7SEG
                cpx #0
                bne CONTADOR_DISP               ; Si llega a 0, se le suman 5000 y volvemos a hacer conversiones
                movw #5000,CONT_7SEG
                jsr CONV_BIN_BCD
                jsr BCD_7SEG

CONTADOR_DISP:
                ldaa CONT_TICKS                 ; revisamos contador de ticks
                cmpa #100
                bne MUX
                clra                            ; si llega a 100, se borra
                staa CONT_TICKS
                inc CONT_DIG                    ;cambia de digito para pasar a multiplexar
                ldab CONT_DIG
                cmpb #5                         ; Si llega a 5 se borra
                bne MUX
                clr CONT_DIG

MUX:
                tst CONT_TICKS                  ; si el contador no es 0 pasa a ver el ciclo de trabajo
                bne DT_BRILLO

                movb #$02,PTJ                   ; cuando es 0 ponemos dato en un display
                movb #$FF,PTP

                ldab CONT_DIG                   ; cual display?
                cmpb #0
                beq P4
                cmpb #1
                beq P3
                cmpb #2
                beq P2
                cmpb #3
                beq P1

                movb #$00,PTJ
                movb LEDS,PORTB
                bra DT_BRILLO
P4:
                movb #$F7,PTP                         ;se habilita display 4
                movb DISP4,PORTB
                bra DT_BRILLO
P3:
                movb #$FB,PTP                         ;se habilita display 3
                movb DISP3,PORTB
                bra DT_BRILLO
P2:
                movb #$FD,PTP                         ;se habilita display 2
                movb DISP2,PORTB
                bra DT_BRILLO
P1:
                movb #$FE,PTP                         ;se habilita display 1
                movb DISP1,PORTB

DT_BRILLO:
                LDAA #5                                 ;Correccion de escala
                ldab BRILLO
                mul
                stab DT

                ldaa CONT_TICKS                 ; si el contador llega a ciclo, termina
                cmpa DT
                bne FIN_OC4
                movb #$FF,PTP                         ;deshabilita displays de 7 segmentos
                movb #$02,PTJ                         ;deshabilita LEDS
FIN_OC4:
                inc CONT_TICKS
                bset TFLG1,$10                         ;reinicia la bandera de interrupcion
                ldd TCNT                         ;Carga el valor actual de TCNT
                addd #30                         ;60 por preestaclador 8
                std TC4                         ;actualiza el nuevo valor a alcanzar.
                rti

;------------------------------------------------------------------------------
;   Subrutina Send_Command: se encarga de enviar al LCD el comando que recibe
;     por el acumulador A.
;     INPUTS: Acumulador A, 260us
;------------------------------------------------------------------------------
Send_Command:   psha                    ;se guarda a en pila
                anda #$F0               ;mascara de parte alta
                lsra                    ;deja limpios los dos bits menos significativos
                lsra

                staa PORTK              ;guarda a en portk
                bclr PORTK,$01          ;modif bits menos significativos
                bset PORTK,$02

                movb D260uS,Cont_Delay  ;delay
                jsr Delay

                bclr PORTK,$02
                pula                    ;trae a
                anda #$0F               ;mascara parte baja
                lsla
                lsla

                staa PORTK
                bclr PORTK,$01
                bset PORTK,$02

                movb D260uS,Cont_Delay  ; delay
                jsr Delay

                bclr PORTK,$02
                rts

;------------------------------------------------------------------------------
Send_Data:
                psha ;el dato se recibe en acumulador A y se protege para poder analizar sus nibbles por separado
                anda #$F0 ;Se deja solo el nibble superior del dato
                lsra
                lsra ;se alinea nibble con bus datos en PORTK5-PORTK2.
                staa PORTK ;se carga parte alta del dato en el bus de datos.
                bset PORTK,$03 ;Se habilita el envio de dato y comunicacion con la LCD
                movb D260us,Cont_Delay ;se inicia el retardo de 260us
                jsr Delay
                bclr PORTK,$02 ;Se deshabilita comunicacion con la LCD
                pula ;se recupera el dato original de la pila
                anda #$0F ;Se deja solo el nibble inferior del dato
                lsla
                lsla ;se alinea nibble con bus datos en PORTK5-PORTK2.
                staa PORTK ;se carga parte baja del dato en el bus de datos.
                bset PORTK,$03 ;Se habilita envio de datos y comunicacion con la LCD
                movb D260us,Cont_Delay ;se inicia el retardo de 260us.
                jsr Delay
                bclr PORTK,$02 ;Se deshabilita comunicacion con la LCD
                rts


;------------------------------------------------------------------------------
;   Subrutina Delay: se mantiene en un loop cerrado hasta que Cont_Delay sea 0.
;     Cont_Delay es descontado por OC4 a 50 kHz.
;     INPUTS: Cont_Delay
;------------------------------------------------------------------------------
Delay:
                tst Cont_Delay      ;Espera hasta que OC4 disminuya
                bne Delay
                rts


;------------------------------------------------------------------------------
;   Subrutina BCD_BIN: el arreglo Num_Array corresponde a un numero en BCD donde
;     cada entrada es un digito. Esta subrutina toma este arreglo y calcula en
;     binario el valor numerico del arreglo. El resultado se almacena en ValorVueltas.
;     INPUTS: NUM_ARRAY
;     OUTPUTS:ValorVueltas
;------------------------------------------------------------------------------
BCD_BIN:        ldx #Num_Array
                ldab 1,x           ;revisamos si la unidad es distinta de FF
                cmpb #$FF
                beq UNIDAD         ;si es FF, el valor no es valido
                stab ValorVueltas       ;Si no, lo guarda en ValorVueltas
                bra DECENA         ;lee decenas

UNIDAD:         movb Num_Array,ValorVueltas
                rts

DECENA:
                clra
                ldab Num_Array    ;carga en b
                ldy #10           ;multiplica decenas por 10
                emul

                addb ValorVueltas
                stab ValorVueltas

                rts

;------------------------------------------------------------------------------
;   Subrutina BIN_BCD: esta subrutina realiza la conversion de un numero
;     binario entre 0 y 99 (inclusivos) a su representacion en BCD. El numero
;     a convertir se recibe como parametro por el registro A. El resultado en
;     BCD se devuelve por la variable BCD_L, donde el nibble mas significativo son
;     las decenas y el menos significativo las unidades.
;------------------------------------------------------------------------------
BIN_BCD:

                Ldab #7  ; Contador B=15
                Clr BCD_L

lazo:
                Lsla
                Rol BCD_L  ;Lo mismo para la variable BCD_L y BCD_H
                Psha

                Ldaa BCD_L ;Cargamos en A el BCD_L
                Anda #$0F  ;Tomamos solo en cuenta los 4LSB
                Cmpa #5   ;Comparamos con 5
                Blo men031  ;Si es menor, salte a men031
                Adda #3  ;En caso de mayor, sume 3

men031:
                Staa LOW  ;Guardamos temporalmente el resultado anterior

                Ldaa BCD_L
                Anda #$F0 ;En A tenemos cargado del bit 4 al 7
                Cmpa #$50  ;Comparamos con $50
                Blo men301
                Adda #$30   ;Si es mayor, sume 30

men301:
                Adda LOW   ;Se suman los bits para obtener los 4 LSB de resultado
                Staa BCD_L  ;Se guarda el resultado
                Pula
                Dbne b, lazo

                Lsla
                Rol BCD_L

                Rts

;------------------------------------------------------------------------------
;   Subrutina CONV_BIN_BCD: recibe como parametros de entrada las variables BIN1 y
;     BIN2 y realiza la conversion a BCD de cada una de estas variables. Luego de la conversion, si
;     el numero es menor que 10 significa que el display de 7 segmentos utilizado
;     para las decenas no es necesario que este encendido; en este caso se escribe
;     $B en el nibble mas significativo de BCD1 y BCD2 para indicarlo. Carga $BB
;     en BCD1 o BCD2 dependiendo de si deben estar apagados, mientras que carga
;     $AA si deben tener una raya.
;     INPUTS: BIN1, BIN2
;     OUTPUTS: BCD1, BCD2
;------------------------------------------------------------------------------
CONV_BIN_BCD:
                Ldaa BIN1

                Cmpa #$BB
                Beq BIN1_BB

                Cmpa #$AA
                Beq BIN1_AA

                Bra BIN1_CALC

BIN1_BB:        Movb #$BB,BCD1
                Bra BIN2_CHECK

BIN1_AA:        Movb #$AA,BCD1
                Bra BIN2_CHECK


BIN1_CALC:
                Jsr BIN_BCD ;Pasamos BIN1 a BCD
                Ldaa BCD_L
                Cmpa #10
                Bhs mayor1
                Adda #$B0 ;Si solo tiene un digito, agrega B como "decenas"

mayor1:         Staa BCD1 ;Guardamos el valor en BCD1


BIN2_CHECK:        Ldaa BIN2

                Cmpa #$BB
                Beq BIN2_BB

                Cmpa #$AA
                Beq BIN2_AA

                Bra BIN2_CALC

BIN2_BB:        Movb #$BB,BCD2
                Bra FIN_CONV

BIN2_AA:        Movb #$AA,BCD2
                Bra FIN_CONV


BIN2_CALC:
                Jsr BIN_BCD ;Pasamos BIN1 a BCD
                Ldaa BCD_L
                Cmpa #10
                Bhs mayor2
                Adda #$B0 ;Si solo tiene un digito, agrega B como "decenas"

mayor2:          Staa BCD2 ;Guardamos el valor en BCD1


FIN_CONV:        Rts

;------------------------------------------------------------------------------
; Subrutina TAREA_TECLADO: En esta subrutina se da la lectura del teclado. Aqui
;     se lee el teclado en el puerto A, se suprimen los rebotes, y se maneja la
;     situacion de tecla retenida.
;     INPUTS: Cont_Reb, Tecla, ARRAY_OK, Tecla_IN
;     OUTPUTS: TCL_LISTA
;------------------------------------------------------------------------------
TAREA_TECLADO:
                Ldaa Cont_Reb
                Cmpa #0
                Bne RETORNAR
                Jsr MUX_TECLADO
                Ldaa Tecla
                Cmpa #$FF
                Bne PRESIONADA
                Brclr Banderas,$01,RETORNAR                 ; Si TCL_LISTA es 0, no hay tecla que registrar por lo que se termina la subrutina
                Bclr Banderas,#$03                         ; Caso contrario se registra la tecla. Se ponen en 0 TCL_LISTA y TCL_LEIDA para la siguiente tecla
                Jsr FORMAR_ARRAY
                        Bra RETORNAR

PRESIONADA:
                Brclr Banderas,$02,NotProc
                Ldaa Tecla_IN
                Cmpa Tecla
                Bne Delete
                Bset Banderas,$01                         ; La tecla esta lista para registro
                bra RETORNAR



NotProc:
                Movb Tecla, Tecla_IN
                Bset Banderas, #2
                Movb #10,Cont_Reb
                Bra RETORNAR

DELETE:
                Movb #$FF,Tecla
                Movb #$FF,Tecla_IN
                Bclr Banderas, #3

RETORNAR:
                RTS


;------------------------------------------------------------------------------
MUX_TECLADO:    movb #$EF,Patron                ; Patron inicial
                ldd #$F000                       ; final cuando se desplaza patron

BUSCAR_COLUMNA: movb Patron,PORTA
                brclr PORTA,$08,columna2            ; Verificamos se la tecla est? en la columna2
                brclr PORTA,$04,columna1
                brclr PORTA,$02,columna0
                lsl Patron                          ; Se desplaza el patron para verificar siguiente fila
                addb #3                             ; Se suman 3 para aumentar esa cantidad en el array de posibilidades
                cmpa Patron
                bne BUSCAR_COLUMNA
                movb #$FF,Tecla
TERMINAR:       rts

columna2:       incb                                ; Incrementa en 2 si salta ac?
columna1:       incb                                ; Incrementa en 1 si salta ac?
columna0:       ldx #Teclas
                movb B,X,Tecla                      ; Se mueve la tecla encontrada
                bra TERMINAR

;------------------------------------------------------------------------------
FORMAR_ARRAY:   ldaa Tecla_IN                   ; valor ingresado
                ldab Cont_TCL                   ; cantidad de numeros
                ldx #Num_Array                   ; Posici?n del array

                cmpb MAX_TCL                    ; comparamos si ya est? lleno
                beq ARRAY_LLENO
                cmpb #0                         ; vemos si est? vac?o
                beq PRIMER_VAL
                cmpa #$0B                       ; tecla borrar
                beq BORRAR
                cmpa #$0E                       ; tecla enter
                beq ENTER
                staa b,x                        ; guarda en Num_array + cont_TCL
                inc Cont_TCL
                bra end_formar

ARRAY_LLENO:    cmpa #$0B
                bne ARRAY_LLENO_1
                decb
                movb #$FF,b,x                    ; Para borrar reemplazamos valor actual con ff
                dec Cont_TCL
                bra end_formar

ARRAY_LLENO_1:  cmpa #$0E                         ; es enter?
                bne end_formar
                bset Banderas,$04                ; bandera de array ok
                clr Cont_TCL                     ; vac?a contador tc
                bra end_formar

PRIMER_VAL:     cmpa #$0B
                beq end_formar                         ; terminar

PRIMER_VAL_1:   cmpa #$0E
                beq end_formar
                movb Tecla_IN,b,x
                inc Cont_TCL
                bra end_formar

ENTER:          bset Banderas,#$04                    ; bandera de array_ok
                bclr Cont_TCL,#$FF                    ; pone contador en 0
                bra end_formar


BORRAR:         dec Cont_TCL
                decb
                movb #$FF,b,x


end_formar:     movb #$FF,Tecla_IN
                rts

;------------------------------------------------------------------------------
;   Subrutina MODO_CONFIG: Esta subrutina corresponde a las operaciones necesarias
;     llevar a cabo la configuracion del sistema. Primero pone el valor adecuado
;     de los LEDS para que el usuario pueda saber el modo. Posteriormente, con
;     el uso de TAREA_TECLADO se da la lectura del valor ValorVueltas. Una vez que el
;     usuario presiona ENTER se valida que el valor de ValorVueltas este entre 5 y 25.
;     Si es asi entonces coloca este valor en BIN1 para que pueda ser desplegado
;     en los displays 3 y 4.
;     INPUTS: ValorVueltas
;     OUTPUTS: BIN1, BIN2
;------------------------------------------------------------------------------
MODO_CONFIG:
                BCLR Banderas,$80   ; borra bandera de competencia just in case
    ;si no es ni modo competencia ni modo resumen, se limpian VELOC, VUELTAS, VELPROM y se deshabilitan interrupciones por TCNT y PTH
    		clr VELOC
		bclr TSCR2,$80
    		bclr PIEH,$09
		ldx #MSG_CONFIG1 ;carga el mensaje de configuracion
                ldy #MSG_CONFIG2
                jsr Cargar_LCD
                movb #$02,LEDS ;carga el LED asociado al modo
                movw #$0000,TICK_EN ;borra TICK_EN y TICK_DIS
                movw #$0000,TICK_DIS
                movb #$BB,BIN2
		movb NumVueltas, BIN1                       ;Movemos CatnPQ a bin1
                brset Banderas,$04,DATA_CHECK           ;Revisa bandera arrayok
                jsr TAREA_TECLADO                       ;Si no arrayok, va a teclado
                rts

DATA_CHECK:
                jsr BCD_BIN                              ;pasa de bcd a bin
                ldaa #3                                 ;limites
                cmpa ValorVueltas
                bgt INVALIDO
                ldaa #23
                cmpa ValorVueltas
                bge VALIDO

INVALIDO:                                                ;caso en que el valor no esta en rango
                bclr Banderas,$04
                Clr ValorVueltas
                Clr Num_Array
                rts

VALIDO:
                bclr Banderas, $04                       ;caso en rango, se guarda
                movb ValorVueltas,NumVueltas
                movb ValorVueltas,BIN1
                Clr ValorVueltas
                Clr Num_Array
                rts

;------------------------------------------------------------------------------
;   Subrutina MODO_RESUM: Esta subrutina corresponde a la operacion del modo resumen.
;      Carga el mensaje resume resumen en la pantalla LCD y los valores de VUELTAS y
;      VELPROM en las pantallas de 7 segmentos.
;      INPUTS: VUELTAS, VELPROM
;      OUTPUTS: BIN1, BIN2
;------------------------------------------------------------------------------

MODO_RESUM:
                bclr Banderas,$80    ;se actualiza el modo previo
    		bclr PIEH,$09        ;apaga los sensores
		ldx #MSG_RESUMEN ;carga el mensaje resumen en la pantalla LCD
                ldy #MSG_COMPE2
                jsr Cargar_LCD
                movb #$08,LEDS
                movb VUELTAS,BIN2 ;carga los valores de VUELTAS y VELPROM en 7 segmentos
                movb VELPROM,BIN1
                rts
;------------------------------------------------------------------------------
;   Subrutina PANT_CTRL: Esta subrutina se encarga de manipular las pantallas
;      en el modo competencia. Cuando se detecta el sensor S1 se pone el mensaje
;      calculando. Cuando se activa S2, si la velocidad no esta en el rango valido,
;      pone el mensaje de alerta en la pantalla LCD y pone rayas en la pantalla de 7 segmentos,
;      caso contrario se comporta de acuerdo a las especificaciones definidas en el enunciado.
;      INPUTS: VELOC, VUELTAS, PANT_FLG, CANT_VUELTAS_MAXIMA, VELOCIDAD_VAL, DISPLAY_CALC
;      OUTPUTS: BIN1, BIN2
;------------------------------------------------------------------------------
PANT_CTRL:
                bclr PIEH,$09 ;deshabilita las interrupciones del puerto H

                ldx SAVE_MED ;se lee la cantidad de ticks medidos
                cpx #129 ;259 ticks para 35 km/h
                bhi INVALID_PANT ;si es mayor a esto, la velocidad es menor a 35 km/h
                cpx #48 ;95 ticks para 95 km/h
                blo INVALID_PANT ;si es menor a esto, la velocidad es mayor a 95 km/h
                clr SAVE_MED

		BRSET Banderas,$10,SECOND_CHECK_PANT ;La velocidad es valida, se revisa CALC_TICKS
                BSET Banderas,$10 ;si CALC_TICKS es 0, se realizan los calculos asociados y se pone en 1


		ldy #55 ;Y tiene la cantidad de ticks que pasan en 55 m
		ldd #100
                emul ;D tiene la cantidad de ticks que pasan en 5500 m
                ldx #55
                idiv ;X tiene la cantidad de ticks que pasan en 100 m
                stx TICKS_TIME ;TICKS_TIME es la cantidad de ticks que pasan en 100 m

		tfr x,d
                addd TICKS_TIME
                std TICK_EN ;TICK_EN es el tiempo para recorrer 200 m, ya que la pantalla se halla a 300 m de S2
                addd TICKS_TIME
                std TICK_DIS ;TICK_DIS es el tiempo para recorrer 300 m, pasando la pantalla
                movw #$0000,TICKS_TIME ;TICKS_TIME se limpia
                rts


INVALID_PANT:	ldaa BIN1 ;la velocidad es invalida, se revisa si BIN1 es $AA
                cmpa #$AA
                beq FIRST_CHECK_PANT ;si no es $AA, se debe cargar $AA para poner rayas en la pantalla
                movw #$0000,TICK_EN ;no es $AA, se borra TICK_EN
                movw #69,TICK_DIS ;3 s
                movb #$AA,BIN1 ;se ponen rayas en la pantalla de 7 segmentos
                movb #$AA,BIN2
                bset Banderas %00001000 ;se levanta PANT_FLG
                ldx #MSG_ALERTA1 ;se carga el mensaje de alerta
                ldy #MSG_ALERTA2
                jsr Cargar_LCD
                rts

FIRST_CHECK_PANT:
                brset Banderas,$08,FIN_PANT ;BIN1 es $AA, se revisa PANT_FLG

		ldx #MSG_LIBRE1 ;carga del mensaje inicial
                ldy #MSG_ESPERA
                jsr Cargar_LCD
                movb #$BB,BIN1 ;se apagan las pantallas de 7 segmentos
                movb #$BB,BIN2

                ldab VUELTAS        ; se revisa si se lleg'o al maximo
                cmpb NumVueltas
                beq CLEAR_PANT_VARS
                bset PIEH,$09       ; deshabilida interrupciones por puertos ph3 y ph0

CLEAR_PANT_VARS:
                BCLR Banderas,$10 ;se limpian las variables de la pantalla
                CLR VELOC
                RTS

SECOND_CHECK_PANT:
                BRSET Banderas,$08,CHECK_BIN1_BB_COMP ;CALC_TICKS era 1, se hacen las revisiones de $BB segun PANT_FLG
                LDAA BIN1 ;PANT_FLG es 0
                CMPA #$BB
                BNE FIRST_CHECK_PANT ;si A es $BB, se retorna
                RTS


CHECK_BIN1_BB_COMP:
                LDAA BIN1
                CMPA #$BB
                BEQ BIN1_BB_COMP_MSG
                RTS

BIN1_BB_COMP_MSG:
                LDX #MSG_COMPE1
                LDY #MSG_COMPE2
                JSR Cargar_LCD
                MOVB VUELTAS,BIN2
                MOVB VELOC,BIN1
FIN_PANT:
	       RTS

;------------------------------------------------------------------------------
;   Subrutina MODO_COMP: Esta subrutina corresponde a la operacion del modo competencia.
;      Pasa revisando el valor de VELOC para acceder a PANT_CTRL
;------------------------------------------------------------------------------
MODO_COMP:
                brset Banderas,$80,ESTADO_COMPE
                bset Banderas,$80 ;se actualiza el modo previo
		ldx #MSG_LIBRE1   ;se carga el mensaje inicial
		ldy #MSG_ESPERA
		jsr Cargar_LCD
		movb #$BB,BIN1
		movb #$BB,BIN2
		bset TSCR2,$80        ;se habilita TCNT
		bset PIEH,$09   ;se habilita keywakeup en PH0 y PH3.
		bclr Banderas,$04 ;en este caso solo es necesario borrar ARRAY_OK

		clr ValorVueltas
		clr VUELTAS
		clr VELPROM
		clr VELOC
		clr Num_Array

ESTADO_COMPE:
		movb #$04,LEDS
                brclr Banderas,$40,VEL_ZERO ;se revisa si se debe imprimir el mensaje calculando
                bclr Banderas,$40
                ldx #MSG_LIBRE1 ;se imprime el mensaje calculando
                ldy #MSG_CALCULANDO
                jsr Cargar_LCD

VEL_ZERO:
                tst VELOC         ;Entra a control de pantalla hasta que la velocidad no es 0
                beq FIN_COMP
                jsr PANT_CTRL

FIN_COMP:
                RTS



;------------------------------------------------------------------------------
;   Subrutina MODO_LIBRE: Esta subrutina corresponde a la operacion del modo libre.
;      Se imprime en pantalla el mensaje del modo libre, se apaga la pantalla de 7
;      segmentos y se deshabilitan las interrupciones OC4 y RTI. Al deshabilitar la RTI
;      no se realizan nuevos ciclos de conversion del ATD por lo que virtualmente esta
;      deshabilitado.
;------------------------------------------------------------------------------
MODO_LIBRE:
                brclr CRGINT,$80,FIN_LIBRE ;Revisa si ya estuvo en libre
		clr VELOC
    		clr VUELTAS
    		bclr TSCR2,$80
    		bclr PIEH %00001001
    		bclr Banderas,$80

                ldx #MSG_LIBRE1
                ldy #MSG_LIBRE2
                jsr Cargar_LCD
                bclr CRGINT,$80
                bclr TIE,$10
                movb #$FF,PTP
                movb #$00,PTJ ;se habilitan los LEDS
                movb #$01,PORTB ;se coloca en puerto B el estado de los LEDS.
FIN_LIBRE:
                rts



;------------------------------------------------------------------------------
;   Subrutina Cargar_LCD: esta subrutina se encarga de enviar a la pantalla LCD
;     cada caracter, uno por uno, de ambas lineas del LCD. Recibe los parametros
;     en los registros indice X y Y, que contienen las direcciones de inicio a
;     los mensajes de las lineas 1 y 2 respectivamente.
;------------------------------------------------------------------------------
Cargar_LCD:     ldaa ADD_L1                           ;inicio de linea
                jsr Send_Command                ;env?a comando
                movb D40uS,Cont_Delay           ;delay
                jsr Delay

LINEA1:         ldaa 1,x+                         ;Se va cargando mensaje
                cmpa #$00
                beq CARGAR_LINEA2               ;Se pasa a cargar linea 2

                jsr Send_Data

                movb D40uS,Cont_Delay
                jsr Delay
                bra LINEA1


CARGAR_LINEA2:  ldaa ADD_L2                     ;inicio linea 2
                jsr Send_Command
                movb D40uS,Cont_Delay
                jsr Delay


LINEA2:         ldaa 1,y+                       ; Se va cargando mensaje
                cmpa #$00
                beq TERMINA_LCD
                jsr Send_Data
                movb D40uS,Cont_Delay           ; delay
                jsr Delay
                bra LINEA2

TERMINA_LCD:    rts

;------------------------------------------------------------------------------
;   Subrutina BCD_7SEG: esta subrutina se encarga de tomar los valores en BCD1
;     y BCD2 y determinar el valor de DISP1, DISP2, DISP3, DISP4. Estas ultimas
;     cuatro variables son las que indican cuales segmentos de los displays se
;     deben encender para que se muestre el numero deseado. Sencillamente se
;     se analiza cada nibble de BCD1 y BCD2, y se toman decisiones a partir de
;     sus valores.


;------------------------------------------------------------------------------
BCD_7SEG:
                Ldx #SEGMENT
                Ldy #DISP4 ;Recorremos displays de derecha a izquierda
                Ldaa #0
                ;Carga de BCD1
                Ldaa BCD1
                Ldab BCD1
                Anda #$0F
                Movb A,X,1,Y- ;se guarda en display4
                Lsrb
                Lsrb
                Lsrb
                Lsrb
                Movb B,X,1,Y-  ;se guarda en display3


                Ldx #SEGMENT
                Ldy #DISP2 ;Recorremos displays de derecha a izquierda
                Ldaa #0
                ;Carga de BCD1
                Ldaa BCD2
                Ldab BCD2
                Anda #$0F
                Movb A,X,1,Y- ;se guarda en display2
                Lsrb
                Lsrb
                Lsrb
                Lsrb
                Movb B,X,1,Y-  ;se guarda en display1

returnBCD_7SEG: Rts