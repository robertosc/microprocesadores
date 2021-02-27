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
;   despliegue de datos en la pantalla LCD y la pantalla de 7 segmentos, as� como el teclado matricial en el caso del modo config.
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
                ORG $1000\
                        ;COMPE:X:X:
Banderas:       DS 1  ;Tiene el formato: COMPE:X:CALC_TICKS:X:PANT_FLG:ARRAY_OK:TCL_LEIDA:TCL_LISTA.
                      ;MOD_PREV_H y MOD_PREV_L indican el modo de funcionamiento previo al que se utiliza, usado para funcionamiento de los modos competencia y libre
                      ;PANT_FLG indica el estado de las pantallas a utilizar por PANT_CTRL
                      ;ARRAY_OK indica que se presiono la tecla Enter y que en el arreglo ya se tienen todos los valores leidos.
                      ;TCL_LEIDA indica que ya se habia tenido una lectura del teclado y que se estaba esperando a que se diera la supresion de rebotes.
                      ;TCL_LISTA indica que luego de la supresion de rebotes se confirmo que si se presiono una tecla.

NumVueltas:     DS 1  ;cantidad maxima de vueltas a leer
ValorVueltas:   DS 1  ;variable de transicion para la lectura de NumVueltas con rebote de botones
MAX_TCL:        DB 2  ;cantidad maximas de teclas que se leen
Tecla:          DS 1  ;en esta variable se almacena el valor leido del teclado en la subrutina MUX_TECLADO.
Tecla_IN:       DS 1  ;en esta variable se almacena temporalmente el valor de Tecla antes de la supresion de rebotes. Si despues de la supresion se da que Tecla y Tecla_IN son iguales es porque efectivamente se presiono una tecla que debe ser guardada.
Cont_Reb:       DS 1  ;es gel contador de ticks del RTI, usado para suprimir rebotes.
Cont_TCL:       DS 1  ;es el indice utilizado para escribir en el arreglo que guarda las teclas presionadas.
Patron:         DS 1  ;es el indice que lleva las iteraciones en subrutina MUX_TECLADO.
Num_Array:      DS 2  ;en este arreglo se almacenan todas las teclas presionadas por el usuario.
BRILLO:         DS 1  ;Variable controlada por PTH3/PTH2 para incrementar/decrementar el brillo de la pantalla LCD.
POT:            DS 1  ;Variable que lee el potenciometro
TICK_EN:        DS 2  ;define el tiempo que estara encendida la pantalla
TICK_DIS:       DS 2  ;define el tiempo que estara apagada la pantalla
VELOC:          DS 1  ;la velocidad medida
VUELTAS:        DS 1  ;cantidad de vueltas realizadas
VELPROM:        DS 1  ;velocidad promedio del ciclista
TICK_MED:       DS 2  ;ticks utilizados para sensar la velocidad del ciclista
BIN1:           DS 1  ;variable de entrada a subrutina CONV_BIN_BCD. Utilizada para ValorVueltas
BIN2:           DS 1  ;variable de entrada a subrutina CONV_BIN_BCD.
BCD1:           DS 1  ;variable de salida de subrutina BIN_BCD. Tambien es entrada para BCD_7SEG. Utilizada para ValorVueltas.
BCD2:           DS 1  ;variable de salida de subrutina BIN_BCD. Tambien es entrada para BCD_7SEG. 
BCD_L:          DS 1  ;variable donde se guarda la salida de BIN_BCD. Utilizada para CONV_BIN_BCD.
BCD_H:          DS 1  ;variable de conversion
TEMP:           DS 1  ;variable temporal para conversiones
LOW:            DS 1  ;variable requerida para el algoritmo de la subrutina BIN_BCD.
DISP1:          DS 1  ;corresponde al valor que se escribe en el display de 7 segmentos.
DISP2:          DS 1  ;BCD2 utiliza DISP1 y DISP2 para desplegarse
DISP3:          DS 1  ;corresponde al valor que se escribe en el display de 7 segmentos.
DISP4:          DS 1  ;BCD1 utiliza DISP3 y DISP4 para desplegarse

LEDS            DS 1  ;guarda el estado de los LEDS
CONT_DIG:       DS 1  ;cuenta cual digito de 7 segmentos se debe habilitar. Cambia cada vez que CONT_TICKS alcanza 100.
CONT_TICKS:     DS 1  ;contador de ticks de Output Compare para multiplexar.
DT:             DS 1  ;ciclo de trabajo. DT = N-K.
CONT_7SEG:      DS 2  ;contador de ticks de OC4 para lograr refrescamiento de LEDS y Displays a 10Hz.
CONT_200:       DS 1  ;para conteo de 200 ms en RTI para el ATD
Cont_Delay:     DS 1  ;esta variable se carga con alguna de las siguientes tres constantes para generar retrasos temporales.
D2ms:           DB 100  ;100 ticks a 50kHz son 2 milisegundos
D260us:         DB 13  ;13 ticks a 50kHz son 260 microsegundos
D40us:          DB 3  ;2 ticks a 50kHz son 40 microsegundos
CLEAR_LCD:      DB $01  ;comando para limpiar el LCD
ADD_L1:         DB $80  ;direccion inicio de linea 1
ADD_L2:         DB $C0  ;direccion inicio de linea 2
BanderasEx:     DS 1    ;Banderas de uso adicionales. Tiene el formato: X:X:X:X:X:DISPLAY_CALC:CANT_VUELTAS_MAXIMA:X
                        ;VELOCIDAD_VAL indica si la velocidad medida en PH0 esta en el rango valido
                        ;CANT_VUELTAS_MAXIMA indica cuando Vueltas se vuelve igual a NumVueltas
                        ;DISPLAY_CALC es utilizado para mostrar el Mensaje Calculando en el momento apropiado
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
RunMeter:       FCC "  RunMeter 623  "
                DB EOM
Esperando:      FCC "  ESPERANDO...  "
                DB EOM
Calculando:     FCC "CALCULANDO...   "
                DB EOM
LIBRE_MSG:      FCC "   MODO LIBRE   "
                DB EOM
CONFIG_MSG1:    FCC "  MODO CONFIG   "
                DB EOM
CONFIG_MSG2:    FCC "  NUM VUELTAS   "
                DB EOM
COMP_MSG1:      FCC " M. COMPETENCIA "
                DB EOM
COMP_MSG2:      FCC "VUELTA     VELOC"
                DB EOM
RESUM_MSG1:     FCC "  MODO RESUMEN  "
                DB EOM
RESUM_MSG2:     FCC "VUELTAS    VELOC"
                DB EOM
ALERT_MSG1:     FCC "** VELOCIDAD ** "
                DB EOM
ALERT_MSG2:     FCC "*FUERA DE RANGO*"
                DB EOM
;------------------------------------------------------------------------------



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
    BCLR PIEH,%00001001   ;se deshabilita keywakeup en PH0 y PH3.
    MOVB #$00,PPSH ;las interrupciones deben ocurrir en el flanco decreciente.

;Configuracion PH7-PH6 como entrada de proposito general por polling: (Dipswitch)
    BCLR DDRH %11000000

;Configuracion del teclado en puerto A:
    MOVB #$F0,DDRA        ;parte alta de A como salida y parte baja como entrada
    BSET PUCR %00000001   ;resistencias de pull-up en puerto A. Son necesarias para que haya un 1 en el PAD cuando no se presiona ningun boton del teclado.

;Configuracion del modulo de Timer como Output Compare en el Canal 4:
    BSET TSCR1 %10000000 ;se habilita modulo de timer.
    BSET TSCR2 %00000011 ;prescaler es 2^3 = 8
    BSET TIOS %00010000 ;se configura el canal 4 como Output Compare.
    BSET TIE %00010000 ;se habilita interrupcion del canal 4.
    BCLR TCTL1 3 ;no es necesario que haya una salida en puerto T. Solo se requiere la interrupcion.

;Configuracion de los displays de 7 segmentos y los LEDS.
    MOVB #$FF,DDRB ;puerto core B se configura como salida de proposito general. (LEDS y SEGMENTOS)
    MOVB #$0F,DDRP ;parte baja de puerto P se configura como salida de proposito general. (~Habilitador Segmentos)
    BSET DDRJ %00000010 ;se configura bit 1 del puerto J como salida de proposito general . (~Habilitador LEDS)

;Configuracion de pantalla LCD
    MOVB #$FF,DDRK ;todos los pines del puerto K se configura como salida para controlar la LCD.

;Configuracion del ATD
    MOVB #$30,ATD0CTL3
    MOVB #$B9,ATD0CTL4
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
    
    
LoopCLR:
    MOVB #$FF,A,X
    DBNE A,LoopCLR

;Displays de 7 segmentos y LEDS:
    CLR CONT_7SEG
    CLR CONT_TICKS
    CLR CONT_DIG
    MOVB #50,BRILLO
    MOVB #$02,LEDS
    CLR BCD1
    CLR BCD2
    MOVB SEGMENT,DISP3 ;para tener DISP3 produciendo un 0
    MOVB SEGMENT,DISP4 ;para tener DISP4 produciendo un 0. Importa mas que nada si se desea que en DISP3 y DISP4 presenten el ultimo valor valido introducido de ValorVueltas, con OC4

;Programa:
    CLR VUELTAS
    CLR ValorVueltas
    CLR NumVueltas
    CLR VELOC
    CLR VELPROM
    CLR Banderas
    CLR BanderasEx
    CLR Cont_Reb
    CLR Cont_TCL
    MOVW #$0000,TICKS_TIME
    MOVW #$0000,TICK_EN
    MOVW #$0000,TICK_DIS

;Inicializacion del LCD
    LDX #iniDsp+1 ;Se carga en X la tabla que contiene los comandos de inicializacion. Posicion 0 tiene el tamano de la tabla.
    CLRB
COMMANDS:
    LDAA B,X ;Se recorren los comandos con direccionamiento indexado por acumulador B
    JSR Send_Command ;Se ejecuta cada comando
    MOVB D40us,Cont_Delay ;40us son necesarios luego de enviar cualquiera de los comando de inicializacion
    JSR Delay
    INCB ;siguiente comando
    CMPB iniDsp
    BNE COMMANDS ;Si ya se ejecutaron todos los comandos de la tabla, terminar comandos de inicializacion
    LDAA CLEAR_LCD ;Cargar comando de limpiar pantalla
    JSR Send_Command ;enviar comando de limpiar pantalla
    MOVB D2ms,Cont_Delay ;luego de enviar comando limpiar pantalla se debe esperar 2ms
    JSR Delay
    
;------------------------------------------------------------------------------
MAIN:
    BSET TIE,$10 ;se habilitan las interrupciones por output compare en canal 4
    BSET TSCR1,$80 ;se habilita el modulo de timer
    LDD TCNT
    ADDD #60
    STD TC4 ;se carga el valor inicial para interrupcion de OC4
    BSET CRGINT,$80 ;se habilitan las interrupciones RTI
    ;se habilitan las interrupciones por ATD0
    MOVB #$C2,ATD0CTL2
    LDAA #160
CONFIG_ATD:
    DBNE A,CONFIG_ATD ;3 ciclos del reloj * 160 * (1/48MHz) = 10 us. Tiempo de inicio del ATD

;Entra a la primera configuracion
FIRST_CONFIG:
    MOVB #$BB,BIN2
    movb #$BB,BIN1
    JSR MODO_CONFIG ;Mientras NumVueltas no sea distinto de 0, se seguira leyendo por un valor valido
    TST NumVueltas
    BEQ FIRST_CONFIG
    
LECTURA_MODO:
    brclr PTIH,#$C0,GO_LIBRE
    BRSET CRGINT,$80,CONTINUE_CHECK
    ;BCLR Banderas,$10   ;BORRARRRRR
    BSET TIE,$10 ;se habilitan las interrupciones por output compare en canal 4
    BSET CRGINT,$80 ;se habilitan las interrupciones RTI
CONTINUE_CHECK:
    brset PTIH,#$40,CONF_COMP
    bra GO_RESUM
CONF_COMP:
    brclr PTIH,#$80,GO_CONF
    bra GO_COMP

GO_LIBRE:
    ;si no es ni modo competencia ni modo resumen, se limpian VELOC, VUELTAS, VELPROM y se deshabilitan interrupciones por TCNT y PTH
    JSR MODO_LIBRE
    BRA LECTURA_MODO
    
GO_COMP:
    LDAA Banderas ;se verifica el modo previo
    ANDA #$80
    CMPA #$80
    BEQ NOT_FIRST_COMP
    LDX #RunMeter   ;se carga el mensaje inicial
    LDY #Esperando
    JSR Cargar_LCD
    MOVB #$BB,BIN1
    MOVB #$BB,BIN2
    BSET TSCR2,$80        ;se habilita TCNT
    BSET PIEH,%00001001   ;se habilita keywakeup en PH0 y PH3.
    BCLR Banderas,$04 ;en este caso solo es necesario borrar ARRAY_OK
    CLR Num_Array
    CLR ValorVueltas
    CLR VELOC
    CLR VUELTAS
    CLR VELPROM
NOT_FIRST_COMP:
    BSET Banderas,$80 ;se actualiza el modo previo
    JSR MODO_COMP     ;se ejecuta el modo competicion
    JMP LECTURA_MODO  ;se vuelve a leer el modo de operacion
    
GO_RESUM:
    Bclr Banderas,$80    ;se actualiza el modo previo
    ;BCLR Banderas,$40
    BCLR PIEH %00001001  ;se deshabilita keywakeup en PH0 y PH3
    JSR MODO_RESUM       ;se ejecuta el modo resumen
    JMP LECTURA_MODO     ;se vuelve a leer el modo de operacion
    
GO_CONF:
        ;se actualiza el modo previo
    BCLR Banderas,$80
    ;si no es ni modo competencia ni modo resumen, se limpian VELOC, VUELTAS, VELPROM y se deshabilitan interrupciones por TCNT y PTH
    CLR VELOC
    ;CLR VUELTAS
    ;CLR VELPROM
    BCLR TSCR2,$80
    BCLR PIEH %00001001
    JSR MODO_CONFIG      ;se ejecuta el modo config
    JMP LECTURA_MODO     ;se vuelve a leer el modo de operacion
    
    
    
    
;------------------------------------------------------------------------------
;       Subrutinas de interrupciones
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion PTH CALCULAR: Subrutina utilizada para la lectura de los sensores y subsecuentes
;      calculos de velocidad, velocidad promedio, cantidad de vueltas y ticks necesarios para recorrer 100 m.
;------------------------------------------------------------------------------


CALCULAR:
                BRSET PIFH,$01,PH0_ISR          ; se revisa cual interrupcion es
                BRSET PIFH,$08,PH3_ISR
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

		cpx #259 ;259 ticks para 35 km/h
                bhi VEL_INVAL ;si es mayor a esto, la velocidad es menor a 35 km/h
                cpx #95 ;95 ticks para 95 km/h
		blo VEL_INVAL ;si es menor a esto, la velocidad es mayor a 95 km/h



		;BSET BanderasEx,$01 ;se levanta la bandera de velocidad valida
                STX TICKS_TIME ;se guarda la cantidad de ticks necesarios para recorrer 55 m
                LDD #9064 ;D = 9064
                IDIV ;X = (D/X) = 9064/TICKS = VELOC
                TFR X,D ;D = VELOC
                STAB VELOC ;la velocidad siempre es menor a 1 byte, por lo que se guarda en VELOC
                LDAA VELPROM ;se carga la velocidad promedio en A
                LDAB VUELTAS ;se carga la cantidad de vueltas-1 en B
                MUL ;D = VELPROM*(VUELTAS-1)
                TFR D,X ;X = VELPROM*(VUELTAS-1)
                LDAB VELOC ;B = VELOC
                ABX ;X = VELPROM*(VUELTAS-1) + VELOC
                INC VUELTAS ;se actualiza el valor de VUELTAS
                LDAB VUELTAS
                CLRA ;D = VUELTAS
                XGDX ;D = VELPROM*(VUELTAS-1) + VELOC, X = VUELTAS
                IDIV ;X = (D/X) = VELPROM
                TFR X,D ;D = VELPROM
                STAB VELPROM ;se actualiza la velocidad promedio
                LDAB VUELTAS
                CMPB NumVueltas ;se ve si se llego a NumVueltas
                BLO MAX_VUELTAS_ALCANZADAS ;si no ha llegado a NumVueltas, retorna
                BSET BanderasEx,$02 ;si se llego a NumVueltas se levanta CANT_VUELTAS_MAXIMA
                BRA FIN_PH0

VEL_INVAL:
                movb #$FF,VELOC ;la velocidad medida es invalida, por lo que se desactiva la bandera de velocidad valida
                bclr BanderasEx,$01
                bra FIN_PH0

MAX_VUELTAS_ALCANZADAS:
                bclr BanderasEx,$02

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
                bne END_PH3
                clr SAVE_MED
                BSET BanderasEx,$04 ;se levanta DISPLAY_CALC
                MOVB #100,Cont_Reb
                MOVW #$0000,TICK_MED ;se borra TICK_MED

END_PH3:
                RTI



;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion TCNT: Subrutina de atencion por timer overflow. Se encarga de incrementar TICK_MED y
;      decrementar TICK_DIS y TICK_EN, esto con el objetivo de calcular la velocidad y marcar los tiempos en los que se deben actualizar
;      las pantallas. Usa prescaler de 8, el tiempo de tick es dado por Ttick = 8*2^(16) / 24MHz, que es aproximadamente 21.8 ms
;      OUTPUTS: TICK_MED, TICK_EN, TICK_DIS, PANT_FLG
;------------------------------------------------------------------------------

TCNT_ISR:
                BSET TFLG2,$80 ;se limpia la bandera de interrupcion
                LDX TICK_MED ;se carga TICK_MED y se compara si llego a $FFFF, para evitar overflow
                CPX #$FFFF
                BEQ CHECK_EN_TCNT
                INX ;si no es $FFFF, se incrementa y se guarda el valor
                STX TICK_MED

CHECK_EN_TCNT:
                LDX TICK_EN ;se carga TICK_EN y se ve si es 0. Si fuera 0, se debe levantar PANT_FLG
                CPX #0
                BEQ EN_IS_0_TCNT
                CPX #$FFFF ;se ve si es $FFFF, lo cual significa que ya se llego a 0 y no se ha cargado un nuevo valor
                BEQ CHECK_DIS_TCNT
                DEX ;si no se cumple ninguna las dos condiciones revisadas, se decrementa y guarda el valor
                STX TICK_EN
                BRA CHECK_DIS_TCNT
                
EN_IS_0_TCNT:
                MOVW #$FFFF,TICK_EN
                BSET Banderas,$08 ;se levanta PANT_FLG

CHECK_DIS_TCNT:
                LDX TICK_DIS ;se carga TICK_DIS y se ve si es 0. Si fuera 0, se debe desactivar PANT_FLG
                CPX #0
                BEQ DIS_IS_0_TCNT
                CPX #$FFFF ;se ve si es $FFFF, lo cual significa que ya se llego a 0 y no se ha cargado un nuevo valor
                BEQ FIN_TCNT
                DEX ;si no se cumple ninguna las dos condiciones revisadas, se decrementa y guarda el valor
                STX TICK_DIS
                BRA FIN_TCNT

DIS_IS_0_TCNT:
                MOVW #$FFFF,TICK_DIS
                BCLR Banderas,$08 ;se desactiva PANT_FLG
                
FIN_TCNT:
                RTI
    
;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion ATD0: Subrutina utilizada para la conversion analogica digital del potenciometro
;      de la tarjeta dragon 12, utilizado para controlar el brillo de los leds y las pantallas de 7 segmentos.
;      Se toman 6 mediciones y se calcula el promedio.
;      INPUTS: ADR00H,ADR01H,ADR02H,ADR03H,ADR04H,ADR05H
;      OUTPUTS: BRILLO, DT
;------------------------------------------------------------------------------
    
ATD_ISR:
                LDX #6
                LDD ADR00H   ;Se calcula el promedio de las 6 medidas del potenciometro
                ADDD ADR01H
                ADDD ADR02H
                ADDD ADR03H
                ADDD ADR04H
                ADDD ADR05H
                IDIV
                TFR X,D
                STAB POT ;Guardar el promedio
                LDAA #20
                MUL
                LDX #255
                IDIV
                TFR X,D
                STAB BRILLO
                LDAA #5 ;Se multiplica por 5 para volverlo en escala a 100
                MUL
                STAB DT
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


NO_RESET:           dec CONT_200                          ;Decrementamos el contador de rebotes si aun no ha llegado a cero


FIN_RTI:        Rti
                
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
                ldaa #100                       ;Modifica el ciclo de trabajo para aumentar o disminuir el brillo
                suba BRILLO
                staa DT

                ldaa CONT_TICKS                 ; si el contador llega a ciclo, termina
                cmpa DT
                bne FIN_OC4
                movb #$FF,PTP                         ;deshabilita displays de 7 segmentos
                movb #$02,PTJ                         ;deshabilita LEDS
FIN_OC4:
                inc CONT_TICKS
                bset TFLG1,$10                         ;reinicia la bandera de interrupcion
                ldd TCNT                         ;Carga el valor actual de TCNT
                addd #60                         ;60 por preestaclador 8
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

men031
                Staa LOW  ;Guardamos temporalmente el resultado anterior

                Ldaa BCD_L
                Anda #$F0 ;En A tenemos cargado del bit 4 al 7
                Cmpa #$50  ;Comparamos con $50
                Blo men301
                Adda #$30   ;Si es mayor, sume 30

men301
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

BIN1_BB:	Movb #$BB,BCD1
		Bra BIN2_CHECK

BIN1_AA:	Movb #$AA,BCD1
		Bra BIN2_CHECK


BIN1_CALC:
                Jsr BIN_BCD ;Pasamos BIN1 a BCD
                Ldaa BCD_L
                Cmpa #10
                Bhs mayor1
                Adda #$B0 ;Si solo tiene un digito, agrega B como "decenas"

mayor1          Staa BCD1 ;Guardamos el valor en BCD1


BIN2_CHECK:	Ldaa BIN2

		Cmpa #$BB
		Beq BIN2_BB

		Cmpa #$AA
		Beq BIN2_AA

		Bra BIN2_CALC

BIN2_BB:	Movb #$BB,BCD2
		Bra FIN_CONV

BIN2_AA:	Movb #$AA,BCD2
		Bra FIN_CONV


BIN2_CALC:
                Jsr BIN_BCD ;Pasamos BIN1 a BCD
                Ldaa BCD_L
                Cmpa #10
                Bhs mayor2
                Adda #$B0 ;Si solo tiene un digito, agrega B como "decenas"

mayor2          Staa BCD2 ;Guardamos el valor en BCD1


FIN_CONV:	Rts

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
                ldx #CONFIG_MSG1 ;carga el mensaje de configuracion
                ldy #CONFIG_MSG2
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
                ldx #RESUM_MSG1 ;carga el mensaje resumen en la pantalla LCD
                ldy #RESUM_MSG2
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
                cpx #259 ;259 ticks para 35 km/h
                bhi INVALID_PANT ;si es mayor a esto, la velocidad es menor a 35 km/h
                cpx #95 ;95 ticks para 95 km/h
                blo INVALID_PANT ;si es menor a esto, la velocidad es mayor a 95 km/h
                clr SAVE_MED

		BRSET Banderas %00100000 SECOND_CHECK_PANT ;La velocidad es valida, se revisa CALC_TICKS
                BSET Banderas %00100000 ;si CALC_TICKS es 0, se realizan los calculos asociados y se pone en 1
                LDY TICKS_TIME ;Y tiene la cantidad de ticks que pasan en 55 m
                LDD #100
                EMUL ;D tiene la cantidad de ticks que pasan en 5500 m
                LDX #55
                IDIV ;X tiene la cantidad de ticks que pasan en 100 m
                STX TICKS_TIME ;TICKS_TIME es la cantidad de ticks que pasan en 100 m
                TFR X,D
                ADDD TICKS_TIME
                STD TICK_EN ;TICK_EN es el tiempo para recorrer 200 m, ya que la pantalla se halla a 300 m de S2
                ADDD TICKS_TIME
                STD TICK_DIS ;TICK_DIS es el tiempo para recorrer 300 m, pasando la pantalla
                MOVW #$0000,TICKS_TIME ;TICKS_TIME se limpia
                RTS
                
                
INVALID_PANT:	LDAA BIN1 ;la velocidad es invalida, se revisa si BIN1 es $AA
                CMPA #$AA
                BEQ FIRST_CHECK_PANT ;si no es $AA, se debe cargar $AA para poner rayas en la pantalla
                MOVW #$0000,TICK_EN ;no es $AA, se borra TICK_EN
                MOVW #137,TICK_DIS ;137*21.8ms = 3 s
                MOVB #$AA,BIN1 ;se ponen rayas en la pantalla de 7 segmentos
                MOVB #$AA,BIN2
                BSET Banderas %00001000 ;se levanta PANT_FLG
                LDX #ALERT_MSG1 ;se carga el mensaje de alerta
                LDY #ALERT_MSG2
                JSR Cargar_LCD
                RTS

FIRST_CHECK_PANT:
                BRCLR Banderas %00001000 BIN1_BB_INITIAL_MSG ;BIN1 es $AA, se revisa PANT_FLG
                RTS




SECOND_CHECK_PANT:
                BRSET Banderas %00001000 CHECK_BIN1_BB_COMP ;CALC_TICKS era 1, se hacen las revisiones de $BB segun PANT_FLG
                LDAA BIN1 ;PANT_FLG es 0
                CMPA #$BB
                BNE BIN1_BB_INITIAL_MSG ;si A es $BB, se retorna
                RTS

BIN1_BB_INITIAL_MSG:
                LDX #RunMeter ;carga del mensaje inicial
                LDY #Esperando
                JSR Cargar_LCD
                MOVB #$BB,BIN1 ;se apagan las pantallas de 7 segmentos
                MOVB #$BB,BIN2
                BRSET BanderasEx $02 CLEAR_PANT_VARS ;se verifica si se llego a la ultima vuelta
                BSET PIEH,$09

CLEAR_PANT_VARS:
                BCLR Banderas %00100000 ;se limpian las variables de la pantalla
                CLR VELOC
                RTS

CHECK_BIN1_BB_COMP:
                LDAA BIN1
                CMPA #$BB
                BEQ BIN1_BB_COMP_MSG
                RTS

BIN1_BB_COMP_MSG:
                LDX #COMP_MSG1
                LDY #COMP_MSG2
                JSR Cargar_LCD
                MOVB VUELTAS,BIN2
                MOVB VELOC,BIN1
                RTS
                
;------------------------------------------------------------------------------
;   Subrutina MODO_COMP: Esta subrutina corresponde a la operacion del modo competencia.
;      Pasa revisando el valor de VELOC para acceder a PANT_CTRL
;------------------------------------------------------------------------------
MODO_COMP:
                MOVB #$04,LEDS
                BRCLR BanderasEx $04 CHECK_VEL_COMP ;se revisa si se debe imprimir el mensaje calculando
                BCLR BanderasEx,$04
                LDX #RunMeter ;se imprime el mensaje calculando
                LDY #Calculando
                JSR Cargar_LCD
CHECK_VEL_COMP:
                TST VELOC
                BEQ FIN_COMP
                JSR PANT_CTRL

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
                brclr CRGINT,$80,NOT_FIRST_LIBRE
		clr VELOC
    		clr VUELTAS
      		;clr VELPROM
    		bclr TSCR2,$80
    		bclr PIEH %00001001
    		bclr Banderas,$80
                ;BSET Banderas,$10 ;borraaar
                ldx #RunMeter
                ldy #LIBRE_MSG
                jsr Cargar_LCD
                bclr CRGINT,$80
                bclr TIE,$10
                movb #$FF,PTP
                movb #$00,PTJ ;se habilitan los LEDS
                movb #$01,PORTB ;se coloca en puerto B el estado de los LEDS.
NOT_FIRST_LIBRE:
                rts



;------------------------------------------------------------------------------
;   Subrutina Cargar_LCD: esta subrutina se encarga de enviar a la pantalla LCD
;     cada caracter, uno por uno, de ambas lineas del LCD. Recibe los parametros
;     en los registros indice X y Y, que contienen las direcciones de inicio a
;     los mensajes de las lineas 1 y 2 respectivamente.
;------------------------------------------------------------------------------
Cargar_LCD:     ldaa ADD_L1                           ;inicio de linea
                jsr Send_Command                ;env�a comando
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