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
                ORG $1000
Banderas:       DS 1  ;Tiene el formato: MOD_PREV_H:MOD_PREV_L:CALC_TICKS:LIBRE_PREVIO:PANT_FLG:ARRAY_OK:TCL_LEIDA:TCL_LISTA.
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
BanderasEx:     DS 1    ;Banderas de uso adicionales. Tiene el formato: X:X:X:X:X:DISPLAY_CALC:CANT_VUELTAS_MAXIMA:VELOCIDAD_VAL
                        ;VELOCIDAD_VAL indica si la velocidad medida en PH0 esta en el rango valido
                        ;CANT_VUELTAS_MAXIMA indica cuando Vueltas se vuelve igual a NumVueltas
                        ;DISPLAY_CALC es utilizado para mostrar el Mensaje Calculando en el momento apropiado
TICKS_TIME:     DS 2  ;Variable WORD utilizada para medir la cantidad de ticks que deben pasar para recorrer 100 m

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
    JSR MODO_CONFIG ;Mientras NumVueltas no sea distinto de 0, se seguira leyendo por un valor valido
    TST NumVueltas
    BEQ FIRST_CONFIG
LECTURA_MODO:
    LDAA PTIH ;Se leen PH7:PH6 por polling, para determinar el modo de funcionamiento
    ANDA #$C0 ;Se dejan solamente los 2 bits mas significativos, correspondientes a PH7 y PH6
    CMPA #$00
    BEQ GO_LIBRE
    BRCLR Banderas $10 CONTINUE_CHECK
    BCLR Banderas,$10
    BSET TIE,$10 ;se habilitan las interrupciones por output compare en canal 4
    BSET CRGINT,$80 ;se habilitan las interrupciones RTI
CONTINUE_CHECK:
    CMPA #$C0
    BEQ GO_COMP
    CMPA #$80
    BEQ GO_RESUM
    CMPA #$40
    BEQ GO_CONF
    
GO_LIBRE:
    ;si no es ni modo competencia ni modo resumen, se limpian VELOC, VUELTAS, VELPROM y se deshabilitan interrupciones por TCNT y PTH
    CLR VELOC
    CLR VUELTAS
    CLR VELPROM
    BCLR TSCR2,$80
    BCLR PIEH %00001001
    BCLR Banderas,$C0
    JSR MODO_LIBRE
    BRA LECTURA_MODO
    
GO_COMP:
    LDAA Banderas ;se verifica el modo previo
    ANDA #$C0
    CMPA #$C0
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
NOT_FIRST_COMP:
    BSET Banderas,$C0 ;se actualiza el modo previo
    JSR MODO_COMP     ;se ejecuta el modo competicion
    JMP LECTURA_MODO  ;se vuelve a leer el modo de operacion
    
GO_RESUM:
    BSET Banderas,$80    ;se actualiza el modo previo
    BCLR Banderas,$40
    BCLR PIEH %00001001  ;se deshabilita keywakeup en PH0 y PH3
    JSR MODO_RESUM       ;se ejecuta el modo resumen
    JMP LECTURA_MODO     ;se vuelve a leer el modo de operacion
    
GO_CONF:
    BSET Banderas,$40    ;se actualiza el modo previo
    BCLR Banderas,$80
    ;si no es ni modo competencia ni modo resumen, se limpian VELOC, VUELTAS, VELPROM y se deshabilitan interrupciones por TCNT y PTH
    CLR VELOC
    CLR VUELTAS
    CLR VELPROM
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
                BSET PIFH,$01 ;se limpia la bandera
                TST Cont_Reb ;se revisa si se terminaron los rebotes
                BNE FIN_PH0
                MOVB #100,Cont_Reb ;se recarga cont_reb para control de rebotes
                LDX TICK_MED ;se lee la cantidad de ticks medidos
                CPX #259 ;259 ticks para 35 km/h
                BHI VEL_INVAL ;si es mayor a esto, la velocidad es menor a 35 km/h
                CPX #95 ;95 ticks para 95 km/h
                BLO VEL_INVAL ;si es menor a esto, la velocidad es mayor a 95 km/h
                BSET BanderasEx,$01 ;se levanta la bandera de velocidad valida
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
                MOVB #$FF,VELOC ;la velocidad medida es invalida, por lo que se desactiva la bandera de velocidad valida
                BCLR BanderasEx,$01
                BRA FIN_PH0
                
MAX_VUELTAS_ALCANZADAS:
                BCLR BanderasEx,$02

FIN_PH0:
                RTI
                
;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion PTH3: Subrutina de atencion a la interrupcion por key wakeup en PH3. Simula el primer sensor
;      del velodromo y se encarga de borrar TICK_MED, asi como indicar que se debe cargar el Mensaje Cargando en la pantalla LCD
;      INPUTS: Cont_Reb
;      OUTPUTS: TICK_MED, DISPLAY_CALC
;------------------------------------------------------------------------------
PH3_ISR:
                BSET PIFH,$08
                TST Cont_Reb
                BNE FIN_PH3
                BSET BanderasEx,$04 ;se levanta DISPLAY_CALC
                MOVB #100,Cont_Reb
                MOVW #$0000,TICK_MED ;se borra TICK_MED

FIN_PH3:
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
RTI_ISR:
                BSET CRGFLG %10000000 ;se limpia la bandera de interrupcion RTI
                TST Cont_Reb ;se ve si el contador de rebotes llego a 0
                BEQ CHECK_TIMER ;si no, se pasa a revisar el timer
                DEC Cont_Reb
CHECK_TIMER:
                TST CONT_200 ;se revisa si el timer ha llegado a 0
                BEQ RESET_CONT_200
                DEC CONT_200
                BRA FIN_RTI
RESET_CONT_200:
                MOVB #200,CONT_200
                MOVB #$87,ATD0CTL5
FIN_RTI:
                RTI
;ok                
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
                BSET TFLG1 %00010000 ;se reinicia la bandera de interrupcion
                TST Cont_Delay
                BEQ REFRESH ;se descuenta el contador solo si no es cero
                DEC Cont_Delay
REFRESH:
                LDD CONT_7SEG ;por tratarse de un WORD se debe traer al registro D para restarle 1
                SUBD #1
                STD CONT_7SEG ;se guarda el nuevo valor, y esto a la vez afecta la bandera Z
                BNE SELECT_DISP ;cuando CONT_7SEG=0 se refrescan los valores de los displays
                MOVW #5000,CONT_7SEG ;se reinicia el contador de refrescamiento de la informacion
                JSR CONV_BIN_BCD
                JSR BCD_7SEG ;se refresca la informacion
SELECT_DISP:
                LDAA #100
                CMPA CONT_TICKS ;cuando CONT_TICKS=N se debe cambiar de digito
                BNE MULTIPLEX ;si no es igual entonces no hay que cambiar de digito y se puede continuar
                CLR CONT_TICKS ;se reinicia el contador de ticks
                INC CONT_DIG ;se pasa al siguiente digito
                LDAA #5
                CMPA CONT_DIG ;cuando CONT_DIG alcance 5 se debe volver a colocar en 0 para que sea circular
                BNE MULTIPLEX ;si no es 5 no hay que corregir nada y se puede continuar
                CLR CONT_DIG
MULTIPLEX:
                TST CONT_TICKS
                BNE DUTY_CYCLE ;cuando CONT_TICKS=0 se debe habiliar algun Display. Si no, se puede pasar a comprobar el ciclo de trabajo
                MOVB #$02,PTJ ;se deshabilitan los LEDS
                MOVB #$FF,PTP ;se deshabilitan displays de 7 segmentos
                LDAA CONT_DIG ;se comparan todos los posibles valores para determinar cual display encender
                CMPA #0
                BEQ DIG0
                CMPA #1
                BEQ DIG1
                CMPA #2
                BEQ DIG2
                CMPA #3
                BEQ DIG3
                ;Ningun Display se debe habilitar, entonces son los LEDS
                MOVB #$00,PTJ ;se habilitan los LEDS
                MOVB LEDS,PORTB ;se coloca en puerto B el estado de los LEDS.
                BRA DUTY_CYCLE ;se pasa a comprobar el ciclo de trabajo
DIG0:
                MOVB #$F7,PTP ;se habilita unicamente el display 4
                MOVB DISP4,PORTB ;se coloca en el puerto B el valor del display 4
                BRA DUTY_CYCLE
DIG1:
                MOVB #$FB,PTP ;se habilita unicamente el display 3
                MOVB DISP3,PORTB ;se coloca en el puerto B el valor del display 3
                BRA DUTY_CYCLE
DIG2:
                MOVB #$FD,PTP ;se habilita unicamente el display 2
                MOVB DISP2,PORTB ;se coloca en el puerto B el valor del display 2
                BRA DUTY_CYCLE
DIG3:
                MOVB #$FE,PTP ;se habilita unicamente el display 1
                MOVB DISP1,PORTB ;se coloca en el puerto B el valor del display 1
DUTY_CYCLE:
                LDAA CONT_TICKS
                CMPA DT
                BNE FIN_OC4
                MOVB #$FF,PTP ;se deshabilitan displays de 7 segmentos
                MOVB #$02,PTJ ;se deshabilitan los LEDS
FIN_OC4:
                INC CONT_TICKS
                LDD TCNT ;se carga el valor actual de TCNT para reajustar el output compare
                ADDD #60 ;60 cuentas equivalen 50kHz con prescalador=8
                STD TC4 ;se actualiza el nuevo valor a alcanzar.
                RTI


;------------------------------------------------------------------------------
;   Subrutina Send_Command: se encarga de enviar al LCD el comando que recibe
;     por el acumulador A.
;     INPUTS: Acumulador A, 260us
;------------------------------------------------------------------------------
Send_Command:
                PSHA ;el comando se recibe en acumulador A y se protege para poder analizar sus nibbles por separado
                ANDA #$F0 ;Se deja solo el nibble superior del comando a ejecutar
                LSRA
                LSRA ;se alinea nibble con bus datos en PORTK5-PORTK2.
                STAA PORTK ;se carga parte alta del comando en el bus de datos.
                BCLR PORTK,$01 ;Se habilita el envio de comandos.
                BSET PORTK,$02 ;Se habilita comunicacion con la LCD.
                MOVB D260us,Cont_Delay ;se inicia el retardo de 260us.
                JSR Delay
                BCLR PORTK,$02 ;Se deshabilita comunicacion con la LCD
                PULA ;se recupera el comando original de la pila
                ANDA #$0F ;Se deja solo el nibble inferior del comando a ejecutar
                LSLA
                LSLA ;se alinea nibble con bus datos en PORTK5-PORTK2.
                STAA PORTK ;se carga parte baja del comando en el bus de datos.
                BCLR PORTK,$01 ;Se habilita el envio de comandos.
                BSET PORTK,$02 ;Se habilita comunicacion con la LCD.
                MOVB D260us,Cont_Delay ;se inicia el retardo de 260us.
                JSR Delay
                BCLR PORTK,$02 ;Se deshabilita comunicacion con la LCD
                RTS
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;   Subrutina Send_Data: se encarga de enviar al LCD el dato que recibe
;     por el acumulador A.
;     INPUTS: Acumulador A, 260us
;------------------------------------------------------------------------------
Send_Data:
                PSHA ;el dato se recibe en acumulador A y se protege para poder analizar sus nibbles por separado
                ANDA #$F0 ;Se deja solo el nibble superior del dato
                LSRA
                LSRA ;se alinea nibble con bus datos en PORTK5-PORTK2.
                STAA PORTK ;se carga parte alta del dato en el bus de datos.
                BSET PORTK,$03 ;Se habilita el envio de dato y comunicacion con la LCD
                MOVB D260us,Cont_Delay ;se inicia el retardo de 260us
                JSR Delay
                BCLR PORTK,$02 ;Se deshabilita comunicacion con la LCD
                PULA ;se recupera el dato original de la pila
                ANDA #$0F ;Se deja solo el nibble inferior del dato
                LSLA
                LSLA ;se alinea nibble con bus datos en PORTK5-PORTK2.
                STAA PORTK ;se carga parte baja del dato en el bus de datos.
                BSET PORTK,$03 ;Se habilita envio de datos y comunicacion con la LCD
                MOVB D260us,Cont_Delay ;se inicia el retardo de 260us.
                JSR Delay
                BCLR PORTK,$02 ;Se deshabilita comunicacion con la LCD
                RTS


;------------------------------------------------------------------------------
;   Subrutina Delay: se mantiene en un loop cerrado hasta que Cont_Delay sea 0.
;     Cont_Delay es descontado por OC4 a 50 kHz.
;     INPUTS: Cont_Delay
;------------------------------------------------------------------------------
Delay:
                TST Cont_Delay
                BNE Delay
                RTS


;------------------------------------------------------------------------------
;   Subrutina BCD_BIN: el arreglo Num_Array corresponde a un numero en BCD donde
;     cada entrada es un digito. Esta subrutina toma este arreglo y calcula en
;     binario el valor numerico del arreglo. El resultado se almacena en ValorVueltas.
;     INPUTS: NUM_ARRAY
;     OUTPUTS:ValorVueltas
;------------------------------------------------------------------------------
BCD_BIN:
                LDX #Num_Array ;direccion base del numero en BCD a convertir
                CLRA ;iterador
                CLRB ;acumulador para el resultado
CIFRA:
                ADDB A,X ;se suma el digito al acumulado
                INCA ;se incrementa el indice
                PSHB ;se protege el acumulado del resultado
                LDAB #$FF ;valor que marca el final del arreglo Num_Array
                CMPB A,X  ;se compara el valor final con el proximo valor de Num_Array.
                BEQ FIN_BCD_BIN ;si son iguales es porque no hay mas numeros en el arreglo y se puede terminar la conversion
                CMPA MAX_TCL ;se compara la cantidad de valores procesados con la canitdad maxima
                BEQ FIN_BCD_BIN ;si son iguales es porque no hay mas numeros en el arreglo y se puede terminar la conversion
                PULB ;se recupera el acumulado del resultado
                PSHA ;se protege el indice
                LDAA #10 ;se prepara el multiplicador.
                MUL ;se multiplica por 10 el acumulador. En BCD cada digito esta en base 10.
                PULA ;se recupera el indice
                BRA CIFRA ;se repite lo anterior
FIN_BCD_BIN:
                PULB ;se recupera el acumulado del resultado
                STAB ValorVueltas ;se guarda el resultado en la variable designada
                RTS

;------------------------------------------------------------------------------
;   Subrutina BIN_BCD: esta subrutina realiza la conversion de un numero
;     binario entre 0 y 99 (inclusivos) a su representacion en BCD. El numero
;     a convertir se recibe como parametro por el registro A. El resultado en
;     BCD se devuelve por la variable BCD_L, donde el nibble mas significativo son
;     las decenas y el menos significativo las unidades.
;------------------------------------------------------------------------------
BIN_BCD:        CLRB ;acumulador para el resultado.
                LDX #7 ;contador de desplazamiento.
NEXT_BIT:
                LSLA ;se extrae un bit del numero binario y queda en C
                ROLB ;se inserta el bit en el acumulador de resultado
                PSHA ;se protege el numero en conversion
                TBA ;A ahora tiene el contenido del acumulador de resultado
                ANDA #$0F ;en el nibble menos significativo de A se tienen los 4 bits correspondientes a las unidades del resultado.
                CMPA #5
                BLO DECENAS ;si el campo de las unidades es menor que 5 se puede continuar a analizar las decenas
                ADDA #$03 ;cuando las unidades son 5 o mas se deben sumar 3 unidades.
DECENAS:
                STAA LOW ;se conservan temporalmente las unidades.
                TBA ;se vuelve a cargar en A el contenido del acumulador resultado
                ANDA #$F0 ;en el nibble mas significativo de A se tienen los 4 bits correspondientes a las decenas del resultado.
                CMPA #$50
                BLO CONFECCIONAR ;si el campo de las unidades es menor que 5 se puede continuar a analizar las decenas
                ADDA #$30 ;cuando las decenas son 5 o mas se deben sumar 3 decenas.
CONFECCIONAR:
                ADDA LOW ;se suman las unidades
                TAB ;se traslada a B el nuevo resultado parcial
                PULA ;se recupera el numero binario que se esta convirtiendo
                DEX ;se decrementa el contador de desplazamientos
                BNE NEXT_BIT ;cuando el contador no es cero significa que quedan bits por analizar.
                LSLA ;se extrae el ultimo bit
                ROLB ;se inserta el ultimo bit en el resultado final.
                STAB BCD_L
                RTS

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
                LDAA BIN1 ;se carga parametro de entrada a BIN_BCD_BASE
                CMPA #$BB ;se ve si BIN1 es $BB, por si se debe borrar su pantalla de 7 segmentos correspondiente
                BNE BIN1_NOTBB
                MOVB #$BB,BCD1 ;si es $BB, se carga $BB en BCD1 y se pasa a revisar BIN2
                BRA CHECK_BIN2
BIN1_NOTBB:
                CMPA #$AA ;si BIN1 no es $BB, se revisa si es $AA
                BNE BIN1_NOTAA
                MOVB #$AA,BCD1 ;si es $AA, se carga $AA en BCD1 y se revisa BIN2
                BRA CHECK_BIN2
BIN1_NOTAA:
                JSR BIN_BCD ;si BIN1 no es ni $BB ni $AA, se convierte su valor y se guarda en BCD1
                MOVB BCD_L,BCD1
CHECK_BIN2:
                LDAA BIN2 ;se carga parametro de entrada a BIN_BCD
                CMPA #$BB ;se ve si BIN2 es $BB, por si se debe borrar su pantalla de 7 segmentos correspondiente
                BNE BIN2_NOTBB
                MOVB #$BB,BCD2 ;si es $BB, se carga $BB en BCD2 y se terminan las conversiones
                BRA FIN_BIN_BCD
BIN2_NOTBB:
                CMPA #$AA ;si BIN2 no es $BB, se revisa si es $AA
                BNE BIN2_NOTAA
                MOVB #$AA,BCD2 ;si es $AA, se carga $AA en BCD2 y se terminan las conversiones
                BRA FIN_BIN_BCD
BIN2_NOTAA:
                JSR BIN_BCD ;si no es ni $BB ni $AA, se convierte su valor y se guarda en BCD2
                MOVB BCD_L,BCD2
FIN_BIN_BCD:
                RTS

;------------------------------------------------------------------------------
; Subrutina TAREA_TECLADO: En esta subrutina se da la lectura del teclado. Aqui
;     se lee el teclado en el puerto A, se suprimen los rebotes, y se maneja la
;     situacion de tecla retenida.
;     INPUTS: Cont_Reb, Tecla, ARRAY_OK, Tecla_IN
;     OUTPUTS: TCL_LISTA
;------------------------------------------------------------------------------
TAREA_TECLADO:  TST Cont_Reb
                BNE fin_TAREA ; Se ve si el contador ha llegado a 0, para asegurar la supresion de rebotes
                JSR MUX_TECLADO ; Si el contador llega a 0, se corrobora la presion de una tecla y se salta a MUX_TECLADO
                LDAA Tecla ; Luego de MUX_TECLADO ya la tecla leida esta en la variable Tecla
                CMPA #$FF ; Se ve si se presiono una tecla valida
                BEQ checkTCL_LISTA ; Puede haber una tecla lista para guardar en el arreglo. Esto es para evitar el caso de tecla retenida
                BRCLR Banderas,$02,setReb ; Si ARRAY_OK es 0, hay que preparar el contador para rebotes
                CMPA Tecla_IN ; Si no, ya esta listo y se verifica que la tecla recibida (despues de la supresion de rebotes) es valida
                BEQ setTCL_LISTA ; Si Tecla_IN y Tecla son iguales, se registra la tecla presionada
                MOVB #$FF,Tecla ; Caso contrario se borra la tecla y TCL_LISTA y TCL_LEIDA se ponen en 0
                MOVB #$FF,Tecla_IN
                BCLR Banderas,$03
fin_TAREA:      RTS

checkTCL_LISTA: BRCLR Banderas,$01,fin_TAREA ; Si TCL_LISTA es 0, no hay tecla que registrar por lo que se termina la subrutina
                BCLR Banderas,$03 ; Caso contrario se registra la tecla. Se ponen en 0 TCL_LISTA y TCL_LEIDA para la siguiente tecla
                JSR FORMAR_ARRAY
                BRA fin_TAREA

setReb:         MOVB Tecla,Tecla_IN ; Almacenamiento temporal de la tecla detectada en Tecla_IN para revision de rebotes
                BSET Banderas,$02 ; TCL_LEIDA se levanta setea
                MOVB #10,Cont_Reb ; Se inicia el contador para rebotes
                BRA fin_TAREA
setTCL_LISTA:   BSET Banderas,$01 ; La tecla esta lista para registro
                BRA fin_TAREA

;------------------------------------------------------------------------------
; Subrutina MUX_TECLADO: esta subrutina es la que se encarga de leer el teclado
;     en el puerto A. Como se habilitaron las resistencias de pull up, la parte
;     baja del puerto A (la entrada) siempre tendra valor %1111. Al colocar un
;     unico cero en alguno de los bits de la parte alta, si se presiona un boton
;     entonces se tendra un cero tambien en algun bit de la parte baja. Ambos
;     ceros en parte alta y baja definen la fila y columna de la tecla presionada.
;     Con esta informacion se calcula la posicion en el arreglo de teclas en el
;     que se encuentra el valor de la tecla presionada. El valor de la tecla se
;     devuelve en la variable Tecla. Si no se presiona nada entonces Tecla = $FF.
;     INPUTS: Teclas
;     OUTPUTS: Tecla
;------------------------------------------------------------------------------
MUX_TECLADO:    MOVB #$EF,Patron ; Se inicializa el patron de busqueda
                LDAA #$F0 ; Para comparacion para final de iteraciones
                CLRB ; Para acceso indexado por acumulador por
loop_patron:    MOVB Patron,PORTA ; Se carga el patron en el puerto A
                BRCLR PORTA,$08,col2 ; Se busca el patron en la ultima columna  verifica la ultima columna
                BRCLR PORTA,$04,col1 ; Se busca el patron en la columna media
                BRCLR PORTA,$02,col0 ; Se busca el patron en la primera columna
                LSL Patron ; Si no hubo coincidencias con el patron, se desplaza a la izquierda y se suman 3 como offset de busqueda de la columna (para buscar en TeclasTECLAS) el  no se
                ADDB #3
                CMPA Patron ; Se ve si ya se buscaron todos los posibles valores
                BNE loop_patron ; Si no se han recorrido todas las filas, se itera sobre la siguientehecho la busqueda en todo Teclas, se itera otra vez
                MOVB #$FF,Tecla ; Caso contrario la tecla registrada no era valida y se borra
fin_MUX:        RTS

col2:           INCB ; Incremento del indice de busqueda segun la columna y registro de la tecla hallada en Teclas a Tecla
col1:           INCB
col0:           LDX #Teclas
                MOVB B,X,Tecla
                BRA fin_MUX

;------------------------------------------------------------------------------
; Subrutina FORMAR_ARRAY: recibe en la variable Tecla_IN el valor de la tecla
;     presionada que se quiere almacenar en array. Este arreglo representa un
;     numero en formato BCD. Se cuida que el arreglo no supere el tamano maximo.
;     INPUTS: MAX_TCL, Tecla_IN, CONT_TCL
;     OUTPUTS: Num_Array
;------------------------------------------------------------------------------
FORMAR_ARRAY:   LDX #Num_Array ; Se guarda la posicion de Num_Array para empezar a guardar datos
                LDAA Tecla_IN ; Se guarda la tecla registrada
                LDAB Cont_TCL ; Se inicia el iterador para iterar sobre Num_Array
                CMPB MAX_TCL ; Se ve si Num_array esta lleno
                BEQ max_teclas ; Si esta lleno, se cubren los casos de Num_Array lleno
                CMPB #$00 ; Se ve si Num_Array esta vacio
                BEQ cero_teclas ; Si esta vacio, se ven los casos para la primera tecla
                CMPA #$0B ; Si no esta ni vacio ni lleno, se procede con la operacion normal. Si la tecla es $0B, se borra un elemento
                BEQ borrar_tecla
                CMPA #$0E ; Si la tecla es $0E, se registran los datos en Num_Array, se reinicia el contador y se levanta la bandera ARRAY_OK
                BEQ enter_tecla
                MOVB Tecla_IN,B,X ; Si era una tecla numerica, se ingresa
                INC Cont_TCL ; El iterador para a la siguiente posicion de Num_Array
fin_FORMAR:     MOVB #$FF,Tecla_IN ; Si no era ninguno de los casos contemplados, la tecla es invalida y se borra Tecla_IN. No se modifica Num_Array
                RTS

max_teclas:     CMPA #$0B ; Caso de encontrar un boton B cuando el arreglo esta lleno. Si no es B, se pasa al siguiente caso, mientras que si lo es se elimina el elemento
                BNE enterMax
                DECB ; Se debe decrementar porque el puntero estaba listo para escribir
                MOVB #$FF,B,X ; Para borrar la tecla, se carga un $FF en la posicion correspondiente de Num_Array
                DEC Cont_TCL ; Se decrementa la posicion del iterador sobre Num_ARRAY
                BRA fin_FORMAR
enterMax:       CMPA #$0E ; Caso de encontrar un boton E cuando el arreglo esta lleno. Si no es un E, no se hace nada
                BNE fin_FORMAR
                BSET Banderas,$04 ; Si era E, se setea la bandera ARRAY_OK y se libera Cont_TCL para recibir otro arreglo. El inicio de Num_Array queda en $1007
                CLR Cont_TCL
                BRA fin_FORMAR
cero_teclas:    CMPA #$0B ; Si el arreglo esta vacio y se lee un B o un E, no se hace nada
                BEQ fin_FORMAR
                CMPA #$0E
                BEQ fin_FORMAR
                MOVB Tecla_IN,B,X ; Si es una tecla numerica, se registra en la primera posicion de Num_Array
                INC Cont_TCL
                BRA fin_FORMAR
borrar_tecla:   DECB ; Se debe decrementar 1 porque el puntero estaba listo para escribir
                MOVB #$FF,B,X ; Se borra una tecla si se puede y se recibio un B
                DEC Cont_TCL
                BRA fin_FORMAR
enter_tecla:    BSET Banderas,$04 ; Se ingresa el arreglo aunque no este lleno
                CLR Cont_TCL
                BRA fin_FORMAR

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
                LDX #CONFIG_MSG1 ;carga el mensaje de configuracion
                LDY #CONFIG_MSG2
                JSR Cargar_LCD
                MOVB #$02,LEDS ;carga el LED asociado al modo
                MOVW #$0000,TICK_EN ;borra TICK_EN y TICK_DIS
                MOVW #$0000,TICK_DIS
                MOVB NumVueltas,BIN1 ;pone NumVueltas en BIN1 para que se presente en la pantalla de 7 segmentos
                MOVB #$BB,BIN2 ;no se pone el valor de BIN2 en la pantalla de 7 segmentos
                JSR TAREA_TECLADO
                BRCLR Banderas $04 FIN_CONF ;se verifica con ARRAY_OK si la secuencia de teclas esta lista
                JSR BCD_BIN ;si si esta lista, entonces se carga el valor
                LDAA ValorVueltas ;se verifica que ValorVueltas se encuentre en el rango de valores valido
                CMPA #5
                BLO ERASE_TEC_STRUCS
                CMPA #25
                BHI ERASE_TEC_STRUCS
                MOVB ValorVueltas,NumVueltas ;si el valor es valido, se cargan NumVueltas y BIN1
                MOVB NumVueltas,BIN1

ERASE_TEC_STRUCS:
                BCLR Banderas,$04
                CLR Num_Array
                CLR ValorVueltas
                
FIN_CONF:
                RTS

;------------------------------------------------------------------------------
;   Subrutina MODO_RESUM: Esta subrutina corresponde a la operacion del modo resumen.
;      Carga el mensaje resume resumen en la pantalla LCD y los valores de VUELTAS y
;      VELPROM en las pantallas de 7 segmentos.
;      INPUTS: VUELTAS, VELPROM
;      OUTPUTS: BIN1, BIN2
;------------------------------------------------------------------------------

MODO_RESUM:
                LDX #RESUM_MSG1 ;carga el mensaje resumen en la pantalla LCD
                LDY #RESUM_MSG2
                JSR Cargar_LCD
                MOVB #$08,LEDS
                MOVB VUELTAS,BIN2 ;carga los valores de VUELTAS y VELPROM en 7 segmentos
                MOVB VELPROM,BIN1
                RTS
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
                BCLR PIEH,$09 ;deshabilita las interrupciones del puerto H
                BRSET BanderasEx $01 VEL_VAL_PANT ;Se revisa si la velocidad es valida
                LDAA BIN1 ;la velocidad es invalida, se revisa si BIN1 es $AA
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

VEL_VAL_PANT:
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
                BRSET Banderas $10 NOT_FIRST_LIBRE
                BSET Banderas,$10
                LDX #RunMeter
                LDY #LIBRE_MSG
                JSR Cargar_LCD
                BCLR CRGINT,$80
                BCLR TIE,$10
                MOVB #$FF,PTP
                MOVB #$00,PTJ ;se habilitan los LEDS
                MOVB #$01,PORTB ;se coloca en puerto B el estado de los LEDS.
NOT_FIRST_LIBRE:
                RTS



;------------------------------------------------------------------------------
;   Subrutina Cargar_LCD: esta subrutina se encarga de enviar a la pantalla LCD
;     cada caracter, uno por uno, de ambas lineas del LCD. Recibe los parametros
;     en los registros indice X y Y, que contienen las direcciones de inicio a
;     los mensajes de las lineas 1 y 2 respectivamente.
;------------------------------------------------------------------------------
Cargar_LCD:
                LDAA ADD_L1 ;Se carga la direccion de la primera posici�n de la primera fila de la LCD
                JSR Send_Command ;Se ejecuta el comando
                MOVB D40uS,Cont_Delay
                JSR Delay
LINE1:
                LDAA 1,X+ ;Se carga cada caracter en A
                BEQ CARG_2 ;Si se encuentra un caracter de EOM ($00) se termino de imprimir la primera fila
                JSR Send_Data ;Se imprime cada caracter
                MOVB D40us,Cont_Delay
                JSR Delay
                BRA LINE1
CARG_2:
                LDAA ADD_L2 ;Se carga la direccion de la primera posicion de la segunda fila de la LCD
                JSR Send_Command
                MOVB D40us,Cont_Delay
                JSR Delay
LINE2:
                LDAA 1,Y+ ;Se carga cada caracter en A
                BEQ FIN_Cargar_LCD ;Si se encuentra un caracter de EOM ($00) se termin� de imprimir la primera fila
        	JSR Send_Data ;Se imprime cada caracter
		MOVB D40us,Cont_Delay
		JSR Delay
		BRA LINE2
FIN_Cargar_LCD:
		RTS

;------------------------------------------------------------------------------
;   Subrutina BCD_7SEG: esta subrutina se encarga de tomar los valores en BCD1
;     y BCD2 y determinar el valor de DISP1, DISP2, DISP3, DISP4. Estas ultimas
;     cuatro variables son las que indican cuales segmentos de los displays se
;     deben encender para que se muestre el numero deseado. Sencillamente se
;     se analiza cada nibble de BCD1 y BCD2, y se toman decisiones a partir de
;     sus valores.
;------------------------------------------------------------------------------
BCD_7SEG:
                LDX #SEGMENT
                LDY #DISP4 ;se llenan los displays de derecha a izquierda
                LDAA #0
                LDAB BCD1
                BRA subrutinabcd
loadBCD2:       LDAB BCD2
subrutinabcd:   PSHB
                ANDB #$0F
                MOVB B,X,1,Y- ;se recorre cada 'caracter' BCD
                PULB
                LSRB
                LSRB
                LSRB
                LSRB
                MOVB B,X,1,Y-
                CPY #DISP2
                BEQ loadBCD2
returnBCD_7SEG: RTS