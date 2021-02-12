;##############################################################################
;                                 Tarea #5
;   Fecha: 3 de noviembre del 2020.
;   Autor: Victor Yeom Song
;
;   Descripcion: este programa simula el conteo y empaquetado de tornillos
;     en una linea de produccion. En el modo CONFIG se pueden definir
;     cuantos tornillos incluye cada paquete (entre 12 y 96). En el modo RUN se
;     simula el conteo de tornillos a una tasa de 4 tornillos por segundo y cuando
;     la cuenta alcanza el valor programado se aumenta un acumulador de paquetes
;     y se activa un relay dando asi indicacion de que el paquete esta completo.
;##############################################################################
#include registers.inc

;------------------------------------------------------------------------------
;     Declaracion de las estructuras de datos y vectores de interrupcion
;------------------------------------------------------------------------------
;Vectores de interrupcion:
               ORG $3E70   ;direccion del vector de interrupcion RTI.
               DW RTI_ISR  ;direccion de la subrutina de servicio a interrupcion RTI.
               ORG $3E4C   ;direccion del vector de interrupcion por key wakeup del puerto H.
               DW PTH_ISR  ;direccion de la subrutina de servicio a interrupcion del puerto H.
               ORG $3E66   ;direccion del vector de interrupcion OC4.
               DW OC4_ISR  ;direccion de la subrutina de servicio a interrupcion OC4.


;Estructuras de datos:
                ORG $1000
Banderas:       DS 1  ;Tiene el formato: X:X:CAMBIO_MODO:MODSEL:X:ARRAY_OK:TCL_LEIDA:TCL_LISTA.
                      ;ARRAY_OK indica que se presiono la tecla Enter y que en el arreglo ya se tienen todos los valores leidos.
                      ;TCL_LEIDA indica que ya se habia tenido una lectura del teclado y que se estaba esperando a que se diera la supresion de rebotes.
                      ;TCL_LISTA indica que luego de la supresion de rebotes se confirmo que si se presiono una tecla.
                      ;MODSEL es el selector del modo, asociado al dipswitch PH7. Modo CONFIG en ON (1), modo RUN en OFF (0).
                      ;CAMBIO_MODO cuando esta en 1 indica que se dio un cambio de modo y permite refrescar LCD.
MAX_TCL:        DB 2  ;cantidad maximas de teclas que se leen
Tecla:          DS 1  ;en esta variable se almacena el valor leido del teclado en la subrutina MUX_TECLADO.
Tecla_IN:       DS 1  ;en esta variable se almacena temporalmente el valor de Tecla antes de la supresion de rebotes. Si despues de la supresion se da que Tecla y Tecla_IN son iguales es porque efectivamente se presiono una tecla que debe ser guardada.
Cont_Reb:       DS 1  ;es el contador de ticks del RTI, usado para suprimir rebotes.
Cont_TCL:       DS 1  ;es el indice utilizado para escribir en el arreglo que guarda las teclas presionadas.
Patron:         DS 1  ;es el indice que lleva las iteraciones en subrutina MUX_TECLADO.
Num_Array:      DS 2  ;en este arreglo se almacenan todas las teclas presionadas por el usuario.
CUENTA:         DS 1  ;cantidad de tornillos contados para cada empaque.
AcmPQ:          DS 1  ;cantidad de empaques completados. Entre 0 y 99 y puede rebasar.
CantPQ:         DS 1  ;cantidad maxima de tornillos por cada empaque. Entre 12 y 96. Definido por usuario en MODO CONFIG.
TIMER_CUENTA:   DS 1  ;da la cedencia de incremento de CUENTA. Simula el paso de tornillos. Decrementada por RTI_ISR.
LEDS:           DS 1  ;guarda el estado de los LEDS. LED PB1 corresponde a modo CONFIG, LED PB0 a modo RUN.
BRILLO:         DS 1  ;Variable controlada por PTH3/PTH2 para incrementar/decrementar el brillo de la pantalla LCD.
CONT_DIG:       DS 1  ;cuenta cual digito de 7 segmentos se debe habilitar. Cambia cada vez que CONT_TICKS alcanza 100.
CONT_TICKS:     DS 1  ;contador de ticks de Output Compare para multiplexar.

DT:             DS 1  ;ciclo de trabajo. DT = N-K.
BIN1:           DS 1  ;variable de entrada a subrutina CONV_BIN_BCD. Utilizada para CantPQ y CUENTA.
BIN2:           DS 1  ;variable de entrada a subrutina CONV_BIN_BCD. Utilizada para AcmPQ.
BCD_L:          DS 1  ;variable donde se guarda la salida de BIN_BCD. Utilizada para CONV_BIN_BCD.
LOW:            DS 1  ;variable requerida para el algoritmo de la subrutina BIN_BCD.
VMAX:           DB 245 ;valor maximo de la variable TIMER_CUENTA. Usado para volver a iniciar la cuenta regresiva.
BCD1:           DS 1  ;variable de salida de subrutina BIN_BCD. Tambien es entrada para BCD_7SEG. Utilizada para CantPQ y CUENTA.
BCD2:           DS 1  ;variable de salida de subrutina BIN_BCD. Tambien es entrada para BCD_7SEG. Utilizada para AcmPQ.
DISP1:          DS 1  ;corresponde al valor que se escribe en el display de 7 segmentos.
DISP2:          DS 1  ;BCD2 utiliza DISP1 y DISP2 para desplegarse
DISP3:          DS 1  ;corresponde al valor que se escribe en el display de 7 segmentos.
DISP4:          DS 1  ;BCD1 utiliza DISP3 y DISP4 para desplegarse

CONT_7SEG:      DS 2  ;contador de ticks de OC4 para lograr refrescamiento de LEDS y Displays a 10Hz.
Cont_Delay:     DS 1  ;esta variable se carga con alguna de las siguientes tres constantes para generar retrasos temporales.
D2ms:           DB 100  ;100 ticks a 50kHz son 2 milisegundos
D260us:         DB 13  ;13 ticks a 50kHz son 260 microsegundos
D40us:          DB 2  ;2 ticks a 50kHz son 40 microsegundos
CLEAR_LCD:      DB $01  ;comando para limpiar el LCD
ADD_L1:         DB $80  ;direccion inicio de linea 1
ADD_L2:         DB $C0  ;direccion inicio de linea 2

                ORG $1030
Teclas:         DB $01,$02,$03,$04,$05,$06,$07,$08,$09,$0B,$00,$0E ;valores de las teclas

                ORG $1040
SEGMENT:        DB $3F,$06,$5B,$4F,$66,$6D,$7D,$07,$7F,$6F ;patrones para el display de 7 segmentos de los digitos de 0 a 9.

                ORG $1050
iniDsp:         DB 4,FUNCTION_SET,FUNCTION_SET,ENTRY_MODE_SET,DISPLAY_ON

                ORG $1060
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
CONFIG_MSG1:    FCC "MODO CONFIG"
                DB EOM
CONFIG_MSG2:    FCC "Ingrese CantPQ:"
                DB EOM
RUN_MSG1:       FCC "MODO RUN"
                DB EOM
RUN_MSG2:       FCC "AcmPQ   CUENTA"
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
    BSET PIEH %00001100   ;se habilita keywakeup en PH2 y PH3. Note que PH0 y PH1 se habilitan en modo RUN. PH7 es por polling.
    BCLR PPSH $FF   ;las interrupciones deben ocurrir en el flanco decreciente.

;Configuracion PH7 como entrada de proposito general por polling: (Dipswitch)
    BCLR DDRH %10000000

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

;Configuracion de la salida SAL: relay en puerto PORTE4.
    MOVB #$04,DDRE ;se configura PORTE4 como salida

;Configuracion de pantalla LCD
    MOVB #$FF,DDRK ;todos los pines del puerto K se configura como salida para controlar la LCD.

    CLI        ;habilita interrupciones mascarables.
;------------------------------------------------------------------------------
;                       Inicializacion de variables
;------------------------------------------------------------------------------
    LDS #$3BFF  ;inicializa el stack
;Teclado matricial:
    MOVB #$FF,Tecla
    MOVB #$FF,Tecla_IN
    MOVB #$FF,Num_Array
    CLR Cont_Reb
    CLR Cont_TCL
    CLR Patron

;Displays de 7 segmentos y LEDS:
    CLR CONT_7SEG
    CLR CONT_TICKS
    CLR CONT_DIG
    MOVB #50,BRILLO
    MOVB #$02,LEDS
    CLR BCD1
    CLR BCD2
    MOVB SEGMENT,DISP3 ;para tener DISP3 produciendo un 0
    MOVB SEGMENT,DISP4 ;para tener DISP4 produciendo un 0. Importa mas que nada si se desea que en DISP3 y DISP4 presenten el ultimo valor valido introducido de CantPQ, con OC4

;Programa:
    CLR CantPQ
    CLR CUENTA
    MOVB VMAX,TIMER_CUENTA
    CLR AcmPQ
    CLR Banderas
    BSET Banderas,%00010000 ;CambMod=1 (MODO_CONFIG)

    LDD TCNT ;se carga el valor actual de TCNT para reajustar el output compare
    ADDD #60 ;60 cuentas equivalen 50kHz con prescalador=8
    STD TC4 ;se actualiza el nuevo valor a alcanzar.
    JSR INIT_LCD
;------------------------------------------------------------------------------
MAIN:
    TST CantPQ
    BEQ CONFIG ;CantPQ=0? Ir a CONFIG
    JSR DETERMINE_MODE
    BRSET Banderas %00001000 INIT_CONFIG ;ModActual=1? Ir a INIT_CONFIG

RUN:
    BRCLR Banderas %00010000 EX_RUN ;CambMod=0? Ir a EX_RUN
INIT_RUN:
    BSET PIEH %00000011 ;habilita keywakeup para PH0 y PH1
    MOVB #$01,LEDS ;PB0=ON en modo config.
    LDX #RUN_MSG1
    LDY #RUN_MSG2
    BCLR Banderas %00010000 ;CambMod=0
    JSR Cargar_LCD
EX_RUN:
    JSR MODO_RUN
    BRA COMUN

CONFIG:
    BSET Banderas %00001000 ;ModActual=1. Necesario porque si CantPQ=0 y PH7=0, al momento en que CantPQ tenga un valor valido se pasara directo a MODO_RUN sin pasar por INIT_RUN.
INIT_CONFIG:
    BSET CRGINT %10000000 ;Habilita interrupciones de RTI para poder usar el teclado
    BRCLR Banderas %00010000 EX_CONFIG ;CambMod=0? Ir a EX_CONFIG
    BCLR PIEH %00000011 ;deshabilita keywakeup para PH0 y PH1
    CLR CUENTA ;borra la cuenta que se llevaba puesto que se cambia el tamano del empaque
    CLR AcmPQ ;borra la cuenta de empaques completados
    MOVB #$00,PORTE ;apaga la salida del relay.
    MOVB #$02,LEDS ;PB1=ON en modo config.
    LDX #CONFIG_MSG1
    LDY #CONFIG_MSG2
    BCLR Banderas %00010000 ;CambMod=0
    MOVB CantPQ,BIN1 ;si hay un cambio de modo, se debe actualizar el display
    JSR Cargar_LCD
EX_CONFIG:
    JSR MODO_CONFIG

COMUN:
    BRA MAIN

;------------------------------------------------------------------------------
;   Subrutina Send_Command: se encarga de enviar al LCD el comando que recibe
;     por el acumulador A.
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
;   Subrutina INIT_LCD: se encarga de inicializar la pantalla LCD ejecutando la
;     secuencia correcta de comandos.
;------------------------------------------------------------------------------
INIT_LCD:
                LDX #iniDsp+1 ;Se carga en X la tabla que contiene los comandos de inicializacion. Posicion 0 tiene el tamano de la tabla.
                CLRB
COMMANDS:
                LDAA B,X ;Se recorren los comandos con direccionamiento indexado por acumulador B
                JSR Send_Command ;Se ejecuta cada comando
                MOVB D40us,Cont_Delay ;40us son necesarios luego de enviar cualquiera de los comando de inicializacion
                JSR Delay
                INCB ;siguiente comando
                CMPB iniDsp
                BNE COMMANDS ;Si ya se ejecutaron todos los comandos de la tabla, terminar comandos de inicialización
                LDAA CLEAR_LCD ;Cargar comando de limpiar pantalla
                JSR Send_Command ;enviar comando de limpiar pantalla
                MOVB D2ms,Cont_Delay ;luego de enviar comando limpiar pantalla se debe esperar 2ms
                JSR Delay
                RTS

;------------------------------------------------------------------------------
;   Subrutina Delay: se mantiene en un loop cerrado hasta que Cont_Delay sea 0.
;     Cont_Delay es descontado por OC4 a 50 kHz.
;------------------------------------------------------------------------------
Delay:
                TST Cont_Delay
                BNE Delay
                RTS

;------------------------------------------------------------------------------
;   Subrutina DETERMINE_MODE: esta subrutina calcula el estado de las banderas
;     ModActual y CambMod cuando se verifica si MODSEL=ModActual.
;------------------------------------------------------------------------------
DETERMINE_MODE:
                LDAA PTIH
                LSLA ;PTH7 esta en C
                BCS MODSEL1 ;se revisa si MODSEL esta en 1
                BRCLR Banderas %00001000 FIN_DETERMINE ;MODSEL es 0, se verifica por modo RUN
                BCLR Banderas %00001000 ;MODSEL es 0, se pone ModActual en Banderas en 0
                BSET Banderas %00010000 ;Se denota que hubo un cambio de modo con CambMod=1
                BRA CLEAN_SCREEN ;se limpia la pantalla si hubo un cambio de modo
MODSEL1:
                BRSET Banderas %00001000 FIN_DETERMINE ;MODSEL es 1, se verifica por modo CONFIG
                BSET Banderas %00001000 ;MODSEL es 1, se pone ModActual en Banderas en 1
                BSET Banderas %00010000 ;Se denota que hubo un cambio de modo con CambMod=1
CLEAN_SCREEN:
                LDAA CLEAR_LCD ;cuando se cambia de modo, se limpia la pantalla
                JSR Send_Command ;envio de comando de limpieza de pantalla
                MOVB D2ms,Cont_Delay ;luego de enviar comando limpiar pantalla se debe esperar 2ms
                JSR Delay
FIN_DETERMINE:
                RTS

;------------------------------------------------------------------------------
;   Subrutina BCD_BIN: el arreglo Num_Array corresponde a un numero en BCD donde
;     cada entrada es un digito. Esta subrutina toma este arreglo y calcula en
;     binario el valor numerico del arreglo. El resultado se almacena en CantPQ.
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
                STAB CantPQ ;se guarda el resultado en la variable designada
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
;     BIN2 y realiza la conversion a BCD de cada una de estas variables. La
;     conversion de BIN2 solo se da en el modo RUN puesto que es el unico modo
;     en el que BIN2 tiene valores relevantes. Ademas, luego de la conversion, si
;     el numero es menor que 10 significa que el display de 7 segmentos utilizado
;     para las decenas no es necesario que este encendido; en este caso se escribe
;     $B en el nibble mas significativo de BCD1 y BCD2 para indicarlo.
;------------------------------------------------------------------------------
CONV_BIN_BCD:
                LDAA BIN1 ;se carga parametro de entrada a BIN_BCD_BASE
                JSR BIN_BCD
                LDAA BCD_L
                CMPA #10
                BHS TRF_BCD1 ;si el numero es mayor o igual a 10 no hay que apagar ninguno display
                ORAA #$F0 ;
                SUBA #$40 ;se pone $B en nibble mas significativo para indicar que el display se debe apagar.
TRF_BCD1:
                STAA BCD1 ;se guarda resultado en variable de salida
                BRSET Banderas %00001000 FIN_BIN_BCD ;en modo CONFIG (ModActual=1) no es necesario convertir BIN2
                LDAA BIN2 ;se carga parametro de entrada a BIN_BCD
                JSR BIN_BCD
                LDAA BCD_L
                CMPA #10
                BHS TRF_BCD2 ;si el numero es mayor o igual a 10 no hay que apagar ninguno display
                ORAA #$F0
                SUBA #$40 ;se pone $B en nibble mas significativo para indicar que el display se debe apagar. $F-$4=$B
TRF_BCD2:
                STAA BCD2 ;se guarda resultado en variable de salida
FIN_BIN_BCD:
                RTS

;------------------------------------------------------------------------------
; Subrutina TAREA_TECLADO: En esta subrutina se da la lectura del teclado. Aqui
;     se lee el teclado en el puerto A, se suprimen los rebotes, y se maneja la
;     situacion de tecla retenida.
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
;     el uso de TAREA_TECLADO se da la lectura del valor CantPQ. Una vez que el
;     usuario presiona ENTER se valida que el valor de CantPQ este entre 20 y 90.
;     Si es asi entonces coloca este valor en BIN1 para que pueda ser desplegado
;     en los displays 3 y 4. Cuando el valor no es valido se borra CantPQ.
;------------------------------------------------------------------------------
MODO_CONFIG:
                BRSET Banderas %00000100 CHECK_CONFIG ;se revisa si ARRAY_OK es 0
                JSR TAREA_TECLADO ;si es 0, se revisa por un CantPQ
                RTS
CHECK_CONFIG:
                JSR BCD_BIN ;si ARRAY_OK es 1, se verifica si la cantidad ingresada es valida
                LDAA #20
                LDAB #90 ;limites de valores para CantPQ
                CMPA CantPQ
                BHI ERASE_CANTPQ ;se ve si Cant_PQ es mayor que 20
                CMPB CantPQ
                BLO ERASE_CANTPQ ;se ve si Cant_PQ es menor que 90
                MOVB CantPQ,BIN1 ;si el valor es valido, se carga CantPQ en BIN1
                BRA ERASE_ARRAYOK ;el valor es valido por lo que no se borra
ERASE_CANTPQ:   CLR CantPQ ;si el valor es invalido se borra CantPQ
ERASE_ARRAYOK:
                BCLR Banderas %00000100 ;se limpia ARRAY_OK para poder recibir otra cantidad
                RTS

;------------------------------------------------------------------------------
;   Subrutina MODO_RUN: esta subrutina se encarga de simular el conteo de
;     tornillos a 4Hz por medio del contador de ticks TIMER_CUENTA. Los tornillos
;     se cuentan en la variable CUENTA y cuando esta alcanza CantPQ se enciende
;     el relay en el puerto PORTE4, ademas de incrementar AcmPQ para mostrar que
;     se termino otro paquete. No se permite que AcmPQ supere un valor de 99.
;------------------------------------------------------------------------------
MODO_RUN:
                LDAA CantPQ ;carga CantPQ en A para comparaciones
                TST TIMER_CUENTA ;se ve si TIMER_CUENTA ha llegado a 0
                BNE COMUN_RUN ;si no ha llegado a 0, se retorna
                INC CUENTA ;caso contrario, la cuenta incrementa en 1 tornillo
                MOVB VMAX,TIMER_CUENTA ;se recarga TIMER_CUENTA
                CMPA CUENTA ;se ve si la cuenta ha llegado a CantPQ
                BNE COMUN_RUN ;si no, se retorna
                INC AcmPQ ;si se llega a CantPQ, se lleno un paquete, por lo que se incrementa AcmPQ
                BCLR CRGINT %10000000 ;se deshabilitan las interrupciones RTI
                MOVB #$04,PORTE ;se activa el relay
                LDAA #100
                CMPA AcmPQ ;se ve si AcmPQ ha llegado a 100 por rebase
                BNE COMUN_RUN ;si no se ha llegado a 100, retorna
                CLR AcmPQ ;si se llega a 100, se hace rebase
COMUN_RUN:
                MOVB CUENTA,BIN1
                MOVB AcmPQ,BIN2
                RTS

;------------------------------------------------------------------------------
;   Subrutina Cargar_LCD: esta subrutina se encarga de enviar a la pantalla LCD
;     cada caracter, uno por uno, de ambas lineas del LCD. Recibe los parametros
;     en los registros indice X y Y, que contienen las direcciones de inicio a
;     los mensajes de las lineas 1 y 2 respectivamente.
;------------------------------------------------------------------------------
Cargar_LCD:
                LDAA ADD_L1 ;Se carga la direccion de la primera posición de la primera fila de la LCD
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
		BEQ FIN_Cargar_LCD ;Si se encuentra un caracter de EOM ($00) se terminó de imprimir la primera fila
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
;     sus valores. En modo CONFIG (MODSEL=1) se apagan DISP1 y DISP2. Cuando
;     CPROG=0 se tiene la excepcion que se quieren desplegar ambos ceros
;------------------------------------------------------------------------------
BCD_7SEG:
                TST CantPQ
		BNE CONVERSION ;cuando CPROG es cero se deben forzar al display ambos ceros.
		CLR DISP1 ;apaga displays de ACUMUL
		CLR DISP2 ;apaga displays de ACUMUL
		MOVB SEGMENT,DISP3 ;fuerza el cero en displays de CUENTA
                MOVB SEGMENT,DISP4 ;fuerza el cero en displays de CUENTA. Si se quisiera poner el ultimo valor valido, se borran estas 2 lineas
                BRA FIN_BCD_7SEG

CONVERSION:
		LDX #SEGMENT ;direccion base de los valores para escribir en el puerto B.
		LDAB #$0F ;mascara para nibble menos significativo
		LDAA #$F0 ;mascara para nibble mas significativo
		ANDB BCD1 ;se extrae el nibble menos significativo de BCD1.
                MOVB B,X,DISP4
		ANDA BCD1 ;se extrae el nibble mas significativo de BCD1.
                CMPA #$B0
		BEQ DISP3_OFF ;cuando el nibble mas significativo es $B se debe apagar el DISP3
                LSRA
                LSRA
                LSRA
		LSRA ;se traslada el nibble mas significativo a la parte baja del byte.
                MOVB A,X,DISP3
                BRA AHORA_BCD2
DISP3_OFF:
                MOVB #$00,DISP3

AHORA_BCD2:
                BRSET Banderas %00001000 NO_BCD2
                LDAB #$0F ;mascara para nibble menos significativo
                LDAA #$F0 ;mascara para nibble mas significativo
                ANDB BCD2 ;se extrae el nibble menos significativo de BCD1.
                MOVB B,X,DISP2
                ANDA BCD2 ;se extrae el nibble mas significativo de BCD1.
                CMPA #$B0
                BEQ DISP1_OFF ;cuando el nibble mas significativo es $F se debe apagar el DISP1
                LSRA
                LSRA
                LSRA
                LSRA ;se traslada el nibble mas significativo a la parte baja del byte.
                MOVB A,X,DISP1
                BRA FIN_BCD_7SEG
DISP1_OFF:
                MOVB #$00,DISP1
                BRA FIN_BCD_7SEG

NO_BCD2:
                MOVB #$00,DISP1
                MOVB #$00,DISP2
FIN_BCD_7SEG:
		RTS

;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion RTI: Esta subrutina descuenta contadores
;     siempre y cuando no sean cero. Los ticks del RTI duran 1.024 ms, por lo
;     que si se cargan variables con X valor se pueden contar aproximadamente
;     X milisegundos. Cont_Reb tiene un valor maximo de 10; se utiliza para
;     suprimir rebotes contando ~10ms. TIMER_CUENTA tiene un valor maximo de
;     VMAX (245), y cuenta ~250ms; se utiliza para simular la cuenta de tornillos
;     a 4 Hz.
;------------------------------------------------------------------------------
RTI_ISR:
                BSET CRGFLG %10000000 ;se limpia la bandera de interrupcion RTI
                TST Cont_Reb ;se ve si el contador de rebotes llego a 0
                BEQ CHECK_TIMER;si no, se pasa a revisar el timer
                DEC Cont_Reb
CHECK_TIMER:
                TST TIMER_CUENTA ;se revisa si el timer ha llegado a 0
                BEQ FIN_RTI
                DEC TIMER_CUENTA
FIN_RTI:
                RTI

;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion keywakeup en puerto H: revisa las 4
;     posibles fuentes de interrupcion PH0, PH1, PH2, PH3. La primer borra el
;     valor de CUENTA, la segunda borra el valor de AcmPQ, la tercera disminuye
;     el brillo en un 5%, la cuarta aumenta el brillo en un 5%.
;------------------------------------------------------------------------------
PTH_ISR:
                BRSET Banderas %00001000 NO_RUN ;las interrupciones PTH
                BRCLR PIFH $01 PTH1 ;si es 1, se revisa por la siguiente fuente de interrupcion
                BSET PIFH $01 ;se apaga la bandera de la fuente de interrupcion
                BSET CRGINT %10000000 ;Habilita interrupciones de RTI
                MOVB #$00,PORTE ;se apaga el relay
                CLR CUENTA ;se reinicia la cuenta
PTH1:
                BRCLR PIFH $02 NO_RUN ;si es 1, se revisa por la siguiente fuente de interrupcion
                BSET PIFH $02 ;se apaga la bandera de la fuente de interrupcion
                CLR AcmPQ ;se limpia el acumulador
NO_RUN:
		LDAA BRILLO
                LDAB #5
                BRCLR PIFH $04 PTH3 ;si es 1, se revisa por la siguiente fuente de interrupcion
                BSET PIFH $04 ;se apaga la bandera de la fuente de interrupcion
                TSTA
                BEQ PTH3 ;si BRILLO es 0, no se le puede restar 5 y se revisa por la siguiente interrupcion
                SBA
                STAA BRILLO ;se le resta 5 a BRILLO
PTH3:
                BRCLR PIFH $08 FIN_PTH ;si es 1, se termina la subrutina
                BSET PIFH $08 ;se apaga la bandera de la fuente de interrupcion
                CMPA #100
                BEQ FIN_PTH ;si BRILLO es 100, no se le puede sumar 5 y se termina la rutina
                ABA
                STAA BRILLO ;se le suma 5 a BRILLO
FIN_PTH:
                RTI ;termina la rutina de atencion a interrupciones

;------------------------------------------------------------------------------
;   Subrutina de servicio a interrupcion por output compare en el canal 4:
;     Descuenta Cont_Delay, refresca cada 100 ms (5000ticks) los valores de
;     DISP1-DISP4, multiplexa el bus del puerto B para mostrar informacion en
;     los displays de 7 segmentos y los LEDS, y todo con un ciclo de trabajo
;     variable que depende de BRILLO.
;------------------------------------------------------------------------------
OC4_ISR:
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
                LDAA #100
                SUBA BRILLO
                STAA DT
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
                BSET TFLG1 %00010000 ;se reinicia la bandera de interrupcion
                LDD TCNT ;se carga el valor actual de TCNT para reajustar el output compare
                ADDD #60 ;60 cuentas equivalen 50kHz con prescalador=8
                STD TC4 ;se actualiza el nuevo valor a alcanzar.
                RTI