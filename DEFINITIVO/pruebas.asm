;##############################################################################
;                            Proyecto Final: Runmeter623
;   Fecha: 01/03/2020
;   Autores: Roberto S�nchez Y Guillermo Ram�rez
;
;   Este programa tiene como fin simular un dispositivo capaz de medir la velocidad
;   a la cual se mueve un ciclista por una pista. Para ello se utilizan las inte-
;   rrupciones PH� y PJ0 para simular los botones y se tienen insterrupciones para
;   llevar los tiempos. Adem�s posee 4 modos accesables por medio de las configuraciones
;   en los dipswitch ph7 y ph6.
;
;##############################################################################
#include registers.inc

;===============================================================================
;     Declaracion de las estructuras de datos y vectores de interrupcion
;===============================================================================
;LCD:
EOM:            EQU $00
CLEAR_DISPLAY:  EQU $01
RETURN_HOME:    EQU $02
ENTRY_MODE_SET: EQU $06
DISPLAY_ON:     EQU $0C
DDRAM_ADDR1:    EQU $80
DDRAM_ADDR2:    EQU $C0
FUNCTION_SET:   EQU $28

;Interrupciones
                org $3E4C   ;Interrupcion PTH
                dw CALCULAR ;direccion de interrupcion PTH
                org $3E52   ;Interrupcion ATD0
                dw ATD_ISR  ;direccion de interrupcion ATD0
                org $3E5E   ;Interrupcion TCNT
                dw TCNT_ISR ;direccion de interrupcion TCNT
                org $3E66   ;Interrupcion OC4
                dw OC4_ISR  ;direccion de interrupcion OC4
                org $3E70   ;Interrupcion RTI
                dw RTI_ISR  ;direccion de interrupcion RTI


;Estructuras de datos
                org $1000

Banderas:       ds 1  ;COMPE:COMPE_STATE:X:CALC_TICKS:PANT_FLG:ARRAY_OK:TCL_LEIDA:TCL_LISTA.
                      ;COMPE = indica si ya se configuro el modo competencia
                      ;COMPE_STATE = indica el estado en que esta el modo competencia
                      ;X
                      ;CALC_TICKS = indica si ya se contaron los ticks
                      ;PANT_FLAG = Indica que se debe imprimir en pantalla
                      ;Arrayok = indica si el array esta bien
                      ;tcl_leida = indica si se leyo un tecla en cierta corrida


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
DT:             ds 1  ;ciclo de trabajoDT = N-K
CONT_7SEG:      ds 2  ;contador de ticks de OC4

CONT_200:       ds 1

Cont_Delay:     ds 1
D2ms:           db 100  ;2 milisegundos
D240us:         db 12   ;240us
D40us:          db 2  ;40 microsegundos
CLEAR_LCD:      db $01  ;comando para limpiar el LCD
ADD_L1:         db $80  ;direccion inicio de linea 1
ADD_L2:         db $C0  ;direccion inicio de linea 2

SAVE_MED:       DS 2  ;WORD PARA LOS 55 M
TICKS_100:      DS 2  ;Variable WORD utilizada para medir la cantidad de ticks que deben pasar para recorrer 100 m

                org $1040
Teclas:         db $01,$02,$03,$04,$05,$06,$07,$08,$09,$0B,$00,$0E ;valores de las teclas

                org $1050
SEGMENT:        db $3F,$06,$5B,$4F,$66,$6D,$7D,$07,$7F,$6F,$40,$00 ;patrones para el display de 7 segmentos de los digitos

                org $1060
iniDsp:         db 4,FUNCTION_SET,FUNCTION_SET,ENTRY_MODE_SET,DISPLAY_ON


                ORG $1070 ;mensajes
MSG_LIBRE1:     fcc "  RunMeter 623  "
                db EOM
MSG_LIBRE2:     fcc "   MODO LIBRE   "
                db EOM
MSG_COMPE1:     fcc " M.COMPETENCIA "
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

;===============================================================================
;                            Configuracion del hardware
;===============================================================================

                org $2000
                cli        ;habilita interrupciones mascarables
                lds #$3BFF  ;inicializa el stack

;DIPSWITCH PH7 Y PH6
                bclr DDRH,$C0

;Configuracion RTI
                bset CRGINT $80             ;Habilita RTI
                movb #$17,RTICTL            ;periodo 1.024ms

;Keywakeup en puerto H
                bclr PIEH,$0F   ;se deshabilita keywakeup en PH0 y PH3.
                movb #$00,PPSH ;las interrupciones deben ocurrir en el flanco decreciente.


;Output Compare en Canal 4
                bset TIOS $10 ;Habilida canal 4
                bset TIE $10 ;Interrup en canal 4
                bclr TCTL1 $03
                bset TSCR1 $80 ;Habilita temporizacion
                bset TSCR2 $04 ;Preescalador magnitud 16

;Pantalla 7 segmentos y leds
                movb #$0F,DDRP ;habilita pantallas de 7 segmentos
                movb #$FF,DDRB ;configura b como salidas
                bset DDRJ,$02 ;salidas en los leds

;LCD
                movb #$FF,DDRK ;Control de LCD

;Configuracion del ATD
                movb #$30,ATD0CTL3
                movb #$B3,ATD0CTL4
                movb #$80,ATD0CTL5

;Teclado puerto A
                movb #$F0,DDRA        ;parte alta de A como salida y parte baja como entrada
                bset PUCR $01       ;resistencias de pullup para el teclado


;===============================================================================
;                       Inicializacion de variables
;===============================================================================

;Variables teclado matricial
                movb #$FF,Tecla
                movb #$FF,Tecla_IN
                movb #$FF,Num_Array
                clr Cont_Reb
                clr Cont_TCL
                clr Patron
                ldaa MAX_TCL
                ldx #NUM_ARRAY-1

BORRAR_ARRAY:                   
                movb #$FF,A,X
                dbne A,BORRAR_ARRAY         ;borra variables respectivas al teclado

                movb SEGMENT,DISP3 ;
                movb SEGMENT,DISP4 ;
                movb #0,BRILLO
                movb #$02,LEDS
                clr CONT_7SEG
                clr CONT_TICKS
                clr CONT_DIG
                clr BCD1
                clr BCD2

                movw #$0000,TICKS_100
                movw #$0000,TICK_EN
                movw #$0000,TICK_DIS
                clr Banderas
                clr NumVueltas
                clr VUELTAS
                clr ValorVueltas
                clr VELPROM
                clr VELOC
                clr Cont_Reb
                clr Cont_TCL

                bset TIE,$10 ;se habilitan las interrupciones por output compare en canal 4
                bset TSCR1,$80 ;se habilita el modulo de timer
                bset CRGINT,$80 ;se habilitan las interrupciones RTI
                movb #$C2,ATD0CTL2
    
                ldd TCNT
                addd #30    ; ticks para el prescalador elegido
                std TC4 ;se carga el valor inicial para interrupcion de OC4
    ;se habilitan las interrupciones por ATD0
                ldaa #160
CONFIG_ATD:
                    dbne A,CONFIG_ATD ;3;tiempo de inicio del ATD (10us)

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


;===============================================================================
;===============================================================================
;===============================================================================

MAIN:
FIRST_CONFIG:
                movb #$BB,BIN2           ;apaga pantallas 7seg
                movb #$BB,BIN1
                jsr MODO_CONFIG ;Se mantiene hasta que se ingrese un numero de vueltas
                tst NumVueltas
                beq FIRST_CONFIG
                bra DIP_SWITCH


LLAMAR_LIBRE:
                bclr PIEH,%00001001
                jsr MODO_LIBRE
                bra DIP_SWITCH


LLAMAR_COMPE:
                jsr MODO_COMPETENCIA     ;se ejecuta el modo competicion
                bra DIP_SWITCH  ;se vuelve a leer el modo de operacion


LLAMAR_RESUMEN:
                bclr PIEH,%00001001
                jsr MODO_RESUMEN       ;se ejecuta el modo resumen
                bra DIP_SWITCH     ;se vuelve a leer el modo de operacion


LLAMAR_CONFIF:
                bclr PIEH,%00001001
                jsr MODO_CONFIG      ;se ejecuta el modo config
                bra DIP_SWITCH     ;se vuelve a leer el modo de operacion



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
    
;===============================================================================
;===============================================================================
;===============================================================================

MODO_CONFIG:
                BCLR Banderas,$80   ; borra bandera de competencia just in case
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

;===============================================================================
;===============================================================================
;===============================================================================

BCD_BIN:        ldx #Num_Array
                ldab 1,x           ;revisamos si la unidad es distinta de FF
                cmpb #$FF
                beq UNIDAD         ;si es FF, el valor no es valido
                stab ValorVueltas       ;Si no, lo guarda en ValorVueltas
                bra DECENA         ;lee decenas

UNIDAD:
                movb Num_Array,ValorVueltas
                rts

DECENA:
                clra
                ldab Num_Array    ;carga en b
                ldy #10           ;multiplica decenas por 10
                emul
                addb ValorVueltas
                stab ValorVueltas
                rts

;===============================================================================
;===============================================================================
;===============================================================================

RTI_ISR:        bset CRGFLG,$80                 ;borra bandera de interrupcion RTI
                tst Cont_Reb                    
                beq TIMER                       ;si llegaron los rebotes a 0, se termina la rubrutina
                dec Cont_Reb

TIMER:          tst CONT_200
                bne NO_RESET
                Movb #200,CONT_200              ;Volvemos al contador con 200
                Movb #$87,ATD0CTL5
                Bra FIN_RTI


NO_RESET:
                dec CONT_200                    ;Decrementamos el contador de rebotes si aun no ha llegado a cero


FIN_RTI:
                Rti

;===============================================================================
;===============================================================================
;===============================================================================

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

;===============================================================================
;===============================================================================
;===============================================================================

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

;===============================================================================
;===============================================================================
;===============================================================================

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

;===============================================================================
;===============================================================================
;===============================================================================


ATD_ISR:
                ldd ADR00H                      ;Se hace en d para sumar words
                addd ADR01H
                addd ADR02H
                addd ADR03H
                addd ADR04H
                addd ADR05H
                ldx #6
                idiv
                tfr X,D
                stab POT                        ;Guardar el promedio
                ldaa #20                        ;calculo de POT
                mul
                ldx #255
                idiv
                tfr X,D
                stab BRILLO                     ;se guarda brillo

                RTI

;===============================================================================
;===============================================================================
;===============================================================================

MODO_COMPETENCIA:
                brset Banderas,$80,ESTADO_COMPE ;revisa si ya esta se configuro la competencia una vez
                clr Banderas                    ;borra todas las baderas para inicial de nuevo
                bset Banderas,$80               ;se actualiza el modo previo
                ldx #MSG_LIBRE1                 ;se carga el mensaje inicial
                ldy #MSG_ESPERA
                jsr Cargar_LCD
                bclr Banderas,$04               ;Borrar ARRAY_OK
                bset TSCR2,$80                  ;habilita TCNT
                bset PIEH,$09                   ;habilita keywakeup en PH0 y PH3
                movb #$BB,BIN1
                movb #$BB,BIN2

                clr ValorVueltas
                clr VELOC
                clr Num_Array
                clr VUELTAS
                clr VELPROM


ESTADO_COMPE:
                movb #$04,LEDS
                brclr Banderas,$40,VEL_ZERO     ;se revisa si se debe imprimir el mensaje calculando
                bclr Banderas,$40
                ldx #MSG_LIBRE1                 ;se imprime el mensaje calculando
                ldy #MSG_CALCULANDO
                jsr Cargar_LCD

VEL_ZERO:
                tst VELOC                       ;control de pantalla hasta que la velocidad no es 0
                beq FIN_COMP
                jsr PANT_CTRL

FIN_COMP:
                rts

;===============================================================================
;===============================================================================
;===============================================================================

PANT_CTRL:
                bclr PIEH,$09       ;deshabilita el puerto H

                ldx SAVE_MED        ;se trean los ticks alcanzados
                cpx #129            ;129 ticks son 35 km/h, limite inferior
                bhi INVALID_PANT 
                cpx #48             ;95 ticks 95 km/h, limite superior
                blo INVALID_PANT 
                clr SAVE_MED

                BRSET Banderas,$10,VALID_PANT ;La velocidad es valida, se revisa CALC_TICKS
                BSET Banderas,$10 ;si CALC_TICKS es 0, se realizan los calculos asociados y se pone en 1


                ldy TICKS_100 ;los ticks leidos son medidos en 55m
                ldd #100 ;se multiplican por 100
                emul ;D tiene la cantidad de ticks que pasan en 5500 m
                ldx #55 ; divide entre 55 y quedan 100m
                idiv 
                stx TICKS_100 ;TICKS que pasan en 100 m para medir el tiempo que se tarda en mostrar mensaje

                tfr x,d
                addd TICKS_100
                std TICK_EN ;TICKS para 200m
                addd TICKS_100
                std TICK_DIS ;TICKS para 300m
                movw #$0000,TICKS_100 ;se borra el temporizador de 100m
                rts
                
VALID_PANT:
                BRSET Banderas,$08,RESULTADO ;CALC_TICKS 1 se revisa si pantalla apagada
                LDAA BIN1 ;PANT_FLG es 0
                CMPA #$BB
                BNE ESTADO_ESPERA ;si A es $BB, se retorna
                RTS

INVALID_PANT:        ldaa BIN1               ;la velocidad es invalida, se revisa si BIN1 es $AA
                cmpa #$AA
                beq ESTADO_ESPERA    ;Si no se han puesto las lineas, se ponen
                movw #$0000,TICK_EN     ;no es $AA, borra TICK_EN
                movw #69,TICK_DIS       ;mantiene 3s
                movb #$AA,BIN1          ;se ponen rayas en la pantalla de 7 segmentos
                movb #$AA,BIN2
                bset Banderas,$08 ;se levanta PANT_FLG
                ldx #MSG_ALERTA1        ;se carga el mensaje de alerta
                ldy #MSG_ALERTA2
                jsr Cargar_LCD
                rts

ESTADO_ESPERA:
                brset Banderas,$08,FIN_PANT ;BIN1 es $AA, se revisa PANT_FLG

                ldx #MSG_LIBRE1 ;carga del mensaje inicial
                ldy #MSG_ESPERA
                jsr Cargar_LCD
                movb #$BB,BIN1 ;se apagan las pantallas de 7 segmentos
                movb #$BB,BIN2

                ldab VUELTAS        ; se revisa si se lleg'o al maximo
                cmpb NumVueltas
                beq BORRAR_PANTALLAS
                bset PIEH,$09       ; deshabilida interrupciones por puertos ph3 y ph0

BORRAR_PANTALLAS:
                bclr Banderas,$10 ;se limpian las variables de la pantalla
                clr VELOC
                rts

RESULTADO:
                ldaa BIN1
                cmpa #$BB
                bne FIN_PANT

                ldx #MSG_COMPE1
                ldy #MSG_COMPE2
                jsr Cargar_LCD
                movb VUELTAS,BIN2
                movb VELOC,BIN1
FIN_PANT:
                rts

;===============================================================================
;===============================================================================
;===============================================================================

CALCULAR:
                brset PIFH,$08,PH3_ISR          ; se revisa cual interrupcion es
                brset PIFH,$01,PH0_ISR
NO_VALID_CALC:
                rti
                
;===============================================================================
;===============================================================================
;===============================================================================
PH0_ISR:
                bset PIFH,$01                   ;borra bandera de interrupcion
                tst Cont_Reb                    ;revisa rebotes
                bne FIN_PH0
                movb #100,Cont_Reb              ;configur rebotes
                ldx TICK_MED                    ;se lee los ticks medidos
                movw TICK_MED,SAVE_MED
                cpx #129                        ;129 ticks para 35 km/h, limite inferior
                bhi VELOCID_INVALID             
                cpx #48                         ;48 ticks para 95 km/h, llimite inferior
                blo VELOCID_INVALID             
                stx TICKS_100                  ;se guarda la cantidad de ticks necesarios para recorrer 55 m
                ldd #4532                       ;D = 4532
                idiv                            
                tfr X,D                         ;veloc = 4532/TICKS 
                stab VELOC                      ;guardamos velocidad
                inc VUELTAS                     ;incrementa vueltas
                ldab VELPROM                    ;calculo del promedio = (velprom*vueltas - velprom + veloc)/vueltas
                ldaa VUELTAS 
                MUL
                tfr b,a
                ldab VELPROM
                sba
                ldab VELOC
                aba
                tfr a,b
                clra
                tfr d,y
                ldab VUELTAS
                tfr d,x
                tfr y,d
                idiv
                tfr x,d
                stab VELPROM                    ;velocidad promedio     
                BRA FIN_PH0

VELOCID_INVALID:
                bset VELOC,$FF                  ;velocidad fuera de limites
                bra FIN_PH0

FIN_PH0:
                rti


;===============================================================================
;===============================================================================
;===============================================================================

PH3_ISR:
                bset PIFH,$08                   ;limpia bandera de interrup para volver a usarla
                tst Cont_Reb
                bne FIN_PH3
                clr SAVE_MED
                BSET Banderas,$40               ;activa DISPLAY_CALC
                movb #100,Cont_Reb              ;recarga contadore de rebases
                movw #$0000,TICK_MED            ;se borra TICK_MED

FIN_PH3:
                rti

;===============================================================================
;===============================================================================
;===============================================================================

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

;===============================================================================
;===============================================================================
;===============================================================================

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

;===============================================================================
;===============================================================================
;===============================================================================

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

;===============================================================================
;===============================================================================
;===============================================================================

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

;===============================================================================
;===============================================================================
;===============================================================================

Cargar_LCD:     ldaa ADD_L1                     ;inicio de linea
                jsr Send_Command                ;env?a comando
                movb D40uS,Cont_Delay           ;delay
                jsr Delay

LINEA1:         ldaa 1,x+                       ;Se va cargando mensaje
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

;===============================================================================
;===============================================================================
;===============================================================================


TCNT_ISR:
                bset TFLG2,$80                  ;activa interrupcion

                ldd TICK_EN                     ;Se ve si es 0 para cargar en pantalla
                cpd #0
                beq TICKEN_ZERO
                cpd #$FFFF                      ;o si es FFFF para reiniiar contador 
                beq TICKMED_FULL
                subd #1                         ;si no se cumple ninguna las dos condiciones revisadas, se decrementa y guarda el valor
                std TICK_EN
                bra TICKMED_FULL

TICKEN_ZERO:
                bset Banderas,$08               ;se actuva PANT_FLG
                ldy TICK_EN
                bset TICK_EN,$FF
                bset 1,Y,$FF                    ;llena de FF
                
TICKMED_FULL:
                ldd TICK_MED                    ;Si llega a FF se reinicia a 0
                cpd #$FFFF
                beq TICKDIS_REV
                addd #1                         ;si no es $FFFF, se incrementa y se guarda el valor
                std TICK_MED

TICKDIS_REV:
                ldd TICK_DIS                    ;se carga TICK_DIS y si es 0 desactiva pnatalla
                cpd #0                          ; no se puede usar tst, es un word
                beq TICKDIS_ZERO
                cpd #$FFFF                      ;se ve si es $FFFF, si es FFFF termian
                beq FIN_TCNT
                subd #1                         ;si no se cumple ninguna las dos condiciones revisadas, se decrementa y guarda el valor
                std TICK_DIS
                bra FIN_TCNT

TICKDIS_ZERO:
                bclr Banderas,$08               ;desactiva PANT_FLG
                ldy TICK_DIS
                bset TICK_DIS,$FF               ;recarga FF
                bset 1,Y,$FF

FIN_TCNT:
                rti

;===============================================================================
;===============================================================================
;===============================================================================

Delay:
                tst Cont_Delay      ;Espera hasta que OC4 disminuya
                bne Delay
                rts

;===============================================================================
;===============================================================================
;===============================================================================

Send_Command:   psha                    ;se guarda a en pila
                anda #$F0               ;mascara de parte alta
                lsra                    ;deja limpios los dos bits menos significativos
                lsra
                staa PORTK              ;guarda a en portk
                bclr PORTK,$01          ;modif bits menos significativos
                bset PORTK,$02
                movb D240us,Cont_Delay  ;delay
                jsr Delay
                bclr PORTK,$02
                pula                    ;trae a
                anda #$0F               ;mascara parte baja
                lsla
                lsla
                staa PORTK
                bclr PORTK,$01
                bset PORTK,$02
                movb D240us,Cont_Delay  ; delay
                jsr Delay
                bclr PORTK,$02
                rts

;===============================================================================
;===============================================================================
;===============================================================================

Send_Data:
                psha 
                anda #$F0 
                lsra
                lsra 
                staa PORTK 
                bset PORTK,$03 
                movb D240us,Cont_Delay
                jsr Delay
                bclr PORTK,$02 
                pula 
                anda #$0F 
                lsla
                lsla 
                staa PORTK 
                bset PORTK,$03 
                movb D240us,Cont_Delay
                jsr Delay
                bclr PORTK,$02
                rts

;===============================================================================
;===============================================================================
;===============================================================================

MODO_RESUMEN:
                bclr Banderas,$80    ;se actualiza el modo previo
    		bclr PIEH,$09        ;apaga los sensores
		ldx #MSG_RESUMEN ;carga el mensaje resumen en la pantalla LCD
                ldy #MSG_COMPE2
                jsr Cargar_LCD
                movb #$08,LEDS
                movb VUELTAS,BIN2 ;carga los valores de VUELTAS y VELPROM en 7 segmentos
                movb VELPROM,BIN1
                rts

;===============================================================================
;===============================================================================
;===============================================================================

MODO_LIBRE:
                brclr CRGINT,$80,FIN_LIBRE  ;Revisa si ya estuvo en libre 1 vez para no volver a configurar
                clr VELOC
                clr VUELTAS
                bclr TSCR2,$80
                bclr PIEH %00001001         ;deshabilita los botones por pieh
                bclr Banderas,$80           ;borra bandera de competencia

                ldx #MSG_LIBRE1
                ldy #MSG_LIBRE2
                jsr Cargar_LCD
                bclr CRGINT,$80
                bclr TIE,$10
                movb #$FF,PTP
                movb #$00,PTJ               ;habilita LEDS
                movb #$01,PORTB             ;enciende los leds
FIN_LIBRE:
                rts

