;##############################################################################
;                                 Tarea #5
;   Fecha: 14 de febrero 2021
;   Autor: Luis Guillermo Ram�rez y Roberto S�nchez
;
;   Descripcion: El programa en cuesti�n simula una sistema capaz de tomar como
;   entrada un valor num�rico entre 25 y 85, el c�al cuando se carga con Enter,
;   le indica al sistema que empiece un conteo desde 0 hasta el valor indicado
;   aumentando a una frecuencia de 3Hz. Cuando llega al valor m�ximo se detiene
;   hasta que se activa la interrupci�n PH0. Se lleva un conteo de cuantas cuentas
;   se llevan que se puede reiniciar con el bot�n PH1. Adem�s los botones PH2 y
;   PH3 controlan el brillo de la pantalla de 7 segmentos. Con el dip switch 7
;   se puede mover entre modos de RUN o de Configuraci�n.
;
;##############################################################################
#include registers.inc

;------------------------------------------------------------------------------
;                               Estructuras de datos
;------------------------------------------------------------------------------
                org $1000

CONT_RTI:       ds 1 ;
CUENTA:         ds 1  ;Contador de tornillos para cada empaque.
AcmPQ:          ds 1  ;Contador de empaques completados
TIMER_CUENTA:   ds 1  ;Conteo de tornillos por medio de interrupcion rti



VMAX:           db 245 ;valor maximo de la variable TIMER_CUENTA
D260us:         db 13  ;260 microsegundos





; MENSAJES
                org $1070
CONFIG_MSG1:    FCC "MODO CONFIG"
                db EOM
CONFIG_MSG2:    FCC "ValorVueltas:"
                db EOM
RUN_MSG1:       FCC "MODO RUN"
                db EOM
RUN_MSG2:       FCC "AcmPQ   CUENTA"
                db EOM
;------------------------------------------------------------------------------
Banderas:       ds 1  ;X:X:CAMBIO_MODO:MODSEL:X:ARRAY_OK:TCL_LEIDA:TCL_LISTA.

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

TICK_MED:       ds 1

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
D240us:         db 12  ;modificar    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
D40us:          db 2  ;40 microsegundos
CLEAR_LCD:      db $01  ;comando para limpiar el LCD
ADD_L1:         db $80  ;direccion inicio de linea 1
ADD_L2:         db $C0  ;direccion inicio de linea 2

DISPONIBLES1:    ds 2
DEISPONIBLES2:         ds 2

                org $1040
Teclas:         db $01,$02,$03,$04,$05,$06,$07,$08,$09,$0B,$00,$0E ;valores de las teclas

                org $1050
SEGMENT:        db $3F,$06,$5B,$4F,$66,$6D,$7D,$07,$7F,$6F ;patrones para el display de 7 segmentos de los digitos de 0 a 9.

                org $1060
iniDsp:         db 4,FUNCTION_SET,FUNCTION_SET,ENTRY_MODE_SET,DISPLAY_ON

; Comandos LCD
FUNCTION_SET:   equ $28
ENTRY_MODE_SET: equ $06
DISPLAY_ON:     equ $0C
CLEAR_DISPLAY:  equ $01
RETURN_HOME:    equ $02
DDRAM_ADDR1:    equ $80
DDRAM_ADDR2:    equ $C0
EOM:            equ $00


; MENSAJES
                org $1070
MSG_LIBRE1:     fcc "RunMeter 623"
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

MSG_ESPERA:     fcc "  ESPERANDO...  "
                db EOM

MSG_CALCULANDO: fcc "  CALCULANDO... "
                db EOM

MSG_ALERTA1:    fcc "** VELOCIDAD **"
                db EOM

MSG_ALERTA2:    fcc "*FUERA DE RANGO*"
                db EOM

MSG_RESUMEN:    fcc "  MODO RESUMEN  "
                db EOM
;------------------------------------------------------------------------------
;                         Vectores de interrupcion:
;------------------------------------------------------------------------------

               org $3E70   ;Interrupcion RTI.
               dw RTI_ISR
               ;org $3E4C   ;Interrupcion key wakeup puerto H.
               ;dw PTH_ISR
               org $3E66   ;Interrupcion OC4.
               dw OC4_ISR



;------------------------------------------------------------------------------
;                          PROGRAMA PRINCIPAL
;------------------------------------------------------------------------------
                org $2000

    ; conf de interrupciones
                bset CRGINT,$80                 ;Habilita RTI
                movb #$17,RTICTL              ;periodo 1.024 ms

                BSET TSCR1,$80                 ;Modulo de timer.
                BSET TSCR2,$03                 ;prescaler es 2^3 = 8
                BSET TIOS,$10                 ;se configura el canal 4 como Output Compare.
                BSET TIE,$10                 ;se habilita interrupcion del canal 4.
                BCLR TCTL1 3                 ;no es necesario que haya una salida en puerto T. Solo se requiere la interrupcion.

                bset PIEH $0C           ;se habilita keywakeup en PH2 y PH3 para brillo, inicialmente entra a modo config
                bclr PPSH $FF           ;interrupcionesen flanco decreciente.
                bclr DDRH $80

                movb #$F0,DDRA          ;IO en puerto A
                bset PUCR,$01           ;resistencia de pull-up puerto A.

                movb #$FF,DDRB                 ;LEDS y pantalla de 7 segmentos
                movb #$0F,DDRP                 ;Habilitador Segmentos
                bset DDRJ,$02                 ;Habilita LEDS
                movb #$FF,DDRK                 ;IO puerto K como salida (LCD)
                movb #$04,DDRE                 ;IO PORTE4 como salida

                ;Configuracion del ATD
                    MOVB #$30,ATD0CTL3
                    MOVB #$B9,ATD0CTL4
                    MOVB #$87,ATD0CTL5

                cli        ;interrupciones mascarables.
;------------------------------------------------------------------------------
;                       Inicializacion de variables
;------------------------------------------------------------------------------
                lds #$3BFF  ;SP

    ;Displays de 7 segmentos y LEDS:
                Clr BCD1
                Clr BCD2
                Clr CONT_7SEG
                Clr CONT_TICKS
                Clr CONT_DIG
                Clr BRILLO

                Movb #$02,LEDS
                Movb SEGMENT,DISP3 ;DISP3 genera un 0
                Movb SEGMENT,DISP4 ;para tener DISP4 produciendo un 0


                    ;config teclado mismo a la tarea anterior. Teclas en FF que indica no presionada
                Clr Cont_Reb
                Clr Cont_TCL
                Clr Patron
                Movb #$FF,Tecla
                Movb #$FF,Tecla_IN
                Movb #$FF,Num_Array


;Programa:
                Clr ValorVueltas               ;limpia variables
                Clr CUENTA
                Clr AcmPQ
                Clr Banderas
                Movb VMAX,TIMER_CUENTA


                Ldd TCNT                 ;Reajustar el output compare
                Addd #60                 ;50kHz
                Std TC4                 ;Valor a alcanzar.
                Bset Banderas,$10         ;MODO_CONFIG activado

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
                tst ValorVueltas
                beq ESTADO_ZERO ;ValorVueltas=0? Ir a CONFIG
                ;Revisamos si modsel==modactual
                Brclr PTIH %10000000 MODE0 ; Si modesel 0

MODE1:
                Brset Banderas %00001000 FIN_COMP;No hubo cambio de modo
                Bset Banderas %00001000 ;Modo actual en 1
                Bra CAMBIO_MODE ;hubo cambio de modo

MODE0:
                Brclr Banderas %00001000 FIN_COMP ;No hubo cambio de modo
                Bclr Banderas %00001000 ; bandera ModActual en 0
                ;Si llega a este punto, hubo cambio


CAMBIO_MODE:
                Bset Banderas,%00010000 ;hubo cambio de modo que se ve en la bandera correspondiente
                ldaa CLEAR_LCD ;cuando se cambia de modo, se limpia la pantalla
                Jsr Send_Command
                Movb D2ms,Cont_Delay ;delay  2ms
                Jsr Delay
FIN_COMP:
                Brset Banderas %00001000 CONFIG_LCD ;Si ModActual es 1 salta a INIT_CONFIG


CONFIG_MODE:
                Brset Banderas,$08,CONFIG_LCD  ;Nos fijamos cual modo esta seleccionado



ESTADO_ZERO:
                bset Banderas,$08

CONFIG_LCD:
                bset CRGINT,$80
                brclr Banderas,$10,CALL_CONFIG  ; Entra SOLO en primera iteraci?n
                bclr Banderas,$10 ; se pone cambio de modo en 0

                bclr PIEH,$03     ;se deshabilitan puertos H 0 y 1
                bclr Banderas,$10

                ldx #CONFIG_MSG1
                ldy #CONFIG_MSG2

                clr CUENTA
                clr AcmPQ

                movb #$00,PORTE
                movb #$02,LEDS ; enciende led pb1

                movb ValorVueltas,BIN1

                ; CONFIGURACION PREVIA AL LCD, en primera iter entra ac?

                jsr CARGAR_LCD



CALL_CONFIG:
                jsr MODO_CONFIG


volver_main:    jmp MAIN
;------------------------------------------------------------------------------
; SUBRUTINAS EN ORDEN
;------------------------------------------------------------------------------
MODO_CONFIG:
                movb #02,LEDS ;enciende led
                ldx #MSG_CONFIG1
                ldy #MSG_CONFIG2
                jsr Cargar_LCD
                
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

ATD_ISR:





;------------------------------------------------------------------------------

PANT_CTRL: rts


;------------------------------------------------------------------------------

CALCULAR:


;------------------------------------------------------------------------------

TCNT_ISR:


;------------------------------------------------------------------------------
CONV_BIN_BCD:
                Ldaa BIN1
                Jsr BIN_BCD ;Pasamos BIN1 a BCD
                Ldaa BCD_L
                Cmpa #10
                Bhs mayor1
                Adda #$B0 ;Si solo tiene un digito, agrega B como "decenas"

mayor1          Staa BCD1 ;Guardamos el valor en BCD1
                Ldaa BIN2
                Jsr BIN_BCD ;Pasamos BIN2 a BCD
                Ldaa BCD_L
                Cmpa #10
                Bhs mayor2
                Adda #$B0 ;Si es de un solo digito, agrega B en decenas

mayor2          Staa BCD2 ;Guardamos en BCD2
                Rts


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
BCD_7SEG:       ldaa ValorVueltas                    ; Cargamos valor en ValorVueltas
                cmpa #0                         ; si es zero se apagan pantallas 3 y 4
                bne NOT_ZERO                    ; 1 y 2 en 0
                movb #$00,DISP1
                movb #$00,DISP2
                movb SEGMENT,DISP3
                movb SEGMENT,DISP4
                bra return_7seg

NOT_ZERO:       ldx #SEGMENT                    ; carga dir segmentos
                ldaa #$0F                       ; mascara parte alta
                anda BCD1
                movb a,x,DISP4                  ; usamos direccionamiento indexado por acumulador usando valor en parte alta
                ldaa #$F0                       ; mascara parte baja
                anda BCD1
                cmpa #$B0                       ; caso de digito vac�o
                beq caso_B                      ; movemos el nibble m�s alto hacia la derecha
                lsra
                lsra
                lsra
                lsra
                movb a,x,DISP3                  ; movemos a pantalla 3
                bra CUENTA_ACMPQ

caso_B:         movb #$00,DISP3                 ; pantalla 3 apagada

CUENTA_ACMPQ:                                   ;Cantidad de paquetes en BCD2
                brset Banderas,$08,BCD2_vacio

                ldaa #$0F
                anda BCD2
                movb a,x,DISP2
                ldaa #$F0
                anda BCD2
                cmpa #$B0
                beq caso_B_disp1
                lsra
                lsra
                lsra
                lsra
                movb a,x,DISP1
                bra return_7seg

caso_B_disp1:                                         ;No hay paquetes completos a�n
                movb #$00,DISP1
                bra return_7seg


BCD2_vacio:     movb #$00,DISP1                       ;apaga 1 y 2
                movb #$00,DISP2

return_7seg:
                rts

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
Delay:
                tst Cont_Delay      ;Espera hasta que OC4 disminuya
                bne Delay
                rts

;------------------------------------------------------------------------------

Send_Command:   psha                    ;se guarda a en pila
                anda #$F0               ;mascara de parte alta
                lsra                    ;deja limpios los dos bits menos significativos
                lsra

                staa PORTK              ;guarda a en portk
                bclr PORTK,$01          ;modif bits menos significativos
                bset PORTK,$02

                movb D240uS,Cont_Delay  ;delay
                jsr Delay

                bclr PORTK,$02
                pula                    ;trae a
                anda #$0F               ;mascara parte baja
                lsla
                lsla

                staa PORTK
                bclr PORTK,$01
                bset PORTK,$02

                movb D240uS,Cont_Delay  ; delay
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
                movb D240us,Cont_Delay ;se inicia el retardo de 260us
                jsr Delay
                bclr PORTK,$02 ;Se deshabilita comunicacion con la LCD
                pula ;se recupera el dato original de la pila
                anda #$0F ;Se deja solo el nibble inferior del dato
                lsla
                lsla ;se alinea nibble con bus datos en PORTK5-PORTK2.
                staa PORTK ;se carga parte baja del dato en el bus de datos.
                bset PORTK,$03 ;Se habilita envio de datos y comunicacion con la LCD
                movb D240us,Cont_Delay ;se inicia el retardo de 260us.
                jsr Delay
                bclr PORTK,$02 ;Se deshabilita comunicacion con la LCD
                rts




;------------------------------------------------------------------------------
;nuevo en proceso
MODO_LIBRE:
                ;REVISAR BANDERAS
                ldx #MSG_LIBRE1
                ldy #MSG_LIBRE2
                jsr Cargar_LCD
                bclr CRGINT,$80                 ; DETIENE ADT
                bclr TIE,$10
                movb #$00,PTJ ;se habilitan los LEDS
                movb #$01,PORTB ;se coloca en puerto B el estado de los LEDS.
		movb #$FF,PTP                  ; deshabilita pantalla 7 segmentos
                rts



;-------------------------------------------------------------------------------
COMPETENCIA:
                ldx #MSG_LIBRE1
                ldy #MSG_ESPERA

		movb #$FF,PTP                  ; deshabilita pantalla 7 segmentos
                movb #$00,PTJ
		movb #$04,LEDS
		tst VELOC
                beq COMPETENCIA_RETURN
                jsr PANT_CTRL
COMPETENCIA_RETURN:
                rts

