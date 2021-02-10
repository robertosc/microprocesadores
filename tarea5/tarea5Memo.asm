;##############################################################################
;                                 Tarea #4
;   Fecha: 05 de Febrero de 2021
;   Autor: Luis guillermo Ramirez y Roberto Sánchez
;
;   Este programa tiene como fin leer el teclado matricual que ccontiene la tarjeta
;   Drago 12+, para ello se genera un flujo iterativo que está reccoriendose
;   en busca de que se presione una tecla. PPara ello se genera una subrutina que
;   identifica si se presionó algo. El dato presionado se almacena en memoria y
;   luego se mueve a un array de datos. También tiene una interrupción de tiempo
;   real y una por botón.
;##############################################################################

#include registers.inc

;------------------------------------------------------------------------------
;                       Declaraciones
;------------------------------------------------------------------------------

                ;Estructuras de datos:
                org $1000
Banderas:       ds 1  ; X:X:X:CambMod:ModActual:ARRAY_OK: TCL_LEIDA:TCL_LISTA
MAX_TCL:        db 2  ; Datos máximos
Tecla:          ds 1  ; Espacio para dato leido
Tecla_IN:       ds 1  ; Guarda el dato para formar el array
Cont_Reb:       ds 1
Cont_TCL:       ds 1  ; Llevar cuenta de número de teclas
Patron:         ds 1  ; Recorrer el teclado
Num_Array:      ds 2  ; array de datos
CUENTA:         ds 1
AcmPQ:          ds 1
CantPQ:         ds 1
TIMER_CUENTA:   ds 1
LEDS:           ds 1
BRILLO:         ds 1
CONT_DIG:       ds 1
CONT_TICKS:     ds 1
DT:             ds 1
BIN1:           ds 1
BIN2:           ds 1
BCD_L:          ds 1
LOW:            ds 1
TEMP:           ds 1
BCD1:           ds 1
BCD2:           ds 1
DISP1:          ds 1
DISP2:          ds 1
DISP3:          ds 1
DISP4:          ds 1
CONT_7SEG:      ds 2
Cont_Delay:     ds 1
; constantes
D2mS:           db 0
D260uS:         db 0
D40uS:          db 0
Clear_LCD:      db $01
ADD_L1:         db $80
ADD_L2:         db $C0
VMAX:           db 245 ;valor maximo de la variable TIMERCUENTA


                org $1030
Teclas:         db $01,$02,$03,$04,$05,$06,$07,$08,$09,$0B,$00,$0E ; Posibles teclas


                org $1040
SEGMENT:        db $3F,$06,$5B,$4F,$66,$6D,$7D,$07,$7F,$6F


                org $1050
iniDsp:         db 0

; MENSAJES
                org $1060


;LCD:
FUNCTION_SET:   equ $28
ENTRY_MODE_SET: equ $06
DISPLAY_ON:     equ $0C
CLEAR_DISPLAY:  equ $01
RETURN_HOME:    equ $02
DDRAM_ADDR1:    equ $80
DDRAM_ADDR2:    equ $C0
EOM:            equ $00

CONFIG_MSG1:    fcc "MODO CONFIG"
                db $00
CONFIG_MSG2:    fcc "Ingrese CantPQ:"
                db $00
RUN_MSG1:       fcc "MODO RUN"
                db $00
RUN_MSG2:       fcc "AcmPQ   CUENTA"
                db $00

; Vectores para interrupciones
                org $3E70
                dw RTI_ISR
                org $3E4C
                dw PH0_ISR

;------------------------------------------------------------------------------
;                       PROGRAMA
;------------------------------------------------------------------------------

                org $2000

                bset CRGINT,$80                 ; Habilita RTI
                bset PIEH,$0C                   ; se habilita keywakeup en puerto H
                bclr PPSH,$FF                   ; flanco decreciente en portH

                bset PUCR,$01                   ; Activa resistencias pull-up en PORTA
                movb #$17,RTICTL                ; periodo de aprox 1ms (1.024ms)
                movb #$F0,DDRA                  ; parte alta de A como salida y parte baja como entrada para matriz

                Lds #$3BFF                      ; stack
                Cli
;Inicializacion de variables y banderas
;------------------------------------------------------------------------------

        ; Se borran las variables de interés
                Clr Cont_Reb
                Clr Cont_TCL
                Clr Patron
                Clr Banderas
                clr CONT_7SEG
                clr CONT_TICKS
                clr CONT_DIG
                clr BCD1
                clr BCD2
                clr CantPQ
                clr CUENTA
                clr AcmPQ

        ; Se llena num array con FFs
                Ldaa MAX_TCL
                Ldx #Num_Array

fill_array:
                Movb #$FF,1,X+
                Dbne a, fill_array


                ;tecla y tecla_in se cargan en FF, valor vacío
                Movb #$FF, Tecla
                Movb #$FF, Tecla_IN
                movb SEGMENT,DISP3
                movb SEGMENT,DISP4 ; PANTALLAS MOSTRANDO 0


;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
                        bset Banderas,$10 ; Se entra en modo config X:X:X:CambMod:ModActual:ARRAY_OK: TCL_LEIDA:TCL_LISTA

MAIN_LOOP:              tst CantPQ
                        beq ESTADO_ZERO
                        ;Revisamos si modsel==modactual

                	Brset Banderas %00001000 MODSEL1 ;se revisa si MODSEL esta en 1
                        ;Caso de MODSEL==0
			Brclr PTIH %10000000 CONFIG_MODE ;MODSEL es 0, se verifica por modo RUN
                	Bclr Banderas %00001000 ;MODSEL es 0, se pone ModActual en Banderas en
                	Bset Banderas %00010000
                	Bra CLEAN_SCREEN
MODSEL1:
               		Brset PTIH %10000000 CONFIG_MODE ;MODSEL es 1, se verifica por modo CONFIG
                        Bset Banderas %00011000 ;Se actualiza modsel y se pone en uno el cambio

                        ;MAE SINCERAMENTE ESTA PARTE NO SE SI ES NECESARIA
                        ;NO VIENE EN EL DIAGRAMA PERO VICTOR LO METIO EN LO DE EL
CLEAN_SCREEN            LDAA CLEAR_LCD ;cuando se cambia de modo, se limpia la pantalla
                	JSR Send_Command ;envio de comando de limpieza de pantalla
                	MOVB D2ms,Cont_Delay ;luego de enviar comando limpiar pantalla se debe esperar 2ms
                	JSR Delay

CONFIG_MODE:            Brset Banderas,$8 CONFIG_LCD
                        

CONFIG_RUN:		brclr Banderas,$10,CALL_RUN

                        bset PIEH,$03     ;se habilitan puertos H 0 y 1
                        bclr Banderas,$10  ;CAMBIO DE MOD EN 0

                        ldx #RUN_MSG1
                        ldy #RUN_MSG2

                        movb #$01,LEDS ; enciende led pb0

                        ldx #CONFIG_MSG1
                        ldy #CONFIG_MSG2


                        ; CONFIGURACION PREVIA AL LCD, en primera iter entra acá

                        jsr CARGAR_LCD

CALL_RUN:               jsr MODO_RUN
                        jmp MAIN_LOOP


ESTADO_ZERO:            bset Banderas,$08

CONFIG_LCD:             bset CRGINT,$80 ;NO ES NECESARIA, CRGINT YA HABILITADAS
                        brclr Banderas,$10,CALL_CONFIG
                        bclr Banderas,$10 ; se pone cambio de modo en 0

                        bclr PIEH,$03     ;se deshabilitan puertos H 0 y 1
                        bclr Banderas,$10

                        clr CUENTA
                        clr AcmPQ

                        ldx #CONFIG_MSG1
                        ldy #CONFIG_MSG2

                        movb #$00,PORTE
                        movb #$02,LEDS ; enciende led pb1

                        ldx #CONFIG_MSG1
                        ldy #CONFIG_MSG2

                        movb CantPQ,BIN1

                        ; CONFIGURACION PREVIA AL LCD, en primera iter entra acá

                        jsr CARGAR_LCD



CALL_CONFIG:            jsr MODO_CONFIG
                        jmp MAIN_LOOP





;-------------------------------------------------------------------------------

MODO_CONFIG:                   movb CantPQ, BIN1
                        brset Banderas,$04,DATA_CHECK
                        jsr TAREA_TECLADO
                        rts


DATA_CHECK:                    jsr BCD_BIN
                               ldaa #25
                               cmpa CantPQ
                               bgt INVALIDO
                               ldaa #85
                               cmpa CantPQ
                               bge VALIDO

INVALIDO:               bclr Banderas,$04
                        bclr CantPQ,$FF
                        rts

VALIDO:                        bclr Banderas, $04
                        movb CantPQ,BIN1
                        rts




ending: bra *
;-------------------------------------------------------------------------------

BCD_BIN:        ldx #Num_Array
                ldab 1,x
                cmpb #$FF
                beq UNIDAD
                stab CantPQ
                bra DECENA

UNIDAD:         movb Num_Array,CantPQ
                rts

DECENA:                clra
                ldab Num_Array
                ldy #10
                emul

                addb CantPQ
                stab CantPQ

                rts
;-------------------------------------------------------------------------------
Cargar_LCD:     ldaa ADD_L1
                jsr Send_Command
                movb D40uS,Cont_Delay
                jsr Delay

LINEA1:         ldaa 1,x+ ;Se va cargando mensaje
                cmpa EOM
                beq CARGAR_LINEA2

                jsr Send_Data

                movb D40uS,Cont_Delay
                jsr Delay
                bra LINEA1


CARGAR_LINEA2:  ldaa ADD_L2
                jsr Send_Command
                movb D40uS,Cont_Delay
                jsr Delay


LINEA2:         ldaa 1,y+
                cmpa EOM
                beq TERMINA_LCD
                jsr Send_Data
                movb D40uS,Cont_Delay
                jsr Delay
                bra LINEA2

TERMINA_LCD:   rts

;-------------------------------------------------------------------------------
Send_Command:


Send_Data:



;-------------------------------------------------------------------------------
Delay:  tst Cont_Delay
        dec Cont_Delay ; TEMPORAL, NO VA ACÁ
        bne Delay
        rts



;-------------------------------------------------------------------------------
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

DELETE: Movb #$FF,Tecla
        Movb #$FF,Tecla_IN
        Bclr Banderas, #3

RETORNAR:
        RTS


;------------------------------------------------------------------------------
MUX_TECLADO:    movb #$EF,Patron                ; Patron inicial
                ldd #$F000                       ; final cuando se desplaza patron

BUSCAR_COLUMNA: movb Patron,PORTA
                brclr PORTA,$08,columna2            ; Verificamos se la tecla está en la columna2
                brclr PORTA,$04,columna1
                brclr PORTA,$02,columna0
                lsl Patron                          ; Se desplaza el patron para verificar siguiente fila
                addb #3                             ; Se suman 3 para aumentar esa cantidad en el array de posibilidades
                cmpa Patron
                bne BUSCAR_COLUMNA
                movb #$FF,Tecla
TERMINAR:       rts

columna2:       incb                                ; Incrementa en 2 si salta acá
columna1:       incb                                ; Incrementa en 1 si salta acá
columna0:       ldx #Teclas
                movb B,X,Tecla                      ; Se mueve la tecla encontrada
                bra TERMINAR

;------------------------------------------------------------------------------
FORMAR_ARRAY:   ldaa Tecla_IN                   ; valor ingresado
                ldab Cont_TCL                   ; cantidad de numeros
                ldx #Num_Array                   ; Posición del array

                cmpb MAX_TCL                    ; comparamos si ya está lleno
                beq ARRAY_LLENO
                cmpb #0                         ; vemos si está vacío
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
                clr Cont_TCL                     ; vacía contador tc
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
MODO_RUN:
                Tst TIMER_CUENTA ;si timer cuenta es cero
                Bne Fin_Run ;si no lo es, se retorna
                ;Caso en que timer cuenta es cero
                Movb VMAX,TIMER_CUENTA ;se recarga con vmax
                Inc CUENTA ;incrementamos cuenta
                Ldaa CUENTA ;
                Cmpa CantPQ ;
                Bne Fin_Run ;si cant!=cuenta
                Inc AcmPQ ;se incrementa AcmPQ
                Bclr CRGINT %10000000 ;se deshabilitamos RTI
                Movb #$04,PORTE ;se activa el relay
                Ldaa #100
                Cmpa AcmPQ ;se ve si AcmPQ ha llegado a 100 por rebase
                Bne Fin_Run ;si no se ha llegado a 100, retorna
                CLR AcmPQ ;si se llega a 100, se hace rebase
Fin_Run:
                MOVB CUENTA,BIN1
                MOVB AcmPQ,BIN2
                RTS

;------------------------------------------------------------------------------
RTI_ISR:        bset CRGFLG,$80                 ; Se reinicia la bandera de interrupcion
                ldx Cont_Reb
                cpx #0
                beq fin_RTI                         ; Si el contador esta en 0 no se debe decrementar
                dec Cont_Reb
fin_RTI:        rti

;------------------------------------------------------------------------------
PH0_ISR:        Bset PIFH,$01                         ; Se reinicia la bandera de interrupcion
                Bclr Banderas,$04
                Clr Cont_TCL
                Ldaa MAX_TCL
                Ldx #Num_Array

vaciado_ph0:       Movb #$FF,1,X+                         ; Iteración para vaciar array
                Dbne a, vaciado_ph0

                Rti