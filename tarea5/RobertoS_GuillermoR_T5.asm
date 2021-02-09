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
CONT_7SEG:      dW 1
Cont_Delay:     ds 1
; constantes
D2mS:           db 0
D260uS:         db 0
D40uS:          db 0
Clear_LCD:      db 0
ADD_L1:         db 0
ADD_L2:         db 0


                org $1030
Teclas:         db $01,$02,$03,$04,$05,$06,$07,$08,$09,$0B,$00,$0E ; Posibles teclas


                org $1040
SEGMENT:        db $3F,$06,$5B,$4F,$66,$6D,$7D,$07,$7F,$6F


                org $1050
iniDsp:         db 0

; MENSAJES
                org $1060

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


                ; CHECK MODSEL







ESTADO_ZERO:            bset Banderas,$08
CONFIG_LCD:             brclr Banderas,$10,CALL_CONFIG
                        bclr Banderas,$10
                
                        ; CONFIGURACION PREVIA AL LCD, en primera iter entra acá
                
                        ; jsr CARGAR_LCD
                


CALL_CONFIG:            jsr MODO_CONFIG
                        bra MAIN_LOOP





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
                
                

                
                
;-------------------------------------------------------------------------------

BCD_BIN:        ldab CantPQ
                clra
                lsrd
                lsrd
                lsrd
                lsrd

                stab BCD_L
                ldab #$0F
                andb BCD_L

                ldy #10
                emul
                std TEMP

                ldab CantPQ
                stab BCD_L

                ldab #$0F
                andb BCD_L
                addd TEMP
                
                movb TEMP, CantPQ
                rts
                

ending:        bra *
;----------------------------------------------------------------------------
Cargar_LCD: bra *



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