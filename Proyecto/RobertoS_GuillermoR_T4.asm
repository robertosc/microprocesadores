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
MAX_TCL:        db 5  ; Datos máximos
Tecla:          ds 1  ; Espacio para dato leido
Tecla_IN:       ds 1  ; Guarda el dato para formar el array
Cont_Reb:       ds 1
Cont_TCL:       ds 1  ; Llevar cuenta de número de teclas
Patron:         ds 1  ; Recorrer el teclado
Banderas:       ds 1  ; Se guardan banderas indicadas en los 3 bist menos significativos
Num_Array:      ds 6  ; array de datos
Teclas:         db $01,$02,$03,$04,$05,$06,$07,$08,$09,$0B,$00,$0E ; Posibles teclas

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
                bset PIEH,$01                   ; se habilita keywakeup en puerto H
                bset PUCR,$01                   ; Activa resistencias pull-up en PORTA
		movb #$17,RTICTL                      ; periodo de aprox 1ms (1.024ms)
                movb #$F0,DDRA                   ; parte alta de A como salida y parte baja como entrada para matriz


;Inicializacion de variables y banderas
;------------------------------------------------------------------------------

        ; Se borran las variables de interés
        Clr Cont_Reb
        Clr Cont_TCL
        Clr Patron
        Clr Banderas
        ;tecla y tecla_in se cargan en FF, valor vacío
        Movb #$FF, Tecla
        Movb #$FF, Tecla_IN

        Ldaa MAX_TCL
        Ldx #Num_Array

        ; Se llena num array con FFs
fill_array:
        Movb #$FF,1,X+
        Dbne a, fill_array

;------------------------------------------------------------------------------

        Lds #$3BFF
        Cli

;------------------------------------------------------------------------------

ESPERA: Brset Banderas,$04, ESPERA                 ; Revisa si el bit de Array ok esta en alto y salta si se cumple
        jsr TAREA_TECLADO
        bra ESPERA                                 ; continua el loop


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

ARRAY_LLENO:    cmpb #$0B
                bne ARRAY_LLENO_1
                decb
                movb #$FF,b,x                    ; Para borrar reemplazamos valor actual con ff
                dec Cont_TCL
                bra end_formar

ARRAY_LLENO_1:  cmpb #$0E                         ; es enter?
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