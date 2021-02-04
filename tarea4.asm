;##############################################################################
;                             Programa Conversiones
;##############################################################################
;       Fecha: 05 de Febrero 2021
;       Autores: Roberto Sánchez Cárdenas y Luis Guillermo Ramírez Rodríguez
;
;
;
;
;
;
;
;
;
;
;
;
;
;------------------------------------------------------------------------------
;                  DECLARACIÓN DE ESTRUCTURAS DE DATOS
;------------------------------------------------------------------------------
#include registers.inc

                        org $1000

MAX_TCL:                db 6
Tecla:                  ds 1
Tecla_IN:               ds 1
Cont_Reb:               ds 1
Cont_TCL:               ds 1
Patron:                 ds 1
Banderas:               ds 1
Teclas:                 db #1, #2, #3, #4, #5, #6, #7, #8, #9, $0B, $00, $0E




                        org $1200
Num_Array:              ds 6




;------------------------------------------------------------------------------
;                          PROGRAMA Principal
;------------------------------------------------------------------------------

                        org $2000
                        
;Configuracion de Hardware
;------------------------------------------------------------------------------
                BSET CRGINT,$80 ;Habilitamos las RTI
                MOVB #$17,RTICTL      ;periodo de 1.024 ms
                BSET PIEH,$01   ;se habilita keywakeup en puerto H
                MOVB #$F0,DDRA   ;parte alta de A como salida y parte baja como entrada para matriz
                BSET PUCR,$01   ;Activamos las resistencias de pull-up en puerto A
                
;Inicializacion de variables y banderas
;------------------------------------------------------------------------------

        ;Ponemos todas las variables en 0
        Clr Cont_Reb
        Clr Cont_TCL
        Clr Patron
        Clr Banderas
        ;tecla y tecla_in se cargan en FF por ser un valor desconocido para el teclado
        Movb #$FF, Tecla
        Movb #$FF, Tecla_IN
        
        Ldaa MAX_TCL
        Ldx Num_Array

        ;Cargamos el vector NUM ARRAY con FF
fill_array:
        Movb #$FF,#1,X+
        Dbne a, fill_array

;Puntero de pila e interrupciones habilitadas
;------------------------------------------------------------------------------

        Lds #$3BFF
        Cli

;Loop de espera
;------------------------------------------------------------------------------
        
ESPERA: Brset Banderas,$04, ESPERA ; Revisa si el bit de Array ok esta en alto y salta si se cumple
        jsr TAREA_TECLADO
        bra ESPERA


;------------------------------------------------------------------------------
;                  Tarea Teclado
;------------------------------------------------------------------------------


TAREA_TECLADO:
        Ldaa Cont_Reb
        Cmpa #0
        Bne RETORNAR
        Jsr MUX_TECLADO
        Ldaa Tecla
        Cmpa #$FF
        Bne PRESIONADA
        Brclr Banderas,$01,RETORNAR ; Si TCL_LISTA es 0, no hay tecla que registrar por lo que se termina la subrutina
        Bclr Banderas,#$03 ; Caso contrario se registra la tecla. Se ponen en 0 TCL_LISTA y TCL_LEIDA para la siguiente tecla
        Jsr FORMAR_ARRAY
        Bra RETORNAR
        



PRESIONADA:
        Brclr Banderas,$02,NotProc
        Ldaa Tecla_IN
        Cmpa Tecla
        Bne Delete
        Bset Banderas,$01 ; La tecla esta lista para registro
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

;*******************************************************************************

MUX_TECLADO:    ldd #$F000
                movb #$EF,Patron


INDENTIF_KEY:   ldx  Patron
                stx PORTA ;Carga patron a puerto A
                brclr PORTA,$08,suma2 ; está en columa 3
                brclr PORTA,$04,suma1 ; está en columa 2
                brclr PORTA,$02,nosuma ; está en columa 1
                lsl Patron
                addb #3
                cmpa Patron
                bne INDENTIF_KEY
                ldy  #$FF
                sty Tecla
                ; movb #$FF,Tecla
                bra retorno


suma2:           incb

suma1:                 incb

nosuma:         ldy Teclas
                movb  b,y,Tecla

retorno:        rts

;*******************************************************************************
                org $2000

FORMAR_ARRAY:   ldaa Tecla_IN           ; valor ingresado
                ldab Cont_TCL           ; cantidad de numeros
                ldx Num_Array           ; Posici{o del array

                cmpb MAX_TCL            ; comparamos si ya está lleno
                beq ARRAY_LLENO
                cmpb #0                 ;vemos si está vacío
                beq PRIMER_VAL
                cmpa #$0B               ;tecla borrar
                beq BORRAR
                cmpa #$0E               ;tecla enter
                beq ENTER
                staa b,x                ;guarda en Num_array + cont_TCL

ARRAY_LLENO:    cmpb #$0B
                bne ARRAY_LLENO_1
                movb #$FF,b,x            ; Para borrar reemplazamos valor actual con ff

                dec Cont_TCL

                bra end_formar

ARRAY_LLENO_1:  cmpb #$0E                ; es enter?
                bne end_formar
                bset Banderas,$04        ; bandera de array ok
                clr Cont_TCL             ; vacía contador tcl

                bra end_formar


PRIMER_VAL:     cmpa #$0B
                beq end_formar         ; terminar

PRIMER_VAL_1:   cmpa #$0E
                beq end_formar
                movb Tecla_IN,b,x
                inc Cont_TCL
                bra end_formar

ENTER:          bset Banderas,#$04    ; bandera de array_ok
                bclr Cont_TCL,#$FF    ; pone contador en 0
                bra end_formar


BORRAR:        	dec Cont_TCL
                decb
                movb #$FF,b,x

end_formar:     movb #$FF,Tecla_IN
                rts























