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
Banderas:
Teclas:                 db #1, #2, #3, #4, #5, #6, #7, #8, #9, $0B, $00, $0E




                        org $1200
Num_Array:              ds 6


;------------------------------------------------------------------------------
;                            INTERRUPCIONES
;------------------------------------------------------------------------------


                        org $3E70
                        ; dw RTI_ISR  ;subrutina interrupcion RTI
                        org $3E4C
                        ; dw PH0_ISR  ;subrutina interrupcion de puerto H


;------------------------------------------------------------------------------
;                          PROGRAMA
;------------------------------------------------------------------------------

                        org $2000

;Configuracion RTI:
                BSET CRGINT,$80 ;se habilita RTI
                MOVB #$31,RTICTL      ;periodo de 1.024 ms

;Configuracion keywakeup en puerto H:
                BSET PIEH,$01   ;se habilita keywakeup en PH0

;Configuracion del teclado en puerto A:
                MOVB #$F0,DDRA   ;parte alta de A como salida y parte baja como entrada
                BSET PUCR,$01   ;resistencias de pull-up en puerto A. Son necesarias para que haya un 1 en el PAD cuando no se presiona ningun boton del teclado.


		LDS #$3BFF
                CLI

                        

Main:   clra
        ldx #Num_Array

EMPTY:  movb #$FF,1,X+
        inca
        cmpa MAX_TCL
        bne EMPTY
        ;limpiamos memorias de interés
        clr Cont_Reb
        clr Patron
        clr Banderas
        clr Cont_TCL
        ;ponemos FF en memorias para  teclas
        movb #$ff,Tecla
        movb #$ff,Tecla_IN
        
ESPERA: ldd #$04
        cpd Banderas
        beq ESPERA
        jsr TAREA_TECLADO
        bra ESPERA








TAREA_TECLADO:
        RTS




























