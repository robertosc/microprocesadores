;                       LUIS GUILLERMO RAMIREZ RODRIGUEZ  B76222
;                       Programa que convierte de BIN a BCD y viceversa

;                       Declaracion de estructuras

                        Org $1000
BIN1: db $9
BIN2: db $7




                        Org $1010
NUM_BCD: ds #2

                        Org $1020
NUM_BIN: ds #2



;Para BIN_BCD
LOW: ds 1
BCD_L: ds 1
BCD1: ds 1
BCD2: ds 1




; Programa Principal

                        Org $2000
;------------------------------------------------------------------
;               Cambios Memo
;------------------------------------------------------------------
CONV_BIN_BCD:
                Ldaa BIN1
                Jsr BIN_BCD
                Ldaa BCD_L
                Cmpa #10
                Bhs mayor1
                Adda #$B0
                
mayor1          Staa BCD1
                ;Esto lo tiene el de victor pero no se si es necesario
                ;BRSET Banderas %00001000 FIN_BIN_BCD ;en modo CONFIG (ModActual=1) no es necesario convertir BIN2
                ;------------------------------------------
		Ldaa BIN2
                Jsr BIN_BCD
                Ldaa BCD_L
                Cmpa #10
                Bhs mayor2
                Adda #$B0
                        
mayor2          Staa BCD2
                Rts

;final   bra *

;------------------------------------------------------------------
;BIN_BCD
;------------------------------------------------------------------
                        
BIN_BCD:

        Ldab #7  ; Contador B=15
        Clr BCD_L

lazo    Lsla
        Rol BCD_L  ;Lo mismo para la variable BCD_L y BCD_H
        Psha

        Ldaa BCD_L ;Cargamos en A el BCD_L
        Anda #$0F  ;Tomamos solo en cuenta los 4LSB
        Cmpa #5   ;Comparamos con 5
        Blo men031  ;Si es menor, salte a men031
        Adda #3  ;En caso de mayor, sume 3

men031  Staa LOW  ;Guardamos temporalmente el resultado anterior

        Ldaa BCD_L
        Anda #$F0 ;En A tenemos cargado del bit 4 al 7
        Cmpa #$50  ;Comparamos con $50
        Blo men301
        Adda #$30   ;Si es mayor, sume 30

men301  Adda LOW   ;Se suman los bits para obtener los 4 LSB de resultado
        Staa BCD_L  ;Se guarda el resultado
        Pula
        Dbne b, lazo

        Lsla
        Rol BCD_L

        Rts









