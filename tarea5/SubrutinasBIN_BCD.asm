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


