;##############################################################################
;                                 Tarea #4
;   Fecha: 05 de Febrero de 2021
;   Autor: Luis guillermo Ramirez y Roberto S�nchez
;
;   Este programa tiene como fin leer el teclado matricual que ccontiene la tarjeta
;   Drago 12+, para ello se genera un flujo iterativo que est� reccoriendose
;   en busca de que se presione una tecla. PPara ello se genera una subrutina que
;   identifica si se presion� algo. El dato presionado se almacena en memoria y
;   luego se mueve a un array de datos. Tambi�n tiene una interrupci�n de tiempo
;   real y una por bot�n.
;##############################################################################

#include registers.inc

;------------------------------------------------------------------------------
;                       Declaraciones
;------------------------------------------------------------------------------

                ;Estructuras de datos:
                org $1000
Banderas:       ds 1  ; X:X:X:CambMod:ModActual:ARRAY_OK: TCL_LEIDA:TCL_LISTA
MAX_TCL:        db 2  ; Datos m�ximos
Tecla:          ds 1  ; Espacio para dato leido
Tecla_IN:       ds 1  ; Guarda el dato para formar el array
Cont_Reb:       ds 1
Cont_TCL:       ds 1  ; Llevar cuenta de n�mero de teclas
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
VMAX:           db 0
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
D2mS:           db 100
D260uS:         db 13
D40uS:          db 2
CLEAR_LCD:      db $01
ADD_L1:         db $80
ADD_L2:         db $C0


                org $1030
Teclas:         db $01,$02,$03,$04,$05,$06,$07,$08,$09,$0B,$00,$0E ; Posibles teclas


                org $1040
SEGMENT:        db $3F,$06,$5B,$4F,$66,$6D,$7D,$07,$7F,$6F


                org $1050
iniDsp:         db 4,FUNCTION_SET,FUNCTION_SET,ENTRY_MODE_SET,DISPLAY_ON

; MENSAJES Y CONSTANTES
                org $1060
                
CONFIG_MSG1:    fcc "MODO CONFIG"
                db $00
CONFIG_MSG2:    fcc "Ingrese CantPQ:"
                db $00
RUN_MSG1:       fcc "MODO RUN"
                db $00
RUN_MSG2:       fcc "AcmPQ   CUENTA"
                db $00
                
;LCD:
FUNCTION_SET:   equ $28
ENTRY_MODE_SET: equ $06
DISPLAY_ON:     equ $0C
CLEAR_DISPLAY:  equ $01
RETURN_HOME:    equ $02
DDRAM_ADDR1:    equ $80
DDRAM_ADDR2:    equ $C0
EOM:            equ $00



; Vectores para interrupciones
                org $3E70
                dw RTI_ISR
                org $3E4C
                dw PH0_ISR
                org $3e66
                dw OC4_ISR

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
                        MOVB #$FF,DDRB ;puerto core B se configura como salida de proposito general. (LEDS y SEGMENTOS)
                            MOVB #$0F,DDRP ;parte baja de puerto P se configura como salida de proposito general. (~Habilitador Segmentos)
                            BSET DDRJ %00000010 ;se configura bit 1 del puerto J como salida de proposito general . (~Habilitador LEDS)

;Configuracion de la salida SAL: relay en puerto PORTE4.
                            MOVB #$04,DDRE ;se configura PORTE4 como salida

;Configuracion de pantalla LCD
                            MOVB #$FF,DDRK ;todos los pines del puerto K se configura como salida para controlar la LCD.


                
                ;Modulo timer
                    bset TSCR1 %10000000 ;se habilita modulo de timer.
                    bset TSCR2 %00000100 ;prescaler es 2^3 = 8
                    bset TIOS %00010000 ;se configura el canal 4 como Output Compare.
                    bset TIE %00010000 ;se habilita interrupcion del canal 4.
                    bclr TCTL1 3 ;no es necesario que haya una salida en puerto T. Solo se requiere la interrupcion.

                Lds #$3BFF                      ; stack
                Cli
;Inicializacion de variables y banderas
;------------------------------------------------------------------------------

        ; Se borran las variables de inter�s
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
        
                        movb #50,BRILLO
                        movb #$02,LEDS   ; conf de leds como salida
                ;tecla y tecla_in se cargan en FF, valor vac�o
                        Movb #$FF, Tecla
                        Movb #$FF, Tecla_IN
                        movb SEGMENT,DISP3
                        movb SEGMENT,DISP4 ; PANTALLAS MOSTRANDO 0

;-------------------------------------------------------------------------------
;INICIALIZACI�N DE PANTALLA LCD y OC4
;-------------------------------------------------------------------------------
                        ldd TCNT
                        addd #30 ; preesc 8
                        std TC4

                        ldx #iniDsp
                        inx
                        clrb
                        
INITIALIZE_LCD:         ldaa b,x
                        jsr Send_Command
                        movb D40us,Cont_Delay
                        jsr Delay
                        incb
                        cmpb iniDsp
                        bne INITIALIZE_LCD
                        ldaa CLEAR_LCD
                        jsr Send_Command
                        movb D2ms,Cont_Delay
                        jsr Delay


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


CONFIG_RUN:                brclr Banderas,$10,CALL_RUN

                        bset PIEH,$03     ;se habilitan puertos H 0 y 1
                        bclr Banderas,$10  ;CAMBIO DE MOD EN 0

                        ldx #RUN_MSG1
                        ldy #RUN_MSG2

                        movb #$01,LEDS ; enciende led pb0

                        ldx #CONFIG_MSG1
                        ldy #CONFIG_MSG2


                        ; CONFIGURACION PREVIA AL LCD, en primera iter entra ac�

                        jsr CARGAR_LCD

CALL_RUN:               jsr MODO_RUN
                        jmp MAIN_LOOP


;-------------------------------------------------------------------------------


ESTADO_ZERO:            bset Banderas,$08

CONFIG_LCD:             bset CRGINT,$80 ;NO ES NECESARIA, CRGINT YA HABILITADAS
                        brclr Banderas,$10,CALL_CONFIG  ; Entra SOLO en primera iteraci�n
                        bclr Banderas,$10 ; se pone cambio de modo en 0
                        
                        bclr PIEH,$03     ;se deshabilitan puertos H 0 y 1
                        bclr Banderas,$10
                        
                        ldx #CONFIG_MSG1
                        ldy #CONFIG_MSG2
                        
                        clr CUENTA
                        clr AcmPQ

                        movb #$00,PORTE
                        movb #$02,LEDS ; enciende led pb1
                        
                        movb CantPQ,BIN1
                
                        ; CONFIGURACION PREVIA AL LCD, en primera iter entra ac�
                
                        jsr CARGAR_LCD
                


CALL_CONFIG:            jsr MODO_CONFIG


volver_main:            jmp MAIN_LOOP



;-------------------------------------------------------------------------------

MODO_CONFIG:            movb CantPQ, BIN1
                        brset Banderas,$04,DATA_CHECK
                        jsr TAREA_TECLADO
                        rts


DATA_CHECK:             jsr BCD_BIN
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
                cmpa #$00
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
                cmpa #$00
                beq TERMINA_LCD
                jsr Send_Data
                movb D40uS,Cont_Delay
                jsr Delay
                bra LINEA2

TERMINA_LCD:           rts

;-------------------------------------------------------------------------------
Send_Command:   psha
                anda #$F0
                lsra
                lsra

                staa PORTK
                bclr PORTK,$01
                bset PORTK,$02

                movb D260uS,Cont_Delay
                jsr Delay

                bclr PORTK,$02
                pula
                anda #$0F
                lsla
                lsla

                staa PORTK
                bclr PORTK,$01
                bset PORTK,$02

                movb D260uS,Cont_Delay
                jsr Delay

                bclr PORTK,$02
                rts
                

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



;-------------------------------------------------------------------------------
Delay:  tst Cont_Delay
        bne Delay
        rts

;-------------------------------------------------------------------------------
OC4_ISR:        tst Cont_Delay
                beq REFRESH
                dec Cont_Delay

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

;-------------------------------------------------------------------------------

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
                brclr PORTA,$08,columna2            ; Verificamos se la tecla est� en la columna2
                brclr PORTA,$04,columna1
                brclr PORTA,$02,columna0
                lsl Patron                          ; Se desplaza el patron para verificar siguiente fila
                addb #3                             ; Se suman 3 para aumentar esa cantidad en el array de posibilidades
                cmpa Patron
                bne BUSCAR_COLUMNA
                movb #$FF,Tecla
TERMINAR:       rts

columna2:       incb                                ; Incrementa en 2 si salta ac�
columna1:       incb                                ; Incrementa en 1 si salta ac�
columna0:       ldx #Teclas
                movb B,X,Tecla                      ; Se mueve la tecla encontrada
                bra TERMINAR

;------------------------------------------------------------------------------
FORMAR_ARRAY:   ldaa Tecla_IN                   ; valor ingresado
                ldab Cont_TCL                   ; cantidad de numeros
                ldx #Num_Array                   ; Posici�n del array

                cmpb MAX_TCL                    ; comparamos si ya est� lleno
                beq ARRAY_LLENO
                cmpb #0                         ; vemos si est� vac�o
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
                clr Cont_TCL                     ; vac�a contador tc
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
;-------------------------------------------------------------------------------
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

vaciado_ph0:       Movb #$FF,1,X+                         ; Iteraci�n para vaciar array
                Dbne a, vaciado_ph0

                Rti
                
                
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