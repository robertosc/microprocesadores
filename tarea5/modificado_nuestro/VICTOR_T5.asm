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
Banderas:       ds 1  ;X:X:CAMBIO_MODO:MODSEL:X:ARRAY_OK:TCL_LEIDA:TCL_LISTA.

MAX_TCL:        db 2  ;Cantidad de teclas
Tecla:          ds 1  ;Valor leido en tecladp
Tecla_IN:       ds 1  ;en esta variable se almacena temporalmente el valor de Tecla antes de la supresion de rebotes. Si despues de la supresion se da que Tecla y Tecla_IN son iguales es porque efectivamente se presiono una tecla que debe ser guardada.
Cont_Reb:       ds 1  ;es el contador de ticks del RTI, usado para suprimir rebotes.
Cont_TCL:       ds 1  ;es el indice utilizado para escribir en el arreglo que guarda las teclas presionadas.
Patron:         ds 1  ;es el indice que lleva las iteraciones en subrutina MUX_TECLADO.
Num_Array:      ds 2  ;en este arreglo se almacenan todas las teclas presionadas por el usuario.
CUENTA:         ds 1  ;cantidad de tornillos contados para cada empaque.
AcmPQ:          ds 1  ;cantidad de empaques completados. Entre 0 y 99 y puede rebasar.
CantPQ:         ds 1  ;cantidad maxima de tornillos por cada empaque. Entre 12 y 96. Definido por usuario en MODO CONFIG.
TIMER_CUENTA:   ds 1  ;da la cedencia de incremento de CUENTA. Simula el paso de tornillos. Decrementada por RTI_ISR.
LEDS:           ds 1  ;guarda el estado de los LEDS. LED PB1 corresponde a modo CONFIG, LED PB0 a modo RUN.
BRILLO:         ds 1  ;Variable controlada por PTH3/PTH2 para incrementar/decrementar el brillo de la pantalla LCD.
CONT_DIG:       ds 1  ;cuenta cual digito de 7 segmentos se debe habilitar. Cambia cada vez que CONT_TICKS alcanza 100.
CONT_TICKS:     ds 1  ;contador de ticks de Output Compare para multiplexar.

DT:             ds 1  ;ciclo de trabajo. DT = N-K.
BIN1:           ds 1  ;variable de entrada a subrutina CONV_BIN_BCD. Utilizada para CantPQ y CUENTA.
BIN2:           ds 1  ;variable de entrada a subrutina CONV_BIN_BCD. Utilizada para AcmPQ.
BCD_L:          ds 1  ;variable donde se guarda la salida de BIN_BCD. Utilizada para CONV_BIN_BCD.
LOW:            ds 1  ;variable requerida para el algoritmo de la subrutina BIN_BCD.
VMAX:           db 245 ;valor maximo de la variable TIMER_CUENTA. Usado para volver a iniciar la cuenta regresiva.
BCD1:           ds 1  ;variable de salida de subrutina BIN_BCD. Tambien es entrada para BCD_7SEG. Utilizada para CantPQ y CUENTA.
BCD2:           ds 1  ;variable de salida de subrutina BIN_BCD. Tambien es entrada para BCD_7SEG. Utilizada para AcmPQ.
DISP1:          ds 1  ;corresponde al valor que se escribe en el display de 7 segmentos.
DISP2:          ds 1  ;BCD2 utiliza DISP1 y DISP2 para desplegarse
DISP3:          ds 1  ;corresponde al valor que se escribe en el display de 7 segmentos.
DISP4:          ds 1  ;BCD1 utiliza DISP3 y DISP4 para desplegarse

CONT_7SEG:      ds 2  ;contador de ticks de OC4 para lograr refrescamiento de LEDS y Displays a 10Hz.
Cont_Delay:     ds 1  ;esta variable se carga con alguna de las siguientes tres constantes para generar retrasos temporales.
D2ms:           db 100  ;100 ticks a 50kHz son 2 milisegundos
D260us:         db 13  ;13 ticks a 50kHz son 260 microsegundos
D40us:          db 2  ;2 ticks a 50kHz son 40 microsegundos
CLEAR_LCD:      db $01  ;comando para limpiar el LCD
ADD_L1:         db $80  ;direccion inicio de linea 1
ADD_L2:         db $C0  ;direccion inicio de linea 2

                org $1030
Teclas:         db $01,$02,$03,$04,$05,$06,$07,$08,$09,$0B,$00,$0E ;valores de las teclas

                org $1040
SEGMENT:        db $3F,$06,$5B,$4F,$66,$6D,$7D,$07,$7F,$6F ;patrones para el display de 7 segmentos de los digitos de 0 a 9.

                org $1050
iniDsp:         db 4,FUNCTION_SET,FUNCTION_SET,ENTRY_MODE_SET,DISPLAY_ON

                org $1060

; Comandos LCD
FUNCTION_SET:   equ $28
ENTRY_MODE_SET: equ $06
DISPLAY_ON:     equ $0C
CLEAR_DISPLAY:  equ $01
RETURN_HOME:    equ $02
DDRAM_ADDR1:    equ $80
DDRAM_ADDR2:    equ $C0
EOM:            equ $00


; mensajes a mostrar
                org $1070
CONFIG_MSG1:    FCC "MODO CONFIG"
                db EOM
CONFIG_MSG2:    FCC "Ingrese CantPQ:"
                db EOM
RUN_MSG1:       FCC "MODO RUN"
                db EOM
RUN_MSG2:       FCC "AcmPQ   CUENTA"
                db EOM
;------------------------------------------------------------------------------
;                         Vectores de interrupcion:
;------------------------------------------------------------------------------

               org $3E70   ;Interrupcion RTI.
               dw RTI_ISR
               org $3E4C   ;Interrupcion key wakeup puerto H.
               dw PTH_ISR
               org $3E66   ;Interrupcion OC4.
               dw OC4_ISR



;------------------------------------------------------------------------------
;                          PROGRAMA PRINCIPAL
;------------------------------------------------------------------------------
    org $2000

    bset CRGINT %10000000         ;Habilita RTI
    movb #$17,RTICTL              ;periodo 1.024 ms

    bset PIEH %00001100           ;se habilita keywakeup en PH2 y PH3 para brillo, inicialmente entra a modo config
    bclr PPSH $FF                   ;interrupcionesen flanco decreciente.
    bclr DDRH %10000000

    movb #$F0,DDRA                ;IO en puerto A
    bset PUCR,$01                 ;resistencia de pull-up puerto A.

    movb #$FF,DDRB ;puerto core B se configura como salida de proposito general. (LEDS y SEGMENTOS)
    movb #$0F,DDRP ;parte baja de puerto P se configura como salida de proposito general. (~Habilitador Segmentos)
    bset DDRJ %00000010 ;se configura bit 1 del puerto J como salida de proposito general . (~Habilitador LEDS)
    movb #$FF,DDRK ;IO puerto K como salida (LCD)
    movb #$04,DDRE ;IO PORTE4 como salida

    bset TSCR1 %10000000 ;Habilita timer.
    bset TSCR2 %00000011 ;Preescalador de 8
    bset TIOS %00010000 ;PT4 como OC
    bset TIE %00010000 ;interrupcion del canal 4.
    bclr TCTL1 3


    cli        ;interrupciones mascarables.
;------------------------------------------------------------------------------
;                       Inicializacion de variables
;------------------------------------------------------------------------------
    lds #$3BFF  ;Stack

    MOVB #$FF,Tecla            ;config teclado
    MOVB #$FF,Tecla_IN
    MOVB #$FF,Num_Array
    CLR Cont_Reb
    CLR Cont_TCL
    CLR Patron

;Displays de 7 segmentos y LEDS:
    CLR CONT_7SEG
    CLR CONT_TICKS
    CLR CONT_DIG
    MOVB #0,BRILLO
    MOVB #$02,LEDS
    CLR BCD1
    CLR BCD2
    MOVB SEGMENT,DISP3 ;para tener DISP3 produciendo un 0
    MOVB SEGMENT,DISP4 ;para tener DISP4 produciendo un 0. Importa mas que nada si se desea que en DISP3 y DISP4 presenten el ultimo valor valido introducido de CantPQ, con OC4

;Programa:
    CLR CantPQ
    CLR CUENTA
    MOVB VMAX,TIMER_CUENTA
    CLR AcmPQ
    CLR Banderas
    BSET Banderas,%00010000 ;CambMod=1 (MODO_CONFIG)

    LDD TCNT ;se carga el valor actual de TCNT para reajustar el output compare
    ADDD #60 ;60 cuentas equivalen 50kHz con prescalador=8
    STD TC4 ;se actualiza el nuevo valor a alcanzar.
    
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
    
    
;------------------------------------------------------------------------------
MAIN:
                            tst CantPQ
                            beq ESTADO_ZERO ;CantPQ=0? Ir a CONFIG
                            ;Revisamos si modsel==modactual
                        Brclr PTIH %10000000 MODE0 ; Si modesel 0

MODE1:                        Brset Banderas %00001000 FIN_COMP;No hubo cambio de modo
                        Bset Banderas %00001000 ;Modo actual en 1
                        Bra CAMBIO_MODE ;hubo cambio de modo

MODE0:                  Brclr Banderas %00001000 FIN_COMP ;No hubo cambio de modo
                        Bclr Banderas %00001000 ; bandera ModActual en 0
                ;        Si llega a este punto, hubo cambio


CAMBIO_MODE:
                        Bset Banderas,%00010000 ;hubo cambio de modo que se ve en la bandera correspondiente
                        ldaa CLEAR_LCD ;cuando se cambia de modo, se limpia la pantalla
                        Jsr Send_Command
                               Movb D2ms,Cont_Delay ;delay  2ms
                               Jsr Delay
FIN_COMP:                        Brset Banderas %00001000 CONFIG_LCD ;Si ModActual es 1 salta a INIT_CONFIG


CONFIG_MODE:                    Brset Banderas,$08,CONFIG_LCD


CONFIG_RUN:                     brclr Banderas,$10,CALL_RUN

                                bset PIEH,$03     ;se habilitan puertos H 0 y 1
                                bclr Banderas,$10  ;CAMBIO DE MOD EN 0
                                ldx #RUN_MSG1
                                ldy #RUN_MSG2
                                movb #$01,LEDS ; enciende led pb0
                                ldaa #1
                        ; CONFIGURACION PREVIA AL LCD, en primera iter entra ac?

                        jsr CARGAR_LCD

CALL_RUN:               jsr MODO_RUN
                        jmp MAIN


ESTADO_ZERO:            bset Banderas,$08

CONFIG_LCD:             bset CRGINT,$80 ;NO ES NECESARIA, CRGINT YA HABILITADAS
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

                        movb CantPQ,BIN1

                        ; CONFIGURACION PREVIA AL LCD, en primera iter entra ac?

                        jsr CARGAR_LCD



CALL_CONFIG:
                        jsr MODO_CONFIG


volver_main:            jmp MAIN

;------------------------------------------------------------------------------
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

;------------------------------------------------------------------------------
Send_Data:
                psha ;el dato se recibe en acumulador A y se protege para poder analizar sus nibbles por separado
                anda #$F0 ;Se deja solo el nibble superior del dato
                lsra
                lsra ;se alinea nibble con bus datos en PORTK5-PORTK2.
                staa PORTK ;se carga parte alta del dato en el bus de datos.
                bset PORTK,$03 ;Se habilita el envio de dato y comunicacion con la LCD
                movb D260us,Cont_Delay ;se inicia el retardo de 260us
                jsr Delay
                bclr PORTK,$02 ;Se deshabilita comunicacion con la LCD
                pula ;se recupera el dato original de la pila
                anda #$0F ;Se deja solo el nibble inferior del dato
                lsla
                lsla ;se alinea nibble con bus datos en PORTK5-PORTK2.
                staa PORTK ;se carga parte baja del dato en el bus de datos.
                bset PORTK,$03 ;Se habilita envio de datos y comunicacion con la LCD
                movb D260us,Cont_Delay ;se inicia el retardo de 260us.
                jsr Delay
                bclr PORTK,$02 ;Se deshabilita comunicacion con la LCD
                rts

;------------------------------------------------------------------------------
Delay:
                tst Cont_Delay      ;Espera hasta que OC4 disminuya
                bne Delay
                rts


;------------------------------------------------------------------------------
BCD_BIN:        ldx #Num_Array
                ldab 1,x           revisamos si la unidad es distinta de FF
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


;------------------------------------------------------------------------------
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


;------------------------------------------------------------------------------
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

VALIDO:
                        bclr Banderas, $04
                        movb CantPQ,BIN1
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

TERMINA_LCD:    rts


;------------------------------------------------------------------------------
BCD_7SEG:       ldaa CantPQ
                cmpa #0
                bne NOT_ZERO
                movb #$00,DISP1
                movb #$00,DISP2
                movb SEGMENT,DISP3
                movb SEGMENT,DISP4
                bra return_7seg
                
NOT_ZERO:       ldx #SEGMENT
                ldaa #$0F
                anda BCD1
                movb a,x,DISP4
                ldaa #$F0
                anda BCD1
                cmpa #$B0
                beq caso_B
                lsra
                lsra
                lsra
                lsra
                movb a,x,DISP3
                bra CUENTA_ACMPQ

caso_B:         movb #$00,DISP3

CUENTA_ACMPQ:   brset Banderas %00001000 BCD2_vacio

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
                
caso_B_disp1:
                movb #$00,DISP1
                bra return_7seg


BCD2_vacio:     movb #$00,DISP1
                movb #$00,DISP2

return_7seg:         rts





;------------------------------------------------------------------------------
RTI_ISR:        bset CRGFLG %10000000 ;limpiamos la bandera de interrupcion RTI
                tst TIMER_CUENTA
                beq REBOTES  ;Si TIMER CUENTA es cero, pasamos a revisar los rebotes
                dec TIMER_CUENTA ;Solo se decrementa si TIMER CUENTA no es cero

REBOTES:	tst Cont_Reb
                beq FIN_RTI;si llegaron los rebotes a 0, se termina la rubrutina
                dec Cont_Reb ;Decrementamos el contador de rebotes si aun no ha llegado a cero

FIN_RTI:
                rti



;------------------------------------------------------------------------------
PTH_ISR:        Brset Banderas,$08,PH_CONFIG ;Si esa en modo config no toma en cuenta PH0 Y PH1
                Brclr PIFH $01 PTH1 ;si es 1, se revisa por la siguiente fuente de interrupcion
                ;CASO DE PH0
		Bset PIFH $01 ;apagamos las interrupciones de PH0
		Clr CUENTA ;borramos cuenta
                Bset CRGINT %10000000 ;Habilita interrupciones de RTI
                Clr PORTE ;apagamos el relay

PTH1:
                BRCLR PIFH $02 PH_CONFIG ;Si no tiene PH1, revisa PH2
                ;Caso de PH1
		BSET PIFH $02 ;se apaga la bandera de la fuente de interrupcion
                CLR AcmPQ ;se limpia el acumulador
PH_CONFIG:
                Brclr PIFH $04 PTH3 ;No hay PH3, revisa ph4
                Bset PIFH $04 ;se apaga la bandera de la fuente de interrupcion
                Tst BRILLO
                Beq PTH3 ;Caso de brillo minimo, ya no se puede restar
                Ldaa BRILLO
                Suba #5
                Staa BRILLO ;se le resta 5 a BRILLO
PTH3:
                Brclr PIFH $08 FIN_PTH ;no hay ph4, termina de revisar
                Bset PIFH $08 ;se apaga la fuente de interrupcion
                Ldaa #100
                Cmpa BRILLO
                Beq FIN_PTH ;Maximo brillo posible, termina la subrutina
                Ldaa BRILLO
                Adda #5
                Staa BRILLO ;se le suma 5 a BRILLO
FIN_PTH:
                Rti ;termina la subrutina


;------------------------------------------------------------------------------


OC4_ISR:
                ldaa Cont_Delay
                cmpa #0
                beq CONTROL_PANTALLA
                dec Cont_Delay
                
CONTROL_PANTALLA:
                ldx CONT_7SEG
                       dex
                      stx CONT_7SEG
                       cpx #0
                       bne CONTADOR_DISP
                       movw #5000,CONT_7SEG
                       jsr CONV_BIN_BCD
                       jsr BCD_7SEG
                       
CONTADOR_DISP:
                ldaa CONT_TICKS
                cmpa #100
                bne MUX
                clra
                staa CONT_TICKS
                inc CONT_DIG
                ldab CONT_DIG
                cmpb #5
                bne MUX
                clr CONT_DIG
MUX:
                tst CONT_TICKS
                bne DUTY_CYCLE

                movb #$02,PTJ
                movb #$FF,PTP

                ldab CONT_DIG
                cmpb #0
                beq DIG0
                cmpb #1
                beq DIG1
                cmpb #2
                beq DIG2
                cmpb #3
                beq DIG3

                movb #$00,PTJ
                movb LEDS,PORTB
                bra DUTY_CYCLE
DIG0:
                movb #$F7,PTP ;se habilita unicamente el display 4
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
                ldaa #100
                suba BRILLO
                staa DT

                    ldaa CONT_TICKS
                cmpa DT
                bne FIN_OC4
                movb #$FF,PTP ;se deshabilitan displays de 7 segmentos
                movb #$02,PTJ ;se deshabilitan los LEDS
FIN_OC4:
                inc CONT_TICKS
                bset TFLG1 %00010000 ;se reinicia la bandera de interrupcion
                ldd TCNT ;se carga el valor actual de TCNT para reajustar el output compare
                addd #60 ;60 cuentas equivalen 50kHz con prescalador=8
                std TC4 ;se actualiza el nuevo valor a alcanzar.
                rti