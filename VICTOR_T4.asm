;##############################################################################
;                                 Tarea #4
;   Fecha: 16 de octubre del 2020.
;   Autor: Victor Yeom Song
;
;   Descripcion: En esta tarea se implementa un programa que permite leer el
;       teclado matricial de la Dragon 12 en el puerto A, y tomar estas lecturas
;       para formar un arreglo que representa la secuencia de teclas presionadas
;       como un numero en BCD, almacenado en un arreglo donde cada posicion es
;       un digito del numero.
;                         -------------
;                         | 1 | 2 | 3 |
;                         | 4 | 5 | 6 |
;                         | 7 | 8 | 9 |
;                         | B | 0 | E |
;                         -------------
;       El programa implementa supresion de rebotes y contempla la situacion de
;       tecla retenida. Una interrupcion keywakeup en el puerto PH0 permite
;       reiniciar el programa.
;##############################################################################
#include registers.inc

;------------------------------------------------------------------------------
;     Declaracion de las estructuras de datos y vectores de interrupcion
;------------------------------------------------------------------------------
;Estructuras de datos:
                ORG $1000
MAX_TCL:        DB 5  ;Cantidad maxima de teclas que se leen
Tecla:          DS 1  ;Almacena el valor leido del teclado en la subrutina MUX_TECLADO.
Tecla_IN:       DS 1  ;Almacena temporalmente el valor de Tecla antes de la supresion de rebotes.
Cont_Reb:       DS 1  ;Contador de ticks del RTI, usado para suprimir rebotes.
Cont_TCL:       DS 1  ;Indice utilizado para escribir en el arreglo que guarda las teclas presionadas.
Patron:         DS 1  ;Indice para recorrer el puerto A y detectar la tecla presionada
Banderas:       DS 1  ;Tiene el formato: X:X:X:X:X:ARRAY_OK:TCL_LEIDA:TCL_LISTA.
                      ;ARRAY_OK indica que se presiono la tecla Enter y que en el arreglo ya se tienen todos los valores leidos.
                      ;TCL_LEIDA indica que ya se habia tenido una lectura del teclado y que se estaba esperando a que se diera la supresion de rebotes.
                      ;TCL_LISTA indica que luego de la supresion de rebotes se confirmo que si se presiono una tecla.
Num_Array:      DS 6  ;Arreglo donde se almacenan todas las teclas presionadas. Es de tamaño maximo 5 pero en la declaracion del enunciado se indica que cubre los espacios $1007 a $100C
Teclas:         DB $01,$02,$03,$04,$05,$06,$07,$08,$09,$0B,$00,$0E

;Vectores de interrupcion:
                ORG $3E70   ;direccion del vector de interrupcion RTI.
                DW RTI_ISR  ;direccion de la subrutina de servicio a interrupcion RTI.
                ORG $3E4C   ;direccion del vector de interrupcion por key wakeup del puerto H.
                DW PH0_ISR  ;direccion de la subrutina de servicio a interrupcion del puerto H.
;------------------------------------------------------------------------------

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
        Ldx #Num_Array

        ;Cargamos el vector NUM ARRAY con FF
fill_array:
        Movb #$FF,1,X+
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
; Subrutina TAREA_TECLADO: En esta subrutina se da la lectura del teclado. Aqui
;     se lee el teclado en el puerto A, se suprimen los rebotes, y se maneja la
;     situacion de tecla retenida.
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
                
;------------------------------------------------------------------------------
; Subrutina MUX_TECLADO: esta subrutina es la que se encarga de leer el teclado
;     en el puerto A. Como se habilitaron las resistencias de pull up, la parte
;     baja del puerto A (la entrada) siempre tendra valor %1111. Al colocar un
;     unico cero en alguno de los bits de la parte alta, si se presiona un boton
;     entonces se tendra un cero tambien en algun bit de la parte baja. Ambos
;     ceros en parte alta y baja definen la fila y columna de la tecla presionada.
;     Con esta informacion se calcula la posicion en el arreglo de teclas en el
;     que se encuentra el valor de la tecla presionada. El valor de la tecla se
;     devuelve en la variable Tecla. Si no se presiona nada entonces Tecla = $FF.
;------------------------------------------------------------------------------
MUX_TECLADO:    MOVB #$EF,Patron ; Se inicializa el patron de busqueda
                LDAA #$F0 ; Para comparacion para final de iteraciones
                CLRB ; Para acceso indexado por acumulador por
loop_patron:    MOVB Patron,PORTA ; Se carga el patron en el puerto A
                BRCLR PORTA,$08,col2 ; Se busca el patron en la ultima columna  verifica la ultima columna
                BRCLR PORTA,$04,col1 ; Se busca el patron en la columna media
                BRCLR PORTA,$02,col0 ; Se busca el patron en la primera columna
                LSL Patron ; Si no hubo coincidencias con el patron, se desplaza a la izquierda y se suman 3 como offset de busqueda de la columna (para buscar en TeclasTECLAS) el  no se
                ADDB #3
                CMPA Patron ; Se ve si ya se buscaron todos los posibles valores
                BNE loop_patron ; Si no se han recorrido todas las filas, se itera sobre la siguientehecho la busqueda en todo Teclas, se itera otra vez
                MOVB #$FF,Tecla ; Caso contrario la tecla registrada no era valida y se borra
fin_MUX:        RTS

col2:           INCB ; Incremento del indice de busqueda segun la columna y registro de la tecla hallada en Teclas a Tecla
col1:           INCB
col0:           LDX #Teclas
                MOVB B,X,Tecla
                BRA fin_MUX

;------------------------------------------------------------------------------
; Subrutina FORMAR_ARRAY: recibe en la variable Tecla_IN el valor de la tecla
;     presionada que se quiere almacenar en array. Este arreglo representa un
;     numero en formato BCD. Se cuida que el arreglo no supere el tamano maximo.
;------------------------------------------------------------------------------
FORMAR_ARRAY:   ldaa Tecla_IN           ; valor ingresado
                ldab Cont_TCL           ; cantidad de numeros
                ldx #Num_Array           ; Posici{o del array

                cmpb MAX_TCL            ; comparamos si ya está lleno
                beq ARRAY_LLENO
                cmpb #0                 ;vemos si está vacío
                beq PRIMER_VAL
                cmpa #$0B               ;tecla borrar
                beq BORRAR
                cmpa #$0E               ;tecla enter
                beq ENTER
                staa b,x                ;guarda en Num_array + cont_TCL
                inc Cont_TCL
                bra end_formar

ARRAY_LLENO:    cmpb #$0B
                bne ARRAY_LLENO_1
                decb
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


BORRAR:         dec Cont_TCL
                decb
                movb #$FF,b,x


end_formar:     movb #$FF,Tecla_IN
                rts
                
;------------------------------------------------------------------------------
; Subrutina de servicio a interrupcion RTI: sencillamente descuenta un contador
;     siempre y cuando el contador no sea cero. Los ticks del RTI duran 1.024 ms,
;     por lo que esta interrupcion permite contar Cont_Reb milisegundos. El uso
;     que se le da en este programa es suprimir los rebotes de boton con una
;     cuenta de 10 ticks (~10 ms).
;------------------------------------------------------------------------------
RTI_ISR:        BSET CRGFLG,$80 ; Se reinicia la bandera de interrupcion
                TST Cont_Reb
                BEQ fin_RTI ; Si el contador esta en 0 no se debe decrementar
                DEC Cont_Reb
fin_RTI:        RTI

;------------------------------------------------------------------------------
; Subrutina de servicio a interrupcion key wakeup del puerto PH0: esta subrutina
;     se encarga de limpiar el arrglo Num_Array y la bandera ARRAY_OK.
;------------------------------------------------------------------------------
PH0_ISR:	Bset PIFH,$01 ; Se reinicia la bandera de interrupcion
                Bclr Banderas,$04
                Clr CONT_TCL
                
                Ldaa MAX_TCL
        	Ldx #Num_Array

        	;Cargamos el vector NUM ARRAY con FF
fill_ph0:
        	Movb #$FF,1,X+
       		Dbne a, fill_ph0
       		
                Rti
                
                
                
                
