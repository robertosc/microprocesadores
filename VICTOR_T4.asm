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
;                          Configuracion del hardware
;------------------------------------------------------------------------------
                ORG $2000
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

;------------------------------------------------------------------------------


;*******************************************************************************
;                             Programa principal
;*******************************************************************************
;inicializacion de variables:
                LDX #Num_Array
                CLRA
init_fill:      MOVB #$FF,A,X
                INCA
                CMPA MAX_TCL
                BNE init_fill
                MOVB #$FF,Tecla
                MOVB #$FF,Tecla_IN
                CLR Cont_Reb
                CLR Cont_TCL
                CLR Patron
                CLR Banderas
wait:           BRSET Banderas,$04,wait ; Si ARRAY_OK=1 no se espera lectura
                JSR TAREA_TECLADO
                BRA wait

;------------------------------------------------------------------------------
; Subrutina TAREA_TECLADO: En esta subrutina se da la lectura del teclado. Aqui
;     se lee el teclado en el puerto A, se suprimen los rebotes, y se maneja la
;     situacion de tecla retenida.
;------------------------------------------------------------------------------
TAREA_TECLADO:  TST Cont_Reb
                BNE fin_TAREA ; Se ve si el contador ha llegado a 0, para asegurar la supresion de rebotes
                JSR MUX_TECLADO ; Si el contador llega a 0, se corrobora la presion de una tecla y se salta a MUX_TECLADO
                LDAA Tecla ; Luego de MUX_TECLADO ya la tecla leida esta en la variable Tecla
                CMPA #$FF ; Se ve si se presiono una tecla valida
                BEQ checkTCL_LISTA ; Puede haber una tecla lista para guardar en el arreglo. Esto es para evitar el caso de tecla retenida
                BRCLR Banderas,$02,setReb ; Si ARRAY_OK es 0, hay que preparar el contador para rebotes
                CMPA Tecla_IN ; Si no, ya esta listo y se verifica que la tecla recibida (despues de la supresion de rebotes) es valida
                BEQ setTCL_LISTA ; Si Tecla_IN y Tecla son iguales, se registra la tecla presionada
                MOVB #$FF,Tecla ; Caso contrario se borra la tecla y TCL_LISTA y TCL_LEIDA se ponen en 0
                MOVB #$FF,Tecla_IN
                BCLR Banderas,$03
fin_TAREA:      RTS

checkTCL_LISTA: BRCLR Banderas,$01,fin_TAREA ; Si TCL_LISTA es 0, no hay tecla que registrar por lo que se termina la subrutina
                BCLR Banderas,$03 ; Caso contrario se registra la tecla. Se ponen en 0 TCL_LISTA y TCL_LEIDA para la siguiente tecla
                JSR FORMAR_ARRAY
                BRA fin_TAREA

setReb:         MOVB Tecla,Tecla_IN ; Almacenamiento temporal de la tecla detectada en Tecla_IN para revision de rebotes
                BSET Banderas,$02 ; TCL_LEIDA se levanta setea
                MOVB #10,Cont_Reb ; Se inicia el contador para rebotes
                BRA fin_TAREA
setTCL_LISTA:   BSET Banderas,$01 ; La tecla esta lista para registro
                BRA fin_TAREA
                
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
                BRCLR PORTA,$04,col2 ; Se busca el patron en la ultima columna  verifica la ultima columna
                BRCLR PORTA,$02,col1 ; Se busca el patron en la columna media
                BRCLR PORTA,$01,col0 ; Se busca el patron en la primera columna
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
FORMAR_ARRAY:   LDX #Num_Array ; Se guarda la posicion de Num_Array para empezar a guardar datos
                LDAA Tecla_IN ; Se guarda la tecla registrada
                LDAB Cont_TCL ; Se inicia el iterador para iterar sobre Num_Array
                CMPB MAX_TCL ; Se ve si Num_array esta lleno
                BEQ max_teclas ; Si esta lleno, se cubren los casos de Num_Array lleno
                CMPB #$00 ; Se ve si Num_Array esta vacio
                BEQ cero_teclas ; Si esta vacio, se ven los casos para la primera tecla
                CMPA #$0B ; Si no esta ni vacio ni lleno, se procede con la operacion normal. Si la tecla es $0B, se borra un elemento
                BEQ borrar_tecla
                CMPA #$0E ; Si la tecla es $0E, se registran los datos en Num_Array, se reinicia el contador y se levanta la bandera ARRAY_OK
                BEQ enter_tecla
                MOVB Tecla_IN,B,X ; Si era una tecla numerica, se ingresa
                INC Cont_TCL ; El iterador para a la siguiente posicion de Num_Array
fin_FORMAR:     MOVB #$FF,Tecla_IN ; Si no era ninguno de los casos contemplados, la tecla es invalida y se borra Tecla_IN. No se modifica Num_Array
                RTS

max_teclas:     CMPA #$0B ; Caso de encontrar un boton B cuando el arreglo esta lleno. Si no es B, se pasa al siguiente caso, mientras que si lo es se elimina el elemento
                BNE enterMax
                DECB ; Se debe decrementar porque el puntero estaba listo para escribir
                MOVB #$FF,B,X ; Para borrar la tecla, se carga un $FF en la posicion correspondiente de Num_Array
                DEC Cont_TCL ; Se decrementa la posicion del iterador sobre Num_ARRAY
                BRA fin_FORMAR
enterMax:       CMPA #$0E ; Caso de encontrar un boton E cuando el arreglo esta lleno. Si no es un E, no se hace nada
                BNE fin_FORMAR
                BSET Banderas,$04 ; Si era E, se setea la bandera ARRAY_OK y se libera Cont_TCL para recibir otro arreglo. El inicio de Num_Array queda en $1007
                CLR Cont_TCL
                BRA fin_FORMAR
cero_teclas:    CMPA #$0B ; Si el arreglo esta vacio y se lee un B o un E, no se hace nada
                BEQ fin_FORMAR
                CMPA #$0E
                BEQ fin_FORMAR
                MOVB Tecla_IN,B,X ; Si es una tecla numerica, se registra en la primera posicion de Num_Array
                INC Cont_TCL
                BRA fin_FORMAR
borrar_tecla:   DECB ; Se debe decrementar 1 porque el puntero estaba listo para escribir
                MOVB #$FF,B,X ; Se borra una tecla si se puede y se recibio un B
                DEC Cont_TCL
                BRA fin_FORMAR
enter_tecla:    BSET Banderas,$04 ; Se ingresa el arreglo aunque no este lleno
                CLR Cont_TCL
                BRA fin_FORMAR
                
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
PH0_ISR:        BSET PIFH,$01 ; Se reinicia la bandera de interrupcion
                LDX #Num_Array ; Se limpia el contenido del arreglo
                CLRA
                BCLR Banderas,$04
loop_PH0:       MOVB #$FF,A,X
                INCA
                CMPA MAX_TCL
                BNE loop_PH0
                CLR CONT_TCL
                RTI