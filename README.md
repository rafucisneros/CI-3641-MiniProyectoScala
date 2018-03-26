# CI-3641-MiniProyecto
Parser, interprete y REPL de una calculadora simple y un minilenguaje de manejo de Grafos.

## Instalacion:
* Windows:
        Descargar los binarios de Scala para windows en el siguiente link.
        https://www.scala-lang.org/download/  

* Linux:
        Usar en la consola de comandos  
```
sudo apt-get install Scala
```

Para ambos es necesario tener instalado Java JDK 8.

## Ejecucion de los programas:

Para ejecutar la calculadora se usa el comando:
```
scala calculadora.scala
```

Los operadores de esta calculadora serán, +, -, *, /, % y ^ para sus respectivas operaciones matemáticas 
incluidos el + y el - unarios y el operador = para la asignación de variables. Las variables aceptadas
será cualquier palabra en minúsculas, y los números serán enteros o con decimales. Se puede asignar a una
variable cualquier expresión excepto por otras asignaciones. El programa finaliza al presionar Enter sin 
nada escrito.

Ejemplos
```
> x = 4 * 5
20.0
> 15 - x
-5.0
```
Para ejecutar el interprete de comandos para grafos se usa el comando:
```
scala parsergrafos.scala
```
El interprete inicia con un multigrafo dirigido vacío. Se agregan nodos introduciendo palabras que inician 
con letra Mayuscula. Se agregan nodos escribiendo los nombres de dos nodos, entre ambos nombres se indica 
la direccion del lado con ->,<- o <->,  y opcionalmente el peso del lado. Si no se espefica el peso, este 
será cero.
Ejemplos: 
```
> A
Nodos:
A
> B
Nodos:
B
A
> A <-> C 6
Lados:
0 A -> C 6.0
1 C -> A 6.0
```
Ademas se incluyen los siguientes comandos:

* **edges**: imprime los lados del grafo.  
* **nodes**: imprime los nodos del grafo.  
* **del x**: Si x es un numero, se elimina el x-esimo lado del grafo (consultables con el comando edges). Si x es
String, se elimina el nodo que coincida con el String.  
* **reset**: elimina todos los nodos y lados del grafo.  
* **edge x**: imprime el peso del x-esimo lado.  
* **edge x y**: actualiza el peso del x-esimo lado al valor y.  
* **bfs x**: imprime el recorrido bfs del grafo iniciando desde el nodo con valor x.  
* **dfs x**: imprime el recorrido dfs del grafo iniciando desde el nodo con valor x.   
* **quit**: Termine el programa.  

## Autores

* **Erick Flejan** - *12-11555* 
* **Rafael Cisneros** - *13-11156*
