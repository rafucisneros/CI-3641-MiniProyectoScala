import scala.collection.mutable.Queue
import scala.collection.mutable.MutableList
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.CharSequenceReader
import scala.collection.mutable.{Map => MutableMap}
import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer
// Clase Abstracta GrafoBase. 
// Representara como seran los grafos en su manera basica.
// T: Tipo del valor de los nodos del grafo.
// U: Tipo del peso de los lados del grafo.
abstract class GrafoBase[T, U]{
    // Usamos "Case Classes" las cuales son un tipo especial
    // de clases optimizadas para su uso en pattern matching.

    // Cuando tengamos un objeto de tipo Lado o Nodo podremos
    // usar pattern matching para capturar el tipo correcto.

    // Clase Lado. Representara un lado del grafo.
    // n1, n2: Nodos unidos por este lado.
    // valor: peso del lado.
    case class Lado(n1: Nodo, n2: Nodo, var valor: U) {
        // comoTupla: Devuelve la representacion como tupla del
        // objeto, devolviendo el valor de cada nodo seguido
        // por el peso del lado.
        def comoTupla = (n1.valor, n2.valor, valor)
    }

    // Clase Nodo. Representara un nodo del grafo.
    // valor: Valor del nodo
    case class Nodo(valor: T) {
        // Lista de lado que inciden sobre este nodo
        var adj: List[Lado] = Nil
        // Devuelve la lista de vecinos de este nodo
        // Mapeamos sobre esta lista la funcion "otroLado"
        // para conseguirlos.
        def vecinos: List[Nodo] = adj.map(otroLado(_, this).get)
    }
    //Mapa de nodos del grafo
    //key de tipo valor del nodo
    //value es el nodo mismo
    var nodos: Map[T, Nodo] = Map()

    //Lista de Lados del grafo
    var lados: ArrayBuffer[Lado] = new ArrayBuffer()

    def otroLado(e: Lado, n: Nodo): Option[Nodo]

    def addNodo(value: T) = {
        val n = new Nodo(value)
        nodos = Map(value -> n) ++ nodos
        n
    }

    def printNodos{
		println("Nodos:")
		for((llave, valor) <- nodos){
			println(llave)
		}
	}

	def printLados{
		println("Lados:")
		for (i <- 0 to lados.length-1){
			println(f"$i ${lados(i).n1.valor} -> ${lados(i).n2.valor} ${lados(i).valor}")
		}
	}
	 def printPeso(k: Int){
		println(f"Peso ${lados(k).valor}")
	 }

	 def actualizarPeso(k: Int, p: U){
		lados(k).valor = p
	 }

	def eliminarLado(k: Int){
		lados.remove(k)
	}

	def eliminarNodo(llave: T){
		
		lados = lados.filter(l => l.n1.valor != llave && l.n2.valor != llave)
		nodos = nodos - llave
	} 

	def reiniciar{
		lados = new ArrayBuffer()
		nodos = Map()
	}
}

abstract class GrafoConstruible[T,U] extends GrafoBase[T,U] {
    def addLado(lado1: T, lado2: T, peso: U): Unit

    def crearGrafo(nodos: List[T], lados: List[(T,T,U)]) = {
        nodos.map(n => addNodo(n))
        lados.map({case (l1, l2, p) => addLado(l1, l2, p) })
        ()
    }
}

// Clase heredará GrafoDirigigo como clase, y un GrafoNoDirigido lo
// heredera por roles, para mostrar ambas funciones
trait RecorridoGrafos[T,U] extends GrafoBase[T,U] {

	def dfs(nombreInicial: T) : MutableList[T] = {
        val nodosVisitados: MutableList[T] = new MutableList()

		def dfs_aux(nombreInicial: T): Unit = {
            nodosVisitados += nombreInicial
			val nodoInicial = nodos(nombreInicial)
			
			for (hijo <- nodoInicial.vecinos 
                 if !nodosVisitados.contains(hijo.valor))
                dfs_aux(hijo.valor)
		}

		dfs_aux(nombreInicial)
        nodosVisitados
	}
		
	def bfs(nombreInicial: T) : MutableList[T] = {
        val cola: Queue[T] = Queue(nombreInicial)
        val nodosVisitados: MutableList[T] = new MutableList()

		def bfs_aux: Unit = {
            val nombreInicial = cola.dequeue
            nodosVisitados += nombreInicial
            val nodoInicial = nodos(nombreInicial)

			for(hijo <- nodoInicial.vecinos
				if !nodosVisitados.contains(hijo.valor))
				cola.enqueue(hijo.valor)
			
			if (!cola.isEmpty){
				bfs_aux
			}
		}

		bfs_aux
        nodosVisitados
	}
}

// printNodos
// printLados
// printPeso(k)
// trait GrafoImprimible[T,U] extends GrafoBase[T,U] {
//     def printNodos = {
//         println("Nodo")
//     }
// } 

class GrafoNoDirigido[T, U] extends GrafoConstruible[T,U]{
    def otroLado(l: Lado, n: Nodo): Option[Nodo] = 
        if (l.n1 == n) Some(l.n2)
        else if (l.n2 == n) Some(l.n1)
        else None

    override def addLado(lado1: T, lado2: T, peso: U) = {
        val l = new Lado(nodos(lado1), nodos(lado2), peso)
        lados += l
        nodos(lado1).adj = l :: nodos(lado1).adj
        nodos(lado2).adj = l :: nodos(lado2).adj
    }

    def this(nodos: List[T], lados: List[(T,T,U)]) = {
        this()
        crearGrafo(nodos, lados)
    }
}

class GrafoDirigido[T,U] extends GrafoConstruible[T,U] with RecorridoGrafos[T,U]{
    def otroLado(l: Lado, n: Nodo): Option[Nodo] = 
        if (l.n1 == n) Some(l.n2)
        else None

	override def addLado(fuente: T, destino: T, peso: U){
		val a = new Lado(nodos(fuente),nodos(destino), peso)
		lados += a
		nodos(fuente).adj = a :: nodos(fuente).adj
	}

    def this(nodos: List[T], lados: List[(T,T,U)]) = {
        this()
        crearGrafo(nodos, lados)
    }
}

// val grafoNoDirigido = new GrafoNoDirigido[String,Int] with RecorridoGrafos[String,Int]

// Arbol Sintactico 

sealed trait Comando
//Agregar
case class NodoAdd(nombre: String) extends Comando
case class LadoAdd(nombre1: String, nombre2: String, peso: Double) extends Comando
case class MultiLado(nombre1: String, nombre2: String, peso: Double) extends Comando
//Consultar
case class ConsultaNodos() extends Comando
case class ConsultaLados() extends Comando
case class ConsultaPeso(key: Int) extends Comando
//Actualizar
case class ActualizaPeso(key: Int, peso: Double) extends Comando
//Eliminar
case class EliminaNodo(nombre: String) extends Comando
case class EliminaLado(key: Int) extends Comando
case class Reiniciar() extends Comando
//Recorrido
case class Bfs(nombre: String) extends Comando
case class Dfs(nombre: String) extends Comando
//Salir
case class Salir() extends Comando

// Operadores

sealed trait Flechas
case object Derecha extends Flechas
case object Izquierda extends Flechas
case object Doble extends Flechas


class ParserGrafo extends RegexParsers with PackratParsers {
    // Regex
    val decimal = "\\d+(\\.\\d*)?|\\d*\\.\\d+".r
    val numero = "0|[0-9][1-9]*".r
    val nodo = "[A-Z][A-Za-z]*".r

    lazy val agregar: PackratParser[Comando] = {
        nodo ~ opt(("<->"  ^^^ Doble   |
                    "->"  ^^^ Derecha  |
                    "<-" ^^^ Izquierda ) ~ nodo ~ opt(decimal) ) ^^ {
        case n1 ~ None    => NodoAdd(n1)
        case n1 ~ Some(e) => e match{
             case Izquierda ~ n2 ~ Some(p) => LadoAdd  (n2, n1, p.toDouble)
             case Derecha ~ n2 ~ Some(p)   => LadoAdd  (n1, n2, p.toDouble)
             case Doble ~ n2 ~ Some(p)     => MultiLado(n1, n2, p.toDouble)
             case Izquierda ~ n2 ~ None => LadoAdd  (n2, n1, 0)
             case Derecha ~ n2 ~ None   => LadoAdd  (n1, n2, 0)
             case Doble ~ n2 ~ None     => MultiLado(n1, n2, 0)
            } 
        }
    }

    lazy val consultar: PackratParser[Comando] = {
        "nodes"^^^ConsultaNodos() |
        "edges"^^^ConsultaLados() |
        "edge" ~> numero ~ opt(decimal) ^^ {
            case n ~ None    => ConsultaPeso(n.toInt)
            case n ~ Some(i) => ActualizaPeso(n.toInt, i.toDouble)
        }
    }

    lazy val eliminar: PackratParser[Comando] = {
        "del" ~> nodo   ^^ {n => EliminaNodo(n)} |
        "del" ~> numero ^^ {n => EliminaLado(n.toInt)} |
        "reset"^^^Reiniciar() 
    }

    lazy val recorrido: PackratParser[Comando] = {
        "bfs" ~> nodo ^^ {n => Bfs(n)} |
        "dfs" ~> nodo ^^ {n => Dfs(n)}
    }

    lazy val salir: PackratParser[Comando] = "quit"^^^{Salir()}

    lazy val comando: PackratParser[Comando] = salir | recorrido | eliminar | consultar | agregar

    def parseAll[T](p: Parser[T], input:String) = 
        phrase(p)(new PackratReader(new CharSequenceReader(input)))
}

object InterpreteGrafo {
     val grafo = new GrafoDirigido[String, Double] with RecorridoGrafos[String, Double]

     def apply(comando: Comando)  = {
         comando match {
             case NodoAdd(n)           => grafo.addNodo(n); grafo.printNodos
             case LadoAdd(n1, n2, p)   => grafo.addLado(n1, n2, p); grafo.printLados
             case MultiLado(n1, n2, p) => {
                 grafo.addLado(n1, n2, p) 
                 grafo.addLado(n2, n1, p)
                 grafo.printLados
             }

             case ConsultaNodos() => grafo.printNodos
             case ConsultaLados() => grafo.printLados
             case ConsultaPeso(k) => grafo.printPeso(k)

             case ActualizaPeso(k, p) => grafo.actualizarPeso(k, p); grafo.printLados

             case EliminaNodo(n) => grafo.eliminarNodo(n); grafo.printNodos
             case EliminaLado(k) => grafo.eliminarLado(k); grafo.printNodos
             case Reiniciar()    => grafo.reiniciar

             case Bfs(n) => grafo.bfs(n)
             case Dfs(n) => grafo.dfs(n)

             case Salir() => ()
         }
     }
}
/*
val grafoNoDirigido = new GrafoNoDirigido[String,Int] with RecorridoGrafos[String,Int]

grafoNoDirigido.crearGrafo(
    List( "A", "AA", "AB", "AC", "AD", 
          "AAA", "AAB", "AAC", "ABA", "ABB", "ABC", 
          "ACA", "ACB", "ACC", "ADA", "ADB", "ADC"
        ),
    List( ("A","AA",5),("A","AB",5),("A","AC",5),
          ("A","AD",5),("AA","AAA",5),("AA","AAB",5),
          ("AA","AAC",5),("AB","ABA",5),("AB","ABB",5),
          ("AB","ABC",5),("AC","ACA",5),("AC","ACB",5),
          ("AC","ACC",5),("AD","ADA",5),("AD","ADB",5),
          ("AD","ADC",5)
        )
)

var grafoDirigido = new GrafoDirigido[String,Int](
    List( "A", "AA", "AB", "AC", "AD", 
          "AAA", "AAB", "AAC", "ABA", "ABB", "ABC", 
          "ACA", "ACB", "ACC", "ADA", "ADB", "ADC"
        ),
    List( ("A","AA",5),("A","AB",5),("A","AC",5),
          ("A","AD",5),("AA","AAA",5),("AA","AAB",5),
          ("AA","AAC",5),("AB","ABA",5),("AB","ABB",5),
          ("AB","ABC",5),("AC","ACA",5),("AC","ACB",5),
          ("AC","ACC",5),("AD","ADA",5),("AD","ADB",5),
          ("AD","ADC",5)
        )
)*/

object REPL {
	import parser.{Success, NoSuccess}
	val parser = new ParserGrafo
	def loop {
		while(true){
			val entrada = StdIn.readLine("-Comando:")
			parser.parseAll(parser.comando,entrada) match {
				case Success(Salir(),_) => return
				case Success(x,_) => InterpreteGrafo(x)
				case err: NoSuccess => println(err)	
			}
		}
	}

	def main(argv: Array[String]){
		loop
	}
}
