import scala.collection.mutable.Queue
import scala.collection.mutable.MutableList

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
    case class Lado(n1: Nodo, n2: Nodo, valor: U) {
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
    var lados: List[Lado] = Nil

    def otroLado(e: Lado, n: Nodo): Option[Nodo]

    def addNodo(value: T) = {
        val n = new Nodo(value)
        nodos = Map(value -> n) ++ nodos
        n
    }
}

abstract class GrafosConstruibles[T,U] extends GrafoBase[T,U] {
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

class GrafoNoDirigido[T, U] extends GrafosConstruibles[T,U]{
    def otroLado(l: Lado, n: Nodo): Option[Nodo] = 
        if (l.n1 == n) Some(l.n2)
        else if (l.n2 == n) Some(l.n1)
        else None

    override def addLado(lado1: T, lado2: T, peso: U) = {
        val l = new Lado(nodos(lado1), nodos(lado2), peso)
        lados = l :: lados
        nodos(lado1).adj = l :: nodos(lado1).adj
        nodos(lado2).adj = l :: nodos(lado2).adj
    }

    def this(nodos: List[T], lados: List[(T,T,U)]) = {
        this()
        crearGrafo(nodos, lados)
    }
}

class GrafoDirigido[T,U] extends GrafosConstruibles[T,U] with RecorridoGrafos[T,U]{
    def otroLado(l: Lado, n: Nodo): Option[Nodo] = 
        if (l.n1 == n) Some(l.n2)
        else None

	override def addLado(fuente: T, destino: T, peso: U){
		val a = new Lado(nodos(fuente),nodos(destino), peso)
		lados = a :: lados
		nodos(fuente).adj = a :: nodos(fuente).adj
	}

    def this(nodos: List[T], lados: List[(T,T,U)]) = {
        this()
        crearGrafo(nodos, lados)
    }
}

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
)