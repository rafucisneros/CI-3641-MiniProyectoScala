import scala.collection.mutable.Queue
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

// Clase heredará GrafoDirigigo como clase, y un GrafoNoDirigido lo
// heredera por roles, para mostrar ambas funciones
trait RecorridoGrafos[T,U] extends GrafoBase[T,U] {

	def dfs(nombreInicial: T) : List[T] = {
		// Definiciones anidadas de funciones
		def dfs_aux(nombreInicial: T, visitados: List[T]) : List[T] = {
			var nodoInicial = nodos(nombreInicial)
			var nodosVisitados: List[T] = visitados 
			nodosVisitados = nodoInicial.valor :: nodosVisitados
			var vecinos = nodoInicial.vecinos
			for (hijo <- vecinos){
				if (!(nodosVisitados.contains(hijo.valor))){
					nodosVisitados = dfs_aux(hijo.valor,nodosVisitados)
				}
			}
			nodosVisitados
		}
		dfs_aux(nombreInicial,Nil).reverse
	}
		

	def bfs(nombreInicial: T) : List[T] = {	
		def bfs_aux(nombreInicial: T, cola: Queue[T], visitados: List[T]) : List[T] = {
			var nodoInicial = nodos(nombreInicial)
			var nodosVisitados = nombreInicial :: visitados
			var vecinos = nodoInicial.vecinos
			for(hijo <- vecinos){
				if(!nodosVisitados.contains(hijo.valor)){
					cola.enqueue(hijo.valor)
				}
			}
			cola.dequeue
			if (!cola.isEmpty){
				nodosVisitados = bfs_aux(cola(0), cola, nodosVisitados)
			}			
			nodosVisitados
		}
		bfs_aux(nombreInicial, Queue(nombreInicial), Nil).reverse
	}
}


class GrafoNoDirigido[T, U] extends GrafoBase[T, U]{
    def otroLado(l: Lado, n: Nodo): Option[Nodo] = 
        if (l.n1 == n) Some(l.n2)
        else if (l.n2 == n) Some(l.n1)
        else None

    def addLado(lado1: T, lado2: T, peso: U) = {
        val l = new Lado(nodos(lado1), nodos(lado2), peso)
        lados = l :: lados
        nodos(lado1).adj = l :: nodos(lado1).adj
        nodos(lado2).adj = l :: nodos(lado2).adj
    }
}

class GrafoDirigido[T,U] extends GrafoBase[T,U] with RecorridoGrafos[T,U]{
    def otroLado(l: Lado, n: Nodo): Option[Nodo] = 
        if (l.n1 == n) Some(l.n2)
        else None

	def addLado(fuente: T, destino: T, peso: U){
		val a = new Lado(nodos(fuente),nodos(destino), peso)
		lados = a :: lados
		nodos(fuente).adj = a :: nodos(fuente).adj
	}
}
        
var grafoNoDirigido = new GrafoNoDirigido[String,Int] with RecorridoGrafos[String,Int]
grafoNoDirigido.addNodo("1") 
grafoNoDirigido.addNodo("11") 
grafoNoDirigido.addNodo("12") 
grafoNoDirigido.addNodo("13") 
grafoNoDirigido.addNodo("14") 
grafoNoDirigido.addNodo("111") 
grafoNoDirigido.addNodo("112") 
grafoNoDirigido.addNodo("113") 
grafoNoDirigido.addNodo("121") 
grafoNoDirigido.addNodo("122") 
grafoNoDirigido.addNodo("123") 
grafoNoDirigido.addNodo("131") 
grafoNoDirigido.addNodo("132") 
grafoNoDirigido.addNodo("133") 
grafoNoDirigido.addNodo("141") 
grafoNoDirigido.addNodo("142") 
grafoNoDirigido.addNodo("143")
grafoNoDirigido.addLado("1","11",1)
grafoNoDirigido.addLado("1","12",1)
grafoNoDirigido.addLado("1","13",1)
grafoNoDirigido.addLado("1","14",1)
grafoNoDirigido.addLado("11","111",1)
grafoNoDirigido.addLado("11","112",1)
grafoNoDirigido.addLado("11","113",1)
grafoNoDirigido.addLado("12","121",1)
grafoNoDirigido.addLado("12","122",1)
grafoNoDirigido.addLado("12","123",1)
grafoNoDirigido.addLado("13","131",1)
grafoNoDirigido.addLado("13","132",1)
grafoNoDirigido.addLado("13","133",1)
grafoNoDirigido.addLado("14","141",1)
grafoNoDirigido.addLado("14","142",1)
grafoNoDirigido.addLado("14","143",1)
var grafoDirigido = new GrafoDirigido[String,Int]
grafoDirigido.addNodo("1") 
grafoDirigido.addNodo("11") 
grafoDirigido.addNodo("12") 
grafoDirigido.addNodo("13") 
grafoDirigido.addNodo("14") 
grafoDirigido.addNodo("111") 
grafoDirigido.addNodo("112") 
grafoDirigido.addNodo("113") 
grafoDirigido.addNodo("121") 
grafoDirigido.addNodo("122") 
grafoDirigido.addNodo("123") 
grafoDirigido.addNodo("131") 
grafoDirigido.addNodo("132") 
grafoDirigido.addNodo("133") 
grafoDirigido.addNodo("141") 
grafoDirigido.addNodo("142") 
grafoDirigido.addNodo("143")
grafoDirigido.addLado("1","11",1)
grafoDirigido.addLado("1","12",1)
grafoDirigido.addLado("1","13",1)
grafoDirigido.addLado("1","14",1)
grafoDirigido.addLado("11","111",1)
grafoDirigido.addLado("11","112",1)
grafoDirigido.addLado("11","113",1)
grafoDirigido.addLado("12","121",1)
grafoDirigido.addLado("12","122",1)
grafoDirigido.addLado("12","123",1)
grafoDirigido.addLado("13","131",1)
grafoDirigido.addLado("13","132",1)
grafoDirigido.addLado("13","133",1)
grafoDirigido.addLado("14","141",1)
grafoDirigido.addLado("14","142",1)
grafoDirigido.addLado("14","143",1)


