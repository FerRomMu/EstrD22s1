struct NodoL {
  int elem;        //Valor de nodo
  NodoL* siguiente;//Puntero al siguiente nodo
}

strut LinkedListSt {
  //INV. REP.: cantidad indica la cantidad de nodos que se pueden recorrer desde primero por siguiente
  //           hasta alcanzar a NULL.
  int cantidad;   //Cantidad de elementos.
  NodoL* primero; //Puntero al primer nodo.
  NodoL* ultimo;
}

typedef LinkedListSt* LinkedList; 
  //INV. REP.: El puntero NO es NULL.

struct IteratorSt {
  NodoL* current;
}

typedef IteratorSt* ListIterator;
  //INV. REP.: EL puntero NO es NULL.

LinkedList nil(){
  LinkedListSt* xs = new LinkedListSt;
  xs->cantidad=0;
  xs->primero=null;
  xs->ultimo=null;
  return xs;
}

bool isEmpty(LinkedList xs){
  return (xs->cantidad==0);
}

int head(LinkedList xs){
  return(xs->primero->elem);
}

void Cons(int x, LinkedList xs){
  NodoL* xn = new NodoL;
  xn -> elem = x;
  xn -> siguiente = xs -> primero;
  xs -> primero = xn;
  if(xn -> siguiente == NULL) {
    xs -> ultimo = xs -> primero;
  }
  xs -> cantidad++;
}

void Tail(LinkedList xs){
  NodeL* temp = xs -> primero;
  xs -> primero = temp -> siguiente;
  if(xs -> primero == NULL) {
    xs -> ultimo = NULL;
  }
  xs -> cantidad--;
  delete temp;
}

int length(LinkedList xs){
  return xs -> cantidad;
}

void Snoc(int x, LinkedList xs){
  NodeL* temp = new NodoL;
  temp -> elem = x;
  temp -> siguiente = NULL;

  xs -> ultimo -> siguiente = temp;
  xs -> ultimo = temp;
  if (xs -> primero == null) {
    xs -> primero = temp;
  } else {
    xs -> ultimo -> siguiente = temp;
  }
  xs -> ultimo = temp;
  xs -> cantidad++;
}

void Append(LinkedList xs, LinkedList ys) {
  xs -> ultimo -> siguiente = ys -> primero;
  delete ys;
}

ListIterator getIterator(LinkedList xs){
  IteratorSL* i = new IteratorSL;
  i -> current = xs -> primero;
  return i;
}

int current (ListIterator ixs){
  return ixs -> current;
}

void SetCurrent(int x, ListIterator ixs){
  ixs -> current -> elem = x;
}

void Next(ListIterator ixs){
  ixs -> current = ixs -> current -> siguiente;
}

bool atEnd(ListIterator ixs){
  return xs -> current == NULL;
}

void DisposeIterator(ListIterator xs){
  delete xs;
}

void DestroyL(LinkedList xs){
  NodeL* temp = xs -> primero;
  while(xs -> primero != NULL) {
    xs -> primero = xs -> primero -> siguiente;
    delete temp;
    temp = xs -> primero;
  }
  delete xs;
}
