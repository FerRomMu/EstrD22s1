struct NodoL {
  int elem;        //Valor de nodo
  NodoL* siguiente;//Puntero al siguiente nodo
};

struct LinkedListSt {
  //INV. REP.: cantidad indica la cantidad de nodos que se pueden recorrer desde primero por siguiente
  //           hasta alcanzar a NULL.
  int cantidad;   //Cantidad de elementos.
  NodoL* primero; //Puntero al primer nodo.
};

typedef LinkedListSt* LinkedList; 
  //INV. REP.: El puntero NO es NULL.

struct IteratorSt {
  NodoL* current;
};

typedef IteratorSt* ListIterator;
  //INV. REP.: EL puntero NO es NULL.

LinkedList nil();
bool isEmpty(LinkedList xs);
int head(LinkedList xs)
void Cons(int x, LinkedList xs);
void Tail(LinkedList xs);
int length(LinkedList xs)
void Snoc(int x, LinkedList xs);
ListIterator getIterator(LinkedList xs);
int current (ListIterator ixs);
void SetCurrent(int x, ListIterator ixs);
void Next(ListIterator ixs);
bool atEnd(ListIterator ixs);
void DisposeIterator(ListIterator xs);
void DestroyL(LinkedList xs);
