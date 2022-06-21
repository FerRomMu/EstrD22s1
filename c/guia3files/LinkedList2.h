struct NodoL {
  int elem;
  NodoL* siguiente;
}

strut LinkedListSt {
  int cantidad;
  NodoL* primero;
  NodoL* ultimo;
}

typedef LinkedListSt* LinkedList;

struct IteratorSt {
  NodoL* current;
}

typedef IteratorSt* ListIterator;

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

g++ -i main prueba.cpp tad1.cpp tadn.cpp
