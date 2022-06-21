#include "LinkedList.h"

LinkedList nil(){
  LinkedListSt* xs = new LinkedListSt;
  xs->cantidad=0;
  xs->primero=null;
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
  xs -> cantidad++;
}

void Tail(LinkedList xs){
  NodeL* temp = xs -> primero;
  xs -> primero = temp -> siguiente;
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

  NodeL* actual = new NodoL;
  if (xs -> primero != null) {
    actual = xs -> primero;
    while(actual -> siguiente != NULL) {
      actual = actual -> siguiente;
    }
    actual -> siguiente = nodo;
  } else {
    xs -> primero = nodo;
  }
  xs -> cantidad++;
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
