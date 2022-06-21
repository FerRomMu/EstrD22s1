#include "Set.h"

struct NodoS {
  int elem;         //Valor del nodo
  NodoS* siguiente; //Puntero al siguiente nodo
}

struct SetSt {
  int cantidad;  // Cantidad de elementos diferentes
  NodoS* primero;// Puntero al primer nodo
}

typedef SetSt* Set;

Set emptyS() {
  SetSt* s = new SetSt;
  s -> cantidad = 0;
  s -> primero = NULL;
  return s;
}

bool isEmptyS(Set s){
  return s -> cantidad == 0;
}

bool belongS(int x, Set s) {
  NodoS* actual = s -> primero;
  while(actual != NULL && actual -> elem != x) {
    actual = actual -> siguiente;
  }
  return actual != NULL;
}

void AddS (int x, Set s){
  NodoS* actual = s -> primero;
  while(actual != NULL && actual -> elem != x) {
    actual = actual -> siguiente;
  }
  NodoS* temp = new NodoS;
  temp -> elem = x;
  temp -> siguiente = NULL;
  actual -> siguiente = temp;
  s -> cantidad ++;
}

void RemoveS (int x, Set s){
  NodoS* actual = s -> primero;
  NodoS* anterior = actual;
  while(actual != NULL && actual -> elem != x) {
    anterior = actual;
    actual = actual -> siguiente;
  }
  if(actual != NULL) {
    anterior -> siguiente = actual -> siguiente;
    delete actual;
    s -> cantidad --;
  }
}

int sizeS(Set s){
  return s -> cantidad;
}

LinkedList setToList(Set s){
  LinkedList l = nil();
  NodoS* actual = s -> primero;
  while(actual != NULL) {
    Cons(actual -> elem, l);
    actual = actual -> siguiente;
  }
  return l;
}

void DestroyS(Set s){
  NodeS* actual = s -> primero;
  NodeS* temp = NULL;
  while(actual != NULL) {
    temp = actual;
    actual = actual -> siguiente;
    delete temp;
  }
  delete s;
}

