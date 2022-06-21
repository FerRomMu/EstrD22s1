#include "ArrayList.h"

ArrayList newArrayList(){
  ArrayListSt* a;
  a->cantidad=0;
  a->elementos=new int[16];
  a->capacidad=16;
  return a;
}

ArrayList newArrayListWith(int capacidad){
  ArrayListSt* a;
  a->cantidad=0;
  a->elementos=new int[capacidad];
  a->capacidad=capacidad;
  return a;
}

int lengthAL(ArrayListSt* xs){
  return xs->cantidad;
}

int get(int i, ArrayList xs){
  return xs->elementos[i];
}

void set(int i, int x, ArrayList xs){
  //Precon hay elemento en indice dado.
  xs->elementos[i]=x;
}

void resize(int capacidad, ArrayList xs){
  int* ys = new int[capacidad];
  int c = 0;
  for(int i=0; i< capacidad && i <= xs->cantidad; i++){
    ys[i] = xs->elementos[i];
    c++;
  }
  xs->cantidad = c;
  xs->capacidad = capacidad;
  delete xs->elementos;
  xs->elementos = ys;
}

void add(int x, ArrayList xs){

  if(xs->capacidad <= xs->cantidad){
    resize(xs->capacidad*2, xs);
  }
  xs->elementos[xs->cantidad]=x;
  xs->cantidad++;
}

void remove(ArrayList xs){
  xs->cantidad--;
}
