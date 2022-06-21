include "LinkedList.h";

int sumatoria(LinkedList xs) {
  ListIterator* i = getIterator(xs);
  int ac = 0;
  while(! atEnd(i)) {
    ac += current(i);
    Next(i);
  }
  return ac;
}

void Sucesores(LinkedList xs){
  ListIterator* i = getIterator(xs);
  while(! atEnd(i)) {
    setCurrent(current(i)+1);
    Next(i);
  }
}

bool pertenece(int x, LinkedList xs){
  ListIterator* i = getIterator(xs);
  while(! atEnd(i) && current(i) != x) {
    Next(i);
  }
  return atEnd(i);
}

int apariciones(int x, LinkedList xs){
  ListIterator* i = getIterator(xs);
  int c = 0;
  while(! atEnd(i)) {
    if(current(i) == x) {
      c++;
    }
    Next(i)
  }
  return c;
}

int minimo(LInkedList xs){
  //Precondición: La linked list dada tiene elemento.
  ListIterator* i = getIterator(xs);
  int min = current(i);
  while(! atEnd(i)) {
    if(current(i) < min) {
      min = current(i);
    }
  }
  return min;
}

LinkedList copy(LinkedList xs) {
  LinkedList l = nil();
  ListIterator* i = getInterator(xs);
  while(! atEnd(i)) {
    Snoc(current(i), l);
  }
  return l;
}

void Append(LinkedList xs, LinkedList ys) {
  ListIterator* i = getInterator(ys);
  while(! atEnd(i)) {
    Snoc(current(i), xs);
  }
  DestroyL(ys);
}

