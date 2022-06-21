struct NodoS {
  int elem;         //Valor del nodo
  NodoS* siguiente; //Puntero al siguiente nodo
}

struct SetSt {
  int cantidad;  // Cantidad de elementos diferentes
  NodoS* primero;// Puntero al primer nodo
}

typedef SetSt* Set;

Set emptyS();
bool isEmptyS(Set s);
bool belongS(int x, Set s);
void AddS (int x, Set s);
void RemoveS (int x, Set s);
int sizeS(Set s);
LinkedList setToList(Set s);
void DestroyS(Set s);

