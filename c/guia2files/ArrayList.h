struct ArrayListSt {
  int cantidad;
  int* elementos;
  int capacidad;
};

typedef ArrayListSt* ArrayList;

ArrayList newArrayList();
ArrayList newArrayListWith(int capcidad);
int lengthAL(ArrayList xs);
int get(int i, ArrayList xs);
void set(int i, int x, ArrayList xs);
void resize(int capacidad, ArrayList xs);
void add(int x, ArrayList xs);
void remove(ArrayList xs);
