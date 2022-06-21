struct NodoQ {
  int elem; //valor del nodo
  NodoQ* siguiente; //puntero al siguiente nodo
};

struct QueueSt {
  int cantidad;  //cantidad de elementos
  NodoQ* primero;//puntero al primer nodo
  NodoQ* ultimo; //puntero del ultimo nodo
};

typedef QueueSt* Queue;

Queue emptyQ();
bool isEmptyQ(Queue q);
int fistQ(Queue q);
void Enqueue(int x, Queue q);
void Dequeue(Queue q);
int lengthQ (Queue q);
void MergeQ(Queue q1, Queue q2);
void DestroyQ(Queue q);

