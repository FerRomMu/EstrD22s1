#include "Queue.h"

Queue emptyQ(){
  QueueSt* q = new QueueSt;
  q -> cantidad = 0;
  q -> primero = NULL;
  q -> ultimo = NULL;
  return q;
}

bool isEmptyQ(Queue q){
  return q->primero == NULL;
}

int fistQ(Queue q){
  return q->primero;
}

void Enqueue(int x, Queue q){
  NodoQ* temp = new NodoQ;
  temp -> siguiente = NULL;
  temp -> elem = x;
  if(q->primero == NULL) {
    q->primero = temp;
    q->ultimo = temp;
  }else{
    q->ultimo->siguiente = temp;
  }
  q->ultimo = temp;
  q->cantidad++;
}

void Dequeue(Queue q){
  NodoQ* temp= q->primero;
  q->primero = q->primero->siguiente;
  if(q->primero == NULL){
    q->ultimo = NULL;
  }
  delete temp;
  q->cantidad--;
}

int lengthQ (Queue q){
  return q->cantidad;
}

void MergeQ(Queue q1, Queue q2){
  q1->ultimo->siguiente=q2->primero;
  q1->ultimo = q2->ultimo;
  q1->cantidad += q2->cantidad;
  delete q2;
}

void DestroyQ(Queue q){
  NodoQ* temp = q->primero;
  while(q->primero != NULL){
    temp = q->primero;
    q->primero = q->primero->siguiente;
    delete temp;
  }
  delete q;
}
