#include <iostream>
#include "Par.h"
using namespace std;

Par consPar(int x, int y){
  Par p;
  p.x = x;
  p.y = y;
  return p;
}

int fst(Par p){
  return p.x;
}

int snd(Par p){
  return p.y;
}

int maxDelPar(Par p){
  return max(p.x, p.y);
}

Par swap(Par p){
  return consPar(p.y, p.x);
}

Par divisionYResto(int n, int m){
  float d = n/m;
  int r = n%m;
  return consPar(d,r);
}

void ShowPar(Par p){
  cout << "Par(";
  cout << "x <- \"" << p.x << "\", ";
  cout << "y <- \"" << p.y << "\"";
  cout << ")" << endl;
}

int main(){
  Par par = consPar(5,4);
  ShowPar(par);
  return 0;
}
