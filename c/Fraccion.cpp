#include <iostream>
#include "Fraccion.h"

using namespace std;

Fraccion consFraccion(int numerador, int denominador){
  Fraccion f;
  f.numerador = numerador;
  f.denominador = denominador;
  return f;
}

int numerador(Fraccion f){
   return f.numerador;
}

int denominador(Fraccion f){
  return f.denominador;
}

float division(Fraccion f){
  return f.numerador / f.denominador;
}

Fraccion multF(Fraccion f1, Fraccion f2){
  int nu = f1.numerador * f2.numerador;
  int de = f1.denominador * f2.denominador;
  return consFraccion(nu, de);
}

Fraccion simplificada(Fraccion f){
  int m = min(f.numerador, f.denominador);
  int nu = f.numerador;
  int de = f.denominador;
  for(int i=m;i>1;i--){
    if(esDivisor(i,nu) && esDivisor(i,de)){
      nu = nu/i;
      de = de/i;
      i = min(nu,de);
    }
  }
  return consFraccion(nu,de);
}

bool esDivisor(int n, int m){
  return m%n==0;
}

Fraccion sumF(Fraccion f1, Fraccion f2){
  int nu = (f1.numerador * f2.denominador) + (f2.numerador * f1.denominador);
  int de = f1.denominador * f2.denominador;
  return consFraccion(nu,de);
}  

void ShowFraccion(Fraccion f) {
  cout << f.numerador;
  cout << " / ";
  cout << f.denominador << endl;
}

int main() {
  Fraccion f = consFraccion(4,8);
  float a = division(f) * 100; //¿Porque devuelve 0???
  cout << numerador(f)+denominador(f) << endl;
  cout << a << endl;
  ShowFraccion(simplificada(f));
  Fraccion f2 = consFraccion(3,2);
  ShowFraccion(multF(f,f2));
  ShowFraccion(sumF(f,f2));
}
