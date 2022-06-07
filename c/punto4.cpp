#include <iostream>
using namespace std;

//1.
//Recursivo
void printN (int n, string s){
  if(n>0){
    cout << s << endl;
    printN (--n, s);
  }
}

//Iterativo
void printNI (int n, string s){
  for(int i=0; i<n;i++){
    cout << s << endl;
  }
}

//----------------------------
//2.
//Recursivo
void cuentaRegresiva(int n){
  if(n>=0){
    cout << n << endl;
    cuentaRegresiva(--n);
  }
}

//Iterativo
void cuentaRegresivaI(int n){
  while(n>0){
    cout << n << endl;
    n--;
  }
}

//----------------------------
//3.
//Recursivo
void desdeCeroHastaN(int n){
  if(n > 0){
    desdeCeroHastaN(--n);
  }
  cout << n << endl;
}

//Iterativo
void desdeCeroHastaNI(int n){
  for(int i=0; i<=n;i++){
    cout << i << endl;
  }
}

//----------------------------
//4.
//Recursivo
int mult(int n, int m){
  if(m > 1) {
    return n + mult(n, --m);
  }
  return n;
}

//iterativo
int multI(int n, int m){
  for(int i=1; i<m; i++){
    n+=n;
  }
  return n;
}

//----------------------------
//5.
//Recursivo
void primerosN(int n, string s) {
  if(n-1 >= 0) {
    primerosN(--n,s);
    cout << s[n];
  }
}

//Iterativo
void primerosNI(int n, string s) {
  for(int i=0; i<n; i++){
    cout << s[i];
  }
}

//----------------------------
//6.
//Recursivo
bool esCaracter(char c, string s, int i){
  if(i < s.length()) {
    return (c == s[i]) || esCaracter(c,s,i+1);
  }
  return false;
}

bool pertenece (char c, string s) {
  return esCaracter(c, s, 0);
}

//Iterativo
bool perteneceI (char c, string s) {
  int i = 0;
  bool esta = false;
  while(i < s.length() && !esta){
    esta = s[i] == c;
  }
  return esta;
}

//----------------------------
//7.
//Recursivo

int unoSi(bool b){
 if(b){return 1;}else{return 0;}
}

int apariciones(char c, string s) {
 return esCaracter(c, s, 0);
}

int cantCaracter(char c, string s, int i){
  if(i < s.length()) {
    return unoSi(c == s[i]) || cantCaracter(c,s,i+1);
  }
  return 0;
}

//Iterativo
int aparicionesI(char c, string s){
  int i = 0;
  int cont = 0;
  while(i < s.length()) {
    cont = unoSi(c == s[i]);
    i++;
  }
  return cont;
}

int main() {
  
  printN(3, "Hola");
  printNI(3, "Hola iterativo");

  cuentaRegresiva(3);
  cuentaRegresivaI(3);

  desdeCeroHastaN(5);
  desdeCeroHastaNI(5);

  primerosN(4, "Holaa");
  primerosNI(4, "Holaa");

  cout << endl;
  cout << mult(3,3) << endl;
  cout << multI(3,3) << endl;
  if(pertenece('H', "Hola")){
    cout << aparicionesI('H', "Hola");
    cout << apariciones('H', "Hola");
  }
  return 0;
}
