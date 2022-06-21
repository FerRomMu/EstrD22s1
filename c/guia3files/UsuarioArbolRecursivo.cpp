#include "Tree.h"
#include "ArrayList.h"

int sumarT(Tree t){
  if(isEmptyT t){
    return 0;
  }else{
    return (rootT (t))+sumarT(left(t))+sumarT(right(t));
  }
}

int sizeT(Tree t) {
  if(isEmptyT t){
    return 0;
  }else{
    return 1 + sizeT(left(t)) + sizeT(right(t));
  }
}

bool perteneceT(int e, Tree t){
  if(isEmptyT t){
    return false;
  }else{
    return (rootT(t) == e) || perteneceT(left(t)) || perteneceT(right(t));
  }
}

unoSi(bool b) {
  if(b){
    return 1;
  }else{
    return 0;
  }
}

int aparicionesT(int e, Tree t){
  if(isEmptyT t){
    return 0;
  }else{
    return unoSi(rootT(t)==e) + aparicionesT(left(t)) + aparicionesT(right(t));
  }
}

int heightT(Tree t){
  if(isEmptyT t){
    return 0;
  }else{
    if(heightT(left(t)) > height(right(t))){
      return 1 + heightT(left(t));
    }else{
      return 1 + heightT(right(t));
    }
  }
}

ArrayList toList(Tree t){
  if(isEmptyT t){
    return newArrayList();
  }else {
    return add(rootT(t), unir(toList(left(t)), toList(right(t))));
  }
}

ArrayList leaves(Tree t){
  if(isEmptyT t){
    return newArrayList();
  }else if(isEmptyT(left (t)) && isEmptyT(right(t))){
    return add(rootT t, newArrayList());
  }else {
    return unir(leaves(left(t)), leaves(right(t)));
  }
}

ArrayList levelN(int n, Tree t){
  if(isEmptyT t){
    return newArrayList();
  }else if(n=0){
    return add(rootT(t), newArrayList());
  }else {
    return unir(levelN(--n,left(t)), levelN(--n,right(t)));
  }
}
