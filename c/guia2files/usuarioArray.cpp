int sumatoria(ArraList xs){
  int ac = 0;
  for(int i=0; i < lengthAL(xs); i++){
    ac+=get(i,xs);
  }
  return ac;
}

void sucesores(ArrayList xs) {
  for(int i=0; i < lengthAL(xs); i++){
    set(i,get(i,xs)+1,xs);
  }
}

bool pertenece(int x, ArrayList xs){
  int i=0;
  while(i<lengthAL(xs) && get(i,xs)!=x){
    i++;
  }
  return i<lengthAL(xs);
}

int apariciones(int x, ArrayList xs){
  int c=0;
  for(int i=0; i < lengthAL(xs); i++){
    if(x == get(i,xs)){
      c++;
    }
  }
  return c;
}

ArrayList append(ArrayList xs, ArrayList ys){

}

int minimo(ArrayList xs){
  int min = get(0,xs);
  for(int i=1; i < lengthAL(xs); i++){
    if(min >= get(i,xs)){
      min = get(i,xs);
    }
  }
}
