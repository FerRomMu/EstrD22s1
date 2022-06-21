typedef string TipoDePokemon;

struct EntrenadorSt {
  string nombre;
  Pokemon* pokemones;
  int cantidad;
};

typedef EntrenadorSt* Entrenador;

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
  EntrenadorSt* e = new EntrenadorSt;
  e->nombre = nombre;
  e->cantidad = cantidad;
  e->pokemones = pokemon;
}

string nombreDeEntrenador(Entrenador e){
  return e->nombre;
}

int cantidadDePokemon(TipodePokemon tipo, Entrenador e){
  return e->cantidad;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
  return filtrarLosDe(tipo, e->pokemones);
}

Pokemon pokemonNro(int i, Entrenador e){
  return e->pokemones[i];
}

venceAAlguno(Pokemon p, Entrenador e) {
  int i = 0;
  while(i < e->cantidad && !superaA(p, e->pokemon[i])){
    i++;
  }
  return i < e->cantidad;
}

leGanaATodos(Entrenador e1, Entrenador e2){
  int i = 0;
  while(i < e->cantidad && venceAAlguno(e->pokemon[i], e2)){
    i++;
  }
  return i < e->cantidad;
}
