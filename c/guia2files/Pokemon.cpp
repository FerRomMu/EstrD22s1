typedef string TipoDePokemon;

struct PokeSt {
  TipoDePokemon tipo;
  int energia;
};

typeDef PokeSt* Pokemon;

Pokemon consPokemon(TipoDePokemon tipo) {
  PokeSt* p;
  p->tipo = tipo;
  p->energia = 100;
}

tipoDePokemon tipoDePokemon(Pokemon p){
  return p->tipo;
}

int energia(Pokemon p){
 return p->energia;
}

void perderEnergia(int energia, Pokemon p){
  p->energia -= energia;
}

bool esTipoSuperador(TipoDePokemon t1, TipoDePokemon t2) {
  return (t1 == "Agua" && t2 == "Fuego") ||
         (t1 == "Fuego" && t2 == "Planta") ||
         (t1 == "Planta" && t2 == "Agua")
}

bool superaA(Pokemon p1, Pokemon p2){
  return esTipoSuperador(p1->tipo, p2->tipo)
}
