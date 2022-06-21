typedef string TipoDePokemon;

struct PokeSt {
  TipoDePokemon tipo;
  int energia;
}

typeDef PokeSt* Pokemon;

Pokemon consPokemon(TipoDePokemon tipo);
tipoDePokemon tipoDePokemon(Pokemon p);
int energia(Pokemon p);
void perderEnergia(int energia, Pokemon p);
bool superaA(Pokemon p1, Pokemon p2);
