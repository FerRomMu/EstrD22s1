typedef string TipoDePokemon;

struct EntrenadorSt {
  string nombre;
  Pokemon* pokemones;
  int cantidad;
}

typedef EntrenadorSt* Entrenador;

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon);
string nombreDeEntrenador(Entrenador e);
int cantidadDePokemon(TipodePokemon tipo, Entrenador e);
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e);
Pokemon pokemonNro(int i, Entrenador e);
leGanaATodos(Entrenador e1, Entrenador e2);
