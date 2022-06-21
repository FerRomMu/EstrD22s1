#include <strdio.h>

struct PersonaSt {
  string nombre;
  int edad;
}

typedef PersonaSt* Persona;
Persona consPersona(string nombre, int edad);
string nombre(Persona p);
int edad(Persona p);
void crecer (Persona P);
void cambioDeNombre(string nombre, Persona p);
bool esMayorQueLaOtra(Persona p1, Persona p2);
Persona laQueEsMayor(Persona p1, Persona p2);

