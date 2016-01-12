# Implementación de FOIL en Haskell
Foil es un algoritmo de **programación lógica inductiva** usado para aprender reglas usando lógica de primer orden que describan un predicado a partir de un conocimiento base y de un conjunto de ejemplos, denominado conjunto de entrenamiento.

Esta implementación de FOIL hace uso de la asumpción de mundo cerrado, es decir, que todo aquellos ejemplos no descritos se consideran negativos.

# Instalación

ejecuta: `cabal install` y tendrás el ejecutable en `dist/build/foil/`

# Ejecución

en el mismo directorio que el archivo ejecutable (`dist/build/foil/`):

```
    ./foil <dataset> <"regla objetivo">
```

donde:
    * **dataset** es la ruta al archivo con el conocimiento de fondo y los ejemplos.
    * **"regla objetivo"** es la regla que queremos que foil extraiga. Entre comillas.

Ejemplo: 
```
    >$ ./foil test1.dat "nieta(X,Y)"
    nieta(X,Y):- mujer(Y), padre(Z0,X), padre(Y,Z0).
```

# Licencia

Este codigo está distribuido bajo la licencia GPL-2
