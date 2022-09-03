Para lanzar el programa una vez cargado en ghci es necasario hacer

>leerCSV "./ejemplos/steam.csv"




Para compilar el programa hay que ejecutar el siguienta comando:

>ghc Proyecto.hs -o procesarCSV.exe         (en Windows)
>ghc Proyecto.hs -o procesarCSV             (en Linux)

Una vez compilado, se puede ejecutar con el siguiente comando:

>./procesarCSV.exe ./ejemplos/steam.csv     (en Windows)
>./procesarCSV ./ejemplos/steam.csv         (en Linux)

