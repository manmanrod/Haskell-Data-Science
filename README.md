# Haskell-Data-Science
Proyecto de la asignatura Programación Declarativa (Universidad de Sevilla) sobre el estudio de unos datos con el lenguaje Haskell.

Este proyecto lo realicé en colaboración con mi compañero manvenpac (uvus).


## Estructura del código

El código lo hemos dividido en diferentes módulos, en los que nos encontramos:

- **Módulo PilaTA:** Módulo auxiliar usado en clase para implementar pilas mediante tipos de datos algebráicos
- **Módulo DataProcessing:** Módulo que contiene las funciones de procesado de datos
- **Módulo Datos:** Módulo que contiene los tipos de datos algebráicos para albergar los datos leídos del CSV y las funciones necesarias para leer y parsear los datos
- **Módulo Graficos:** Módulo que contiene las funciones necesarias para crear los gráficos circular, de barras y de ejes a partir de unos parámetros dados
- **Archivo Proyecto.hs:** Archivo que contiene la función main
- **Archivo steam.csv:** Archivo CSV que contiene los datos a procesar


## Librerías utilizadas

Para este proyecto hemos utilizado las siguientes librerías

- Librería **System (Environment, Directory)**
- Librería **Control (Exception)**
- Librería **Text (CSV)**
- Librería **Data (Dates,Time,List, List.Split, Map, Map.Strict)**
- Librería **Graphics.GChart**

Es necesario instalar:

    cabal install dates           (para Data.Dates)
    cabal install split             (para Data.List.Split)
    cabal install hs-gchart    (para Graphics.GChart)

**Dates** sirve para realizar operaciones con fechas
**Split** sirve para dividir listas respecto a un delimitador
**hs-gchart** sirve para generar gráficos en Haskell usando el API de “Google Charts”
