# TFM_SETDO
Trabajo de fin de master: Sintesis de Explicaciones en Tablas de Decisiones Óptimas

El repositorio contiene dos tipos de ficheros:

1. Los relacionados con la infraestructura del problema: Cambios de base, calcular la lista KBM2L y cálculo de la tabla de decisión óptima.
2. Los relacionados con los métodos heurísticos: EDA UMDA, TAN y método exhaustivo. Estos dos primeros, debido a su extensión, estan divididos en varios ficheros.

Puede encontrar la memoria del proyecto en el siguiente **[enlace](https://oa.upm.es/75298/)**.

## Resumen de la memoria

La toma de decisiones es una asignatura que se ha ido tornando cada vez más difícil con los años y no es más que un reflejo de la complejidad del mundo en el que vivimos y la velocidad a la que cambia.

Es por ello que hace unas décadas se crearon los sistemas de ayuda de toma de decisión (DSS). Estos permiten un marco de análisis y evaluación de las políticas de decisión metodológico y eficiente, ahorrando así esfuerzo a las personas a la hora de tomar decisiones.

Pero, aun así y a causa de la propia complejidad de las bases de conocimiento que modelizan los problemas de decisión, es necesario utilizar estructuras que sinteticen el conocimiento sin alterar el mismo, y que, a su vez necesiten menos recursos computacionales. Debido a ello se plantearon las listas KBM2L, las cuales son estructuras que recogen toda la información de las bases de conocimiento y a su vez ocupan menos tamaño que sus contra-partes. Sin embargo, esta aproximación tiene una desventaja y es que la búsqueda de la mejor KB2ML es un problema combinatorio complejo.

Se torna necesario entonces utilizar técnicas meta heurísticas como los algoritmos de estimación de distribuciones (EDAs), y que ayudan a buscar entre todas las posibles soluciones aquellas con las que obtendríamos una lista de menor tamaño mediante permutaciones de atributos y de dominio.

Por último, y debido a la demanda de las personas que toman las decisiones, se busca también que aquellos dictámenes que ofrezcan los software que toman decisiones aporten una explicación de por qué han tomado esa decisión y no otra, ya que permiten a las personas poder validar y entender el problema.

