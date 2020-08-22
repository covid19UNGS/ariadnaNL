
# ariadnaNL

## El hilo para salir de este laberinto (COVID19 - 2020)

## Descripción del modelo

  - Un modelo de agentes para covid19 con redes bipartitas usando
    NetLogo

  - El objetivo es modelar la epidemia de \#covid19 utilizando una red
    de contactos que se aproxime a las redes de contacto reales. Los
    individuos modelados mediante agentes tienen una casa y un trabajo,
    que son siempre los mismos, y constituyen su red bipartita. Ademas
    tienen un tiempo de viaje donde tienen la posibilidad de
    interaccionar con otras personas. Cuando se supera la cantidad de
    camas de hospitalizacion la mortalidad de los hospitalizados aumenta
    de acuerdo a un parámetro.

  - Los individuos tienen distintos estados que determinan la evolución
    de la epidemia. Los estados son:
    
      - **Susceptible** : Individuo susceptible de ser infectado
      - **Latente** : Individuo infectado que todavía no desarrolló
        carga viral por lo tanto no infecta a otros
      - **Presintomático**: Individuo que no tiene síntomas pero que ya
        tiene carga viral por lo tanto infecta
      - **Asintomático** : Luego del período Presintomático permanece
        sin síntomas
      - **Sintomático** : Luego del período Presintomático desarrolla
        síntomas
      - **Hospitalizado** : Luego de un período de pre-hospitalización
        es hospitalizado
      - **Infectado leve**: Como tiene síntomas leves permanece en la
        casa
      - **Fallecido** : Fallece luego de un período de internación o
        luego del período de pre-hospitalización
      - **Recuperado** : Se recupera de la enfermedad y no puede se
        contagiado nuevamente

  - Los parámetros más importantes son:
    
      - **Beta**: es la tasa de infección/transmisión, los individuos
        Susceptibles se infectan en su casa o en el trabajo de acuerdo a
        esta tasa, a la cantidad de horas que están en estos lugares y
        proporcionalmente a la cantidad de individuos infectados en cada
        uno de estos sitios. Luego durante el viaje los susceptibles se
        pueden infectar proporcionalmente al número global de
        infectados, a las horas de viaje y al beta.
    
      - **Cantidad de horas en el trabajo**
    
      - **Cantidad de horas en viaje**
        
          - **Proporción de hospitalizados**
        
          - **Proporción de fallecimiento de hospitalizados**
        
          - **Fallecido sin hospitalización**: Proporción de
            fallecimiento de no hospitalizados
        
          - **Proporción de asintomáticos**
        
          - **Capacidad de camas**: Nro. de camas disponibles, por
            encima de este valor de hospitalizados cambia la proporción
            de fallecimiento.
        
          - **Proporción de fallecimiento saturada**: la proporción que
            fallece luego que los servicios médicos están saturados

## Escenarios y ajustes

  - A partir de bibliografía y de los datos abiertos del ministerio de
    salud de la Nación Argentina, llamados datos SISA, que se pueden
    descargar de
    <https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv>
    se estimaron la mayoría de los parametros excepto **beta**,
    **cantidad de horas de trabajo**, **cantidad de horas en viaje** que
    se ajustaron a los datos de la Ciudad de Buenos Aires y también a la
    Provincia de Buenos Aires.

  - Para el ajuste se tomaron distintos períodos asumiendo que las
    distintas intervenciones (Cuarentena, aislamiento social, etc.)
    modificaron los parámetros, y que la respuesta de la población fue
    variando durante la pandemia. Y se utilizó el número de fallecidos
    por considerase este el dato más confiable.

  - En el primer perído de ajuste entre 0-33 Dias se asumio que la
    diamica de la epidemia era la mas parecida a la natural/libre sin
    por lo tanto se tomaron los parametros ajustados maximizando la
    cantidad de horas en viaje y horas en trabajo

  - En los siguiente períodos se ajusto utilizando la restriccion de
    numero de camas ocupadas en una fecha donde esta en información
    estuviera disponible, ya que no se puede estimar a partir de los
    datos abiertos SISA.

### Simulaciones con Parametros ajustados segun datos SISA al 11/08 para CABA

  - Parametros Ajustados con proporción de fallecidos de hospitalizados
    = 0.0925 proporcion hospitalizados = 0.19 y proporcion de fallecidos
    sin hospitalizacion = 0.005

| periodo\_fit | beta | Horas\_en\_viaje | Horas\_en\_trabajo |
| :----------- | ---: | ---------------: | -----------------: |
| 0-33         | 0.37 |             2.06 |               6.20 |
| 33-63        | 0.45 |             1.40 |               3.57 |
| 63-93        | 0.42 |             2.92 |               2.13 |
| 93-121       | 0.46 |             0.43 |               5.03 |
| 121-139      | 0.41 |             2.74 |               2.11 |
| 139-170      | 0.41 |             2.74 |               2.11 |

![](Figures/Ajuste_CABA%20121-139_Fallecidos_log.pdf.png)
![](Figures/Ajuste_CABA%20121-139_Hospitalizados.pdf.png)

### Escenarios utilizando los parametros ajustados para 121-139 Días

| Escenario | beta | Horas en viaje | Horas en trabajo | proporción hospitalizados | Proporción fallecimiento hospitalizados |
| :-------: | ---: | -------------: | ---------------: | ------------------------: | --------------------------------------: |
|     1     | 0.41 |            2.4 |              7.5 |                      0.19 |                                   0.092 |
|     2     | 0.41 |            2.0 |             5.00 |                      0.19 |                                   0.092 |
|     3     | 0.41 |           2.74 |             2.11 |                      0.19 |                                   0.092 |
|     4     | 0.41 |           1.00 |             2.00 |                      0.19 |                                   0.092 |

![](Figures/escenariosCABA_Fallecidos_log.pdf.png)

![](Figures/escenariosCABA_Fallecidos.pdf.png)

![](Figures/escenariosCABA_Hospitalizados.pdf.png)

  - Para comparación al dia 21/08 la cantidad de camas ocupadas total en
    CABA = 2691 = 1697+713+281 (Leves+moderados+graves)

![](Figures/escenariosCABA_Uti.pdf.png)

![](Figures/escenariosCABA_Sintomaticos.pdf.png)

![](Figures/escenariosCABA_Sintomaticos_dia.pdf.png)

## Resultados y discusión

  - Observando los ajustes y escenarios de CABA
    
      - A partir de los 121 días (01/07/2020) no hay cambios en los
        parámetros ajustados
    
      - Como las medidas de aislamiento van modificando la dinámica de
        la epidemia es muy poco probable que un ajuste con un set de
        parámetros fijos pueda predecir con certeza el desarrollo de la
        misma.
    
      - En los escenarios se observa que aunque los fallecimientos
        ajustan razonablemente bien, la cantidad de casos diarios y
        hospitalizados es mucho menor, con lo cual probablemente haya
        variaciones en los parámetros de fallecidos y hospitalizados
