# ariadnaNL

## El hilo para salir de este laberinto (COVID19 - 2020)

* Un modelo de agentes para covid19 con redes bipartitas usando NetLogo

* El objetivo es modelar la epidemia de #covid19 utilizando una red de contactos que se aproxime a las redes de contacto reales. Los individuos modelados mediante agentes tienen una casa y un trabajo, que son siempre los mismos, y constituyen su red bipartita. Ademas tienen un tiempo de viaje donde tienen la posibilidad de interaccionar con otras personas. Cuando se supera la cantidad de camas de hospitalizacion la mortalidad de los hospitalizados aumenta de acuerdo a un parámetro. 

* Los individuos tienen distintos estados que determinan la evolución de la epidemia. Los estados son:

	* **Suceptible**    : Individuo suceptible de ser infectado
	* **Latente**       : Individuo infectado que todavia no desarrolló carga viral por lo tanto no infecta a otros
	* **Presintomático**: Individuo que no tiene síntomas pero que ya tiene carga viral por lo tanto infecta
	* **Asintomático**  : Luego del período Presintomático permanece sin sintomas 
	* **Sintomático**   : Luego del período Presintomático desarrolla sintomas
	* **Hospitalizado** : Luego de un período de pre-hospitalización es hospitalizado
	* **Infectado leve**: Como tiene sintomas leves permanece en la casa
	* **Fallecido**     : Fallece luego de un período de internacion o luego del período de pre-hospitalización
	* **Recuperado**    : Se recupera de la enfermedad y no puede se contagiado nuevamente

* Los parámetros más importantes son:

	* **Beta**: es la tasa de infección/transmición, los individuos Suceptibles se infectan en su casa o en el trabajo de acuerdo a esta tasa, a la cantidad de horas que están en estos lugares y proporcionalmente a la cantidad de individuos infectados en cada uno de estos sitios. Luego durante el viaje los susceptibles se pueden infectar proporcionalmente al número global de infectados, a las horas de viaje y al beta.

	* **Proporcion de hospitalizados** 

	* **Proporción de fallecimiento de hospitalizados**

	* **Proporción de fallecimiento saturada**: la proporcion que fallece luego que los servicios medicos 
	    estan saturados

	* **Fallecido sin hospitalización**: Proporcion de fallecimiento de no hospitalizados** 

	* **Proporción de asintomáticos**

	* **Capacidad de camas**: Nro. de camas disponibles, por encima de este valor de hospitalizados cambia la proporción de fallecimiento.








