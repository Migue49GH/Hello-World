library(tidyverse)
data(starwars)
#angela prado y miguel cornejo

#Filtrar y seleccionar datos:

#Ejemplos usando select
# Seleccionar todas las columnas menos el nombre --- %>% select (- ) selecciona columnas y - invierte la busqueda
starwars %>% select(-name)
#Seleccionar sólo las columnas que tienen subraya (_)
starwars %>% select(contains("_"))
#Seleccionar sólo las columnas que empiezan con "s"
starwars %>% select(starts_with("s"))
#Crear un data frame con los nombres y planeta de origen (homeworld)
homeworld <- starwars %>% select(name, homeworld)
#Filtrar datos 
#Filtrar por especies: sólo humanos ---- pq habia dos =?
human <- starwars %>% filter(species == "Human")
#Filtrar por especies: sólo humanos del planeta Tatooine
starwars %>% filter(species == "Human", homeworld == "Tatooine")
#Crear un nuevo datframe con todas las especies menos los Droides ---- ! antes de = para excluir
starwars_nodroids <- starwars %>% filter(species != "Droid")

#---PREGUNTA 1---¿Cuántos registros cumplen las condiciones finales? Vemos que la variable starwars_ndroids tiene 77 objetos mientras que el starwars original tenia 87


#SELECCIONAR Y AGRUPAR

#Usamos group_by y tally (2ndo muestra todos)
starwars %>% group_by(species) %>% tally()
print(starwars %>% group_by(species) %>% tally(), n = 38)

#Añadiendo otra variable
starwars %>% group_by(species, gender) %>% tally()

#Si lo quieres guardar en el environment recuerda asignarle un nombre
table_gender <- starwars %>% group_by(species, gender) %>% tally()


#Calcular algunos estadísticos

starwars %>% group_by(species) %>% summarise(mean_height = mean(height, na.rm = T),mean_mass = mean(mass,na.rm = T))

#-----PREGUNTA 2----- ¿Cómo calcularías la desviación estándar (sd) de esos parámetros? Recuerda consultar con ? si no sabes como usar una función o comando. Por ejemplo: ?summarise() ,?sd().

starwars %>% group_by(species) %>% summarise(sd_height = sd(height, na.rm = T),sd_mass = sd(mass,na.rm = T))


#Crear gráficos y modificar algunos elementos.

#Hacer un gráfico de la altura vs. la masa de los personajes
ggplot(starwars, aes(height, mass)) + geom_point()

#Puedes modificar el color 
ggplot(starwars, aes(height, mass)) + geom_point(colour = "red")

#Modificando el color y el punto
ggplot(starwars, aes(height, mass)) + geom_point(colour = "purple", pch = 3)

#Modificando el color y el fondo 
ggplot(starwars, aes(height, mass)) + geom_point(colour = "lightblue") + theme_dark()

#3-----PREGUNTA 3-----Al crear los gráficos puedes observar que hay un punto que corresponde a un personaje con una masa muy grande. Inspecciona el datset, filtra usando las funciones de tidyverse, crea un nuevo dataframe sin ese personaje y crea de nuevo el gráfico final. (Exporta el gráfico con la opción exportar en el panel derecho y adjúntalo en el pdf)
starwars_nofats = starwars %>% filter (species != "Hutt")
ggplot(starwars_nofats, aes(height, mass)) + geom_point(colour = "lightblue") + theme_dark()


toy <- read_csv("C:/Users/migue/Downloads/toy.csv")

table_gender <- starwars %>% group_by(gender) %>% tally()

#-----PREGUNTA 4----evercicio toy.csv:

#-Inspecciona el dataset, haz un resumen de la media (mean) de las variables (Peso, Altura,IMC, IAS, CCintura). Agrupando por sexo.
toy %>% group_by(Sex) %>% summarise(mean_Heightcm = mean(Height_cm, na.rm = T),mean_Weight_Kg = mean(Weight_Kg,na.rm = T),mean_IMC = mean(IMC,na.rm = T),mean_IAS = mean(IAS,na.rm = T),mean_Ccintura = mean(Ccintura,na.rm = T))
#Peso, Altura,IMC, IAS, CCintura
#-Haz una tabla sólo con los pacientes femeninos ¿Cuántos registros cumplen las condiciones? ¿De estos cuantos tienen Sobrepeso (Overweight)?  Usa select y filter.
Women_table = toy %>% filter(Sex == "Women") #hay 58 registros 
Women_table_OW = toy %>% filter(Sex == "Women") %>% filter(IMC_clas == "Overweight") # hay 9
#-Haz un gráfico usando ggplot relacionando el IMC (Indice de masa corporal) con el peso (Weight_Kg) de todos los pacientes.
#--Repítelo filtrando sólo los pacientes categorizados como "Overweight" y "Obesity".
ggplot(toy, aes(IMC, Weight_Kg)) + geom_point(colour = "lightblue") + theme_dark()
ggplot(toy %>% filter(IMC_clas == c("Overweight", "Obesity")), aes(IMC, Weight_Kg)) + geom_point(colour = "lightblue") + theme_dark()

install.packages("ape")
install.packages("phangorn")
install.packages("phytools")


