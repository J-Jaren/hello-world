#laboratorio2
#cargar librería y dataset
  library(tidyverse)
  data(starwars)
  
#FILTRAR Y SELECCIONAR DATOS
  # Seleccionar todas las columnas menos el nombre
    starwars %>% select(-name)
  #Seleccionar sólo las columnas que tienen subraya (_)
    starwars %>% select(contains("_"))
  #seleccionar sólo las columnas que empiezan con "s"
    starwars %>% select(starts_with("s"))
  #Crear un dafta frame con los nombres y planeta de origen
    homeworld = starwars %>% select(name, homeworld)
  
  #Filtrar datos
  #Filtrar por especies: sólo humano
    human = starwars %>% filter(species=="Human")
  #Filtrar por especies: sólo humanos del planeta Tatooine
    human_Tatooine = starwars %>% filter(species == "Human", homeworld == "Tatooine")
  #Crear un nuevo daraframe con todas las especies menos los droides   
    starwars_nodroids = starwars %>% filter(species != "Droid") 
      # 77 personajes no droides
    
#SELECCIONAR Y AGRUPAR DATOS
  #Usamos group_by y tally (cuenta), da una tabla según el grupo que selcciones
    starwars %>% group_by(species) %>% tally()
  # Lo mismo pero con otra variable
    starwars %>% group_by(species, gender) %>% tally()
  # Si lo quieres guardar en el enviroment recuerda asignarle un nombre
    table_gender = starwars %>% group_by(species, gender) %>% tally()
    
#CALCULAR ALGUNOS ESADISTICOS
  # na.rm=T, quiere decir que elimina los NA (valores no asignado)
    starwars %>% group_by(species) %>% summarise(mean_height = mean(height, na.rm = T), mean_mass = mean(mass, na.rm = T))
    
  # Cálculo desviación estandar
    Tabla = starwars %>% group_by(species) %>% summarise(mean_height = mean(height, na.rm = T), mean_mass = mean(mass, na.rm = T), sd_height = sd(height, na.rm = T), sd_mass = sd(mass, na.rm = T))
    
# CREAR GRÁFICOS Y MODIFICAR ALGUNOS ELEMENTOS
  # Hacer un gráfico de la altura vs. la masa de los personajes. geom_point significa que la tabla es de puntos
    ggplot(starwars, aes(height, mass)) + geom_point()
    
  #Puedes modificar el color, en geom_point
    ggplot(starwars, aes(height, mass)) + geom_point(colour = "red")
    
  #Modificando el color y el punto
    ggplot(starwars, aes(height, mass)) + geom_point(colour = "purple", pch = 3)
    
  #Modificando el color y el fondo
    ggplot(starwars, aes(height, mass)) + geom_point(colour = "red") + theme_light()
  
  #Quitar personaje masa grande, 
    # descubrir que personaje pesa más.
    masas = starwars %>% select(name, mass)
    lamasa = starwars %>% filter(mass > 1000)
    # quitar al obeso
    nojabba = starwars %>% filter(name != "Jabba Desilijic Tiure") 
    # Crear nuevo plot del nuevo data frame
    ggplot(nojabba, aes(height, mass)) + geom_point(colour = "green", pch = 3)
    
#EJERCICIOS
  #Cargamos en Rstudio el toy csv
    toy = read.csv("C:/Users/Usuario/Downloads/toy.csv")
    
  #Inspeccionar el dataset, haz un resumen de la media de las variables. Agrupado por sexo.
    media_variable_sexo = toy %>% group_by(Sex) %>% summarise(mean_masa = mean(Weight_Kg, na.rm = T ), mean_altura = mean(Height_cm, na.rm = T), mean_IMC = mean(IMC, na.rm = T), mean_IAS = mean(IAS, na.rm = T), mean_Ccintura = mean(Ccintura, na.rm = T))
    
  #Tabla con pacientes femeninos. 
    Pacientes_femeninos = toy %>% filter(Sex != "Men")
    
  #¿Cuántos resgistros cumplen las condiciones?
    #58 registros femeninos
    
  #¿De estos cuantos tienen sobrepeso?
    Pacientes_femeninos %>% filter(IMC_clas == "Overweight") %>% tally()
    # 9 personas tienen sobrepeso.
    
  #Gráfico usando ggplot realcionando IMC y Weight_Kg todos los pacientes
    ggplot(toy, aes(IMC, Weight_Kg)) + geom_point()
    
  #Gráfico filtrando los pacientes categorizados como "Overweight" y "Obesity"
    pacientes_sobrepeso = toy %>% filter(IMC_clas != "Normal")
    ggplot(pacientes_sobrepeso, aes(IMC, Weight_Kg)) + geom_point()
    

    
    
    
    