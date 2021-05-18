#Importación del archivo
datos <- readRDS("/Users/user/Desktop/covid.rds")
str(datos)

# Pregunta 1.1

# 1a) (1pt) ¿Qué clase de objetos es casos? 
# Son listas de variables númericas (Interger)
class(covid)
typeof(covid) 
mode(covid)

# 1b) (1pt) ¿Cuántos elementos posee este objeto? 
# 5 Elementos
length(covid)

# 1c) (1pt) ¿Cuáles son los nombres de este objeto?
# "abr" "jul" "jun" "mar" "may"
ls(covid)

#1d) (2pt) ¿Qué clase de objeto está almacenado en el slot correspondiente al mes de junio ("jun") y cuántos elementos posee?
#  Es un objeto lista con 15 elementos
class(covid[["jun"]])
length(covid[["jun"]])
typeof(covid[["jun"]])

# 1e) (2pt) Para el mes de junio, ¿a qué clase de objeto corresponde casos_totales y cuántos elementos posee?
# Para el mes de junio y casos totales el objeto correspone a una lista que posee 2 elementos

class(covid[["jun"]][["casos_totales"]])
typeof(covid[["jun"]][["casos_totales"]]) 
mode(covid[["jun"]][["casos_totales"]])
length(covid[["jun"]][["casos_totales"]])

#Pregunta 1.2

# 2a) (2pts) ¿Cuántos casos activos existían al 25 de marzo? (indique claramente la expresión de R para obtener el valor solicitado) .
# Existían 1117 hasta el 25 de marzo
a<- filter(datos[["mar"]][["casos_activos"]], dia_del_mes == 25)

# 2b) (1pts) Genere cinco nuevas variables llamadas totales_marzo,totales_abril , totales_mayo, totales_junio y totales_julio, que contengan los dataframes correspondientes al slot casos_totales del mes respectivo, (por ejemplo, la variable totales_julio debe contener una dataframe de dimensión  8×2 .
# 
meses <- c("marzo", "abril", "mayo", "junio", "julio")
names(datos) <- meses
lapply(meses, function(x) assign(paste("totales", x ,sep = "_"),
                                 datos[[x]][["casos_totales"]],.GlobalEnv))
dim(totales_julio)
ls()

# 2c) (2pts) Para cada dataframe del paso anterior, genere una nueva columna llamada mes, que repita el nombre del mes correspondiente por cada fila.

totales_marzo <- mutate(totales_marzo  , mes = "marzo")
totales_abril <- mutate(totales_abril  , mes = "abril")
totales_mayo  <- mutate(totales_mayo , mes = "mayo")
totales_junio <- mutate(totales_junio , mes = "junio")
totales_julio <- mutate(totales_julio , mes = "julio")
head(totales_marzo)

# # 2d) (3pts) En cada data.frame, agregue una nueva columna llamada casos_diarios que contanga la información correspontiende al total de casos nuevos diarios(casos_nuevos_totales).

totales_marzo <- mutate( totales_marzo , casos_nuevos_totales = total - lag(total))
totales_abril <- mutate( totales_abril , casos_nuevos_totales = total - lag(total))
totales_mayo <- mutate( totales_mayo , casos_nuevos_totales = total - lag(total))
totales_junio <- mutate( totales_junio , casos_nuevos_totales = total - lag(total))
totales_julio <- mutate( totales_julio , casos_nuevos_totales = total - lag(total))
head(totales_marzo)

# Pregunta 1.3

# 3a) (1pts) Con la ayuda de la función rbind() , una las filas de las 5 tablas generadas en la pregunta anterior y guarde la tabla resultante en una variable llamada casos_hist.

casos_hist <- rbind(
  totales_marzo,
  totales_abril,
  totales_mayo,
  totales_junio,
  totales_julio)
head(casos_hist)

sum(is.na(casos_hist$casos_nuevos_totales))
casos_hist <- mutate(casos_hist, casos_nuevos_totales = total - lag(total))
sum(is.na(casos_hist$casos_nuevos_totales))

# 3b) (1pt) En promedio, ¿Cuántos casos hay diariamente desde el 03 de marzo hasta el 08 de julio?
# El promedio es 2367.828 entre el 03 de marzo y el 08 de julio
mean(casos_hist$casos_nuevos_totales, na.rm = T)

#3c) (2pts) En promedio, ¿Cuántos casos hay diariamente en cada mes ?.
# En Marzo 94
# En Abril 448
# En Mayo 2756
# En Junio 6008
# En Julio 3005

mean(totales_marzo$casos_nuevos_totales, na.rm = T)
mean(totales_abril$casos_nuevos_totales, na.rm = T)
mean(totales_mayo$casos_nuevos_totales, na.rm = T)
mean(totales_junio$casos_nuevos_totales, na.rm = T)
mean(totales_julio$casos_nuevos_totales, na.rm = T)


#Importación de los nuevos datos 

library(tidyverse)
mujeres <- readRDS("/Users/user/Desktop/temp_m.rds")
hombres <- readRDS("/Users/user/Desktop/temp_h.rds")

# Pregunta 2.2

# P1) (4pts) A partir de los dos vectores cargados temp_h y temp_m ustred deberá crear un data.frame llamado casos con tres columnas y un total de 1230 filas. Las columnas deben ser las siguientes; una con el id del registro, otra con el género y otra con la temperatura registrada. A modo de ejemplo, a continuación se muestra como debería quedar el data.frame:

df_hombres <- data.frame("temperatura" = hombres) %>% 
  rownames_to_column() %>% 
  rename(id = rowname) %>%  
  mutate(genero = "hombre") 


df_mujeres <- data.frame("temperatura" = mujeres) %>% 
  rownames_to_column() %>% 
  rename(id = rowname) %>% 
  mutate(genero = "mujer")

casos <- rbind(df_hombres, df_mujeres)
head(casos)

# Importación de los nuevos datos 
sintomas <- readRDS("/Users/user/Desktop/sintomas.rds")

#2b) (1pt) ¿Cuál es la clase de este objeto?.
# es una "matrix" "array" 
class(sintomas)

# 2c) (1pt) Note que cada fila undica con un 1 si la persona presentó el respectivo síntoma y 0 si no. ¿Qué sintomas presentó la persona ubicada en el registro 450?.
# Es un hombre que presenta tos, dolor de cabeza, perdida de olfato y dolor de pecho 
rownames(sintomas)[450]
sintomas[450,]

# 2d) (2pts) ¿Qué síntomas y qué temperatura presentó la persona con id "h_537"?. Si es le es de utilidad puede utilizar la función rownames(), la cual permite obtener los nombres (id) de las filas de una matriz.sintomas[rownames(sintomas) == "h_537",]
# El hombre tiene 36,6 de temperatura y presenta dolor de pecho
sintomas[rownames(sintomas) == "h_537",]

# Pregunta 2.3

#3a) (3pt) ¿Cuántos registros presentaron exactamente los 4 síntomas?
# Son 229 con todos los síntomas.

sintomas_df <- data.frame(sintomas) 
sintomas_df %>% 
  mutate(todos_los_sintomas = rowSums( select(., starts_with("tiene")))/ 4 ) %>% 
  filter(todos_los_sintomas == 1) %>% 
  count(todos_los_sintomas)

# 3b) (1pt) ¿Cúantas personas presentaron tos? 
#  608 personas presentaron tos 

sintomas_df %>% 
  filter(tiene_tos==1) %>% 
  count()

# 3c) (2pt) ¿Cuál es el síntoma más frecuente?
#El sintomas mas frecuente es el dolor de cabeza con casi el 30 %

sintomas_df %>% 
  select(-temperatura) %>% 
  colSums()%>% 
  data.frame() %>% 
  arrange(desc(.)) %>% 
  cbind(prop.table(.))

# Pregunta 2.4

# 4a) (1pt) La función as.data.frame() permite coercionar un objeto de otra clase a data.frame. cree una variable llamada sintomas_df con la información de sintomas pero como data.frame.
sintomas_df <- as.data.frame(sintomas)
head(sintomas)


# 4b) (1pts) Se puede observar que los ids solo están disponibles en los nombres de las filas. Sin embargo esútil tener esta información como una columna más de la tabla. Para ello cree una nueva columna en la tabla sintomas_df llamada id que contenga los ids de cada registro.
sintomas_df <- sintomas_df %>% 
  rownames_to_column("id")
head(sintomas_df)

# 4c) (2pts) ¿Cuántas personas sólo presentaron tos y dolor de cabeza?
# 494 presentaron tos y dolor de cabeza
sintomas_df %>% filter(tiene_tos == 1 & tiene_dolor_cabeza == 1) %>% 
  count()

# 1pt) Se sabe que una persona tiene fiebre cuando su temperatura es mayor o igual a los 37.2°. Cree una nueva columna en la tabla sintomas_df llamada tiene_fiebre que contenga TRUE cuando tiene fiebre y FALSE en caso contrario.
# 580 presentaron fiebre
sintomas_df <- sintomas_df %>% 
  mutate(tiene_fiebre = if_else(temperatura >= 37.2 , TRUE ,FALSE) ) 
count(sintomas_df,tiene_fiebre)
head(sintomas_df)



