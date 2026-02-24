#'Buffer GIS Tools
#'\code(sig_buffer)
#'
#'@param capa_entrada
#'@param distancia
#'@param dissolve
#'
#'
#'
#'
#'
#'
#'

#Crear una versión de la herramienta buffer

#la herramienta depende de los siguientes paquetes
library(sf)
library(dplyr)

# toma una capa vectorial de entrada
# requiere de especificar una distancia de vecindad
# Opciones:
#  - Fusionar o no geometrías
#  - Fusionar en función de un atributo
#  - Mantener atributos en la capa resultante

parcelas <- read_sf("C:/Users/AlumnoMaster/Desktop/Programacion/parcelas") |>
  mutate(Especie=1)
parcelas_buffer <- st_buffer(parcelas, dist=10000)|>
  #group_by(Especie) |>
  summarise(geometry = st_union(geometry))

sig_buffer <- function(capa_entrada, distancia, dissolve=TRUE){
  if(dissolve==TRUE){
    buffer <- st_buffer(capa_entrada, dist=distancia)|>
      summarise(geometry = st_union(geometry))
  }else{
    buffer <- st_buffer(capa_entrada, dist =distancia)
  }
  return(buffer)
}

parcelas_buffer_function <-sig_buffer(parcelas, 5000, dissolve =FALSE)
plot(parcelas_buffer_function)
