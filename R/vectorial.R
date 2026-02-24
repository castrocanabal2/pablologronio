#' Buffer GIS tools
#'
#' \code{sig_buffer} devuelve la zona de influencia entorno a las geometrias de un shapefile
#'
#' @param capa_entrada un objeto de tipo vectorial de la clase sf
#' @param distancia Distancia de vecindad para determinar la zona de influencia en m
#' @param dissolve Parámetro para controlar la fusión de geometrias.
#' @param campo Parámetro para fusionar por un atributo. Opcional.
#'
#' @return Un objeto vectorial de la case sfc_POLYGON
#'
#' @examples
#' \dontrun
#' sig_buffer(capa_entrada,1000)
#' sig_buffer(capa_entrada,1000, dissolve = TRUE)
#' sig_buffer(capa_entrada,1000, dissolve = TRUE, 'Especie')


sig_buffer <- function(capa_entrada,distancia, dissolve = TRUE){

  if(dissolve==TRUE){
    buffer <-st_buffer(capa_entrada, dist= distancia) |>
      summarise(geometry = st_union(geometry))

    if(exists(campo)){
      buffer <- st_buffer(capa_entrada, dist = distancia)|>
        group_by({{campo}}) |>
        summarise(geometry = st_union(geometry))
    }


  }else{
    buffer <- st_buffer(capa_entrada, dist= distancia)
  }

  return(buffer)
}


