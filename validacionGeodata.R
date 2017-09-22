####################################
#Creado por Fernando Dorantes Nieto
#                                   <(°) 
#                                     ( >)"
#                                      /|
####################################


library(magrittr)
c("data.table", "ggmap", "dplyr", "tidyr", 
  "lubridate", "geonames", "readxl") %>% 
  sapply(require, character.only=T)


setwd("~/ReposDesarollo/csvGeoData/")

geocodeQueryCheck()

# Agregar el nombre de usuario de geonames
options(geonamesUsername="ferbase10")



# Funciones ---------------------------------------------------------------
Localidad <- function(longitud, latitud){
  if(is.na(longitud) | is.na(latitud)){
    return(NULL)
  }
  x <- tryCatch(
     revgeocode(c(longitud, latitud), output = "more"),
     error = function(e){return(NULL)}
   )
  if(is.null(x)){
    return(NULL)
  }
  x <- revgeocode(c(longitud, latitud), output = "more")
  direccion <- as.character(x$address)
  calle <- as.character(x$route)
  numero <- as.character(x$street_number)
  cp <- as.character(x$postal_code)
  colonia <- as.character(x$political)
  ciudad <- as.character(x$locality)
  estado <- as.character(x$administrative_area_level_1)
  pais <- as.character(x$country)
  
  direccion <- ifelse(identical(direccion, character(0)), NA, direccion)
  calle <- ifelse(identical(calle, character(0)), NA, calle)
  numero <- ifelse(identical(numero, character(0)), NA, numero)
  cp <- ifelse(identical(cp, character(0)), NA, cp)
  colonia <- ifelse(identical(colonia, character(0)), NA, colonia)
  ciudad <- ifelse(identical(ciudad, character(0)), NA, ciudad)
  estado <- ifelse(identical(estado, character(0)), NA, estado)
  pais <- ifelse(identical(pais, character(0)), NA, pais)
  X <- data.frame(direccion,  calle, numero, colonia, cp, 
                  ciudad_municipio= ciudad, estado, pais, 
                  lat = latitud, long= longitud )
  nombres <- names(X)
  nombres <- sapply(nombres, paste, "_", "Verificado", sep="")
  names(X) <- nombres
  return(X)
}



# Datos -------------------------------------------------------------------

acopio <- read.csv("acopio/acopio/Centros-de-Acopio.csv",
         header = T, stringsAsFactors = F) 

acopio <- acopio %>% 
  data.table %>% 
  .[, coordenadas := paste(X, Y, sep=", ")] 


acopioLocalidad <- mapply("Localidad", longitud= acopio$X, latitud = acopio$Y,
                         SIMPLIFY = F)                       

acopioLocalidad2 <- do.call("rbind", acopioLocalidad) 

acopioLocalidad2 <- acopioLocalidad2 %>% 
  data.table %>% 
  .[, coordenadas := paste(long_Verificado, lat_Verificado, sep=", ")] 


acopioGeneral <- merge(data.frame(acopio), 
                       data.frame(acopioLocalidad2), by="coordenadas")


acopioGeneral %>% 
  write.csv("acopio/acopio/acopioVerificado.csv", row.names=F)

refugios <- read.csv("refugios/refugios/Central-Mexico-Earthquake-Shelters.csv",
                   header = T, stringsAsFactors = F) 


refugios <- refugios %>% 
  data.table %>% 
  .[, coordenadas := paste(X, Y, sep=", ")] 


refugiosLocalidad <- mapply("Localidad", longitud= refugios$X, 
                            latitud = refugios$Y,
                          SIMPLIFY = F)                       


refugiosLocalidad2 <- do.call("rbind", refugiosLocalidad) 

refugiosLocalidad2 <- refugiosLocalidad2 %>% 
  data.table %>% 
  .[, coordenadas := paste(long_Verificado, lat_Verificado, sep=", ")] 


refugiosGeneral <- merge(data.frame(refugios), 
                       data.frame(refugiosLocalidad2), by="coordenadas")

refugiosGeneral %>%  
  write.csv("refugios/refugios/refugiosVerificado.csv", row.names=F)



archivos <- list.files("edificioColapsado/colaborativo/Mapeo-Colaborativo-19s-DATOS-NECESITAN-VERIFICAR-/",
           all.files = T, full.names = T, pattern = "*.csv")


lapply(archivos, function(file){
  x <- read.csv(file, stringsAsFactors = F)
  # archivo <- (gsub(".*//","", file)
  archivo <- gsub(".csv","", file, fixed=T)
  archivo <- paste0(archivo,  "_Verificado", ".csv")
  x <- x %>%
    filter(X!="" | !is.na(X)) %>%
    data.table %>%
    .[, coordenadas := paste(X, Y, sep=", ")]

  xLocalidad <- mapply("Localidad", longitud= x$X,
                              latitud = x$Y,
                              SIMPLIFY = F)
  xLocalidad2 <- do.call("rbind", xLocalidad)

  xLocalidad2 <- xLocalidad2 %>%
    data.table %>%
    .[, coordenadas := paste(long_Verificado, lat_Verificado, sep=", ")]


  xGeneral <- merge(data.frame(x),
                           data.frame(xLocalidad2), by="coordenadas")
  xGeneral %>%
    write.csv(archivo, row.names=F)
  print(file)
})


manos <- read.csv("manos_Obra/formResponses.csv", header = T)

manos <- manos %>% 
  data.table %>% 
  .[, coordenadas := paste(Longitude, Latitude, sep=", ")] 


manosLocalidad <- mapply("Localidad", longitud= manos$Longitude, 
                            latitud = manos$Latitude,
                            SIMPLIFY = F)                       


manosLocalidad2 <- do.call("rbind", manosLocalidad) 

manosLocalidad2 <- manosLocalidad2 %>% 
  data.table %>% 
  .[, coordenadas := paste(long_Verificado, lat_Verificado, sep=", ")] 


manosGeneral <- merge(data.frame(manos), 
                         data.frame(manosLocalidad2), by="coordenadas")

manosGeneral %>%  
  write.csv("manos_Obra/formResponses_Verificado.csv", row.names=F)



chiapas <- list.files("chiapas/areasCriticas/Ubicacion-de-areas-criticas-CHIAPAS-Sismo-07092017/",
                       all.files = T, full.names = T, pattern = "*.csv",
                      recursive = T)


lapply(chiapas, function(file){
  x <- read.csv(file, stringsAsFactors = F)
  # archivo <- (gsub(".*//","", file)
  archivo <- gsub(".csv","", file, fixed=T)
  archivo <- paste0(archivo,  "_Verificado", ".csv")
  x <- x %>%
    filter(X!="" | !is.na(X)) %>%
    data.table %>%
    .[, coordenadas := paste(X, Y, sep=", ")]
  
  xLocalidad <- mapply("Localidad", longitud= x$X,
                       latitud = x$Y,
                       SIMPLIFY = F)
  xLocalidad2 <- do.call("rbind", xLocalidad)
  
  xLocalidad2 <- xLocalidad2 %>%
    data.table %>%
    .[, coordenadas := paste(long_Verificado, lat_Verificado, sep=", ")]
  
  
  xGeneral <- merge(data.frame(x),
                    data.frame(xLocalidad2), by="coordenadas")
  xGeneral %>%
    write.csv(archivo, row.names=F)
  print(file)
})




