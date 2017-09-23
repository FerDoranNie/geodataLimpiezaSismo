library(RJSONIO)
library(RCurl)

source("api_key.R")
urlRev <- "https://maps.googleapis.com/maps/api/geocode/json?latlng="
getGeoData <- function(location){
  location <- gsub(' ','+',location)
  geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",
                           location,"&key=", api_key,
                           sep=""))
  raw_data_2 <- fromJSON(geo_data)
  return(raw_data_2)
}

# getGeoDataReversa <- function(latitud, longitud){
#   latitud <- as.numeric(latitud)
#   longitud <- as.numeric(longitud)
#   reversa <- getURL(paste(urlRev, latitud, ",", longitud, "&key=", api_key,
#                    sep=""))
#   
#   data <- fromJSON(reversa)
#   data <- data$results[[1]]
#   x <- data
#   direccion <- as.character(x$formatted_address)
#   numero <- as.character(x$address_components[[1]]$long_name)
#   calle <- as.character(x$address_components[[2]]$long_name)
#   cp <- tryCatch(as.character(x$address_components[[9]]$long_name),
#                  error=function(e){character(0)})
#   colonia <- as.character(x$address_components[[3]]$long_name)
#   ciudad <- as.character(x$address_components[[6]]$long_name)
#   estado <- as.character(x$address_components[[7]]$long_name)
#   pais <- tryCatch(as.character(x$address_components[[8]]$long_name),
#                    error=function(e){character(0)})
# 
#   direccion <- ifelse(identical(direccion, character(0)), NA, direccion)
#   calle <- ifelse(identical(calle, character(0)), NA, calle)
#   numero <- ifelse(identical(numero, character(0)), NA, numero)
#   cp <- ifelse(identical(cp, character(0)), NA, cp)
#   colonia <- ifelse(identical(colonia, character(0)), NA, colonia)
#   ciudad <- ifelse(identical(ciudad, character(0)), NA, ciudad)
#   estado <- ifelse(identical(estado, character(0)), NA, estado)
#   pais <- ifelse(identical(pais, character(0)), NA, pais)
#   X <- data.frame(direccion,  calle, numero, colonia, cp, 
#                   ciudad_municipio= ciudad, estado, pais, 
#                   lat = latitud, long= longitud )
#   nombres <- names(X)
#   nombres <- sapply(nombres, paste, "_", "Verificado", sep="")
#   names(X) <- nombres
#   print(paste(urlRev, latitud,",", longitud, "&key=", api_key,
#               sep=""))
#   print(c(latitud, longitud))
#   return(X)
#   }


getGeoDataReversa <- function(latitud, longitud){
  latitud <- as.numeric(latitud)
  longitud <- as.numeric(longitud)
  reversa <- getURL(paste(urlRev, latitud, ",", longitud, "&key=", api_key,
                          sep=""))
  data <- fromJSON(reversa)
  data <- data$results[[1]]
  direcciones <- data$address_components
  direccion <- lapply(1:length(direcciones), function(d){
    z <- direcciones[[d]]
    z <- unlist(z)
    z <- z[1:3]
    x <- names(z)
    y <- unname(z)
    x <- gsub("[0-9]", "", x)
    y <- data.frame(y[1], y[2], y[3])
    names(y) <- x
    return(y)
  }) %>% 
    do.call("rbind", .) %>% 
    data.frame
  dir <- as.character(data$formatted_address)
  dir <- ifelse(identical(dir, character(0)), NA, dir)
  print(class(direccion))
  print(c(latitud, longitud))
  cp     <- as.character(direccion[direccion$types=="postal_code",]$long_name)
  cp <- ifelse(identical(cp, character(0)), NA, cp)
  calle  <- as.character(direccion[direccion$types=="route",]$long_name)
  calle <- ifelse(identical(calle, character(0)), NA, calle)
  numero <- as.character(direccion[direccion$types=="street_number",]$long_name)
  numero <- ifelse(identical(numero, character(0)), NA, numero)
  colonia <- as.character(direccion[direccion$types=="political",]$long_name)
  colonia <- ifelse(identical(colonia, character(0)), NA, colonia)
  ciudad  <- as.character(direccion[direccion$types=="locality",]$long_name)
  ciudad <- ifelse(identical(ciudad, character(0)), NA, ciudad)
  estado  <- as.character(direccion[direccion$types=="administrative_area_level_1",]$long_name)
  estado <- ifelse(identical(estado, character(0)), NA, estado)
  pais   <- as.character(direccion[direccion$types=="country",]$long_name)
  pais <- ifelse(identical(pais, character(0)), NA, pais)
  X <- data.frame(direccion = dir,  calle, numero, colonia, cp,
                  ciudad_municipio= ciudad, estado, pais,
                  lat = latitud, long= longitud )

  X[X=="integer(0)"]<- NA
  # for(i in 1:length(direcciones)){
  #   # postal_code <- route <- NULL
  #   z <- direcciones[[i]]
  #   z <- unlist(z)
  #   z <- z[1:3]
  #   x <- names(z)
  #   y <- unname(z)
  #   x <- gsub("[0-9]", "", x)
  #   y <- (rbind(x,y))
  #   print(y)
  #   
  # }
  nombres <- names(X)
  nombres <- sapply(nombres, paste, "_", "Verificado", sep="")
  names(X) <- nombres
  return(X)
  }

.contadorQueries <- 0
trace(getGeoDataReversa, 
      tracer=function() .contadorQueries<<- .contadorQueries+1)



