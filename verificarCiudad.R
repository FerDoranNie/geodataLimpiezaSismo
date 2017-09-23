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
setwd("~/ReposDesarrollo/geodataLimpiezaSismo/")

geocodeQueryCheck()

# Agregar el nombre de usuario de geonames
options(geonamesUsername="ferbase10")
#options(geonamesUsername="tu User Name") #####TIENES QUE PONER TU NOMBRE DE USUARIO DE GEONAMES


#####BASE DE DATOS DEL SERVICIO POSTAL MEXICANO PARA LA CIUDAD DE MÉXICO
codigos <- read.csv("CodigosPostalesCiudadMexico.csv", header = T, 
                    stringsAsFactors = F)

codigos2 <- codigos %>% 
  select(Código.Postal, Municipio) %>% 
  data.table %>% 
  .[, Código.Postal := as.numeric(Código.Postal)]

names(codigos2)<- c("cp", "ciudad_municipio" )
# Funciones ---------------------------------------------------------------
###Usarse en casos excepcionales

codigoPostal <- function(postal){
  postal <- as.numeric(postal)
  busqueda <- GNpostalCodeLookup(postalcode=postal)
  busqueda <- busqueda %>% 
    filter(countryCode=="MX")
  print(busqueda)
  #print(busqueda$placeName)
  postal <- as.character(postal)
  postal <-ifelse(nchar(postal)<4, paste0("0", postal), postal)
  Y <-data.frame(Verificado = busqueda$adminName2,
                 cp = postal)
  return(unique(Y))
}

archivos <- list.files("verificadosTotal",
                       all.files = T, full.names = T, pattern = "*.csv",
                       recursive = T)
#archivos <- archivos[-grep("Chiapas", archivos)]


lapply(archivos, function(X){
  file <- X
  file <- gsub(".*/","", file)
  file <- paste0("verificadosVuelta2Total/", file)
  X <- read.csv(X, header = T, 
                stringsAsFactors = F)
  
  X1 <- X %>%  
    filter(ciudad_municipio_Verificado!= "Ciudad de México") %>% 
    data.frame
  
  X2 <- X %>%  
    filter(ciudad_municipio_Verificado == "Ciudad de México") 
  
  X2 <- merge(X2, codigos2, by.x="cp_Verificado", by.y="cp") %>% 
    data.table %>% 
    .[, ciudad_municipio_Verificado := ciudad_municipio ] %>% 
    select(one_of(names(X2))) %>% 
    data.frame
  X <- rbind(X1, X2)
  X <- data.frame(X)
  X %>%
   write.csv(file, row.names=F)
})
  




