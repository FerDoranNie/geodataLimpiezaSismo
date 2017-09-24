library(magrittr)

c("data.table", "ggmap", "dplyr", "tidyr", 
  "lubridate", "geonames", "sp", "spatial", 
  "XML", "rgdal") %>% 
  sapply(require, character.only=T)

setwd("~/ReposDesarrollo/geodataLimpiezaSismo/")

archivo <- "edificioColapsado/Mapeo Colaborativo 19s  (DATOS NECESITAN VERIFICAR).kml"

temporal <- "Mapeo Colaborativo 19s  (DATOS NECESITAN VERIFICAR).zip"


kml_To_Csv <- function(kml){
  capasLayer <- ogrListLayers(kml)
  print(capasLayer)
  if(length(capasLayer)>1){
    lapply(capasLayer, function(capa){
      archivo <- paste0(capa, ".csv")
      data <- readOGR(kml, layer = capa )
      print(ogrInfo(kml, capa))
      data <- as.data.frame(data)
      data %>% 
        write.csv(archivo, row.names=F,
                  fileEncoding = "utf-8")
    })
  }else{
    archivo <- paste0(capasLayer, ".csv")
    data <- readOGR(kml, layer = capasLayer)
    data <- as.data.frame(data)
    data %>% 
      write.csv(archivo, row.names=F,
                fileEncoding = "utf-8")
    
  }
}
setwd("~/test/")
test <- ogrListLayers("Mapeo Colaborativo 19s  (DATOS NECESITAN VERIFICAR).kml")
readOGR("Mapeo Colaborativo 19s  (DATOS NECESITAN VERIFICAR).kml",
        # test[4]
        "Daños Colaborativos",encoding = "utf-8", use_iconv = T
        
        ) %>% head


