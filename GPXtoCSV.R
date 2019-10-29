GPXtoCSV <- function(direccion){
  # librerias ----
  library('curl')    # Para descargar archivos
  library('readxl')  # Para leer Hojas de Excel 
  library('tidyr')   # Procesar data 
  library('dplyr')   # mo data proce
  library('forcats') # mo mo data processing
  library('ggplot2') # para graficar
  library("sf")      # para datos espaciales
  library("sp")      # para datos espaciales
  library("rgdal")    # para datos espaciales
  
  # almacena los datos en un objeto ---- 
  datos <- sf::st_read(direccion, layer = "track_points") #lectura de datos sf
  datos1 <- rgdal::readOGR(direccion, layer = "track_points") #lectura de datos 
  
  # Elevacion msnm----
  elevacion <- datos$ele
  # Fecha- Hora ----
  fecha_hora <- datos$time
  
  # Proyeccion a UTM 21S ----
  Proy <- spTransform(datos1, CRS("+proj=utm +zone=21 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) #Proyeccion de las coordenadas a UTM 21S
  latlon <- Proy@coords # Almacena los objetos en un objeto latlon
  lon_utm <- latlon[,1] # Guarda la longitud reproyectada en un objeto 
  lat_utm <- latlon[,2] # Guarda la latitud reproyectada en un objeto 
  
  # Tiempo GPS ----
  tiempo_gps <- data.frame() # data frame vacio
  for(i in 1:length(datos$time)){
    tiempo_gps[1,1] <- 0  
    tiempo_gps[i,1] <- difftime(datos$time[i+1], datos$time[i], units = "secs")  
  }
  colnames(tiempo_gps) <- "tiempo_GPS" # renombro la columna
  
  # hora local ajustada ----
  hrs <- function(u) { #Funcion para calcular hs
    x <- u * 3600
    return(x)
  }
  horal <- datos$time + hrs(3) #suma la hs mas 3 
  horal <- format(as.POSIXct(horal,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
  # Extrae por formato la hora y almacena en un objeto 
  
  # Calculo de la distancia ----
  distancia <- data.frame() #crea un data frame vacio
  for(i in 1:(length(lat_utm)-1)){
    distancia[i+1,1] <- sqrt(((lat_utm[i]-lat_utm[i+1])^2)+((lon_utm[i]-lon_utm[i+1])^2)) # loop para calcular la distancia euclidea entre coordenadas 
  } 
  colnames(distancia) <- "distancia" # Nombre de columna
  
  # Genera un data frame de salida ----
  matriz <- cbind(elevacion,fecha_hora,lat_geo,lon_geo,lat_utm,lon_utm,tiempo_gps, hora, horal, distancia)
  
  return(matriz)
  }