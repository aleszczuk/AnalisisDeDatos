library('curl')    # Para descargar archivos
library('readxl')  # Para leer Hojas de Excel 
library('tidyr')   # Procesar data 
library('dplyr')   # mo data proce
library('forcats') # mo mo data processing
library('ggplot2') # para graficar
library("sf")      # para datos espaciales
library("sp")      # para datos espaciales
library("rgdal")    # para datos espaciales

#Direccion del archivo GPX

dir <- "C:/Users/Lechu/Desktop/DiscoPosMorten/Investigacion/0-Propia/TESIS_DOCTORAL/AnalsisDatosFW/DatosSelvaSRL/ProcesamientoVIdeo/Track_2019-10-09 164842.gpx"


datos <- st_read(dir, layer = "track_points") #lectura de datos


#fecha <- as.Date(data$time) #Funciona pero lo de abajo tambien
fecha <- as.Date(datos$time)
hora <- format(as.POSIXct(datos$time,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
lat <- datos$geometry

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- do.call(rbind,sf::st_geometry(x))
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

Transformacion <- sfc_as_cols(datos, names = c("X","Y"))
lon_geo <- Transformacion$X
lat_geo <- Transformacion$Y

#Proyectar a UTM
datos1 <- rgdal::readOGR(dir, layer = "track_points")
Proy <- spTransform(datos1, CRS("+proj=utm +zone=21 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) #Proyeccion de las coordenadas a UTM 21S
latlon <- Proy@coords # posee los objetos como 
lon_utm <- latlon[,1] # Guarda la longitud reproyectada en un objeto 
lat_utm <- latlon[,2] # Guarda la latitud reproyectada en un objeto 

# Hora actual 
a <- data.frame()
for(i in 1:length(datos$time)){
a[1,1] <- 0  
a[i,1] <- difftime(datos$time[i+1], datos$time[i], units = "secs")  
}
colnames(a) <- "tiempo_GPS"
# hora local 
hrs <- function(u) { #Funcion para calcular hs
  x <- u * 3600
  return(x)
}

horal <- datos$time + hrs(3)
horal <- format(as.POSIXct(horal,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")


#distancia
distancia <- data.frame()
for(i in 1:(length(lat_utm)-1)){
  distancia[i+1,1] <- sqrt(((lat_utm[i]-lat_utm[i+1])^2)+((lon_utm[i]-lon_utm[i+1])^2)) 
} #bucle for para calcular la distancia euclidea
colnames(distancia) <- "distancia"
head(distancia)  

# Juntamos todo en una matriz
elevacion <- datos$ele
fechahora <- datos$time

matriz<- cbind(elevacion,fechahora,lat_geo,lon_geo,lat_utm,lon_utm,a, hora, horal, distancia)
head(matriz, n=5L)
str(matriz)

write.csv(matriz, "gpxprocess.csv")

track <- coordinates(datos)
track <- Line(track)
track <- list(Lines(track, 
                    ID = "Dent de Crolles"))
track <- SpatialLines(track)
proj4string(datos)
proj4string(track) <- proj4string(datos)
library(leaflet)
map <- leaflet()
map <- addTiles(map)
map <- addPolylines(map, data = track)
map

library(geosphere)
datos$distance <- 0
for(i in c(1:nrow(datos))){
  point <- datos[i,]
  if(i > 1){
    datos$distance[i] <- distm(
      coordinates(datos[(i-1),]),
      coordinates(datos[i,]),
      fun = distHaversine) +
      datos$distance[i-1]
  }
}
data <- data.frame(datos)
data$time <- as.character(data$time)

ggplot(data.frame(datos), aes(distance, ele))+geom_line()

fecha <- unlist(strsplit(as.character(data$time), " ", fixed = T))
