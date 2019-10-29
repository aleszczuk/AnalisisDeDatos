
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
  
  # guarda los datos en un objeto ---- 
  datos <- st_read(direccion, layer = "track_points") #lectura de datos

  
  }