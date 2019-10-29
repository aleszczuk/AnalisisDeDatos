library('curl')    # download files
library('readxl')  # read from Excel sheets
library('tidyr')   # data processing
library('dplyr')   # mo data processing
library('forcats') # mo mo data processing
library('ggplot2') # para graficar

# lectura de datos
datos <- read_excel("C:/Users/Lechu/Desktop/DiscoPosMorten/Investigacion/0-Propia/TESIS_DOCTORAL/AnalsisDatosFW/DatosSelvaSRL/ProcesamientoVIdeo/Estudio de tiempos_LS_6.19_V1 FW.xlsm", sheet = "Bloque1")

# Remover datos vacios
sapply(datos, function(x) sum(is.na(x))) #Te dice el numero NA por columnas
datos <- datos[!is.na(datos$Posicion),]

#Crear una columna al final con la resta de las posiciones
for (i in 1:length(datos$Posicion)) {
datos$tiempo
datos$tiempo[1] <- 0 # Al primer valor le asigna el valor cero 
datos$tiempo[i+1] <- (datos$Posicion[i+1])-(datos$Posicion[i]) #Al resto le aplica la resta
 }

for (i in 1:(length(datos$Posicion)-1)) {
if(datos$Evento[i]=="Parada" & datos$Evento[i+1]=="Parada"){
  datos$Actividad[i+1] <- "Parada"
} else{
  if(datos$Evento[i]=="Parada" & datos$Evento[i+1]!="Parada"){
    datos$Actividad[i+1] <- datos$Evento[i+1]  
  } else {
    if(datos$Evento[i]=="iMOV" | datos$Evento[i]=="fMOV"){
      datos$Actividad[i] <- "MOV CARGA" 
    }else{
      datos$Actividad[i+1] <- datos$Evento[i+1]
    }}
}
}



      