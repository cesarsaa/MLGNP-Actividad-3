# MLGNP Tarea 3
# Cesar A Saavedra
# Angie Rodriguez uque
#----------------------------------------------------------------------------------------#
# Librerias 
suppressMessages(library(dplyr))
suppressMessages(library(readxl))
suppressMessages(library(tidyverse))
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(foreign))
suppressMessages(library(corrplot))
suppressMessages(library(polycor))
suppressMessages(library(psych))
suppressMessages(library(gplots))
suppressMessages(library(gridExtra))
suppressMessages(library(viridis))
suppressMessages(library(lsr))
suppressMessages(library(DescTools))
suppressMessages(library(magrittr))
suppressMessages(library(nlme))
suppressMessages(library(MASS))
suppressMessages(library(multilevel))
suppressMessages(library(reshape))
suppressMessages(library(homals))
suppressMessages(library(GGally))
suppressMessages(library(CCA))
suppressMessages(library(plotly))
suppressMessages(library(broom))
suppressMessages(library(readr))
suppressMessages(library(lubridate))
suppressMessages(library(purrr))
suppressMessages(library(VGAM))
#----------------------------------------------------------------------------------------#
# Fijar directorio
setwd("/Users/cesar.saavedra/Documents/GitHub/MLGNP-Actividad-3")
setwd("")
#----------------------------------------------------------------------------------------#
# Cargar los datos
Datos <- read.table("Datos.txt",header=T,sep = ",")
Datos
# Tamaño de la muestra
n <- 60
# Selección de la muestra
set.seed(917)
muestra <- Datos %>% sample_n(size=n,replace=FALSE)
muestra
# Correlacion
corrplot(cor(muestra), method="square", type="upper", order="hclust", tl.col="black")
# Exploracion de datos
ggplot()+ geom_point(data = muestra, aes(y = fixed.acidity, x = pH)) + 
  ylab("Acidez fija") + xlab("pH")
# Estimacion de sigma
