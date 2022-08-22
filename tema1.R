#TEMA 1
PATH_DATA <- file.path('/home','ruben','Documentos','Programas','practical-statistics-for-data-scientists-master')

#Carga el archivo csv y lo guarda en state
state <- read.csv(file.path(PATH_DATA, 'data','state.csv'))
state #muestro los datos

#MEDIAS
mean(state[['Population']])  #calcula la media de la columna Population

mean(state[['Population']], trim=0.1)  #Calcula la media truncada quitando el 10% por arriba y por debajo

median(state[['Population']])  #Calcula la mediana

weighted.mean(state[['Murder.Rate']], w=state[['Population']])  #Media ponderada

#Calculo de la mediana truncada, es necesario cargar la libreria matrixStats
library('matrixStats')
weightedMedian(state[['Murder.Rate']], w=state[['Population']])

sd(state[['Population']])  #Desviación estandar

IQR(state[['Population']])   ## Rango intercuartilico (mitad del percentil 75 y 25)

mad(state[['Population']])  #Desviación absuluta mediana de la mediana


##Diferentes percentiles de homicidios
quantile(state[['Murder.Rate']], p=c(.05, .25, .5, .75, .95))

#Diagrama de caja  para la población por estados
boxplot(state[['Population']]/1000000, ylab='Population (millions)')


#Crear división del rango de Population en diferentes segmentos igualmente espaciados
breaks <- seq(from=min(state[['Population']]),to=max(state[['Population']]), length=11)
#genero trabla con la frecuencia que los estados estan en ese segmento de poblacion
pop_freq <- cut(state[['Population']], breaks=breaks, right=TRUE, include.lowest=TRUE)
table(pop_freq)


#Genero histograma con la cantidad de estados con los diferentes segmentos de población
hist(state[['Population']], breaks=breaks)

#Genero histograma con la tasa de asesinatos frente a el porcentaje de estados en los que existe esa tasa
hist(state[['Murder.Rate']], freq=FALSE, main='Homicidios frente a porcentaje de estados')
#genero diagrama de densidad para el caso con density
lines(density(state[['Murder.Rate']]), lwd=3, col='blue')

#CARGO NUEVO CSV CON DATOS
dfw <- read.csv(file.path(PATH_DATA, 'data', 'dfw_airline.csv'))
#Genero grafico de barras para los valores almacenados en dfw
barplot(as.matrix(dfw)/6, cex.axis=0.8, cex.names = 0.7, xlab='Causa del retraso', ylab = 'Recuento')

#CORRELACIÓN
#Cargo los archivos de datos
sp500_px <- read.csv(file.path(PATH_DATA, 'data', 'sp500_data.csv.gz'), row.names=1)
sp500_sym <- read.csv(file.path(PATH_DATA, 'data', 'sp500_sectors.csv'), stringsAsFactors = FALSE)
#genero tabla para rentabilidades por fecha para las acciones de telecomunicaciones
etfs <- sp500_px[row.names(sp500_px) > '2012-07-01', sp500_sym[sp500_sym$sector == 'etf', 'symbol']]
library(corrplot) ##cargo libreria corrplot
corrplot(cor(etfs), method='ellipse') #genero el gráfico

#PRUEBAS
state
png(filename = "/home/ruben/Documentos/histograma.png", width = 1024, height = 768)
plot(x=state[['Population']]/1000000, y=state[['Murder.Rate']], main='Densidad de asesinatos',
     xlab='Millones de habitantes', ylab='Tasa de homicidios')
dev.off()

