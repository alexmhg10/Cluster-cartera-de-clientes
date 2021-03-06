
# paquetes de instalción 
install.packages("cluster")
install.packages("clValid")
install.packages("corrplot")
install.packages("max.print")
install.packages("NbClust")
install.packages("tidyverse")
install.packages("factoextra")
install.packages("dendextend")
install.packages("aricode")
install.packages("plyr")

# librerías 
library(dendextend)
library(factoextra)
library(tidyverse)
library(cluster)
library(clValid)
library(dendextend)
library(corrplot)
library(NbClust)
library(aricode)
library(plyr)
# Deber de agrupación de datos 
#Nombre: Alex Hernández

#Datos leidos 
library(readr)
segmentation_data <- read_csv("C:/Users/alexm/Desktop/Módulos de maestría/Maestría herraminetas para controlar procesos/Deber de clustering/segmentation_data.csv")
View(segmentation_data)
data=(segmentation_data)
data

#Normalización de los datos 
data.escala <-as.data.frame(scale(data[,1:4]))

#matriz de distancia
m.distancia<-get_dist (data.escala,method="euclidean")
options(max.print = 10000)

# número de clusters
fviz_nbclust(data.escala, kmeans, method="wss") 
fviz_nbclust(data.escala, kmeans, method="silhouette") 
fviz_nbclust(data.escala, kmeans, method="gap_stat") 
#Para este caso se observa que la agrupación se la puede realizar tanto en 4 clusters como en 
# aplicando el método de codo.

#creación de clusters
grupos=kmeans(data.escala,centers = 4,nstart = 50)
grupos
str(grupos)

#Gráfico de clusters
fviz_cluster(grupos,data=data.escala)
#La gráfica de clusters permite identificar como se encuentran agrupados los
# datos, de acuerdo a la cantidad de clientes en cada cluster, el grupo más
# con maypr cantidad de datos es el cluster 3, seguido del cluster 4, cluster 1
# para finalizar con el cluster 2.

#método de cluster jerárquico


gruposhc <- hcut(data.escala,k=4,stand = TRUE)
fviz_dend(gruposhc)
str(gruposhc)

#resumen de resultados con hc

tamañohc=gruposhc$size
tamañohc
agrupacionhc=gruposhc$cluster
agrupacionhc
datarealhc<-data%>%
  mutate(Clusterhc=gruposhc$cluster)%>%
  group_by(Clusterhc)%>%
  summarise_all("mean")
datarealhc



#resumen de resultados con k-meas

tamaño=grupos$size
agrupaciones=grupos$cluster
datareal<-data%>%
mutate(Cluster=grupos$cluster)%>%
group_by(Cluster)%>%
summarise_all("mean")
datareal
#El cluster 1 pertenece a los clientes VIP, estos clientes poseen una media de valos monetario y frecuencia muy altos 
#El cluster 2 pertenece a los cliente potenciales VIP ya que la media de su frecuencia y valor monetario es la segunda despues de los clientes VIP
#El cluster 3 pertenece a los clietes nuevos, el valor de frecuencia no es el más bajo, el valor de recency no es el más alto 
#El cluster 4 correppnde a los clientes con baja frecuencia, poseen una media de recency muy elevada y baja frecuencia  

#Comparación de resultados 
indice1=dunn(m.distancia,agrupaciones)
indice2=dunn(m.distancia,agrupacionhc)

#el resultado para k-meas es 0.0009,es menor que el resultado de 0.0017 arrojado 
#por el algorítmo hc, por lo que los resultado se encuentran mejor compactados en el algorítmo HC.


#Rendimiento del algorítmo hc con respecto a k-means
ground=agrupaciones
ARIHC=ARI(ground,agrupacionhc)
AMIHC=AMI(ground,agrupacionhc)
NMIHC=NMI(ground,agrupacionhc)

# El rendimiento del algorítmo de HC, no posee gran diferencia con respecto al algorítmo de 
# K-meas, es por ello que los valores de ARIHC, AMIHC y NMIHC, se aproxima a uno, es decir que 
# se ajustan a las agrupaciones determinadas mediante k-means, consideradala agrupación ideal(ground Truth).

