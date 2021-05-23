library(dplyr)
library(tidyr)
library(ggplot2)
library(pdist)

rdist.w.na <- function(X,Y)
{
  if (!is.matrix(X)) 
    X = as.matrix(X)
  if (!is.matrix(Y)) 
    Y = as.matrix(Y)
  distances <- matrix(pdist(X,Y)@dist, ncol=nrow(X), byrow = TRUE)
  #count NAs
  na.count <- sapply(1:nrow(X),function(i){rowSums(is.na(Y) | is.na(X[i,]))})
  #scaling to number of cols
  distances * sqrt(ncol(X)/(ncol(X) - na.count))
}


datos.nidos.avispa.asiatica <- read.csv("~/data/DSLab/Proyectos/avispas/avispas/datos/datos-nidos-avispa-asiatica.csv")


a=datos.nidos.avispa.asiatica%>%mutate(UDALERRIA.MUNICIPIO=sub("ABANTO Y CIERVANA",
                                                                "ABANTO Y CIÉRVANA-ABANTO ZIERBENA",
                                                                 UDALERRIA.MUNICIPIO))

a=a%>%mutate(UDALERRIA.MUNICIPIO=sub("ABANTO Y CIÉRVANA-ABANTO ZIERBENA-ABANTO ZIERBENA",
                                                               "ABANTO Y CIÉRVANA-ABANTO ZIERBENA",
                                                               UDALERRIA.MUNICIPIO))
a=a%>%mutate(UDALERRIA.MUNICIPIO=sub("KARRANTZA HARANA-VALLE DE CARRANZA",
                                  "KARRANTZA HARANA/VALLE DE CARRANZA",
                                  UDALERRIA.MUNICIPIO))
a=a%>%mutate(UDALERRIA.MUNICIPIO=sub("MUNITIBAR-ARBATZEGI GERRIKAITZ",
                                     "MUNITIBAR",
                                     UDALERRIA.MUNICIPIO))
a=a%>%mutate(long1=as.numeric(gsub(",",".",longitude)),lat1=as.numeric(gsub(",",".",latitude)))

datos = a%>% group_by(UDALERRIA.MUNICIPIO,URTEA.ANIO)%>% summarise_at(c("long1", "lat1"), mean, na.rm = TRUE)

datos2 = a%>% group_by(UDALERRIA.MUNICIPIO,URTEA.ANIO)%>% summarise(y=n())
datos= cbind(datos,datos2$y)

# trabajar con log y

hist(log(datos$y,10))

ggplot(data = datos, aes(x = as.factor(URTEA.ANIO), y = log(y), color = URTEA.ANIO)) +
  geom_boxplot() +
  theme_bw()

anova <- aov(log(datos$y) ~ as.factor(datos$URTEA.ANIO))
summary(anova)
plot(anova)


APICULTURA_COLMENAS_KOPURU <- read.csv("~/data/DSLab/Proyectos/avispas/avispas/datos/APICULTURA_COLMENAS_KOPURU.csv", sep=";")

a=APICULTURA_COLMENAS_KOPURU%>%mutate(NOMBRE.MUNICIPIO=sub("ABANTO Y CIERVANA",
                                                               "ABANTO Y CIÉRVANA-ABANTO ZIERBENA",
                                                           NOMBRE.MUNICIPIO))
a=a%>%mutate(NOMBRE.MUNICIPIO=sub("ABANTO Y CIERVANA-ABANTO ZIERBENA",
                                                           "ABANTO Y CIÉRVANA-ABANTO ZIERBENA",
                                                           NOMBRE.MUNICIPIO))

colmenas=a %>% group_by(NOMBRE.MUNICIPIO) %>% summarise(y=n())
colmenas$NOMBRE.MUNICIPIO=toupper(colmenas$NOMBRE.MUNICIPIO)
datos2= merge(datos,colmenas,by.x="UDALERRIA.MUNICIPIO",by.y="NOMBRE.MUNICIPIO",all.x=TRUE,all.y=FALSE)
colnames(datos2)[3]="NIDOS"
colnames(datos2)[4]="COLMENAS"
datos2$COLMENAS=datos2$COLMENAS%>%replace_na(0)

FRUTALES.DECLARADOS.KOPURU <- read.csv("~/data/DSLab/Proyectos/avispas/avispas/datos/FRUTALES-DECLARADOS-KOPURU.csv", sep=";")
TEMP.MIN.MEDIA.2019 <- read.csv("~/data/DSLab/Proyectos/avispas/avispas/datos/CSV 2019 ESTACIONES/TEMPERATURA MÍNIMA MEDIA (ºC) 2019.csv", sep=";")

a=TEMP.MIN.MEDIA.2019 %>%distinct(ESTACION,.keep_all=TRUE)
#write.csv(file="bob.csv",x=a$ESTACION)
#leer coordenadas de las estaciones metereológicas

estaciones_long_lat <- read.csv("~/data/DSLab/Proyectos/avispas/avispas/estaciones_long_lat.csv")

estaciones_long_lat=estaciones_long_lat%>%mutate(long=as.numeric(gsub(",",".",long)),lat=as.numeric(gsub(",",".",lat)))



library(geosphere)
#coordenadas.mun<- read.csv("~/data/DSLab/Proyectos/avispas/avispas/datos/coordenadas_municipios.txt", header=FALSE)
#colnames(coordenadas.mun)=c("MUNICIPIO","lat","lon")
#coordenadas.mun$MUNICIPIO=toupper(coordenadas.mun$MUNICIPIO)
#a=coordenadas.mun%>%mutate(MUNICIPIO=sub("ABADINO","ABADIÑO",MUNICIPIO))
#a=a%>%mutate(MUNICIPIO=sub("ABANTO-Y-CIERVANA-ABANTO-ZIERB","ABANTO Y CIÉRVANA-ABANTO ZIERBENA",MUNICIPIO))

#datos3= merge(datos2,a,by.x="UDALERRIA.MUNICIPIO",by.y="MUNICIPIO",all.x=TRUE,all.y=FALSE)



distm(c(lon3, lat3), c(lon1, lat1), fun = distHaversine)
