library(dplyr)

datos.nidos.avispa.asiatica <- read.csv("~/data/DSLab/Proyectos/avispas/avispas/datos/datos-nidos-avispa-asiatica.csv")

datos = datos.nidos.avispa.asiatica %>% group_by(UDALERRIA.MUNICIPIO,URTEA.ANIO) %>% summarise(n=n())
