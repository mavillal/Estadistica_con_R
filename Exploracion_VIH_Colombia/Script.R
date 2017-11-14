# Descripción de los datos
datos <- read.csv("VIH.csv")
summary(datos[, -c(1:4)])

par(lwd = 4)
hist(datos$Gini_2002,
     nclass = 9,
     col = "gray35",
     main = "Distribución del coeficiente GINI para el año 2002",
     xlab = "Coeficiente GINI",
     ylab = "Densidad",
     freq = FALSE)
lines(density(datos[-c(25:33),10]), col = "red", lwd = 5)

par(lwd = 4)
hist(datos$Pobreza,
     nclass = 9,
     col = "gray35",
     main = "Distribución del porcentaje de personas en condición de pobreza",
     xlab = "Pobreza (% de personas)",
     ylab = "Densidad",
     freq = FALSE)
lines(density(datos$Pobreza), col = "blue", lwd = 5)

boxplot(datos$MuVIH_2005,
        col = "gray35",
        main = "Muertes por VIH para el año 2005",
        xlab = "",
        ylab = "Número de muertos por VIH")

#Depuración
library(dplyr)
datos2 <- datos %>%
  filter(Gini_2002 != "") %>%
  select(-Gini_2011)

summary(datos2[, -c(1:4)])

boxplot(datos2$MuVIH_2005,
        col = "gray35",
        main = "Muertes por VIH para el año 2005",
        xlab = "",
        ylab = "Número de muertos por VIH")

library(tidyr)
datos3 <- datos2 %>%
  gather(value = Valor,
         key = Item,
         -c(1:4))

library(DT)
datatable(datos3)

datos4 <- datos3 %>%
  group_by(Item) %>%
  summarise(Promedio = round(mean(Valor), digits = 2),
            Desviacion = round(sd(Valor), digits = 2),
            Varianza = round(var(Valor), digits = 2),
            Minimo = min(Valor),
            Máaimo = max(Valor)) 

library(pander)
names(datos4) <- c("Variable", "Promedio", "Desviación", "Varianza", "Mínimo", "Máximo")
pander(datos4)
  
datosgg <- datos4[1:5,]

library(ggplot2)
ggplot(data = datosgg, aes(x = Variable, y = Promedio)) +
  geom_bar(stat = "identity", color = "darkblue", fill = "gray12", lwd = 1.5) +
  geom_text(aes(label = Promedio),
            size = 8,
            color = c("white"), 
            position = position_stack(vjust = 0.5)) +
  labs(x = "",
       y = "Promedio",
       title = "Número de casos promedio de VIH 2008-2012") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, colour = "Darkred"),
        axis.text.x = element_text(size = 15, colour = "Black"),
        axis.title.y = element_text(size = 15, colour = "Black"),
        axis.text.y = element_text(size = 15, colour = "Black")) 

datosgg2 <- datos4[19:26,]

ggplot(data = datosgg2, aes(x = Variable, y = Promedio)) +
  geom_bar(stat = "identity", color = "darkblue", fill = "gray12", lwd = 1.5) +
  geom_text(aes(label = Promedio),
            size = 8,
            color = c("white"), 
            position = position_stack(vjust = 0.5)) +
  labs(x = "",
       y = "Promedio",
       title = "Número de muertos promedio por VIH 2005-2012") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, colour = "Darkred"),
        axis.text.x = element_text(size = 15, colour = "Black"),
        axis.title.y = element_text(size = 15, colour = "Black"),
        axis.text.y = element_text(size = 15, colour = "Black")) 

datosgg3 <- datos4[7:16,]

library(ggplot2)
ggplot(data = datosgg3, aes(x = Variable, y = Promedio)) +
  geom_bar(stat = "identity", color = "darkblue", fill = "gray12", lwd = 1.5) +
  geom_text(aes(label = Promedio),
            size = 8,
            color = c("white"), 
            position = position_stack(vjust = 0.5)) +
  labs(x = "",
       y = "Promedio",
       title = "Coeficiente de GINI [2002-2005; 2008-2014]") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, colour = "Darkred"),
        axis.text.x = element_text(size = 15, colour = "Black"),
        axis.title.y = element_text(size = 15, colour = "Black"),
        axis.text.y = element_text(size = 15, colour = "Black")) 

# Situación por departamento
datos5 <- datos[, -c(10:20)]

datos5.1 <- datos5 %>%
  gather(key = Variable,
         value = Valor,
         -c(1:4)) 

datatable(datos5.1)

datos5gg <- datos5.1[1:165,]

datos5gg1 <- datos5gg %>%
  group_by(Departamento) %>%
  summarise(Promedio = mean(Valor),
            Desviación = sd(Valor),
            Mínimo = min(Valor),
            Máximo = max(Valor),
            CV = (Desviación/Promedio)*100) %>%
  arrange(desc(Promedio))

datos5gg1[, c(3:4,6)] <- round(datos5gg1[,c(3:4,6)], digits = 2)

datatable(datos5gg1)

ggplot(data = datos5gg1[1:10,], aes(x = Departamento, y = Promedio)) +
  geom_bar(stat = "identity", color = "darkblue", fill = "gray12", lwd = 1.5) +
  geom_text(aes(label = Promedio),
            size = 8,
            color = c("white"), 
            position = position_stack(vjust = 0.5)) +
  labs(x = "",
       y = "Promedio",
       title = "10 departamentos con mayor casos promedio de VIH en colombia (2008-2012)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, colour = "Darkred"),
        axis.text.x = element_text(size = 15, colour = "Black", angle = 90),
        axis.title.y = element_text(size = 15, colour = "Black"),
        axis.text.y = element_text(size = 15, colour = "Black")) 

datos6gg <- datos5.1[166:429,]

datos6gg1 <- datos6gg %>%
  filter(Departamento != "Amazonas" & Departamento != "Putumayo") %>%
  group_by(Departamento) %>%
  summarise(Promedio = mean(Valor),
            Desviación = sd(Valor),
            Mínimo = min(Valor),
            Máximo = max(Valor),
            CV = (Desviación/Promedio)*100) %>%
  arrange(desc(Promedio))

datos6gg1[, c(2:4,6)] <- round(datos6gg1[,c(2:4,6)], digits = 0)

datatable(datos6gg1)

ggplot(data = datos6gg1[1:10,], aes(x = Departamento, y = Promedio)) +
  geom_bar(stat = "identity", color = "dodgerblue", fill = "gray12", lwd = 1.5) +
  geom_text(aes(label = Promedio),
            size = 6,
            color = c("white"), 
            position = position_stack(vjust = 0.5)) +
  labs(x = "",
       y = "Promedio",
       title = "10 departamentos con mayor muertes promedio por VIH en colombia (2005-2012)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, colour = "Darkred"),
        axis.text.x = element_text(size = 15, colour = "Black", angle = 90),
        axis.title.y = element_text(size = 15, colour = "Black"),
        axis.text.y = element_text(size = 15, colour = "Black")) 

datos7gg <- datos5.1[-c(1:429),]

datatable(datos7gg[,-c(1:3)])

par(lwd = 4,
    col = "blue")
hist(datos7gg$Valor,
     nclass = 40,
     col = "gray20",
     main = "Distribución del número de personas con NBI para el año 2005",
     cex.main = 1.5,
     xlab = "Personas (%)",
     ylab = "Densidad",
     freq = FALSE)
lines(density(datos7gg$Valor), col = "red", lwd = 5)

ggplot(data = datos7gg[1:33,], aes(x = Departamento, y = Valor)) +
  geom_bar(stat = "identity", color = "royalblue3", fill = "gray12", lwd = 1) +
  labs(x = "",
       y = "Personas (%)",
       title = "Porcentaje de personas en condición de pobreza por departamento (2005)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, colour = "Darkred"),
        axis.text.x = element_text(size = 15, colour = "Black", angle = 90),
        axis.title.y = element_text(size = 15, colour = "Black"),
        axis.text.y = element_text(size = 15, colour = "Black")) 

ggplot(data = datos7gg[67:99,], aes(x = Departamento, y = Valor)) +
  geom_bar(stat = "identity", color = "firebrick3", fill = "gray12", lwd = 1) +
  labs(x = "",
       y = "Personas (%)",
       title = "Porcentaje de personas con servicios inadecuados por departamento (2005)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, colour = "Darkred"),
        axis.text.x = element_text(size = 15, colour = "Black", angle = 90),
        axis.title.y = element_text(size = 15, colour = "Black"),
        axis.text.y = element_text(size = 15, colour = "Black")) 

ggplot(data = datos7gg[100:132,], aes(x = Departamento, y = Valor)) +
  geom_bar(stat = "identity", color = "royalblue3", fill = "gray12", lwd = 1.2) +
  labs(x = "",
       y = "Personas (%)",
       title = "Porcentaje de personas en condición de hacinamiento por departamento (2005)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, colour = "Darkred"),
        axis.text.x = element_text(size = 15, colour = "Black", angle = 90),
        axis.title.y = element_text(size = 15, colour = "Black"),
        axis.text.y = element_text(size = 15, colour = "Black")) 

ggplot(data = datos7gg[133:165,], aes(x = Departamento, y = Valor)) +
  geom_bar(stat = "identity", color = "red3", fill = "gray12", lwd = 1.1) +
  labs(x = "",
       y = "Personas (%)",
       title = "Porcentaje de personas con inasistencia escolar por departamento (2005)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, colour = "Darkred"),
        axis.text.x = element_text(size = 15, colour = "Black", angle = 90),
        axis.title.y = element_text(size = 15, colour = "Black"),
        axis.text.y = element_text(size = 15, colour = "Black")) 

datos2.1 <- datos2[, c(1:4, 10:19)]

datos2.1_1 <- datos2.1 %>%
  gather(key = variable,
         value = GINI,
         -c(1:4))

datos2_2 <- datos2.1_1 %>%
  group_by(Departamento) %>%
  summarise(Promedio = mean(GINI),
            Desviación = sd(GINI),
            CV = (Desviación/Promedio)*100)

datos2_2[, c(2:4)] <- round(datos2_2[, c(2:4)], digits = 2)

datatable(datos2_2)

ggplot(data = datos2_2, aes(x = Departamento, y = Promedio)) +
  geom_bar(stat = "identity", color = "deepskyblue4", fill = "gray12", lwd = 1.1) +
  labs(x = "",
       y = "Promedio",
       title = "Promedio de coeficiente GINI para los años 2002-2014 por departamento") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, colour = "Darkred"),
        axis.text.x = element_text(size = 15, colour = "Black", angle = 90),
        axis.title.y = element_text(size = 15, colour = "Black"),
        axis.text.y = element_text(size = 15, colour = "Black")) 


# Correlación entre variables
datoscor <- datos %>%
  select(c(5:33))

library(corrplot)
library(RColorBrewer)
df2 <- cor(datoscor, use = "na.or.complete")
corrplot(df2, method = 'circle', tl.col="black", tl.srt=90, col=brewer.pal(n=20, name="PuOr"))

# Análisis de componentes principales
datosstd <- data.frame(scale(datos2[, 5:33]))
acp <- prcomp(datosstd, cor = TRUE)

a <- summary(acp)
desviacion <- round(a$sdev, digits = 3)
total <- round(sum(desviacion^2), digits = 3)
proporcion <- round(desviacion^2/total, digits = 3)
acumulada <- c("0.375", "0.618", "0.792", "0.867", "0.915", "0.939", "0.957", "0.969",
               "0.978", "0.983", "0.988", "0.991", "0.993", "0.995", "0.996", "0.998", 
               "0.998", "0.999", "0.999", "0.999", "0.999", "0.999", "1.000", "1.000")
acumulada2 <- as.numeric(acumulada)
Componente <- paste0("Comp.", rep(1:24))

importancia <- data.frame(Componente, desviacion, proporcion, acumulada2)
names(importancia) <- c("Componente Principal","Desviación estándar", "Propoción de varianza",
                        "Proporción de varianza acumulada")
datatable(importancia)

# Gráfico de las componentes principales 1 y 2 

datos2 <- data.frame(datos2, acp$x[, c(1, 2, 3)])

color <- rainbow(32, start = 0.1, end = 1, v = 0.4)
with(datos2, plot(PC1, PC2, col = color,
                  pch = 19,
                  cex = 1.5,  xlab = "CP1",
                  ylab = "CP2",
                  main = "Departamentos sobre las componentes principales 1 y 2"))
abline(h = 0, col = "blue")
abline(v = 0, col = "blue")
arrows(0, 0,
       acp$rotation[, 1]*20,
       acp$rotation[, 2]*20,
       col = "darkred",
       lwd = 2.5)
text(acp$rotation[, 1]*20.5,
     acp$rotation[, 2]*20.5,
     row.names(acp$rotation),
     cex = 1,
     pos = 1)
text(datos2$PC1,
     datos2$PC2,
     labels = datos2$Departamento,
     pos = 3)

# Gráfico de las componentes principales 1 y 3
color <- rainbow(32, start = 0.1, end = 1, v = 0.4)
with(datos2, plot(PC1, PC3, col = color,
                  pch = 19,
                  cex = 1.5,  xlab = "CP1",
                  ylab = "CP3",
                  main = "Departamentos sobre las componentes principales 1 y 3"))
abline(h = 0, col = "blue")
abline(v = 0, col = "blue")
arrows(0, 0,
       acp$rotation[, 1]*10,
       acp$rotation[, 3]*10,
       col = "darkred",
       lwd = 2.5)
text(acp$rotation[, 1]*10.5,
     acp$rotation[, 3]*10.5,
     row.names(acp$rotation),
     cex = 1,
     pos = 1)
text(datos2$PC1,
     datos2$PC3,
     labels = datos2$Departamento,
     pos = 3)

# Gráfico de las componentes principales 2 y 3
color <- rainbow(32, start = 0.1, end = 1, v = 0.4)
with(datos2, plot(PC2, PC3, col = color,
                  pch = 19,
                  cex = 1.5,  xlab = "CP2",
                  ylab = "CP3",
                  main = "Departamentos sobre las componentes principales 2 y 3"))
abline(h = 0, col = "blue")
abline(v = 0, col = "blue")
arrows(0, 0,
       acp$rotation[, 2]*13,
       acp$rotation[, 3]*13,
       col = "darkred",
       lwd = 2.5)
text(acp$rotation[, 2]*13.5,
     acp$rotation[, 3]*13.5,
     row.names(acp$rotation),
     cex = 1,
     pos = 1)
text(datos2$PC2,
     datos2$PC3,
     labels = datos2$Departamento,
     pos = 3)


# Análisis de Cluster (Agrupamiento)
cluster1 <- hclust(dist(datosstd))
plot(cluster1, 
     xlab = "",
     main = "Dendrograma mediante el método de enlace completo (vecino más lejano)",
     col = "darkblue")
abline(h=12, col = "darkred", lwd = 2, lty = 2)

datos2$grupo <- cutree(cluster1, 3)
datatable(datos2[,c(1:4, 37)])

a <- lapply(split(datos2[,5:33], datos2$grupo), summary)
a[1]
a[2]
a[3]

# Componentes principales + cluster
colores <- c("forestgreen", "orange4", "magenta4")
simbolos <- c(15, 17, 19)
with(datos2, plot(PC1, PC2, col = colores[grupo], pch = simbolos[grupo], 
                  xlab = "Componente principal 1", ylab = "Componente principal 2", 
                  main = "Observaciones (departamentos) sobre componentes principales 1 y 2", 
                  cex = 1.7))
legend("bottomleft", legend = c("Grupo 1", "Grupo 2", "Grupo 3"), pch = simbolos, col = colores, ncol =1, cex = 1.2)
abline(h = 0, col = "blue")
abline(v = 0, col = "blue")
arrows(0, 0,
       acp$rotation[, 1]*13,
       acp$rotation[, 2]*13,
       col = "darkred",
       lwd = 2.5)
text(acp$rotation[, 1]*13.5,
     acp$rotation[, 2]*13.5,
     row.names(acp$rotation),
     cex = 1,
     pos = 1)
text(datos2$PC1,
     datos2$PC2,
     labels = datos2$Departamento,
     pos = 3)

colores <- c("forestgreen", "orange4", "magenta4")
simbolos <- c(15, 17, 19)
with(datos2, plot(PC1, PC2, col = colores[grupo], pch = simbolos[grupo], 
                  xlab = "Componente principal 1", ylab = "Componente principal 2", 
                  main = "Observaciones (departamentos) sobre componentes principales 1 y 2", 
                  cex = 1.7))
legend("bottomleft", legend = c("Grupo 1", "Grupo 2", "Grupo 3"), pch = simbolos, col = colores, ncol =1, cex = 1.2)
abline(h = 0, col = "blue")
abline(v = 0, col = "blue")
arrows(0, 0,
       acp$rotation[, 1]*13,
       acp$rotation[, 2]*13,
       col = "darkred",
       lwd = 2.5)
text(acp$rotation[, 1]*13.5,
     acp$rotation[, 2]*13.5,
     row.names(acp$rotation),
     cex = 1,
     pos = 1)

colores <- c("chocolate3", "green3", "darkmagenta")
simbolos <- c(15, 17, 19)
with(datos2, plot(PC1, PC3, col = colores[grupo], pch = simbolos[grupo], 
                  xlab = "Componente principal 1", ylab = "Componente principal 3", 
                  main = "Observaciones (departamentos) sobre componentes principales 1 y 3", 
                  cex = 1.7))
legend("topleft", legend = c("Grupo 1", "Grupo 2", "Grupo 3"), pch = simbolos, col = colores, ncol =1, cex = 1.2)
abline(h = 0, col = "blue")
abline(v = 0, col = "blue")
arrows(0, 0,
       acp$rotation[, 1]*13,
       acp$rotation[, 3]*13,
       col = "darkred",
       lwd = 2.5)
text(acp$rotation[, 1]*13.5,
     acp$rotation[, 3]*13.5,
     row.names(acp$rotation),
     cex = 1,
     pos = 1)

colores <- c("mediumorchid4", "mediumseagreen", "yellow4")
simbolos <- c(15, 17, 19)
with(datos2, plot(PC2, PC3, col = colores[grupo], pch = simbolos[grupo], 
                  xlab = "Componente principal 2", ylab = "Componente principal 3", 
                  main = "Observaciones (departamentos) sobre componentes principales 2 y 3", 
                  cex = 1.7))
legend("topleft", legend = c("Grupo 1", "Grupo 2", "Grupo 3"), pch = simbolos, col = colores, ncol =1, cex = 1.2)
abline(h = 0, col = "blue")
abline(v = 0, col = "blue")
arrows(0, 0,
       acp$rotation[, 2]*13,
       acp$rotation[, 3]*13,
       col = "darkred",
       lwd = 2.5)
text(acp$rotation[, 2]*13.5,
     acp$rotation[, 3]*13.5,
     row.names(acp$rotation),
     cex = 1,
     pos = 1)

# Mapa de Colombia
library(rgdal)
colombia <- readOGR(dsn = ".", layer="depto")

colores <- c("gray25", "black", "green", "green2", "green4", "yellow", "orange", "navy", "blue", "red",
             "chocolate4", "magenta4", "purple", "darkgreen", "darkorange", "gold", "gold4", "darkviolet",
             "deeppink", "lawngreen", "lightgreen", "lightseagreen", "mediumseagreen", "magenta", "maroon",
             "olivedrab1", "olivedrab", "orchid", "palegreen4", "royalblue2", "seagreen", "seagreen2", "sienna1")
plot(colombia,
     col = colores, lwd = 3)
legend("bottomleft", legend = datos$Departamento, pch = 15, col = colores, ncol = 3, cex = 0.7)


# Mapas de indicadores de pobreza (2005)
levels(colombia$NOMBRE_DPT) <- levels(datos$Departamento) 

library(viridis)
colombia@data <- datos
spplot(colombia, "Pobreza", col.regions = viridis(30, alpha = 0.9, begin = 0.2, end = 0.95, option = "C", direction = -1),
       lwd = 3)

indicadores <- c("Pobreza", "Hacinamiento", "InasistenciaEscolar", "Vivienda",
                 "DependenciaEconomica", "ServiciosInadecuados")

library(lattice)
bgColors <- rep("black", 6)
txtColors <- rep("white", 6)

myStripStyle <- function(which.panel, factor.levels, ...) {
  panel.rect(0, 0, 1, 1,
             col = bgColors[which.panel],
             border = 1)
  panel.text(x = 0.5, y = 0.5,
             font=2,
             lab = factor.levels[which.panel],
             col = txtColors[which.panel])
}    

paleta <- brewer.pal(7, "OrRd")
spplot(colombia, indicadores, col.regions = paleta, cuts = 6, lwd = 3, strip=myStripStyle,
       main = "Porcentaje de personas con Niveles Básicos Insatisfechos (NBI)")

# Mapas de coeficiente GINI
gini <- c("Gini_2002", "Gini_2003", "Gini_2004", "Gini_2005", "Gini_2008", "Gini_2009", "Gini_2010",
          "Gini_2012", "Gini_2013", "Gini_2014")

bgColors <- rep("black", length(gini))
txtColors <- rep("white", length(gini))

myStripStyle <- function(which.panel, factor.levels, ...) {
  panel.rect(0, 0, 1, 1,
             col = bgColors[which.panel],
             border = 1)
  panel.text(x = 0.5, y = 0.5,
             font=2,
             lab = factor.levels[which.panel],
             col = txtColors[which.panel])
}    

paleta <- brewer.pal(7, "Blues")
spplot(colombia, gini, col.regions = paleta, cuts = 6, lwd = 3, strip=myStripStyle,
       main = "")

# Mapas de número de casos notificados de VIH
casos <- c("CasosVIH2008", "CasosVIH2009", "CasosVIH2010", "CasosVIH2011", "CasosVIH2012")

bgColors <- rep("black", length(casos))
txtColors <- rep("white", length(casos))

myStripStyle <- function(which.panel, factor.levels, ...) {
  panel.rect(0, 0, 1, 1,
             col = bgColors[which.panel],
             border = 1)
  panel.text(x = 0.5, y = 0.5,
             font=2,
             lab = factor.levels[which.panel],
             col = txtColors[which.panel])
}    

paleta <- brewer.pal(7, "Greens")
spplot(colombia, casos, col.regions = paleta, cuts = 6, lwd = 2, strip=myStripStyle,
       main = "")

# Mapas de número de muertes por VIH 
muertes <- c("MuVIH_2005", "MuVIH_2006", "MuVIH_2007", "MuVIH_2008", "MuVIH_2009", "MuVIH_2010",
             "MuVIH_2011", "MuVIH_2012")

bgColors <- rep("black", length(muertes))
txtColors <- rep("white", length(muertes))

myStripStyle <- function(which.panel, factor.levels, ...) {
  panel.rect(0, 0, 1, 1,
             col = bgColors[which.panel],
             border = 1)
  panel.text(x = 0.5, y = 0.5,
             font=2,
             lab = factor.levels[which.panel],
             col = txtColors[which.panel])
}    

colombia[27:28, c(21:28)] <- NA

paleta <- brewer.pal(7, "YlOrBr")
spplot(colombia, muertes, col.regions = paleta, cuts = 6, lwd = 2, strip=myStripStyle)

