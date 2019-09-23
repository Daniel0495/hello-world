# importamos desde excel la bd en formato csv
datos <- read.csv(file.choose(), header = T, sep=";")

# extraemos c/u de las variables de la matriz datos
precio <- datos$Precio..en.millones.
km <- datos$Kilometraje..en.miles.de.km.
transmision <- datos$Transmisión

plot(km, precio)

# corremos un modelo de reg. lineal múltiple
modelo1 <- lm(precio~km+transmision)
summary(modelo1)


# fijamos la transmisión
modeloautomatico <- lm(datos$Precio..en.millones.[transmision=="Automática"]~datos$Kilometraje..en.miles.de.km.[transmision=="Automática"])
summary(modeloautomatico)
modelo3 <- lm(datos$Precio..en.millones.[transmision=="Mecánica"]~datos$Kilometraje..en.miles.de.km.[transmision=="Mecánica"])
summary(modelo3)
# graficamos precio vs km pero fijando la transmisión
plot(datos$Kilometraje..en.miles.de.km.[transmision=="Mecánica"],datos$Precio..en.millones.[transmision=="Mecánica"])

plot(datos$Kilometraje..en.miles.de.km.[transmision=="Automática"],datos$Precio..en.millones.[transmision=="Automática"], xlab = "km", ylab = "precio versión automática")

# para ver la matriz de diseño
model.matrix(modelo1)

# para hacer el Anova
anova(modelo1)

modelo2 <- lm(precio~km)
summary(modelo2)
# para comparar dos modelos anidados
anova(modelo2, modelo1)
