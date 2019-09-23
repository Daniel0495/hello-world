# Para leer la base de datos desde excel
datos <- read.csv2(file.choose(), header=T, sep = ";")
plot(datos)
precio <- datos$precio.por.metro.millones
estrato <- as.character(datos$Estrato)
baños <- datos$banos
area <- datos$area
hab <- datos$habitaciones
parq <- datos$parqueadero
admonxm2 <- datos$Admon.por.mt.2
admon <- datos$Admón

# Verifiquemos si la variable respuesta tiene dist normal
hist(precio)
summary(precio)

plot(density(precio))
shapiro.test(precio)
boxplot(precio, horizontal = T)

# Se concluye en este caso que:
# la variable respuesta NO TIENE DIST NORMAL

#para calcular una correlación lineal. Se hace el gráfico
#para tener intuición
plot(area, hab)
cov(area, hab)/(sd(area)*sd(hab))

# Para calcular la matriz de correlaciones entre un subgrupo
# de columnas de la base de datos
cor(datos[,1:3])


modelo1 <- lm(precio~estrato+area+hab)
summary(modelo1)
anova(modelo1)

plot(modelo1)
plot(datos)
shapiro.test(modelo1$residuals)

modelo2 <- lm(precio~estrato+area+hab+parq)
summary(modelo2)
anova(modelo1, modelo2)
summary(parq)

modelo3 <- lm(precio~estrato+area+hab+parq+baños)
summary(modelo3)


