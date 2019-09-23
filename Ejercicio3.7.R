#leer datos: file/importar dataset
y <- ejercicio37$y
x1 <- ejercicio37$desplamiento
x2 <- ejercicio37$cabafuerza
x3<- ejercicio37$torque
x4 <- ejercicio37$carburador
x5 <- ejercicio37$cambios
x6 <- ejercicio37$lg
x7 <- ejercicio37$ancho
x8 <- ejercicio37$peso
x9 <- ejercicio37$tipotra

#comprobamos la normalidad de la y
shapiro.test(y)
#obtenemos un valor p de 0.004935 no hay normalidad

qqnorm(y)
qqline(y)
hist(y)



#_______________________________________
#aplicamos la transformacion logaritmica para normalizar

logy <- log(y)

shapiro.test(logy)
hist(logy)
qqnorm(logy)
qqline(logy)
#___________________________________________

#Modelo de regresión lineal múltiple para w
modelo_completo <- lm(logy~x1+x2+x3+x4+x5+x6+x7+x8+x9)
summary(modelo_completo)
#el resumen del modelo dice que el modelo es significativo. Analizamos las 
#correlaciones

datos <- data.frame(x1,x2,x3,x4,x5,x6,x7,x8)
cor(datos)

#____________________________________
#Modelo solo con x1 y x6

