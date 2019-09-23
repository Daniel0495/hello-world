# EJERCICIO. Ajustar un modelo con interacciÃ³n entre regresoras. Los datos son:
x1 <- c(1, 1, -1, -1, 0, 0, 0, 0, 0, 1.5, -1.5)
x2 <- c(-1, 1, 1, -1, 0, 0, 0, 1.5, -1.5, 0, 0)
w <- c(83, 113, 92, 82, 100, 96, 98, 95, 80, 100, 92)
x1x2 <- x1*x2
x22 <- x2*x2
x11 <- x1*x1
datos <- data.frame(x1,x2,x11,x22,x1x2)

shapiro.test(w)
cor(data.frame(x1,x2))

modelo2 <- lm(w~x1+x2+x1x2+x22+x11)
modelo1 <- lm(w~ x1+x2)


summary(modelo1)
#x1 no significativo,x2 sí pero todo el modelo es explicativo
#r2aj =  0.5573;   y valor p =  0.01574

summary(modelo2)
#x1x2, x11 no son significativos, sin embargo todo el modelo es explicativo
#r2aj = 0.8309;   y valor p =  0.01028


modelo3 <- lm(w~ x1+x2+x22)

summary(modelo3)
 #modelo es explicativo, sin embargo solo x2 es significativo
#realizamos análisis de correlacion

modelo4 <- lm(w~ x1+x2+x22+ x1x2)
cor(datos)
#no hay correlacion lineal directa entre cada par de variables


anova(modelo1, modelo2)
#valor p indica que las variables cuadráticas no son significativas
#hacemos prueba de hipótesis sobre este supuesto

ssres0 <- ((6.39)^2) * 3
ssres <- ((3.948)^2) * 5
f <- ((ssres0 - ssres)*5)/(ssres*3)
f
qf(0.025,3,5)
qf(0.975,3,5) 

#f < f_qf => se acepta hipótsis nula, es decir los terminos cuadraticos 
#no son significativos



#w = 97.095+ 4x1 + 7.35x2 - 4.358x22 + 5x1x2
#r2adj = 0.85