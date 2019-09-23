# Ejm: Los siguientes datos corresponden a las ventas anuales de una compañía.
# x representa el año, y representa ventas.

x <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
y <- c(18.5, 22.6, 27.2, 31.2, 33, 44.9, 49.4, 35)

# 1) Ajuste un modelo de R.L.S a los datos (con Y como variable dependiente)
# 2) Ajuste un modelo cuadrático a los datos (con Y como variable dependiente)
# 3) ¿Hay evidencia de un efecto cuadrático en la relación entre Y e X?. Responda con una P.H al nivel alpha = 0.10
# 4) Encuentre un I.C al 90% para beta_2.

# Solución. 1)

plot(x,y)
modelolineal <- lm(y~x)
summary(modelolineal)

# Se concluye que X sí influye sobre Y (tanto el test global (F) como el test individual (t) lo confirman). 
# R2adj=0.67.

# 2) Creamos un vector que contenga los valores de x al cuadrado.
xnueva <- c()
for (i in 1:length(x)) {
  xnueva[i] <- x[i]^2
}
x2 <- 

modelocuad <- lm(y~x+xnueva)
summary(modelocuad)

# El test global indica que al menos una de las variables regresoras es significativa, el test individual dice
# que tanto beta_1 como beta_2 no son significativos. Según la teoría, hay colinealidad. R2adj=0.69

# 3). Con el modelo grande, "modelocuad", creamos una P.H para un subgrupo de parámetros, la matriz A que 
# multiplica al vector beta es (0, 0, 1).

SSRes_0 <- (6.024)^2 * 6
SSRes <- 5.808^2 * 5
f <- ((SSRes_0-SSRes)/(6-5))/(SSRes/8-5)
f
# Comparo el valor del estadístico con el quantil 90 de una F(1,3)
qf(0.90, 1, 3)

# Como f < qf(0.90, 1, 3), no se rechaza la hipótesis nula, es decir, no hay evidencia suficiente para decir 
# que existe un efecto cuadrático de X sobre Y.

# Se concluye que el mejor modelo entre los planteados es el modelo lineal, pese a que tenga un R2adj menor.

# otra forma más rápida
datos <- data.frame(x,y)
modelonuevo <- lm(y~x+x*x, data = datos)
summary(modelonuevo)

# EJERCICIO. Ajustar un modelo con interacción entre regresoras. Los datos son:
x1 <- c(1, 1, -1, -1, 0, 0, 0, 0, 0, 1.5, -1.5)
x2 <- c(-1, 1, 1, -1, 0, 0, 0, 1.5, -1.5, 0, 0)
w <- c(83, 113, 92, 82, 100, 96, 98, 95, 80, 100, 92)

shapiro.test(w)
cor(data.frame(x1,x2))

# Este es el caso ideal para la regresión múltiple, y es que la v. regresoras tienen correlación lineal 0, y 
# la v. respuesta tiene dist. normal.
