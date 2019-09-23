#ejemplo modeo logistico

#ejemplo modeo logistico

y <- c(0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1)
x <- c(30, 30, 30, 31, 32, 33, 34, 35, 35, 35, 36, 37, 38, 39, 40, 40, 40, 41, 42, 43, 44, 45, 45, 45, 46, 47, 48, 49, 50, 50)

#como la variable respuesta solo tiene valores de ceros y unos
#aplicamos glm con familia binomial 
modelo <- glm(y~x,family = "binomial")
summary(modelo)

#obtenemos que b0=-4.80751 y b1=0.125078
#por lo tanto pigorro=(1+e^{4.80751-0.125078x})^-1

w <- exp(0.125078)
#w=1.133237 por cada valor que aumento en x, la probabilidad 
#de que pase y aumenta en un 13%.

