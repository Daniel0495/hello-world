#ejemplo del libro con 3 parametros 
y <- c(16.68, 11.50, 12.03, 14.88, 13.75, 18.11, 8.00, 17.83, 79.24, 21.50, 40.33, 21.00, 13.50, 19.75, 24.00, 29.00, 15.35, 19.00, 9.50, 35.10, 17.90, 52.32, 18.75, 19.83, 10.75)
x1 <- c(7, 3, 3, 4, 6, 7, 2, 7, 30, 5, 16, 10, 4, 6, 9, 10, 6, 7, 3, 17, 10, 26, 9, 8, 4)
x2 <- c(560, 220, 340, 80, 150, 330, 110, 210, 1460, 605, 688, 215, 255, 462, 448, 776, 200, 132, 36, 770, 140, 810, 450, 635, 150 )

modelo <- lm(y~x1+x2)

modelo
# así b0gorro=2.34123 b1gorro=1.61591 b2gorro=0.01438

modelo$residuals #muestra todos lo residuos del modelo
ygorro <- 2.34123+1.61591*x1+0.01438*x2
ygorro

summary(modelo)

sumayi1 <- sum(y^2) #forma 1 haciendo cada yi al cuadrado
sumayi2 <- t(y)%*%y #forma 2 haciendo producto de vectores 

SSres = sum((y - ygorro)^2)
SSres

persp(x1, x2, y)
