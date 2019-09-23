# Ejercicio 213
D <- c(91, 105, 106, 108, 88, 91, 58, 82, 81, 65, 61, 48, 61, 43, 33, 36)
I <- c(16.7, 17.1, 18.2, 18.1, 17.2, 18.2, 16.0, 17.2, 18.0, 17.2, 16.9, 17.1, 18.2, 17.3, 17.5, 16.6)
# Inciso a (Diagrama de disperción)
plot(I,D, xlab = "Indice", ylab = "Dias con ppm mayor que 0.20", main = "Diagrama del número de dias con ppm mayor a 0.20", pch = 19)
# Inciso b (ecuación de predición)
M <- lm(D~I)
M

abline(M, col="red")
# Entonces segun el metodo de minimos cuadrados la ecuación de predicción propuesta es:
# D = -193.0 + 15.3I
# inciso c (test de significancia)
summary(M)
#no es posible rechazar la hipotesis nula
#es decir no se puede garantizar total dependencia de los días con respecto al indice metereologico
# R^2=0.1585 muy alejado del 1, por lo tanto no explica de manera adecuada la variabilidad total. 
# inciso d (intervalo de prediccion) en las bandas, es decir, I=18.2 e I=16.0
MSRES <- sum((M$residuals)^2)/14
Ibarra <- sum(I)/16
SII <- sum((I-Ibarra)^2)
#para I=18.2
D0 <- -193.0 + 15.3*18.2 #estimado
IP1 <- c(D0-2.144787*sqrt(MSRES*(1+1/16+(18.2-Ibarra)^2/SII)),D0+2.144787*sqrt(MSRES*(1+1/16+(18.2-Ibarra)^2/SII)))
#así IP1=(30.08215, 140.83785) se puede notar que el intervalo de confianza tiene un tamaño considerable
#esto se debe a que el modelo no tiene un buen ajuste, por lo tanto sus residuales son grandes
#para I=16.0
D01=-193.0 + 15.3*16.0
IP2 <- c(D01-2.144787*sqrt(MSRES*(1+1/16+(16.0-Ibarra)^2/SII)),D01+2.144787*sqrt(MSRES*(1+1/16+(16.0-Ibarra)^2/SII)))
# así IP2=(-7.399568, 110.999568)
#esto se debe a que el modelo no tiene un buen ajuste, por lo tanto sus residuales son grandes

rstandard(M) #residules internamente studentizados
x <- matrix(c(rep(1,16),16.7, 17.1, 18.2, 18.1, 17.2, 18.2, 16.0, 17.2, 18.0, 17.2, 16.9, 17.1, 18.2, 17.3, 17.5, 16.6),16,2)
w <- solve(t(x)%*%x)
H <- x%*%w%*%t(x)
r1 <- (28.53435)/sqrt(MSRES*(1-0.127461546))

rstudent(M) #residuales externamente studentizados 
sigma1 <- ((t(M$residuals)%*%M$residuals)-(28.53435*1/(1-0.127461546)*28.53435))/13
t1 <- (28.53435)/sqrt(sigma1*(1-0.127461546))

dffits(M)
dffit1 <- t1*sqrt(0.127461546/(1-0.127461546))

#vamos a considerar el modelos sin la primera componente 
y <- c(105, 106, 108, 88, 91, 58, 82, 81, 65, 61, 48, 61, 43, 33, 36)
x <- c(17.1, 18.2, 18.1, 17.2, 18.2, 16.0, 17.2, 18.0, 17.2, 16.9, 17.1, 18.2, 17.3, 17.5, 16.6)

M1 <- lm(y~x)
summary(M1)
MSRES1 <- sum((M1$residuals)^2)/13
dfbeta11 <- (15.3-18.596)/(sqrt(MSRES1*47.215342))
#este es el dfbeta. 

