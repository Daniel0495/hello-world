pureza <- c(86.91, 89.85, 90.28, 86.34, 92.58, 87.33, 86.29, 91.86, 95.61, 89.86, 96.73, 99.42, 98.66, 96.07, 93.65, 87.31, 95.00, 96.85, 85.20, 90.56)
hidrocarburos <- c(1.02, 1.11, 1.43, 1.11, 1.01, 0.95, 1.11, 0.87, 1.43, 1.02, 1.46, 1.55, 1.55, 1.55, 1.40, 1.15, 1.01, 0.99, 0.95, 0.98)

plot(hidrocarburos, pureza , ylab="pureza", pch=16, xlab="hidrocarburos", main = "diagrama de dispersión")
pureza_hidro <- lm(pureza ~ hidrocarburos)
pureza_hidro
abline(pureza_hidro, col ="red", lwd=2)
summary(pureza_hidro)

# Para hallar la correlación lineal
cor(hidrocarburos, pureza)
cor(hidrocarburos, pureza)^2
prediccion <- predict(pureza_hidro, interval = c("prediction"))
confianza <- predict(pureza_hidro, interval = c("confidence"))
prediccion
confianza
qt(0.975, 8)

qqnorm(demanda_precio$residuals)
qqline(demanda_precio$residuals)
shapiro.test(demanda_precio$residuals)





