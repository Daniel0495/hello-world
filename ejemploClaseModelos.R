
precio <- c(20, 19, 18, 18, 15, 14, 12, 11, 10, 9)
demanda <- c(5, 7, 8, 10, 11, 13, 14, 15, 17, 20)
plot(precio,demanda,xlab="precio", pch=16, ylab="cantidad vendida", main = "diagrama de dispersiÃ³n")
demanda_precio <- lm(demanda ~ precio)
demanda_precio
abline(demanda_precio, col ="red", lwd=2)
summary(demanda_precio)
# Para hallar la correlación lineal
cor(precio, demanda)
cor(precio, demanda)^2
prediccion <- predict(demanda_precio, interval = c("prediction"))
confianza <- predict(demanda_precio, interval = c("confidence"))
prediccion
confianza
qt(0.975, 8)

qqnorm(demanda_precio$residuals)
qqline(demanda_precio$residuals)
shapiro.test(demanda_precio$residuals)
