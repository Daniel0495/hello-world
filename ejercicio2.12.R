temperatura <- c(21, 24, 32, 47, 50, 59, 68, 74, 62, 50, 41, 30)
libras_vapor <- c(185.79, 214.47, 288.03, 424.84, 454.68, 539.03, 621.55, 675.06, 562.03, 452.93, 369.95, 273.98)



plot(temperatura, libras_vapor ,xlab="temperatura", pch=16, ylab="libras_vapor", main = "diagrama de dispersion")
libras_temp <- lm(libras_vapor ~ temperatura)
libras_temp
abline(libras_temp, col ="red", lwd=2)
summary(libras_temp)

# Para hallar la correlación lineal
cor(temperatura, libras_vapor)
cor(temperatura, libras_vapor)^2
prediccion <- predict(libras_temp, interval = c("prediction"))
confianza <- predict(libras_temp, interval = c("confidence"))
prediccion
confianza
qt(0.975, 8)

qqnorm(libras_temp$residuals)
qqline(libras_temp$residuals)
shapiro.test(libras_temp$residuals)
