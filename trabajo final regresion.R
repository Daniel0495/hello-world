
#Verificamos la normalidad de la variable respuesta
y <- renault_cars$Precio
shapiro.test(y)
valores_p <- c(shapiro.test(y)$p.value, ad.test(y)$p.value, pearson.test(y)$p.value, cvm.test(y)$p.value)
valores_p


#NO hay normalidad, entonces aplicamos la transformación 1/x
z <- 1/(y+1)

valores_p <- c(shapiro.test(z)$p.value, 
               ad.test(z)$p.value, pearson.test(z)$p.value,
               cvm.test(z)$p.value)
valores_p
par(mfrow = c(1,1) )
hist(z, freq = FALSE, col="aliceblue", 
     main = "Histograma variable transformada",
     xlab = "Z")
lines(density(z), lwd = 2, col="red3")
curve(dnorm(x, mean = mean(z), sd=sd(z)), add = TRUE, 
      from = 2.072754e-08, to = 6.711409e-08, lwd =2, col ="royalblue" 
        )
boxplot(z, horizontal = TRUE, main ="Boxplot Z",xlab="")

#Tomamos significancia = 0.03
#_________________________________________________________________________________

#extraemos variables:
V <- renault_cars$Version
A <- renault_cars$Año
K <- renault_cars$Kilometraje
C <- renault_cars$Color
T <- renault_cars$Transmision
D <- renault_cars$Direccion

#M <- step(modelo1, direction = "backward", trace = 1)


modelo2 <- lm(z~V+A+T+D)
summary(modelo2)
anova(modelo2)

#Kilometraje es significativo, sin embargo, kilometraje
#y años son altamente correlacionados

#el mejor modelo hasta es el modelo 2. 

par(mfrow = c(2,2), mar = c(4,4,2,1.5))
plot(modelo2)

valores_pr <- c(shapiro.test(modelo2$residuals)$p.value,
                ad.test(modelo2$residuals)$p.value, 
                pearson.test(modelo2$residuals)$p.value,
                cvm.test(modelo2$residuals)$p.value)
valores_pr
#__________________________________________________

ols_plot_dffits(modelo2)


#ANALISIS DE RESIDUALES

dff <- dffits(modelo2)
abs(dff) > 2*sqrt(6/427)
indices <- c()
for (i in 1:426) {
  if (abs(dff[i]) > 2*sqrt(6/427) ) 
  {indices <- c(indices, i)}
  }
dff
indi <- c()

for (i in 75:1) {
  indi <- c(indi, indices[i])
  
}



#______________________________________________________
y <- renault_cars$Precio
z <- 1/(y+1)
V <- renault_cars$Version
A <- renault_cars$Año
K <- renault_cars$Kilometraje
C <- renault_cars$Color
T <- renault_cars$Transmision
D <- renault_cars$Direccion



for (i in indi) {
  z <- z[-i] 
  V <- V[-i]
  A <- A[-i]
  T <- T[-i]
  D <- D[-i]
  y <- y[-i]
}

modelo_final <- lm(z~V+A+T+D)

modelo_final
summary(modelo_final)




par(mfrow = c(2,2), mar = c(4,4,2,1.5))
plot(modelo_final)
shapiro.test(resid(modelo_final))


yn <- (1-z)/z

coeficientes <- (1-modelo_final$coefficients)/modelo_final$coefficients
