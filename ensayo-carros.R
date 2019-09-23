precio <- c(20, 19, 18, 18, 15, 14, 12, 11, 10, 9) #en miles de millones
kilometros <- c(5, 7, 8, 10, 11, 13, 14, 15, 17, 20) #en miles de kilo
transmisión <- c(1,0,1,0,1,0,1,0,1,0)   #donde 1 es automática, y 0 mecánica

modelo <- lm(precio~kilometros + transmisión)
modelo
plot(kilometros, precio)
abline(modelo)
summary(modelo)
anova(modelo)
#b0gorro = 25.3330
#b1gorro = -0.8564    # modelo: y = 25.3330 - 0.8564x1 - 0.9128x2
#b2gorro = -0.9128


#modelo fijando la transmisión mecánica:

modelo2 <- lm(precio[transmisión==1]~kilometros[transmisión==1])
modelo2
modelo

summary(modelo)
summary(modelo2)


plot(kilometros, precio)
abline(modelo)




plot(kilometros, precio)
abline(modelo2)


modelo3<- lm(precio~kilometros)
