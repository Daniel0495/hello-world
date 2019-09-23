library("MPV")
data("table.b4")
table.b4
y <- table.b4$y 
x1 <- table.b4$x1
x2 <- table.b4$x2
x3 <- table.b4$x3
x4 <- table.b4$x4
x5 <- table.b4$x5
x6 <- table.b4$x6
x7 <- table.b4$x7
x8 <- table.b4$x8
x9 <- table.b4$x9

#verificamos normalidad de la variable respuesta:
shapiro.test(y) # vp =0.1 no se rechaza normalidad
hist(y)
plot(density(y))
boxplot(y, horizontal = TRUE)



# miramos modelo general
modelo1 <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9)
summary(modelo1)

cor(data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9))

#viendo correlaciones, x7 y x6 grande, pero x6 mas con las otras que x7- se descarta x6
#se espera tener x1 en el modleo
modelo_6 <- lm(y~x1+x2+x3+x4+x5+x7+x8+x9)
summary(modelo_6)


anova(modelo_6, modelo1)
cor(data.frame(y,x1,x2,x3,x4,x5,x7,x8,x9))



modelo_64 <- lm(y~x1+x2+x3+x5+x7+x8+x9)
anova(modelo_64, modelo_6)
summary(modelo_64)

#a pesar de que x4 esta muy correlacionado con y, vimos que tambien esta muy 
#correlacionado con las demás variables. El test de anova nos permite no rechazar
#la hipotesis nula de que b_4 es cero


modelo_64 <- lm(y~x1+x2+x3+x5+x7+x8+x9)
cor(data.frame(y,x1,x2,x3,x5,x7,x8,x9))

modelo_642 <- lm(y~x1+x3+x5+x7+x8+x9)
summary(modelo_642)
modelo_643 <- lm(y~x1+x2+x5+x7+x8+x9)
summary(modelo_643)


#comparando los dos modelos anteriores vemos que al sacar x2 aumenta la variabilidad
#es decir, bajo el r2ad. veamos el anova
anova(modelo_642, modelo_64)
anova(modelo_643, modelo_64)

#las pruebas anova nos confirman que no debemos eliminar x2, mas sí x3


modelo_643 <- lm(y~x1+x2+x5+x7+x8+x9)
cor(data.frame(y,x1,x2,x5,x7,x8,x9))
summary(modelo_643)


# x7 y x9 tienen muy poca correlacion con y. Veamos con anova si son significativos
modelo_6437 <- lm(y~x1+x2+x5+x8+x9)
summary(modelo_6437)
modelo_6439 <- lm(y~x1+x2+x5+x7+x8)
summary(modelo_6439)

#el sumary nos da que quitando x9 disminuye el raj, mientras que si quitamos el 
# x7 aumenta. veamo el anova

anova(modelo_6437, modelo_643)
anova(modelo_6439, modelo_643)


# juntando resultado anova y summary, decidimos quitar el x7
modelo_6437 <- lm(y~x1+x2+x5+x8+x9)
summary(modelo_6437)
cor(data.frame(y,x1,x2,x5,x8,x9))

#el x5 esta muy poco significativo, es el menor. Y además, es el que está
# mas correlacionado con x1 y x2

modelo_64375 <- lm(y~x1+x2+x8+x9)
summary(modelo_64375)

modelo_64375_26 <- lm(y~x1+x2+x8+x9+x6*x6+x6)
summary(modelo_64375_26)

