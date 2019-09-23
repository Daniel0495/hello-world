# EJERCICIO. Ajustar un modelo con interacción entre regresoras. Los datos son:
x1 <- c(1, 1, -1, -1, 0, 0, 0, 0, 0, 1.5, -1.5)
x2 <- c(-1, 1, 1, -1, 0, 0, 0, 1.5, -1.5, 0, 0)
w <- c(83, 113, 92, 82, 100, 96, 98, 95, 80, 100, 92)
x1x2 <- x1*x2
x22 <- x2*x2
x11 <- x1*x1

#___________________________________
shapiro.test(w)
#da un valor p = 0.5104.  La hipótesis nula es que w es normal, no hay evidencias
# para rechazarla.

cor(data.frame(x1,x2))
# aparentemente x1 y x2 son totalmente incorrelacionadas, por lo que ambas inicial/
# deberían tenerse en cuenta en el modelo.



#______________________________
modelo1 <- lm(w~x1+x2)
summary(modelo1)
 # El resumen del modelo 1 indica que es explicativo, sin embargo la variable x1,
# no es significativa.
modelo1aov <- aov ( w ~ x1+x2) 
summary(modelo1aov)
anova(modelo1)

modelo11 <- lm(w~x2)
anova(modelo11, modelo1)
summary.aov(modelo1)
# Anova nos confirma que no es significativo el aporte de x1


# Nos quedamos solo con x2, pero analizamos aportes cuadráticos y conjunto:
#_____________________________
modelo2 <- lm(w~x2+x1x2+x22+x11)
summary(modelo2)
# El resumen del modelo2 indica que x1x2 y x11 no es significativos. Hacemos prueba 
# de hipótesis para verificar esto:

modelo21 <-lm(w~x2+x22)
summary(modelo21)

SSres01 <- ((6.316)^2)*8
SSres1 <- ((5.971)^2)*6

f1 <- ((SSres01-SSres1)*6)/(SSres1*2)
f1
# no se rechaza la hipótesis nula, lo que confirma que x1x2 y x11 no son 
# significativos para explicar w

anova(modelo21,modelo2)
#anova confirma que las variables x1x2 y x11 no son significativas
#____________________________
modelo3 <- lm(w~x2+x22)
summary(modelo3)




#sum(modelo3$residuals^2) > sum(modelo_bueno$residuals^2)  :(
#___________________________________

#si iniciamos al revés, tomamos como modelo inicial el de todas las interacciones:

modelo_intento <- lm(w~x1+x2+x1x2+x22+x11)
summary(modelo_intento)
#el resumen nos indica que x11 no es significativa, y probablemente x1x2 tampoco
# ensayamos con:

modelo_bueno <- lm(w~x1+x2+x1x2+x22)
modelo_bueno
summary(modelo_bueno)
# y ese si funciona :( :( 




