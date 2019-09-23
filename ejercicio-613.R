tabla <- table.b12
malos <- c()
y <- tabla$pitch
x1 <- tabla$temp
x2 <- tabla$soaktime
x3 <- tabla$soakpct
x4 <- tabla$difftime
x5 <- tabla$diffpct

modelo <- lm(y~x1+x2+x4)
summary(modelo)
abs(dffits(modelo)) > 2*sqrt(4/32)

y1 <- y[-6]
x11 <- x1[-6]    
x21 <- x2[-6]
x41 <- x4[-6]

modelo1 <-lm(y1~x11+x21+x41) 
summary(modelo1)

X <- model.matrix(modelo)
xinv <- solve(t(X)%*%X)
sisma1 <- sum((modelo1$residuals)^2)*27
p <- c(0,0,0)

for (i in 1:3) {
  p[i] <- (modelo$coefficients[i+1] - modelo1$coefficients[i+1])/sqrt(xinv[i+1,i+1]*sisma1)
}
p
abs(p) > 2/sqrt(32)
#quitamos el dato 6

#___________________________________________
y2 <- y[-28]
x12 <- x1[-28]    
x22 <- x2[-28]
x42 <- x4[-28]

modelo2 <-lm(y2~x12+x22+x42) 
summary(modelo1)

X <- model.matrix(modelo)
xinv <- solve(t(X)%*%X)
sisma2 <- sum((modelo2$residuals)^2)*27
p2 <- c(0,0,0)

for (i in 1:3) {
  p2[i] <- (modelo$coefficients[i+1] - modelo2$coefficients[i+1])/sqrt(xinv[i+1,i+1]*sisma2)
}
p2
abs(p2) > 2/sqrt(32)
#quitamos el dato 28




#___________________________________________
y3 <- y[-29]
x13 <- x1[-29]    
x23 <- x2[-29]
x43 <- x4[-29]

modelo3 <-lm(y3~x13+x23+x43) 


X <- model.matrix(modelo)
xinv <- solve(t(X)%*%X)
sisma3 <- sum((modelo3$residuals)^2)*27
p3 <- c(0,0,0)

for (i in 1:3) {
  p3[i] <- (modelo$coefficients[i+1] - modelo3$coefficients[i+1])/sqrt(xinv[i+1,i+1]*sisma3)
}
p3
abs(p3) > 2/sqrt(32)
#quitamos el dato 29




#___________________________________________
y4 <- y[-32]
x14 <- x1[-32]    
x24 <- x2[-32]
x44 <- x4[-32]

modelo4 <-lm(y4~x14+x24+x44) 


X <- model.matrix(modelo)
xinv <- solve(t(X)%*%X)
sisma4 <- sum((modelo4$residuals)^2)*27
p4 <- c(0,0,0)

for (i in 1:3) {
  p4[i] <- (modelo$coefficients[i+1] - modelo4$coefficients[i+1])/sqrt(xinv[i+1,i+1]*sisma4)
}
p4
abs(p4) > 2/sqrt(32)
#quitamos el dato 32



#___________________________________________
yr <- (((y[-32])[-29])[-28])[-6]
x1r <- (((x1[-32])[-29])[-28])[-6]    
x2r <- (((x2[-32])[-29])[-28])[-6]
x4r <- (((x4[-32])[-29])[-28])[-6]

modelor <-lm(yr~x1r+x2r+x4r) 
summary(modelor)
shapiro.test(modelor$residuals)
shapiro.test(modelo$residuals)
