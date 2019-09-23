#ejercicio 6.13
datos<-table.b12

y<-datos$pitch
x_1<-datos$temp
x_2<-datos$soaktime
x_3<-datos$soakpct
x_4<-datos$difftime
x_5<-datos$diffpct

modelo<-lm(y~x_1+x_2+x_3+x_4+x_5)
summary(modelo)

modelo1<-lm(y~x_2+x_4)
summary(modelo1)

fal<-abs(dffits(modelo1))>2*sqrt(3/32)
abs(dffits(modelo))>2*sqrt(6/32)

y1<-y[-22]
x_11<-x_1[-22]
x_21<-x_2[-22]
x_31<-x_3[-22]
x_41<-x_4[-22]
x_51<-x_5[-22]

modelo2<-lm(y1~x_11+x_21+x_31+x_41+x_51)
summary(modelo2)
p<-c(0,0,0,0,0)

X<-model.matrix(modelo)
XX<-t(X)%*%X
XXinv<-solve(XX)

for (i in 1:5) {
  p[i]<-(modelo$coefficients[i+1]-modelo2$coefficients[i+1])/(0.0022*sqrt(XXinv[i+1,i+1]))
  
}
abs(p)>sqrt(4/32)
p
(modelo$coefficients[2]-modelo2$coefficients[2])/(0.0022*sqrt(XXinv[2,2]))

