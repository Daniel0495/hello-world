em <- function(R, N) {
  #set.seed(200)
  mu=2
  sigma=1
  Xzero=1
  T=1
 # N=2^8
  dt=1/N
  dw=sqrt(dt)*rnorm(N)
  w=cumsum(dw)
  t=seq(dt,T,by=dt)
  t1=seq(0,T,by=dt)
  Xtrue=Xzero*exp((mu-0.5*sigma^2)*t+sigma*w)
  #R=4
  Dt=R*dt
  L=N/R
  Xem=0
  Xtemp=Xzero
  for (j in 1:L) {
    Winc=sum(dw[(R*(j-1)+1):(R*j)])
    Xtemp=Xtemp + Dt*mu*Xtemp + sigma*Xtemp*Winc
    Xem[j]=Xtemp} 
  t2=seq(0,T,by=Dt)
  emerr=abs(Xem[length(Xem)]-Xtrue[length(Xtrue)])

plot(t1,c(Xzero,Xtrue),type="l",col="blue",xlab="t",ylab="X")
lines(t2,c(Xzero,Xem),col="red",lty=2) 
title(main=R, col.main="red", font.main=4)

return(emerr)
}

