library('maxLik')
gen=function(n){
  dn=1:(length(n)-1)
  for (i in 1:(length(n)-1)) {
    dn[i]=n[i+1]-n[i]
  }
  return(dn)
}

Goel_Okumoto1 =function(theta){
  lam = (1-exp(-theta[1]*t))
  a=sum(lam*n)/sum(lam*lam)
  return(-sqrt(sum((n-a*lam)^2))/length(lam))
}

Goel_Okumoto2 =function(theta,x){
  lam = (1-exp(-theta[1]*t))
  lamx=(1-exp(-theta[1]*x))
  a=sum(lam*n)/sum(lam*lam)
  return(a*lamx)
}

G_O1 =function(theta){
  
  lam = (1-(1+theta[1]*t)*exp(-theta[1]*t))
  a=sum(lam*n)/sum(lam*lam)
  return(-sqrt(sum((n-a*lam)^2))/length(lam))
}

G_O2 =function(theta,x){

  lam = (1-(1+theta[1]*t)*exp(-theta[1]*t))
  lamx=(1-(1+theta[1]*x)*exp(-theta[1]*x))
  a=sum(lam*n)/sum(lam*lam)
  return(a*lamx)
}


Hossain_Dahiya1 =function(theta){

  lam = (1-exp(-theta[1]*t))/(1+theta[2]*exp(-theta[1]*t))
  a=sum(lam*n)/sum(lam*lam)
  return(-sqrt(sum((n-a*lam)^2))/length(lam))
}

Hossain_Dahiya2 =function(theta,x){

  lam = (1-exp(-theta[1]*t))/(1+theta[2]*exp(-theta[1]*t))
  lamx=(1-exp(-theta[1]*x))/(1+theta[2]*exp(-theta[1]*x))
  a=sum(lam*n)/sum(lam*lam)
  return(a*lamx)
}


Gompertz1 =function(theta){

  lam = (theta[1]^(theta[2]^t))
  a=sum(lam*n)/sum(lam*lam)
  return(-sqrt(sum((n-a*lam)^2))/length(lam))
}

Gompertz2 =function(theta,x){

  lam = (theta[1]^(theta[2]^t))
  lamx= (theta[1]^(theta[2]^x))
  a=sum(lam*n)/sum(lam*lam)
  return(a*lamx)
}


Pareto1 =function(theta){


  lam = (1-(1+t/theta[2])^(1-theta[1]))
  a=sum(lam*n)/sum(lam*lam)
  return(-sqrt(sum((n-a*lam)^2))/length(lam))
}

Pareto2 =function(theta,x){

  lam = (1-(1+t/theta[2])^(1-theta[1]))
  lamx=(1-(1+x/theta[2])^(1-theta[1]))
  a=sum(lam*n)/sum(lam*lam)
  return(a*lamx)
}

Weibull1 =function(theta){

  lam = (1-exp(-theta[1]*t^theta[2]))
  a=sum(lam*n)/sum(lam*lam)
  return(-sqrt(sum((n-a*lam)^2))/length(lam))
}

Weibull2 =function(theta,x){

  lam = (1-exp(-theta[1]*t^theta[2]))
  lamx= (1-exp(-theta[1]*x^theta[2]))
  a=sum(lam*n)/sum(lam*lam)
  return(a*lamx)
}


Yamada_Exp1 =function(theta){

  lam = (1-exp(-theta[1]*(1-exp(-theta[2]*t))))
  a=sum(lam*n)/sum(lam*lam)
  return(-sqrt(sum((n-a*lam)^2))/length(lam))
}

Yamada_Exp2 =function(theta,x){

  lam = (1-exp(-theta[1]*(1-exp(-theta[2]*t))))
  lamx=(1-exp(-theta[1]*(1-exp(-theta[2]*x))))
  a=sum(lam*n)/sum(lam*lam)
  return(a*lamx)
}

Yamada_Raleigh1 =function(theta){
  lam = (1-exp(-theta[1]*(1-exp(-theta[2]*t^2/2))))
  a=sum(lam*n)/sum(lam*lam)
  return(-sqrt(sum((n-a*lam)^2))/length(lam))
}

Yamada_Raleigh2 =function(theta,x){

  lam = (1-exp(-theta[1]*(1-exp(-theta[2]*t^2/2))))
  lamx=(1-exp(-theta[1]*(1-exp(-theta[2]*x^2/2))))
  a=sum(lam*n)/sum(lam*lam)
  return(a*lamx)
}

t0=c(0, 32.4375, 64.875, 97.3125, 129.75, 162.1875, 194.625, 227.0625, 259.5, 291.9375, 324.375, 356.8125, 389.25, 421.6875, 454.125, 486.5625, 519.0, 575.125, 631.25, 687.375, 743.5, 799.625, 855.75, 911.875, 968.0, 1122.0, 1276.0, 1430.0, 1507.1666666666667, 1584.3333333333333, 1661.5, 1738.6666666666667, 1815.8333333333335, 1893.0, 1967.625, 2042.25, 2116.875, 2191.5, 2266.125, 2340.75, 2415.375, 2490.0, 2561.0, 2632.0, 2703.0, 2774.0, 2845.0, 2916.0, 2987.0, 3058.0, 3171.4, 3284.8, 3398.2, 3511.6, 3625.0, 3824.25, 4023.5, 4222.75, 4422.0, 4494.363636363636, 4566.727272727273, 4639.090909090909, 4711.454545454545, 4783.818181818182, 4856.181818181818, 4928.545454545454, 5000.909090909091, 5073.272727272727, 5145.636363636364, 5218.0, 5318.833333333333, 5419.666666666667, 5520.5, 5621.333333333333, 5722.166666666667, 5823.0, 5942.333333333333, 6061.666666666667, 6181.0, 6300.333333333333, 6419.666666666667, 6539.0, 6647.8, 6756.6, 6865.4, 6974.2, 7083.0, 7184.0, 7285.0, 7386.0, 7487.0, 7606.666666666667, 7726.333333333333, 7846.0, 7965.666666666667, 8085.333333333333, 8205.0, 8384.5, 8564.0, 8923.0, 9282.0)
n0=1:length(t0)-1

n1 = c(0,16,24,27,33,41,49,54,58,69,75,81,86,90,93,96,98,99,100,100,100)
t1 = c(0,519,968,1430,1893,2490,3058,3625,4422,5218,5823,6539,7083,7487,7846,8205,8564,8923,9282,9641,10000)
n2 = c(0,13,18,26,34,40,48,61,75, 84, 89, 95, 100, 104, 110 ,112, 114, 117, 118, 120)
t2 = c(0,384,1186,1471,2236,2772,2967,3812,4880,6104,6634,7229,8072,8484,8847,9253,9712,10083,10174,10272)
n3 = c(0,6, 9, 13, 20, 28, 40, 48, 54, 57, 59, 60, 61)
t3 = c(0,162, 499, 715, 1137, 1799 ,2438 ,2818, 3574 ,4234, 4680 ,4955, 5053)
n4 = c(0,1,3,8,9,11,16,19,25,27,29,32,32,36,38,39,39,41,42,42)
t4 = c(0,254,788,1054,1393,2216,2880,3593,4281,5180,6003,7621,8783,9604,10064,10560,11008,11237,11243,11305)
n5 = c(n2,n3[2:13]+120)
t5 = c(t2,t3[2:13]+10272)
plot(t5,n5)

# xvec <- rnorm(100,-3,4)
# fn <- function(theta) {sum ( 0.5*(xvec - theta[1])^2/theta[2] + 0.5* log(theta[2]) )}
# summ =nlm(fn, theta <- c(1
#                          ,7), hessian=TRUE)
# print(summ)

unlockBinding("t", globalenv())
t=xdat
n<<-1:length(t)
dn <<- gen(n)
l <<- length(t)
# s = Goel_Okumoto(c(107,0.1))
A=matrix(c(0,1,1,0),2,2)
B=matrix(c(-0.000000001,-.0000000001),2,1)
A1=matrix(c(1),1,1)
B1=matrix(-0.0001,1,1)
plot(t,n)
summ =maxLik(Goel_Okumoto, start = c(110,0.002),constraints=list(ineqA=A,ineqB=B))
print(1)
summ1 =maxLik(Goel_Okumoto1, start = c(0.002))
print(summ1)
lines(seq(0,t[l],by=1),Goel_Okumoto2(summ1$estimate,seq(0,t[l],by=1)),col='red')
summ2 =maxLik(G_O1, start = c(0.002),constraints=list(ineqA=A1,ineqB=B1))
print(2)
print(summ2)
lines(seq(0,t[l],by=1),G_O2(summ2$estimate,seq(0,t[l],by=1)),col='blue')
summ3 =maxLik(Hossain_Dahiya1, start = c(0.002,.2),constraints=list(ineqA=A,ineqB=B))
print(3)
print(summ3)
lines(seq(0,t[l],by=1),Hossain_Dahiya2(summ3$estimate,seq(0,t[l],by=1)),col='yellow')
print(4)

summ4 =maxLik(Gompertz1, start = c(0.001,0.9999),constraints=list(ineqA=A,ineqB=B))
lines(seq(0,t[l],by=1),Gompertz2(summ4$estimate,seq(0,t[l],by=1)),col='purple')
print(summ4)

summ5 =maxLik(Pareto1, start = c(0.002,.2),constraints=list(ineqA=A,ineqB=B))
print(5)
print(summ5)
lines(seq(0,t[l],by=1),Pareto2(summ5$estimate,seq(0,t[l],by=1)),col='green')

summ6 =maxLik(Weibull1, start = c(0.002,.2),constraints=list(ineqA=A,ineqB=B))
print(6)
print(summ6)
lines(seq(0,t[l],by=1),Weibull2(summ6$estimate,seq(0,t[l],by=1)),col='pink')

summ7 =maxLik(Yamada_Exp1, start = c(0.02,.002),constraints=list(ineqA=A,ineqB=B))
print(7)
print(summ7)
lines(seq(0,t[l],by=1),Yamada_Exp2(summ7$estimate,seq(0,t[l],by=1)),col='brown')

summ8 =maxLik(Yamada_Raleigh1, start = c(0.15,.0000015),constraints=list(ineqA=A,ineqB=B))
print(8)
print(summ8)
lines(seq(0,t[l],by=1),Yamada_Raleigh2(summ8$estimate,seq(0,t[l],by=1)),col='orange')

