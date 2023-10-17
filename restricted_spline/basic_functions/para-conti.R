library('maxLik')
gen=function(n){
  dn=1:(length(n)-1)
  for (i in 1:(length(n)-1)) {
    dn[i]=n[i+1]-n[i]
  }
  return(dn)
}
Goel_Okumoto0 =function(theta){
  dn=gen(n)
  dmu=gen(theta[1]*(1-exp(-theta[2]*t1)))
  mun=theta[1]*(1-exp(-theta[2]*t1[l]))
  return(sum(dn*log(dmu))-mun)
}

Goel_Okumoto01 =function(theta){
  dn=gen(n)
  lamn=1-exp(-theta[1]*t1[l])
  lam = (1-exp(-theta[1]*t1))
  a=n[l]/lamn
  grad=(a*theta[1]*exp(-theta[1]*t1))[2:length(t1)]
  mun=n[l]
  return(sum(log(grad))-mun)
}




Goel_Okumoto02 =function(theta,x){
  dn=gen(n)
  lamn=1-exp(-theta[1]*t1[l])
  lam = (1-exp(-theta[1]*t1))
  lamx=(1-exp(-theta[1]*x))
  a=n[l]/lamn
  dmu=gen(a*lam)
  mun=n[l]
  ans = new.env()
  ans$func = a*lamx
  ans$a=a
  ans$ll=sum(dn*log(dmu))-mun
  return(ans)
}

G_O0 =function(theta){
  dn=gen(n)
  dmu=gen(theta[1]*(1-(1+theta[2]*t1)*exp(-theta[2]*t1)))
  mun=theta[1]*(1-(1+theta[2]*t1[l])*exp(-theta[2]*t1[l]))
  sum(dn*log(dmu))-mun
}

G_O01 =function(theta){
  dn=gen(n)
  lamn=(1-(1+theta[1]*t1)*exp(-theta[1]*t1[l]))
  lam = (1-(1+theta[1]*t1)*exp(-theta[1]*t1))
  a=n[l]/lamn
  grad=(a*theta[1]^2*t1*exp(-theta[1]*t1))[2:length(t1)]
  mun=n[l]
  return(sum(log(grad))-mun)
}

G_O02 =function(theta,x){
  dn=gen(n)
  lamn=(1-(1+theta[1]*t1[l])*exp(-theta[1]*t1[l]))
  lam = (1-(1+theta[1]*t1)*exp(-theta[1]*t1))
  lamx=(1-(1+theta[1]*x)*exp(-theta[1]*x))
  a=n[l]/lamn
  dmu=gen(a*lam)
  mun=n[l]
  ans = new.env()
  ans$func = a*lamx
  ans$a=a
  ans$ll=sum(dn*log(dmu))-mun
  return(ans)
}

Hossain_Dahiya0 =function(theta){
  dn=gen(n)
  dmu=gen(theta[1]*(1-exp(-theta[2]*t1))/(1+theta[3]*exp(-theta[2]*t1)))
  mun=theta[1]*(1-exp(-theta[2]*t1[l]))/(1+theta[3]*exp(-theta[2]*t1[l]))
  sum(dn*log(dmu))-mun
}
Hossain_Dahiya01 =function(theta){
  dn=gen(n)
  lamn=(1-exp(-theta[1]*t1[l]))/(1+theta[2]*exp(-theta[1]*t1[l]))
  lam = (1-exp(-theta[1]*t1))/(1+theta[2]*exp(-theta[1]*t1))
  a=n[l]/lamn
  grad=(a*(1+theta[2])*theta[1]*exp(-theta[1]*t1)/(1+theta[2]*exp(-theta[1]*t1))^2)[2:length(t1)]
  mun=n[l]
  return(sum(log(grad))-mun)
}

Hossain_Dahiya02 =function(theta,x){
  dn=gen(n)
  lamn=(1-exp(-theta[1]*t1[l]))/(1+theta[2]*exp(-theta[1]*t1[l]))
  lam = (1-exp(-theta[1]*t1))/(1+theta[2]*exp(-theta[1]*t1))
  lamx=(1-exp(-theta[1]*x))/(1+theta[2]*exp(-theta[1]*x))
  a=n[l]/lamn
  dmu=gen(a*lam)
  mun=n[l]
  ans = new.env()
  ans$func = a*lamx
  ans$a=a
  ans$ll=sum(dn*log(dmu))-mun
  return(ans)
}


Gompertz0 =function(theta){
  dn=gen(n)
  dmu=gen(theta[1]*(theta[2]^(theta[3]^t1)))
  mun=theta[1]*(theta[2]^(theta[3]^t1[l]))
  sum(dn*log(dmu))-mun
}

Gompertz01 =function(theta){
  dn=gen(n)
  lamn=(theta[1]^(theta[2]^t1[l]))
  lam = (theta[1]^(theta[2]^t1))
  a=n[l]/lamn
  grad=(a*log(theta[1])*log(theta[2])*theta[2]^t1*(theta[1]^(theta[2]^t1)) )[2:length(t1)]
  mun=n[l]
  return(sum(log(grad))-mun)
}

# Gompertz01(c(0.0482,0.990))
Gompertz02 =function(theta,x){
  dn=gen(n)
  lamn=(theta[1]^(theta[2]^t1[l]))
  lam = (theta[1]^(theta[2]^t1))
  lamx=(theta[1]^(theta[2]^x))
  a=n[l]/lamn
  dmu=gen(a*lam)
  mun=n[l]
  ans = new.env()
  ans$func = a*lamx
  ans$a=a
  ans$ll=sum(dn*log(dmu))-mun
  return(ans)
}


Pareto0 =function(theta){
  dn=gen(n)
  dmu=gen(theta[1]*(1-(1+t1/theta[2])^(-theta[1])))
  mun=theta[1]*(1-(1+t1[l]/theta[2])^(-theta[1]))
  sum(dn*log(dmu))-mun
}
Pareto01 =function(theta){
  dn=gen(n)
  lamn=(1-(1+t1[l]/theta[2])^(-theta[1]))
  lam = (1-(1+t1/theta[2])^(-theta[1]))
  a=n[l]/lamn
  grad=((a*(theta[1])*(1+t1/theta[2])^(-1-theta[1])/theta[2])[2:length(t1)])
  mun=n[l]
  return(sum(log(grad))-mun)
}
# Pareto01(c(0.002,0.2) )
Pareto02 =function(theta,x){
  dn=gen(n)
  lamn=(1-(1+t1[l]/theta[2])^(-theta[1]))
  lam = (1-(1+t1/theta[2])^(-theta[1]))
  lamx=(1-(1+x/theta[2])^(-theta[1]))
  a=n[l]/lamn
  dmu=gen(a*lam)
  mun=n[l]
  ans = new.env()
  ans$func = a*lamx
  ans$a=a
  ans$ll=sum(dn*log(dmu))-mun
  return(ans)
}


Weibull0 =function(theta){
  dn=gen(n)
  dmu=gen(theta[1]*(1-exp(-theta[2]*t1^theta[3])))
  mun=theta[1]*(1-exp(-theta[2]*t1[l]^theta[3]))
  sum(dn*log(dmu))-mun
}
Weibull01 =function(theta){
  dn=gen(n)
  lamn=(1-exp(-theta[1]*t1[l]^theta[2]))
  lam = (1-exp(-theta[1]*t1^theta[2]))
  a=n[l]/lamn
  grad=((a*theta[1]*theta[2]*t1^(theta[2]-1)*exp(-theta[1]*t1^theta[2]))[2:length(t1)])
  mun=n[l]
  return(sum(log(grad))-mun)
}

# Weibull01(c(0.00069,0.924))

Weibull02 =function(theta,x){
  dn=gen(n)
  lamn=(1-exp(-theta[1]*t1[l]^theta[2]))
  lam = (1-exp(-theta[1]*t1^theta[2]))
  lamx= (1-exp(-theta[1]*x^theta[2]))
  a=n[l]/lamn
  dmu=gen(a*lam)
  mun=n[l]
  ans = new.env()
  ans$func = a*lamx
  ans$a=a
  ans$ll=sum(dn*log(dmu))-mun
  return(ans)
}


Yamada_Exp0 =function(theta){
  dn=gen(n)
  dmu=gen(theta[1]*(1-exp(-theta[2]*(1-exp(-theta[3]*t1)))))
  mun=theta[1]*(1-exp(-theta[2]*(1-exp(-theta[3]*t1[l]))))
  sum(dn*log(dmu))-mun
}
Yamada_Exp01 =function(theta){
  dn=gen(n)
  lamn=(1-exp(-theta[1]*(1-exp(-theta[2]*t1[l]))))
  lam = (1-exp(-theta[1]*(1-exp(-theta[2]*t1))))
  a=n[l]/lamn
  grad=(a*theta[1]*theta[2]*exp(-theta[2]*t1)*exp(-theta[1]*(1-exp(-theta[2]*t1))))[2:length(t1)]
  mun=n[l]
  return(sum(log(grad))-mun)
}


# Yamada_Exp01(c(0.002,0.2))
Yamada_Exp02 =function(theta,x){
  dn=gen(n)
  lamn=(1-exp(-theta[1]*(1-exp(-theta[2]*t1[l]))))
  lam = (1-exp(-theta[1]*(1-exp(-theta[2]*t1))))
  lamx=(1-exp(-theta[1]*(1-exp(-theta[2]*x))))
  a=n[l]/lamn
  dmu=gen(a*lam)
  mun=n[l]
  ans = new.env()
  ans$func = a*lamx
  ans$a=a
  ans$ll=sum(dn*log(dmu))-mun
  return(ans)
}


Yamada_Raleigh0 =function(theta){
  dn=gen(n)
  dmu=gen(theta[1]*(1-exp(-theta[2]*(1-exp(-theta[3]*t1^2/2)))))
  mun=theta[1]*(1-exp(-theta[2]*(1-exp(-theta[3]*t1[l]^2/2))))
  sum(dn*log(dmu))-mun
}
Yamada_Raleigh01 =function(theta){
  dn=gen(n)
  lamn=(1-exp(-theta[1]*(1-exp(-theta[2]*t1[l]^2/2))))
  lam = (1-exp(-theta[1]*(1-exp(-theta[2]*t1^2/2))))
  a=n[l]/lamn
  grad=(a*theta[1]*theta[2]*t1*exp(-theta[2]*t1^2/2)*exp(-theta[1]*(1-exp(-theta[2]*t1^2/2))))[2:length(t1)]
  mun=n[l]
  return(sum(log(grad))-mun)
}

Yamada_Raleigh02 =function(theta,x){
  dn=gen(n)
  lamn=(1-exp(-theta[1]*(1-exp(-theta[2]*t1[l]^2/2))))
  lam = (1-exp(-theta[1]*(1-exp(-theta[2]*t1^2/2))))
  lamx=(1-exp(-theta[1]*(1-exp(-theta[2]*x^2/2))))
  a=n[l]/lamn
  dmu=gen(a*lam)
  mun=n[l]
  ans = new.env()
  ans$func = a*lamx
  ans$a=a
  ans$ll=sum(dn*log(dmu))-mun
  return(ans)
}


# t0=c(0, 32.4375, 64.875, 97.3125, 129.75, 162.1875, 194.625, 227.0625, 259.5, 291.9375, 324.375, 356.8125, 389.25, 421.6875, 454.125, 486.5625, 519.0, 575.125, 631.25, 687.375, 743.5, 799.625, 855.75, 911.875, 968.0, 1122.0, 1276.0, 1430.0, 1507.1666666666667, 1584.3333333333333, 1661.5, 1738.6666666666667, 1815.8333333333335, 1893.0, 1967.625, 2042.25, 2116.875, 2191.5, 2266.125, 2340.75, 2415.375, 2490.0, 2561.0, 2632.0, 2703.0, 2774.0, 2845.0, 2916.0, 2987.0, 3058.0, 3171.4, 3284.8, 3398.2, 3511.6, 3625.0, 3824.25, 4023.5, 4222.75, 4422.0, 4494.363636363636, 4566.727272727273, 4639.090909090909, 4711.454545454545, 4783.818181818182, 4856.181818181818, 4928.545454545454, 5000.909090909091, 5073.272727272727, 5145.636363636364, 5218.0, 5318.833333333333, 5419.666666666667, 5520.5, 5621.333333333333, 5722.166666666667, 5823.0, 5942.333333333333, 6061.666666666667, 6181.0, 6300.333333333333, 6419.666666666667, 6539.0, 6647.8, 6756.6, 6865.4, 6974.2, 7083.0, 7184.0, 7285.0, 7386.0, 7487.0, 7606.666666666667, 7726.333333333333, 7846.0, 7965.666666666667, 8085.333333333333, 8205.0, 8384.5, 8564.0, 8923.0, 9282.0)
# n0=1:length(t0)-1
# n1 = c(0,16,24,27,33,41,49,54,58,69,75,81,86,90,93,96,98,99,100,100,100)
# t1 = c(0,519,968,1430,1893,2490,3058,3625,4422,5218,5823,6539,7083,7487,7846,8205,8564,8923,9282,9641,10000)
# n2 = c(0,13,18,26,34,40,48,61,75, 84, 89, 95, 100, 104, 110 ,112, 114, 117, 118, 120)
# t2 = c(0,384,1186,1471,2236,2772,2967,3812,4880,6104,6634,7229,8072,8484,8847,9253,9712,10083,10174,10272)
# n3 = c(0,6, 9, 13, 20, 28, 40, 48, 54, 57, 59, 60, 61)
# t3 = c(0,162, 499, 715, 1137, 1799 ,2438 ,2818, 3574 ,4234, 4680 ,4955, 5053)
# n4 = c(0,1,3,8,9,11,16,19,25,27,29,32,32,36,38,39,39,41,42,42)
# t4 = c(0,254,788,1054,1393,2216,2880,3593,4281,5180,6003,7621,8783,9604,10064,10560,11008,11237,11243,11305)
# n5 = c(n2,n3[2:13]+120)
# t5 = c(t2,t3[2:13]+10272)
# plot(t5,n5)

# xvec <- rnorm(100,-3,4)
# fn <- function(theta) {sum ( 0.5*(xvec - theta[1])^2/theta[2] + 0.5* log(theta[2]) )}
# summ =nlm(fn, theta <- c(1
#                          ,7), hessian=TRUE)
# print(summ)


para_con1=function(xdat,i,n=1,theta=1){
  if(theta==1){
    list0=list(c(0.002),c(0.002),c(0.002,.2), c(0.001,0.9999),c(0.002,.2),c(0.002,.2),c(0.02,.002), c(0.15,.00000015))
  }
  t1<<-xdat
  l<<-length(t1)
  if(length(n)==1){
    n=1:l-1
  }
  n<<-n
  
  dn<<-gen(n)
  A=matrix(c(0,1,1,0),2,2)
  B=matrix(c(0.000000001,.0000000001),2,1)
  A1=matrix(c(1),1,1)
  B1=matrix(-0.0000000001,1,1)
  if(i==1){
    if(theta==1){theta=list0[[i]]}
    summ =maxLik(Goel_Okumoto01, start =theta,constraints=list(ineqA=A1,ineqB=B1))
    res=new.env()
    res$l=seq(0,t1[l],by=1)
    sqrt(sum((Goel_Okumoto02(summ$estimate,t1)$func-n)^2)/(length(t1)-1))
    res$su=Goel_Okumoto02(summ$estimate,seq(0,t1[l],by=1))$func
    res$MLE=summ$maximum
    res$MSE=sqrt(sum((Goel_Okumoto02(summ$estimate,t1)$func-(n-1))^2))/(length(t1)-1)
    res$summ=summ
    return(res)
  }
  
  if(i==2){
    if(theta==1){theta=list0[[i]]}
    summ =maxLik(G_O01, start = theta,constraints=list(ineqA=A1,ineqB=B1))
    res=new.env()
    res$l=seq(0,t1[l],by=1)
    res$su=G_O02(summ$estimate,seq(0,t1[l],by=1))$func
    res$MSE=sqrt(sum((G_O02(summ$estimate,t1)$func-(n-1))^2))/(length(t1)-1)
    
    res$MLE=summ$maximum
    res$summ=summ
    return(res)
    
  }
  if(i==3){
    if(theta==1){theta=list0[[i]]}
    summ =maxLik(Hossain_Dahiya01, start = theta,constraints=list(ineqA=A,ineqB=B))
    res=new.env()
    res$l=seq(0,t1[l],by=1)
    res$su=Hossain_Dahiya02(summ$estimate,seq(0,t1[l],by=1))$func
    res$MSE=sqrt(sum((Hossain_Dahiya02(summ$estimate,t1)$func-(n-1))^2))/(length(t1)-1)
    
    res$MLE=summ$maximum
    res$summ=summ    
    return(res)
    
  }
  if(i==4){
    if(theta==1){theta=list0[[i]]}
    summ =maxLik(Gompertz01, start = theta,constraints=list(ineqA=A,ineqB=B))
    res=new.env()
    res$l=seq(0,t1[l],by=1)
    res$su=Gompertz02(summ$estimate,seq(0,t1[l],by=1))$func
    res$MSE=sqrt(sum((Gompertz02(summ$estimate,t1)$func-(n-1))^2))/(length(t1)-1)
    
    res$MLE=summ$maximum
    res$summ=summ
    return(res)
    
  }
  if(i==5){
    if(theta==1){theta=list0[[i]]}
    summ =maxLik(Pareto01, start = theta,constraints=list(ineqA=A,ineqB=B))
    res=new.env()
    res$l=seq(0,t1[l],by=1)
    res$su=Pareto02(summ$estimate,seq(0,t1[l],by=1))$func
    res$MSE=sqrt(sum((Pareto02(summ$estimate,t1)$func-(n-1))^2))/(length(t1)-1)
    
    res$MLE=summ$maximum
    res$summ=summ
    return(res)
    
  }
  if(i==6){
    if(theta==1){theta=list0[[i]]}
    summ =maxLik(Weibull01, start = theta,constraints=list(ineqA=A,ineqB=B))
    res=new.env()
    res$l=seq(0,t1[l],by=1)
    res$su=Weibull02(summ$estimate,seq(0,t1[l],by=1))$func
    res$MSE=sqrt(sum((Weibull02(summ$estimate,t1)$func-(n-1))^2))/(length(t1)-1)
    
    res$MLE=summ$maximum
    res$summ=summ
    return(res)
    
  }
  if(i==7){
    if(theta==1){theta=list0[[i]]}
    summ =maxLik(Yamada_Exp01, start = theta,constraints=list(ineqA=A,ineqB=B))
    res=new.env()
    res$l=seq(0,t1[l],by=1)
    res$su=Yamada_Exp02(summ$estimate,seq(0,t1[l],by=1))$func
    res$MSE=sqrt(sum((Yamada_Exp02(summ$estimate,t1)$func-(n-1))^2))/(length(t1)-1)
    
    res$MLE=summ$maximum
    res$summ=summ
    return(res)
    
  }
  if(i==8){
    if(theta==1){theta=list0[[i]]}
    summ =maxLik(Yamada_Raleigh01, start = theta,constraints=list(ineqA=A,ineqB=B))
    res=new.env()
    res$l=seq(0,t1[l],by=1)
    res$su=Yamada_Raleigh02(summ$estimate,seq(0,t1[l],by=1))$func
    res$MSE=sqrt(sum((Yamada_Raleigh02(summ$estimate,t1)$func-(n-1))^2))/(length(t1)-1)
    
    res$MLE=summ$maximum
    res$summ=summ
    return(res)
  }
}  

# res=para_con1(data0,1,theta = 0.00002)
# lines(res$l,res$su,type='l',lwd=2,col='blue')


# t=xdat
# print(1)
# summ1 =maxLik(Goel_Okumoto01, start = c(0.00035),constraints=list(ineqA=A1,ineqB=B1))
# print(summ1)
# lines(seq(0,t1[l],by=1),Goel_Okumoto02(summ1$estimate,seq(0,t1[l],by=1))$func,col='red')

# summ2 =maxLik(G_O01, start = c(0.002),constraints=list(ineqA=A1,ineqB=B1))
# print(2)
# print(summ2)
# lines(seq(0,t1[l],by=1),G_O02(summ2$estimate,seq(0,t1[l],by=1))$func,col='blue')
# 
# summ3 =maxLik(Hossain_Dahiya01, start = c(0.002,.2),constraints=list(ineqA=A,ineqB=B))
# print(3)
# print(summ3)
# lines(seq(0,t1[l],by=1),Hossain_Dahiya02(summ3$estimate,seq(0,t1[l],by=1))$func,col='yellow')
# 
# print(4)
# summ4 =maxLik(Gompertz01, start = c(0.001,0.9999),constraints=list(ineqA=A,ineqB=B))
# print(summ4)
# lines(seq(0,t1[l],by=1),Gompertz02(summ4$estimate,seq(0,t1[l],by=1))$func,col='purple')
# 
# summ5 =maxLik(Pareto01, start = c(0.002,.2),constraints=list(ineqA=A,ineqB=B))
# print(5)
# print(summ5)
# lines(seq(0,t1[l],by=1),Pareto02(summ5$estimate,seq(0,t1[l],by=1))$func,col='green')
# 
# summ6 =maxLik(Weibull01, start = c(0.002,.2),constraints=list(ineqA=A,ineqB=B))
# print(6)
# print(summ6)
# lines(seq(0,t1[l],by=1),Weibull02(summ6$estimate,seq(0,t1[l],by=1))$func,col='pink')
# 
# summ7 =maxLik(Yamada_Exp01, start = c(0.02,.002),constraints=list(ineqA=A,ineqB=B))
# print(7)
# print(summ7)
# lines(seq(0,t1[l],by=1),Yamada_Exp02(summ7$estimate,seq(0,t1[l],by=1))$func,col='brown')
# 
# summ8 =maxLik(Yamada_Raleigh01, start = c(0.15,.00000015),constraints=list(ineqA=A,ineqB=B))
# print(8)
# print(summ8)
# lines(seq(0,t[l],by=1),Yamada_Raleigh02(summ8$estimate,seq(0,t[l],by=1))$func,col='orange')
# 

# t=t0
# n=n0
# dn = gen(n)
# l = length(t)
# # s = Goel_Okumoto(c(107,0.1))
# A=matrix(c(0,1,1,0),2,2)
# B=matrix(c(-0.000000001,-.0000000001),2,1)
# A1=matrix(c(1),1,1)
# B1=matrix(-0.0001,1,1)
# # summ =nlm(Goel_Okumoto1, theta = c(110,0.001), constraints=list(ineqA=A,ineqB=B),hessian=TRUE)
# plot(t0,n0)
# # summ =maxLik(Goel_Okumoto, start = c(110,0.002),constraints=list(ineqA=A,ineqB=B))



# t=xdat
# n=1:length(t)
# #
# plot(xdat,1:length(xdat))
# res=para_con1(xdat,1)
# lines(res$l,res$su,type='l',lwd=2,col='blue')
# legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
# print(res$MLE)
# res=para_con1(xdat,2)
# lines(res$l,res$su,type='l',lwd=2,col='yellow')
# legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
# print(res$MLE)
# res=para_con1(xdat,3)
# lines(res$l,res$su,type='l',lwd=2,col='black')
# legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
# print(res$MLE)
# res=para_con1(xdat,4)
# lines(res$l,res$su,type='l',lwd=2,col='red')
# legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
# print(res$MLE)
# # res=para_con1(xdat,5)
# # lines(res$l,res$su,type='l',lwd=2,col='orange')
# # legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
# # print(res$MLE)
# res=para_con1(xdat,6)
# lines(res$l,res$su,type='l',lwd=2,col='purple')
# legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
# print(res$MLE)
# res=para_con1(xdat,7)
# lines(res$l,res$su,type='l',lwd=2,col='green')
# legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
# print(res$MLE)
# res=para_con1(xdat,8)
# lines(res$l,res$su,type='l',lwd=2,col='brown')
# legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
# print(res$MLE)
# 
# 

# list0=list(c(120,0.00023),c(120,0.0006),c(120,0.0004,.001),c(120,0.0005,0.9996),c(120,2.5,6000),c(120,0.00032,1.02),c(120,3,0.0001),c(120,18,.000000007))
# func_name=c('Goel_Okumoto','G_O','Hossain_Dahiya','Gompertz','Pareto','Weibull','Yamada_Exp','Yamada_Raleigh')
# 
# for(p in 1:8){
#   print(p)
#   theta=list0[[p]]
#   MSE=matrix(1:4000,nrow=8,ncol=500)
#   MLE=matrix(1:4000,nrow=8,ncol=500)
#   data=readRDS('conti.rds')[[p]]
#   for(i in 1:500){
#     # print(i)
#     xdat=data[[i]]
#     xdat=c(0,xdat)
#     for( j in c(1,2,3,4,5,6,7,8)){
#       res=para_con1(xdat,j)
#       l = seq(1,max(res$l),by=10)
#       MSE0=sqrt(sum((res$su[l]-eval(parse(text = func_name[p]))(l,theta))^2))/(length(l)-1)
#       MSE[j,i]=MSE0
#       MLE[j,i]=res$MLE
#     }
#   }
#   # print(p)
#   MSE_mean=rowMeans(MSE)
#   print(MSE_mean)
#   MSE_variance=apply(MSE,1,var)
#   print(MSE_variance)
#   MLE_mean=rowMeans(MLE)
#   print(MLE_mean)
#   MLE_variance=apply(MLE, 1,var)
#   print(MLE_variance)
# }

# for(p in 1:8){
#   print(p)
#   theta=list0[[p]]
#   MSE=matrix(1:4000,nrow=8,ncol=500)
#   MLE=matrix(1:4000,nrow=8,ncol=500)
#   data=readRDS('discon.rds')[[p]]
#   for(i in 1:500){
#     # print(i)
#     xdat=data[[i]]
#     xdat=c(0,xdat)
#     for( j in c(1,2,3,4,5,6,7,8)){
#       res=para_con1(xdat,j)
#       l = seq(1,max(res$l),by=10)
#       MSE0=sqrt(sum((res$su[l]-Hossain_Dahiya(l,theta))^2))/(length(l)-1)
#       MSE[j,i]=MSE0
#       MLE[j,i]=res$MLE
#     }
#   }
#   MSE_mean=rowMeans(MSE)
#   print(MSE_mean)
#   MSE_variance=apply(MSE,1,var)
#   print(MSE_variance)
#   MLE_mean=rowMeans(MLE)
#   print(MLE_mean)
#   MLE_variance=apply(MLE, 1,var)
#   print(MLE_variance)
# }
# 



# MSE_mean=rowMeans(MSE)
# MSE_variance=apply(MSE,1,var)
# MLE_mean=rowMeans(MLE)
# MLE_variance=apply(MLE, 1,var)

# write.csv(cbind(MLE_s,MSE_s), file = "3_con_spl_120_.0004_.001.csv", row.names = FALSE)
# write.csv(cbind(MLE_p,MSE_p), file = "3_con_pen_ab_120_.0004_.001.csv", row.names = FALSE)


# data=readRDS('real_conti.rds')
# 
# for (i in 5:16) {
#   n=length(data[[i]])
#   data0=c(0)
#   data1=c(0)
#   for(j in 1:n){
#     data0=c(data0,data[[i]][[j]][1])####hours
#     data1=c(data0,data[[i]][[j]][2])####days
#   }
#   data0=cumsum(data0)
#   data1=cumsum(data1)
#   plot(data0,0:(length(data0)-1))
#   plot(data1,0:(length(data1)-1))
#   res=para_con1(data0,1,theta=0.00002)
#   lines(res$l,res$su,type='l',lwd=2,col='blue')
#   legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#   print(res$MLE)
#   res=para_con1(data0,2,theta=0.0000008)
#   lines(res$l,res$su,type='l',lwd=2,col='red')
#   legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#   print(res$MLE)
#   res=para_con1(data0,3,theta=c(0.00003,0.15))
#   lines(res$l,res$su,type='l',lwd=2,col='yellow')
#   legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#   print(res$MLE)
#   res=para_con1(data0,4,theta = c(0.1,0.99995))
#   lines(res$l,res$su,type='l',lwd=2,col='orange')
#   legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#   print(res$MLE)
#   res=para_con1(data0,5,theta=c(0.000001,3700))
#   lines(res$l,res$su,type='l',lwd=2,col='brown')
#   legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#   print(res$MLE)
#   res=para_con1(data0,6,theta=c(0.0007,0.68))
#   lines(res$l,res$su,type='l',lwd=2,col='black')
#   legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#   print(res$MLE)
#   res=para_con1(data0,7,theta=c(1.71,0.00002))
#   lines(res$l,res$su,type='l',lwd=2,col='purple')
#   legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#   print(res$MLE)
#   res=para_con1(data0,8,theta = c(0.15,0.000000001))
#   lines(res$l,res$su,type='l',lwd=2,col='blue')
#   legend(x=c(7000),y=c(20),legend = c('Best parametric'),col = c('blue'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#   print(res$MLE)
#   
#   
# }




