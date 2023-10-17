# nonincre2=function(x,t){
#   n=length(t)
#   i0 =  length(t)-sum((x<=t))-1
#   if(i0==0){
#     return(0)
#   }
#   m=1:(n-i0-1)*0
#   J=1:(n-2)
#   for(i in 1:(n-2)){
#     J[i]=(n-i-1)*(t[i+2]-t[i+1])
#   }
#   for (j in (i0+1):(n-1)){
#     for(i in 1:(i0)){
#       if((j-i)/(sum(J[i:(j-1)]))>m[j-i0]){
#         m[j-i0]=(j-i)/(sum(J[i:(j-1)]))
#       }
#     }
#   }
#   m0=min(m)
#   return(m0)
# }
# 
# nondecre2=function(x,t){
#   n=length(t)
#   i0 =  length(t)-sum((x<=t))-1
#   if(i0==0){
#     return(0)
#   }
#   m=1:(n-i0-1)*0+100
#   J=1:(n-2)
#   for(i in 1:(n-2)){
#     J[i]=(n-i-1)*(t[i+2]-t[i+1])
#   }
#   for (j in (i0+1):(n-1)){
#     for(i in 1:(i0)){
#       if((j-i)/(sum(J[i:(j-1)]))<m[j-i0]){
#         m[j-i0]=(j-i)/(sum(J[i:(j-1)]))
#       }
#     }
#   }
#   m0=max(m)
#   return(m0)
# }

nonincre2=function(x,t){
  n=length(t)
  i0 =  length(t)-sum((x<=t))-1
  J=((n-2):1)*diff(t)[3:length(t)-1]
  # apply(  as.matrix(apply(as.matrix(1:100), 1, function(x){length(t)-sum((x<=t))-1})),1,
  #         function(i0){if(i0==0){0}else{min(apply(as.matrix((i0+1):(n-1)),1,
  #         function(j){max(apply(matrix(1:i0,ncol=1,nrow=i0), 1,
  #         function(x){(j-x)/(sum(J[x:(j-1)]))}))}))}})
  if(i0==0){
    return(0)
  }
  m=1:(n-i0-1)*0
  # J=1:(n-2)
  # for(i in 1:(n-2)){
  #   J[i]=(n-i-1)*(t[i+2]-t[i+1])
  # }
  
  for (j in (i0+1):(n-1)){
    for(i in 1:(i0)){
      if((j-i)/(sum(J[i:(j-1)]))>m[j-i0]){
        m[j-i0]=(j-i)/(sum(J[i:(j-1)]))
      }
    }
  }
  m0=min(m)
  return(m0)
}


nondecre2=function(x,t){
  n=length(t)
  i0 =  length(t)-sum((x<=t))-1
  # if(i0==0){
  #   return(0)
  # }
  m=1:(n-i0-1)*0+100
  J=1:(n-1)
  for(i in 0:(n-2)){
    J[i]=(n-i-1)*(t[i+2]-t[i+1])
  }
  for (j in (i0+1):(n-1)){
    for(i in 0:(i0)){
      if((j-i)/(sum(J[(i+1):(j)]))<m[j-i0]){
        m[j-i0]=(j-i)/(sum(J[(i+1):(j)]))
      }
    }
  }
  m0=max(m)
  
  
  return(m0)
}


t = c(0, 32.4375, 64.875, 97.3125, 129.75, 162.1875, 194.625, 227.0625, 259.5, 291.9375, 324.375, 356.8125, 389.25, 421.6875, 454.125, 486.5625, 519.0, 575.125, 631.25, 687.375, 743.5, 799.625, 855.75, 911.875, 968.0, 1122.0, 1276.0, 1430.0, 1507.1666666666667, 1584.3333333333333, 1661.5, 1738.6666666666667, 1815.8333333333335, 1893.0, 1967.625, 2042.25, 2116.875, 2191.5, 2266.125, 2340.75, 2415.375, 2490.0, 2561.0, 2632.0, 2703.0, 2774.0, 2845.0, 2916.0, 2987.0, 3058.0, 3171.4, 3284.8, 3398.2, 3511.6, 3625.0, 3824.25, 4023.5, 4222.75, 4422.0, 4494.363636363636, 4566.727272727273, 4639.090909090909, 4711.454545454545, 4783.818181818182, 4856.181818181818, 4928.545454545454, 5000.909090909091, 5073.272727272727, 5145.636363636364, 5218.0, 5318.833333333333, 5419.666666666667, 5520.5, 5621.333333333333, 5722.166666666667, 5823.0, 5942.333333333333, 6061.666666666667, 6181.0, 6300.333333333333, 6419.666666666667, 6539.0, 6647.8, 6756.6, 6865.4, 6974.2, 7083.0, 7184.0, 7285.0, 7386.0, 7487.0, 7606.666666666667, 7726.333333333333, 7846.0, 7965.666666666667, 8085.333333333333, 8205.0, 8384.5, 8564.0, 8923.0, 9282.0)


sum0=function(t,p){
  l = seq(1,max(t),by=1)
  y0 = 1:length(l)
  if(p==2){for(i in 1:length(l)){y0[i]=nonincre2(l[i],t)}}
  if(p==3){for(i in 1:length(l)){y0[i]=nondecre2(l[i],t)}}
  su=1:length(l)*0
  for (i in 1:length(l)){su[i]=sum(y0[1:i])}
  an = new.env()
  an$su=su
  an$y=y0
  return(an)
}
ifr=function(t,u){
  l = seq(1,max(t),by=1)
  t0 = round(t)
  t0[1]=1
  if(max(t0)>max(l)){t0[length(t0)]=t0[length(t0)]-1}
  suu=sum0(t,2)
  su=suu$su
  y=suu$y
  lam=y[t0]*exp(-su[t0])*length(t)/(1-exp(-su[length(su)]))
  lam=lam[which(lam>0)]
  su2=(1-exp(-su))*length(t)/(1-exp(-su[length(su)]))
  

  ###MLE
  MLE=sum(log(lam))-su2[length(su2)]
  ###MSE
  MSE=sqrt(sum((1:length(t)-su2[t0])^2))/(length(t)-1)
  re = new.env()
  re$MLE=MLE
  re$MSE=MSE
  # print(MSE)
  re$su = su2
  re$lam=lam
  re$t0=t0
  re$l=l
  return(re)
}


dfr=function(t){
  l = seq(1,max(t),by=1)
  t0 = round(t)
  if(t0[length(t0)]>length(l)){t0[length(t0)]=t0[length(t0)]-1}
  t0[1]=1
  suu=sum0(t,3)
  su=suu$su
  y=suu$y
  lam=y[t0]*exp(-su[t0])*length(t)/(1-exp(-su[length(su)]))
  lam=lam[which(lam>0)]
  su3=(1-exp(-su))*length(t)/(1-exp(-su[length(su)]))
  
  ###MLE
  MLE=sum(log(lam))-su3[length(su3)]
  ###MSE
  MSE=sqrt(sum((1:length(t)-su3[t0])^2))/(length(t)-1)
  re = new.env()
  re$MLE=MLE
  re$MSE=MSE
  re$su = su3
  re$lam=lam
  re$t0=t0
  re$l=l
  return(re)
}

set.seed(123)
G_O_gre=function(x,theta){
  theta[1]*theta[2]*(theta[2]*x)*exp(-theta[2]*x)
}
poi=function(the,theta,t,f){
  x=sort(runif(rpois(1,the*t),0,1))*t
  y=x
  u=0
  for (i in 1:length(x)) {
    p=f(x[i],theta)/the
    l=rbinom(1,1,p)
    if(l==0){
      y=y[-i+u]
      u=u+1
    }
  }
  return(y)
}
# t= poi(0.04,c(120,.0007),10000,G_O_gre)
# xdat2=
# t[1:60]=(xdat[1:60]+1000*xdat[1:60]^(1/3))/(xdat[60]+1000*xdat[60]^(1/3))*xdat[60]
# t=c(0,t)


nonpara_s=function(u,t){
  nt=length(t)
  t1=t[1:u]
  t2=t[(u+1):nt]
  t3=t2-t[u+1]
  result1=dfr(t1)
  result2=ifr(t3)
  # t0 =result2$t0
  l1=result1$l
  l2=result2$l
  l=c(l1,l2+l1[length(l1)])
  su=c(result1$su,result2$su+result1$su[length(result1$su)])
  MSE=sqrt((result1$MSE*(length(t1)-1))^2+(result2$MSE*(length(t2)-1))^2)/(length(t)-1)
  MLE=result1$MLE+result2$MLE
  res=new.env()
  res$MLE=MLE
  res$MSE=MSE
  res$su=su
  res$l=l
  return(res)
}



t=data1
for(u in seq(10,(length(t)-1),by=5)){
  nt=length(t)
  t1=t[1:u]
  t2=t[(u):nt]
  t3=t2-t[u]
  result1=dfr(t1)
  result2=ifr(t3)
  # t0 =result2$t0
  l1=result1$l
  l2=result2$l
  l=c(l1,l2+l1[length(l1)])
  su=c(result1$su,result2$su+result1$su[length(result1$su)])
  MSE=sqrt((result1$MSE*(length(t1)-1))^2+(result2$MSE*(length(t2)-1))^2)/(length(t)-1)
  MLE=result1$MLE+result2$MLE
  print(u)
  print(MSE)
  print(MLE)
  plot(c(0,max(t)),c(0,length(t)),pch=" ")
  lines(t,0:(length(t)-1),lty=2)
  lines(l,su)
}
