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
#   # if(i0==0){
#   #   return(0)
#   # }
#   m=1:(n-i0-1)*0+100
#   J=1:(n-1)
#   for(i in 0:(n-2)){
#     J[i]=(n-i-1)*(t[i+2]-t[i+1])
#   }
#   for (j in (i0+1):(n-1)){
#     for(i in 0:(i0)){
#       if((j-i)/(sum(J[(i+1):(j)]))<m[j-i0]){
#         m[j-i0]=(j-i)/(sum(J[(i+1):(j)]))
#       }
#     }
#   }
#   m0=max(m)
#   
#   
#   return(m0)
# }
nonincre2=function(x,t){
  n=length(t)
  i0 =  length(t)-sum((x<=t))-1
  J=((n-2):1)*diff(t)[3:length(t)-1]
    if(i0==0){return(0)}
  m=1:(n-i0-1)*0
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
# nonincre2=function(i0,n){
#   J=((n-2):1)*diff(t)[3:n-1]
#   if(i0==0){return(0)}
#   m=1:(n-i0-1)*0
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



# t = c(0, 32.4375, 64.875, 97.3125, 129.75, 162.1875, 194.625, 227.0625, 259.5, 291.9375, 324.375, 356.8125, 389.25, 421.6875, 454.125, 486.5625, 519.0, 575.125, 631.25, 687.375, 743.5, 799.625, 855.75, 911.875, 968.0, 1122.0, 1276.0, 1430.0, 1507.1666666666667, 1584.3333333333333, 1661.5, 1738.6666666666667, 1815.8333333333335, 1893.0, 1967.625, 2042.25, 2116.875, 2191.5, 2266.125, 2340.75, 2415.375, 2490.0, 2561.0, 2632.0, 2703.0, 2774.0, 2845.0, 2916.0, 2987.0, 3058.0, 3171.4, 3284.8, 3398.2, 3511.6, 3625.0, 3824.25, 4023.5, 4222.75, 4422.0, 4494.363636363636, 4566.727272727273, 4639.090909090909, 4711.454545454545, 4783.818181818182, 4856.181818181818, 4928.545454545454, 5000.909090909091, 5073.272727272727, 5145.636363636364, 5218.0, 5318.833333333333, 5419.666666666667, 5520.5, 5621.333333333333, 5722.166666666667, 5823.0, 5942.333333333333, 6061.666666666667, 6181.0, 6300.333333333333, 6419.666666666667, 6539.0, 6647.8, 6756.6, 6865.4, 6974.2, 7083.0, 7184.0, 7285.0, 7386.0, 7487.0, 7606.666666666667, 7726.333333333333, 7846.0, 7965.666666666667, 8085.333333333333, 8205.0, 8384.5, 8564.0, 8923.0, 9282.0)


sum0=function(t,p){
  l = seq(1,max(t),by=1)
  y0 = 1:length(l)
  if(p==2){for(i in 1:length(l)){y0[i]=nonincre2(l[i],t)}}
  if(p==3){for(i in 1:length(l)){y0[i]=nondecre2(l[i],t)}}
  # su=1:length(l)*0
  # for (i in 1:length(l)){su[i]=sum(y0[1:i])}
  su=cumsum(y0)
  an = new.env()
  an$su=su
  an$y=y0
  return(an)
}



# sum0=function(t,p){
#   l = seq(1,max(t),by=1)
#   # t = c(0, 32.4375, 64.875, 97.3125, 129.75, 162.1875, 194.625, 227.0625, 259.5, 291.9375, 324.375, 356.8125, 389.25, 421.6875, 454.125, 486.5625, 519.0, 575.125, 631.25, 687.375, 743.5, 799.625, 855.75, 911.875, 968.0, 1122.0, 1276.0, 1430.0, 1507.1666666666667, 1584.3333333333333, 1661.5, 1738.6666666666667, 1815.8333333333335, 1893.0, 1967.625, 2042.25, 2116.875, 2191.5, 2266.125, 2340.75, 2415.375, 2490.0, 2561.0, 2632.0, 2703.0, 2774.0, 2845.0, 2916.0, 2987.0, 3058.0, 3171.4, 3284.8, 3398.2, 3511.6, 3625.0, 3824.25, 4023.5, 4222.75, 4422.0, 4494.363636363636, 4566.727272727273, 4639.090909090909, 4711.454545454545, 4783.818181818182, 4856.181818181818, 4928.545454545454, 5000.909090909091, 5073.272727272727, 5145.636363636364, 5218.0, 5318.833333333333, 5419.666666666667, 5520.5, 5621.333333333333, 5722.166666666667, 5823.0, 5942.333333333333, 6061.666666666667, 6181.0, 6300.333333333333, 6419.666666666667, 6539.0, 6647.8, 6756.6, 6865.4, 6974.2, 7083.0, 7184.0, 7285.0, 7386.0, 7487.0, 7606.666666666667, 7726.333333333333, 7846.0, 7965.666666666667, 8085.333333333333, 8205.0, 8384.5, 8564.0, 8923.0, 9282.0)
#   if(p==2){
#     n=length(t)
#     J=((n-2):1)*diff(t)[3:n-1]
#     y0=apply(  as.matrix(apply(as.matrix(l), 1, function(x){length(t)-sum((x<=t))-1})),1,
#                function(i0){if(i0==0){0}else{min(apply(as.matrix((i0+1):(n-1)),1,
#                                                        function(j){max(apply(matrix(1:i0,ncol=1,nrow=i0), 1, 
#                                                                              function(x){(j-x)/(sum(J[x:(j-1)]))}))}))}})
#   }
#   
#   if(p==3){
#     n=length(t)
#     J=((n-1):1)*diff(t)[2:n-1]
#     y0=apply(  as.matrix(apply(as.matrix(l), 1, function(x){length(t)-sum((x<=t))-1})),1,
#                function(i0){max(apply(as.matrix((i0+1):(n-1)),1,
#                                       function(j){min(apply(matrix(0:i0), 1, 
#                                                             function(x){(j-x)/(sum(J[(x+1):(j)]))}))}))})
#   }
#   su=cumsum(y0)
#   an = new.env()
#   an$su=su
#   an$y=y0
#   return(an)
# }

######1 is continuous case 2 is discontinuous case
ifr1=function(t){
  l = seq(1,max(t),by=10)
  t0 = round(t)
  t0[t0==0]=1
  t0[t0>floor(max(t))]=floor(max(t))
  suu=sum0(t,2)
  su=suu$su
  y=suu$y
  lam=y[t0]*exp(-su[t0])*(length(t)-1)/(1-exp(-su[length(su)]))
  lam=lam[which(lam>0)]
  su2=(1-exp(-su))*(length(t)-1)/(1-exp(-su[length(su)]))

  ###MLE
  MLE=sum(log(lam))-su2[length(su2)]
  ###MSE
  MSE=sqrt(sum((0:(length(t)-1)-su2[t0])^2))/(length(t)-1)
  re = new.env()
  if(identical(as.character(substitute(data_name)), "data_name")){
    MSE0=sqrt(sum((su2[l]-data_name(l,theta))^2))/(length(l)-1)
    re$MSE0=MSE0
  }

  re$t0=t0
  re$l=l
  re$MLE=MLE
  re$MSE=MSE
  re$su = su2
  return(re)
}
ifr2=function(t){
  l = seq(1,max(t),by=10)####10!!!
  t0 = round(t)
  t0[t0==0]=1
  t0[t0>floor(max(t))]=floor(max(t))
  suu=sum0(t,2)
  su=suu$su
  y=suu$y
  lam=y[t0]*exp(-su[t0])*length(t)/(1-exp(-su[length(su)]))
  lam=lam[which(lam>0)]
  su2=(1-exp(-su))*length(t)/(1-exp(-su[length(su)]))

  ###MLE
  # MLE=sum(log(lam[nd]))-su2[length(su2)]
  uu=which(gen(su2[t0])>0)
  MLE=sum(gen(1:length(t))[uu]*log(gen(su2[t0][uu])))-su2[length(su2)]
  ###MSE
  MSE=sqrt(sum((0:(length(su2[t0])-1)-su2[t0])^2))/(length(t)-1)
  # if(identical(as.character(substitute(data_name)), "data_name")){
  #   MSE0=sqrt(sum((su2[l]-data_name(l,theta))^2))/(length(l)-1)
  #   re$MSE0=MSE0
  # }
  #

  re = new.env()
  re$l=l

  re$MLE=MLE
  re$MSE=MSE
  re$su = su2
  return(re)
}
# res=nonpara(xdat,dfr1,eval(parse(text = func_name[1])),list0[[1]])
dfr1=function(t){
  l = seq(1,max(t),by=10)
  t0 = round(t)
  t0[t0==0]=1
  t0[t0>floor(max(t))]=floor(max(t))
  suu=sum0(t,3)
  su=suu$su
  y=suu$y
  lam=y[t0]*exp(-su[t0])*(length(t)-1)/(1-exp(-su[length(su)]))
  lam=lam[which(lam>0)]
  su3=(1-exp(-su))*(length(t)-1)/(1-exp(-su[length(su)]))

  ###MLE
  MLE=sum(log(lam))-su3[length(su3)]
  ###MSE
  MSE=sqrt(sum((0:(length(t)-1)-su3[t0])^2))/(length(t)-1)
  re = new.env()
  if(identical(as.character(substitute(data_name)), "data_name")){
    MSE0=sqrt(sum((su3[l]-data_name(l,theta))^2))/(length(l)-1)
    re$MSE0=MSE0
  }

  re$l=l

  re$MLE=MLE
  re$MSE=MSE
  re$su = su3
  return(re)
}
dfr2=function(t){
  l = seq(1,max(t),by=1)
  t0 = round(t)
  t0[t0==0]=1
  t0[t0>floor(max(t))]=floor(max(t))
  suu=sum0(t,3)
  su=suu$su
  y=suu$y
  lam=y[t0]*exp(-su[t0])*length(t)/(1-exp(-su[length(su)]))
  lam=lam[which(lam>0)]
  su3=(1-exp(-su))*length(t)/(1-exp(-su[length(su)]))

  ###MLE
  MLE=sum(log(lam[nd]))-su3[length(su3)]
  ###MSE
  # MSE=sqrt(sum((1:length(t)-su3[t0])^2))/(length(t)-1)
  MSE=sqrt(sum((nd[2:length(nd)]-su3[t0[nd]])^2))/(length(nd)-1)

  re = new.env()
  # if(identical(as.character(substitute(data_name)), "data_name")){
  #   MSE0=sqrt(sum((su3[l]-data_name(l,theta))^2))/(length(l)-1)
  #   re$MSE0=MSE0
  # }

  re$l=l
  re$MLE=MLE
  re$MSE=MSE
  re$su = su3
  return(re)
}
# res=nonpara(xdat,dfr1)
nonpara=function(xdat,func,data_name=1,theta=1){
  # xdat
  l= seq(1,max(xdat),by=1)
  t0 = round(xdat)
  t0[1]=1
  if(t0[length(t0)]>max(l)){
    t0[length(t0)]=t0[length(t0)]-1
  }
  data_name<<-data_name
  theta<<-theta
  result = func(xdat)
  MSE=result$MSE
  MLE=result$MLE
  if(identical(as.character(substitute(data_name))[1], "eval")){MSE0=result$MSE0}
  res=new.env()
  if(identical(as.character(substitute(data_name))[1], "eval")){res$MSE0=MSE0}
  res$MSE=MSE
  res$MLE=MLE
  res$l=l
  res$su=result$su
  return(res)
  
}
# res=nonpara(xdat,ifr1,eval(parse(text = func_name[p])),list0[[1]])
# res=nonpara(xdat,ifr1,eval(parse(text = func_name[p])),list0[[1]])

# nonpara_d=function(xdat,func,data_name=1,theta=1){
#   # xdat
#   l= seq(1,max(xdat),by=1)
#   t0 = round(xdat)
#   t0[1]=1
#   if(t0[length(t0)]>max(l)){
#     t0[length(t0)]=t0[length(t0)]-1
#   }
#   data_name<<-data_name
#   theta<<-theta
#   result = func(xdat)
#   MSE=result$MSE
#   MLE=result$MLE
#   if(identical(as.character(substitute(data_name))[1], "eval")){MSE0=result$MSE0}
#   res=new.env()
#   if(identical(as.character(substitute(data_name))[1], "eval")){res$MSE0=MSE0}
#   res$MSE=MSE
#   res$MLE=MLE
#   res$l=l
#   res$su=result$su
#   return(res)
#   
# }


# plot(xdat,1:length(xdat))

# res=nonpara(data0,ifr1,Hossain_Dahiya,c(120,0.0004,.001))
# lines(res$l,res$su,type='l',lwd=2,col='red')
# legend(x=c(5000),y=c(15),legend = c('IFR'),col = c('green'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
# print(res$MLE)
#
# res=nonpara(xdat,dfr2)
# lines(res$l,res$su,type='l',lwd=2,col='orange')
# legend(x=c(5000),y=c(10),legend = c('DFR'),col = c('orange'),lty=1, lwd=5,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
# print(res$MLE)
# 
# nonpara(xdat,ifr1,eval(parse(text = func_name[p])),list0[[p]])

list0=list(c(120,0.00023),c(120,0.0006),c(120,0.0004,.001),c(120,0.0005,0.9996),c(120,2.5,6000),c(120,0.00032,1.02),c(120,3,0.0001),c(120,18,.000000007))
func_name=c('Goel_Okumoto','G_O','Hossain_Dahiya','Gompertz','Pareto','Weibull','Yamada_Exp','Yamada_Raleigh')


# 
# for (p in c(1,3,4,5,6,7)) {
#   print(p)
#   MSE_i=1:500
#   MSE_d=1:500
#   MLE_i=1:500
#   MLE_d=1:500
#   data=readRDS('conti.rds')[[p]]
#   
#   for (i in 1:500) {  
#     print(i)
#     xdat=data[[i]]
#     xdat=c(0,xdat)
#     ###non-parametric
#     res=nonpara(xdat,ifr1,eval(parse(text = func_name[p])),list0[[p]])
#     MSE_i[i]=res$MSE0
#     MLE_i[i]=res$MLE
#     
#     
#     res=nonpara(xdat,dfr1,eval(parse(text = func_name[p])),list0[[p]])
#     MSE_d[i]=res$MSE0
#     MLE_d[i]=res$MLE
#   }
#   filename <- paste(p,'ifr.csv', sep = "_")
#   write.csv(cbind(MLE_i,MSE_i), file = filename, row.names = FALSE)
#   filename <- paste(p,'dfr.csv', sep = "_")
#   write.csv(cbind(MLE_d,MSE_d), file = filename, row.names = FALSE)
# }
# 

######this part is reduced 
ifr3=function(t,w){
  l = seq(1,max(t),by=1)
  t0 = round(t)
  t0[t0==0]=1
  t0[t0>floor(max(t))]=floor(max(t))
  suu=sum1(t,2,w)
  su=suu$su
  y=suu$y
  lam=y[t0]*exp(-su[t0])*(length(t)-1)/(1-exp(-su[length(su)]))
  lam=lam[which(lam>0)]
  su2=(1-exp(-su))*(length(t)-1)/(1-exp(-su[length(su)]))
  
  ###MLE
  MLE=sum(log(lam))-su2[length(su2)]
  ###MSE
  MSE=sqrt(sum((0:(length(t)-1)-su2[t0])^2))/(length(t)-1)
  re = new.env()
  if(identical(as.character(substitute(data_name)), "data_name")){
    MSE0=sqrt(sum((su2[l]-data_name(l,theta))^2))/(length(l)-1)
    re$MSE0=MSE0
  }
  
  re$t0=t0
  re$l=l
  re$MLE=MLE
  re$MSE=MSE
  re$su = su2
  return(re)
}


dfr3=function(t,w){
  l = seq(1,max(t),by=1)
  t0 = round(t)
  t0[t0==0]=1
  t0[t0>floor(max(t))]=floor(max(t))
  suu=sum1(t,3,w)
  su=suu$su
  y=suu$y
  lam=y[t0]*exp(-su[t0])*(length(t)-1)/(1-exp(-su[length(su)]))
  lam=lam[which(lam>0)]
  su3=(1-exp(-su))*(length(t)-1)/(1-exp(-su[length(su)]))
  
  ###MLE
  MLE=sum(log(lam))-su3[length(su3)]
  ###MSE
  MSE=sqrt(sum((0:(length(t)-1)-su3[t0])^2))/(length(t)-1)
  re = new.env()
  
  re$l=l
  
  re$MLE=MLE
  re$MSE=MSE
  re$su = su3
  return(re)
}
sum1=function(t,p,w){
  l = seq(1,max(t),by=1)
  l0=seq(1,max(t),by=w)
  y0 = l0
  y1=l
  if(p==2){for(i in 1:length(l0)){
    # print(i)
    y0[i]=nonincre2(w*(i-1)+1,t)}}
  if(p==3){for(i in 1:length(l0)){
    # print(i)
    y0[i]=nondecre2(w*(i-1)+1,t)}}
  
  y_diff=c(diff(y0),0)
  for (i in 1:max(t)) {
    m=sum(i>=l0)
    y1[i]=y0[m]+y_diff[m]*(i-w*(m-1))/w
  }
  su=cumsum(y1)
  an = new.env()
  an$su=su
  an$y=y1
  return(an)
}
nonpara_red=function(xdat,func,w,data_name=1,theta=1){
  # xdat
  l= seq(1,max(xdat),by=1)
  t0 = round(xdat)
  t0[1]=1
  if(t0[length(t0)]>max(l)){
    t0[length(t0)]=t0[length(t0)]-1
  }
  data_name<<-data_name
  theta<<-theta
  result = func(xdat,w)
  MSE=result$MSE
  MLE=result$MLE
  if(identical(as.character(substitute(data_name))[1], "eval")){MSE0=result$MSE0}
  res=new.env()
  if(identical(as.character(substitute(data_name))[1], "eval")){res$MSE0=MSE0}
  res$MSE=MSE
  res$MLE=MLE
  res$l=l
  res$su=result$su
  return(res)
  
}
