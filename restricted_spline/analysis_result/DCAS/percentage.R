source("basic_functions/function.R")
data=readRDS(file.path(getwd(),'realdata/real_conti.rds'))
name1=c('G-O','G-O-S','H-D','Gompertz','Pareto','Weibull','Y-Exp','Y-M','DFR-IFR')
data_name=c(1,2,3,4,5,6,'14C',17,27,40,'SS1A','SS1B','SS1C','SS2','SS3','SS4')
name2=c('IFR','DFR','DFR-IFR')
name3=c('Spline','S-shape','Spline-DID','Spline-IDI')
best_1=c(6,  6, 5, 1, 6, 6, 4, 6, 5, 5, 6, 7 ,6 ,4, 4 ,6)
best_04=c(6, 6, 6, 6, 5, 5, 4, 6, 2 ,1 ,6 ,6 ,6 ,4, 4 ,6)
best_06=c(6 ,6 ,5 ,6 ,6 ,3 ,4 ,6 ,5 ,7 ,6 ,6 ,3 ,4 ,4 ,3)
best_08=c(6 ,6 ,5 ,3 ,6 ,3 ,4 ,6 ,5 ,5 ,6 ,6 ,5 ,4 ,4 ,6)
list0=list(c(0.002),c(0.002),c(0.002,.2), c(0.001,0.9999),c(0.002,.2),c(0.002,.2),c(0.02,.002), c(0.15,.00000015))
nons=c(35)
ra=10
par(cex.lab = 1.5, cex.main = 2, cex.axis = 1.8)

####percentage x% point of the data
percent=0.8

d=c(0.4,0.6,0.8,1)
D=list(best_04,best_06,best_08,best_1)
best=D[[which(d==percent)]]
# MLE=matrix(0,nrow = 16,ncol=8)
# MSE=MLE
# for (i in 1:16) {
#   n=length(data[[i]])
#   data0=c(0)
#   data1=c(0)
#   p=1
#   for(j in 1:n){
#     data0=c(data0,data[[i]][[j]][1])####hours
#   }
#   data1=cumsum(data0)
#   data1=data1*10000/max(data1)
#   data1=data1[data1<(percent*10000)]
#   for(j in 1:8){
#     res=para_con1(data1,j)
#     print(res$MLE)
#     print(res$MSE)
#     MLE[i,j]=res$MLE
#     MSE[i,j]=res$MSE
#   }
# }
# print(apply(MSE, 1, which.min))



results_list <- list()
for (i in 5) {
  data_name=c(1,2,3,4,5,6,'14C',17,27,40,'SS1A','SS1B','SS1C','SS2','SS3','SS4')
  dataset_results <- list()
  i00=i
  print('Number:')
  print(i)
  print('Percent')
  print(percent)
  n=length(data[[i]])
  data0=c(0)
  data1=c(0)
  p=1
  for(j in 1:n){
    data0=c(data0,data[[i]][[j]][1])####hours
  }
  data1=cumsum(data0)
  data1=data1*10000/max(data1)
  len_data=length(data1)
  data1=data1[1:(len_data*percent)]
  ndat=length(data1)
  c=ndat/25  #the number of 
  ###data
  plot(data1,0:(length(data1)-1),type='l',lwd=4,xlab = 'time',ylab='Number of failures')
  legend(x=c(0),y=ndat,cex=1.5,legend = 'real data',col = c('black'),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  
  ###PARA
  print('parametric results:')
  best0=best[i]
  res=para_con1(data1,best0)
  lines(res$l,res$su,lty=2,lwd=2,col='blue')
  legend(x=c(0),y=c(ndat-c),cex=1.5,legend = name1[[best0]],col = c('blue'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  print(res$MLE)
  print(res$MSE)
  MSE=res$MSE
  MLE=res$MLE
  method_results <- list(res$l, res$su, MSE, MLE)
  dataset_results[[paste("Method", "parametric", sep = "_")]] <- method_results
  
  
  
  ##nonpara
  data1=data1/ra
  print('ifr_results')
  
  res=nonpara(data1,ifr1,eval(parse(text = func_name[p])),list0[[1]])
  print(res$MLE-(length(data1)-1)*log(ra))
  
  print(res$MSE)
  MSE=res$MSE
  MLE=res$MLE-(length(data1)-1)*log(ra)
  lines(res$l*ra,res$su,lty=2,lwd=2,col='yellowgreen')
  legend(x=c(0),ndat-7*c,cex=1.5,legend = name2[1],col = c('yellowgreen'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  method_results <- list(res$l*ra, res$su, MSE, MLE)
  dataset_results[[paste("Method", "ifr", sep = "_")]] <- method_results
  
  
  print('dfr results')
  
  res=nonpara(data1,dfr1,eval(parse(text = func_name[p])),list0[[1]])
  print(res$MLE-(length(data1)-1)*log(ra))
  print(res$MSE)
  MSE=res$MSE
  MLE=res$MLE-(length(data1)-1)*log(ra)
  lines(res$l*ra,res$su,lty=2,lwd=2,col='yellow3')
  legend(x=c(0),ndat-8*c,cex=1.5,legend = name2[2],col = c('yellow3'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  data1=data1*ra
  method_results <- list(res$l*ra, res$su, MSE, MLE)
  dataset_results[[paste("Method", "dfr", sep = "_")]] <- method_results
  # print(t0)
  
  
  
  ###spline
  k=4
  m0=1:4*0
  for(k in 3:6){
    res=continuous(data1,k,eval(parse(text = func_name[p])),list0[[1]])
    m0[k]=res$MLE-2*k
  }
  k=which.max(m0)+2
  res=continuous(data1,k,eval(parse(text = func_name[p])),list0[[1]])
  c0=max(n)/max(res$su)
  print('spline results')
  
  lines(res$l,res$su*c0,lty=1,lwd=3,col='tomato4')
  legend(x=c(0),ndat-2*c,cex=1.5,legend = name3[1],col = c('tomato4'),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  print(res$MLE)
  print(res$MSE)
  MSE=res$MSE
  MLE=res$MLE
  method_results <- list(res$l, res$su*c0, MSE, MLE, k)
  dataset_results[[paste("Method", "spline", sep = "_")]] <- method_results
  
  
  
  ###s-shape spline
  m0=1:4*0
  for(k in c(4,5,6,7)){
    print(k)
    start=min(data1)+2
    end=max(data1)-48
    interval=500
    e=1:((end-start)/interval+1)
    for(u0 in seq(start,end,by=interval)){
      # print(u0)
      res2=spline_con_s(1:length(data1)-1,data1,k,u0)
      M=res2$M
      y=t(res2$beta)%*%M
      c0=max(n)/max(y)
      MSE=sqrt(sum((0:(length(data1)-1)-y*c0)^2))/(length(data1)-1)
      e[(u0-start)/interval+1]=MSE
    }
    u0=(which.min(e)-1)*interval+start
    res=spline_con_s(1:length(data1)-1,data1,k,u0)
    M=res$M
    y=t(res$beta)%*%M
    c0=max(n)/max(y)
    MLE=sum(log(c0*res$beta%*%res$M0))-max(n)
    m0[k]=MLE-2*k
  }
  k=which.max(m0)+3
  
  # start=min(data1)+2
  # end=max(data1)-498
  # interval=500
  interval=max(data1)/50
  # print(interval)
  e=1:((end-start)/interval+1)
  for(u0 in seq(start,end,by=interval)){
    res2=spline_con_s(1:length(data1)-1,data1,k,u0)
    M=res2$M
    y=t(res2$beta)%*%M
    c0=max(n)/max(y)
    # plot(data1,1:length(data1))
    # lines(data1,y*c0)
    ###MSE
    MSE=sqrt(sum((0:(length(data1)-1)-y*c0)^2))/(length(data1)-1)
    e[(u0-start)/interval+1]=MSE
    # print(MSE)
  }
  u0=(which.min(e)-1)*interval+start
  res=spline_con_s(1:length(data1)-1,data1,k,u0)
  M=res$M
  y=t(res$beta)%*%M
  c0=max(n)/max(y)
  print('s-spline results')
  lines(data1,y*c0,lty=1,lwd=2,col='salmon')
  legend(x=c(0),y=c(ndat-3*c),cex=1.5,legend = name3[2],col = c('salmon'),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  print(sum(log(c0*res$beta%*%res$M0))-max(n))
  print(sqrt(sum((n-y*c0)^2))/(length(data1)-1))
  MSE=sqrt(sum((n-y*c0)^2))/(length(data1)-1)
  MLE=sum(log(c0*res$beta%*%res$M0))-max(n)
  print(u0)
  print(k)
  method_results <- list(data1, y*c0, MSE, MLE, u0,k)
  dataset_results[[paste("Method", "S_shape", sep = "_")]] <- method_results
  
  
  
  ###nonpara-s
  t=data1/ra
  w=round(ndat/10)
  start=10
  end=length(data1)-10
  if(length(data1)<50){
    start=5
    end=length(data1)-5
  }
  
  interval=round((end-start)/10)
  if(interval<=2){
    interval=3
  }
  e=1:((end-start)/interval+1)
  print('nonparas results:')
  if(length(t)<200){
    for(u in seq(start,end,by=interval)){
      print(u)
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
      # print(MSE)
      e[(u-start)/interval+1]=MSE
    }
    u=(which.min(e)-1)*interval+start
    if(i==5){
      u=300
    }
    # u=300
    t=t*ra
    nt=length(t)
    t1=t[1:u]
    t2=t[(u):nt]
    t3=t2-t[u]
    result1=dfr(t1)
    result2=ifr(t3)
    l1=result1$l
    l2=result2$l
    l=c(l1,l2+l1[length(l1)])
    su=c(result1$su,result2$su+result1$su[length(result1$su)])
    # MSE=sqrt((result1$MSE*(length(t1)-1))^2+(result2$MSE*(length(t2)-1))^2)/(length(t)-1)
    t0 = round(t)
    t0=replace(t0,t0>length(l),length(l))
    t0=replace(t0,t0==0,1)
    MSE=sqrt(sum((su[t0]-0:(length(data1)-1))^2))/(length(data1)-1)
    MLE=result1$MLE+result2$MLE
    lines(l,su,lty=2,lwd=2,col=c('purple'))
    legend(x=c(0),y=c(ndat-6*c),cex=1.5,legend = name2[3],col = c('purple'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
    print(MLE)
    print(MSE)
    print(u)
    method_results <- list(l, su, MSE, MLE, u)
    dataset_results[[paste("Method", "ifr_dfr", sep = "_")]] <- method_results
  }
  else{
    for(u in seq(start,end,by=interval)){
      # print(u)
      nt=length(t)
      t1=t[1:u]
      t2=t[(u):nt]
      t3=t2-t[u]
      result1=dfr3(t1,w)
      result2=ifr3(t3,w)
      # t0 =result2$t0
      l1=result1$l
      l2=result2$l
      l=c(l1,l2+l1[length(l1)])
      su=c(result1$su,result2$su+result1$su[length(result1$su)])
      MSE=sqrt((result1$MSE*(length(t1)-1))^2+(result2$MSE*(length(t2)-1))^2)/(length(t)-1)
      # print(MSE)
      e[(u-start)/interval+1]=MSE
    }
    u=(which.min(e)-1)*interval+start
    # if(i==5){
    #   u=300
    # }
    # u=300
    t=t*ra
    nt=length(t)
    t1=t[1:u]
    t2=t[(u):nt]
    t3=t2-t[u]
    w=round(ndat/20)
    result1=dfr3(t1,w)
    result2=ifr3(t3,w)
    l1=result1$l
    l2=result2$l
    l=c(l1,l2+l1[length(l1)])
    su=c(result1$su,result2$su+result1$su[length(result1$su)])
    # MSE=sqrt((result1$MSE*(length(t1)-1))^2+(result2$MSE*(length(t2)-1))^2)/(length(t)-1)
    t0 = round(t)
    t0=replace(t0,t0>length(l),length(l))
    t0=replace(t0,t0==0,1)
    MSE=sqrt(sum((su[t0]-0:(length(data1)-1))^2))/(length(data1)-1)
    MLE=result1$MLE+result2$MLE
    lines(l,su,lty=2,lwd=2,col=c('purple'))
    legend(x=c(0),y=c(ndat-6*c),cex=1.5,legend = name2[3],col = c('purple'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
    print(MLE)
    print(MSE)
    print(u)
    method_results <- list(l, su, MSE, MLE, u)
    dataset_results[[paste("Method", "ifr_dfr", sep = "_")]] <- method_results
  }
  
  
  
  ###DID
  xdat=data1
  m0=1:4*0
  m1=1:4*0
  for(k in 4:7){
    print(k)
    ndat1=ndat
    c1=c
    q0=100
    q1=max(data1)/10
    start1=min(data1)+q0
    end1=max(data1)-q0
    start2=min(data1)+q0
    end2=max(data1)-q0
    interval=q1
    E=matrix(1000,ncol=((end2-start2)/interval+1),nrow=((end1-start1)/interval+1))
    # knots=round((0:(k+1))*ndat/(k+1))
    # knots[1]=1
    # knots[k+2]=ndat
    # t=data1[knots]
    knots=round((0:(k+5))*ndat/(k+5))
    knots[1]=1
    knots[k+6]=ndat
    knots=c(knots[1],knots[6:length(knots)])
    t=data1[knots]
    m=k+2
    MSE1=c(1,1)
    for(u in seq(start1,end1-100,by=q1)){
      for(v in seq(u,end2,by=q1)){
        if((sum(u<t))==(sum(v<t))){
          next
        }
        # print(u)
        res=ss_con(data1,k,u,v,2)
        y=res$beta%*%res$M
        c0=max((length(data1)-1))/max(y)
        y0=res$beta%*%res$M0*c0
        MSE=sqrt(sum((0:(length(data1)-1)-y*c0)^2))/(length(data1)-1)
        beta=res$beta
        MSE1[2]=MSE
        if(MSE1[2]<MSE1[1]){
          MSE1[1]=MSE1[2]
          beta1=beta
        }
        E[(u-start1)/interval+1,(v-start2)/interval+1]=MSE
      }
    }
    
    u=(which(E==min(E),arr.ind = T)[1]-1)*interval+start1
    v=(which(E==min(E),arr.ind = T)[2]-1)*interval+start1
    # print(E)
    # print(u)
    # print(v)
    res=ss_con(data1,k,u,v,2)
    y=res$beta%*%res$M
    c0=(ndat-1)/max(y)
    y0=res$beta%*%res$M0*c0
    MLE= sum(log((res$beta*c0)%*%res$M0))-y[length(y)]*c0
    
    m0[k-3]=MLE-2*k
    # m1[k-3]=sqrt(sum((0:(length(xdat)-1)-y*c0)^2))/(length(xdat)-1)
  }
  k=which.max(m0)+3
  c1=c
  q0=100
  q1=max(data1)/25
  start1=min(data1)+q0
  end1=max(data1)-q0
  start2=min(data1)+q0
  end2=max(data1)-q0
  interval=q1
  # e=1:((end-start)/interval+1)
  E=matrix(1000,ncol=((end2-start2)/interval+1),nrow=((end1-start1)/interval+1))
  xdat=data1
  # k=10
  ndat=length(xdat)
  # knots=round((0:(k+1))*ndat/(k+1))
  # knots[1]=1
  # knots[k+2]=ndat
  knots=round((0:(k+5))*ndat/(k+5))
  knots[1]=1
  knots[k+6]=ndat
  knots=c(knots[1],knots[6:length(knots)])
  t=data1[knots]
  m=k+2
  MSE1=c(1,1)
  for(u in seq(start1,end1-100,by=q1)){
    for(v in seq(u,end2,by=q1)){
      if((sum(u<t))==(sum(v<t))){
        next
      }
      res=ss_con(xdat,k,u,v,2)
      y=res$beta%*%res$M
      c0=max((length(xdat)-1))/max(y)
      y0=res$beta%*%res$M0*c0
      MSE=sqrt(sum((0:(length(data1)-1)-y*c0)^2))/(length(xdat)-1)
      beta=res$beta
      MSE1[2]=MSE
      if(MSE1[2]<MSE1[1]){
        MSE1[1]=MSE1[2]
        beta1=beta
      }
      # print(u)
      # print(v)
      # plot(data1,0:(ndat-1),lty=1)
      # lines(xdat,y*c0)
      # print(MSE)
      E[(u-start1)/interval+1,(v-start2)/interval+1]=MSE
    }
  }
  u=(which(E==min(E),arr.ind = T)[1]-1)*interval+start1
  v=(which(E==min(E),arr.ind = T)[2]-1)*interval+start1
  res=ss_con(xdat,k,u,v,2)
  y=res$beta%*%res$M
  c0=(ndat-1)/max(y)
  y0=res$beta%*%res$M0*c0
  lines(data1,y*c0,lty=1,lwd=2,col=c('cyan'))
  c=c1
  legend(x=c(0),y=c(ndat-5*c),cex=1.5,legend = name3[3],col = c('cyan'),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  MLE= sum(log((res$beta*c0)%*%res$M0))-y[length(y)]*c0
  print('DID_result')
  print(k)
  print(MLE)
  MSE=sqrt(sum((0:(length(xdat)-1)-y*c0)^2))/(length(xdat)-1)
  print(MSE)
  print(u)
  print(v)
  method_results <- list(data1, y*c0, MSE, MLE, u, v ,k)
  dataset_results[[paste("Method", "DID", sep = "_")]] <- method_results
  
  
  
  ####IDI
  m0=1:4*0
  for(k in 4:7){
    print(k)
    ndat1=ndat
    c1=c
    q0=100
    q1=1000
    start1=min(data1)+q0
    end1=max(data1)-q0
    start2=min(data1)+q0
    end2=max(data1)-q0
    interval=q1
    E=matrix(1000,ncol=((end2-start2)/interval+1),nrow=((end1-start1)/interval+1))
    ndat=length(data1)
    # knots=round((0:(k+1))*ndat/(k+1))
    # knots[1]=1
    # knots[k+2]=ndat
    # t=data1[knots]
    knots=round((0:(k+5))*ndat/(k+5))
    knots[1]=1
    knots[k+6]=ndat
    knots=c(knots[1],knots[6:length(knots)])
    t=data1[knots]
    m=k+2
    MSE1=c(1,1)
    for(u in seq(start1,end1-100,by=q1)){
      for(v in seq(u,end2,by=q1)){
        if((sum(u<t))==(sum(v<t))){
          next
        }
        res=ss_con(data1,k,u,v,1)
        y=res$beta%*%res$M
        c0=max((length(xdat)-1))/max(y)
        y0=res$beta%*%res$M0*c0
        MSE=sqrt(sum((0:(length(data1)-1)-y*c0)^2))/(length(xdat)-1)
        beta=res$beta
        MSE1[2]=MSE
        if(MSE1[2]<MSE1[1]){
          MSE1[1]=MSE1[2]
          beta1=beta
        }
        E[(u-start1)/interval+1,(v-start2)/interval+1]=MSE
      }
    }
    u=(which(E==min(E),arr.ind = T)[1]-1)*interval+start1
    v=(which(E==min(E),arr.ind = T)[2]-1)*interval+start1
    res=ss_con(data1,k,u,v,1)
    y=res$beta%*%res$M
    c0=(ndat-1)/max(y)
    y0=res$beta%*%res$M0*c0
    # MLE=sum(dn*log((res$beta*c0)%*%t(apply(res$M,1, diff))))-y[length(y)]*c0
    MLE= sum(log((res$beta*c0)%*%res$M0))-y[length(y)]*c0
    m0[k-3]=MLE-2*k
  }
  k=which.max(m0)+3
  c1=c
  start1=min(data1)+q0
  end1=max(data1)-q0
  start2=min(data1)+q0
  end2=max(data1)-q0
  q1=max(data1)/25
  interval=q1
  E=matrix(1000,ncol=((end2-start2)/interval+1),nrow=((end1-start1)/interval+1))
  xdat=data1
  # k=10
  ndat=length(xdat)
  # knots=round((0:(k+1))*ndat/(k+1))
  # knots[1]=1
  # knots[k+2]=ndat
  # t=xdat[knots]
  knots=round((0:(k+5))*ndat/(k+5))
  knots[1]=1
  knots[k+6]=ndat
  knots=c(knots[1],knots[6:length(knots)])
  t=data1[knots]
  m=k+2
  for(u in seq(start1,end1-100,by=q1)){
    for(v in seq(u,end2,by=q1)){
      if((sum(u<t))==(sum(v<t))){
        next
      }
      res=ss_con(xdat,k,u,v,1)
      y=res$beta%*%res$M
      c0=max((length(xdat)-1))/max(y)
      y0=res$beta%*%res$M0*c0
      MSE=sqrt(sum((0:(length(xdat)-1)-y*c0)^2))/(length(xdat)-1)
      # print(u)
      # print(v)
      # plot(xdat,0:(length(xdat)-1))
      # lines(xdat,y*c0)
      # print(MSE)
      E[(u-start1)/interval+1,(v-start2)/interval+1]=MSE
    }
  }
  u=start1+(which(E==min(E),arr.ind = T)[1]-1)*interval
  v=start2+(which(E==min(E),arr.ind = T)[2]-1)*interval
  res=ss_con(xdat,k,u,v,1)
  y=res$beta%*%res$M
  c0=max(0:(length(xdat)-1))/max(y)
  y0=res$beta%*%res$M0*c0
  
  lines(data1,y*c0,lty=1,lwd=2,col= 'gold')
  c=c1
  legend(x=c(0),ndat-4*c,cex=1.5,legend =name3[4],col = 'gold',lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  MLE= sum(log((res$beta*c0)%*%res$M0))-y[length(y)]*c0
  print("IDI_result")
  print(k)
  print(MLE)
  MSE=sqrt(sum((0:(length(xdat)-1)-y*c0)^2))/(length(xdat)-1)
  print(MSE)
  print(u)
  print(v)
  method_results <- list(data1, y*c0, MSE, MLE, u, v,k)
  dataset_results[[paste("Method", "IDI", sep = "_")]] <- method_results
  results_list[[paste("Dataset", i00, sep = "_")]] <- dataset_results
}

# saveRDS(results_list, file.path("analysis_result", "DCAS", paste(paste('DCAS_results',percent,sep='_'),'.rds',sep='')))
# 
# 
# ####save MSE
# percent=0.8
# loaded_results <- readRDS(file.path("analysis_result", "DCAS", paste(paste('DCAS_results',percent,sep='_'),'.rds',sep='')))
# i=0
# c=matrix(1,8,16)
# for (x in loaded_results) {
#   i=i+1
#   print(i)
#   j=0
#   for ( y in x) {
#     j=j+1
#     c[j,i]=round(y[[3]],digit=3)
#     # c=paste(c,round(y[[3]],digit=3),seq='')
#     print(round(y[[3]],digit=3))
#     # c.append(1)
#   }
# }
# for (x in loaded_results) {
#   print(x[[1]][[3]])
# }
# c=as.data.frame(c)
# write.csv(c,paste(paste('MSE',percent,sep='_'),'.csv',sep=''),row.names = FALSE)
