source('basic_functions/function.r')
Data1= c(0,411,411,362,192,228,229,175,201,177,104,109,58)
Data2= c(0,7,22,32,47,26,25,16,48,36,53,57,39,71,80,65,57,90,60,57,90,46,57,29,40,16,18,37,15,8,28,6,5,3,3,12)
Data3=c(0,44,135,210,162,97,39,72,83,147,127,77,41,5)
Data4=c(0,203,136,183,47,46,71,54,57,80,64,27,42,55,62,11)
Data5=c(0,238,179,222,73,56,88,78,92,104,85,53,45,59,84,27)
Data6=c(0,369,298,398,115,83,150,142,172,202,168,89,87,111,253,70)
Data7=c(0,334,255,359,89,73,133,118,137,178,147,63,84,107,231,54)
Data1=cumsum(Data1)
Data2=cumsum(Data2)
Data3=cumsum(Data3)
Data4=cumsum(Data4)
Data5=cumsum(Data5)
Data6=cumsum(Data6)
Data7=cumsum(Data7)
# n1 = c(0,16,24,27,33,41,49,54,58,69,75,81,86,90,93,96,98,99,100,100,100)
# t1 = c(0,519,968,1430,1893,2490,3058,3625,4422,5218,5823,6539,7083,7487,7846,8205,8564,8923,9282,9641,10000)
# n2 = c(0,13,18,26,34,40,48,61,75, 84, 89, 95, 100, 104, 110 ,112, 114, 117, 118, 120)
# t2 = c(0,384,1186,1471,2236,2772,2967,3812,4880,6104,6634,7229,8072,8484,8847,9253,9712,10083,10174,10272)
# n3 = c(0,6, 9, 13, 20, 28, 40, 48, 54, 57, 59, 60, 61)
# t3 = c(0,162, 499, 715, 1137, 1799 ,2438 ,2818, 3574 ,4234, 4680 ,4955, 5053)
# n4 = c(0,1,3,8,9,11,16,19,25,27,29,32,32,36,38,39,39,41,42,42)
# t4 = c(0,254,788,1054,1393,2216,2880,3593,4281,5180,6003,7621,8783,9604,10064,10560,11008,11237,11243,11305)
func_name=c('Goel_Okumoto','G_O','Hossain_Dahiya','Gompertz','Pareto','Weibull','Yamada_Exp','Yamada_Raleigh')

# #####1980 Data
# Data1= c(0,411,411,362,192,228,229,175,201,177,104,109,58)
# Data2= c(0,7,22,32,47,26,25,16,48,36,53,57,39,71,80,65,57,90,60,57,90,46,57,29,40,16,18,37,15,8,28,6,5,3,3,12)
# Data3= c(0,44,135,210,162,97,39,72,83,147,127,77,41,5)
# Data4= c(0,203,136,183,47,46,71,54,57,80,64,27,42,55,62,11)
# Data5= c(0,238,179,222,73,56,88,78,92,104,85,53,45,59,84,27)
# Data6= c(0,369,298,398,115,83,150,142,172,202,168,89,87,111,253,70)
# Data7= c(0,334,255,359,89,73,133,118,137,178,147,63,84,107,231,54)
# 
# 
t1=1:length(Data1)*720-720
t2=1:length(Data2)*720-720
t3=1:length(Data3)*720-720
t4=1:length(Data4)*720-720
t5=1:length(Data5)*720-720
t6=1:length(Data6)*720-720
t7=1:length(Data7)*720-720


func_name=c('Goel_Okumoto','G_O','Hossain_Dahiya','Gompertz','Pareto','Weibull','Yamada_Exp','Yamada_Raleigh')

name1=c('G-O','G-O-S','H-D','Gompertz','Pareto','Weibull','Y-Exp','Y-M','DFR-IFR')
name2=c('IFR','DFR','DFR-IFR')
name3=c('Spline','S-shape','Spline-DID','Spline-IDI')

source('basic_functions/discon-new.R')
source('basic_functions/para-dis.R')
t00=list(t1,t2,t3,t4,t5,t6,t7)
nnew=list(Data1,Data2,Data3,Data4,Data5,Data6,Data7)

best=c(3,6,2,5,6,6,6)
###plot for tandem data
color=c('red','blue','cyan','brown','green','gold','purple')
name=list('Spline','Penalize','IFR','DFR','G-O','G-O-S','H-D','Gompertz','Pareto','Weibull','Y-Exp','Y-M')
p=c(3,4,5,6)
p1=c(0,1,2,6,3,4,5)
# par(cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
# 
# pdf("New data.pdf", width=5, height=3.5)
# plot(c(0,26000),c(0,4000),pch='',xlab = 'Test hours',ylab='Number of failures')
# for(i in 1:7){
#   lines(t00[[i]],nnew[[i]],type='o',lwd=2,col=color[i],pch=p1[i])
#   legend(x=c(0),y=4100-60*i,cex = 1.5,legend = paste("Data",i),col = color[i],lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
# }
# 
# 
# dev.off()  # 关闭目标图形设备
# par(cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)

# plot(c(0,11000),c(0,2500),pch='',xlab = 'Test hours',ylab='Number of failures',main='Data7')
# for(i in c(7)){
#   lines(t00[[i]],nnew[[i]],type='o',lwd=2,col=color[i],pch=p1[i])
# }


results_list <- list()
for (i in 2) {
  i00=i
  dataset_results <- list()
  print('Number')
  print(i)
  
  data1=t00[[i]]
  n0=nnew[[i]]
  dn=gen(n0)
  c=max(n0)/25
  
  plot(data1,n0,type='l',lwd=3,xlab = 'Test hours',ylab='Number of failures',main=paste('Behaviors of mean value function on release',i))
  legend(x=c(0),y=max(n0),cex = 1.5,legend = 'real data',col = c('black'),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  ndat=max(n0)
  c=ndat/25
  
  list0=list(c(0.00016),c(0.00006),c(0.00015,0.0000026),c(0.06,0.9995),c(0.0000005,0.64),c(1.31,0.00008),c(0.15,0.0000001))
  res=para_dis(data1,best[i],n=n0,theta=list0[[best[i]]])
  lines(res$l,res$su,lty=2,lwd=2,col='blue')
  legend(x=c(0),y=c(ndat-1*c),cex = 1.5,legend = name1[[best[i]]],col = c('blue'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  print('parametric results:')
  print(res$MLE)
  print(res$MSE)
  MSE=res$MSE
  MLE=res$MLE
  method_results <- list(res$l, res$su, MSE, MLE)
  dataset_results[[paste("Method", "parametric", sep = "_")]] <- method_results

  #
  # res=para_dis(data1,1,n=n0,theta=0.00016)
  # lines(res$l,res$su,lty=2,lwd=2,col='blue')
  # legend(x=c(0),y=c(ndat-2*c),legend = name[[5]],col = c('blue'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  # print(res$MLE)
  # print(res$MSE)
  # res=para_dis(data1,2,n0,theta=0.00006)
  # lines(res$l,res$su,lty=2,lwd=2,col='red')
  # legend(x=c(0),y=c(ndat-3*c),legend = name[[6]],col = c('red'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  # print(res$MLE)
  # print(res$MSE)
  # res=para_dis(data1,3,n0,theta=c(0.00015,0.0000026))
  # lines(res$l,res$su,lty=2,lwd=2,col='yellow')
  # legend(x=c(0),y=ndat-4*c,legend = name[[7]],col = c('yellow'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  # print(res$MLE)
  # print(res$MSE)
  # res=para_dis(data1,4,n0,theta = c(0.06,0.9995))
  # lines(res$l,res$su,lty=2,lwd=2,col='orange')
  # legend(x=c(0),ndat-5*c,legend = name[[8]],col = c('orange'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  # print(res$MLE)
  # print(res$MSE)
  # res=para_dis(data1,5,n0,theta=c(0.0000005,2216))
  # lines(res$l,res$su,lty=2,lwd=2,col='brown')
  # legend(x=c(0),ndat-6*c,legend =name[[9]],col = c('brown'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  # print(res$MLE)
  # print(res$MSE)
  # res=para_dis(data1,6,n0,theta=c(0.000000002,0.64))
  # lines(res$l,res$su,lty=2,lwd=2,col='black')
  # legend(x=c(0),y=ndat-7*c,legend = name[[10]],col = c('black'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  # print(res$MLE)
  # print(res$MSE)
  # res=para_dis(data1,7,n0,theta=c(1.31,0.00008))
  # lines(res$l,res$su,lty=2,lwd=2,col='purple')
  # legend(x=c(0),ndat-8*c,legend = name[[11]],col = c('purple'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  # print(res$MLE)
  # print(res$MSE)
  # res=para_dis(data1,8,n0,theta = c(0.15,0.0000001))
  # lines(res$l,res$su,lty=2,lwd=2,col='pink')
  # legend(x=c(0),ndat-9*c,legend = name[[12]],col = c('pink'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  # print(res$MLE)
  # print(res$MSE)
  #

  ####IFR
  a=gen(data1)/gen(n0)
  xdat=c(0,cumsum(rep(a,gen(n0))))
  w=max(xdat)/20

  p=1
  res=nonpara_red(xdat,ifr3,w,eval(parse(text = func_name[p])),list0[[1]])
  su2=res$su[data1+c(1:(length(data1)-1)*0+1,0)]
  # uu=which(gen(su2)>0)
  # MLE=sum(gen(n0)[uu]*log(gen(su2)[uu]))-su2[length(su2)]
  MLE=sum(gen(n0)*log(gen(su2)))-su2[length(su2)]
  MSE=sqrt(sum((su2-n0)^2))/(length(n0)-1)
  print('ifr_results')
  print(MLE)
  print(MSE)
  lines(res$l,res$su,lty=2,lwd=2,col='yellowgreen')
  legend(x=c(0),ndat-8*c,cex = 1.5,legend = name2[[1]],col = c('yellowgreen'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)

  method_results <- list(res$l, res$su, MSE, MLE)
  dataset_results[[paste("Method", "ifr", sep = "_")]] <- method_results

  ####DFR
  res=nonpara_red(xdat,dfr3,w,eval(parse(text = func_name[p])),list0[[1]])
  su2=res$su[data1+c(1:(length(data1)-1)*0+1,0)]
  # uu=which(gen(su2)>0)
  # MLE=sum(gen(n0)[uu]*log(gen(su2)[uu]))-su2[length(su2)]
  MLE=sum(gen(n0)*log(gen(su2)))-su2[length(su2)]
  MSE=sqrt(sum((su2-n0)^2))/(length(n0)-1)
  print('dfr results')
  print(MLE)
  print(MSE)
  lines(res$l,res$su,lty=2,lwd=2,col='yellow3')
  legend(x=c(0),ndat-7*c,cex = 1.5,legend = name2[[2]],col = c('yellow3'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)

  method_results <- list(res$l, res$su, MSE, MLE)
  dataset_results[[paste("Method", "dfr", sep = "_")]] <- method_results

  #####spline
  m0=1:4*0
  for(k in 3:6){
    res=discon(n0,data1,k)
    m0[k-2]=res$MLE-2*k
  }
  k=which.max(m0)+2
  res=discon(n0,data1,k)
  lines(res$l,res$su,lty=1,lwd=3,col='tomato4')
  legend(x=c(0),ndat-2*c,cex = 1.5,legend = name3[[1]],col = c('tomato4'),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  print('spline results')
  print(k)
  print(res$MLE)
  print(res$MSE)
  MLE=res$MLE
  MSE=res$MSE
  method_results <- list(res$l, res$su, MSE, MLE, k)
  dataset_results[[paste("Method", "spline", sep = "_")]] <- method_results


  #### s-shape spline
  m0=1:4*0
  for(k in c(4,5,6,7)){
    print(k)
    start=min(data1)+10
    end=max(data1)-10
    interval=max(data1)/10
    e=1:((end-start)/interval+1)
    for(u0 in seq(start,end,by=interval)){
      res2=spline_dis_s(n0,data1,k,u0)
      # print(u0)
      M=res2$M
      y=t(res2$beta)%*%M
      c0=max(n)/ max(y)
      MSE=sqrt(sum((n0-y*c0)^2))/(length(data1)-1)
      e[(u0-start)/interval+1]=MSE
    }
    u0=(which.min(e)-1)*interval+start
    res=spline_dis_s(n0,data1,k,u0)
    M=res$M
    y=t(res$beta)%*%M
    c0=max(n0)/max(y)
    MLE=sum(dn*log((res$beta*c0)%*%t(apply(res$M,1, diff))))-y[length(y)]*c0
    m0[k-3]=MLE-2*k
  }
  k=which.max(m0)+3
  start=min(data1)+10
  end=max(data1)-10
  interval=max(data1)/100
  for(u0 in seq(start,end,by=interval)){
    res2=spline_dis_s(n0,data1,k,u0)
    M=res2$M
    y=t(res2$beta)%*%M
    c0=max(n0)/max(y)

    ###MSE
    MSE=sqrt(sum((n0-y*c0)^2))/(length(data1)-1)
    e[(u0-start)/interval+1]=MSE
    # print(u0)
    # plot(data1,n0)
    # lines(data1,y*c0)
    # print(MSE)
  }
  u0=(which.min(e)-1)*interval+start
  res=spline_dis_s(n0,data1,k,u0)
  M=res$M
  y=t(res$beta)%*%M
  c0=max(n0)/max(y)
  lines(data1,y*c0,lty=1,lwd=2,col='salmon')
  legend(x=c(0),y=c(ndat-3*c),cex = 1.5,legend = name3[2],col = c('salmon'),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  # print(sum(log(c0*res$beta%*%res$M0))-max(n))
  MLE=sum(dn*log((res$beta*c0)%*%t(apply(res$M,1, diff))))-y[length(y)]*c0
  print('s-spline results')
  print(k)
  print(MLE)
  MSE=sqrt(sum((n0-y*c0)^2))/(length(data1)-1)
  print(sqrt(sum((n0-y*c0)^2))/(length(data1)-1))
  print(u0)
  method_results <- list(data1, y*c0, MSE, MLE, u0,k)
  dataset_results[[paste("Method", "S_shape", sep = "_")]] <- method_results

  ####nonpara-s
  t=xdat
  w=round(max(xdat)/10)
  start=2
  end=length(xdat)-10
  interval=round(max(n0)/10)
  e=1:((end-start)/interval+1)
  for(u in seq(start,end,by=interval)){
    print(u)
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
  w=round(max(xdat)/20)
  u=(which.min(e)-1)*interval+start
  nt=length(t)
  t1=t[1:u]
  t2=t[(u):nt]
  t3=t2-t[u]
  result1=dfr3(t1,w)
  result2=ifr3(t3,w)
  l1=result1$l
  l2=result2$l
  l=c(l1,l2+l1[length(l1)])
  su=c(result1$su,result2$su+result1$su[length(result1$su)])

  # MSE=sqrt((result1$MSE*(length(t1)-1))^2+(result2$MSE*(length(t2)-1))^2)/(length(t)-1)
  # MSE=sqrt(sum((su[t0]-0:(length(xdat)-1))^2))/(length(xdat)-1)
  # MLE=result1$MLE+result2$MLE

  su2=su[data1+c(1:(length(data1)-1)*0+1,-1)]
  MSE=sqrt(sum((su2-n0)^2))/(length(n0)-1)
  MLE=sum(gen(n0)*log(gen(su2)))-su2[length(su2)]


  lines(l,su,lty=2,lwd=2,col=c('purple'))
  legend(x=c(0),y=c(ndat-6*c),legend = name2[3],cex = 1.5,col = c('purple'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  print('nonparas results:')
  print(MLE)
  print(MSE)
  print(u)
  method_results <- list(l, su, MSE, MLE, u)
  dataset_results[[paste("Method", "ifr_dfr", sep = "_")]] <- method_results


  #DID
  m0=1:4*0
  for(k in 4:7){
    print(k)
    ndat1=ndat
    c1=c
    q0=100
    q1=500
    start1=min(data1)+q0
    end1=max(data1)-q0
    start2=min(data1)+q0
    end2=max(data1)-q0
    interval=q1
    E=matrix(1,ncol=((end2-start2)/interval+1),nrow=((end1-start1)/interval+1))
    ndat=length(n0)
    knots=round((0:(k+1))*ndat/(k+1))
    knots[1]=1
    knots[k+2]=ndat
    t=data1[knots]
    m=k+2
    MSE1=c(1,1)
    for(u in seq(start1,end1-100,by=q1)){
      for(v in seq(u,end2,by=q1)){
        if((sum(u<t))==(sum(v<t))){
          next
        }
        res=ss_dis(n0,data1,k,u,v,2)
        y=res$beta%*%res$M
        c0=max((length(xdat)-1))/max(y)
        y0=res$beta%*%res$M0*c0
        MSE=sqrt(sum((n0-y*c0)^2))/(length(xdat)-1)
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
    res=ss_dis(n0,data1,k,u,v,2)
    y=res$beta%*%res$M
    c0=max(n0)/max(y)
    y0=res$beta%*%res$M0*c0
    MLE=sum(dn*log((res$beta*c0)%*%t(apply(res$M,1, diff))))-y[length(y)]*c0
    m0[k-3]=MLE-2*k

  }
  k=which.max(m0)+3
  # k=6
  c1=c
  q0=100
  q1=max(xdat)/50
  start1=min(data1)+q0
  end1=max(data1)-q0
  start2=min(data1)+q0
  end2=max(data1)-q0
  # q1=100
  # start1=500
  # start2=1000
  # end1=1500
  # end2=3000
  #
  interval=q1
  # e=1:((end-start)/interval+1)
  ndat1=ndat
  E=matrix(100,ncol=((end2-start2)/interval+1),nrow=((end1-start1)/interval+1))
  ndat=length(n0)
  # knots=round((0:(k+1))*ndat/(k+1))
  # knots[1]=1
  # knots[k+2]=ndat
  knots=round((0:(k+1))*ndat/(k+1))
  knots[1]=1
  knots[k+2]=ndat
  t=data1[knots]
  m=k+2
  MSE1=c(1,1)
  for(u in seq(start1,end1-100,by=q1)){
    for(v in seq(u,end2,by=q1)){
      if((sum(u<t))==(sum(v<t))){
        next
      }
      res=ss_dis(n0,data1,k,u,v,2)
      y=res$beta%*%res$M
      c0=max(n0)/max(y)
      y0=res$beta%*%res$M0*c0
      MSE=sqrt(sum((n0-y*c0)^2))/(length(data1)-1)
      beta=res$beta
      MSE1[2]=MSE
      if(MSE1[2]<MSE1[1]){
        MSE1[1]=MSE1[2]
        beta1=beta
      }
      # print(u)
      # print(v)
      # plot(data1,n0,lty=1)
      # lines(data1,y*c0)
      # print(MSE)
      E[(u-start1)/interval+1,(v-start2)/interval+1]=MSE
    }
  }
  u=(which(E==min(E),arr.ind = T)[1]-1)*interval+start1
  v=(which(E==min(E),arr.ind = T)[2]-1)*interval+start1
  res=ss_dis(n0,data1,k,u,v,2)
  y=res$beta%*%res$M
  c0=max(n0)/max(y)
  y0=res$beta%*%res$M0*c0
  lines(data1,y*c0,lty=1,lwd=2,col=c('cyan'))
  c=c1
  ndat=ndat1
  legend(x=c(0),y=c(ndat-5*c),cex = 1.5,legend = name3[3],col = c('cyan'),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  # MLE= sum(log((res$beta*c0)%*%res$M0))-y[length(y)]*c0
  MLE=sum(dn*log((res$beta*c0)%*%t(apply(res$M,1, diff))))-y[length(y)]*c0
  print('did results')
  print(k)
  print(MLE)
  MSE=sqrt(sum((n0-y*c0)^2))/(length(data1)-1)
  print(MSE)
  print(u)
  print(v)
  method_results <- list(data1, y*c0, MSE, MLE, u, v ,k)
  dataset_results[[paste("Method", "DID", sep = "_")]] <- method_results
  
  

  ###IDI
  q0=100
  q1=500
  m0=1:4*0
  for(k in 4:7){
    print(k)
    c1=c
    start1=min(data1)+q0
    end1=max(data1)-q0
    start2=min(data1)+q0
    end2=max(data1)-q0
    interval=q1
    E=matrix(1,ncol=((end2-start2)/interval+1),nrow=((end1-start1)/interval+1))
    ndat=length(n0)
    knots=round((0:(k+1))*ndat/(k+1))
    knots[1]=1
    knots[k+2]=ndat
    t=xdat[knots]
    m=k+2
    for(u in seq(start1,end1-100,by=q1)){
      for(v in seq(u,end2,by=q1)){
        if((sum(u<t))==(sum(v<t))){
          next
        }
        res=ss_dis(n0,data1,k,u,v,1)
        y=res$beta%*%res$M
        c0=max(n0)/max(y)
        y0=res$beta%*%res$M0*c0
        MSE=sqrt(sum((n0-y*c0)^2))/(length(data1)-1)
        E[(u-start1)/interval+1,(v-start2)/interval+1]=MSE
      }
    }
    u=start1+(which(E==min(E),arr.ind = T)[1]-1)*interval
    v=start2+(which(E==min(E),arr.ind = T)[2]-1)*interval
    res=ss_dis(n0,data1,k,u,v,1)
    y=res$beta%*%res$M
    c0=max(n0)/max(y)
    y0=res$beta%*%res$M0*c0
    MLE=sum(dn*log((res$beta*c0)%*%t(apply(res$M,1, diff))))-y[length(y)]*c0
    m0[k-3]=MLE-2*k
  }
  print(m0)
  k=which.max(m0)+3
  c1=c
  start1=min(data1)+q0
  end1=max(data1)-q0
  start2=min(data1)+q0
  end2=max(data1)-q0
  interval=q1
  E=matrix(100,ncol=((end2-start2)/interval+1),nrow=((end1-start1)/interval+1))
  ndat=length(n0)
  knots=round((0:(k+1))*ndat/(k+1))
  knots[1]=1
  knots[k+2]=ndat
  ndat1=ndat
  t=data1[knots]
  m=k+2
  for(u in seq(start1,end1-100,by=q1)){
    for(v in seq(u,end2,by=q1)){
      if((sum(u<t))==(sum(v<t))){
        next
      }
      res=ss_dis(n0,data1,k,u,v,1)
      y=res$beta%*%res$M
      c0=max(n0)/max(y)
      y0=res$beta%*%res$M0*c0
      MSE=sqrt(sum((n0-y*c0)^2))/(length(data1)-1)
      # print(u)
      # print(v)
      # plot(data1,n0,lty=1)
      # lines(data1,y*c0)
      # print(MSE)
      E[(u-start1)/interval+1,(v-start2)/interval+1]=MSE
    }
  }
  u=start1+(which(E==min(E),arr.ind = T)[1]-1)*interval
  v=start2+(which(E==min(E),arr.ind = T)[2]-1)*interval
  res=ss_dis(n0,data1,k,u,v,1)
  y=res$beta%*%res$M
  c0=max(n0)/max(y)
  y0=res$beta%*%res$M0*c0

  lines(data1,y*c0,lty=1,lwd=2,col= 'gold')
  c=c1
  ndat=ndat1
  legend(x=c(0),ndat-4*c,legend =name3[4],cex = 1.5,col = 'gold',lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  MLE=sum(dn*log((res$beta*c0)%*%t(apply(res$M,1, diff))))-y[length(y)]*c0
  print('IDI results:')
  print(k)
  print(MLE)
  MSE=sqrt(sum((n0-y*c0)^2))/(length(data1)-1)
  print(MSE)
  print(u)
  print(v)
  method_results <- list(data1, y*c0, MSE, MLE, u, v,k)
  dataset_results[[paste("Method", "IDI", sep = "_")]] <- method_results
  results_list[[paste("Dataset", i00, sep = "_")]] <- dataset_results
}
saveRDS(results_list, file.path("analysis_result", "MUSA", "MUSA_results.rds"))
# loaded_results <- readRDS(file.path("analysis_result", "MUSA", "MUSA_results.rds"))
