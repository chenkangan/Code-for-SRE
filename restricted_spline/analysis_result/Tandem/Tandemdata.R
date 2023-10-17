# xdat = c(0, 32.4375, 64.875, 97.3125, 129.75, 162.1875, 194.625, 227.0625, 259.5, 291.9375, 324.375, 356.8125, 389.25, 421.6875, 454.125, 486.5625, 519.0, 575.125, 631.25, 687.375, 743.5, 799.625, 855.75, 911.875, 968.0, 1122.0, 1276.0, 1430.0, 1507.1666666666667, 1584.3333333333333, 1661.5, 1738.6666666666667, 1815.8333333333335, 1893.0, 1967.625, 2042.25, 2116.875, 2191.5, 2266.125, 2340.75, 2415.375, 2490.0, 2561.0, 2632.0, 2703.0, 2774.0, 2845.0, 2916.0, 2987.0, 3058.0, 3171.4, 3284.8, 3398.2, 3511.6, 3625.0, 3824.25, 4023.5, 4222.75, 4422.0, 4494.363636363636, 4566.727272727273, 4639.090909090909, 4711.454545454545, 4783.818181818182, 4856.181818181818, 4928.545454545454, 5000.909090909091, 5073.272727272727, 5145.636363636364, 5218.0, 5318.833333333333, 5419.666666666667, 5520.5, 5621.333333333333, 5722.166666666667, 5823.0, 5942.333333333333, 6061.666666666667, 6181.0, 6300.333333333333, 6419.666666666667, 6539.0, 6647.8, 6756.6, 6865.4, 6974.2, 7083.0, 7184.0, 7285.0, 7386.0, 7487.0, 7606.666666666667, 7726.333333333333, 7846.0, 7965.666666666667, 8085.333333333333, 8205.0, 8384.5, 8564.0, 8923.0, 9282.0)
n1 = c(0,16,24,27,33,41,49,54,58,69,75,81,86,90,93,96,98,99,100,100,100)
t1 = c(0,519,968,1430,1893,2490,3058,3625,4422,5218,5823,6539,7083,7487,7846,8205,8564,8923,9282,9641,10000)
n2 = c(0,13,18,26,34,40,48,61,75, 84, 89, 95, 100, 104, 110 ,112, 114, 117, 118, 120)
t2 = c(0,384,1186,1471,2236,2772,2967,3812,4880,6104,6634,7229,8072,8484,8847,9253,9712,10083,10174,10272)
n3 = c(0,6, 9, 13, 20, 28, 40, 48, 54, 57, 59, 60, 61)
t3 = c(0,162, 499, 715, 1137, 1799 ,2438 ,2818, 3574 ,4234, 4680 ,4955, 5053)
n4 = c(0,1,3,8,9,11,16,19,25,27,29,32,32,36,38,39,39,41,42,42)
t4 = c(0,254,788,1054,1393,2216,2880,3593,4281,5180,6003,7621,8783,9604,10064,10560,11008,11237,11243,11305)

func_name=c('Goel_Okumoto','G_O','Hossain_Dahiya','Gompertz','Pareto','Weibull','Yamada_Exp','Yamada_Raleigh')

name1=c('G-O','G-O-S','H-D','Gompertz','Pareto','Weibull','Y-Exp','Y-M','DFR-IFR')
name2=c('IFR','DFR','DFR-IFR')
name3=c('Spline','S-shape','Spline-DID','Spline-IDI')

source ('basic_functions/function.R')
source('basic_functions/discon-new.R')
source('basic_functions/para-dis.R')
list0=list(c(0.002),c(0.002),c(0.002,.2), c(0.001,0.9999),c(0.002,.2),c(0.002,.2),c(0.02,.002), c(0.15,.00000015))

t00=list(t1,t2,t3,t4)
nnew=list(n1,n2,n3,n4)

best=c(3,1,3,6)
# ###plot for tandem data
# color=c('red','blue','cyan','brown')
# name=list('Spline','Penalize','IFR','DFR','G-O','G-O-S','H-D','Gompertz','Pareto','Weibull','Y-Exp','Y-M')
# p=c(3,4,5,6)
# p1=c(0,1,2,6)
# par(cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
# 
# pdf("Tandem.pdf", width=5, height=3.5)
# plot(c(0,12000),c(0,120),pch='',xlab = 'Test hours',ylab='Number of failures')
# for(i in 1:4){
#   lines(t00[[i]],nnew[[i]],type='o',lwd=2,col=color[i],pch=p1[i])
#   legend(x=c(0),y=128-6*i,cex = 1.5,legend = paste("Release",i),col = color[i],lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
# }
# 
# 
# dev.off()  # 关闭目标图形设备
par(cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)

# plot(c(0,12000),c(0,120),pch='',xlab = 'Test hours',ylab='Number of failures',main='Tandem data for all four releases')
# for(i in 1:4){
#   lines(t00[[i]],nnew[[i]],type='o',lwd=2,col=color[i],pch=p1[i])
#   
# }

# results_list <- list()
for (i in 4) {
  # c=4.5
  # if(i==2){
  #   c=5.5
  # }
  # if(i==3){
  #   c=2.5
  # }
  # if(i==4){
  #   c=2
  # }
  i00=i
  print(i)
  data1=t00[[i]]
  n0=nnew[[i]]
  dataset_results <- list()
  
  if(i==1){
    dat=data1
    nn=n0
  }

  ndat=max(n0)
  c=ndat/25
  if(i==1){
    l=c(1:(length(n0)-3),c(length(n0)))
    n0new=n0[l]
    data1new=data1[l]
    a=gen(data1new)/gen(n0new)
    xdat=c(0,cumsum(rep(a,gen(n0new))))
  }
  else if(i==4){
    l=c(1:11,13:15,17:18,20)
    n0new=n0[l]
    data1new=data1[l]
    a=gen(data1new)/gen(n0new)
    xdat=c(0,cumsum(rep(a,gen(n0new))))
  }
  else{
    a=gen(data1)/gen(n0)
    xdat=c(0,cumsum(rep(a,gen(n0))))
  }



  plot(data1,n0,type='l',lwd=3,xlab = 'Test hours',ylab='Number of failures',main=paste('Behaviors of mean value function on release',i))
  legend(x=c(0),y=ndat,cex = 1.5,legend = 'real data',col = c('black'),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)

  
  
  
  res=para_dis(data1,best[i],n=n0,theta=list0[[best[i]]])
  lines(res$l,res$su,lty=2,lwd=2,col='blue')
  legend(x=c(0),y=c(ndat-1*c),cex = 1.5,legend = name1[[best[i]]],col = c('blue'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  print('parametric results:')
  MSE=res$MSE
  MLE=res$MLE
  
  print(res$MLE)
  print(res$MSE)
  method_results <- list(res$l, res$su, MSE, MLE)
  dataset_results[[paste("Method", "parametric", sep = "_")]] <- method_results
  
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

  
  
  ####ifr
  p=1
  w=max(xdat)/50
  res=nonpara_red(xdat,ifr3,w,eval(parse(text = func_name[p])),list0[[1]])

  
  # res=nonpara(xdat,ifr1,eval(parse(text = func_name[p])),list0[[1]])
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
  
  
  
  ####dfr
  res=nonpara_red(xdat,dfr3,w,eval(parse(text = func_name[p])),list0[[1]])
  # res=nonpara(xdat,dfr1,eval(parse(text = func_name[p])),list0[[1]])
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
  
  
  
  ####spline
  # k=4
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
  MLE=res$MLE
  MSE=res$MSE
  print(res$MLE)
  print(res$MSE)
  method_results <- list(res$l, res$su, MSE, MLE, k)
  dataset_results[[paste("Method", "spline", sep = "_")]] <- method_results
  
  
  ##s-shape spline
  m0=1:7*0
  for(k in c(4,5,6,7,8,9,10)){
    start=min(data1)+2
    end=max(data1)-48
    interval=500
    e=1:((end-start)/interval+1)
    print(k)
    for(u0 in seq(start,end,by=interval)){
      # print(u0)
      res2=spline_dis_s(n0,data1,k,u0)
      M=res2$M
      y=t(res2$beta)%*%M
      c0=max(n)/max(y)
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
  interval=100
  for(u0 in seq(start,end,by=interval)){
    res2=spline_dis_s(n0,data1,k,u0)
    M=res2$M
    y=t(res2$beta)%*%M
    c0=max(n)/max(y)

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
  print(sqrt(sum((n0-y*c0)^2))/(length(data1)-1))
  MSE=sqrt(sum((n0-y*c0)^2))/(length(data1)-1)
  print(u0)
  method_results <- list(data1, y*c0, MSE, MLE, u0,k)
  dataset_results[[paste("Method", "S_shape", sep = "_")]] <- method_results
  


  ####nonpara-s
  t=xdat
  start=2
  w=round(max(xdat)/50)
  end=length(xdat)-10
  interval=round(max(n0)/10)
  e=1:((end-start)/interval+1)
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

  print('nonparas results:')
  lines(l,su,lty=2,lwd=2,col=c('purple'))
  legend(x=c(0),y=c(ndat-6*c),legend = name2[3],cex = 1.5,col = c('purple'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  print(MLE)
  print(MSE)
  print(u)
  method_results <- list(l, su, MSE, MLE, u)
  dataset_results[[paste("Method", "ifr_dfr", sep = "_")]] <- method_results
  
  
  # safe_divide <- function(x, y) {
  #   result <- tryCatch(
  #     expr = {
  #       # 尝试执行除法操作
  #       result <- x / y
  #       return(result)
  #     },
  #     error = function(e) {
  #       # 捕获错误并返回NA
  #       return(NA)
  #     }
  #   )
  #   return(result)
  # }
  
  # 测试 safe_divide 函数
  # result1 <- safe_divide(10, 2)  # 正常情况下，result1应该是5
  # result2 <- safe_divide(10, 0)  # 除以0，应该返回NA
  # 
  # print(result1)  # 打印结果
  # print(result2)  # 打印结果
  # 
  
  ##DID
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
  # if(i==1){
  #   k=4
  # }
  # if(i==2) {
  #   k=10
  # }
  # if(i==3){
  #   k=3
  # }
  # if(i==4){
  #   k=3
  # }
  k=which.max(m0)+3
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
  E=matrix(1,ncol=((end2-start2)/interval+1),nrow=((end1-start1)/interval+1))
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
      c0=max((length(xdat)-1))/max(y)

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
  method_results <- list(data1, y*c0, MSE, MLE, u, v,k)
  dataset_results[[paste("Method", "DID", sep = "_")]] <- method_results
  
  
      ####IDI
  
  for(k in 4:7){
    print(k)
    c1=c
    start1=min(data1)+q0
    end1=max(data1)-q0
    start2=min(data1)+q0
    end2=max(data1)-q0
    q1=1000
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
  # if(i==1){
  #   k=4
  # }
  # if(i==2) {
  #   k=4
  # }
  # if(i==3){
  #   k==3
  # }
  # if(i==4){
  #   k=1
  # }
  k=which.max(m0)+3
  c1=c
  start1=min(data1)+q0
  end1=max(data1)-q0
  start2=min(data1)+q0
  end2=max(data1)-q0
  q1=max(data1)/50
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
  method_results <- list(data1, y*c0, MSE, MLE, u, v, k)
  dataset_results[[paste("Method", "IDI", sep = "_")]] <- method_results
  results_list[[paste("Dataset", i00, sep = "_")]] <- dataset_results
  


  # if(i==2){
  #
  #     n1=nn
  #     xdat1=dat
  #     xdat=data1
  #     n=n0
  #
  #     xdat2=xdat
  #     n2=n
  #     ndat=length(xdat)
  #     v=50
  #     q=11
  #     ####calculating beta
  #     res1=dspl1(n1,xdat1,k)
  #     res2=dspl1(n2,xdat2,k)
  #     ans1=Mat(xdat1,k,v)
  #     ans2=Mat(xdat2,k,v)
  #     M1=ans1$M
  #     M2=ans2$M
  #
  #
  #     ###calculating a and b fit the data best
  #     opt=function(theta){
  #       a=theta[1]
  #       b=theta[2]
  #       l=b*max(xdat1)
  #       l=min(l,max(xdat2))
  #       l=ceiling(l/v)####EXPM：xdat1=6003,xdata2=9182, l=601
  #       return(sum((a*res1$beta%*%((Mat(b*xdat1,k,v)$M)[,1:l])-res2$beta%*%(M2[,1:l]))^2)/l)
  #     }
  #     # result <- ga(type = "real-valued", fitness = opt, lower = c(0.5, 0.5), upper = c(1.5, 1.5))
  #     # print(opt(1,1))
  #     N=matrix(1:(q*q),ncol=q,nrow=q)
  #     for(i in 1:q){
  #       for(j in 1:q){
  #         s=opt(c(i*0.05+0.7,j*0.05+0.7))
  #         N[i,j]=s
  #       }
  #     }
  #
  #     print('finish_ab')
  #
  #
  #     ###get a and b
  #     a=which(N==min(N),arr.ind=TRUE)[1]*0.05+0.8
  #     b=which(N==min(N),arr.ind=TRUE)[2]*0.05+0.8
  #     # a=1
  #     # b=1
  #     u= 10
  #     l=b*max(xdat1)
  #     l=floor(min(l,max(xdat2)))
  #     M20=Mat(xdat2,k,u)$M
  #     M1=Mat(b*xdat1,k,u)$M
  #     M1=M1[,1:(l/u)]
  #     M2=M20[,1:(l/u)]
  #     Sig=pinv(M2%*%t(M2))
  #     # print(Sig)
  #     objfun <- function(x) {
  #       sum((x%*%M2-res1$beta%*%M1)^2)
  #     }
  #     grad_fun <- function(x) {
  #       grad <- grad(f = objfun, x = x)
  #       return(grad)
  #     }
  #     # x0=c(1.886911e-11 ,3.762363e-08 ,5.927867e-04 ,1.183419e-02 ,7.380856e-03 ,1.682365e-07,4.764025e-03)
  #     x0=c(0.1,0.1,0.1,0.1,0.1,0.1,0.1)
  #     ci <- rep(0, m+1)
  #     A=diag(m+1)
  #     result <- constrOptim(x0, objfun, ui=A, ci=ci, grad=grad_fun)
  #     beta0=result$par
  #     # beta0=a*solve(M2%*%t(M2))%*%M2%*%t(M1)%*%res1$beta
  #     xdat=xdat2
  #     ndat=length(xdat)
  #     knots=round((0:(k+1))*ndat/(k+1))
  #     knots[1]=1
  #     knots[k+2]=ndat
  #     t=xdat[knots]
  #     m=k+2
  #     M=matrix(1:(length(xdat)*(k+3)),ncol = length(xdat),nrow = k+3)
  #     for (j in 1:(k+3)){
  #       for(i in 1:length(xdat)){
  #         M[j,i]=base_int(xdat[i],j,xdat,k,ndat,knots,t)
  #       }
  #     }
  #     M0=matrix(1:(length(xdat)*(k+3)),ncol = length(xdat),nrow = k+3)
  #     for (j in 1:(k+3)){
  #       for(i in 1:length(xdat)){
  #         M0[j,i]=base_fun(xdat[i],j,xdat,k,ndat,knots,t)
  #       }
  #     }
  #     l=seq(1,max(xdat),by=u)
  #     MM=matrix(1:(length(l)*(k+3)),ncol = length(l),nrow = k+3)
  #     for (j in 1:(k+3)){
  #       for(i in 1:length(l)){
  #         MM[j,i]=base_int(l[i],j,xdat,k,ndat,knots,t)
  #       }
  #     }
  #
  #     # 使用is_symmetric函数判断矩阵是否对称
  #
  #
  #     dn=gen(n)
  #     sigma=t(apply(M, 1, diff))
  #
  #     n1=20####number of segemnt
  #     start=0#####start value
  #     end=0.001####end..
  #     d=(1:n1)*0
  #     sig=0.1
  #     for (j in 1:n1) {
  #       w=start+(end-start)/n1*j###initial w
  #       x <- rmvnorm(n=5000000, mean=beta0, sigma=0.01*(w^-2)*symmetrize_matrix(Sig))####generating beta
  #       for(i in 1:ncol(x)){
  #         if(i==1){u=(x[,1]>0)}
  #         if(i>1){u=u*(x[,i]>0)}
  #       }
  #       y=x[which(u==1),]######calculating posterior beta
  #       d[j]=sum(exp(post(y)+516))/length(y)
  #     }
  #     print('finish_post')
  #     lamb=start+(end-start)/n1*(which.max(d))
  #     if(sum(is.nan(d))==length(d)){
  #       lamb=0.0001
  #       print('y is non')
  #     }
  #     q=10
  #     ans=dspl3(n,xdat,k,lamb)
  #     y=t(ans$beta)%*%M
  #     ym=t(ans$beta)%*%MM
  #     lines(xdat,y,type='l',lwd=2,col='blue')
  #     legend(x=0,y=c(ndat-12*c),legend = c('Penalize'),col = c('blue'),lty=1, lwd=2,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  #     ###MSE
  #     MLE= sum(gen(n)*log(ans$beta%*%t(apply(M, 1, diff))))-y[length(y)]
  #     print(MSE)
  #     ###MLE
  #     y0=t(ans$beta)%*%M0
  #     lam=y0
  #     MLE=sum((log(lam[2:length(lam)])))-max(y)
  #     print(MLE)
  #
  # }
}
saveRDS(results_list, file.path("analysis_result", "Tandem", "Tandem_results.rds"))
loaded_results <- readRDS(file.path("analysis_result", "Tandem", "Tandem_results.rds"))


# plot(t1,n1)
# data1=n1
#
# res=para_con1(data1,1,n=n1,theta=0.00016)
# lines(res$l,res$su,lty=2,lwd=2,col='blue')
# legend(x=c(0),y=c(ndat-2*c),legend = name[[5]],col = c('blue'),lty=2, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
# print(res$MLE)
# print(res$MSE)