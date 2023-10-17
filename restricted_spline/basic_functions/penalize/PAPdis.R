library('mvtnorm')
library('coneproj')
source('function.R')

post=function(beta){
  
  LLF=rowSums(dn*(log(beta%*%sigma)))-beta%*%M[,ncol(M)]
  # LLF=rowSums(log(beta%*%M0))-beta%*%M[,ndat]
  if(w<1e-3 | w>1e10){LLF=0}
  LLF=LLF-1/2*log(2*pi)-log(sig)/2-(w)^2/2/sig
  return(LLF)
}

####get the information of prior data

symmetrize_matrix <- function(mat) {
  n <- nrow(mat)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        mat[i, j] <- (mat[i, j] + mat[j, i]) / 2  # 替换为均值
        mat[j, i] <- mat[i, j]  # 保持对称性
      }
    }
  }
  return(mat)
}



Mat=function(xdat,k,u){####sampling per u unit
  ndat=length(xdat)
  knots=round((0:(k+1))*ndat/(k+1))
  knots[1]=1
  knots[k+2]=ndat
  t=xdat[knots]
  m=k+2
  
  

  M=matrix(1:(ceiling(max(xdat)/u)*(k+3)),ncol = ceiling(max(xdat)/u),nrow = k+3)
  for (j in 1:(k+3)){
    for(i in 1:ceiling(max(xdat)/u)){
      M[j,i]=base_int(u*(i-1)+1,j,xdat,k,ndat,knots,t)
    }
  }
  ans=  new.env()
  ans$M=M 
  return(ans)
}


dspl3=function(n,t,k,lambda,  
               x0=c(1.886911e-11 ,3.762363e-08 ,5.927867e-04 ,1.183419e-02 ,7.380856e-03 ,1.682365e-07,4.764025e-03)
){
  ndat=length(n)
  knots=round((0:(k+1))*ndat/(k+1))
  knots[1]=1
  m=k+2
  kn=1:m
  for(i in 1:m){kn[i]= t[knots[i]]}
  dn=gen(n)
  M=matrix(1:(length(t)*(k+3)),ncol = length(t),nrow = k+3)
  for (j in 1:(k+3)){
    for(i in 1:length(t)){
      M[j,i]=base_int(t[i],j,t,k,length(t),knots,kn)
    }
  }
  M0=matrix(1:(length(t)*(k+3)),ncol = length(t),nrow = k+3)
  for (j in 1:(k+3)){
    for(i in 1:length(t)){
      M0[j,i]=base_fun(t[i],j,t,k,length(t),knots,kn)
    }
  }
  # l=ceiling(max(t)/u)
  
  A=diag(m+1)
  sigma=t(apply(M, 1, diff))
  objfun <- function(x) {
    -(sum(dn*(log(t(x)%*%sigma)))-c(t(x)%*%M[,ncol(M)])
      -lambda*(t(x)-t(beta0))%*%M20%*%t(M20)%*%(x-beta0))
  }
  grad_fun <- function(x) {
    -(sigma %*% t(dn*(1/(t(x) %*% sigma))) - M[,ncol(M)]
    -2*lambda*M20%*%t(M20)%*%(x-beta0))
  }
  grad_fun <- function(x) {
    grad <- grad(f = objfun, x = x)
    return(grad)
  }
  
  # x0 <- rep(.01, m+1)
  # x0=c(1.886911e-11 ,3.762363e-08 ,5.927867e-04 ,1.183419e-02 ,7.380856e-03 ,1.682365e-07,4.764025e-03)
  ci <- rep(0, m+1)
  
  # 运行优化z
  result <- constrOptim(x0, objfun, ui=A, ci=ci, grad=grad_fun)
  beta=result$par
  ans=new.env()
  ans$beta=beta
  ans$xpl=t
  return(ans)
}







list0=list(c(120,0.00023),c(120,0.0006),c(120,0.0004,.001),c(120,0.0005,0.9996),c(120,2.5,6000),c(120,0.00032,1.02),c(120,3,0.0001),c(120,18,.000000007))
func_name=c('Goel_Okumoto','G_O','Hossain_Dahiya','Gompertz','Pareto','Weibull','Yamada_Exp','Yamada_Raleigh')
for (p in 1:3) {
  print(p)
  k=4
  m=k+2
  MSE_p=1:500
  MSE_s=1:500
  MLE_s=1:500
  MLE_p=1:500
  data1=readRDS('discon.rds')[[p]]
  data2=readRDS('discon1.rds')[[p]]
  for (o in 1:500) {
    # o=5
    print(o)
    xdat=c(0,data2[[o]][[1]])
    n=c(0,data2[[o]][[2]])
    xdat1=c(0,data1[[o]][[1]])
    n1=c(0,data1[[o]][[2]])
    xdat2=xdat
    n2=n
    ndat=length(xdat)
    v=50
    q=7
    ####calculating beta
    res1=dspl1(n1,xdat1,k)
    res2=dspl1(n2,xdat2,k)
    ans1=Mat(xdat1,k,v)
    ans2=Mat(xdat2,k,v)
    M1=ans1$M
    M2=ans2$M


    ###calculating a and b fit the data best
    opt=function(theta){
      a=theta[1]
      b=theta[2]
      l=b*max(xdat1)
      l=min(l,max(xdat2))
      l=ceiling(l/v)####EXPM：xdat1=6003,xdata2=9182, l=601
      return(sum((a*res1$beta%*%((Mat(b*xdat1,k,v)$M)[,1:l])-res2$beta%*%(M2[,1:l]))^2)/l)
    }
    # result <- ga(type = "real-valued", fitness = opt, lower = c(0.5, 0.5), upper = c(1.5, 1.5))
    # print(opt(1,1))
    N=matrix(1:(q*q),ncol=q,nrow=q)
    for(i in 1:q){
      for(j in 1:q){
        s=opt(c(i*0.05+0.8,j*0.05+0.8))
        N[i,j]=s
      }
    }
    
    print('finish_ab')


    ###get a and b
    a=which(N==min(N),arr.ind=TRUE)[1]*0.05+0.8
    b=which(N==min(N),arr.ind=TRUE)[2]*0.05+0.8
    # a=1
    # b=1
    u= 10
    l=b*max(xdat1)
    l=floor(min(l,max(xdat2)))
    M20=Mat(xdat2,k,u)$M
    M1=Mat(b*xdat1,k,u)$M
    M1=M1[,1:(l/u)]
    M2=M20[,1:(l/u)]
    Sig=pinv(M2%*%t(M2))
    # print(Sig)
    objfun <- function(x) {
      sum((x%*%M2-res1$beta%*%M1)^2)
    }
    grad_fun <- function(x) {
      grad <- grad(f = objfun, x = x)
      return(grad)
    }
    # x0=c(1.886911e-11 ,3.762363e-08 ,5.927867e-04 ,1.183419e-02 ,7.380856e-03 ,1.682365e-07,4.764025e-03)
    x0=c(0.1,0.1,0.1,0.1,0.1,0.1,0.1)
    ci <- rep(0, m+1)
    A=diag(m+1)
    result <- constrOptim(x0, objfun, ui=A, ci=ci, grad=grad_fun)
    beta0=result$par
    # beta0=a*solve(M2%*%t(M2))%*%M2%*%t(M1)%*%res1$beta
    xdat=xdat2
    ndat=length(xdat)
    knots=round((0:(k+1))*ndat/(k+1))
    knots[1]=1
    knots[k+2]=ndat
    t=xdat[knots]
    m=k+2
    M=matrix(1:(length(xdat)*(k+3)),ncol = length(xdat),nrow = k+3)
    for (j in 1:(k+3)){
      for(i in 1:length(xdat)){
        M[j,i]=base_int(xdat[i],j,xdat,k,ndat,knots,t)
      }
    }
    M0=matrix(1:(length(xdat)*(k+3)),ncol = length(xdat),nrow = k+3)
    for (j in 1:(k+3)){
      for(i in 1:length(xdat)){
        M0[j,i]=base_fun(xdat[i],j,xdat,k,ndat,knots,t)
      }
    }
    l=seq(1,max(xdat),by=u)
    MM=matrix(1:(length(l)*(k+3)),ncol = length(l),nrow = k+3)
    for (j in 1:(k+3)){
      for(i in 1:length(l)){
        MM[j,i]=base_int(l[i],j,xdat,k,ndat,knots,t)
      }
    }

    # 使用is_symmetric函数判断矩阵是否对称

 
    dn=gen(n)
    sigma=t(apply(M, 1, diff))

    n1=20####number of segemnt
    start=0#####start value
    end=0.001####end..
    d=(1:n1)*0
    sig=0.1
    for (j in 1:n1) {
      w=start+(end-start)/n1*j###initial w
      x <- rmvnorm(n=500000, mean=beta0, sigma=(w^-2)*symmetrize_matrix(Sig))####generating beta
      for(i in 1:ncol(x)){
        if(i==1){u=(x[,1]>0)}
        if(i>1){u=u*(x[,i]>0)}
      }
      y=x[which(u==1),]######calculating posterior beta
      d[j]=sum(exp(post(y)+516))/length(y)
    }
    print('finish_post')
    lamb=start+(end-start)/n1*(which.max(d))
    if(sum(is.nan(d))==length(d)){
      lamb=0.0001
      print('y is non')
    }
    q=10
    # x0=dspl3(n,xdat,k,lamb,q)$beta
    # for(i in 1:5){
    #   ans1=dspl3(n,xdat,k,lamb,q,x0)
    # }
    # 
    

    
    ans=dspl3(n,xdat,k,lamb)
    y=t(ans$beta)%*%M
    ym=t(ans$beta)%*%MM
    plot(xdat,n)
    lines(xdat,y,type='l',lwd=2,col='blue')
    legend(x=c(8000),y=c(10),legend = c('Penalize'),col = c('blue'),lty=1, lwd=2,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
    ###MSE
    MSE=sqrt(sum((n-y)^2))/(length(xdat)-1)
    print(MSE)
    MSE0=sqrt(sum((eval(parse(text = func_name[p]))(l,list0[[p]])-ym)^2))/(length(l)-1)
    print(MSE0)
    ###MLE
    y0=t(ans$beta)%*%M0
    # t0=1:length(xdat)
    # for(i in 1:length(xdat)){t0[i]=which.min(abs(xdat[i]-xt))}
    lam=y0
    # MLE=sum((log(lam[2:length(lam)])))-max(y)
    MLE= sum(gen(n)*log(ans$beta%*%t(apply(M, 1, diff))))-y[length(y)]
    
    print(MLE)
    MSE_p[o]=MSE0
    MLE_p[o]=MLE
    
    
    x0=dspl1(n,xdat,k)$beta
    for(i in 1:5){
      ans1=dspl1(n,xdat,k,x0)
    }
    ans1=dspl1(n,xdat,k)
    
    y=t(ans1$beta)%*%M
    c=max(n)/max(y)
    ym=t(ans1$beta)%*%MM
    # plot(xdat,1:ndat)
    lines(xdat,y*c,type='l',lwd=2,col='red')
    legend(x=c(8000),y=c(15),legend = c('Spline'),col = c('red'),lty=1, lwd=2,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
    ###MSE
    MSE=sqrt(sum((n-y*c)^2))/(length(xdat)-1)
    print(MSE)
    MSE0=sqrt(sum((eval(parse(text = func_name[p]))(l,list0[[p]])-ym*c)^2))/(length(l)-1)
    print(MSE0)
    ###MLE
    y0=t(ans$beta)%*%M0
    lam=y0
    # MLE=sum(log(lam[2:length(lam)]))-max(y)
    MLE= sum(gen(n)*log(ans$beta%*%t(apply(M, 1, diff))*c))-c*y[length(y)]
    
    print(MLE)
    MSE_s[o]=MSE0
    MLE_s[o]=MLE
    # print(o)
  }
  filename <- paste(p,'dis','spl.csv', sep = "_")
  write.csv(cbind(MLE_s,MSE_s), file = filename, row.names = FALSE)
  filename <- paste(p,'dis','pen.csv', sep = "_")
  write.csv(cbind(MLE_p,MSE_p), file = filename, row.names = FALSE)
}



