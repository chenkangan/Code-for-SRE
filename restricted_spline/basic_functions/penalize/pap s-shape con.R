library('coneproj')
library('mvtnorm')
source('function.R')
spline_con_s=function(n,xdat,k,u){######u is change point ex, u=2000;
  ndat=length(xdat)
  knots=round((0:(k+1))*ndat/(k+1))
  knots[1]=1
  knots[k+2]=ndat
  t=xdat[knots]
  m=k+2
  M=matrix(1:(length(xdat)*(k+3)),ncol = length(xdat),nrow = k+3)
  for (j in 1:(k+3)){
    for(i in 1:length(xdat)){
      M[j,i]=base_int(xdat[i],j,xdat,k,length(xdat),knots,t)
    }
  }
  M0=matrix(1:(length(xdat)*(k+3)),ncol = length(xdat),nrow = k+3)
  for (j in 1:(k+3)){
    for(i in 1:length(xdat)){
      M0[j,i]=base_fun(xdat[i],j,xdat,k,length(xdat),knots,t)
    }
  }
  sigma0=matrix(1:(m*length(xdat)),nrow=m,ncol=length(xdat))
  j=min(which((u<=t)==1))
  if(u==1){j=2}
  if(j>=3){
    for(l in 1:(j-2)){
      sigma0[l,]=1-M0[l,]
    }
  }
  fj0=function(x){return(base_fun(x,j-1,xdat,k,length(xdat),knots,t))}
  fj=function(x){return(base_fun(x,j,xdat,k,length(xdat),knots,t))}
  f1=function(x){grad(fj,x)}
  f0=function(x){grad(fj0,x)}
  r=f0(u)/f1(u)
  
  
  sigma0[j-1,]=1-M0[j-1,]+r*M0[j,]
  ma=max(sigma0[j-1,])
  sigma0[j-1,]=sigma0[j-1,]/ma
  for(l in j:(k+2)){
    sigma0[l,]=M0[l+1,]
  }
  
  sigma1=matrix(1:(m*length(xdat)),nrow=m,ncol=length(xdat))
  
  j=min(which((u<=t)==1))
  if(u==1){j=2}
  if(j>=3){
    for(l in 1:(j-2)){
      sigma1[l,]=xdat-M[l,]
    }
  }
  sigma1[j-1,]=xdat-M[j-1,]+r*M[j,]
  sigma1[j-1,]=sigma1[j-1,]/ma
  for(l in j:(k+2)){
    sigma1[l,]=M[l+1,]
  }
  
  #####discon
  A=diag(m)
  A=rbind(A,1:m*0)
  for(i in 1:m){
    A[m,i] = sigma0[i,1]
    A[m+1,i] = sigma0[i,length(xdat)]
    if(sigma0[i,length(xdat)]<=0.00001){
      A[m+1,i]=0
    }
  }
  
  objfun <- function(x) {
    -(sum((log(t(x)%*%sigma0)))-c(t(x)%*%sigma1[,ncol(sigma1)]))
    # -lambda*(t(x)-t(beta0))%*%M2%*%t(M2)%*%(x-beta0))
  }
  grad_fun <- function(x) {
    -(sigma0 %*% t((1/(t(x) %*% sigma0))) - sigma1[,ncol(sigma1)])
    # -2*lambda*M2%*%t(M2)%*%(x-beta0))
  }
  grad_fun <- function(x) {
    grad <- grad(f = objfun, x = x)
    return(grad)
  }
  
  x0=rep(0.1,m)
  ci <- rep(1e-300, m+1)
  
  # 运行优化z
  result <- constrOptim(x0, objfun, ui=A, ci=ci, grad=grad_fun)
  beta=result$par
  ans=new.env()
  ans$beta=beta
  ans$r=r
  ans$M0=sigma0
  ans$M=sigma1
  ans$sigma=sigma
  ans$A=A
  return(ans)
}



pap_con_s=function(n,xdat,k,u,lambda){######u is change point ex, u=2000;
  ndat=length(xdat)
  knots=round((0:(k+1))*ndat/(k+1))
  knots[1]=1
  knots[k+2]=ndat
  t=xdat[knots]
  m=k+2
  M=matrix(1:(length(xdat)*(k+3)),ncol = length(xdat),nrow = k+3)
  for (j in 1:(k+3)){
    for(i in 1:length(xdat)){
      M[j,i]=base_int(xdat[i],j,xdat,k,length(xdat),knots,t)
    }
  }
  M0=matrix(1:(length(xdat)*(k+3)),ncol = length(xdat),nrow = k+3)
  for (j in 1:(k+3)){
    for(i in 1:length(xdat)){
      M0[j,i]=base_fun(xdat[i],j,xdat,k,length(xdat),knots,t)
    }
  }
  sigma0=matrix(1:(m*length(xdat)),nrow=m,ncol=length(xdat))
  j=min(which((u<=t)==1))
  if(u==1){j=2}
  if(j>=3){
    for(l in 1:(j-2)){
      sigma0[l,]=1-M0[l,]
    }
  }
  fj0=function(x){return(base_fun(x,j-1,xdat,k,length(xdat),knots,t))}
  fj=function(x){return(base_fun(x,j,xdat,k,length(xdat),knots,t))}
  f1=function(x){grad(fj,x)}
  f0=function(x){grad(fj0,x)}
  r=f0(u)/f1(u)
  
  
  sigma0[j-1,]=1-M0[j-1,]+r*M0[j,]
  ma=max(sigma0[j-1,])
  sigma0[j-1,]=sigma0[j-1,]/ma
  for(l in j:(k+2)){
    sigma0[l,]=M0[l+1,]
  }
  
  sigma1=matrix(1:(m*length(xdat)),nrow=m,ncol=length(xdat))
  
  j=min(which((u<=t)==1))
  if(u==1){j=2}
  if(j>=3){
    for(l in 1:(j-2)){
      sigma1[l,]=xdat-M[l,]
    }
  }
  sigma1[j-1,]=xdat-M[j-1,]+r*M[j,]
  sigma1[j-1,]=sigma1[j-1,]
  for(l in j:(k+2)){
    sigma1[l,]=M[l+1,]
  }
  
  A=diag(m)
  A=rbind(A,1:m*0)
  for(i in 1:m){
    A[m,i] = sigma0[i,1]
    A[m+1,i] = sigma0[i,length(xdat)]
    if(sigma0[i,length(xdat)]<=0.00001){
      A[m+1,i]=0
    }
  }
  
  objfun <- function(x) {
    -(sum((log(t(x)%*%sigma0)))-c(t(x)%*%sigma1[,ncol(sigma1)])
      -lambda*(t(x)-t(beta0))%*%M3%*%t(M3)%*%(x-beta0))
  }
  grad_fun <- function(x) {
    -(sigma0 %*% t((1/(t(x) %*% sigma0))) - sigma1[,ncol(sigma1)]
      -2*lambda*M3%*%t(M3)%*%(x-beta0))
  }
  grad_fun <- function(x) {
    grad <- grad(f = objfun, x = x)
    return(grad)
  }
  x0=rep(0.1,m)
  ci <- rep(1e-300, m+1)
  # 运行优化z
  result <- constrOptim(x0, objfun, ui=A, ci=ci, grad=grad_fun)
  beta=result$par
  ans=new.env()
  ans$beta=beta
  ans$r=r
  ans$M0=sigma0
  ans$M=sigma1
  ans$sigma=sigma
  ans$A=A
  return(ans)
}



post=function(beta){
  LLF=rowSums((log(beta%*%M0)))-beta%*%M[,ncol(M)]
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
  
  
  ans=new.env()
  ans$M=M 
  return(ans)
}

Mat1=function(xdat1,n1,k,v){
  ndat1=length(xdat1)
  knots1=round((0:(k+1))*ndat1/(k+1))
  knots1[1]=1
  knots1[k+2]=ndat1
  t1=xdat1[knots1]
  res1=spline_con_s(n1,xdat1,k,u0)
  r1=res1$r
  ans1=Mat(xdat1,k,v)
  M1=ans1$M
  l1=(1:(ceiling(max(xdat1)/v))-1)*(v)+1
  
  sigma1=matrix(1:(length(l1)*(k+2)),ncol=length(l1),nrow=k+2)
  
  
  j=min(which((u0<=t1)==1))
  if(j>=3){
    for(l in 1:(j-2)){
      sigma1[l,]=l1-M1[l,]
    }
  }
  sigma1[j-1,]=l1-M1[j-1,]+r1*M1[j,]
  for(l in j:(k+2)){
    sigma1[l,]=M1[l+1,]
  }
  ans=new.env()
  ans$l1=l1
  ans$M=sigma1
  return(ans)
}


res2=spline_con_s(1:length(xdat1)-1,xdat1,k,u0)

list0=list(c(120,0.00023),c(120,0.0006),c(120,0.0004,.001),c(120,0.0005,0.9996),c(120,2.5,6000),c(120,0.00032,1.02),c(120,3,0.0001),c(120,18,.000000007))
func_name=c('Goel_Okumoto','G_O','Hossain_Dahiya','Gompertz','Pareto','Weibull','Yamada_Exp','Yamada_Raleigh')



# for discon


for (p in c(2,4,8)) {
  print(p)
  k=4
  m=k+2
  MSE_p=1:500
  MSE_s=1:500
  MLE_s=1:500
  MLE_p=1:500
  data1=readRDS('conti.rds')[[p]]
  data2=readRDS('conti1.rds')[[p]]
  for (o in 1:500) {
    print(o)
    xdat=c(0,data2[[o]])
    n=1:length(xdat)
    xdat1=c(0,data1[[o]])
    n1=1:length(xdat1)      
    xdat2=xdat
    n2=n
    v=50
    q=7
    ndat1=length(xdat1)
    knots1=round((0:(k+1))*ndat1/(k+1))
    knots1[1]=1
    knots1[k+2]=ndat1
    t1=xdat1[knots1]
    ndat2=length(xdat2)
    knots2=round((0:(k+1))*ndat2/(k+1))
    knots2[1]=1
    knots2[k+2]=ndat2
    t2=xdat2[knots2]
    start=1000
    end=3000
    interval=100
    e=1:((end-start)/interval+1)
    for(u0 in seq(start,end,by=interval)){
      res2=spline_con_s(n2,xdat2,k,u0)
      M=res2$M
      y=t(res2$beta)%*%M
      c=max(n2)/max(y)
      ###MSE
      MSE=sqrt(sum((n2-y*c)^2))/(length(xdat2)-1)
      e[(u0-start)/interval+1]=MSE
      # print(MSE)
    }
    u0=(which.min(e)-1)*interval+start    
    ####calculating beta
    res1=spline_con_s(n1,xdat1,k,u0)
    res2=spline_con_s(n2,xdat2,k,u0)
    r1=res1$r
    r2=res2$r
    ans1=Mat(xdat1,k,v)
    ans2=Mat(xdat2,k,v)
    M1=ans1$M
    M2=ans2$M
    A=res2$A
    l1=(1:(ceiling(max(xdat1)/v))-1)*(v)+1
    l2=(1:(ceiling(max(xdat2)/v))-1)*(v)+1
    
    sigma1=matrix(1:(length(l1)*(k+2)),ncol=length(l1),nrow=k+2)
    sigma2=matrix(1:(length(l2)*(k+2)),ncol=length(l2),nrow=k+2)
    
    
    j=min(which((u0<=t1)==1))
    if(j>=3){
      for(l in 1:(j-2)){
        sigma1[l,]=l1-M1[l,]
      }
    }
    sigma1[j-1,]=l1-M1[j-1,]+r1*M1[j,]
    for(l in j:(k+2)){
      sigma1[l,]=M1[l+1,]
    }
    
    
    j=min(which((u0<=t2)==1))
    if(u0==1){j=2}
    if(j>=3){
      for(l in 1:(j-2)){
        sigma2[l,]=l2-M2[l,]
      }
    }
    sigma2[j-1,]=l2-M2[j-1,]+r2*M2[j,]
    for(l in j:(k+2)){
      sigma2[l,]=M2[l+1,]
    }
    
    
    
    ###calculating a and b fit the data best
    opt=function(theta){
      a=theta[1]
      b=theta[2]
      l=b*max(xdat1)
      l=min(l,max(xdat2))
      l=ceiling(l/v)####EXPM：xdat1=6003,xdata2=9182, l=601
      return(sum((a*res1$beta%*%((Mat1(b*xdat1,n1,k,v)$M)[,1:l])-res2$beta%*%(sigma2[,1:l]))^2)/l)
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
    res=Mat1(xdat2,n2,k,u)
    M2=res$M
    l1=res$l1
    M1=Mat1(b*xdat1,n1,k,u)$M
    M1=M1[,1:(l/u)]
    M2=M2[,1:(l/u)]
    l1=l1[1:(l/u)]
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
    x0=c(0.1,0.1,0.1,0.1,0.1,0.1)
    ci <- rep(0, m+1)
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
    M=res2$M
    M3=M
    M0=res2$M0
    l=seq(1,max(xdat),by=u)
    MM=res$M
    
    n3=20####number of segemnt
    start=0#####start value
    end=0.1####end..
    d=(1:n3)*0
    sig=0.1
    for (j in 1:n3) {
      w=start+(end-start)/n3*j###initial w
      x <- rmvnorm(n=500000, mean=beta0, sigma=(w^-2)*symmetrize_matrix(Sig))####generating beta
      for(i in 1:ncol(x)){
        if(i==1){u=(x[,1]>0)}
        if(i>1){u=u*(x[,i]>0)}
      }
      y=x[which(u==1),]######calculating posterior beta
      if(length(y)!=0){
        d[j]=sum(exp(post(y)+500))/length(y)
      }
      else{d[j]=1}
    }
    print('finish_post')
    lamb=start+(end-start)/n3*(which.max(d))
    if(sum(is.nan(d))==length(d)){
      lamb=0.001
      print('y is non')
    }
    1
    ans=pap_con_s(n,xdat,k,u0,lamb)
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
    MLE=sum((log(lam[2:length(lam)])))-max(y)
    print(MLE)
    MSE_p[o]=MSE0
    MLE_p[o]=MLE
    x0=dspl1(n,xdat,k)$beta
    ans1=spline_dis_s(n,xdat,k,u0)
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
    MLE=sum(log(lam[2:length(lam)]))-max(y)
    print(MLE)
    MSE_s[o]=MSE0
    MLE_s[o]=MLE
  }
  filename <- paste(p,'s','con','spl.csv', sep = "_")
  write.csv(cbind(MLE_s,MSE_s), file = filename, row.names = FALSE)
  filename <- paste(p,'s','con','pen.csv', sep = "_")
  write.csv(cbind(MLE_p,MSE_p), file = filename, row.names = FALSE)
}












