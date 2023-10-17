library('coneproj')
#Creates a weight vector to group observations.  Have ngr=1000 groups by default.
source('function.R')
spline_dis_s=function(n,xdat,k,u){######u is change point ex, u=2000;
  ndat=length(xdat)
  knots=round((0:(k+1))*ndat/(k+1))
  knots[1]=1
  knots[k+2]=ndat
  t=xdat[knots]
  m=k+2
  dn=gen(n)
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
  
  sigma=t(apply(sigma1, 1, diff))
  objfun <- function(x) {
    -(sum(dn*(log(t(x)%*%sigma)))-c(t(x)%*%sigma1[,ncol(sigma1)]))
      # -lambda*(t(x)-t(beta0))%*%M2%*%t(M2)%*%(x-beta0))
  }
  grad_fun <- function(x) {
    -(sigma %*% t(dn*(1/(t(x) %*% sigma))) - sigma1[,ncol(sigma1)])
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
  result <- constrOptim(result$par*max(n)/max(result$par%*%sigma1), objfun, ui=A, ci=ci, grad=grad_fun)
  beta=result$par
  ans=new.env()
  ans$beta=beta
  ans$r=r
  ans$M0=sigma0
  ans$M=sigma1
  ans$sigma=sigma
  return(ans)
}


  
# for discon
# 
datac=readRDS('discon.rds')
xdat=datac[[8]][[10]][[1]]
n=datac[[8]][[10]][[2]]
# n=1:length(xdat)
xdat=c(0,xdat)
n=c(0,n)
k=4
ndat=length(xdat)
knots=round((0:(k+1))*ndat/(k+1))
knots[1]=1
knots[k+2]=ndat
t=xdat[knots]
m=k+2

for(u in seq(1000,5000,by=50)){
  print(u)
  if(u %in% t ){
    next
  }
  ans=spline_dis_s(n,xdat,k,u)
  beta=ans$beta
  r=ans$r
  y=t(beta)%*%ans$M
  c=max(n)/max(y)
  M=ans$M
  M0=ans$M0
  y=t(beta)%*%M*c
  plot(xdat,n)
  lines(xdat,y)
  y0=t(beta)%*%M0*c
  MSE=sqrt(sum((n-y)^2))/(length(xdat)-1)
  print(MSE)
  
  ###MLE
  MLE=sum((log(y0))[2:length(y0)])-max(y)
  print(MLE)
}




