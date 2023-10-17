dspl=function(xdat,k,data_name=1,theta=1){
  ndat=length(xdat)
  knots=round((0:(k+1))*ndat/(k+1))
  knots[1]=1
  knots[k+2]=ndat
  t=xdat[knots]
  m=k+2
  M=matrix(1:length(xdat)*(k+3),ncol = length(xdat),nrow = k+3)
  for (j in 1:(k+3)){
    for(i in 1:length(xdat)){
      M[j,i]=base_int(xdat[i],j,xdat,k,ndat,knots,t)
    }
  }
  M0=matrix(1:length(xdat)*(k+3),ncol = length(xdat),nrow = k+3)
  for (j in 1:(k+3)){
    for(i in 1:length(xdat)){
      M0[j,i]=base_fun(xdat[i],j,xdat,k,ndat,knots,t)
    }
  }
  A=diag(m+1)
  objfun <- function(x) {
    -(sum(log(t(x)%*%M0))-t(x)%*%M[,ncol(M)])
  }
  grad_fun <- function(x) {
    -(M0 %*% t((1/t(x) %*% M0)) - M[,ncol(M)])
  }
  x0 <- rep(.01, m+1)
  ci <- rep(0, m+1)
  
  # 运行优化z
  result <- constrOptim(x0, objfun, ui=A, ci=ci, grad=grad_fun)
  beta=result$par
  ans=new.env()
  ans$beta = beta
  return(ans)
}

continuous=function(xdat,k,data_name=1,theta=1){
  if(identical(as.character(substitute(data_name)), "data_name")){ans=dspl(xdat,k,data_name=1,theta=1)}
  else{ans=dspl(xdat,k)}
  # ans=dspl(xdat,k)
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

  
  
  M0=matrix(1:length(xdat)*(k+3),ncol = length(xdat),nrow = k+3)
  for (j in 1:(k+3)){
    for(i in 1:length(xdat)){
      M0[j,i]=base_fun(xdat[i],j,xdat,k,ndat,knots,t)
    }
  }
  y0=ans$beta%*%M0
  y=ans$beta%*%M
  ynew=y*ndat/y[length(y)]
  y=ynew
  ####MSE
  MSE=sqrt(sum((1:length(xdat)-ndat*y/y[length(y)])^2))/(length(xdat)-1)
  
  ###MLE
  lam=y0*ndat/y[length(y)]
  MLE=sum(log(lam)[1:(length(lam)-1)])-ndat
  res=new.env()
  if(identical(as.character(substitute(data_name)), "data_name")){res$MSE0=MSE0}###no use
  res$l=xdat
  res$beta=ans$beta
  res$MLE=MLE
  res$MSE=MSE
  res$su=ynew
  return(res)
  
}



# res=continuous(xdat,k)
# res=continuous(xdat1,4)
