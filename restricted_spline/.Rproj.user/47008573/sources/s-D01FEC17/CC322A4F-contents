source('basic_functions/contiunous-new.R')
source('basic_functions/contiunous.R')
# source('distri_new(1).R')
source('basic_functions/nonpara.R')
source('basic_functions/parametric.R')
source('basic_functions/discon-new.R')
source('basic_functions/para-conti.R')

# result2=ifr(t3)

dspl2=function(xdat,k,lambda){
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
    -(sum(log(t(x)%*%M0))-t(x)%*%M[,ncol(M)]
      -lambda*(t(x)-t(beta0))%*%M%*%t(M)%*%(x-beta0))
  }
  grad_fun <- function(x) {
    -(M0 %*% t((1/t(x) %*% M0)) - M[,ncol(M)]
      -2*lambda*M%*%t(M)%*%(x-beta0))
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
###S-shape
G_O_gre=function(x,theta){
  theta[1]*theta[2]*(theta[2]*x)*exp(-theta[2]*x)
}

###S-shape
Y_R_gre=function(x,theta){
  theta[1]*theta[2]*theta[3]*x*exp(-theta[2]*(1-exp(-theta[3]/2*x^2))-theta[3]/2*x*2)
}

####concave
H_D_gre=function(x,theta){
  theta[1]*theta[2]*exp(-theta[2]*x)*(1+theta[3])/(1+theta[3]*exp(-theta[2]*x))^2
}
###generate poisson 
###the is maximum of f
poi=function(the,theta,t,f){
  # set.seed(as.numeric(Sys.time()))
  
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
######nonpara_s
nonincre2=function(x,t){
  n=length(t)
  i0 =  length(t)-sum((x<=t))-1
  if(i0==0){
    return(0)
  }
  m=1:(n-i0-1)*0
  J=1:(n-2)
  for(i in 1:(n-2)){
    J[i]=(n-i-1)*(t[i+2]-t[i+1])
  }
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
  if(i0==0){
    return(0)
  }
  m=1:(n-i0-1)*0+100
  J=1:(n-2)
  for(i in 1:(n-2)){
    J[i]=(n-i-1)*(t[i+2]-t[i+1])
  }
  for (j in (i0+1):(n-1)){
    for(i in 1:(i0)){
      if((j-i)/(sum(J[i:(j-1)]))<m[j-i0]){
        m[j-i0]=(j-i)/(sum(J[i:(j-1)]))
      }
    }
  }
  m0=max(m)
  return(m0)
}
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
  t0=replace(t0,t0>length(l),length(l))
  t0=replace(t0,t0==0,1)
  suu=sum0(t,2)
  su=suu$su
  y=suu$y
  lam=y[t0]*exp(-su[t0])*(length(t)-1)/(1-exp(-su[length(su)]))
  lam=lam[which(lam>0)]
  su2=(1-exp(-su))*(length(t)-1)/(1-exp(-su[length(su)]))
  if(t0[length(t0)]>max(l)){
    t0[length(t0)]=max(l)
  }
  
  ###MLE
  MLE=sum(log(lam))-su2[length(su2)]
  ###MSE
  MSE=sqrt(sum((0:(length(t)-1)-su2[t0])^2))/(length(t)-1)
  re = new.env()
  re$MLE=MLE
  re$MSE=MSE
  re$su = su2
  re$lam=lam
  re$t0=t0
  re$l=l
  return(re)
}
dfr=function(t){
  l = seq(1,max(t),by=1)
  t0 = round(t)
  t0=replace(t0,t0>length(l),length(l))
  t0=replace(t0,t0==0,1)
  suu=sum0(t,3)
  su=suu$su
  y=suu$y
  lam=y[t0]*exp(-su[t0])*(length(t)-1)/(1-exp(-su[length(su)]))
  lam=lam[which(lam>0)]
  su3=(1-exp(-su))*(length(t)-1)/(1-exp(-su[length(su)]))
  if(t0[length(t0)]>max(l)){
    t0[length(t0)]=max(l)
  }
  ###MLE
  MLE=sum(log(lam))-su3[length(su3)]
  ###MSE
  MSE=sqrt(sum((0:(length(t)-1)-su3[t0])^2))/(length(t)-1)
  re = new.env()
  re$MLE=MLE
  re$MSE=MSE
  re$su = su3
  re$lam=lam
  re$t0=t0
  re$l=l
  return(re)
}
nonpara_s=function(u,t){
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
  MSE=sqrt((result1$MSE*(length(t1)-1))^2+(result2$MSE*(length(t2)-1))^2)/(length(t)-1)
  MLE=result1$MLE+result2$MLE
  res=new.env()
  res$MLE=MLE
  res$MSE=MSE
  res$su=su
  res$l=l
  return(res)
}
#####s-shape
library('coneproj')

lanins= function(a,b,c,i,j){#####i is calculation interval j is a or b or c
  if (i==1){f = function(x) (x-b)*(x-c)/(a-b)/(a-c)} 
  if (i==2){f = function(x) (x-a)*(x-c)/(b-a)/(b-c)} 
  if (i==3){f = function(x) (x-a)*(x-b)/(c-a)/(c-b)} 
  if (j==1){return(integrate(f,a,b))}
  if (j==2){return(integrate(f,b,c))}
}
gen_A = function(x,t,knots,k){
  kn = t[knots]
  s = 1:length(knots)
  for(i in 1:length(knots)){
    s[i] = which.min(abs(x-kn[i]))
  }
  A = matrix(1:(length(x)*(length(x)-1))*0 ,ncol = length(x),nrow = length(x)-1)
  for(j in 1:(k+1)){
    for(i in (s[j]):(s[j+1]-2)){
      A[i,i]=lanins(x[i],x[i+1],x[i+2],1,1)$value
      A[i,i+1]=lanins(x[i],x[i+1],x[i+2],2,1)$value
      A[i,i+2]=lanins(x[i],x[i+1],x[i+2],3,1)$value
    }
    A[s[j+1]-1,s[j+1]-2]=lanins(x[s[j+1]-2],x[s[j+1]-1],x[s[j+1]],1,2)$value
    A[s[j+1]-1,s[j+1]-1]=lanins(x[s[j+1]-2],x[s[j+1]-1],x[s[j+1]],2,2)$value
    A[s[j+1]-1,s[j+1]]=lanins(x[s[j+1]-2],x[s[j+1]-1],x[s[j+1]],3,2)$value
  }
  return(A)
}
base_gredient=function(x,i,j,t){
  if(j==1){
    return((2*(x-t[2])/(t[2]-t[1])^2))
  }
  if(2<=j&j<=(k)){
    if(i==1){return(-2*(x-t[j-1]) / (t[j+1]-t[j-1]) / (t[j]-t[j-1]))}
    if(i==2){return(2*(x-t[j+1])/(t[j+1]-t[j])/(t[j+1]-t[j-1]))}
  }
  if(j==(k+1)){
    if(i==1){return(-2*(x-t[j-1]) / (t[j+1]-t[j-1]) / (t[j]-t[j-1]))}
    if(i==2){return(2*(x-t[j+1])/(t[j+1]-t[j])/(t[j+1]-t[j-1]))}
  }
  if(j==(k+2)){
    return(-2*(x-t[k+1])/(t[k+2]-t[k+1])^2)
  }
}
base_fun = function(x,j,t,k=4,ndat,knots,kn){
  for(l in 1:(ndat-1)){if(t[l]<=x&x<t[l+1]){i=l}}
  if(x>=t[ndat]){i=ndat}
  
  if(j==1){
    if(1<=i&i<=(knots[2]-1)){
      return((x-kn[2])^2/(kn[2]-kn[1])^2)
    }
    if(knots[2]<=i&i<=(knots[k+2])){
      return(0)
    }
  }
  if(2<=j&j<=(k)){
    if(1<=i&i<=(knots[j-1]-1)){ 
      return(1)
    }
    if(knots[j-1]<=i&i<=(knots[j]-1))
    {
      return(1 - (x-kn[j-1])^2 / (kn[j+1]-kn[j-1]) / (kn[j]-kn[j-1]))
    }
    if(knots[j]<=i&i<=(knots[j+1]-1))
    {
      return((x-kn[j+1])^2/(kn[j+1]-kn[j])/(kn[j+1]-kn[j-1]))
    }
    if(knots[j+1]<=i){
      return(0)
    }
  }
  if(j == k+1){
    if(1<=i&i<=(knots[k]-1)){
      return(1)
    }
    if(knots[k]<=i&i<=(knots[k+1]-1)){
      return(1 - (x-kn[k])^2 / (kn[k+2]-kn[k]) / (kn[k+1]-kn[k]))
    }
    if(knots[k+1]<=i&i<=(knots[k+2])){
      return((x-kn[k+2])^2/(kn[k+2]-kn[k+1])/(kn[k+2]-kn[k]))
    }
  }
  if(j==(k+2)){
    if(1<=i&i<=(knots[k+1]-1)){
      return(1)
    }
    if(knots[k+1]<=i&i<=(knots[k+2])){
      return(1-(x-kn[k+1])^2/(kn[k+2]-kn[k+1])^2)
    }
  }
  if(j ==(k+3)){
    return(1)
  }
}
base_int = function(x,j,t,k=4,ndat,knots,kn){
  for(l in 1:(ndat-1)){if(t[l]<=x&x<t[l+1]){i=l}
    if(x>=t[ndat]){i=ndat}
  }
  
  if(j==1){
    if(1<=i&i<=(knots[2]-1)){
      return(((x-kn[2])^3-(kn[1]-kn[2])^3)/(kn[2]-kn[1])^2/3)
    }
    if(knots[2]<=i&i<=(knots[k+2])){
      return(-(kn[1]-kn[2])^3/(kn[2]-kn[1])^2/3)
    }
  }
  
  
  if(2<=j&j<=(k)){
    if(1<=i&i<=(knots[j-1]-1)){ 
      return(x)
    }
    if(knots[j-1]<=i&i<=(knots[j]-1))
    {
      return(x - (x-kn[j-1])^3 / (kn[j+1]-kn[j-1]) / (kn[j]-kn[j-1])/3)
    }
    if(knots[j]<=i&i<=(knots[j+1]-1))
    {
      return(kn[j] - (kn[j]-kn[j-1])^3 / (kn[j+1]-kn[j-1]) / (kn[j]-kn[j-1])/3+((x-kn[j+1])^3-(kn[j]-kn[j+1])^3)/3/(kn[j+1]-kn[j])/(kn[j+1]-kn[j-1]))
    }
    if(knots[j+1]<=i){
      return(kn[j] - (kn[j]-kn[j-1])^3 / (kn[j+1]-kn[j-1]) / (kn[j]-kn[j-1])/3+(-(kn[j]-kn[j+1])^3)/3/(kn[j+1]-kn[j])/(kn[j+1]-kn[j-1]))
    }
  }
  if(j == k+1){
    if(1<=i&i<=(knots[k]-1)){
      return(x)
    }
    if(knots[k]<=i&i<=(knots[k+1]-1)){
      return(x - (x-kn[k])^3/3 / (kn[k+2]-kn[k]) / (kn[k+1]-kn[k]))
    }
    if(knots[k+1]<=i&i<=(knots[k+2])){
      return( kn[k+1] - (kn[k+1]-kn[k])^3/3 / (kn[k+2]-kn[k]) / (kn[k+1]-kn[k])+((x-kn[k+2])^3-(kn[k+1]-kn[k+2])^3)/3/(kn[k+2]-kn[k+1])/(kn[k+2]-kn[k]))
    }
    else{return(kn[k+1] - (kn[k+1]-kn[k])^3/3 / (kn[k+2]-kn[k]) / (kn[k+1]-kn[k])+(-(kn[k+1]-kn[k+2])^3)/3/(kn[k+2]-kn[k+1])/(kn[k+2]-kn[k]))}
  }
  if(j==(k+2)){
    if(1<=i&i<=(knots[k+1]-1)){
      return(x)
    }
    if(knots[k+1]<=i&i<=(knots[k+2])){
      return(x-(x-kn[k+1])^3/3/(kn[k+2]-kn[k+1])^2)
    }
    else(return(kn[k+2]-(kn[k+2]-kn[k+1])^3/3/(kn[k+2]-kn[k+1])^2))
  }
  if(j ==(k+3)){
    return(x)
  }
}
s_shape=function(xdat,u,k){
  k<<-k
  mx=max(xdat)
  ndat=length(xdat)
  ngr=1000
  grid=0:(ngr)*(mx*(1+1/(2*ngr)))/ngr
  xt=1:ngr
  wt=1:ngr
  for(i in 1:ngr){
    xt[i]=(grid[i+1]+grid[i])/2
    wt[i]=sum(xdat>grid[i]&xdat<=grid[i+1])
  }
  xt[1]=0
  knots=round((0:(k+1))*ndat/(k+1))
  knots[1]=1
  m=k+2
  kn=1:m
  obs=1:ngr
  for(i in 1:m){kn[i]=min(obs[abs(xt-xdat[knots[i]])==min(abs(xt-xdat[knots[i]]))])}
  kn[1]=1
  t=xt[kn]
  j=1
  x = seq(min(xdat),max(xdat),by=1)
  ans=dspl1(xdat,k,u)
  M=matrix(1:(length(xt)*(k+2)),ncol = length(xt),nrow = k+2)
  j=min(which((u<=kn)==1))
  if(j>=3){
    for(l in 1:(j-2)){
      for(i in 1:length(xt)){
        M[l,i]=xt[i]-base_int(xt[i],l,xt,k,ngr,kn,t)
      }
    }
  }
  r=base_gredient(xt[u],2,j-1,t)/base_gredient(xt[u],1,j,t)
  for (i in 1:length(xt)) {
    M[j-1,i]=(xt[i]-base_int(xt[i],j-1,xt,k,ngr,kn,t)+r*base_int(xt[i],j,xt,k,ngr,kn,t))/ans$ma
  }
  M[j-1,]=M[j-1,]
  for(l in j:(k+2)){
    for(i in 1:length(xt)){
      M[l,i]=base_int(xt[i],l+1,xt,k,ngr,kn,t)
    }
  }
  
  M0=matrix(1:(length(xt)*(k+2)),ncol = length(xt),nrow = k+2)
  j=min(which((u<=kn)==1))
  if(j>=3){
    for(l in 1:(j-2)){
      for(i in 1:length(xt)){
        M0[l,i]=1-base_fun(xt[i],l,xt,k,ngr,kn,t)
      }
    }
  }
  r=base_gredient(xt[u],2,j-1,t)/base_gredient(xt[u],1,j,t)
  for (i in 1:length(xt)) {
    M0[j-1,i]=(1-base_fun(xt[i],j-1,xt,k,ngr,kn,t)+r*base_fun(xt[i],j,xt,k,ngr,kn,t))/ans$ma
  }
  M0[j-1,]=M0[j-1,]
  for(l in j:(k+2)){
    for(i in 1:length(xt)){
      M0[l,i]=base_fun(xt[i],l+1,xt,k,ngr,kn,t)
    }
  }
  y=t(ans$beta)%*%M
  y0=t(ans$beta)%*%M0
  t0=1:length(xdat)
  for(i in 1:length(xdat)){
    t0[i]=which.min(abs(xdat[i]-xt))
  }
  MSE=sqrt(sum((1:length(xdat)-ndat*y[t0]/y[length(y)])^2))/(length(xdat)-1)
  
  ###MLE
  lam=y0*ndat/y[length(y)]
  # print(log(lam))
  MLE=sum((log(lam))[2:(length(lam)-1)])-ndat
  


  res$beta=ans$beta
  res=new.env()
  res$l=xt
  res$su=y*ndat/y[length(y)]
  res$MSE=MSE
  res$MLE=MLE
  return(res)
}
ss_dis=function(n,xdat,k,u,v,q){######u is change point ex, u=2000;
  dn=gen(n)
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
  A=diag(m-1)
  A=rbind(A,1:(m-1)*0)
  if(q==1){  
    sigma0=matrix(1:((m-1)*length(xdat)),nrow=m-1,ncol=length(xdat))
    sigma1=matrix(1:((m-1)*length(xdat)),nrow=m-1,ncol=length(xdat))
    
    j=min(which((u<=t)==1))
    if(j>=3){  ###before u
      for(l in 1:(j-2)){
        sigma0[l,]=1-M0[l,]
        sigma1[l,]=xdat-M[l,]
        A[m,l]=1-base_fun(v,l,xdat,k,length(xdat),knots,t)
      }
    }
    
    
    ###u r
    fj0=function(x){return(base_fun(x,j-1,xdat,k,length(xdat),knots,t))}
    fj=function(x){return(base_fun(x,j,xdat,k,length(xdat),knots,t))}
    f1=function(x){grad(fj,x)}
    f0=function(x){grad(fj0,x)}
    r=f0(u)/f1(u)
    
    ###inter u and v
    j1=min(which((v<=t)==1))
    if(j<=(j1-3)){
      for(l in j:(j1-3)){
        sigma0[l,]=M0[l+1,]
        sigma1[l,]=M[l+1,]
        A[m,l]=base_fun(v,l+1,xdat,k,length(xdat),knots,t)
        
      }
    }
    ###v r
    fj0=function(x){return(base_fun(x,j1-1,xdat,k,length(xdat),knots,t))}
    fj=function(x){return(base_fun(x,j1,xdat,k,length(xdat),knots,t))}
    f1=function(x){grad(fj,x)}
    f0=function(x){grad(fj0,x)}
    r1=f0(v)/f1(v)
    
    ###u&v
    if(j<=(j1-2)){
      sigma0[j-1,]=1-M0[j-1,]+r*M0[j,]
      ma=max(sigma0[j-1,])
      sigma0[j-1,]=sigma0[j-1,]/ma
      
      sigma1[j-1,]=xdat-M[j-1,]+r*M[j,]
      sigma1[j-1,]=sigma1[j-1,]/ma
      
      sigma0[j1-2,]=M0[j1-1,]+r1*(1-M0[j1,])
      ma1=max(sigma0[j1-2,])
      sigma0[j1-2,]=sigma0[j1-2,]/ma1
      
      sigma1[j1-2,]=M[j1-1,]+r1*(xdat-M[j1,])
      sigma1[j1-2,]=sigma1[j1-2,]/ma1
      
      A[m,j-1]=(1-base_fun(v,j-1,xdat,k,length(xdat),knots,t)
                +r*base_fun(v,j,xdat,k,length(xdat),knots,t))/ma
      A[m,j1-2]=(base_fun(v,j1-1,xdat,k,length(xdat),knots,t)
                 +r1*(1-base_fun(v,j1,xdat,k,length(xdat),knots,t)))/ma1
    }
    # plot(apply(as.matrix(1:9000), 1, function(v){(1-base_fun(v,j-1,xdat,k,length(xdat),knots,t)
    #           +r*base_fun(v,j,xdat,k,length(xdat),knots,t))/ma}))
    
    if(j==(j1-1)){
      sigma0[j-1,]=1-M0[j-1,]+r*M0[j,]+r*r1*(1-M0[j+1,])
      sigma1[j-1,]=xdat-M[j-1,]+r*M[j,]+r*r1*(xdat-M[j+1,])
      
      ma=max(sigma0[j-1,])
      sigma0[j-1,]=sigma0[j-1,]/ma
      sigma1[j-1,]=sigma1[j-1,]/ma
      A[m,j-1]= (1-base_fun(v,j-1,xdat,k,length(xdat),knots,t)
                 +r*base_fun(v,j,xdat,k,length(xdat),knots,t)+r*r1*(1-base_fun(v,j+1,xdat,k,length(xdat),knots,t)))/ma
    }
    
    if((j1-1)<=(k)){###after v                                                                                                                                                                                                
      for(l in (j1-1):(k)){
        sigma0[l,]=1-M0[l+2,]
        sigma1[l,]=xdat-M[l+2,]
        A[m,l]=1-base_fun(v,l+2,xdat,k,length(xdat),knots,t)
      }
    }    
    sigma0[k+1,]=M0[k+3,]
    sigma1[k+1,]=xdat
    A[m,k+1]=1
    # print(sigma0)
    for(i in 1:(m-1)){
      A[m-1,i] = sigma0[i,1]
      # }
    }
  }
  
  if(q==2){  
    sigma0=matrix(1:((m-1)*length(xdat)),nrow=m-1,ncol=length(xdat))
    sigma1=matrix(1:((m-1)*length(xdat)),nrow=m-1,ncol=length(xdat))
    
    j=min(which((u<=t)==1))
    if(j>=3){  ###before u
      for(l in 1:(j-2)){
        sigma0[l,]=M0[l,]
        sigma1[l,]=M[l,]
        A[m,l]=base_fun(u,l,xdat,k,length(xdat),knots,t)
      }
    }
    
    
    ###u r
    fj0=function(x){return(base_fun(x,j-1,xdat,k,length(xdat),knots,t))}
    fj=function(x){return(base_fun(x,j,xdat,k,length(xdat),knots,t))}
    f1=function(x){grad(fj,x)}
    f0=function(x){grad(fj0,x)}
    r=f0(u)/f1(u)
    
    ###inter u and v
    j1=min(which((v<=t)==1))
    if(j<=(j1-3)){
      for(l in j:(j1-3)){
        sigma0[l,]=1-M0[l+1,]
        sigma1[l,]=xdat-M[l+1,]
        A[m,l]=1-base_fun(u,l+1,xdat,k,length(xdat),knots,t)
      }
    }
    ###v r
    fj0=function(x){return(base_fun(x,j1-1,xdat,k,length(xdat),knots,t))}
    fj=function(x){return(base_fun(x,j1,xdat,k,length(xdat),knots,t))}
    f1=function(x){grad(fj,x)}
    f0=function(x){grad(fj0,x)}
    r1=f0(v)/f1(v)
    
    ###u&v
    if(j<=(j1-2)){
      sigma0[j-1,]=M0[j-1,]+r*(1-M0[j,])
      ma=max(sigma0[j-1,])
      sigma0[j-1,]=sigma0[j-1,]/ma
      
      sigma1[j-1,]=M[j-1,]+r*(xdat-M[j,])
      sigma1[j-1,]=sigma1[j-1,]/ma
      
      sigma0[j1-2,]=1-M0[j1-1,]+r1*(M0[j1,])
      ma1=max(sigma0[j1-2,])
      sigma0[j1-2,]=sigma0[j1-2,]/ma1
      
      sigma1[j1-2,]=xdat-M[j1-1,]+r1*M[j1,]
      sigma1[j1-2,]=sigma1[j1-2,]/ma1
      
      A[m,j-1]=(base_fun(u,j-1,xdat,k,length(xdat),knots,t)
                +r*(1-base_fun(u,j,xdat,k,length(xdat),knots,t)))/ma
      A[m,j1-2]=(1-base_fun(u,j1-1,xdat,k,length(xdat),knots,t)
                 +r1*(base_fun(u,j1,xdat,k,length(xdat),knots,t)))/ma1
    }
    
    if(j==(j1-1)){
      sigma0[j-1,]=M0[j-1,]+r*(1-M0[j,])+r*r1*(M0[j+1,])
      sigma1[j-1,]=M[j-1,]+(r*xdat-M[j,])+r*r1*(M[j+1,])
      
      ma=max(sigma0[j-1,])
      sigma0[j-1,]=sigma0[j-1,]/ma
      sigma1[j-1,]=sigma1[j-1,]/ma
      A[m,j-1]= (base_fun(u,j-1,xdat,k,length(xdat),knots,t)
                 +r*(1-base_fun(u,j,xdat,k,length(xdat),knots,t))+r*r1*(base_fun(u,j+1,xdat,k,length(xdat),knots,t)))/ma
    }
    
    if(j1<=(k+1)){###after v                                                                                                                                                                                                
      for(l in (j1-1):(k)){
        sigma0[l,]=M0[l+2,]
        sigma1[l,]=M[l+2,]
        A[m,l]=base_fun(u,l+2,xdat,k,length(xdat),knots,t)
      }
    }
    sigma0[k+1,]=M0[k+3,]
    sigma1[k+1,]=xdat
    A[m,k+1]=1
    for(i in 1:(m-1)){
      A[m-1,i] = sigma0[i,length(xdat)]
      if(sigma0[i,length(xdat)]<=0.00001){
        A[m-1,i]=0
      }
    }  
  }
  # print(A)
  #####discon
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
  
  # objfun <- function(x) {
  #   -(sum((log(t(x)%*%sigma0)))-c(t(x)%*%sigma1[,ncol(sigma1)]))
  #   # -lambda*(t(x)-t(beta0))%*%M2%*%t(M2)%*%(x-beta0))
  # }
  # grad_fun <- function(x) {
  #   -(sigma0 %*% t((1/(t(x) %*% sigma0))) - sigma1[,ncol(sigma1)])
  #   # -2*lambda*M2%*%t(M2)%*%(x-beta0))
  # }S
  # grad_fun <- function(x) {
  #   grad <- grad(f = objfun, x = x)
  #   return(grad)
  # }
  
  # x0 <- rep(.01, m+1)
  # x0=c(1.886911e-11 ,3.762363e-08 ,5.927867e-04 ,1.183419e-02 ,7.380856e-03 ,1.682365e-07)
  x0=rep(0.1,m-1)
  ci <- rep(1e-300, m)
  
  # 运行优化z
  result <- constrOptim(x0, objfun, ui=A, ci=ci, grad=grad_fun)
  # print(result$par%*%sigma1)
  result <- constrOptim(max(n)/(result$par%*%sigma1)[length(xdat)]*result$par, objfun, ui=A, ci=ci, grad=grad_fun)
  # print(result$par%*%sigma1)
  beta=result$par
  ans=new.env()
  ans$beta=beta
  ans$r=r
  ans$M0=sigma0
  ans$M=sigma1
  ans$sigma=sigma
  return(ans)
}
ss_con=function(xdat,k,u,v,q){######u is change point ex, u=2000;
  dn=gen(n)
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
  n0=round(max(xdat))
  # M00=matrix(1:(n0*(k+3)),ncol = n0,nrow = k+3)
  # # apply(as.matrix(1:max(xdat)), 1, function(v){(1-base_fun(v,j-1,xdat,k,length(xdat),knots,t)
  #                                               # +r*base_fun(v,j,xdat,k,length(xdat),knots,t)+r*r1*(1-base_fun(v,j+1,xdat,k,length(xdat),knots,t)))/ma})
  # for(i in 1:(k+3)){
  #  M00[i,]=  apply(as.matrix(1:n0), 1, function(v){(base_fun(v,i,xdat,k,length(xdat),knots,t))})
  # }
  # M01=matrix(1:(n0*(k+3)),ncol = n0,nrow = k+3)
  # # apply(as.matrix(1:max(xdat)), 1, function(v){(1-base_fun(v,j-1,xdat,k,length(xdat),knots,t)
  # # +r*base_fun(v,j,xdat,k,length(xdat),knots,t)+r*r1*(1-base_fun(v,j+1,xdat,k,length(xdat),knots,t)))/ma})
  # for(i in 1:(k+3)){
  #   M01[i,]=  apply(as.matrix(1:n0), 1, function(v){(base_int(v,i,xdat,k,length(xdat),knots,t))})
  # }
  # l0=1:n0
  
  
  A=diag(m-1)
  A=rbind(A,1:(m-1)*0)
  if(q==1){
    sigma0=matrix(1:((m-1)*length(xdat)),nrow=m-1,ncol=length(xdat))
    sigma1=matrix(1:((m-1)*length(xdat)),nrow=m-1,ncol=length(xdat))
    
    
    # sigma00=matrix(1:((m-1)*n0),nrow=m-1,ncol=n0)
    # sigma01=matrix(1:((m-1)*n0),nrow=m-1,ncol=n0)
    
    
    j=min(which((u<=t)==1))
    if(j>=3){  ###before u
      for(l in 1:(j-2)){
        sigma0[l,]=1-M0[l,]
        sigma1[l,]=xdat-M[l,]
        
        # sigma00[l,]=1-M00[l,]
        # sigma01[l,]=l0-M01[l,]
        
        
        A[m,l]=1-base_fun(v,l,xdat,k,length(xdat),knots,t)
      }
    }
    
    
    ###u r
    fj0=function(x){return(base_fun(x,j-1,xdat,k,length(xdat),knots,t))}
    fj=function(x){return(base_fun(x,j,xdat,k,length(xdat),knots,t))}
    f1=function(x){grad(fj,x)}
    f0=function(x){grad(fj0,x)}
    r=f0(u)/f1(u)
    
    ###inter u and v
    j1=min(which((v<=t)==1))
    if(j<=(j1-3)){
      for(l in j:(j1-3)){
        sigma0[l,]=M0[l+1,]
        sigma1[l,]=M[l+1,]
        
        # sigma00[l,]=M00[l+1,]
        # sigma01[l,]=M01[l+1,]
        
        
        A[m,l]=base_fun(v,l+1,xdat,k,length(xdat),knots,t)
        
      }
    }
    ###v r
    fj0=function(x){return(base_fun(x,j1-1,xdat,k,length(xdat),knots,t))}
    fj=function(x){return(base_fun(x,j1,xdat,k,length(xdat),knots,t))}
    f1=function(x){grad(fj,x)}
    f0=function(x){grad(fj0,x)}
    r1=f0(v)/f1(v)
    
    ###u&v
    if(j<=(j1-2)){
      sigma0[j-1,]=1-M0[j-1,]+r*M0[j,]
      ma=max(sigma0[j-1,])
      sigma0[j-1,]=sigma0[j-1,]/ma
      
      # sigma00[j-1,]=(1-M00[j-1,]+r*M00[j,])/ma
      
      sigma1[j-1,]=xdat-M[j-1,]+r*M[j,]
      sigma1[j-1,]=sigma1[j-1,]/ma
      
      # sigma01[j-1,]=(1:n0-M01[j-1,]+r*M01[j,])/ma
      
      sigma0[j1-2,]=M0[j1-1,]+r1*(1-M0[j1,])
      ma1=max(sigma0[j1-2,])
      sigma0[j1-2,]=sigma0[j1-2,]/ma1
      
      # sigma00[j1-2,]=(M00[j1-1,]+r1*(1-M00[j1,]))/ma
      
      sigma1[j1-2,]=M[j1-1,]+r1*(xdat-M[j1,])
      sigma1[j1-2,]=sigma1[j1-2,]/ma1
      
      # sigma01[j1-2,]=(M01[j1-1,]+r1*(1:n0-M01[j1,]))/ma1
      
      
      A[m,j-1]=(1-base_fun(v,j-1,xdat,k,length(xdat),knots,t)
                +r*base_fun(v,j,xdat,k,length(xdat),knots,t))/ma
      A[m,j1-2]=(base_fun(v,j1-1,xdat,k,length(xdat),knots,t)
                 +r1*(1-base_fun(v,j1,xdat,k,length(xdat),knots,t)))/ma1
    }
    # plot(apply(as.matrix(1:9000), 1, function(v){(1-base_fun(v,j-1,xdat,k,length(xdat),knots,t)
    #           +r*base_fun(v,j,xdat,k,length(xdat),knots,t))/ma}))
    
    if(j==(j1-1)){
      sigma0[j-1,]=1-M0[j-1,]+r*M0[j,]+r*r1*(1-M0[j+1,])
      sigma1[j-1,]=xdat-M[j-1,]+r*M[j,]+r*r1*(xdat-M[j+1,])
      
      ma=max(sigma0[j-1,])
      sigma0[j-1,]=sigma0[j-1,]/ma
      sigma1[j-1,]=sigma1[j-1,]/ma
      
      # sigma00[j-1,]=(1-M00[j-1,]+r*M00[j,]+r*r1*(1-M00[j+1,]))/ma
      # sigma01[j-1,]=(l0-M01[j-1,]+r*M01[j,]+r*r1*(l0-M01[j+1,]))/ma
      
      A[m,j-1]= (1-base_fun(v,j-1,xdat,k,length(xdat),knots,t)
                 +r*base_fun(v,j,xdat,k,length(xdat),knots,t)+r*r1*(1-base_fun(v,j+1,xdat,k,length(xdat),knots,t)))/ma
    }
    
    
    # S=matrix(ncol=1:max(xdat),nrow=k+1)
    # for(i in 1:(k+1)){
    #   S[i,]=apply(as.matrix(1:max(xdat),1,function(v){1}))
    # }
    
    
    
    
    if((j1-1)<=(k)){###after v
      for(l in (j1-1):(k)){
        sigma0[l,]=1-M0[l+2,]
        sigma1[l,]=xdat-M[l+2,]
        
        # sigma00[l,]=1-M00[l+2,]
        # sigma01[l,]=l0-M01[l+2,]
        
        
        A[m,l]=1-base_fun(v,l+2,xdat,k,length(xdat),knots,t)
      }
    }
    sigma0[k+1,]=M0[k+3,]
    sigma1[k+1,]=xdat
    
    # sigma00[k+1,]=M00[k+3,]
    # sigma01[k+1,]=l0
    
    A[m,k+1]=1
    # print(sigma0)
    for(i in 1:(m-1)){
      A[m-1,i] = sigma0[i,1]
      # }
    }
  }
  
  if(q==2){
    sigma0=matrix(1:((m-1)*length(xdat)),nrow=m-1,ncol=length(xdat))
    sigma1=matrix(1:((m-1)*length(xdat)),nrow=m-1,ncol=length(xdat))
    
    # sigma00=matrix(1:((m-1)*n0),nrow=m-1,ncol=n0)
    # sigma01=matrix(1:((m-1)*n0),nrow=m-1,ncol=n0)
    
    j=min(which((u<=t)==1))
    if(j>=3){  ###before u
      for(l in 1:(j-2)){
        sigma0[l,]=M0[l,]
        sigma1[l,]=M[l,]
        
        # sigma00[l,]=M00[l,]
        # sigma01[l,]=M01[l,]
        #
        A[m,l]=base_fun(u,l,xdat,k,length(xdat),knots,t)
      }
    }
    
    
    ###u r
    fj0=function(x){return(base_fun(x,j-1,xdat,k,length(xdat),knots,t))}
    fj=function(x){return(base_fun(x,j,xdat,k,length(xdat),knots,t))}
    f1=function(x){grad(fj,x)}
    f0=function(x){grad(fj0,x)}
    r=f0(u)/f1(u)
    
    ###inter u and v
    j1=min(which((v<=t)==1))
    if(j<=(j1-3)){
      for(l in j:(j1-3)){
        sigma0[l,]=1-M0[l+1,]
        sigma1[l,]=xdat-M[l+1,]
        
        # sigma00[l,]=1-M00[l+1,]
        # sigma01[l,]=n0--M01[l+1,]
        
        A[m,l]=1-base_fun(u,l+1,xdat,k,length(xdat),knots,t)
      }
    }
    ###v r
    fj0=function(x){return(base_fun(x,j1-1,xdat,k,length(xdat),knots,t))}
    fj=function(x){return(base_fun(x,j1,xdat,k,length(xdat),knots,t))}
    f1=function(x){grad(fj,x)}
    f0=function(x){grad(fj0,x)}
    r1=f0(v)/f1(v)
    
    ###u&v
    if(j<=(j1-2)){
      sigma0[j-1,]=M0[j-1,]+r*(1-M0[j,])
      ma=max(sigma0[j-1,])
      sigma0[j-1,]=sigma0[j-1,]/ma
      
      # sigma00[j-1,]=(M00[j-1,]+r*(1-M00[j,]))/ma
      
      
      sigma1[j-1,]=M[j-1,]+r*(xdat-M[j,])
      sigma1[j-1,]=sigma1[j-1,]/ma
      
      # sigma01[j-1,]=(M01[j-1,]+r*(l0-M01[j,]))/ma
      
      sigma0[j1-2,]=1-M0[j1-1,]+r1*(M0[j1,])
      
      # sigma00[j1-2,]=(1-M00[j1-1,]+r1*(M00[j1,]))/ma
      
      
      ma1=max(sigma0[j1-2,])
      sigma0[j1-2,]=sigma0[j1-2,]/ma1
      
      sigma1[j1-2,]=xdat-M[j1-1,]+r1*M[j1,]
      sigma1[j1-2,]=sigma1[j1-2,]/ma1
      
      # sigma01[j1-2,]=(l0-M01[j1-1,]+r1*(M01[j1,]))/ma1
      
      
      A[m,j-1]=(base_fun(u,j-1,xdat,k,length(xdat),knots,t)
                +r*(1-base_fun(u,j,xdat,k,length(xdat),knots,t)))/ma
      A[m,j1-2]=(1-base_fun(u,j1-1,xdat,k,length(xdat),knots,t)
                 +r1*(base_fun(u,j1,xdat,k,length(xdat),knots,t)))/ma1
    }
    
    
    
    
    
    if(j==(j1-1)){
      sigma0[j-1,]=M0[j-1,]+r*(1-M0[j,])+r*r1*(M0[j+1,])
      sigma1[j-1,]=M[j-1,]+r*(xdat-M[j,])+r*r1*(M[j+1,])
      
      ma=max(sigma0[j-1,])
      sigma0[j-1,]=sigma0[j-1,]/ma
      sigma1[j-1,]=sigma1[j-1,]/ma
      
      # sigma00[j-1,]=(M00[j-1,]+r*(1-M00[j,])+r*r1*(M00[j+1,]))/ma
      # sigma01[j-1,]=(M01[j-1,]+r*(l0-M01[j,])+r*r1*(M01[j+1,]))/ma
      
      A[m,j-1]= (base_fun(u,j-1,xdat,k,length(xdat),knots,t)
                 +r*(1-base_fun(u,j,xdat,k,length(xdat),knots,t))+r*r1*(base_fun(u,j+1,xdat,k,length(xdat),knots,t)))/ma
    }
    
    if(j1<=(k+1)){###after v
      for(l in (j1-1):(k)){
        sigma0[l,]=M0[l+2,]
        sigma1[l,]=M[l+2,]
        #
        # sigma00[l,]=M00[l+2,]
        # sigma01[l,]=M01[l+2,]
        
        A[m,l]=base_fun(u,l+2,xdat,k,length(xdat),knots,t)
      }
    }
    sigma0[k+1,]=M0[k+3,]
    sigma1[k+1,]=xdat
    
    # sigma00[k+1,]=M00[k+3,]
    # sigma01[k+1,]=l0
    
    
    A[m,k+1]=1
    for(i in 1:(m-1)){
      A[m-1,i] = sigma0[i,length(xdat)]
      if(sigma0[i,length(xdat)]<=0.00001){
        A[m-1,i]=0
      }
    }
  }
  # print(A)
  #####discon
  
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
  
  # objfun=function(x){
  #   sum((t(x)%*%sigma1-0:(length(xdat)-1))^2)
  # }
  # grad_fun=function(x){
  #   grad <- grad(f = objfun, x = x)
  #   return(grad)
  # }
  # 
  
  
  
  # objfun <- function(x) {
  #   -(sum((log(t(x)%*%sigma0)))-c(t(x)%*%sigma1[,ncol(sigma1)]))
  #   # -lambda*(t(x)-t(beta0))%*%M2%*%t(M2)%*%(x-beta0))
  # }
  # grad_fun <- function(x) {
  #   -(sigma0 %*% t((1/(t(x) %*% sigma0))) - sigma1[,ncol(sigma1)])
  #   # -2*lambda*M2%*%t(M2)%*%(x-beta0))
  # }S
  # grad_fun <- function(x) {
  #   grad <- grad(f = objfun, x = x)
  #   return(grad)
  # }
  
  # x0 <- rep(.01, m+1)
  # x0=c(1.886911e-11 ,3.762363e-08 ,5.927867e-04 ,1.183419e-02 ,7.380856e-03 ,1.682365e-07)
  x0=rep(0.1,k+1)
  ci <- rep(1e-300, m)
  
  # 运行优化z
  result <- constrOptim(x0, objfun, ui=A, ci=ci, grad=grad_fun)
  # print(result$par%*%sigma1)
  result <- constrOptim(max(n)/(result$par%*%sigma1)[length(xdat)]*result$par, objfun, ui=A, ci=ci, grad=grad_fun)
  # print(result$par%*%sigma1)
  beta=result$par
  ans=new.env()
  ans$beta=beta
  # ans$M00=M00
  # ans$M01=M01
  ans$M0=sigma0
  ans$M=sigma1
  ans$sigma=sigma
  return(ans)
}
# res=ss_con(seq(1,max(data1),by=10),k,u,v,2)

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

# res2=spline_dis_s(n0,data1,k,u0)