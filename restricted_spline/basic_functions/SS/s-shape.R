library('coneproj')
#Creates a weight vector to group observations.  Have ngr=1000 groups by default.

source('function.R')

dspl_s=function(xdat,k,u){
  ndat=length(xdat)
  ngr=1000
  mx=max(xdat)
  grid=0:(ngr)*(mx*(1+1/(2*ngr)))/ngr
  sm=0.00000001
  nrep=0
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
  delta=matrix(1:((m+1)*ngr),nrow=m+1,ncol=ngr)
  delta[m+1,]=1:ngr*0+1
  t=xt[kn]
  
  
  for(i in 1:kn[2]){
    delta[1,i]=(xt[i]-t[2])^2/(t[1]-t[2])^2
  }
  for(i in (kn[2]+1):kn[k+2]){
    delta[1,i]=0
  }
  for(j in 1:(k-1)){
    for(i in 1:kn[j]){
      delta[j+1,i] = 1
    }
    for(i in (kn[j]+1):kn[j+1]){
      delta[j+1,i] = 1 - (xt[i]-t[j])^2 / (t[j+2]-t[j]) / (t[j+1]-t[j])
    }
    for(i in (kn[j+1]+1):kn[j+2]){
      delta[j+1,i] = (xt[i]-t[j+2])^2/(t[j+2]-t[j+1])/(t[j+2]-t[j])
    }
    for(i in (kn[j+2]+1):ngr){
      delta[j+1,i]=0
    }
  }
  
  for(i in 1:kn[k]){
    delta[k+1,i] = 1
  }
  for(i in (kn[k]+1):kn[k+1]){
    delta[k+1,i] = (1 - (xt[i]-t[k])^2 / (t[k+2]-t[k]) / (t[k+1]-t[k]))
  }
  for(i in (kn[k+1]+1):kn[k+2]){
    delta[k+1,i] = ((xt[i]-t[k+2])^2/(t[k+2]-t[k+1])/(t[k+2]-t[k]))
  }
  
  
  for(i in 1:kn[k+1]){
    delta[k+2,i]=1
  }
  for(i in (kn[k+1]+1):kn[k+2]){
    delta[k+2,i]=(1-(xt[i]-t[k+1])^2/(t[k+2]-t[k+1])^2)
  }
  
  sigma=matrix(1:(m*ngr),nrow=m,ncol=ngr)


  j=min(which((u<=kn)==1))
  j1=min(which((xt[u]<=kn)==1))
  print(j)
  print(j1)
  if(u==1){j=2}
  if(j>=3){
    for(l in 1:(j-2)){
      sigma[l,]=1-delta[l,] 
    }
  }
  r=base_gredient(xt[u],2,j-1,t)/base_gredient(xt[u],1,j,t)
  sigma[j-1,]=1-delta[j-1,]+r*delta[j,]
  # sigma[j-1,]=sigma[j-1,]/max(sigma[j-1,])
  
  ma=max(sigma[j-1,])
  sigma[j-1,]=sigma[j-1,]/max(sigma[j-1,])
  for(l in j:(k+2)){
    sigma[l,]=delta[l+1,]
  }

  
  # make c-vector of weights
  
  c=1:ngr*0
    
  for(j in 1:(k+1)){
    for(i in (kn[j]+1):(kn[j+1]-1)){
      c[i-1]=c[i-1]+(xt[i]-xt[i-1])*(3*xt[i+1]-xt[i]-2*xt[i-1])/6/(xt[i+1]-xt[i-1])
      c[i]=c[i]+(xt[i]-xt[i-1])*(3*xt[i+1]-2*xt[i]-xt[i-1])/6/(xt[i+1]-xt[i])
      c[i+1]=c[i+1]-(xt[i]-xt[i-1])^3/(xt[i+1]-xt[i])/(xt[i+1]-xt[i-1])/6
    }
    i=kn[j+1]-1
    c[i-1]=c[i-1]-(xt[i+1]-xt[i])^3/(xt[i+1]-xt[i-1])/(xt[i]-xt[i-1])/6
    c[i]=c[i]+(xt[i+1]-xt[i])*(xt[i+1]+2*xt[i]-3*xt[i-1])/6/(xt[i]-xt[i-1])
    c[i+1]=c[i+1]+(xt[i+1]-xt[i])*(2*xt[i+1]+xt[i]-3*xt[i-1])/6/(xt[i+1]-xt[i-1])
  }

  
  
  c=c*ndat
  y=wt/c
  nc=ngr
  
  A=diag(m)
  A=rbind(A,1:m*0)
  for(i in 1:m){
    A[m,i] = sigma[i,1]
    A[m+1,i] = sigma[i,nc]
  }
  sig=solve(crossprod(t(sigma)))%*%sigma
  
  
  
  
  ans3=coneA(y,A%*%sig)
  yhat0 = t(sigma)%*%sig%*%ans3$thetahat
  sigmas=sigma
  stop=0
  theta=yhat0
  small=0.0001
    
  # loop!
    
  while(stop==0&nrep<50){
    nrep=nrep+1
    stop=1
    theta[theta<=0]=small
    if( abs(sum(wt-c*theta))/ngr>small){
      print(nrep)
      print(abs(sum(wt-c*theta))/ngr)
      stop=0
    }
    for(j in 1:(m)){
      if( sum((wt/theta-c)*sigma[j,])/ngr > small ){
    stop=0
    print(j)
    }
    }
    # w=c/theta
    # ys=y*sqrt(w)
    ys=y
    # for(i in 1:(m)){
    #   sigmas[i,]=sigma[i,]*sqrt(w)
    # }
    
    sig=solve(crossprod(t(sigmas)))%*%sigmas
    ans3=coneA(ys,A%*%sig,w=as.vector(c/theta))
    ths = t(sigmas)%*%sig%*%ans3$thetahat
    # theta=ths/sqrt(w)
    theta=ths
  }
  beta = sig%*%ans3$thetahat
  print(nrep)
  ans=new.env()
  ans$xpl=xt
  ans$theta=theta
  ans$beta = beta
  ans$knots=xt[kn]
  ans$ma=ma
  return(ans)
}


set.seed(12)
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

xdat= poi(0.05,c(120,0.0004,.001),10000,H_D_gre)
y=poi(0.05,c(120,0.0004,.001),10000,H_D_gre)
xdat_S= poi(0.03,c(120,18,.000000007),10000,Y_R_gre)
xdat=xdat_S
# xdat = c(0, 32.4375, 64.875, 97.3125, 129.75, 162.1875, 194.625, 227.0625, 259.5, 291.9375, 324.375, 356.8125, 389.25, 421.6875, 454.125, 486.5625, 519.0, 575.125, 631.25, 687.375, 743.5, 799.625, 855.75, 911.875, 968.0, 1122.0, 1276.0, 1430.0, 1507.1666666666667, 1584.3333333333333, 1661.5, 1738.6666666666667, 1815.8333333333335, 1893.0, 1967.625, 2042.25, 2116.875, 2191.5, 2266.125, 2340.75, 2415.375, 2490.0, 2561.0, 2632.0, 2703.0, 2774.0, 2845.0, 2916.0, 2987.0, 3058.0, 3171.4, 3284.8, 3398.2, 3511.6, 3625.0, 3824.25, 4023.5, 4222.75, 4422.0, 4494.363636363636, 4566.727272727273, 4639.090909090909, 4711.454545454545, 4783.818181818182, 4856.181818181818, 4928.545454545454, 5000.909090909091, 5073.272727272727, 5145.636363636364, 5218.0, 5318.833333333333, 5419.666666666667, 5520.5, 5621.333333333333, 5722.166666666667, 5823.0, 5942.333333333333, 6061.666666666667, 6181.0, 6300.333333333333, 6419.666666666667, 6539.0, 6647.8, 6756.6, 6865.4, 6974.2, 7083.0, 7184.0, 7285.0, 7386.0, 7487.0, 7606.666666666667, 7726.333333333333, 7846.0, 7965.666666666667, 8085.333333333333, 8205.0, 8384.5, 8564.0, 8923.0, 9282.0)
k=4
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
for(u in seq(340,350,by=1)){
  print(xt[u])
  if(u %in% kn ){
    next
  }
  ans=dspl_s(xdat,k,u)
  # plot(xt,ans$theta)
  # y = seq(min(xdat),max(xdat),by=1)
  M=matrix(1:(length(xt)*(k+2)),ncol = length(xt),nrow = k+2)
  
  j=min(which((u<=kn)==1))
  # if(u==1){j=2}
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
  # if(u==1){j=2}
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
  # plot(c(0,max(xdat)),c(0,max(ans$theta)),pch=" ")
  plot(xdat,1:ndat)
  lines(xt,y*ndat/y[length(y)])
  print(u)
  y0=t(ans$beta)%*%M0
  t0=1:length(xdat)
  for(i in 1:length(xdat)){
    t0[i]=which.min(abs(xdat[i]-xt))
  }
  MSE=sqrt(sum((1:length(xdat)-ndat*y[t0]/y[length(y)])^2))/(length(xdat)-1)
  print(MSE)
  
  ###MLE
  lam=y0*ndat/y[length(y)]
  MLE=sum((log(lam))[2:length(lam)])-ndat
  print(MLE)
}

####MSE
t0 = round(xdat)
t0[1]=1
print(sqrt(sum((1:length(xdat)-ndat*y[t0]/y[length(y)])^2))/(length(xdat)-1))

###MLE
lam=y0*ndat/y[length(y)]
MLE=sum(log(lam))-ndat
print(MLE)
# plot(c(0,max(xdat)),c(0,max(ans$theta)),pch=" ")

# lines(ans$xpl,ans$theta)

points(xdat,1:length(xdat)*0,pch="|")

points(ans$knots,1:(k+2)*0,pch="X",col=2)



