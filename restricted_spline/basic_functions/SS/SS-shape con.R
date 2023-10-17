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


ss_con=function(xdat,k,u,v,q){######u is change point ex, u=2000;
  dn=gen(n)
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
  
  objfun=function(x){
    sum((t(x)%*%sigma1-0:(length(xdat)-1))^2)
  }
  grad_fun=function(x){
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
  x0=rep(0.1,k+1)
  ci <- rep(1e-300, m)

  # 运行优化z
  result <- constrOptim(x0, objfun, ui=A, ci=ci, grad=grad_fun)
  # result=constrOptim(result$par, objfun, ui=A, ci=ci, grad=grad_fun)
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

datac=readRDS('conti.rds')
xdat=datac[[8]][[10]]
n=0:length(xdat)
xdat=c(0,xdat)

xdat=data1
n=0:(length(xdat)-1)

k=10
ndat=length(xdat)
knots=round((0:(k+1))*ndat/(k+1))
knots[1]=1
knots[k+2]=ndat
t=xdat[knots]
m=k+2

for(u in seq(1800,3000,by=100)){
  for(v in seq(3500,5000,by=100)){
    print(u)
    print(v)
    u=3000
    v=6000
    res=ss_con(xdat,k,u,v,2)
    plot(xdat,n)
    y=res$beta%*%res$M
    c=max(n)/max(y)
    lines(xdat,y*c)
    y0=res$beta%*%res$M0*c
    MSE=sqrt(sum((n-y*c)^2))/(length(xdat)-1)
    print(MSE)
    
    ###MLE
    # MLE=sum((log(y0))[2:length(y0)])-max(y)
    # print(MLE)
    MLE= sum(gen(n)*log(c*res$beta%*%res$M0))-y[length(y)]*c
    # 
    # MLE=sum((log(y0))[2:length(y0)])-max(y)
    print(MLE)
  }
}

res=ss_con(xdat,k,800,3800,2)
plot(xdat,n)
y=res$beta%*%res$M
c=max(n)/max(y)
lines(xdat,y*c)
y0=res$beta%*%res$M0*c
MSE=sqrt(sum((n-y*c)^2))/(length(xdat)-1)
print(MSE)

###MLE
# MLE=sum((log(y0))[2:length(y0)])-max(y)
# print(MLE)
MLE= sum(gen(n)*log(c*res$beta%*%t(apply(res$M, 1, diff))))-y[length(y)]*c
# 
# MLE=sum((log(y0))[2:length(y0)])-max(y) 
print(MLE)




# res=continuous(xdat,k)
# res=continuous(xdat1,4)
