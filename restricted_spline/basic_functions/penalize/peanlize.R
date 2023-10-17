library('mvtnorm')
lanins= function(a,b,c,i,j){#####i is calculation interval j is a or b or c
  if (i==1){f = function(x) (x-b)*(x-c)/(a-b)/(a-c)} 
  if (i==2){f = function(x) (x-a)*(x-c)/(b-a)/(b-c)} 
  if (i==3){f = function(x) (x-a)*(x-b)/(c-a)/(c-b)} 
  if (j==1){return(integrate(f,a,b))}
  if (j==2){return(integrate(f,b,c))}
}
# 
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

xdat = c(0, 32.4375, 64.875, 97.3125, 129.75, 162.1875, 194.625, 227.0625, 259.5, 291.9375, 324.375, 356.8125, 389.25, 421.6875, 454.125, 486.5625, 519.0, 575.125, 631.25, 687.375, 743.5, 799.625, 855.75, 911.875, 968.0, 1122.0, 1276.0, 1430.0, 1507.1666666666667, 1584.3333333333333, 1661.5, 1738.6666666666667, 1815.8333333333335, 1893.0, 1967.625, 2042.25, 2116.875, 2191.5, 2266.125, 2340.75, 2415.375, 2490.0, 2561.0, 2632.0, 2703.0, 2774.0, 2845.0, 2916.0, 2987.0, 3058.0, 3171.4, 3284.8, 3398.2, 3511.6, 3625.0, 3824.25, 4023.5, 4222.75, 4422.0, 4494.363636363636, 4566.727272727273, 4639.090909090909, 4711.454545454545, 4783.818181818182, 4856.181818181818, 4928.545454545454, 5000.909090909091, 5073.272727272727, 5145.636363636364, 5218.0, 5318.833333333333, 5419.666666666667, 5520.5, 5621.333333333333, 5722.166666666667, 5823.0, 5942.333333333333, 6061.666666666667, 6181.0, 6300.333333333333, 6419.666666666667, 6539.0, 6647.8, 6756.6, 6865.4, 6974.2, 7083.0, 7184.0, 7285.0, 7386.0, 7487.0, 7606.666666666667, 7726.333333333333, 7846.0, 7965.666666666667, 8085.333333333333, 8205.0, 8384.5, 8564.0, 8923.0, 9282.0)
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


M=matrix(1:(length(xdat)*(k+3)),ncol = length(xdat),nrow = k+3)
for (j in 1:(k+3)){
  for(i in 1:length(xdat)){
    M[j,i]=base_int(xdat[i],j,xt,k,ngr,kn,t)
  }
}
M0=matrix(1:(length(xdat)*(k+3)),ncol = length(xdat),nrow = k+3)
for (j in 1:(k+3)){
  for(i in 1:length(xdat)){
    M0[j,i]=base_fun(xdat[i],j,xt,k,ngr,kn,t)
  }
}

Sigma=diag(c(0.0000001,0.0000001,0.0000001,0.0000001,0.0000001,0.0000001,0.0000001))
sig=0.01

beta0 = c(-2.243199e-18,4.622856e-18,-7.213663e-18,9.797940e-05,1.422011e-04,-1.659203e-18,6.383972e-19)

post=function(beta){
  LLF=0
  for (i in 1:ndat) {
    LLF=LLF+log(((beta)%*%(M0[,i])))
  }
  c = pnorm(1,0,sig)-pnorm(0,0,sig)
  LLF=LLF-(beta%*%M[,ndat])
  # print(LLF)
  if(w<0){
    Lpos=LLF-log(2*c)-(1/2*log(2*pi)-log(sig)/2-(w)^2/2/sig)
  }
  if(w>=0){
    Lpos=LLF-1/2*log(2*pi)-log(sig)/2-(w)^2/2/sig
  }
  # print(c)
  # print(1/2*log(2*pi)-log(sig)/2-(w)^2/2/sig)
  return(Lpos)
}

n1=100####number of segemnt
start=-0.5#####start value
end=1.5####end..
d=1:n1
for (j in 1:n1) {
  w=start+(end-start)/n1*j###initial w
  x <- rmvnorm(n=1000000, mean=(1+w)*beta0, sigma=(1+w)^2*Sigma)####generating beta
  for(i in 1:ncol(x)){
    if(i==1){u=(x[,1]>0)}
    if(i>1){u=u*(x[,i]>0)}
  }
  y=x[which(u==1),]######calculating posterior beta
  summ=0
  for (i in 1:nrow(y)) {
    b=(post(y[i,]))+1200
    summ=summ+exp(b)/length(y)
  }####calculating margin posterior of w
  d[j]=summ
  print(j)
  print(d[j])
}



