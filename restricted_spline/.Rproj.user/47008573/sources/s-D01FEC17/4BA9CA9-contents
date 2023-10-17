source('basic_functions/function.r')
output_path='/analysis_result/output/plot/spline_basis/'

######convex
k=4
par(font.axis=2)
xdat=seq(1,10100,by=100)
ndat=length(xdat)
knots=round((0:(k+1))*ndat/(k+1))
knots[1]=1
knots[k+2]=ndat
t=xdat[knots]
m=k+2
# par(cex.lab = 1.5, cex.main = 2, cex.axis = 1.8)
png(filename =  paste(getwd(),output_path,paste(paste('convex','knots',k+2,sep = '_'),sep=''),".png",sep=''),width = 800,height=600,res=100,units = 'px')
par(cex.axis = 1)  # 1.2是字体大小的倍数，您可以根据需要进行调整
par(mar = c(2.5, 2.5, 1, 1)) 
plot(c(0,10500),c(0,1.),pch='',ylab='',xlab='',xlim = c(0, 10500), ylim = c(0, 1.1))
u=seq(1,10000,by=2)
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
lines(xdat,M0[7,],lty=1,lwd=5,col='brown')
lines(xdat,M0[6,],lty=1,lwd=5,col='cyan')
lines(xdat,M0[5,],lty=1,lwd=5,col='gold')
lines(xdat,M0[4,],lty=1,lwd=5,col='red')
lines(xdat,M0[3,],lty=1,lwd=5,col='purple')
lines(xdat,M0[2,],lty=1,lwd=5,col='green')
lines(xdat,M0[1,],lty=1,lwd=5,col='blue')
dev.off()

#####S-shape
u=4800
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
png(filename =  paste(getwd(),output_path,paste(paste('S-shape','knots',k+2,'CP',u,sep = '_'),sep=''),".png",sep=''),width = 800,height=600,res=100,units = 'px')
par(cex.axis = 1)  # 1.2是字体大小的倍数，您可以根据需要进行调整
par(mar = c(2.5, 2.5, 1, 1)) 
# plot(c(0,10500),c(0,1.05),pch='',xlab = '',ylab = '',main= 'Basis Functions for S-shape, y=4800, k=6')
plot(c(0,10500),c(0,1.05),pch='',xlab = '',ylab = '',main= '')
lines(xdat,sigma0[6,],lty=1,lwd=5,col='gold')
lines(xdat[1:47],sigma0[1,][1:47],lty=1,lwd=5,col='blue')
lines(xdat[1:47],sigma0[2,][1:47],lty=1,lwd=5,col='green')
lines(xdat[47:101],sigma0[5,][47:101],lty=1,lwd=5,col='salmon')
lines(xdat[1:101],sigma0[4,][1:101],lty=1,lwd=5,col='purple')
lines(xdat,sigma0[3,],lty=1,lwd=5,col='red')
dev.off()




##########double S-shape
k=6
m=k+2
u=2200
v=6200
q=1
# l0=1:n0
A=diag(m-1)
A=rbind(A,1:(m-1)*0)
if(q==1){
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
  
  
  sigma0=matrix(1:((m-1)*length(xdat)),nrow=m-1,ncol=length(xdat))
  sigma1=matrix(1:((m-1)*length(xdat)),nrow=m-1,ncol=length(xdat))
  j=min(which((u<=t)==1))
  if(j>=3){  ###before u
    for(l in 1:(j-2)){
      sigma0[l,]=1-M0[l,]
      sigma1[l,]=xdat-M[l,]
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

  }
  if(j==(j1-1)){
    sigma0[j-1,]=1-M0[j-1,]+r*M0[j,]+r*r1*(1-M0[j+1,])
    sigma1[j-1,]=xdat-M[j-1,]+r*M[j,]+r*r1*(xdat-M[j+1,])
    ma=max(sigma0[j-1,])
    sigma0[j-1,]=sigma0[j-1,]/ma
    sigma1[j-1,]=sigma1[j-1,]/ma

  }
 if((j1-1)<=(k)){###after v
    for(l in (j1-1):(k)){
      sigma0[l,]=1-M0[l+2,]
      sigma1[l,]=xdat-M[l+2,]
    }
  }
  sigma0[k+1,]=M0[k+3,]
  sigma1[k+1,]=xdat
}

png(filename =  paste(getwd(),output_path,paste(paste('Double_S_shape','knots',k+2,'CP',u,v,sep = '_'),sep=''),".png",sep=''),width = 800,height=600,res=100,units = 'px')
par(cex.axis = 1)  # 1.2是字体大小的倍数，您可以根据需要进行调整
par(mar = c(2.5, 2.5, 1, 1)) 
plot(c(0,10500),c(0,1.),pch='',ylab='',xlab='',xlim = c(0, 10500), ylim = c(0, 1.1))
lines(xdat,sigma0[3,],lty=1,lwd=5,col='turquoise')
lines(xdat,sigma0[7,],lty=1,lwd=5,col='brown')
lines(xdat,sigma0[6,],lty=1,lwd=5,col='red')
lines(xdat,sigma0[5,],lty=1,lwd=5,col='gold')
lines(xdat,sigma0[1,],lty=1,lwd=5,col='blue')
lines(xdat,sigma0[2,],lty=1,lwd=5,col='green')
lines(xdat,sigma0[4,],lty=1,lwd=5,col='purple')
  # 绘制x轴刻度线和标签
dev.off()
# axis(side = 2, line = -1.76, lwd = 2,ylab=NULL)