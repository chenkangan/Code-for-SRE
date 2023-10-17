# source ('basic_functions/function.R')
# data=readRDS(file.path(getwd(),'realdata/real_conti.rds'))
# name=list('Spline','Penalize','IFR','DFR','G-O','G-O-S','H-D','Gompertz','Pareto','Weibull','Y-Exp','Y-M')
# col=c('brown','green','red','yellow','brown','green','yellow','purple','brown','blue','red','blue','brown','green','red','blue')
# data_name=c(1,2,3,4,5,6,'14C',17,27,40,'SS1A','SS1B','SS1C','SS2','SS3','SS4')
# D1=c(2,7,9)
# D2=c(1,6,11,8)
# D3=c(15,13,16,14)
# D4=c(5,12)
# L=c(8,3,1,1,100,1,1,1,1,1,1,1,1,1,20,1)
# u=0
# par(cex.axis = 1.5)
# output_path='/analysis_result/output/plot/data_exhibit/'
# png(filename =  paste(getwd(),output_path,paste('DCAS4',".png",sep = ''),sep=''), width = 800, height = 600, res = 100,units = 'px')
# par(mar = c(2.5, 2.5, 1, 1)) 
# for (i in D4) {
#   u=u+1
#   print(i)
#   n=length(data[[i]])
#   data0=c(0)
#   data1=c(0)
#   for(j in 1:n){
#     data0=c(data0,data[[i]][[j]][1])####hours
#     data1=c(data1,data[[i]][[j]][2])####days
#   }
#   data1=cumsum(data0)
#   data1=data1/max(data1)*10000
#   # data1=cumsum(data1)
#   ndat=length(data1)
#   if(i %in% c(2,1,15,5)){  
#     p=L[i]
#     n0=ndat
#     plot(data1,0:(ndat-1),type = 'l',xlab = 'time',ylab='Defects',lwd=4,col=col[i])
#     legend(x=c(0),y=n0,cex = 2,legend = data_name[i],col = c(col[i]),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#   }
#   else{
#     lines(data1,0:(ndat-1),type = 'l',xlab = 'time',ylab='Defects',lwd=4,col=col[i])
#     legend(x=c(0),y=n0-(u-1)*p,cex=2,legend = data_name[i],col = c(col[i]),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#   }
# }
# dev.off()


source ('basic_functions/function.R')
data=readRDS(file.path(getwd(),'realdata/real_conti.rds'))
name=list('Spline','Penalize','IFR','DFR','G-O','G-O-S','H-D','Gompertz','Pareto','Weibull','Y-Exp','Y-M')
col=c('brown','green','red','yellow','brown','green','yellow','purple','brown','yellow','red','blue','green','green','red','blue')
data_name=c(1,2,3,4,5,6,'14C',17,27,40,'SS1A','SS1B','SS1C','SS2','SS3','SS4')
D1=c(2,7,9,3)
D2=c(1,6,11,8,4)
D3=c(15,16,14,10)
D4=c(5,12,13)
L=c(8,3,1,1,100,1,1,1,1,1,1,1,1,1,20,1)
u=0
par(cex.axis = 1.5)
output_path='/analysis_result/output/plot/data_exhibit/'
png(filename =  paste(getwd(),output_path,paste('DCAS4',".png",sep = ''),sep=''), width = 800, height = 600, res = 100,units = 'px')
par(mar = c(2.5, 2.5, 1, 1)) 
for (i in D4) {
  u=u+1
  print(i)
  n=length(data[[i]])
  data0=c(0)
  data1=c(0)
  for(j in 1:n){
    data0=c(data0,data[[i]][[j]][1])####hours
    data1=c(data1,data[[i]][[j]][2])####days
  }
  data1=cumsum(data0)
  data1=data1/max(data1)*10000
  # data1=cumsum(data1)
  ndat=length(data1)
  if(i %in% c(2,1,15,5)){  
    p=L[i]
    n0=ndat
    plot(data1,0:(ndat-1),type = 'l',xlab = 'time',ylab='Defects',lwd=4,col=col[i])
    legend(x=c(0),y=n0,cex = 2,legend = data_name[i],col = c(col[i]),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  }
  else{
    lines(data1,0:(ndat-1),type = 'l',xlab = 'time',ylab='Defects',lwd=4,col=col[i])
    legend(x=c(0),y=n0-(u-1)*p,cex=2,legend = data_name[i],col = c(col[i]),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
  }
}
dev.off()
