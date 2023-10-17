# #########MUSA
# loaded_results <- readRDS(file.path("analysis_result", "MUSA", "MUSA_results.rds"))
# output_path='/analysis_result/output/plot/MUSA/'
# par(font.axis=2)
# color=c('red','blue','cyan','brown','green','gold','purple')
# func_name=c('Goel_Okumoto','G_O','Hossain_Dahiya','Gompertz','Pareto','Weibull','Yamada_Exp','Yamada_Raleigh')
# name1=c('G-O','G-O-S','H-D','Gompertz','Pareto','Weibull','Y-Exp','Y-M')
# name2=c('non','IFR','DFR','Convex','S-shape','DFR-IFR','DID','IDI')
# fu=c(1,8,7,2,3,6,5,4)
# li_width=c(2,2,2,2,2,2,2,2)
# best=c(3,6,2,5,6,6,6)
# Data1= c(0,411,411,362,192,228,229,175,201,177,104,109,58)
# Data2= c(0,7,22,32,47,26,25,16,48,36,53,57,39,71,80,65,57,90,60,57,90,46,57,29,40,16,18,37,15,8,28,6,5,3,3,12)
# Data3=c(0,44,135,210,162,97,39,72,83,147,127,77,41,5)
# Data4=c(0,203,136,183,47,46,71,54,57,80,64,27,42,55,62,11)
# Data5=c(0,238,179,222,73,56,88,78,92,104,85,53,45,59,84,27)
# Data6=c(0,369,298,398,115,83,150,142,172,202,168,89,87,111,253,70)
# Data7=c(0,334,255,359,89,73,133,118,137,178,147,63,84,107,231,54)
# Data1=cumsum(Data1)
# Data2=cumsum(Data2)
# Data3=cumsum(Data3)
# Data4=cumsum(Data4)
# Data5=cumsum(Data5)
# Data6=cumsum(Data6)
# Data7=cumsum(Data7)
# t1=1:length(Data1)*720-720
# t2=1:length(Data2)*720-720
# t3=1:length(Data3)*720-720
# t4=1:length(Data4)*720-720
# t5=1:length(Data5)*720-720
# t6=1:length(Data6)*720-720
# t7=1:length(Data7)*720-720
# t00=list(t1,t2,t3,t4,t5,t6,t7)
# nnew=list(Data1,Data2,Data3,Data4,Data5,Data6,Data7)
# li_type=c(2,2,2,1,1,2,1,1)
# col=c('blue','yellowgreen','yellow3','tomato4','salmon','purple','cyan','gold')
# par(cex.axis = 1)  # 1.2是字体大小的倍数，您可以根据需要进行调整
# par(mar = c(4, 4, 0, 0)) 
# 
# for (i in 1:7) {
#   
#   png(filename =  paste(getwd(),output_path,paste(paste('MUSA','Project',i,sep = '_'),sep=''),".png",sep=''),width = 800,height=600,res=100,units = 'px')
#   par(mar = c(4.5, 4.5, 1, 1)) 
#   
#   data1=t00[[i]]
#   n0=nnew[[i]]
#   dn=gen(n0)
#   ndat=max(n0)
#   c=ndat/25
#   
#   plot(data1,n0,type='l',lwd=3,xlab = 'Test hours',ylab='Number of failures')
#   legend(x=c(0),y=max(n0),cex = 1,legend = 'real data',col = c('black'),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#   data=loaded_results[[i]]
#   for(j in 1:8){
#     x=data[[j]][[1]]
#     y=data[[j]][[2]]
#     if(j==1){
#       name=name1[best[i]]
#     }else{
#       name=name2[j]
#     }
#     
#     lines(x,y,lty=li_type[j],lwd=li_width[j],col=col[j])
#     legend(x=c(0),y=c(ndat-fu[j]*c),cex = 1,legend = name,col = col[j],lty=li_type[j], lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#     
#   }
#   dev.off()
# }



########DCAS
loaded_results <- readRDS(file.path("analysis_result", "DCAS", "DCAS_results_1.rds"))
output_path='/analysis_result/output/plot/DCAS/'
par(font.axis=2)
color=c('red','blue','cyan','brown','green','gold','purple')
func_name=c('Goel_Okumoto','G_O','Hossain_Dahiya','Gompertz','Pareto','Weibull','Yamada_Exp','Yamada_Raleigh')
name1=c('G-O','G-O-S','H-D','Gompertz','Pareto','Weibull','Y-Exp','Y-M')
name2=c('non','IFR','DFR','Convex','S-shape','DFR-IFR','DID','IDI')
fu=c(1,8,7,2,3,6,5,4)
li_width=c(2,2,2,2,2,2,2,2)
li_type=c(2,2,2,1,1,2,1,1)
col=c('blue','yellowgreen','yellow3','tomato4','salmon','purple','cyan','gold')
data_name=c(1,2,3,4,5,6,'14C',17,27,40,'SS1A','SS1B','SS1C','SS2','SS3','SS4')
best=c(6,6,5,1,6,6,4,6,5,5,6,7,6,4,4,6)
data=readRDS(file.path(getwd(),'realdata/real_conti.rds'))



par(cex.axis = 1)  # 1.2是字体大小的倍数，您可以根据需要进行调整
par(mar = c(4, 4, 0, 0)) 

for (i in 1:16) {
  print(i)
  data0=c(0)
  data1=c(0)
  n=length(data[[i]])
  for(j in 1:n){
    data0=c(data0,data[[i]][[j]][1])####hours
  }
  data1=cumsum(data0)
  data1=data1*10000/max(data1)
  ndat=length(data1)
  c=ndat/25
  png(filename =  paste(getwd(),output_path,paste(paste('DCAS','System',data_name[i],sep = '_'),sep=''),".png",sep=''),width = 800,height=600,res=100,units = 'px')
  par(mar = c(4.5, 4.5, 1, 1)) 
  if(i==10){
    plot(data1,0:(length(data1)-1),type='l',lwd=3,xlab = 'Test hours',ylab='Number of failures')
    legend(x=c(7000),y=ndat/2,cex = 1,legend = 'real data',col = c('black'),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
    
  }else{
    plot(data1,0:(length(data1)-1),type='l',lwd=3,xlab = 'Test hours',ylab='Number of failures')
    legend(x=c(0),y=ndat,cex = 1,legend = 'real data',col = c('black'),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
    
  }
  data2=loaded_results[[i]]
  # print(length(data))
  for(j in 1:8){
    # print(j)
    x=data2[[j]][[1]]
    y=data2[[j]][[2]]
    if(j==1){
      name=name1[best[i]]
    }else{
      name=name2[j]
    }
    if(i==10){
      lines(x,y,lty=li_type[j],lwd=li_width[j],col=col[j])
      legend(x=c(7000),y=c(ndat/2-fu[j]*c),cex = 1,legend = name,col = col[j],lty=li_type[j], lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
    }else{
      lines(x,y,lty=li_type[j],lwd=li_width[j],col=col[j])
      legend(x=c(0),y=c(ndat-fu[j]*c),cex = 1,legend = name,col = col[j],lty=li_type[j], lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
    }
  }
  dev.off()
}
