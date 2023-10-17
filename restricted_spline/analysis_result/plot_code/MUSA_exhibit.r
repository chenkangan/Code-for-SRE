Data1= c(0,411,411,362,192,228,229,175,201,177,104,109,58)
Data2= c(0,7,22,32,47,26,25,16,48,36,53,57,39,71,80,65,57,90,60,57,90,46,57,29,40,16,18,37,15,8,28,6,5,3,3,12)
Data3=c(0,44,135,210,162,97,39,72,83,147,127,77,41,5)
Data4=c(0,203,136,183,47,46,71,54,57,80,64,27,42,55,62,11)
Data5=c(0,238,179,222,73,56,88,78,92,104,85,53,45,59,84,27)
Data6=c(0,369,298,398,115,83,150,142,172,202,168,89,87,111,253,70)
Data7=c(0,334,255,359,89,73,133,118,137,178,147,63,84,107,231,54)
Data1=cumsum(Data1)
Data2=cumsum(Data2)
Data3=cumsum(Data3)
Data4=cumsum(Data4)
Data5=cumsum(Data5)
Data6=cumsum(Data6)
Data7=cumsum(Data7)
t1=1:length(Data1)*720-720
t2=1:length(Data2)*720-720
t3=1:length(Data3)*720-720
t4=1:length(Data4)*720-720
t5=1:length(Data5)*720-720
t6=1:length(Data6)*720-720
t7=1:length(Data7)*720-720
t00=list(t1,t2,t3,t4,t5,t6,t7)
nnew=list(Data1,Data2,Data3,Data4,Data5,Data6,Data7)
col=c('brown','green','red','yellow','orange','gold','purple')
data_name=c(1,2,3,4,5,6,7)
par(cex.axis = 1.5)  # 1.2是字体大小的倍数，您可以根据需要进行调整

# u=0
# D1=c(2,4,5)
# D2=c(1,3,6,7)
# 
# output_path='/analysis_result/output/plot/data_exhibit/'
# 
# # plot(c(0,10500),c(0,2700),pch='',ylab='',xlab='',xlim = c(0, 10000), ylim = c(0, 2700))
# # for (i in 1:8) {
# #   par(mar = c(0, 0, 0, 0)) 
# #   plot(l,eval(parse(text = func_name[i]))(l,list0[[i]]), axes = FALSE, xlab = "", ylab = "", main = '',lwd=1)
# #   axis(1, labels = FALSE, tick = FALSE)  # 隐藏x轴刻度
# #   axis(2, labels = FALSE, tick = FALSE)  # 隐藏y轴刻度
# #   # box()  # 绘制边框，确保每个子图的大小相同
# # }
# png(filename =  paste(getwd(),output_path,paste('MUSA1',".png",sep = ''),sep=''), width = 800, height = 600, res = 100,units = 'px')
# par(mar = c(3, 3, 1, 1)) 
# for (i in D1) { 
#   data1=t00[[i]]
#   data1=data1/max(data1)*10000
#   u=u+1
#   n0=nnew[[i]]
#   a=gen(data1)/gen(n0)
#   xdat=c(0,cumsum(rep(a,gen(n0))))
#   if(i %in% c(1,2)){
#     if(i==1){
#       ma=2700
#       p=ma/18
#       plot(c(0,10500),c(0,ma),pch='',ylab='',xlab='',xlim = c(0, 10000), ylim = c(0, ma))
#       # mtext("自定义X轴标签", side = 1, line = 3, cex = 1.2)
#       lines(xdat,0:(length(xdat)-1),type = 'l',xlab = 'Time',ylab='Defects',lwd=4,col=col[i])
#       legend(x=c(0),y=ma-(u-1)*p,cex=1.5,legend = data_name[i],col = c(col[i]),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#       
#     }else{
#       ma=1500
#       p=ma/18
#       plot(c(0,10500),c(0,ma),pch='',ylab='',xlab='',xlim = c(0, 10000), ylim = c(0, ma))
#       lines(xdat,0:(length(xdat)-1),type = 'l',xlab = 'Time',ylab='Defects',lwd=4,col=col[i])
#       legend(x=c(0),y=ma-(u-1)*p,cex=1.5,legend = data_name[i],col = c(col[i]),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#     }
#   }else{
#     lines(xdat,0:(length(xdat)-1),type = 'l',xlab = 'time',ylab='Defects',lwd=4,col=col[i])
#     legend(x=c(0),y=ma-(u-1)*p,cex=1.5,legend = data_name[i],col = c(col[i]),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
#     
#   }
# 
#   
# }
# dev.off()

####another_plan

u=0
D1=c(1,2,3)
D2=c(4,5,6,7)

output_path='/analysis_result/output/plot/data_exhibit/'

# plot(c(0,10500),c(0,2700),pch='',ylab='',xlab='',xlim = c(0, 10000), ylim = c(0, 2700))
# for (i in 1:8) {
#   par(mar = c(0, 0, 0, 0)) 
#   plot(l,eval(parse(text = func_name[i]))(l,list0[[i]]), axes = FALSE, xlab = "", ylab = "", main = '',lwd=1)
#   axis(1, labels = FALSE, tick = FALSE)  # 隐藏x轴刻度
#   axis(2, labels = FALSE, tick = FALSE)  # 隐藏y轴刻度
#   # box()  # 绘制边框，确保每个子图的大小相同
# }
png(filename =  paste(getwd(),output_path,paste('MUSA1',".png",sep = ''),sep=''), width = 800, height = 600, res = 100,units = 'px')
par(mar = c(3, 3, 1, 1)) 
for (i in D1) { 
  data1=t00[[i]]
  data1=data1/max(data1)*10000
  u=u+1
  n0=nnew[[i]]
  a=gen(data1)/gen(n0)
  xdat=c(0,cumsum(rep(a,gen(n0))))
  if(i %in% c(1,4)){
    if(i==1){
      ma=2700
      p=ma/18
      plot(c(0,10500),c(0,ma),pch='',ylab='',xlab='',xlim = c(0, 10000), ylim = c(0, ma))
      # mtext("自定义X轴标签", side = 1, line = 3, cex = 1.2)
      lines(xdat,0:(length(xdat)-1),type = 'l',xlab = 'Time',ylab='Defects',lwd=4,col=col[i])
      legend(x=c(0),y=ma-(u-1)*p,cex=1.5,legend = data_name[i],col = c(col[i]),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
      
    }else{
      ma=2700
      p=ma/18
      plot(c(0,10500),c(0,ma),pch='',ylab='',xlab='',xlim = c(0, 10000), ylim = c(0, ma))
      lines(xdat,0:(length(xdat)-1),type = 'l',xlab = 'Time',ylab='Defects',lwd=4,col=col[i])
      legend(x=c(0),y=ma-(u-1)*p,cex=1.5,legend = data_name[i],col = c(col[i]),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
    }
  }else{
    lines(xdat,0:(length(xdat)-1),type = 'l',xlab = 'time',ylab='Defects',lwd=4,col=col[i])
    legend(x=c(0),y=ma-(u-1)*p,cex=1.5,legend = data_name[i],col = c(col[i]),lty=1, lwd=3,horiz = T,seg.len=6,bty='n',xpd = T,inset=-0.25, x.intersp=1.5, y.intersp=0.5)
    
  }
}
dev.off()

