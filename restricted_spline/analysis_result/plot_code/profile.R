
name=list('G-O','G-O-S','H-D','Gompertz','Pareto','Weibull','Y-Exp','Y-M')
list0=list(c(120,0.00023),c(120,0.0006),c(120,0.0004,.001),c(120,0.0005,0.9996),c(120,2.5,6000),c(120,0.00032,1.02),c(120,3,0.0001),c(120,18,.000000007))
func_name=c('Goel_Okumoto','G_O','Hossain_Dahiya','Gompertz','Pareto','Weibull','Yamada_Exp','Yamada_Raleigh')
output_path='/analysis_result/output/plot/profile/'
# par(mfrow = c(3, 3))  # 设置图形布局为3行3列
# par(mar = c(2.5, 2.5, 2.5, 2.5)) 
# par(cex.lab = 2.5, cex.main = 2, cex.axis = 1.2)
# l=1:10000
# # 绘制每个子图并隐藏刻度
# for (i in 1:8) {
#   plot(l,eval(parse(text = func_name[i]))(l,list0[[i]]), axes = FALSE, xlab = "", ylab = "", main = name[[i]])
#   axis(1, labels = FALSE, tick = FALSE)  # 隐藏x轴刻度
#   axis(2, labels = FALSE, tick = FALSE)  # 隐藏y轴刻度
#   box()  # 绘制边框，确保每个子图的大小相同
# }

# par(mfrow = c(1, 1))
# par(mar = c(5, 4, 4, 2) + 0.1)


par(mfrow = c(1, 1))  # 设置图形布局为3行3列
par(mar = c(0, 0, 0, 0)) 
par(cex.lab = 2.5, cex.main = 2, cex.axis = 1.2)
l=1:10000
# 绘制每个子图并隐藏刻度
for (i in 1:8) {
  png(filename =  paste(getwd(),output_path,paste(name[[i]],".png",sep = ''),sep=''), width = 200, height = 80, res = 100,units = 'px')
  par(mar = c(0, 0, 0, 0)) 
  plot(l,eval(parse(text = func_name[i]))(l,list0[[i]]), axes = FALSE, xlab = "", ylab = "", main = '',lwd=1)
  axis(1, labels = FALSE, tick = FALSE)  # 隐藏x轴刻度
  axis(2, labels = FALSE, tick = FALSE)  # 隐藏y轴刻度
  # box()  # 绘制边框，确保每个子图的大小相同
  dev.off()
}

