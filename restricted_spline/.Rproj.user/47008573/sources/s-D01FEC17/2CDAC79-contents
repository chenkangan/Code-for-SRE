source('basic_functions/function.r')
# ##### DCAS CP&Knots
# percent=1
# loaded_results <- readRDS(file.path("analysis_result", "DCAS", paste(paste('DCAS_results',percent,sep='_'),'.rds',sep='')))
# 
# 
# i=0
# ###knots
# c=matrix(1,4,16)
# ###cp
# c1=matrix(1, nrow = 6, ncol = 16)
# for (x in loaded_results) {
#   i=i+1
#   # print(i)
#   j=0
#   k=0
#   l=0
#   for ( y in x) {
#     j=j+1
#     y_len=length(y)
#     # print(y_len)
#     if(j==4){
#       dat=y[[1]]
#     }
#     ###knots
#     if(j %in% c(4,5,7,8)){
#       k=k+1
#       c[k,i]=y[[y_len]]
#     }
#     ###cp
#     if(j %in% c(5)){
#       l=l+1
#       c1[l,i]=round(y[[y_len-1]],digit=3)
#     }
#     if(j %in% c(6)){
#       l=l+1
#       c1[l,i]=round(dat[y[[y_len]]],digit=3)
#     }
#     if(j %in% c(7,8)){
#       l=l+1
#       c1[l,i]=round(y[[y_len-2]],digit=3)
#       l=l+1
#       c1[l,i]=round(y[[y_len-1]],digit=3)
#       
#     }
# 
#   }
# }
# 
# c=as.data.frame(c)
# c1=as.data.frame(c1)
# write.csv(c,paste(paste('Knots',percent,sep='_'),'.csv',sep=''),row.names = FALSE)
# write.csv(c1,paste(paste('CP',percent,sep='_'),'.csv',sep=''),row.names = FALSE)

##### MUSA CP&Knots
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


for(i in 1:7){
  print(max(nnew[[i]]))
}
loaded_results <- readRDS(file.path("analysis_result", "MUSA", "MUSA_results.rds"))

i=0
###knots
c=matrix(1,4,7)
###cp
c1=matrix(1, nrow = 6, ncol = 7)
for (x in loaded_results) {
  i=i+1
  data1=t00[[i]]
  n0=nnew[[i]]
  a=gen(data1)/gen(n0)
  xdat=c(0,cumsum(rep(a,gen(n0))))
  # print(i)
  j=0
  k=0
  l=0
  for ( y in x) {
    j=j+1
    y_len=length(y)
    # print(y_len)
    if(j==4){
      dat=y[[1]]
    }
    ###knots
    if(j %in% c(4,5,7,8)){
      k=k+1
      c[k,i]=y[[y_len]]
    }
    ###cp
    if(j %in% c(5)){
      l=l+1
      c1[l,i]=round(y[[y_len-1]],digit=3)
    }
    if(j %in% c(6)){
      l=l+1
      c1[l,i]=round(xdat[y[[y_len]]],digit=3)
    }
    if(j %in% c(7,8)){
      l=l+1
      c1[l,i]=round(y[[y_len-2]],digit=3)
      l=l+1
      c1[l,i]=round(y[[y_len-1]],digit=3)
      
    }
  }
}
####MSE
i=0
###knots
c2=matrix(1,8,7)
###cp
for (x in loaded_results) {
  i=i+1
  # print(i)
  j=0
  for ( y in x) {
    j=j+1
    c2[j,i]=y[[3]]
      
  }
    
}
print(c)
write.csv(c,'Knots_MUSA.csv',row.names = FALSE)
write.csv(c1,'CP_MUSA.csv',row.names = FALSE)
write.csv(c2,'MSE_MUSA.csv',row.names = FALSE)

# plot(1:10,1:10)
# c=as.data.frame(c)
# c1=as.data.frame(c1)
# write.csv(c,paste('Knots','.csv',sep=''),row.names = FALSE)
# write.csv(c1,paste('CP','.csv',sep=''),row.names = FALSE)