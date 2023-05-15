clonerow<-function(data,n,x,y)
{data<-data.frame(data)
l=length(data)
data<-cbind(data,data.frame(x),data.frame(y))
mydata=data.frame()
i=1
while(i<l)
{
  data1<-data[i,]#逐行获取数据
  j=1
  print(j)
  while(j<=n+1)
  {
  if(j==1){data1$x="N"
  data1$y=0}
  else{data1$x="Y"
  data1$y=j-1}
  j=j+1
  mydata<-rbind(mydata,data1)}
  
  i=i+1
}
return(mydata)
}
#测试
table = data.frame(
  姓名 = c("张三", "李四","王五"),
  工号 = c("001","002","003"),
  月薪 = c(1000, 2000,3000)
)
clonerow(table,3,"X","y")
