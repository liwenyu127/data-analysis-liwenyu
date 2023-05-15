#判断字段是否存在	查询字段是否存在
#克隆行	根据需求复制行数据
#时间字符期串转日	例：(1637359216661)13位毫秒日期转换成“yyyy-MM-dd HH:mm:ss"

#数据类型转换	对数据类型的转换操作
#单位换算	数值数据单位的换算
#提取 sql 的库表名	提取到sql语句中涉及到的库表名称

#################
#96查询字段是否存在
#1.把数据转换成数据框
#2.如果存在返回TRUE，如果不存在返回FALSE
library(xlsx)
mydata=read.xlsx('C:/Users/44168/Desktop/新建 XLSX 工作表.xlsx',sheetIndex = 1)

length(mydata$review)

isexist<-function(data,column){
  mydata<-data.frame(data)
  #column<-as.character(column)
  c=names(mydata)
  print(c)
  print(column)
  if(column %in% c) {
    print("存在")
    return (TRUE)
  } else {
    return (FALSE)
  }
  }


########################
#97克隆行
#1.读取数据
#2.把数据转换成数据框
#克隆x到y行

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
while(j<=n)
{mydata<-rbind(mydata,data1)
print(data1)
print(mydata)

  if(j==1){mydata[i,-2]="N"
  mydata[i,-1]=0}
  else{mydata[i,-2]="Y"
  mydata[i,-1]=j-1}
  j=j+1
}
print('\n')
print(mydata)
  i=i+1
}
return(mydata)
}


#########################
#x为传入的时间字符期串time
#时间戳在线转换工具：http://shijianchuo.wiicha.com/
ms.date<-function(x){
x=substr(x, 1,10)
x<-as.numeric(x)
class(x)='POSIXct'
a=x
return(a)
}



##############################
#numeric
#character
#logical
#factor
#x为待转数据
#y为传入的要转换的目标数据类型
data.transm<-function(x,y){
a<-switch (y,
"numeric"=as.numeric(x),
"character"=as.character(x),
"logical"=as.logical(x),
"factor"=as.factor(x)
)
return(a)
}

#测试
data.transm("789","numeric")
data.transm(1,"logical")
data.transm(6,"character")



#############
#序号101.  找一些常见的单位，可以实现换算的功能
#，比如，bit, byte,kb，mb等之间的换算，可以先做这一类，加上时间单位的换算
#一般转科学计数
ms.change<-function(n,c1,c2){
  if(c2=='bit') {
    if(c1=='bit'){return(n)}
    else if( c1=='byte'){return(8*n)}
    else if( c1=='KB'){return(1024*8*n)}
    else if(  c1=='MB'){return(1024*1024*8*n)}
    else if(  c1=='GB'){return(1024*1024*1024*8*n)}
    else if(  c1=='TB'){return(1024*1024*1024*1024*8*n)}
    else { return(FALSE)}
  } 
  if(c2=='byte') {
    if(c1=='bit'){return(n/8)}
    else if( c2=='byte'){return(n)}
    else if( c2=='KB'){return(1024*n)}
    else if(  c2=='MB'){return(1024*1024*n)}
    else if(  c2=='GB'){return(1024*1024*1024*n)}
    else if(  c2=='TB'){return(1024*1024*1024*1024*n)}
    else { return(FALSE)}
  } 
  if(c2=='KB') {
    if(c1=='bit'){return(n/1024/8)}
    else if( c2=='byte'){return(n/1024)}
    else if( c2=='KB'){return(n)}
    else if(  c2=='MB'){return(1024*n)}
    else if(  c2=='GB'){return(1024*1024*n)}
    else if(  c2=='TB'){return(1024*1024*1024*n)}
    else { return(FALSE)}
  }
  if(c2=='MB') {
    if(c1=='bit'){return(n/1024/1024/8)}
    else if( c2=='byte'){return(n/1024/1024)}
    else if( c2=='KB'){return(n/1024)}
    else if(  c2=='MB'){return(n)}
    else if(  c2=='GB'){return(1024*n)}
    else if(  c2=='TB'){return(1024*1024*n)}
    else { return(FALSE)}
  }
    
    if(c2=='GB') {
      if(c1=='bit'){return(n/1024/1024/1024/8)}
      else if( c2=='byte'){return(n/1024/1024/1024)}
      else if( c2=='KB'){return(n/1024/1024)}
      else if(  c2=='MB'){return(n/1024)}
      else if(  c2=='GB'){return(n)}
      else if(  c2=='TB'){return(1024*n)}
      else { return(FALSE)}
  }
    if(c2=='TB') {
      if(c1=='bit'){return(n/1024/1024/1024/8/1024)}
      else if( c2=='byte'){return(n/1024/1024/1024/1024)}
      else if( c2=='KB'){return(n/1024/1024/1024)}
      else if(  c2=='MB'){return(n/1024/1024)}
      else if(  c2=='GB'){return(n/1024)}
      else if(  c2=='TB'){return(n)}
      else { return(FALSE)}
    }
  
  else { return(FALSE)}
}

######################
s="create table 读者(姓名 char(20),所在单位 char(20),职业 rechar(20),读者号 char(20),地址 char(20),联系方式 char(15));"
#1.table 表名 (
#2.from 表名 
#*s+froms+[w[]]*.?[w[]]*.?[?(bw+)]?[rns]*
#str_extract(pattern ="\s+from\s+[w[]]*.?[w[]]*.?[?(bw+)]?[rns]*,s)
#\*\s+from\s+[\w\[\]]*\.?[\w\[\]]*\.?\[?(\b\w+)\]?[\r\n\s]*
