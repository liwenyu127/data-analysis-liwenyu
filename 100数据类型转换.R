#numeric
#character
#logical
#factor
#x为待转数据
#y为传入的要转换的目标数据类型
#如果是不支持的格式转换则输出NA
data.transm<-function(x,y){
a<-switch (y,
"numeric"=as.numeric(x),
"character"=as.character(x),
"logical"=as.logical(x),
"factor"=as.factor(x)
)
return(a)
}
