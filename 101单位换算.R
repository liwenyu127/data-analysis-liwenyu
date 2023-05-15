#序号101.  找一些常见的单位，可以实现换算的功能
#，比如，bit, byte,kb，mb等之间的换算，可以先做这一类，加上时间单位的换算
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
