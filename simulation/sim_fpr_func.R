#function 
fpr_func <- function(data,n){
  
  r1 <- bshr(data,var='Treatment','Control')
  
  along <- r1[['long']]
  awide <- r1[['wide']]
  
  awide <- awide[!is.na(awide$Mean),]
  
  #p[[i]] <- ggplot(along,aes(x=time,y=value,group=stats,color=stats))+geom_line()
  
  #false <- awide[awide$upper<=0|awide$lower>=0,]
  
  #fpr <- c(fpr,nrow(false)/nrow(awide))
  
  awide$f_upper <- ifelse(awide$upper<0,1,0)
  awide$f_lower <- ifelse(awide$lower>0,1,0)
  awide$simnum <- n
  return(awide)
  
}
 

#function 
fprSE_func <- function(data,n){
  
  awide <- hazardSE(data,var='Treatment',contr = 'Control')
  
  awide <- awide[!is.na(awide$loghr),]

  
  awide$f_upper <- ifelse(awide$up<0,1,0)
  awide$f_lower <- ifelse(awide$low>0,1,0)
  awide$simnum <- n
  return(awide)
  
}