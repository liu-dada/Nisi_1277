bshr <- function(sdata,var,contr){
  
  temp <- subset(sdata, X%in%c(var,contr))
  
  
  
  f <- NULL
  
 
  
  for (i in 1:500) {
    
    bt <- temp[sample(1:nrow(temp), nrow(temp), replace = T),]
    print(table(bt$Group))
    
    f.fit0 <- bshazard(Surv(y,failed)~1, data=subset(bt, X==contr))
    f.fit1 <- bshazard(Surv(y,failed)~1, data=subset(bt, X==var))
    
   
    
    
    f.bhaz0 <- data.frame("time" = f.fit0$time, 
                          "hazard" = f.fit0$hazard, 
                          "lowCI" = f.fit0$lower.ci, 
                          "uppCI" = f.fit0$upper.ci)
    
    f.bhaz1 <- data.frame("time" = f.fit1$time, 
                          "hazard" = f.fit1$hazard, 
                          "lowCI" = f.fit1$lower.ci,
                          "uppCI" = f.fit1$upper.ci)
#-----------------------------------------------------------------------------------
    #interpolate the hazard for constant intervals for direct ratio comparison
    times <- seq(from = min(
                            min(f.bhaz0$time,na.rm = T),
                            min(f.bhaz1$time,na.rm = T)), 
                 to = max(
                          max(f.bhaz0$time,na.rm = T),
                          max(f.bhaz1$time,na.rm = T)), by = 0.5)
    
    
 
    #-------
    
    #treat vs control in female
    fi.bhaz0 <- as.data.frame(approx(x = f.bhaz0$time, y = f.bhaz0$hazard, xout = times))
    
    fi.bhaz1 <- as.data.frame(approx(x = f.bhaz1$time, y = f.bhaz1$hazard, xout = times))
    
    
    f_merged <- merge(fi.bhaz0,fi.bhaz1, by = "x")
    
    names(f_merged) <- c('time','control','treat')
    
    f_merged$hr_f <- f_merged$treat/f_merged$control
    
    f_merged <- f_merged[,c('time','hr_f')]
    f_merged <- subset(f_merged,!is.na(hr_f))
    f[[i]] <- f_merged
    f[[i]]$hr_f <- log(f[[i]]$hr_f)
    
 
    
    
  }
  
  
  all <- reduce(f, full_join, by = "time")
  
  
  final <- data.frame(time=all[,1],
                      "upper" = apply(all[,-1], 1, quantile, probs = 0.975, na.rm = TRUE),
                      "lower" = apply(all[,-1], 1, quantile, probs = 0.025, na.rm = TRUE),
                      "Mean" = apply(all[,-1],1,mean,na.rm = TRUE))
  
  #wide to long
  data_long <- gather(final, stats, value, 2:4, factor_key=TRUE)
  

  
  
  return(list(long=data_long,wide=final))
  
}
