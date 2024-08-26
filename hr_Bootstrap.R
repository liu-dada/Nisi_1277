#-----------------------------------------------------------------------------------------
#hazard ratio function
bshr <- function(sdata,var,contr){
  
  temp <- subset(sdata, Combination%in%c(var,contr))
  ini <- unique(temp$treat)
  ini <- ini[!is.na(ini)]
  temp <- subset(temp,Age>ini)
  
  t <- NULL
  m <- NULL
  f <- NULL
  
  p1 <- NULL
  p2 <- NULL
  
  for (i in 1:1000) {
    
    bt <- temp[sample(1:nrow(temp), nrow(temp), replace = T),]
    print(table(bt$Group))
    
    f.fit0 <- bshazard(Surv(Age,Dead)~Site, data=subset(bt, Sex == "f"&Combination==contr))
    f.fit1 <- bshazard(Surv(Age,Dead)~Site, data=subset(bt, Sex == "f"&Combination==var))
    
    m.fit0 <- bshazard(Surv(Age,Dead)~Site, data=subset(bt, Sex == "m"&Combination==contr))
    m.fit1 <- bshazard(Surv(Age,Dead)~Site, data=subset(bt, Sex == "m"&Combination==var))
    
    
    f.bhaz0 <- data.frame("time" = f.fit0$time, 
                          "hazard" = f.fit0$hazard, 
                          "lowCI" = f.fit0$lower.ci, 
                          "uppCI" = f.fit0$upper.ci)
    
    f.bhaz1 <- data.frame("time" = f.fit1$time, 
                          "hazard" = f.fit1$hazard, 
                          "lowCI" = f.fit1$lower.ci,
                          "uppCI" = f.fit1$upper.ci)
    
    m.bhaz0 <- data.frame("time" = m.fit0$time, 
                          "hazard" = m.fit0$hazard, 
                          "lowCI" = m.fit0$lower.ci,
                          "uppCI" = m.fit0$upper.ci)
    
    
    m.bhaz1 <- data.frame("time" = m.fit1$time, 
                          "hazard" = m.fit1$hazard, 
                          "lowCI" = m.fit1$lower.ci,
                          "uppCI" = m.fit1$upper.ci)
    
    
    
    #-----------------------------------------------------------------------------------
    #interpolate the hazard for constant intervals for direct ratio comparison
    times <- seq(from = min(min(m.bhaz0$time,na.rm = T),
                            min(m.bhaz1$time,na.rm = T),
                            min(f.bhaz0$time,na.rm = T),
                            min(f.bhaz1$time,na.rm = T)), 
                 to = max(max(m.bhaz0$time,na.rm = T),
                          max(m.bhaz1$time,na.rm = T),
                          max(f.bhaz0$time,na.rm = T),
                          max(f.bhaz1$time,na.rm = T)), by = 0.5)
    
    
    #treat vs control in male
    
    mi.bhaz0 <- as.data.frame(approx(x = m.bhaz0$time, y = m.bhaz0$hazard, xout = times))
    
    mi.bhaz1 <- as.data.frame(approx(x = m.bhaz1$time, y = m.bhaz1$hazard, xout = times))
    
    
    m_merged <- merge(mi.bhaz0,mi.bhaz1, by = "x")
    
    names(m_merged) <- c('time','control','treat')
    
    m_merged$hr_m <- m_merged$treat/m_merged$control
    
    m_merged <- m_merged[,c('time','hr_m')]
    m[[i]] <- m_merged
    m[[i]]$hr_m <- log(m[[i]]$hr_m)
    
    #-------
    
    #treat vs control in female
    fi.bhaz0 <- as.data.frame(approx(x = f.bhaz0$time, y = f.bhaz0$hazard, xout = times))
    
    fi.bhaz1 <- as.data.frame(approx(x = f.bhaz1$time, y = f.bhaz1$hazard, xout = times))
    
    
    f_merged <- merge(fi.bhaz0,fi.bhaz1, by = "x")
    
    names(f_merged) <- c('time','control','treat')
    
    f_merged$hr_f <- f_merged$treat/f_merged$control
    
    f_merged <- f_merged[,c('time','hr_f')]
    f[[i]] <- f_merged
    f[[i]]$hr_f <- log(f[[i]]$hr_f)
    
    
    
    #--------------------------------------------------------------------------------------
    #male vs female
    all <- join(m_merged,f_merged,by='time')
    
    all$hr <- log(all$hr_m/all$hr_f)
    
    t[[i]] <- all[,c('time','hr')]
    
    
    names(t[[i]])[2] <- paste0('hr',i)
    
    
  }
  
  
  all <- reduce(t, full_join, by = "time")
  
  
  final <- data.frame(time=all[,1],
                      "upper" = apply(all[,-1], 1, quantile, probs = 0.975, na.rm = TRUE),
                      "lower" = apply(all[,-1], 1, quantile, probs = 0.025, na.rm = TRUE),
                      "Mean" = apply(all[,-1],1,mean,na.rm = TRUE))
  
  #wide to long
  data_long <- gather(final, stats, value, 2:4, factor_key=TRUE)
  
  p1 <- ggplot(data_long,aes(x=time,y=value,group=stats))+
    geom_line(aes(linetype=stats))+
    xlab("Age (Days)")+
    ylab("Hazard ratio (M:F)")+
    ggtitle(var)+
    theme_classic(base_size = 20)+
    scale_linetype_manual(values=c('dashed','dashed','solid'))
  
  
  
  
  
  #-------------------------------------------------------------------------------------------
  #male plot
  
  all_m <- reduce(m, full_join, by = "time")
  
  
  final <- data.frame(time=all_m[,1],
                      "upper" = apply(all_m[,-1], 1, quantile, probs = 0.975, na.rm = TRUE),
                      "lower" = apply(all_m[,-1], 1, quantile, probs = 0.025, na.rm = TRUE),
                      "Mean" = apply(all_m[,-1],1,mean,na.rm = TRUE))
  
  #wide to long
  data_long_m <- gather(final, stats, value, 2:4, factor_key=TRUE)
  
  #-----------------------
  #female plot
  
  all_f <- reduce(f, full_join, by = "time")
  
  
  final <- data.frame(time=all_f[,1],
                      "upper" = apply(all_f[,-1], 1, quantile, probs = 0.975, na.rm = TRUE),
                      "lower" = apply(all_f[,-1], 1, quantile, probs = 0.025, na.rm = TRUE),
                      "Mean" = apply(all_f[,-1],1,mean,na.rm = TRUE))
  
  #wide to long
  data_long_f <- gather(final, stats, value, 2:4, factor_key=TRUE)
  
  
  
  
  data_long_m$sex <- 'Male'
  data_long_f$sex <- 'Female'
  mf <- rbind(data_long_m,data_long_f)
  
  
  p2 <- ggplot(mf,aes(x=time,y=value))+
    geom_line(aes(linetype=stats,color=sex))+
    xlab("Age (Days)")+
    ylab("Hazard ratio by gender")+
    ggtitle(var)+
    theme_classic(base_size = 20)+
    scale_linetype_manual(values=c('dashed','dashed','solid'))
  
  
  
  
  
  return(list(phr=p1,pmf=p2,
              dat=data_long,dmf=mf,v=var,initial=ini))
  
}
