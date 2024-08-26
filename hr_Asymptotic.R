hazardSE <- function(data,contr,var){

  temp <- subset(data, Combination%in%c(var,contr))
  ini <- unique(temp$treat)
  ini <- ini[!is.na(ini)]
  temp <- subset(temp,Age>ini)  

f.fit0 <- bshazard(Surv(Age,Dead)~Site, data=subset(temp, Combination==contr))
f.fit1 <- bshazard(Surv(Age,Dead)~Site, data=subset(temp, Combination==var))

b0 <- data.frame("time" = f.fit0$time, 
                 "hazard" = f.fit0$hazard, 
                 "lowCI" = f.fit0$lower.ci, 
                 "uppCI" = f.fit0$upper.ci)

b1 <- data.frame("time" = f.fit1$time, 
                 "hazard" = f.fit1$hazard, 
                 "lowCI" = f.fit1$lower.ci,
                 "uppCI" = f.fit1$upper.ci)
times <- seq(from = min(
  min(b0$time,na.rm = T),
  min(b1$time,na.rm = T)), 
  to = max(
    max(b0$time,na.rm = T),
    max(b1$time,na.rm = T)), by = 0.5)

#----------------------------------------------------------------------
#control se
bi0 <- as.data.frame(approx(x = b0$time, y = b0$hazard, xout = times))
bl0 <- as.data.frame(approx(x=b0$time, y=b0$lowCI,xout = times))
names(bl0)[2] <- 'low'
bu0 <- as.data.frame(approx(x=b0$time, y=b0$uppCI,xout = times))
names(bu0)[2] <- 'up'

bi0 <- join(bi0,bl0,by='x')
bi0 <- join(bi0,bu0,by='x')


bi0$logh <- log(bi0$y)
bi0$loglow <- log(bi0$low)
bi0$logup <- log(bi0$up)



#se
bi0$se0 <- (bi0$logup-bi0$loglow)/(1.96*2)

#----------------------------------------------------------------------
#treat se
bi1 <- as.data.frame(approx(x = b1$time, y = b1$hazard, xout = times))
bl1 <- as.data.frame(approx(x=b1$time, y=b1$lowCI,xout = times))
names(bl1)[2] <- 'low'
bu1 <- as.data.frame(approx(x=b1$time, y=b1$uppCI,xout = times))
names(bu1)[2] <- 'up'

bi1 <- join(bi1,bl1,by='x')
bi1 <- join(bi1,bu1,by='x')


bi1$logh <- log(bi1$y)
bi1$loglow <- log(bi1$low)
bi1$logup <- log(bi1$up)


#se
bi1$se1 <- (bi1$logup-bi1$loglow)/(1.96*2)

#-------------------------------------------------------
#variance
bi0 <- bi0[,c('x','y','se0')]
names(bi0) <- c('time','h0','se0')
bi1 <- bi1[,c('x','y','se1')]
names(bi1) <- c('time','h1','se1')

bi <- join(bi0,bi1,by='time')
bi$hr <- bi$h1/bi$h0
bi$loghr <- log(bi$hr)
bi$var <- (bi$se0)**2+(bi$se1)**2
#new ci
bi$low <- bi$loghr-1.96*sqrt(bi$var)
bi$up <- bi$loghr+1.96*sqrt(bi$var)
bi$name <- var

bi <- subset(bi,time>=ini)

return(bi)

}

