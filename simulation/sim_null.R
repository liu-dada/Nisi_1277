
out500 <- lapply(1:500,simmer)
saveRDS(out500,'sim500.Rdata')
#===============================================================================
# bootstrap
cluster <- makeCluster(totalCores[1]-1) 
registerDoParallel(cluster)
fpr500 <- foreach(i = 1:500,.packages = c('bshazard',
                                          'plyr','dplyr',
                                          'purrr',
                                          'tidyr','ggplot2','scales',
                                          'boot','tidyverse')) %dopar%  {
                                            fpr_func(data=out500[[i]],n=i)
                                          }
#Stop cluster
stopCluster(cluster)
saveRDS(fpr500,'simdat500.Rdata')
#--------------------------------------------------
#upper
up <- NULL
for (i in 1:500) {
  
  up[[i]] <- fpr500[[i]][,c('time','f_upper')]
  
}
all_up <- reduce(up, full_join, by = "time")
mean_up <- data.frame(time=all_up[,1],
                      "Mean_up" = apply(all_up[,-1],1,mean,na.rm = TRUE))

#low
low <- NULL
for (i in 1:500) {
  
  low[[i]] <- fpr500[[i]][,c('time','f_lower')]
  
}
all_low <- reduce(low, full_join, by = "time")
mean_low <- data.frame(time=all_low[,1],
                       "Mean_low" = apply(all_low[,-1],1,mean,na.rm = TRUE))

#join
mean_ci <- join(mean_low,mean_up,by='time')
hist(mean_ci$Mean_up)
hist(mean_ci$Mean_low)

mean_ci <- mean_ci[order(mean_ci$time),]

ggplot(mean_low,aes(x=time,y=Mean_low))+
  geom_line()

ggplot(mean_up,aes(x=time,y=Mean_up))+
  geom_line()

write.csv(mean_ci,'meanCI.csv')
#===============================================================================
se500 <- NULL
for (i in 1:500) {
  se500[[i]] <- hazardSE(out500[[i]],contr='Control',var='Treatment')
}

#Setup backend to use many processors
totalCores = detectCores()

cluster <- makeCluster(totalCores[1]-1) 

registerDoParallel(cluster)


se500 <- foreach(i = 1:500,.packages = c('bshazard',
                                         'plyr','dplyr',
                                         'purrr',
                                         'tidyr','ggplot2','scales',
                                         'boot','tidyverse')) %dopar%  {
                                           hazardSE(data=out500[[i]],contr='Control',var='Treatment')
                                         }
#Stop cluster
stopCluster(cluster)
#--------------------------------------------------------------------------
#parallel computing
totalCores = detectCores()

cluster <- makeCluster(totalCores[1]-1) 

registerDoParallel(cluster)


sef500 <- foreach(i = 1:500,.packages = c('bshazard',
                                          'plyr','dplyr',
                                          'purrr',
                                          'tidyr','ggplot2','scales',
                                          'boot','tidyverse')) %dopar%  {
                                            fprSE_func(data=out500[[i]],n=i)
                                          }
#Stop cluster
stopCluster(cluster)

#---------------------------------------------------------------------------
#plot
p <- NULL
for (i in 1:500) {
  #variance
  t <- se500[[i]]
  t <- t[,c('time','loghr','low','up')]
  t$simnum <- i
  t$type <- 'var'
  #boot
  t2 <- fpr500[[i]]
  t2 <- t2[,c('time','Mean','lower','upper','simnum')]
  t2$type <- 'boot'
  
  names(t2)[2:4] <- c('loghr','low','up')
  
  compl <- rbind(t,t2)
  
  
  p[[i]] <- ggplot(compl) +
    aes(x=time,#y=loghr, 
        ymin = low, ymax = up, color = type) +
    
    geom_ribbon(alpha = 0.3) + 
    
    geom_line(aes(y = loghr))+
    theme_bw()
  
}


p[[1]]

pdf('bootvsvar.pdf')
p
dev.off()


#------------------------------------------------------------------
#
pb <- NULL
pb2 <- NULL
cou <- NULL
for (i in 1:500) {
  #variance
  t <- sef500[[i]]
  t$sum <- t$f_lower+t$f_upper
  t <- t[,c('time','loghr','low','up','sum','simnum')]
  
  t$type <- 'var'
  #boot
  t2 <- fpr500[[i]]
  t2$sum <- t2$f_lower+t2$f_upper
  t2 <- t2[,c('time','Mean','lower','upper','sum','simnum')]
  
  t2$type <- 'boot'
  
  names(t2) <- names(t)
  
  #both fpr
  tb <- intersect(subset(t,sum==1)$time,
                  subset(t2,sum==1)$time)
  
  #cou[i] <- length(tb)/nrow(t)
  cou[[i]] <- t
  cou[[i]]$f <- ifelse(t$time%in%tb,1,0)
  cou[[i]] <- cou[[i]][,c('time','f')]
  
  t0 <- rbind(t,t2)
  t0$flag <- ifelse(t0$time%in%tb,1,0)
  
  st <- subset(t0,time%in%tb)
  
  pb[[i]] <- ggplot(t0) +
    aes(x=time,#y=loghr, 
        ymin = low, ymax = up, color = interaction(type,flag)) +
    
    geom_ribbon(alpha = 0.3) + 
    
    geom_line(aes(y = loghr))+
    theme_bw()
  
  pb2[[i]] <- ggplot(t0) +
    aes(x=time,#y=loghr, 
        ymin = low, ymax = up, color=type) +
    
    geom_ribbon(alpha = 0.3) + 
    
    geom_line(aes(y = loghr))+
    theme_bw()+
    geom_ribbon(data=st,alpha = 0.5,aes(x=time,ymin = low, ymax = up)) 
  
}

pdf('bootvsvar_both.pdf')
pb
dev.off()

pdf('bootvsvar_both2.pdf')
pb2
dev.off()

#-----------------------------------------------------------------------------------
#over 500
cou_sse <- reduce(cou, full_join, by = "time")
mean_cou_sse <- data.frame(time=cou_sse[,1],
                           "Mean_s" = apply(cou_sse[,-1],1,mean,na.rm = TRUE))

ggplot(mean_cou_sse,aes(x=time,y=Mean_s))+
  geom_line()

