
out2 <- lapply(1:100,simmer2)

saveRDS(out2,'sim2_alternative.Rdata')

#------------------------------------------------------------------------------
packageList <- c("knitr",'plyr','dplyr','magrittr','purrr',"ggplot2","tidyverse","readr",'readxl',
                 "survival","gtsummary",'lubridate','survminer',
                 'devtools','coxed','bshazard',
                 'foreach','parallel','doParallel')
for(package in packageList){
  if(!require(package,character.only = TRUE)){
    install.packages(package);require(package,character.only = TRUE);}
}

library(coxed)
library(bshazard)
library(magrittr)
library(tidyverse)
library(foreach)
library(parallel)
library(doParallel)
library(survminer)
#----------------
#hazard plot
d1 <- out2[[1]]
library(ggplot2)
ggplot(d1,aes(y,hazard,color=X))+geom_line()+scale_y_log10()

pdf('alter.pdf')
ggplot(subset(d1,failed==TRUE),aes(y,hazard,color=X))+geom_line()+scale_y_log10()
ggplot(subset(d1,failed==TRUE),aes(y,hazard,color=X))+geom_line()
dev.off()

#-----------------------------------------------------------
#logrank test
p <- NULL
logrank_pic <- NULL
for (i in 1:100) {
  
  temp <- out2[[i]]
  logr <- survdiff(Surv(y,failed)~X,data=temp)
  p[i] <- logr$pvalue
  fit <- survfit(Surv(y,failed)~X,data=temp)
  logrank_pic[[i]] <- ggsurvplot(fit,data=temp,pval = T)
  
}

table(p<0.05)

pdf('logrank_pic_alter.pdf')
logrank_pic
dev.off()

#-----------------------------------------------------------------------------------
#100 simulation
#Setup backend to use many processors
totalCores = detectCores()

#1:100
#Leave one core to avoid overload your computer
cluster <- makeCluster(totalCores[1]-1) 

registerDoParallel(cluster)

sealter <- foreach(i = 1:100,.packages = c('bshazard',
                                           'plyr','dplyr',
                                           'purrr',
                                           'tidyr','ggplot2','scales',
                                           'boot','tidyverse')) %dopar%  {
                                             hazardSE(data=out2[[i]],contr='Control',var='Treatment')
                                           }
#Stop cluster
stopCluster(cluster)


#--------------------------------------------------------------------------
#parallel computing
#Setup backend to use many processors
totalCores = detectCores()

#1:100
#Leave one core to avoid overload your computer
cluster <- makeCluster(totalCores[1]-1) 

registerDoParallel(cluster)

sealterpos <- foreach(i = 1:100,.packages = c('bshazard',
                                              'plyr','dplyr',
                                              'purrr',
                                              'tidyr','ggplot2','scales',
                                              'boot','tidyverse')) %dopar%  {
                                                fprSE_func(data=out2[[i]],n=i)
                                              }
#Stop cluster
stopCluster(cluster)
#--------------------------------------------------------------------------
#bootstrap
#Setup backend to use many processors
totalCores = detectCores()

#1:100
#Leave one core to avoid overload your computer
cluster <- makeCluster(totalCores[1]-1) 

registerDoParallel(cluster)

bootalterpos <- foreach(i = 1:100,.packages = c('bshazard',
                                                'plyr','dplyr',
                                                'purrr',
                                                'tidyr','ggplot2','scales',
                                                'boot','tidyverse')) %dopar%  {
                                                  fpr_func(data=out2[[i]],n=i)
                                                }
#Stop cluster
stopCluster(cluster)
#---------------------------------------------------------------------------
#plot
p2 <- NULL
for (i in 1:100) {
  #variance
  t <- sealter[[i]]
  t <- t[,c('time','loghr','low','up')]
  t$simnum <- i
  t$type <- 'var'
  #boot
  t2 <- bootalterpos[[i]]
  t2 <- t2[,c('time','Mean','lower','upper','simnum')]
  t2$type <- 'boot'
  
  names(t2)[2:4] <- c('loghr','low','up')
  
  compl <- rbind(t,t2)
  
  
  p2[[i]] <- ggplot(compl) +
    aes(x=time, 
        ymin = low, ymax = up, color = type) +
    
    geom_ribbon(alpha = 0.3) + 
    
    geom_line(aes(y = loghr))+
    theme_bw()
  
}

pdf('bootvsvar_alter.pdf')
p2
dev.off()


