

rm(list=ls())
set.seed( 2011 )

library(adapr)
library(plyr)
library(dplyr)
library(survival)
library(ggplot2)
library(bshazard)
library(scales)
library(RColorBrewer)
library(boot)
library(tidyverse)
library(tidyr)
library(foreach)
library(parallel)
library(doParallel)

source.file <-"se.R"
project.id <- "Nisi_1277"
source_info <- create_source_file_dir(source.description="Description of your program...")


# Program body here

#-----------------------------------------------------------------------------------------

survdata <- Load.branch("clean2_delete.Rdata")

#temp <- subset(survdata, Group=='NDGA'|Combination=="C2004_Control_0_")

#remove c('NDGA_lo','NDGA_mid') because no female treatment

dat2 <- subset(survdata, !(Group %in%c('NDGA_lo','NDGA_mid','17aE2_16m','17aE2_20m')))

v <- unique(dat2$Combination)


#-----------------------------------------------------------------------------------------
#female
fe <- subset(survdata,Sex=='f')

se <- NULL
for (i in unique(dat2$Cohort)) {
y <- subset(fe,Cohort==i)

v <- unique(subset(y,Group!='Control')$Combination)
ct <- unique(subset(y,Group=='Control')$Combination)

#Setup backend to use many processors
totalCores = detectCores()
#Leave one core to avoid overload your computer
cluster <- makeCluster(totalCores[1]-1) 

registerDoParallel(cluster)


se[[i]] <- foreach(i = 1:length(v),.packages = c('bshazard',
                                         'plyr','dplyr',
                                         'purrr',
                                         'tidyr','ggplot2','scales',
                                         'boot','tidyverse')) %dopar%  {
                                           hazardSE(data=y,var=v[i],contr=ct)
                                         }
#Stop cluster
stopCluster(cluster)



}


Write(se,'HRfemaleSE.Rdata')


#plot
p <- NULL
for (i in unique(dat2$Cohort)) {
  temp <- se[[i]]
  p2 <- NULL
  for (j in 1:length(temp)) {
    var <- unique(temp[[j]]$name)
    temp[[j]] <- temp[[j]][,c('time','loghr','low','up')]
    #wide to long
    t <- gather(temp[[j]], stats, value, 2:4, factor_key=TRUE)
    
    
    p2[[j]] <- ggplot(t,aes(x=time,y=value))+
      geom_line(aes(linetype=stats))+
      xlab("Age (Days)")+
      ylab(var)+
      ggtitle(var)+
      theme_classic(base_size = 20)+
      scale_linetype_manual(values=c('solid','dashed','dashed'))+
      geom_hline(yintercept=0, linetype="dashed", 
                 color = "black", size=1)+
      xlim(0,1500)
    
    
    
  }
  p <- c(p,p2)
}
p[[1]]

Graph('HRfemaleSE.pdf',width=12,height=6)
p
dev.off()





#-----------------------------------------------------------------------------------------
#male
male <- subset(survdata,Sex=='m')

se <- NULL
for (i in unique(dat2$Cohort)) {
  y <- subset(male,Cohort==i)
  
  v <- unique(subset(y,Group!='Control')$Combination)
  ct <- unique(subset(y,Group=='Control')$Combination)
  
  #Setup backend to use many processors
  totalCores = detectCores()
  #Leave one core to avoid overload your computer
  cluster <- makeCluster(totalCores[1]-1) 
  
  registerDoParallel(cluster)
  
  
  se[[i]] <- foreach(i = 1:length(v),.packages = c('bshazard',
                                                   'plyr','dplyr',
                                                   'purrr',
                                                   'tidyr','ggplot2','scales',
                                                   'boot','tidyverse')) %dopar%  {
                                                     hazardSE(data=y,var=v[i],contr=ct)
                                                   }
  #Stop cluster
  stopCluster(cluster)
  
  
  
}


Write(se,'HRmaleSE.Rdata')


#plot
p <- NULL
for (i in unique(dat2$Cohort)) {
  temp <- se[[i]]
  p2 <- NULL
  for (j in 1:length(temp)) {
    var <- unique(temp[[j]]$name)
    temp[[j]] <- temp[[j]][,c('time','loghr','low','up')]
    #wide to long
    t <- gather(temp[[j]], stats, value, 2:4, factor_key=TRUE)
    
    
    p2[[j]] <- ggplot(t,aes(x=time,y=value))+
      geom_line(aes(linetype=stats))+
      xlab("Age (Days)")+
      ylab(var)+
      ggtitle(var)+
      theme_classic(base_size = 20)+
      scale_linetype_manual(values=c('solid','dashed','dashed'))+
      geom_hline(yintercept=0, linetype="dashed", 
                 color = "black", size=1)+
      xlim(0,1500)
    
    
    
  }
  p <- c(p,p2)
}
p[[1]]

Graph('HRmaleSE.pdf',width=12,height=6)
p
dev.off()




#--------------------------------------------------------------------------------------
# End Program Body


dependency.out <- finalize_dependency()








