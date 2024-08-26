
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

source.file <-"bootvsse.R"
project.id <- "Nisi_1277"
source_info <- create_source_file_dir(source.description="Description of your program...")


# Program body here

#-----------------------------------------------------------------------------------------

#=========================================================================================
#boot from mf.R
boot <- Load.branch('dall.Rdata')


d <- NULL
for (i in 1:15) {
  d[[i]] <- Reduce(full_join,boot[[i]])
}

all <- Reduce(full_join,d)
length(unique(all$var))

#------------------------------------
#female
f <- subset(all,sex=='Female')

#male
m <- subset(all,sex=='Male')

#=========================================================================================
#SE from se.R
#--------------------------------------------------------------------------------------
#female
se <- Load.branch('HRfemaleSE.Rdata')

sef <- NULL
for (i in 1:13) {
  temp <- se[[i]]
  sef[[i]] <- do.call(rbind.data.frame,temp)
}

sef <- do.call(rbind.data.frame,sef)
sef <- sef[,c('time','loghr','low','up','name')]
#wide to long
sef <- gather(sef, stats, value, 2:4, factor_key=TRUE)

pf <- NULL
for (i in unique(f$var)) {
  temp <- subset(f,var==i)
  ini <- unique(temp$ini)
  #remove before ini
  temp <- subset(temp,time>=ini)
  
  temp2 <- subset(sef,name==i)
  
 
  
  pf[[i]] <- ggplot()+
    geom_line(data=temp,aes(x=time,y=value,linetype=stats,color='green'))+
    geom_hline(yintercept=0, linetype="dashed")+
    geom_vline(xintercept=ini, linetype="dashed")+
    xlab("Age (Days)")+
    ylab("Hazard ratio Female")+
    ggtitle(i)+
    theme_classic(base_size = 20)+
    
    xlim(0,1600)+
    scale_x_continuous(breaks = seq(0,1600,by=400),limits = c(0,1600))+
    geom_line(data=temp2,inherit.aes = F,aes(x=time,y=value,linetype=stats,color='red'))+
    scale_linetype_manual(values=rep(c('solid','dashed','dashed'),2))+
    theme(legend.position="none")
}

Graph('HRfemale_boot_SE.pdf',width=12,height=6)
pf
dev.off()
#--------------------------------------------------------------------------------------
#male
se <- Load.branch('HRmaleSE.Rdata')

sef <- NULL
for (i in 1:13) {
  temp <- se[[i]]
  sef[[i]] <- do.call(rbind.data.frame,temp)
}

sef <- do.call(rbind.data.frame,sef)
sef <- sef[,c('time','loghr','low','up','name')]
#wide to long
sef <- gather(sef, stats, value, 2:4, factor_key=TRUE)

pm <- NULL
for (i in unique(m$var)) {
  temp <- subset(m,var==i)
  ini <- unique(temp$ini)
  #remove before ini
  temp <- subset(temp,time>=ini)
  
  temp2 <- subset(sef,name==i)
  
  
  
  pm[[i]] <- ggplot()+
    geom_line(data=temp,aes(x=time,y=value,linetype=stats,color='green'))+
    geom_hline(yintercept=0, linetype="dashed")+
    geom_vline(xintercept=ini, linetype="dashed")+
    xlab("Age (Days)")+
    ylab("Hazard ratio Male")+
    ggtitle(i)+
    theme_classic(base_size = 20)+
    
    xlim(0,1600)+
    scale_x_continuous(breaks = seq(0,1600,by=400),limits = c(0,1600))+
    geom_line(data=temp2,inherit.aes = F,aes(x=time,y=value,linetype=stats,color='red'))+
    scale_linetype_manual(values=rep(c('solid','dashed','dashed'),2))+
    theme(legend.position="none")
}

Graph('HRmale_boot_SE.pdf',width=12,height=6)
pm
dev.off()













#--------------------------------------------------------------------------------------
# End Program Body


dependency.out <- finalize_dependency()




