
library(ggplot2)
library(bshazard)
library(scales)
library(RColorBrewer)
library(boot)
library(tidyverse)
library(tidyr)
library(ggeasy)
#-----------------------------------------------------------------------------------------
load('dat_all.Rdata')
all <- obj
#------------------------------------
#female
f <- subset(all,sex=='Female')

pf <- NULL
for (i in unique(f$var)) {
  temp <- subset(f,var==i)
  ini <- unique(temp$ini)
  #remove before ini
  temp <- subset(temp,time>=ini)
  pf[[i]] <- ggplot(temp,aes(x=time,y=value))+
    geom_line(aes(linetype=stats))+
    geom_hline(yintercept=0, linetype="dashed")+
    geom_vline(xintercept=ini, linetype="dashed")+
    xlab("Age (Days)")+
    ylab("Hazard ratio Female")+
    ggtitle(i)+
    theme_classic(base_size = 20)+
    scale_linetype_manual(values=c('dashed','dashed','solid'))+
    xlim(0,1600)+
    scale_x_continuous(breaks = seq(0,1600,by=400),limits = c(0,1600))
}

pdf('female.pdf')
pf
dev.off()
#------------------------------------
#male
m <- subset(all,sex=='Male')

pm <- NULL
for (i in unique(m$var)) {
  temp <- subset(m,var==i)
  ini <- unique(temp$ini)
  #remove before ini
  temp <- subset(temp,time>=ini)
  pm[[i]] <- ggplot(temp,aes(x=time,y=value))+
    geom_line(aes(linetype=stats))+
    geom_hline(yintercept=0, linetype="dashed")+
    geom_vline(xintercept=ini, linetype="dashed")+
    xlab("Age (Days)")+
    ylab("Hazard ratio Male")+
    ggtitle(i)+
    theme_classic(base_size = 20)+
    scale_linetype_manual(values=c('dashed','dashed','solid'))+
    xlim(0,1600)+
    scale_x_continuous(breaks = seq(0,1600,by=400),limits = c(0,1600))
}

pdf('male.pdf')
pm
dev.off()

