
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
library(ggeasy)

source.file <-"mf.R"
project.id <- "Nisi_1277"
source_info <- create_source_file_dir(source.description="Description of your program...")


# Program body here

#-----------------------------------------------------------------------------------------
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2004.Rdata')
d1 <- obj
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2005.Rdata')
d2 <- obj
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2006.Rdata')
d3 <- obj
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2007.Rdata')
d4 <- obj
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2009.Rdata')
d5 <- ds
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2010.Rdata')
d6 <- ds
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2010_2.Rdata')
d6_2 <- ds2
d6_2 <- llply(d6_2,function(x){
  x$sex <- 'Male'
  return(x)})
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2011.Rdata')
d7 <- ds
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2012.Rdata')
d8 <- ds
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2013.Rdata')
d9 <- ds
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2014.Rdata')
d10 <- ds
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2015.Rdata')
d11 <- ds
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2016.Rdata')
d12 <- ds
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2016_2.Rdata')
d12_2 <- ds2
d12_2 <- llply(d12_2,function(x){
  x$sex <- 'Male'
  return(x)})
load('W:/Projects/Liu/Nisi_1277/Results/bootstrap_func.R/d2017.Rdata')
d13 <- ds

dall <- list(d1,d2,d3,d4,d5,d6,d6_2,d7,d8,d9,d10,d11,d12,d12_2,d13)

Write(dall,'dall.Rdata')

d <- NULL
for (i in 1:15) {
  d[[i]] <- Reduce(full_join,dall[[i]])
}

all <- Reduce(full_join,d)
length(unique(all$var))

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

Graph('female.pdf')
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

Graph('male.pdf')
pm
dev.off()
#--------------------------------------------------------------------------------------
# End Program Body


dependency.out <- finalize_dependency()

