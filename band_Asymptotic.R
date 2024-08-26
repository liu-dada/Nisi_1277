
rm(list=ls())
set.seed( 2011 )

library(adapr)
library(plyr)
library(dplyr)
library(survival)
library(ggplot2)
library(ggeasy)
#-----------------------------------------------------------------------------
source.file <-"band_Asymptotic.R"
project.id <- "Nisi_1277"
source_info <- create_source_file_dir(source.description="Description of your program...")
#-----------------------------------------------------------------------------
# Program body here
# load median survival
med <- Load.branch('km.R/median.Rdata')

# load initial treatment age
ini <- read.csv('W:/Projects/Liu/Nisi_1277/Results/basic.R/basic_info.csv')
ini <- ini[,c(3,5:6)]
names(ini) <- c('sex','Combination','iniage')
ini$ini <- ini$iniage*30

# load last significant day
last <- Load.branch('last.Rdata')

f <- Load.branch('band_female.Rdata')
m <- Load.branch('band_male.Rdata')
f$sex <- 'f'
m$sex <- 'm'
all <- rbind(f,m)
#-------------------------------------
# find the max and min significant Mean
sig <- subset(all,up<0|low>0)
m0 <- min(sig$loghr,na.rm = T)
m1 <- max(sig$loghr,na.rm = T)

# add a max and min Mean
ma <- data.frame(time=c(0,0.5,1),
                 sex='',
                 name='',
                 
                 up=c(-1,20,21),
                 low=c(-20,1,-1),
                 loghr=c(m0,m1,1),
                 mean2=c(m0,m1,0))

#------------------------------------------------------------------------------
#female
pf <- NULL

for (i in unique(f$name)) {
  a <- subset(all,name==i&sex=='f')
  
  a <- a[!is.na(a$loghr),]
  
  #ci upper<0 or ci lower>0
  aw0 <- a
  aw0$mean2 <- ifelse(aw0$up<0|aw0$low>0,aw0$loghr,0)
  
  #start day
  st <- subset(ini,Combination==i&sex=='f')$ini
  
  #remove mean2 prior to st
  aw0$mean2 <- ifelse(aw0$time<=st,0,aw0$mean2)
  
  if(length(unique(aw0$mean2))==1)
    next
  
  df <- aw0[,c('time','sex','name','loghr','up','low','mean2')]
  
  #remove mean2 prior to st
  firstf <- min(df$time[df$mean2!=0])
  lastf <- max(df$time[df$mean2!=0])

  df <- rbind(df,ma)

  # add median survival time
  med2 <- med[[i]]
  mf <- med2[med2$strata=='Sex=f','median']

  #mark x axis
  br <- data.frame(bre=c(0,400,800,1200,1600,last[last$Combination==i,'female'],st,firstf,lastf,mf),
                   co=c(rep('black',5),'red','red',rep('black',2),'purple'))
  br <- br[order(br$bre),]
  
  pf[[i]] <- ggplot(data=df, aes(x = time, y = 3, color = mean2)) +
    
    geom_line(size = 10)  +
    scale_colour_gradient2(midpoint = 0,high = "darkred", mid = 'white', low = "seagreen") +
    theme(legend.position="none")+
    
    theme_classic()+
    scale_x_continuous(breaks = br$bre,labels = br$bre,limits = c(0,1600))+
    theme(axis.title.y=element_text(angle=0, vjust = 0.5),
          axis.text.x = element_text(colour = br$co))+
    ylab(paste0(i,' Female'))+
    easy_remove_axes(which='y',what = c("ticks",  "text", "line"))+
    ggtitle(i)

}
pf[[39]]
#png file
for (i in 1:length(pf)) {
  Graph(paste0('sefemale_v2_',i,'.png'), width = 480*8, height = 480*4, res = 72*6)
  print(pf[[i]])
  dev.off()
}

#------------------------------------------------------------------------------
#male
pm <- NULL

for (i in unique(m$name)) {
  a <- subset(all,name==i&sex=='m')

  a <- a[!is.na(a$loghr),]
  
  # ci upper<0 or ci lower>0
  aw0 <- a
  aw0$mean2 <- ifelse(aw0$up<0|aw0$low>0,aw0$loghr,0)
  
  # start day
  st <- subset(ini,Combination==i&sex=='m')$ini
  
  # remove mean2 prior to st
  aw0$mean2 <- ifelse(aw0$time<=st,0,aw0$mean2)
  
  if(length(unique(aw0$mean2))==1)
    next
 
  df <- aw0[,c('time','sex','name','loghr','up','low','mean2')]
  
  #remove mean2 prior to st
  
  
  firstf <- min(df$time[df$mean2!=0])
  lastf <- max(df$time[df$mean2!=0])
  
  df <- rbind(df,ma)
  
  #add median survival time
  med2 <- med[[i]]
  mf <- med2[med2$strata=='Sex=m','median']
  if(length(mf)==0){
    mf <- med2$median
  }

  # mark x axis
  br <- data.frame(bre=c(0,400,800,1200,1600,last[last$Combination==i,'male'],st,firstf,lastf,mf),
                   co=c(rep('black',5),'red','red',rep('black',2),'purple'))
  br <- br[order(br$bre),]
  
  pm[[i]] <- ggplot(data=df, aes(x = time, y = 3, color = mean2)) +
    
    geom_line(size = 10)  +
    scale_colour_gradient2(midpoint = 0,high = "darkred", mid = 'white', low = "seagreen") +
    theme(legend.position="none")+
    
    theme_classic()+
    scale_x_continuous(breaks = br$bre,labels = br$bre,limits = c(0,1600))+
    theme(axis.title.y=element_text(angle=0, vjust = 0.5),
          axis.text.x = element_text(colour = br$co))+
    ylab(paste0(i,' Male'))+
    easy_remove_axes(which='y',what = c("ticks",  "text", "line"))+
    ggtitle(i)

}

# png file
for (i in 1:length(pm)) {
  Graph(paste0('semale_v2_',i,'.png'), width = 480*8, height = 480*4, res = 72*6)
  print(pm[[i]])
  dev.off()
}

#--------------------------------------------------------------------------------------
# End Program Body
dependency.out <- finalize_dependency()
