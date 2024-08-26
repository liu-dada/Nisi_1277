rm(list=ls())
set.seed( 2011 )

library(adapr)
library(plyr)
library(dplyr)
library(survival)
library(ggplot2)
library(ggeasy)
#-----------------------------------------------------------------------------
source.file <-"band_Bootstrap.R"
project.id <- "Nisi_1277"
source_info <- create_source_file_dir(source.description="Description of your program...")
#-----------------------------------------------------------------------------
# load median survival
med <- Load.branch('km.R/median.Rdata')

dall <- Load.branch('dat_all.Rdata')
#-----------------------------------------------------------------------------------------
# use year 2004
d1 <- dall[[1]]
# load last day
last <- Load.branch('last04.Rdata')

# find the max and min significant Mean
ss1 <- llply(d1,function(x) {
  wid <- spread(x, key = stats, value = value)
  wid <- subset(wid,upper<0|lower>0)
  m0 <- min(wid$Mean,na.rm = T)
  m1 <- max(wid$Mean,na.rm = T)
  return(data.frame(min=m0,max=m1))
})

ss1 <- do.call(rbind, ss1)

ss <- NULL
for (i in 1:13) {
  ss1 <- llply(dall[[i]],function(x) {
    wid <- spread(x, key = stats, value = value)
    wid <- subset(wid,upper<0|lower>0)
    m0 <- min(wid$Mean,na.rm = T)
    m1 <- max(wid$Mean,na.rm = T)
    return(data.frame(year=i,var=unique(x$var),min=m0,max=m1))
  })
  
  ss[[i]] <- do.call(rbind, ss1)
}

ss <- do.call(rbind, ss)

m0 <- min(ss$min)
m1 <- max(ss$max)

# add a max and min Mean
ma <- data.frame(time=c(0,0.5,1),
                 sex='Female',
                 var='',
                 ini=120,
                 upper=c(-1,20,21),
                 lower=c(-20,1,-1),
                 Mean=c(m0,m1,1),
                 mean2=c(m0,m1,0))

pf <- NULL
pm <- NULL
for (i in 1:length(d1)) {
  a <- d1[[i]]
  var <- unique(a$var)
  
  a <- a[!is.na(a$value),]
  
  #ci upper<0 or ci lower>0
  aw <- spread(a, key = stats, value = value)
  aw0 <- aw
  aw0$mean2 <- ifelse(aw0$upper<0|aw0$lower>0,aw0$Mean,0)
  
  #start day
  st <- unique(aw0$ini)
  
  #remove mean2 prior to st
  aw0$mean2 <- ifelse(aw0$time<=st,0,aw0$mean2)
  
  if(length(unique(aw0$mean2))==1)
    next
    
  f <- subset(aw0,sex=='Female')
  m <- subset(aw0,sex=='Male')
  
  # remove mean2 prior to st
    
  firstf <- min(f$time[f$mean2!=0])
  lastf <- max(f$time[f$mean2!=0])
  
  firstm <- min(m$time[m$mean2!=0])
  lastm <- max(m$time[m$mean2!=0])
  
  f <- rbind(f,ma)
  m <- rbind(m,ma)
  
  #add median survival time
  med2 <- med[[var]]
  mf <- med2[med2$strata=='Sex=f','median']
  mm <- med2[med2$strata=='Sex=m','median']
  
  # mark x axis
  br <- data.frame(bre=c(0,400,800,1200,1600,last[i,'female'],st,firstf,lastf,mf),
                   co=c(rep('black',5),'red','red',rep('black',2),'purple'))
  br <- br[order(br$bre),]
  
  pf[[i]] <- ggplot(data=f, aes(x = time, y = 3, color = mean2)) +
    
    geom_line(size = 10)  +
    scale_colour_gradient2(midpoint = 0,high = "darkred", mid = 'white', low = "seagreen") +
    theme(legend.position="none")+
    xlim(0,1500)+
    theme_classic()+
    scale_x_continuous(breaks = br$bre,labels = br$bre,limits = c(0,1600))+
    theme(axis.title.y=element_text(angle=0, vjust = 0.5),
          axis.text.x = element_text(colour = br$co))+
    ylab(paste0(unique(a$var),' Female'))+
    easy_remove_axes(which='y',what = c("ticks",  "text", "line"))
  
  br <- data.frame(bre=c(0,400,800,1200,1600,last[i,'male'],st,firstm,lastm,mm),
                   co=c(rep('black',5),'red','red',rep('black',2),'purple'))
  br <- br[order(br$bre),]
  
  pm[[i]] <- ggplot(data=m, aes(x = time, y = 3, color = mean2)) +
    
    geom_line(size = 10)  +
    scale_colour_gradient2(midpoint = 0,high = "darkred", mid = 'white', low = "seagreen") +
    theme(legend.position="none")+
    xlim(0,1600)+
    theme_classic()+
    scale_x_continuous(breaks = br$bre,labels = br$bre,limits = c(0,1600))+
    theme(axis.title.y=element_text(angle=0, vjust = 0.5),
          axis.text.x = element_text(colour = br$co))+
    ylab(paste0(unique(a$var),' Male'))+
    easy_remove_axes(which='y',what = c("ticks",  "text", "line"))

}

# png file
for (i in 1:length(pf)) {
  Graph(paste0('2004female_v2_',i,'.png'), width = 480*8, height = 480*4, res = 72*6)
  print(pf[[i]])
  dev.off()
}

for (i in 1:length(pm)) {
  Graph(paste0('2004male_v2_',i,'.png'), width = 480*8, height = 480*4, res = 72*6)
  print(pm[[i]])
  dev.off()
}

#--------------------------------------------------------------------------------------
# End Program Body
dependency.out <- finalize_dependency()
