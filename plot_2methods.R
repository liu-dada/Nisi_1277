
library(ggplot2)
#-----------------------------------------------------------------------------------------
#boot from mf.R
boot <- Load.branch('dat_all.Rdata')
all <- obj
#------------------------------------
#female
f <- subset(all,sex=='Female')

#male
m <- subset(all,sex=='Male')
#=========================================================================================
#SE from se.R
#female
load('HRfemaleSE.Rdata')
se <- obj
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

pdf('HRfemale_boot_SE.pdf',width=12,height=6)
pf
dev.off()
#--------------------------------------------------------------------------------------
#male
load('HRmaleSE.Rdata')
se <- obj
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

pdf('HRmale_boot_SE.pdf',width=12,height=6)
pm
dev.off()
