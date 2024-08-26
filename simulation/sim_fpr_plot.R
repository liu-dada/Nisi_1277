#=====================================================================================
#null
#bootstrap

s <- NULL
for (i in 1:500) {
  
  t <- fpr500[[i]][,c('time','f_upper','f_lower')]
  t$sum <- t$f_upper+t$f_lower
  s[[i]] <- t[,c('time','sum')]
  
}
all_s <- reduce(s, full_join, by = "time")
mean_s <- data.frame(time=all_s[,1],
                      "Mean_s" = apply(all_s[,-1],1,mean,na.rm = TRUE))
write.csv(mean_s,'meanCIsum.csv')


ggplot(mean_s,aes(x=time,y=Mean_s))+
  geom_line()

#----------------
s <- NULL
for (i in 1:100) {
  
  t <- fpr_dat[[i]][,c('time','f_upper','f_lower')]
  t$sum <- t$f_upper+t$f_lower
  s[[i]] <- t[,c('time','sum')]
  
}
all_s <- reduce(s, full_join, by = "time")
mean_s <- data.frame(time=all_s[,1],
                     "Mean_s" = apply(all_s[,-1],1,mean,na.rm = TRUE))


ggplot(mean_s,aes(x=time,y=Mean_s))+
  geom_line()
#-----------------------------------------------------------------------------------
#variance
sse <- NULL
for (i in 1:500) {
  
  t <- sef500[[i]][,c('time','f_upper','f_lower')]
  t$sum <- t$f_upper+t$f_lower
  sse[[i]] <- t[,c('time','sum')]
  
}
all_sse <- reduce(sse, full_join, by = "time")
mean_sse <- data.frame(time=all_sse[,1],
                     "Mean_s" = apply(all_sse[,-1],1,mean,na.rm = TRUE))

write.csv(mean_sse,'meanCIsumSE.csv')


ggplot(mean_sse,aes(x=time,y=Mean_s))+
  geom_line()

#=====================================================================================
#alternative
#bootstrap
s <- NULL
for (i in 1:100) {
  
  t <- bootalterpos[[i]][,c('time','f_upper','f_lower')]
  t$sum <- t$f_upper+t$f_lower
  s[[i]] <- t[,c('time','sum')]
  
}
all_s <- reduce(s, full_join, by = "time")
mean_s <- data.frame(time=all_s[,1],
                     "Mean_s" = apply(all_s[,-1],1,mean,na.rm = TRUE))
write.csv(mean_s,'meanCIsum_alter_boot.csv')

pdf('alter_pr_boot.pdf',width = 8,height = 6)

ggplot(mean_s,aes(x=time,y=Mean_s))+
  geom_line()
dev.off()
#--------------------------------------------------
#upper
up <- NULL
for (i in 1:100) {
  
  up[[i]] <- bootalterpos[[i]][,c('time','f_upper')]
  
}
all_up <- reduce(up, full_join, by = "time")
mean_up <- data.frame(time=all_up[,1],
                      "Mean_up" = apply(all_up[,-1],1,mean,na.rm = TRUE))

#low
low <- NULL
for (i in 1:100) {
  
  low[[i]] <- bootalterpos[[i]][,c('time','f_lower')]
  
}
all_low <- reduce(low, full_join, by = "time")
mean_low <- data.frame(time=all_low[,1],
                       "Mean_low" = apply(all_low[,-1],1,mean,na.rm = TRUE))

#together
mean_s$name <- 'sum'
names(mean_s)[2] <- 'value'
mean_up$name <- 'up'
names(mean_up)[2] <- 'value'
mean_low$name <- 'low'
names(mean_low)[2] <- 'value'
mean_all <- rbind(mean_s,mean_up,mean_low)

pdf('alter_pr_boot_color.pdf',width = 8,height = 6)
ggplot(mean_all,aes(x=time,y=value,group=name,color=name,alpha=name))+
  geom_line(aes(size=name,alpha=name))+
  scale_size_manual(values = c(1,3,1)) + 
  scale_colour_manual(values = c("green","black", "red"))+
  scale_alpha_manual(values=c(1,0.3,1)) +
  geom_hline(yintercept=0.05, linetype='dashed', color='black', size=1)+
  geom_hline(yintercept=0.16, linetype='dashed', color='black', size=1)+
  annotate("text", x=280, y=0.25, label= "logrank test power")
dev.off()

#--------------------------------------------------------------
#variance
sse <- NULL
for (i in 1:100) {
  
  t <- sealterpos[[i]][,c('time','f_upper','f_lower')]
  t$sum <- t$f_upper+t$f_lower
  sse[[i]] <- t[,c('time','sum')]
  
}
all_sse <- reduce(sse, full_join, by = "time")
mean_sse <- data.frame(time=all_sse[,1],
                       "Mean_s" = apply(all_sse[,-1],1,mean,na.rm = TRUE))
write.csv(mean_sse,'meanCIsum_alter_SE.csv')


pdf('alter_pr_SE.pdf',width = 8,height = 6)
ggplot(mean_sse,aes(x=time,y=Mean_s))+
  geom_line()
dev.off()

#--------------------------------------------------
#upper
up <- NULL
for (i in 1:100) {
  
  up[[i]] <- sealterpos[[i]][,c('time','f_upper')]
  
}
all_up <- reduce(up, full_join, by = "time")
mean_up <- data.frame(time=all_up[,1],
                      "Mean_up" = apply(all_up[,-1],1,mean,na.rm = TRUE))

#low
low <- NULL
for (i in 1:100) {
  
  low[[i]] <- sealterpos[[i]][,c('time','f_lower')]
  
}
all_low <- reduce(low, full_join, by = "time")
mean_low <- data.frame(time=all_low[,1],
                       "Mean_low" = apply(all_low[,-1],1,mean,na.rm = TRUE))

#together
mean_sse$name <- 'sum'
names(mean_sse)[2] <- 'value'
mean_up$name <- 'up'
names(mean_up)[2] <- 'value'
mean_low$name <- 'low'
names(mean_low)[2] <- 'value'
mean_all <- rbind(mean_sse,mean_up,mean_low)

pdf('alter_pr_SE_color.pdf',width = 8,height = 6)
ggplot(mean_all,aes(x=time,y=value,group=name,color=name,alpha=name))+
  geom_line(aes(size=name,alpha=name))+
  scale_size_manual(values = c(1,3,1)) + 
  scale_colour_manual(values = c("green","black", "red"))+
  scale_alpha_manual(values=c(1,0.3,1)) +
  geom_hline(yintercept=0.05, linetype='dashed', color='black', size=1)+
  geom_hline(yintercept=0.16, linetype='dashed', color='black', size=1)+
  annotate("text", x=280, y=0.25, label= "logrank test power")
dev.off()
