load('C:/nisi/sim500.Rdata')

out500 <- obj








#----------------
#hazard plot
#----------------------------------------------------------
#null
b <- log(300)/1200

eta <- .0001/b


ys <- DescTools::rGompertz(n = 300,shape = b,rate=b*eta)

df1 <- data.frame(X='Control',y=ys,failed=TRUE)
df1$hazard <-  DescTools::dGompertz(df1$y,shape = b,rate=b*eta)/(1-DescTools::pGompertz(df1$y,shape = b,rate=b*eta))

df2 <- data.frame(X='Treatment',y=ys,failed=TRUE)
df2$hazard <-  DescTools::dGompertz(df2$y,shape = b,rate=b*eta)/(1-DescTools::pGompertz(df2$y,shape = b,rate=b*eta))

df1 <- rbind(df1,df2)

pdf('C:/nisi/null.pdf')
ggplot(df1,aes(y,hazard,color=X))+geom_line(aes(size=X))+scale_y_log10()+
  scale_size_manual(values = c(Control = 2, Treatment = 1.))
ggplot(df1,aes(y,hazard,color=X))+geom_line(aes(size=X))+
  scale_size_manual(values = c(Control = 2, Treatment = 1.))
dev.off()

#-------------------------------------------------------------------------
#
hz <- function(data,contr,var){
bt <- data
f.fit0 <- bshazard(Surv(y,failed)~1, data=subset(bt, X==contr))
f.fit1 <- bshazard(Surv(y,failed)~1, data=subset(bt, X==var))




f.bhaz0 <- data.frame("time" = f.fit0$time, 
                      "hazard" = f.fit0$hazard, 
                      "lowCI" = f.fit0$lower.ci, 
                      "uppCI" = f.fit0$upper.ci)

f.bhaz1 <- data.frame("time" = f.fit1$time, 
                      "hazard" = f.fit1$hazard, 
                      "lowCI" = f.fit1$lower.ci,
                      "uppCI" = f.fit1$upper.ci)
#-----------------------------------------------------------------------------------
#interpolate the hazard for constant intervals for direct ratio comparison
times <- seq(from = min(
  min(f.bhaz0$time,na.rm = T),
  min(f.bhaz1$time,na.rm = T)), 
  to = max(
    max(f.bhaz0$time,na.rm = T),
    max(f.bhaz1$time,na.rm = T)), by = 0.5)

#-------
#treat vs control in female
fi.bhaz0 <- as.data.frame(approx(x = f.bhaz0$time, y = log(f.bhaz0$hazard), xout = times))

fi.bhaz1 <- as.data.frame(approx(x = f.bhaz1$time, y = log(f.bhaz1$hazard), xout = times))


f_merged <- merge(fi.bhaz0,fi.bhaz1, by = "x")


names(f_merged) <- c('time','control','treat')

f_merged$hr_f <- (f_merged$treat-f_merged$control)

#f_merged <- f_merged[,c('time','hr_f')]
f_merged <- subset(f_merged,!is.na(hr_f))

return(f_merged)


}

d0 <- out500[[1]]
t <- hz(d0,'Control','Treatment')

ggplot(t,aes(x=time,y=hr_f))+geom_line()

t <- NULL
for (i in 1:500) {
  t[[i]] <- hz(out500[[i]],'Control','Treatment')
  t[[i]]$n <- i
}

ta <- do.call(rbind.data.frame,t)



pdf('C:/nisi/null_spa.pdf')
ggplot(ta,aes(x=time,y=hr_f,group=n,color=n))+geom_line(alpha=.3)+
  geom_segment(aes(x = min(d0$y), xend = max(d0$y), y = 0, yend = 0),color='black')+
  ylab('')
dev.off()

#----------------------------------------------------------
#alter
load('C:/nisi/sim2_alternative.Rdata')

out2 <- obj
#----------------
#hazard plot
d1 <- out2[[1]]
ggplot(d1,aes(y,hazard,color=X))+geom_line()+scale_y_log10()

pdf('C:/nisi/alter.pdf')
ggplot(subset(d1,failed==TRUE),aes(y,hazard,color=X))+geom_line()+scale_y_log10()
ggplot(subset(d1,failed==TRUE),aes(y,hazard,color=X))+geom_line()
dev.off()

t2 <- NULL
for (i in 1:100) {
  t2[[i]] <- hz(out2[[i]],'Control','Treatment')
  t2[[i]]$n <- i
}

ta2 <- do.call(rbind.data.frame,t2)

#real hr
d1$log <- log(d1$hazard)
#remove censored
d1 <- subset(d1,failed==TRUE)
co <- subset(d1,X=='Control')
tr <- subset(d1,X=='Treatment')

times <- seq(from = min(d1$y), 
  to = max(d1$y), by = 0.5)

hc <- as.data.frame(approx(x = co$y, y = co$log, xout = times))
ht <- as.data.frame(approx(x = tr$y, y = tr$log, xout = times))

hct <- merge(hc,ht, by = "x")
names(hct) <- c('time','Control','Treatment')
hct$loghr <- hct$Treatment-hct$Control
ggplot()+geom_line(data=hct,aes(x=time,y=loghr))

ggplot(ta2,aes(x=time,y=control,group=n,color=n))+geom_line(alpha=.3)+
  geom_line(data=hct,aes(x=time,y=Control),inherit.aes = F)

ggplot(ta2,aes(x=time,y=treat,group=n,color=n))+geom_line(alpha=.3)+
  geom_line(data=hct,aes(x=time,y=Treatment),inherit.aes = F)

# dfout <- dfout[dfout$X=='Treatment',]
# dfout$log <- log(dfout$hazard)
# ggplot(ta2,aes(x=time,y=treat,group=n,color=n))+geom_line(alpha=.3)+
#   geom_line(data=dfout,aes(x=y,y=log),inherit.aes = F)

pdf('C:/nisi/alter_spa.pdf')
ggplot(ta2,aes(x=time,y=hr_f,group=n,color=n))+geom_line(alpha=.3)+
  geom_line(data=hct,aes(x=time,y=loghr),inherit.aes = F)+
  ylab('')
dev.off()
