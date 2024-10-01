
# Step 1, Load libraries needed.

library(survminer)
library(ggeasy)
library(bshazard)
library(plyr)
library(dplyr)
library(survival)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(readxl)

#======================================================================================================
#======================================================================================================


# Step 2: Load the function by running the script below before Step 3.

#Hazard ratio data and plot
#----------------------------------------------------------------------------
#Asymptotic
hazardSE <- function(data,contr,var){
  
  temp <- subset(data, group%in%c(var,contr))
  
  f.fit0 <- bshazard(Surv(age,dead)~1, data=subset(temp, group==contr))
  f.fit1 <- bshazard(Surv(age,dead)~1, data=subset(temp, group==var))
  
  b0 <- data.frame("time" = f.fit0$time, 
                   "hazard" = f.fit0$hazard, 
                   "lowCI" = f.fit0$lower.ci, 
                   "uppCI" = f.fit0$upper.ci)
  
  b1 <- data.frame("time" = f.fit1$time, 
                   "hazard" = f.fit1$hazard, 
                   "lowCI" = f.fit1$lower.ci,
                   "uppCI" = f.fit1$upper.ci)
  times <- seq(from = min(
    min(b0$time,na.rm = T),
    min(b1$time,na.rm = T)), 
    to = max(
      max(b0$time,na.rm = T),
      max(b1$time,na.rm = T)), by = 0.5)
  
  #----------------------------------------------------------------------
  #control se
  bi0 <- as.data.frame(approx(x = b0$time, y = b0$hazard, xout = times))
  bl0 <- as.data.frame(approx(x=b0$time, y=b0$lowCI,xout = times))
  names(bl0)[2] <- 'low'
  bu0 <- as.data.frame(approx(x=b0$time, y=b0$uppCI,xout = times))
  names(bu0)[2] <- 'up'
  
  bi0 <- join(bi0,bl0,by='x')
  bi0 <- join(bi0,bu0,by='x')
  
  
  bi0$logh <- log(bi0$y)
  bi0$loglow <- log(bi0$low)
  bi0$logup <- log(bi0$up)
  
  
  
  #se
  bi0$se0 <- (bi0$logup-bi0$loglow)/(1.96*2)
  
  #----------------------------------------------------------------------
  #treat se
  bi1 <- as.data.frame(approx(x = b1$time, y = b1$hazard, xout = times))
  bl1 <- as.data.frame(approx(x=b1$time, y=b1$lowCI,xout = times))
  names(bl1)[2] <- 'low'
  bu1 <- as.data.frame(approx(x=b1$time, y=b1$uppCI,xout = times))
  names(bu1)[2] <- 'up'
  
  bi1 <- join(bi1,bl1,by='x')
  bi1 <- join(bi1,bu1,by='x')
  
  
  bi1$logh <- log(bi1$y)
  bi1$loglow <- log(bi1$low)
  bi1$logup <- log(bi1$up)
  
  
  #se
  bi1$se1 <- (bi1$logup-bi1$loglow)/(1.96*2)
  
  #-------------------------------------------------------
  #variance
  bi0 <- bi0[,c('x','y','se0')]
  names(bi0) <- c('time','h0','se0')
  bi1 <- bi1[,c('x','y','se1')]
  names(bi1) <- c('time','h1','se1')
  
  bi <- join(bi0,bi1,by='time')
  bi$hr <- bi$h1/bi$h0
  bi$loghr <- log(bi$hr)
  bi$var <- (bi$se0)**2+(bi$se1)**2
  #new ci
  bi$low <- bi$loghr-1.96*sqrt(bi$var)
  bi$up <- bi$loghr+1.96*sqrt(bi$var)
  bi$name <- var
  
  return(bi)
  
}

#======================================================================================================
#-----------------------------------------------------------------------------------------
#bootstrap hazard ratio function
bshr <- function(data,contr,var){
  
  temp <- subset(data, group%in%c(var,contr))
  
  f <- NULL
  
  p1 <- NULL
  p2 <- NULL
  
  for (i in 1:1000) {
    
    bt <- temp[sample(1:nrow(temp), nrow(temp), replace = T),]
    #print(table(bt$Group))
    
    f.fit0 <- bshazard(Surv(age,dead)~1, data=subset(bt, group==contr))
    f.fit1 <- bshazard(Surv(age,dead)~1, data=subset(bt, group==var))
    
    
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
    times <- seq(from = min(min(f.bhaz0$time,na.rm = T),
                            min(f.bhaz1$time,na.rm = T)), 
                 to = max(max(f.bhaz0$time,na.rm = T),
                          max(f.bhaz1$time,na.rm = T)), by = 0.5)
    

    #treat vs control
    fi.bhaz0 <- as.data.frame(approx(x = f.bhaz0$time, y = f.bhaz0$hazard, xout = times))
    
    fi.bhaz1 <- as.data.frame(approx(x = f.bhaz1$time, y = f.bhaz1$hazard, xout = times))
    
    
    f_merged <- merge(fi.bhaz0,fi.bhaz1, by = "x")
    
    names(f_merged) <- c('time','control','treat')
    
    f_merged$hr_f <- f_merged$treat/f_merged$control
    
    f_merged <- f_merged[,c('time','hr_f')]
    f[[i]] <- f_merged
    f[[i]]$hr_f <- log(f[[i]]$hr_f)
 
    
  }
  
  
  all <- reduce(f, full_join, by = "time")
  
  
  final <- data.frame(time=all[,1],
                      "loghr" = apply(all[,-1],1,mean,na.rm = TRUE),
                      "up" = apply(all[,-1], 1, quantile, probs = 0.975, na.rm = TRUE),
                      "low" = apply(all[,-1], 1, quantile, probs = 0.025, na.rm = TRUE)
                      )
  
  final$name <- var
  return(final)
}
  
#=======================================================================================================
#hazard ratio plot 
hrplot <- function(data,var,lim0=0,lim1=1500){
  temp <- data[,c('time','loghr','low','up')]
  #wide to long
  t <- gather(temp, stats, value, 2:4, factor_key=TRUE)
  
  p <- ggplot(t,aes(x=time,y=value))+
    geom_line(aes(linetype=stats))+
    xlab("Age (Days)")+
    ylab('Hazard Ratio')+
    ggtitle(var)+
    theme_classic(base_size = 20)+
    scale_linetype_manual(values=c('solid','dashed','dashed'))+
    geom_hline(yintercept=0, linetype="dashed", 
               color = "black", size=1)+
    xlim(lim0,lim1)
  
  return(p)
  
}

#======================================================================================================
#hazard rate plot 
hplot <- function(data,contr,var,lim0=0,lim1=1500){
  temp <- data[,c('time','h0','h1','name')]
  #wide to long
  t <- gather(temp, stats, value, 2:3, factor_key=TRUE)
  
  p <- ggplot(t,aes(x=time,y=value))+
    geom_line(aes(color=stats))+
    xlab("Age (Days)")+
    ylab('Mortality Hazard')+
    ggtitle(var)+
    scale_y_log10()+
    theme_classic(base_size = 20)+
    scale_color_manual(values=c('black','red'),labels=c(contr,var))+
    labs(color = "Group")+
    xlim(lim0,lim1)
  
  return(p)
  
}

#======================================================================================================
#band
band_func <- function(data1,data2,lim0=0,lim1=1500,var){
  fit<- survfit(Surv(age, dead) ~ group, data = data1)
  #median
  m <- surv_median(fit)
  m$strata <- gsub('group=','',m$strata)
 
  #ci upper<0 or ci lower>0
  aw0 <- data2[!is.na(data2$loghr),]
  aw0$mean2 <- ifelse(aw0$up<0|aw0$low>0,aw0$loghr,0)
  

  if(length(unique(aw0$mean2))==1){p <- NULL}else{

    df <- aw0[,c('time','name','loghr','up','low','mean2')]
    
    #significant min and max
    firstf <- min(df$time[df$mean2!=0])
    lastf <- max(df$time[df$mean2!=0])

    #add median survival time
    mf <- m[m$strata==var,'median']
    
    
    #mark x axis
    br <- data.frame(bre=c(0,400,800,1200,1600,firstf,lastf,mf),
                     co=c(rep('black',5),rep('black',2),'purple'))
    br <- br[order(br$bre),]
    
    p <- ggplot(data=df, aes(x = time, y = 3, color = mean2)) +
      
      geom_line(size = 10)  +
      scale_colour_gradient2(midpoint = 0,high = "darkred", mid = 'white', low = "seagreen") +
      theme(legend.position="none")+
      
      theme_classic()+
      scale_x_continuous(breaks = br$bre,labels = br$bre,limits = c(lim0,lim1))+
      theme(axis.title.y=element_text(angle=0, vjust = 0.5),
            axis.text.x = element_text(colour = br$co))+
      ylab(var)+
      easy_remove_axes(which='y',what = c("ticks",  "text", "line"))+
      ggtitle(var)
  }
  return(p)
}
#======================================================================================================
#run all 

all_func <- function(data,var,contr,lim0=0,lim1=1500){
  #km plot
  fit<- survfit(Surv(age, dead) ~ group, data = data)
  km <- ggsurvplot(fit, data = data, pval = TRUE, palette = c('black', 'red'),)
  
  
  #hazard ratio plot 1
  d1 <- hazardSE(data,contr,var)  
  p1 <- hrplot(d1,var,lim0=0,lim1=1500)
  #hazard ratio plot 2
  d2 <- bshr(data,contr,var)  
  p2 <- hrplot(d2,var,lim0=0,lim1=1500)
  
  #hazard rate plot
  hp <- hplot(d1,contr,var)
  
  #band plot 1
  pb <- band_func(data1=data,data2=d1,lim0=0,lim1=1500,var = var)
  
  #band plot 2
  pb2 <- band_func(data1=data,data2=d2,lim0=0,lim1=1500,var = var)
  
  
  return(list(Asymptotic=d1,
              bootstrap=d2,
              kmplot=km,
              hrplot_asymptotic=p1,
              hrplot_bootstrap=p2,
              hazardplot=hp,
              bandplot1=pb,
              bandplot2=pb2
         ))
  
}

#======================================================================================================
#======================================================================================================

# Step 3: Input the dataset.
# Ensure that the dataset has the following column names: group, age, dead.
# Note: The case (upper or lower) of the column names is important.
# (Refer to the sample file for the correct format)
# Important: Do not change the name "t" in the script.

t <- read_excel("Sample file.xlsx") # Replace "Sample file.xlsx" with the path to your file.

# After loading the data, replace "Control" with the name of your control group in the 'group' column,
# and "GTE" with the name of your test group in the 'group' column.

all <- all_func(t, contr="Control", var="GTE")

#======================================================================================================
#======================================================================================================


# Step 4, export the results.
# Data files will be generated in the same location as the script.

# The output consists of a list with 8 elements:
# 1. Dataframe (Asymptotic Method): A dataframe with 8 columns representing:
#    time: Time point.
#    h0: Hazard rate for the control group.
#    se0: Standard error for the control group.
#    h1: Hazard rate for the treatment group.
#    se1: Standard error for the treatment group.
#    hr: Hazard ratio comparing treatment to control.
#    loghr: Logarithm of the hazard ratio.
#    var: Variance of the log hazard ratio.
#    low: Lower confidence interval of the log hazard ratio.
#    up: Upper confidence interval of the log hazard ratio.
#    name: Name of the treatment group.
write.csv(all[[1]],'hr_asymptotic.csv')

# 2. Dataframe (Bootstrap Method): A dataframe with 5 columns representing:
#    time: Time point.
#    loghr: Logarithm of the hazard ratio.
#    low: Lower confidence interval of the log hazard ratio.
#    up: Upper confidence interval of the log hazard ratio.
#    name: Name of the treatment group.
write.csv(all[[2]],'hr_bootstrap.csv')

# 3. Kaplan-Meier Curve: A Kaplan-Meier survival curve comparing treatment versus control, p value generated using a log-rank test.
png('km.png',width = 480*8, height = 480*4, res = 72*6)
print(all[[3]])
dev.off()

# 4. Hazard Ratio Plot (Asymptotic Method): A plot of the hazard ratio calculated using the asymptotic method.
png('hrplot_asymptotic.png',width = 480*8, height = 480*4, res = 72*6)
print(all[[4]])
dev.off()

# 5. Hazard Ratio Plot (Bootstrap Method): A plot of the hazard ratio calculated using the bootstrap method.
png('hrplot_bootstrap.png',width = 480*8, height = 480*4, res = 72*6)
print(all[[5]])
dev.off()

# 6. Hazard Rate Plot: A plot showing the hazard rates of both the treatment and control groups.
png('hazardplot.png',width = 480*8, height = 480*4, res = 72*6)
print(all[[6]])
dev.off()

# 7. Band Plot (Asymptotic Method): A band plot generated using the asymptotic method.
png('bandplot_asymptotic.png',width = 480*8, height = 480*4, res = 72*6)
print(all[[7]])
dev.off()

# 8. Band Plot (Bootstrap Method): A band plot generated using the bootstrap method.
png('bandplot_bootstrap.png', width = 480*8, height = 480*4, res = 72*6)
print(all[[8]])
dev.off()

# Note for Users:
# When exporting bandplots, you may notice "waves" or banding artifacts in the colors.
# To resolve this issue, you can try the following steps:

# 1. Increase the resolution of the exported image by changing "res = 72*6" to "res = 72*12".
#    This can help smooth out the color transitions.

# 2. Alternatively, run the command "all[[8]]" to generate bootstrap band, or "all[[7]]" for asymptotic band.
#    Then export the image from R studio.
#    This may also help eliminate the wave-like artifacts in the final output.
