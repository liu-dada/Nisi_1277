
mygompertz <- function(t){
  
  b <- log(300)/1200
  
  eta <- .0001/b
  
  smedian <- (1/b)*log((1/eta)*log(2)+1)
  
  #  print(smedian)
  
  return(eta*b*exp(b*t))  
}


mygompertzTVE <- function(t){
  
  return(mygompertz(t)*exp(-(-.3 + (.6/2000)*t)))
  
}

#---------------------------------------------------
#function
simmer <- function(simnum){
  simdata0 <- sim.survdata(N=300, T=10000, hazard.fun = mygompertz,beta = 0,xvars = 1)
  simdata1 <- sim.survdata(N=150, T=10000, hazard.fun = mygompertz,beta = 0,xvars = 1)
  
  simout0 <- simdata0$data
  simout1 <- simdata1$data
  
  
  simout0$X <- 'Control'
  simout1$X <- 'Treatment'
  
  merged <- rbind(simout0,simout1)
 
  merged$simnum <- simnum
  return(merged)
  
}


simmer2 <- function(simnum,censorRate=.05){
  b <- log(300)/1200
  
  eta <- .0001/b
  
  ys <- DescTools::rGompertz(n = 300,shape = b,rate=b*eta)
  
  b2 <- log(50)/1200
  
  eta2 <- .0003/b2
  
  ys2 <- DescTools::rGompertz(n = 150,shape = b2,rate=b2*eta2)

  df1 <- data.frame(X='Control',y=ys,failed=TRUE)
  
  df1$hazard <-  DescTools::dGompertz(df1$y,shape = b,rate=b*eta)/(1-DescTools::pGompertz(df1$y,shape = b,rate=b*eta))
  
  
  df2 <- data.frame(X='Treatment',y=ys2,failed=TRUE)
  
  df2$hazard <-  DescTools::dGompertz(df2$y,shape = b2,rate=b2*eta2)/(1-DescTools::pGompertz(df2$y,shape = b2,rate=b2*eta2))
  
  
  dfout <- rbind(df1,df2)
 
  # Censor process
  censorRate=0.1
  dfout$failed <- !(rbinom(nrow(dfout),1,prob=censorRate))
  dfout$y  <- ifelse(dfout$failed,dfout$y,dfout$y*runif(nrow(dfout),0,1))
  
  dfout$simnum <- simnum
  
  dfout$y <- floor(dfout$y)
  
  return(dfout)
  
}

