#dcm_transition.R----
#This generates the timeseries data, initial and randomly sampled initial parameters
# as well as optimisation routines that call upon the above functions
require(tidyverse)
require(minpack.lm)
require(FME)

require(cowplot)
require(smoother)
require(rstan)

require(lattice)
require(viridis)


parameter_names_short <- c(out, sde, ins, rou,trn,inf,con,dev,sev,sym,rds,fat,tft,sen,del,tes)

parameter_names_FTTI <- c(out, sde, Q,ins, rou,trn,inf,con,dev,sev,sym,rds,fat,tft,sen,del,tes,ttt,exp,bas)

#calling the matrices.R file through dcm_transition function====
dcm_transition <- function(time, param,forecast.forward=F,...) {
  
  forecast.list <- list(...)
  #print(forecast.list)
  duration <- forecast.list$duration
  
   #time - set to duration of time series estimating against
  #ini <- param[1]
  #pop <- param[2]
  
  if(forecast.forward==F){
  mpt <-list()
  mpt[[1]] <- initialiseProbabilities()
  t0 <- 1
  }
  else{
    t0 <- time
    time <- time+duration
  mpt <- forecast.list$mp
  }
    #print(t0)
    #print(time)
  for( t in t0:time){
    #print(t)
    #increment time +1 and update states by transition probabilities
   mpt <- transition_steps(param, mpt, t)
   }
  return(mpt)
}

predicted.onward <- dcm_transition(time=length(est_data)-1,param= mc.param,forecast.forward=T,mp=pt.mc.1,duration=100)
plot(as.numeric(flatten(flatten(predicted.onward)[seq(from=3,length(flatten(predicted.onward)),by=4)])[seq(from=4,length(flatten(predicted.onward)),by=4)]))

#cost_equation----
cost_eq <- function(param){ 

  #expect 17 parameters in total
  #data starts on the 01.03.20 - that is, 1st march

  mpt <- dcm_transition(time =length(est_data3)-1 , param)
  #generating the time series over parameters.

  #POSITIVE TESTS
  (gen <- t(as.data.frame(flatten(mpt)[seq(from=4, to=length(flatten(mpt)),by=4)])))
  (gen <- as.data.frame(gen))
  (y1 <- gen$positive) #len:91

  #DEATHS
  (gen <- t(as.data.frame(flatten(mpt)[seq(from=3, to=length(flatten(mpt)),by=4)])))
  (gen <- as.data.frame(gen))
  (y2<- gen$deceased) #len:91

  #est_data is the CUMULATIVE positive test count
  #est_data3 is the CUMULATIVE deaths count

  #(z1 <- sqrt(abs((y1*1.9*10^6)**2 - est_data**2 ))) including +ve tests
  (z2 <- abs((y2*1.9*10^6)- (est_data3 )))  #/(0.25*10^6)#  #LEAST SQUARES COST FUNCTION NOT EFFECTIVE
  print(z_sum <- sum(z2))
  return(z_sum)
  
}

hold <- function(Oparam6, cost_eq, residuals) {
  #runnning the optimiser ----
  Oparam6_1 <- optim(control = list(
    maxit=2000,reltol=10^(-9)),
    par=Oparam6$par,
    fn = cost_eq)
  
  nlsparam1 <- nls.lm(par = base.parameters(),
                      fn=residuals,
                      #control=list(maxiter=2000),
                      upper =c(out=1,sde=8,Q=1,ins=10,rou=50,trn=1,inf=5,con=5,dev=1,sev=1,sym=5,rds=5,
                               fat=1,tft=5,sen=5,del=5,tes=1),
                      lower = c(out=0,sde=0,Q=0,ins=0,rou=0,trn=0,inf=0,con=0,dev=0,sev=0,sym=0,rds=0,
                                fat=0,tft=0,sen=0,del=0,tes=0)
  )
  
  #modFit()
  #MCMC <- modMCMC(f = NN, p = 0:2, niter = 5000, updatecov = 100,
  #mdMCMC+                  outputlength = 1000, jump = 0.5)
  MCparam <- modMCMC(f=cost_eq,
        p = base.parameters.random.sample(),
        niter=50000,outputlength=5,
        upper =c(out=1,sde=8,Q=1,ins=10,rou=50,trn=1,
                               inf=5,con=5,dev=1,sev=1,sym=5,rds=5,
                               fat=1,tft=5,sen=5,del=5,tes=1),
        lower = c(out=0,sde=-8,Q=0,ins=0,rou=0,trn=0,
                                inf=-5,con=-5,dev=0,sev=0,sym=-5,rds=-5,
                                fat=0,tft=-5,sen=-5,del=-5,tes=0)
  )
}


base.parameters <- function() {
  par = c(
    #ini=4/(1.9*10^6),    #ini
    #pop=1-4/(1.9*10^6),  #pop
    #no thetaM    
    
    out=0.17,             #out
    sde=1.6,               #sde
    Q=0.8,             #Q
    #Q instead of ccu capacity
    
    ins=7,      #ins
    rou=20,     #rou
    trn=0.19,  #trn   
    inf=0.8561758,
    con=0.7116735,
    
    dev=0.47,
    sev=1.7 / 100,
    sym = 0.8187308,
    rds= 0.9200444,
    fat=0.48,
    #no sur - simplification see the 'testing' factor
    #transition matrix comments
    
    tft=500 / (10 ^ 5),
    sen=1 / 100,
    del=0.6095407,
    tes=1 / 8)
  return(par)
}
  base.parameters.random.sample <- function() {
    
    #ini=4/(1.9*10^6),    #ini
    #pop=1-4/(1.9*10^6),  #pop
    #no thetaM
    out  <-  0.17;sde <- 1.6;Q <- 0.8;ins <- 7;rou <- 20; trn <- 0.19
    inf <- 0.8561758;con <- 0.7116735; dev <- 0.47;sev <- 1.7 / 100
    sym <- 0.8187308;rds <- 0.9200444;fat <- 0.48;
    #no sur - simplification see the 'testing' factor
    #transition matrix comments
    tft <- 500 / (10 ^ 5);sen <- 1 / 100;del <- 0.6095407;tes <- 1 / 8
    
    #set.seed(1)
    out <- rnorm(1,mean=out,sd=1/64)
    sde <- rnorm(1,mean=sde,sd=1/64)
    
    Q   <- rnorm(1,mean=sde,sd=1/64)
    ins <- rnorm(1,mean=ins,sd=1/64)
    rou <- rnorm(1,mean=rou,sd=1/64)
    trn <- rnorm(1,mean=trn,sd=1/64)
    inf <- rnorm(1,mean=inf,sd=1/64)
    con <- rnorm(1,mean=con,sd=1/64)
    
    dev <- rnorm(1,mean=dev,sd=1/64)
    sev <- rnorm(1,mean=sev,sd=1/64)
    sym <- rnorm(1,mean=sym,sd=1/64)
    rds <- rnorm(1,mean=rds,sd=1/64)
    fat <- rnorm(1,mean=fat,sd=1/64)
    
    tft <- rnorm(1,mean=tft,sd=1/64)
    sen <- rnorm(1,mean=sen,sd=1/64)
    del <- rnorm(1,mean=del,sd=1/64)
    tes <- rnorm(1,mean=tes,sd=1/64) 
    par <- c(out, sde, Q, ins, rou,trn,inf,con,dev,sev,sym,rds,fat,tft,sen,del,tes)
    par <- check.constraints(par)
    
    return(par)
    
  }
residuals <- function(param){ 

  #expect 17 parameters in total
  #data starts on the 01.03.20 - that is, 1st march

  mpt <- dcm_transition(time =length(est_data3)-1 , param)
  #generating the time series over parameters.

  #DEATHS
  (gen <- t(as.data.frame(flatten(mpt)[seq(from=3, to=length(flatten(mpt)),by=4)])))
  (gen <- as.data.frame(gen))
  (y2<- gen$deceased) #len:91

  #est_data is the CUMULATIVE positive test count
  #est_data3 is the CUMULATIVE deaths count

  #(z1 <- sqrt(abs((y1*1.9*10^6)**2 - est_data**2 ))) including +ve tests
  (z2 <- abs((y2*1.9*10^6)- (est_data3 )))  #/(0.25*10^6)#  #LEAST SQUARES COST FUNCTION NOT EFFECTIVE
  print(sum(z2))
  return(z2)
}

check.constraints <- function(param) {
  out <- ifelse(param[1]>1,out <- 1,ifelse(param[1]<0,out <- 0,out <- param[1]))
  sde <- param[2]
  #cap <- 
  Q   <- ifelse(param[3]>1,Q <- 1,ifelse(param[3]<0,Q <- 0, Q <- param[2]))
  ins <- ifelse(param[4]<0,ins <- 0,param[4])
  rou <- ifelse(param[5]<0,rou <- 0,param[5])
  trn <- ifelse(param[6]>1,trn <- 1,ifelse(param[6]<0,trn <- 0, trn <- param[6]))
  inf <- ifelse(param[7]<0,0,inf <- param[7])
  con <- ifelse(param[8]<0,0,con <- param[8])
  
  dev <- ifelse(param[9]<0,0,ifelse(param[9]>1,1,dev <- param[9]))
  sev <- ifelse(param[10]<0,0,ifelse(param[10]>1,1,sev <- param[10]))
  sym <- ifelse(param[11]<0,0,sym <- param[11])
  rds <- ifelse(param[12]<0,0,rds <- param[12])
  fat <- ifelse(param[13]<0,0,ifelse(param[13]>1,1,fat <- param[13]))
  #sur <- param[]
  
  tft <- ifelse(param[14]<0,0,tft <- param[14])
  sen <- ifelse(param[15]<0,0,sen <- param[15])
  del <- ifelse(param[16]<0,0,del <- param[16])
  tes <- ifelse(param[17]<0,0,ifelse(param[17]>1,1,tes <- param[17]))
  
  par <- c(out, sde, Q, ins, rou, trn, inf, con,dev,sev,sym,rds,fat,tft,sen,del,tes)
  return(par)
}

foo <- function(n, ...){
  print(...elt(n))
  print(list(...))
  print(...length())
  print(..1)
  print(list(...)$all)
  }
foo(n = 2, "a", "b",all= "c")

