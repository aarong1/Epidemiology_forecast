#diff_model_eval.R----
#Using the similar basic idea as the bayesian sampling routine

out <- ifelse(param[1]>1,out <- 1,ifelse(param[1]<0,out <- 0,out <- param[1]))
sde <- param[2]
#cap <- 
Q   <- ifelse(param[3]>1,Q <- 1,ifelse(param[3]<0,Q <- 0, Q <- param[2]))
ins <- ifelse(param[4]<0,ins <- 0,param[4])
rou <- ifelse(param[5]<0,rou <- 0,param[5])
trn <- ifelse(param[6]>1,trn<- 1,ifelse(param[6]<0,trn <- 0, trn <- param[6]))
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

  set.seed(1)
out1 <- rnorm(1,mean=out,sd=1/64)
sde1 <- rnorm(1,mean=sde,sd=1/64)

Q1   <- rnorm(1,mean=sde,sd=1/64)
ins1 <- rnorm(1,mean=ins,sd=1/64)
rou1 <- rnorm(1,mean=rou,sd=1/64)
trn1 <- rnorm(1,mean=trn,sd=1/64)
inf1 <- rnorm(1,mean=inf,sd=1/64)
con1 <- rnorm(1,mean=con,sd=1/64)

dev1 <- rnorm(1,mean=dev,sd=1/64)
sev1 <- rnorm(1,mean=sev,sd=1/64)
sym1 <- rnorm(1,mean=sym,sd=1/64)
rds1 <- rnorm(1,mean=rds,sd=1/64)
fat1 <- rnorm(1,mean=fat,sd=1/64)

tft1 <- rnorm(1,mean=tft,sd=1/64)
sen1 <- rnorm(1,mean=sen,sd=1/64)
del1 <- rnorm(1,mean=del,sd=1/64)
tes1 <- rnorm(1,mean=tes,sd=1/64)

(parameter_names_short <- c(sen, tft, out, sde, trn, Q, dev, sym, sev, rds, fat, ins, inf, con, rou, tes, del))
likelihood <- list()
likelihood[1] <- cost_eq(base.parameters())
while(
for(i in parameter_names_short){
likelihood[j+i] <- cost_eq(param = c(sen, tft, out, sde, trn, Q, dev, sym, sev, rds, fat, ins, inf, con, rou, tes, del))
  ifelse(alpha <- likelihood[i+j]/likelihood[i+j-1]>1,
         sen <- sen1,
         ifelse(runif<1-alpha, #if the random no. is wihtin the prob
                sen <- sen,    #1-a the paramter is not updated.
                sen <- sen1)  ) #else it is. i.e. with a prob a
  
    
  

#ini <- param[1]
#pop <- param[2]

#plot error
#sensitivity analysis
#reintroduce the starting population parameters
#get the hessian and compute the uncertainty in parameters.
#... only valid for maximum likelihood.

safe.keeping <- function(param) {
  
  out <- ifelse(param[1]>1,out <- 1,ifelse(param[1]<0,out <- 0,out <- param[1]))
  sde <- param[2]
  #cap <- 
  Q   <- ifelse(param[3]>1,Q <- 1,ifelse(param[3]<0,Q <- 0, Q <- param[2]))
  ins <- ifelse(param[4]<0,ins <- 0,param[4])
  rou <- ifelse(param[5]<0,rou <- 0,param[5])
  trn <- ifelse(param[6]>1,trn<- 1,ifelse(param[6]<0,trn <- 0, trn <- param[6]))
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

}