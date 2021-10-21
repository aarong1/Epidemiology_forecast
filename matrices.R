transition_steps <- function( param, mpt, t) {
  #Transition Matrices updating marginal probabilities----
parameter_names_short <- c(out, sde, Q,ins, rou,trn,inf,con,dev,sev,sym,rds,fat,tft,sen,del,tes)


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

    #ini <- param[1]
    #pop <- param[2]
    
  #resets probability matrices for debugging
  #marginal_probability <- initialiseProbabilities(ini,pop)
    marginal_probability <- mpt[[t]]
    should.normalise <- sapply(mpt[[t]],sum)
    if(all(should.normalise== c(1,1,1,1))){
      warning('normalisation condition on the marginal probabilities not met')
    }
    
  #print(t)
  #if(t %in% c(1,2,3,4)){
  #print(marginal_probability)
  #  print(mpt[[t]])
  #  }
  #print(paste('mp1',marginal_probability))
  #if(t==1){
  (p_infected <- marginal_probability$infection['infected'])
   # }
  p_infectious <- marginal_probability$infection[['infectious']]
  p_waiting <- marginal_probability$testing[['waiting']]
  
  (Pt <- sen * sigmoid(p_waiting, tft))
  #if(t==1){
  (Pl <- out * (1 - p_infected) ^ sde)
  #}
  (Pi <- 1 - trn * p_infectious)
  if(0>Pt|Pt>1|Pl<0|Pl>1|Pi>1|Pi<0){
    warning('derived probabilities out of bounds')
    }
  
  #---------location transitions--------------------------------------------
  #transitions between location depends on clinical status
  
  #Q <- sigmoid(p_ccu, cap)
  
  (P_asympt <- matrix(c(c(1 - Pl, 1, 1, 0),
                       c(Pl  , 0, 0, 0),
                       c(0  , 0, 0, 0),
                       c(0  , 0, 0, 1)),
                     byrow = T, 4, 4))
  #if(t==1){
  (p_loc_asymp <- P_asympt %*% marginal_probability$location)
  (p_loc_asymp <- p_loc_asymp*marginal_probability$clinical['asymptomatic'])
  #}
  P_sym <- matrix(c(c(1  , 1, 1, 0),
                    c(0  , 0, 0, 0),
                    c(0  , 0, 0, 0),
                    c(0  , 0, 0, 1)),
                  byrow = T, 4, 4)
  
  (p_loc_sym <- P_sym %*% marginal_probability$location)
  (p_loc_sym <- p_loc_sym*marginal_probability$clinical['symptoms'])
  P_ards <- matrix(c(c(1 - Q, 1 - Q, 0, 0),
                     c(0,         0, 0, 0),
                     c(Q,         Q, 1, 0),
                     c(0,         0, 0, 1)), byrow = T, 4, 4)
  
  (p_loc_ards <- P_ards %*% marginal_probability$location)
  (p_loc_ards <- p_loc_ards*marginal_probability$clinical['ards'])
  P_deceased <- matrix(c(c(0, 0, 0, 0),
                         c(0, 0, 0, 0),
                         c(0, 0, 0, 0),
                         c(1, 1, 1, 1)),
                       byrow = T, 4, 4)
  
  (p_loc_deceased <- P_deceased%*%marginal_probability$location)
  (p_loc_deceased <- p_loc_deceased*marginal_probability$clinical['deceased'])
  
  (p_loc <- matrix(c( #byrow=F
    p_loc_asymp,
    p_loc_sym,
    p_loc_ards,
    p_loc_deceased),
    4,4))
  (p_loc_vector <- apply(MARGIN = 1,X = p_loc,FUN = sum))
  (p_loc_nvector <- p_loc_vector/(sum(p_loc_vector)))
  
  #----------------clinical-transition------------------------------------------------------
  #(clinical - dependent on two factors, location- ccu? and infection status)
  #REMOVED DEPENDENCE ON CCU - DIDN'T HIT CAPACITY !!!!!
  #the location is taken into account only within ccu occupancy
  #and its effect on the probabiilty for survival
  
  #clinical matrices-----------------------------
  #given that we are immune OR susceptible (infection factor)
  P_immune_suscep <- matrix(c(c(1, 1, 1, 0),
                              c(0, 0, 0, 0),
                              c(0, 0, 0, 0),
                              c(0, 0, 0, 1)),
                            byrow = T, 4, 4)
  (p_clinical_immune_suscep <- P_immune_suscep%*%marginal_probability$clinical)
  (p_clinical_suscept <- p_clinical_immune_suscep* marginal_probability$infection['suscept'])
  (p_clinical_immune <- p_clinical_immune_suscep* marginal_probability$infection['immune'])
  #given that we are infected or infectious how do we transition
  
  P_infect <-
    matrix(c(
      c(1 - dev, (1 - sym) * (1 - sev), (1 - rds) * (1 - fat), 0),
      c(dev    , sym                  , 0                    , 0),
      c(0      , sev * (1 - sym)      , rds                  , 0),
      c(0      , 0                    , (1 - rds) * fat      , 1)),
    byrow = T, 4, 4)
  
  (p_clinical_inf <- P_infect %*% marginal_probability$clinical)
  (p_clinical_infected <- p_clinical_inf* marginal_probability$infection['infected'])
  (p_clinical_infectious <- p_clinical_inf* marginal_probability$infection['infectious'])
  
  (p_clinical <- matrix(c(#byrow=F
    p_clinical_suscept,
    p_clinical_infected,
    p_clinical_infectious,
    p_clinical_immune),4,4))
  
  (p_clinical_vector <- apply(MARGIN=1,X = p_clinical,FUN = sum))
  p_clinical_nvector <- p_clinical_vector/sum(p_clinical_vector)
  
  #--------infection-transition----------------------------------------------
  #moving between different states of infection depends on location
  
  P_home <- matrix(c(
    c(Pi ^ (ins)    , 0      , 0      , 0),
    c(1 - Pi ^ (ins), inf    , 0      , 0),
    c(0             , 1 - inf, con    , 0),
    c(0             , 0      , 1 - con, 1)),
  byrow = T, 4, 4)
  
  (p_infect_home <- P_home%*%marginal_probability$infection)
  (p_infect_home <- p_infect_home*marginal_probability$location['home'])
  
  P_work <- matrix(c(
    c(Pi ^ (rou)    , 0      , 0      , 0),
    c(1 - Pi ^ (rou), inf    , 0      , 0),
    c(0            , 1 - inf, con    , 0),
    c(0            , 0      , 1 - con, 1)),
  byrow = T, 4, 4)
  
  (p_infect_work <- P_work%*%marginal_probability$infection)
  (p_infect_work <- p_infect_work*marginal_probability$location['work'])
  
  P_ccu <- matrix(c(c(1,       0, 0      , 0),
                    c(0, inf    , 0      , 0),
                    c(0, 1 - inf, con    , 0),
                    c(0, 0      , 1 - con, 1)), byrow = T, 4, 4)
  
  (p_infect_ccu <- P_ccu%*%marginal_probability$infection)
  (p_infect_ccu <- p_infect_ccu*marginal_probability$location['ccu'])
  #morgue is an absorbing state
  P_morgue <- matrix(c(c(0, 0, 0, 0),
                       c(0, 0, 0, 0),
                       c(0, 0, 0, 0),
                       c(1, 1, 1, 1)),
                     byrow = T, 4, 4)
  
  (p_infect_morgue <- P_morgue%*%marginal_probability$infection)
  (p_infect_morgue <- p_infect_morgue*marginal_probability$location['morgue'])
  
  (p_infect <- matrix(c( #byrow = F
    p_infect_home,
    p_infect_work,
    p_infect_ccu,
    p_infect_morgue),4,4))
  
  (p_infect_vector <- apply(MARGIN=1,FUN=sum,X=p_infect)) #vector of marg prob
  (p_infect_nvector <- p_infect_vector/sum(p_infect_vector))
  #---------------testing-transition-------------------------------------------------------
  #testing, only dependency is the marginal probability of waiting from the last generation.
  
  #Transition given the subject is immune OR susceptible
  (Ptest_immune_suscep <- matrix(c(
    c((1 - tes * Pt), 0      , 0, 0),
    c(tes * Pt      , del    , 0, 0),
    c(0             , 0      , 1, 0),
    c(0             , 1 - del, 0, 1)),
  byrow = T, 4, 4))
  
  (p_test_immune_suscept <- Ptest_immune_suscep%*%marginal_probability$testing)
  (p_test_immune <- p_test_immune_suscept*marginal_probability$infection['immune'])
  (p_test_suscept <- p_test_immune_suscept*marginal_probability$infection['suscept'])
  
  #transition matrix given subject is infected OR infectious
  (Ptest_infect <- matrix(c(c(1 - Pt, 0      , 0 , 0),
                           c(Pt    , del    , 0 , 0),
                           c(0    , 1 - del, 1 , 0),
                           c(0    , 0      , 0 , 1)),
                         byrow = T, 4, 4))
  
  (p_test_infect <- Ptest_infect%*%marginal_probability$testing)
  (p_test_infected <- p_test_infect*marginal_probability$infection['infected'])
  (p_test_infectious <- p_test_infect*marginal_probability$infection['infectious'])
  
  (p_test <- matrix(c( #byrow=T
    p_test_suscept,
    p_test_infected,
    p_test_infectious,
    p_test_immune),
  4,4))
  
  (p_test_vector <- apply(MARGIN=1,FUN=sum,X=p_test))
  (p_test_nvector <- p_test_vector/sum(p_test_vector))
  
#------------updating marginal_probabilities for this time step----
  (marginal_probability$infection[1:4] <- p_infect_nvector)
  marginal_probability$location[1:4] <- p_loc_nvector
  marginal_probability$clinical[1:4] <- p_clinical_nvector
  marginal_probability$testing[1:4] <- p_test_nvector
#returning the marginal probability to the cumulative data 
# storage structure, mpt (list of vectors of MPs), 
#for the current time step
  mpt[[t+1]] <- marginal_probability
  return(mpt)
}



