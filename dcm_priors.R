#DCM_priors.R
#specifying the transition matrices for movement between states.
#transform for time constants
time_param <- function(t){
  #transforms time to our scale parameter; unitless
  y <- exp(-1/t)
  return(y)}
#parameters we need

initial_cases <- ini <- 4
#prop_resistant_cases <- res <- 10^(-6)
pop_size <- pop <- 1.9*10**6

#location state----
#prob_staying_in <- ins<- 1-out
prob_going_out <- out <-0.17
social_distance <- sde <- 1.6
#CCU_cap <- cap <- 
prop_ards_toCCU <- Q <- 0.8

#Infection state----
num_contact_home <- ins <- 7
num_contact_work <- rou <- 48
transimisson_strength <-  trn <- 1/4 #probability of catching contagion given contact with contagious
infected_period <- 6.44 #days #infected but not contagious #needs negative exponential transform.
(inf <- time_param(infected_period))
infectious_period <- 2.94 #days #infected and contagious
(con <- time_param(infectious_period))
#immunity_period = ∞, treated in the small time limit as v. long

#clinical----
#incubation_period <- 9 #days 
prob_sym_infected <- dev <- 1/3
prob_ards <- sev <-  1.7/100 # given ur symptomatic
symptom_period<- 5   #days
(sym <- time_param(symptom_period))
ards_period <- 12 #days
(rds<-time_param(ards_period))
ards_fatality_ccu <- fat <- 1/3
ards_survival_home <- sur <- 1/16

#testing----
#track_eff
#test_sensitivity
#sustained_testing
#baseline_test
#prop_infected_tested
test_p_capita <- tft<- 500/(10^5)
rate_test <- sen <- 1/100 #percentage
delay_test_report <- 2.02 #days #transform
(del <-time_param(delay_test_report))
prob_tested_uninfected <- tes <-  1/8

redundant_parameters <- function() {

  #######location
  p_ccu <- 0
  Q=0.5 #temp  #cap <- 
  P=0.5#temporary
  sde <- 1
  
  ########clinical
  survival_rate = 0.99
  #prop_loc_ccu
  fat_rate = 0.01
  fat_rate = 1 - survival_rate
  
  dev=0.5
  sym=0.5
  sev=0.5
  rds=0.5
  fat=0.5
  
  ###########infection
  trn=0.25
  ins=0.5
  inf=0.5
  con=0.5
  ins <- 2/3
  out <- 1/3
  con <- 0.25
  
  ##########testing
  sen=0.5
  tft=0.5
  tes <- 0.75
  del=0.5

}













