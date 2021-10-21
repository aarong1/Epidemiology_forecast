## This implements the Dynamic Choice Model of Friston et al. (2020)a
#define factors and states----

factors <- c('location','infection','clinical','testing')
factors <- list(factors)

factors$location <- c( 'home', 'work', 'ccu', 'morgue') #'isolation',
factors$infection <- c('suscept', 'infected', 'infectious', 'immune') #'resistant',
factors$clinical <- c('asymptomatic', 'symptoms', 'ards','deceased')
factors$testing <- c('untested', 'waiting', 'positive', 'negative')

#initalise starting proportions (more appropiately the marginal probabilities)-----
initialiseProbabilities <- function(){
  #pop <- param[1]
  #ini <- param[2]
  
  #caontains inital parameters fed to the optimiser.
  #initial cases - ini
  #number of initially susceptible people - pop
  marginal_probability <- list()
  marginal_probability$location <-
  c(
    'home' = 0.75,
    'work' = 0.25,
    'ccu' = 0.0,
    'morgue' = 0.0
  ) #'isolation'=0.2,
  marginal_probability$infection <-
  c(
    'suscept' = (1.9*10^6-4)/(1.9*10^6) ,
    'infected' = 1/(1.9*10^6),  #i.e. 4 initial cases in a population 1.9*10^(6)
    'infectious' = 0.0,
    'immune' = 0.0
  ) #'resistant'=0.2,
  marginal_probability$clinical <-
  c(
    'asymptomatic' = 1.0,
    'symptoms' = 0.0,
    'ards' = 0.0,
    'deceased' = 0.0
  )
  marginal_probability$testing <-
  c(
    'untested' = 1.0,
    'waiting' = 0.0,
    'positive' = 0.0,
    'negative' = 0.0
  )
  names(marginal_probability$infection) <- factors$infection
  return(marginal_probability)
  }

sigmoid <- function(variable, scale_parameter) {
  #decreaing sigmoid function
  #variable on top,parameter on the bottom of the exponent

  y = (1 + exp(4 * variable / scale_parameter - 4))
  return(1 / y)
}

