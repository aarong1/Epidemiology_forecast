#sensitivity_analysis.R----
#This script tests the most important parameters by varying them
#and observing their effect on the likelihood of the model generating
#the data.
for(sde in 0.5:7.5){
  sens.param <- function(sde) {
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
  
  }