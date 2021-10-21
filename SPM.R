install.packages('spm12r',repos='http://www.da.ugent.be')
install.packages('spm12r')
#install.packages('BayesFactor')
require('BayesFactor')

spm_P_Bonf <- function(Z,df,STAT,S,n) {

   if(STAT == "Z") {
        P <- 1 - pnorm(Z)
    } else if(STAT == "T") {
        P <- 1 - pt(Z, df[2])
    } else if(STAT == "X") {
        P <- 1 - pchisq(Z, df[2])
    } else if(STAT == "F") {
        P <- 1- pf(Z, df[1], df[2])
    } else {
        stop("wrong value for STAT argument", STAT)
    }

    P <- S*P^n
    P <- min(P, 1.0)

    P
}
