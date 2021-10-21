require(readxl)
require(tidyverse)
require(smoother)
require(rstan)
#--------------------first file sent ~ early may-----
hospital_data <- read.csv(paste(getwd(),'/data/full_hospital_data_aaron.csv',sep=''))
#reading in the hospital .csv data-------------
hospital_data$Date <- as.Date(hospital_data$Date,format = '%d/%m/%y')
View(hospital_data)

ggplot(hospital_data, aes(x=Date ))+
  geom_point(aes(y=Admissions))+
  geom_point(aes(y=FirstAdmissionPerPerson),col='blue')+
  theme_minimal()

plot(hospital_data)

#--------reading in new hospital .xlsx file for positive tests (cumulative)----
positive.tests <- read_xlsx(paste(getwd(),'/data/DoH_Dashboard.xlsx',sep=''),col_names = T,skip = 1,trim_ws = T)
#hosp_data2[,1] <- as.Date(hosp_data2[,1],format = '%d/%m/%y')
View(hosp_data2)

est_data <- positive.tests$`Confirmed COVID-19 Cases`[-(92:102)]
#--------reading in the cases (DoH dashboard) .xlsx file--------------

hosp_data3 <- read_xlsx(paste(getwd(),'/data/31.05_hospitalData.xlsx',sep=''),sheet ='2020-05-31 10.20.08NIExport' )#,col_names = T,skip = 1,trim_ws = T)
View(hosp_data3)
hosp_data3[['DeathWithinTimeFromTest']]
#-------------------------------------------------

deaths <- read_xlsx(paste(getwd(),'/data/DoH_Dashboard.xlsx',sep=''),sheet ='Confirmed COVID-19 Deaths' ,col_names = T,skip = 1)#,trim_ws = T)
deaths <- select(deaths,'Date of Death',`Confirmed COVID-19 Deaths`)
est_data3 <- deaths$`Confirmed COVID-19 Deaths`[-(92:102)]
#-------------------------------------------------

icu.occupancy <- read_xlsx(paste(getwd(),'/data/DoH_Dashboard.xlsx',sep=''),sheet ='ICU Beds and Hospitalisation' ,col_names = T,skip = 1)#,trim_ws = T)
est_data2 <- icu.occupancy$`ICU - COVID Patients (ADULTS)`+icu.occupancy$`ICU - COVID Patients (CHILDREN)`
est_data2 <- est_data2[-(92:102)]

plot(est_data,main='confirmed cases')
lines(smth(est_data2,method='gaussian',))  # looks like a winner. 
plot(est_data2,main='icu occupancy (covid related, adults+children')
plot(est_data3,main='deaths from DoH dashboard',col='red')




