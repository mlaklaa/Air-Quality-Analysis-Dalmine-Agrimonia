library(tidyverse)
library(corrplot)
library(car)
library(knitr)
setwd("C:\\Users\\marwa\\Documents\\PROGETTO")
load("Agrimonia_stations.RData") 
agri = a 
#stazione scelta=Dalmine Via Verdi
ds = agri[agri$NameStation == "Dalmine Via Verdi", ]
plot(ds$Time, ds$AQ_no2)
#######################################################
######## VARIABILI DUMMIES PER LE STAGIONI E COVID ####
#######################################################

ds$month = as.numeric(substr(ds$Time, 6, 7)) 

ds$winter = 0
ds$winter[ds$month >= 12 | ds$month <= 2] = 1
ds$spring = 0
ds$spring[ds$month >= 3 & ds$month <= 5] = 1
ds$summer = 0
ds$summer[ds$month >= 6 & ds$month <= 8] = 1
ds$autumn = 0
ds$autumn[ds$month >= 9 & ds$month <= 11] = 1

ds$covid = 0
ds$covid[ds$Time >= "2020-03-09" & ds$Time <= "2020-05-18"] = 1 # abbiamo scelto come intervallo quello di lockdown totale


ds = ds[order(ds$Time), ] #ordino per data e poi verifico che sia stato fatto giusto l'ordinamento con View(ds)
colSums(is.na(ds))#controllo quali sono le variabili che hanno tante righe mancanti e metto solo quelle che non hanno righe mancanti 

colonne= c("Time", "AQ_no2" ,"WE_temp_2m" , "WE_tot_precipitation", "WE_rh_mean", "WE_surface_pressure","WE_solar_radiation", "WE_wind_speed_10m_mean","WE_wind_speed_100m_mean","WE_blh_layer_max","LI_bovine","WE_mode_wind_direction_10m","winter", "spring", "summer", "autumn",  "covid")
ds=ds[, colonne]
# Per poterlo rendere completo come quello di PM10, aggiugno una colonna per poi vedere se NO2 del giorno precedente abbia qualche effetto significativo
ds$NO2_ieri = c(NA, ds$AQ_no2[1:(nrow(ds)-1)])
ds_dyn = na.omit(ds)
dim(ds_dyn)
###################################################################
######## SUBSET PER OGNI ANNO #####################################
###################################################################

last16 = "2016-12-31"
first17 = "2017-01-01"
last17 = "2017-12-31"
first18 = "2018-01-01"
last18 = "2018-12-31"
first19 = "2019-01-01" 
last19 = "2019-12-31"
first20 = "2020-01-01" 
last20 = "2020-12-31"
first21 = "2021-01-01"
last21 = "2021-12-31"

ds2016 = ds_dyn[ds_dyn$Time >= first16 & ds_dyn$Time <= last16, ]
ds2017 = ds_dyn[ds_dyn$Time >= first17 & ds_dyn$Time <= last17, ]
ds2018 = ds_dyn[ds_dyn$Time >= first18 & ds_dyn$Time <= last18, ]
ds2019 = ds_dyn[ds_dyn$Time >= first19 & ds_dyn$Time <= last19, ]
ds2020 = ds_dyn[ds_dyn$Time >= first20 & ds_dyn$Time <= last20, ]
ds2021 = ds_dyn[ds_dyn$Time >= first21 & ds_dyn$Time <= last21, ]


####################################################################
######### ISTOGRAMMI PER VEDERE CHE TRASFORMAZIONE USARE ###########
####################################################################

par(mfrow=c(6,3), mar=c(1.5, 1.5, 1.5, 0.5))
anni = c(2016, 2017, 2018, 2019, 2020, 2021)
datasets = list(ds2016, ds2017, ds2018, ds2019, ds2020, ds2021)


for(i in 1:6) {
  df = datasets[[i]]
  anno = anni[i]
  
  hist(df$AQ_no2, main=paste(anno, ": Orig"), col="#880e4f", border="white", prob=TRUE, xlab="")
  lines(density(df$AQ_no2), col="#f06292", lwd=1.5)
  
  hist(sqrt(df$AQ_no2), main=paste(anno, ": Sqrt"), col="#ad1457", border="white", prob=TRUE, xlab="")
  lines(density(sqrt(df$AQ_no2)), col="#f48fb1", lwd=1.5)
  
  hist(log(df$AQ_no2), main=paste(anno, ": Log"), col="#d81b60", border="white", prob=TRUE, xlab="")
  lines(density(log(df$AQ_no2)), col="#fce4ec", lwd=1.5)
}

# Notiamo che il logaritmo sembra l'approssimazione perfetta anche se nel 2020 abbiamo un comportamento un po' particolare a causa del COVID
# Ora calcolo la correlazione 

par(mfrow=c(1,1))
plot(ds$WE_temp_2m, ds$AQ_no2, 
     pch = 19,                        
     col = alpha("#A54565", 0.6),   
     xlab = "Temperatura (°C)",       
     ylab = "Concentrazione NO2",  
     main = "",                   
     bty = "n",                       
     cex = 0.8,                       
     las = 1)      
# BOXPLOT PER OGNI STAGIONE

ds_dyn$Season = factor(ifelse(ds_dyn$winter==1, "Inverno", ifelse(ds_dyn$spring==1, "Primavera", 
                                                          ifelse(ds_dyn$summer==1, "Estate", "Autunno"))),
                   levels=c("Estate", "Autunno", "Inverno", "Primavera"))

boxplot(AQ_no2 ~ Season, data=ds_dyn, 
        main="NO2 per Stagione a Dalmine",
        col=c("darkred", "pink", "magenta", "purple"),
        ylab="µg/m3")
M=cor(ds_dyn[ , c( "AQ_no2" ,"WE_temp_2m" , "WE_tot_precipitation", "WE_rh_mean", "WE_surface_pressure","WE_solar_radiation", "WE_wind_speed_10m_mean","WE_wind_speed_100m_mean","WE_blh_layer_max","LI_bovine")]) # qui non metto la direzione del vento perchè non è un numero
corrplot(M, method="number",  mar= c( 0,0,0,0),  title=" CORRELAZIONI", diag=FALSE)
# da questa matrice notiamo che la temepratura (-0.70) ha una correlazione negativa molto forte , seguiti da radiazione solare e e BLH, notiamo una correlazione di 0.85 tra vento a 10 m e 100m (supera la soglia critica)
# ora useremo la stepwise per vedere quali variabili verrano tolte ( come predizioni uno dei due venti)


#####################################################################
####### COSTRUZIONE MODELLI #########################################
#####################################################################

m1=glm(AQ_no2 ~ spring + summer + autumn , family=Gamma(link="log"), data=ds_dyn)
#AIC=14979.7
m2=glm(AQ_no2 ~ spring + summer + autumn + WE_temp_2m + WE_tot_precipitation + WE_rh_mean + WE_surface_pressure +WE_solar_radiation + WE_wind_speed_10m_mean + WE_wind_speed_100m_mean + WE_blh_layer_max+ LI_bovine+ covid, family=Gamma(link="log"), data=ds_dyn)
summary(m2)
vif(m2) 
#AIC=14290.5, covid altamente significativo e con segno negatuvo (-0.4449), molto significativa anche WE_blh_layer_max, temp , vento a 10m  ha segno + a 100m - 
#variabili non significative : radiazione (p =0.89) e pressione (p=0.15)
#VIF, radiazione solare=8.28 vuol dire che la variabile è già spiegata da altre 

#STEPWISE

modello_step=step(m2, direction="both")
summary(modello_step)
plot(modello_step, col="magenta", pch=20)
#AIC=14288, ha rimosso radiazione solare 

#modello con valore no2 del giorno precedente

m4=glm (AQ_no2  ~ spring+summer+autumn+WE_temp_2m+ WE_tot_precipitation + WE_rh_mean + WE_surface_pressure+ WE_wind_speed_10m_mean + WE_wind_speed_100m_mean + WE_blh_layer_max+ LI_bovine + covid+ NO2_ieri, family=Gamma(link="log"), data=ds_dyn)
summary(m4)
#AIC=13367, COVID molto significativo 

#DIAGNOSTICA
par(mfrow=c(2,2))
plot(m4, col="magenta", pch=20)
# modello interazione stagione*temp
m5_no2 = glm(AQ_no2 ~ Season * WE_temp_2m + WE_tot_precipitation + WE_wind_speed_100m_mean+ WE_surface_pressure+ WE_rh_mean+
               WE_wind_speed_10m_mean + WE_blh_layer_max + LI_bovine + covid + NO2_ieri, 
             family=Gamma(link="log"), data=ds_dyn)

summary(m5_no2)
