library(tidyverse)
library(corrplot)
library(car)
library(knitr)
setwd("C:\\Users\\marwa\\Documents\\PROGETTO")
load("Agrimonia_stations.RData") 
agri = a 

ds = agri[agri$NameStation == "Dalmine Via Verdi", ]
#ds = ds[!is.na(ds$AQ_pm10), ]
ds$Time = as.Date(ds$Time)
plot(ds$Time, ds$AQ_pm10)

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

colonne= c("Time", "AQ_pm10" ,"WE_temp_2m" , "WE_tot_precipitation", "WE_rh_mean", "WE_surface_pressure","WE_solar_radiation", "WE_wind_speed_10m_mean","WE_wind_speed_100m_mean","WE_blh_layer_max","LI_bovine","WE_mode_wind_direction_10m", "winter","spring", "summer", "autumn",  "covid")
ds=ds[, colonne]
ds$AQ_pm10_ieri=c(NA, ds$AQ_pm10[1:(nrow(ds)-1)])
ds_dyn=na.omit(ds)
dim(ds_dyn)
########################################################
######### SUBSET ANNUALI ###############################
########################################################

first16 = "2016-01-01"
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

#############################################################
######## ISTOGRAMMI #########################################
#############################################################

#faccio gli istogrammi delle distribuzioni di ogni anno 
par(mfrow=c(6,3),mar=c(1.5, 1.5, 1.5, 0.5))
anni = c(2016, 2017, 2018, 2019, 2020, 2021)
datasets = list(ds2016, ds2017, ds2018, ds2019, ds2020, ds2021)

for(i in 1:6) {
  df = datasets[[i]]
  anno = anni[i]
  
  hist(df$AQ_pm10, main=paste(anno, ": Orig"), col="#880e4f", border="white", prob=TRUE, xlab="")
  lines(density(df$AQ_pm10), col="#f06292", lwd=1.5)

  hist(sqrt(df$AQ_pm10), main=paste(anno, ": Sqrt"), col="#ad1457", border="white", prob=TRUE, xlab="")
  lines(density(sqrt(df$AQ_pm10)), col="#f48fb1", lwd=1.5)

  hist(log(df$AQ_pm10), main=paste(anno, ": Log"), col="#d81b60", border="white", prob=TRUE, xlab="")
  lines(density(log(df$AQ_pm10)), col="#fce4ec", lwd=1.5)
}

par(mfrow=c(1,1))
# Impostiamo i margini per farlo carino
par(mar=c(5, 5, 2, 2)) 

# Grafico PM10 vs Temperatura
plot(ds$WE_temp_2m, ds$AQ_pm10, 
     pch = 19,                        
     col = alpha("#A54565", 0.6),   
     xlab = "Temperatura (°C)",       
     ylab = "Concentrazione PM10",  
     main = "",                   
     bty = "n",                       
     cex = 0.8,                       
     las = 1)                 

# Aggiungiamo una griglia leggera grigia per farlo sembrare pro
grid(col = "lightgray", lty = "dotted")
# ------OSSERVAZIONE SUGLI ISTOGRAMMI APPENA FATTI-----
#dagli istogrammi si nota per tutti gli anni che la trasformazione logaritmica garantisce una normalizzazione dei residui 
#questa conclusione ci consente di poter studiare tutto l'intervallo 2016-2021 con la trasformazione logaritmica

par(mfrow=c(1,1)) #torno alla vista singola nel plot per fare il grafico della correlazione

M = cor(ds_dyn[, c("AQ_pm10", "WE_temp_2m", "WE_tot_precipitation", "WE_rh_mean", "WE_surface_pressure", "WE_solar_radiation", "WE_blh_layer_max", "LI_bovine", "WE_wind_speed_10m_mean","WE_wind_speed_100m_mean")])
corrplot(M, method="number", diag=FALSE)

# ----OSSERVAZIONI SULLA TABELLA DELLE CORRELAZIONI----

# La BLH è il driver fisico principale (-0.50); bovini costanti (0.00) per assenza varianza temporale. 
# L'alta multicollinearità Temp/Rad/BLH (0.77) rende indispensabile lo Stepwise AIC per la parsimonia.


ds_dyn$Season = factor(ifelse(ds_dyn$winter==1, "Inverno", ifelse(ds_dyn$spring==1, "Primavera", 
                                                          ifelse(ds_dyn$summer==1, "Estate", "Autunno"))),
                   levels=c("Estate", "Autunno", "Inverno", "Primavera"))

boxplot(AQ_pm10 ~ Season, data=ds_dyn, 
        main="PM10 per Stagione a Dalmine",
        col=c("", "pink", "magenta", "purple"),
        ylab="µg/m3")

#qui possiamo dire che l'inquinamento in inverno è molto maggiore 


#PRIMO MODELLO BASE 
#faccio un primo modello in cui includo solo le stagioni per vedere come è rispetto all'inverno
m1 = glm(AQ_pm10 ~ spring + summer + autumn, family=Gamma(link="log"), data=ds_dyn)

#SECONDO MODELLO 
#aggiungo tutte le variabili metereologiche 
m2 = glm(AQ_pm10 ~ spring + summer + autumn + WE_temp_2m + WE_tot_precipitation + 
           WE_rh_mean + WE_surface_pressure + WE_solar_radiation + 
           WE_wind_speed_10m_mean + WE_wind_speed_100m_mean + 
           WE_blh_layer_max + LI_bovine + covid, 
         family=Gamma(link="log"), data=ds_dyn)

summary(m2)
#AIC=14731, QUASI TUTTE E NOVE LE VARAIBILI RISULTANO ALTAMENTE SIGNIFICATIVE, IL VENTO A 10 M HA EFFETTO POSITIVO E A 100 
#INVECE E' NEGATIVO PERCHE AGISCE COME PULITORE, radzione solare significativa e negativa (aiuta a ridurre il pm10?)
#non sono risultate significative la temperatura (p value=0.30). Questo perchè in presenza della radiazione solare 
#la temperatura diventa ridondante
#anche la variabile COVID nn risulta essere significativa (p value=0.34)
 
vif(m2)
#non eisste una multicllinearità che invalidi il modello. ci sono alcuni variabli come 
#radiazione solare (8.31), Estate (6.79), Temperatura (6.18). questo perchè hannoin estate 
#la radiazione solare è massima e di consguenza la temperatura aumenta e questo spiega perchè nel summary precedente la temp NON risultava significativa 
#questp perchè i due fenomeni sono sovrapposti
# indipendenti : COVID (1.21) e BLH 

m3 = step(m2, direction="both")
summary(m3)
#AIC=14730 NULL DEVIANCE=716.16, REIDUAL DEVIANCE=348.53
# lo stepwise ha tolto temperatura e la variabile COVID (strano?!?)
#sono rimasti vento a 10m e vento a 100m 
# tutte le variabili rimaste presentano il massimo grado significatività (***)
# ha un aic molto basso, secondo il principio di parsimonia dovrebbe essere il miglior modello perchè più semplice del precedente ma migliore nel descrivere i dati 


par(mfrow=c(2,2))
plot(m3, col="pink",pch=20)

#residual vs fitted=la linea rossa è quasi perfettamente piatta e centrata sullo zero
#Normal Q-Q= i residui seguono fedelmente la bisettrice tratteggiata per lastragrande maggiornaza, si nota comqunue una chiara deviazione nelle code sup eciò significa che dalmine presenta dei picchi estremi che non riusciamo a prevedere per ora (potrebbero esssere picchi di traffico)
#Scale-Location= linea rossa orizzontale e la dispersione dei punti è costante lungo tutto l'asse delle ascisse 
#Residuals vs Leverage= tutti i punti sono concentrati a sinistra e nessuno si trova oltre le linee rosse tratteggiate della Distanza di Cook

#ora proviamo a mettere una nuova colonna che registra i valori del pm10 del giorno prima per vedere se ha qualche effetto 
#perchè magari rimane nell'aria il pm10 e quindi la concetrazione del giorno prima ha un effetto

m4 = glm(formula(m3), family=Gamma(link="log"), data=ds_dyn)
m4 = update(m4, . ~ . + AQ_pm10_ieri)
summary(m4)
#notiamo infatti un AIC=13932
#nonostante la nuova colonna aggiunta notiamo che BLH, vento a 100m e precipitazioni restano indipendenti dal valore di ieri

par (mfrow=c(2,2))
plot(m4, col="magenta", pch=20)

#modello con interazione stagioni e temperatura spiegato poi nel report
m5_pm10 = glm(AQ_pm10 ~ Season * WE_temp_2m + 
                WE_tot_precipitation + 
                WE_rh_mean + 
                WE_surface_pressure + 
                WE_solar_radiation + 
                WE_wind_speed_10m_mean + 
                WE_wind_speed_100m_mean + 
                WE_blh_layer_max + 
                LI_bovine + 
                AQ_pm10_ieri, 
              family=Gamma(link="log"), data=ds_dyn)

summary(m5_pm10)

#AIC 13879  

