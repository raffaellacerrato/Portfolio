###Download library
library(highfrequency)
library(zoo)				
library(fBasics)
library(xts)
library(tseries)
library(forecast)
library(quantmod)
library(rugarch)
library(tidyquant)
library(ggplot2)
library(dplyr)
library(lmtest)
library(car)

# Imposto la working directory
wd <- "D:/backup/Desktop/MAGISTRALE RAFFAELLA/ANALISI DEI MERCATI FINANZIARI"
dir.exists(wd)   # controllo che esista → deve dare TRUE
setwd(wd)
getwd()          # per verificare che sia stata cambiata

################## ANALISI PRELIMINARE

###Caricamento dei dati da Yahoo Finances

#Definiamo le date
start_date <- as.Date("2009-10-01")
end_date <- as.Date("2024-10-01")
getSymbols("^GDAXI", src = "yahoo", from = start_date, to = end_date)
GDAXI_data <- na.approx(GDAXI) #estrapoliamo i valori mancanti

#salviamo i dati in formato .csv
write.table(x = cbind(Date = index(GDAXI_data), coredata(GDAXI_data)),
          sep = ",",
          file = "DAX_data_2009_2024.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

#Controlliamo che i dati siano stati caricati correttamente
GDAXI_data_check <- read.csv("DAX_data_2009_2024.csv")
head(GDAXI_data_check)

#consideriamo solo la chiusura aggiustata
Adj_DAX <- GDAXI_data[, "GDAXI.Adjusted"]
head(Adj_DAX)
N_DAX<-length(Adj_DAX)
summary(Adj_DAX)

#plot semplice
plot(Adj_DAX, main="DAX",col="red")

#Deviazione standard
sd(Adj_DAX)

#Analisi dei correlogrammi
acf(Adj_DAX, main = "ACF adj close DAX")
pacf(Adj_DAX, main = "PACF adj close DAX")

##i dati non sono stazionari, dobbiamo fare le differenze
DAX_stz1<-diff(Adj_DAX, differences = 1)
DAX_stz1_ <- na.approx(DAX_stz1)
acf(DAX_stz1_, main = "ACF DAX diff 1")
pacf(DAX_stz1_, main = "PACF DAX diff1")

#Differenza logaritmica
DAX_log<-diff(log(Adj_DAX))
DAX_log_<- na.omit(DAX_log)
acf(DAX_log_, main = "ACF DAX diff log")
pacf(DAX_log_, main = "PACF DAX diff log")

#Test ADF per verificare la stazionarietà della serie
adf.test(DAX_stz1_)
adf.test(DAX_log_)
#rifiutiamo l'ipotesi nulla di non stazionarietà della serie.

#Statistiche descrittive dei dati
stat_DAX<- cbind(mean(DAX_log_),
                 sd(DAX_log_),
                 min(DAX_log_),
                 max(DAX_log_),
                 skewness(DAX_log_),
                 kurtosis(DAX_log_))
colnames(stat_DAX)<-c("Media","SD","MIN","MAX","Asimm.","Curt.")

round(stat_DAX, 4)

#ci sono valori eccezionali?
h1<-quantile(DAX_log_,0.25) - 1.5*(quantile(DAX_log_,0.75)-quantile(DAX_log_,0.25))
H2<-quantile(DAX_log_,0.75) + 1.5*(quantile(DAX_log_,0.75)-quantile(DAX_log_,0.25))

outliers_lower <-length(which(DAX_log_<h1)) #valori al di sotto del limite inferiore
outliers_upper <-length(which(DAX_log_>H2)) #valori al di sopra del limite inferiore

total_data <- length(DAX_log_) # Lunghezza totale dei dati
outliers_total <- outliers_lower + outliers_upper # Numero totale di outliers
proportion_outliers <- outliers_total / total_data * 100 # Proporzione in percentuale

#Circa il 6% dei dati sono outliers

#density-plot
plot(density(DAX_log_))

################ MODELLAZIONE ARMA

#AR(1)
AR1_DAX <- Arima(DAX_log_, order = c(1,0,0), include.mean = TRUE)
summary(AR1_DAX)
names(AR1_DAX)
AR1_DAX$var.coef

#AR(2)
AR2_DAX <- Arima(DAX_log_, order = c(2,0,0), include.mean = TRUE)
summary(AR2_DAX)
names(AR2_DAX)
AR2_DAX$var.coef

#MA(1)
MA1_DAX <- Arima(DAX_log_, order = c(0,0,1), include.mean = TRUE)
summary(MA1_DAX)
names(MA1_DAX)
MA1_DAX$var.coef

#MA(2)
MA2_DAX <- Arima(DAX_log_, order = c(0,0,2), include.mean = TRUE)
summary(MA2_DAX)
names(MA2_DAX)
MA2_DAX$var.coef

#ARMA(1,1)
ARMA11_DAX <- Arima(DAX_log_, order = c(1,0,1), include.mean = TRUE)
summary(ARMA11_DAX)
names(ARMA11_DAX)
ARMA11_DAX$var.coef
coeftest(ARMA11_DAX)

#ARMA(1,2)
ARMA12_DAX <- Arima(DAX_log_, order = c(1,0,2), include.mean = TRUE)
summary(ARMA12_DAX)
names(ARMA12_DAX)
ARMA12_DAX$var.coef
coeftest(ARMA12_DAX)

#ARMA(2,1)
ARMA21_DAX <- Arima(DAX_log_, order = c(2,0,1), include.mean = TRUE)
summary(ARMA21_DAX)
names(ARMA21_DAX)
ARMA21_DAX$var.coef
coeftest(ARMA21_DAX)

#ARMA(3,1)
ARMA31_DAX <- Arima(DAX_log_, order = c(3,0,1), include.mean = TRUE)
summary(ARMA31_DAX)
names(ARMA31_DAX)
ARMA31_DAX$var.coef
coeftest(ARMA31_DAX)

#ARMA(2,2)
ARMA22_DAX <- Arima(DAX_log_, order = c(2,0,2), include.mean = TRUE)
summary(ARMA22_DAX)
names(ARMA22_DAX)
ARMA22_DAX$var.coef
coeftest(ARMA22_DAX)

#ARMA(2,3)
ARMA23_DAX <- Arima(DAX_log_, order = c(2,0,3), include.mean = TRUE)
summary(ARMA23_DAX)
names(ARMA23_DAX)
ARMA23_DAX$var.coef
coeftest(ARMA23_DAX)

#ARMA(3,2)
ARMA32_DAX <- Arima(DAX_log_, order = c(3,0,2), include.mean = TRUE)
summary(ARMA32_DAX)
names(ARMA32_DAX)
ARMA32_DAX$var.coef
coeftest(ARMA32_DAX)

#ARMA(3,3)
ARMA33_DAX <- Arima(DAX_log_, order = c(3,0,3), include.mean = TRUE)
summary(ARMA33_DAX)
names(ARMA33_DAX)
ARMA33_DAX$var.coef
coeftest(ARMA33_DAX)

#ARMA(4,2)
ARMA42_DAX <- Arima(DAX_log_, order = c(4,0,2), include.mean = TRUE)
summary(ARMA42_DAX)
names(ARMA42_DAX)
ARMA42_DAX$var.coef
coeftest(ARMA42_DAX)

#ARMA(4,3)
ARMA43_DAX <- Arima(DAX_log_, order = c(4,0,3), include.mean = TRUE)
summary(ARMA43_DAX)
names(ARMA43_DAX)
ARMA43_DAX$var.coef
coeftest(ARMA43_DAX)

#ARMA(4,4)
ARMA44_DAX <- Arima(DAX_log_, order = c(4,0,4), include.mean = TRUE)
summary(ARMA44_DAX)
names(ARMA44_DAX)
ARMA44_DAX$var.coef
coeftest(ARMA44_DAX)

#ARMA(2,4)
ARMA24_DAX <- Arima(DAX_log_, order = c(2,0,4), include.mean = TRUE)
summary(ARMA24_DAX)
names(ARMA24_DAX)
ARMA24_DAX$var.coef
coeftest(ARMA24_DAX)

######SCELTA DEL MODELLO

#Creamo una lista per confrontare più agevolmente i risultati
modelli <- list(
  AR1_DAX = Arima(DAX_log_, order = c(1,0,0), include.mean = TRUE),
  AR2_DAX = Arima(DAX_log_, order = c(2,0,0), include.mean = TRUE),
  MA1_DAX = Arima(DAX_log_, order = c(0,0,1), include.mean = TRUE),
  MA2_DAX = Arima(DAX_log_, order = c(0,0,2), include.mean = TRUE),
  ARMA11_DAX = Arima(DAX_log_, order = c(1,0,1), include.mean = TRUE),
  ARMA21_DAX = Arima(DAX_log_, order = c(2,0,1), include.mean = TRUE),
  ARMA22_DAX = Arima(DAX_log_, order = c(2,0,2), include.mean = TRUE),
  ARMA31_DAX = Arima(DAX_log_, order = c(3,0,1), include.mean = TRUE),
  ARMA32_DAX = Arima(DAX_log_, order = c(3,0,2), include.mean = TRUE),
  ARMA33_DAX = Arima(DAX_log_, order = c(3,0,3), include.mean = TRUE),
  ARMA42_DAX = Arima(DAX_log_, order = c(4,0,2), include.mean = TRUE),
  ARMA43_DAX = Arima(DAX_log_, order = c(4,0,1), include.mean = TRUE)
)

#Funzione per raccogliere AIC e BIC
confronta_aic_bic <- function(lista_modelli) {
  sapply(lista_modelli, function(model) c(AIC = AIC(model), BIC = BIC(model)))
}

#Confronto dei modelli
aic_bic_results <- confronta_aic_bic(modelli)
print(aic_bic_results)

# Ordina per AIC (dal più basso al più alto)
aic_bic_ordinati_aic <- aic_bic_results[, order(aic_bic_results["AIC", ])]
print("Modelli ordinati per AIC (dal più basso al più alto):")
print(aic_bic_ordinati_aic)

# Ordina per BIC (dal più basso al più alto)
aic_bic_ordinati_bic <- aic_bic_results[, order(aic_bic_results["BIC", ])]
print("Modelli ordinati per BIC (dal più basso al più alto):")
print(aic_bic_ordinati_bic)

###Dall'analisi del AIC e BIC, attribuendo un peso dello 0,5 a ciascuno dei
###metodi, possiamo fare la seguente graduatoria dal modello più preferibile
###al meno preferibile, considerando solo quel criterio.

# Creiamo un data frame con i modelli e i loro punteggi AIC e BIC

modelli <- data.frame(
  Modello = c("ARMA43_DAX", "ARMA31_DAX", "ARMA42_DAX", "ARMA21_DAX",
              "ARMA33_DAX", "ARMA22_DAX", "ARMA11_DAX", "AR2_DAX",
              "MA2_DAX", "ARMA32_DAX", "MA1_DAX", "AR1_DAX"),
  AIC = c(-22646.43, -22645.38, -22655.44, -22647.29,
          -22657.95, -22652.41, -22649.00, -22649.29,
          -22649.30, -22660.00, -22651.00, -22651.01),
  BIC = c(-22602.71, -22607.91, -22605.47, -22616.06,
          -22607.98, -22614.93, -22624.02, -22624.30,
          -22624.31, -22616.28, -22632.27, -22632.27)
)

# Calcolare il punteggio ponderato
modelli$Weighted_Score <- 0.5 * modelli$AIC + 0.5 * modelli$BIC

# Ordinare il data frame in base al punteggio ponderato
Ordine_CI <- modelli[order(modelli$Weighted_Score), ]

# Stampare i risultati
print("Modelli ordinati per punteggio ponderato (AIC e BIC):")
print(Ordine_CI[, c("Modello", "Weighted_Score")])

#####ANALISI DELLA SIGNIFICATIVITA' DEI PARAMETRI

coeftest(AR1_DAX)
coeftest(AR2_DAX)
coeftest(MA1_DAX)
coeftest(MA2_DAX)
coeftest(ARMA11_DAX)
coeftest(ARMA21_DAX)
coeftest(ARMA22_DAX)
coeftest(ARMA31_DAX)
coeftest(ARMA32_DAX)
coeftest(ARMA33_DAX)
coeftest(ARMA42_DAX)
coeftest(ARMA43_DAX)

##Realizzando il test-t sulla significatività dei parametri individuiamo
#che i modelli ARMA(2,2) e ARMA(3,3) hanno parametri con un maggiore livello
#di significatività.

#TEST LJIUNG-BOX SUI RESIDUI

res_AR1 <- residuals(AR1_DAX)
Box.test(res_AR1, lag = 20, type = "Ljung-Box")

res_AR2 <- residuals(AR2_DAX)
Box.test(res_AR2, lag = 20, type = "Ljung-Box")

res_MA1 <- residuals(MA2_DAX)
Box.test(res_MA1, lag = 20, type = "Ljung-Box")

res_MA2 <- residuals(MA2_DAX)
Box.test(res_MA2, lag = 20, type = "Ljung-Box")

res_ARMA11 <- residuals(ARMA11_DAX)
Box.test(res_ARMA11, lag = 20, type = "Ljung-Box")

res_ARMA21 <- residuals(ARMA21_DAX)
Box.test(res_ARMA21, lag = 20, type = "Ljung-Box")

res_ARMA12 <- residuals(ARMA12_DAX)
Box.test(res_ARMA12, lag = 20, type = "Ljung-Box")

res_ARMA22 <- residuals(ARMA22_DAX)
Box.test(res_ARMA22, lag = 20, type = "Ljung-Box")

res_ARMA23 <- residuals(ARMA23_DAX)
Box.test(res_ARMA23, lag = 20, type = "Ljung-Box")

res_ARMA32 <- residuals(ARMA32_DAX)
Box.test(res_ARMA32, lag = 20, type = "Ljung-Box")

res_ARMA33 <- residuals(ARMA33_DAX)
Box.test(res_ARMA33, lag = 20, type = "Ljung-Box")

res_ARMA43 <- residuals(ARMA43_DAX)
Box.test(res_ARMA43, lag = 20, type = "Ljung-Box")

###Troviamo che solo dal modello ARMA(2,2) in avanti, riusciamo a
###accettare l'ipotesi nulla di assenza di autocorrelazioni nei residui,
###in concreto, i due modelli con p-value più elevato sono ARMA(3,3) e ARMA(4,3)

#Analisi grafica dei residui
res_ARMA22_DAX<-residuals(ARMA22_DAX) # ARMA (2,2)
par(mfrow=c(2,1)) 
acf(res_ARMA22_DAX)
pacf(res_ARMA22_DAX)

res_ARMA33_DAX<-residuals(ARMA22_DAX) # ARMA (3,3)
par(mfrow=c(2,1)) 
acf(res_ARMA33_DAX)
pacf(res_ARMA33_DAX)

#La presenza di alcune barrette fuori banda nel PACF potrebbe essere dovuta a
#fluttuazioni casuali o a specificità del modello. Ma possiamo confermare che non
#ci sono autocorrelazioni significative nei residui con i risultati del Tes Ljiung-Box

jarque.bera.test(res_ARMA22_DAX)
jarque.bera.test(res_ARMA33_DAX)
####i risultati del Jarque-Bera Test indicano chiaramente che i residui dei 
#modelli ARMA(2,2) e ARMA(3,3) non seguono una distribuzione normale.
#Quindi rifiutiamo l'ipotesi nulla.


#######Dopo aver identificato i modelli che hanno parametri significativi e che si sono
#dimostrati di essere i migliori relativamente agli altri modelli testati, abbiamo
#dimostrato che i residui dei modelli ARMA(2,2) e ARMA(3,3) non seguono 
#una distribuzione normale, possiamo dire che ciò indica che il modello ARMA da solo
#non è sufficiente a catturare tutte le caratteristiche dei dati.
#Questo suggerisce la necessità di passare a un modello ARMA-GARCH per
#catturare la volatilità condizionata e migliorare la specificazione del modello.

################## MODELLAZIONE GARCH

# Specificazione dei modelli ARMA(2,2) e ARMA(3,3) con GARCH(1,1) e distribuzione normale
spec_arma22_garch_norm <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(2, 2), include.mean = TRUE),
  distribution.model = "norm"
)

spec_arma33_garch_norm <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 3), include.mean = TRUE),
  distribution.model = "norm"
)

# Specificazione dei modelli con distribuzione t-student simmetrica
spec_arma22_garch_t <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(2, 2), include.mean = TRUE),
  distribution.model = "std"
)

spec_arma33_garch_t <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 3), include.mean = TRUE),
  distribution.model = "std"
)

# Adattamento dei modelli ai dati
fit_arma22_garch_norm <- ugarchfit(spec = spec_arma22_garch_norm, data = DAX_log_)
fit_arma33_garch_norm <- ugarchfit(spec = spec_arma33_garch_norm, data = DAX_log_)

fit_arma22_garch_t <- ugarchfit(spec = spec_arma22_garch_t, data = DAX_log_)
fit_arma33_garch_t <- ugarchfit(spec = spec_arma33_garch_t, data = DAX_log_)

# Visualizziamo i risultati
fit_arma22_garch_norm
fit_arma33_garch_norm
fit_arma22_garch_t
fit_arma33_garch_t

###Troviamo che i migliori due modelli, sotto i criteri dell'AIC e BIC sono i modelli con distribuzione
###t student.

###Proviamo altri ordini di modello GARCH

# Definiamo le specifiche del modello GARCH(1,2) con ARMA(2,2) e distribuzione t-student
spec_arma22_garch12 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
  mean.model = list(armaOrder = c(2, 2), include.mean = TRUE),
  distribution.model = "std"
)

# Stima modello GARCH(1,2) con ARMA(2,2)
fit_arma22_garch12 <- ugarchfit(spec = spec_arma22_garch12, data = DAX_log_)

# Definiamo le specifiche del modello GARCH(2,1) con ARMA(2,2) e distribuzione t-student
spec_arma22_garch21 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2, 1)),
  mean.model = list(armaOrder = c(2, 2), include.mean = TRUE),
  distribution.model = "std"
)

# Stima modello GARCH(2,1) con ARMA(3,3)
fit_arma22_garch21 <- ugarchfit(spec = spec_arma22_garch21, data = DAX_log_)

# Definiamo le specifiche del modello GARCH(1,2) con ARMA(3,3) e distribuzione t-student
spec_arma33_garch12 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
  mean.model = list(armaOrder = c(3, 3), include.mean = TRUE),
  distribution.model = "std"
)

# Stima modello GARCH(1,2) con ARMA(3,3)
fit_arma33_garch12 <- ugarchfit(spec = spec_arma33_garch12, data = DAX_log_)

# Definiamo le specifiche del modello GARCH(2,1) con ARMA(3,3) e distribuzione t-student
spec_arma33_garch21 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2, 1)),
  mean.model = list(armaOrder = c(3, 3), include.mean = TRUE),
  distribution.model = "std"
)

# Stima del modello GARCH(2,1) con ARMA(3,3)
fit_arma33_garch21 <- ugarchfit(spec = spec_arma33_garch21, data = DAX_log_)

###Osserviamo i risultati
fit_arma22_garch12
fit_arma22_garch21
fit_arma33_garch12
fit_arma33_garch21

###Possiamo concludere dai risultati che ordini superiori non si adattano
###meglio ai nostri dati, infatti, tanto il parametro alpha 2 nel modello
###GARCH(2,1) come il parametro beta 2 nel modello GARCH (1,2) non sono
###significativi.

###PROVIAMO MODELLI GARCH ALTERNATIVI

##GJR

#GJR-GARCH(1,1) con ARMA(3,3)
spec_arma33_gjr <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 3), include.mean = TRUE),
  distribution.model = "std"
)
#Fitting del modello
fit_arma33_gjr <- ugarchfit(spec = spec_arma33_gjr, data = DAX_log_)

#GJR-GARCH(1,1) con ARMA(2,2)
spec_arma22_gjr <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(2, 2), include.mean = TRUE),
  distribution.model = "std"
)
#Fitting del modello
fit_arma22_gjr <- ugarchfit(spec = spec_arma22_gjr, data = DAX_log_)

##EGARCH

#SpecificHiamo il modello EGARCH(1,1) con ARMA(3,3)
spec_arma33_egarch <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 3), include.mean = TRUE),
  distribution.model = "std"
)

#Fitting del modello
fit_arma33_egarch <- ugarchfit(spec = spec_arma33_egarch, data = DAX_log_)

# SpecificHiamo il modello EGARCH(1,1) con ARMA(2,2)
spec_arma22_egarch <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(2, 2), include.mean = TRUE),
  distribution.model = "std"
)

# Fitting del modello
fit_arma22_egarch <- ugarchfit(spec = spec_arma22_egarch, data = DAX_log_)

###Calcolo della varianza condizionata dei modelli
cond_vol_fit_ARMA22_GARCH11 <- fit_arma22_garch_t@fit$sigma^2
cond_vol_fit_ARMA33_GARCH11 <- fit_arma33_garch_t@fit$sigma^2
cond_vol_fit_ARMA33_GJR11 <- fit_arma33_gjr@fit$sigma^2
cond_vol_fit_ARMA33_EGARCH11 <- fit_arma33_egarch@fit$sigma^2

summary(cond_vol_fit_ARMA22_GARCH11)
summary(cond_vol_fit_ARMA33_GARCH11)
summary(cond_vol_fit_ARMA33_GJR11)
summary(cond_vol_fit_ARMA33_EGARCH11)

###############################FORECASTING

#Creiamo un oggetto con i rendimenti al quadrato per ottenere una proxy
#della volatilità
r2_DAX<-DAX_log_^2
head(r2_DAX)
summary(r2_DAX)

#Dividiamo il nostro data set in: 80% in-sample e 20% out-of-sample
n <- length(DAX_log_)  # Numero totale di osservazioni
DAX_training <- floor(0.8 * n)  # 80% per training
DAX_IS <- DAX_log_[1:DAX_training] #creo il vettore con i rendimenti in-sample
DAX_OOS <- DAX_log_[(DAX_training + 1):n] #creo il vettore con i rendimenti out-of-sample

#Creiamo il vettore con la lunghezza della serie
n_OOS<-length(DAX_OOS)
n_IS<-length(DAX_IS)

##############ANALISI GRAFICA
# Definizione della variabile times (estrazione delle date dal dataset)
head(GDAXI_data)
times <- index(GDAXI_data)[-1]
head(times)
asset <- "DAX"

# Grafico dei rendimenti stazionari
plot(y = DAX_log_, x = times,
     type = 'l',
     col = 'blue',  # Aggiungo un colore per distinguere
     ylab = "Values",
     xlab = "Time",
     main = paste(asset, "Returns"))

# Grafico dei rendimenti al quadrato come proxy della volatilità
plot(y = r2_DAX, x = times,
     type = 'l',
     col = 'dark red',
     ylab = "Values",
     xlab = "Time",
     main = paste(asset, "Squared Returns (Proxy for Volatility)"))

par(mfrow = c(1, 1))  #reimpostiamo la visualizzazione del plot

#Confronto tra le proxy: r2 e volatilità condizionata
plot(y = r2_DAX, x = times, type = 'l', col = 'dark grey',
     xlab = 'time', ylab = 'r2 vs volatilità condizionata',
     main = "Confronto tra r2 e varianza condizionata dei modelli stimati")
lines(y = cond_vol_fit_ARMA22_GARCH11, x = times, col = 3)
lines(y = cond_vol_fit_ARMA33_GARCH11, x = times, col = 4)
lines(y = cond_vol_fit_ARMA33_GJR11, x = times, col = 5)
lines(y = cond_vol_fit_ARMA33_EGARCH11, x = times, col = 6)
legend('topleft',
       legend = c("r2", "ARMA(2,2)-GARCH(1,1)", "ARMA(3,3)-GARCH(1,1)",
                  "ARMA(3,3)-GJR(1,1)", "ARMA(3,3)-EGARCH(1,1)"),
       lty = 1,
       col = c("dark grey", 3, 4, 5, 6),
       cex = 0.5)

###Plot rendimenti e volatilità

par(mfrow = c(2, 1)) #reimpostiamo la visualizzazione


#Grafico dei rendimenti is/oos
plot(y = DAX_log_, x = times,  # Utilizziamo DAX_log_ per i rendimenti
     type = 'l', 
     ylab = "Values", 
     xlab = "Time", 
     main = paste("DAX Returns"))
lines(y = DAX_OOS, x = times[(DAX_training + 1):n], 
      type = 'l', col = "dark gray")
abline(v = times[DAX_training], col = "blue")  # Linea verticale per separare IS da OOS

# Dettaglio grafico dei rendimenti OOS
plot(y = DAX_OOS, 
     x = times[(DAX_training + 1):n], 
     type = 'l', col = "dark gray", 
     ylab = "Values", 
     xlab = "Time", 
     main = paste("DAX Out-of-Sample Returns"))

# Grafico dei rendimenti al quadrato IS/OOS
plot(y = r2_DAX, x = times, 
     type = 'l', col = 'dark red', 
     ylab = "Values", 
     main = paste("DAX r2"))
lines(y = r2_DAX[(DAX_training + 1):n], 
      x = times[(DAX_training + 1):n], 
      type = 'l', col = "pink")
abline(v = times[DAX_training], col = "blue")

# Dettaglio grafico dei rendimenti al quadrato OOS
plot(y = r2_DAX[(DAX_training + 1):n], 
     x = times[(DAX_training + 1):n], 
     type = 'l', col = "pink", 
     ylab = "Values", 
     xlab = "Time", 
     main = paste("DAX Out-of-Sample r2"))

#Grafico OOS, ensemble
par(mfrow = c(1, 1)) #

plot(y = DAX_OOS,
     x = times[(DAX_training + 1):n],  # Utilizzo DAX_OOS e il range corretto per out-of-sample
     ylim = c(-0.06, 0.07),  # Limiti dell'asse y
     type = 'l', col = "dark gray", 
     ylab = "Values", 
     xlab = "Time", 
     main = paste("DAX Out-of-Sample\nRendimenti vs r2"))  # \n per nuova linea nel titolo

# Aggiungi la linea per i rendimenti al quadrato
lines(y = r2_DAX[(DAX_training + 1):n], 
      x = times[(DAX_training + 1):n], 
      type = 'l', col = "green")
abline(h = 0, lty = 2)

#legenda
legend('topleft', 
       legend = c("Returns", "r2"), 
       lty = 1, 
       col = c("dark gray", "green"), 
       cex = 0.7)


###MODELLI PER LE PREVISIONI
#ARMA(3,3) GARCH(1,1) t-student
spec_arma33_garch_t <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 3), include.mean = TRUE),
  distribution.model = "std"
)

fit_fore_ARMA33_GARCH_11 <- 
  ugarchfit(spec = spec_arma33_garch_t,
            data = DAX_log_,
            out.sample = n_OOS) #fitting del modello


#ARMA(2,2) GARCH(1,1) t-student
spec_arma22_garch_t <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(2, 2), include.mean = TRUE),
  distribution.model = "std"
)

fit_fore_ARMA22_GARCH_11 <- 
  ugarchfit(spec = spec_arma22_garch_t,
            data = DAX_log_,
            out.sample = n_OOS)

#ARMA(3,3) GJR(1,1)   t-student
spec_gjr <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 3), include.mean = TRUE),
  distribution.model = "std"
)

fit_fore_ARMA33_GJR_11 <- 
  ugarchfit(spec = spec_gjr,
            data = DAX_log_,
            out.sample = n_OOS)

#ARMA(3,3) EGARCH(1,1) t-student
spec_egarch <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 3), include.mean = TRUE),
  distribution.model = "std"
)

fit_fore_ARMA33_EGARCH_11 <- 
  ugarchfit(spec = spec_egarch,
            data = DAX_log_,
            out.sample = n_OOS)


###CRITERI DI INFORMAZIONE
# valutazione dei coefficienti


# criteri di informazione
mods_info_crit <-
  lapply(X = c(fit_fore_ARMA33_GARCH_11,
               fit_fore_ARMA22_GARCH_11,
               fit_fore_ARMA33_EGARCH_11,  
               fit_fore_ARMA33_GJR_11),    
         FUN = infocriteria)

names(mods_info_crit) <- c("ARMA_33_GARCH11", "ARMA_22_GARCH11", "ARMA_33_EGARCH11", "ARMA_33_GJR11")

lapply(mods_info_crit, round, 4)

sort(sapply(mods_info_crit, "[", 2), decreasing = FALSE)

##Confermiamo con i criteri di informazione che il miglior modello per effettuare
##delle previsioni, è l'ARMA (3,3) EGARCH (1,1) distribuito come una t-student

#MSE
# MSE per i modelli specificati
mods_mse <-
  sapply(X = list(fit_fore_ARMA33_GARCH_11@fit$sigma,
                  fit_fore_ARMA22_GARCH_11@fit$sigma,
                  fit_fore_ARMA33_EGARCH_11@fit$sigma,
                  fit_fore_ARMA33_GJR_11@fit$sigma), 
         FUN = function(i){
           mean((i - r2_DAX[1:n_IS])^2)*1000  
         })

names(mods_mse) <- c("ARMA_33_GARCH11", "ARMA_22_GARCH11", "ARMA_33_EGARCH11", "ARMA_33_GJR11")

sort(round(mods_mse, 5), decreasing = FALSE)


##Il miglior modello per la previsione è quello che minimizza il Mean Square Error,
##in questo caso, di nuovo, il modello ARMA(3,3) EGARCH(1,1) t-student.

##########PREVISIONI

###ARMA(3,3) GARCH(1,1)
pred_arma33_garch_11 <- 
  ugarchforecast(fitORspec = fit_fore_ARMA33_GARCH_11,  
                 data = DAX_log_,                   
                 n.ahead = 1,                       
                 n.roll = (n_OOS-1))                    
# media condizionata
pred_arma33_garch_11_cond_mean <-
  pred_arma33_garch_11@forecast$seriesFor
# varianza condizionata
pred_arma33_garch_11_cond_var <-
  pred_arma33_garch_11@forecast$sigmaFor 

###ARMA(2,2) GARCH(1,1)
pred_arma22_garch_11 <- 
  ugarchforecast(fitORspec = fit_fore_ARMA22_GARCH_11,  
                 data = DAX_log_,                   
                 n.ahead = 1,                       
                 n.roll = (n_OOS-1))                    
# media condizionata
pred_arma22_garch_11_cond_mean <-
  pred_arma22_garch_11@forecast$seriesFor
# varianza condizionata
pred_arma22_garch_11_cond_var <-
  pred_arma22_garch_11@forecast$sigmaFor 

###ARMA(3,3) GJR(1,1)
pred_arma33_GJR_11 <- 
  ugarchforecast(fitORspec = fit_fore_ARMA33_GJR_11,  
                 data = DAX_log_,                   
                 n.ahead = 1,                       
                 n.roll = (n_OOS-1))                    
# media condizionata
pred_arma33_GJR_11_cond_mean <-
  pred_arma33_GJR_11@forecast$seriesFor
# varianza condizionata
pred_arma33_GJR_11_cond_var <-
  pred_arma33_GJR_11@forecast$sigmaFor

###ARMA(3,3) EGARCH(1,1)
pred_arma33_EGARCH_11 <- 
  ugarchforecast(fitORspec = fit_fore_ARMA33_EGARCH_11,  
                 data = DAX_log_,                   
                 n.ahead = 1,                       
                 n.roll = (n_OOS-1))                    
# media condizionata
pred_arma33_EGARCH_11_cond_mean <-
  pred_arma33_EGARCH_11@forecast$seriesFor
# varianza condizionata
pred_arma33_EGARCH_11_cond_var <-
  pred_arma33_EGARCH_11@forecast$sigmaFor

####GRAFICI PREVISIONI

plot(y = r2_DAX[(n_IS + 1) : n],
     x = times[(n_IS + 1) : n],
     ylim = c(-0, 0.005),
     type ='l', col = "dark gray",
     ylab = "values",
     xlab = "time",
     main = paste(asset, "Out-of-Sample\nOsservati vs r2"))

lines(y = pred_arma33_EGARCH_11_cond_var^2,
      x = times[(n_IS + 1) : n], times[n + 1],
      type ='l', col = 4)
lines(y = pred_arma33_GJR_11_cond_var^2,
      x = times[(n_IS + 1) : n], times[n + 1],
      type ='l', col = 3)
lines(y = pred_arma33_garch_11_cond_var^2,
      x = times[(n_IS + 1) : n], times[n + 1],
      type ='l', col = 2)
lines(y = pred_arma22_garch_11_cond_var^2,
      x = times[(n_IS + 1) : n], times[n + 1],
      type ='l', col = 5)

legend('topleft',
       c("Volatilità", "ARMA(3,3)-GARCH(1,1)", "GJR(1,1)", "EGARCH(1,1)", "ARMA(2,2)-GARCH(1,1)"),
       lty = 1,
       col = c("dark gray", 2, 3, 4, 5), cex = 0.6)


# MSE
r2_vettore<-as.numeric(r2_DAX[(n_IS+1):n])
mods_mse_oos <- 
  sapply(X = list(pred_arma33_EGARCH_11_cond_var,
                  pred_arma33_GJR_11_cond_var,
                  pred_arma33_garch_11_cond_var,
                  pred_arma22_garch_11_cond_var), 
         FUN = function(i) {
           mean((i - r2_vettore)^2) * 1000  # Calcolo dell'errore quadratico medio (MSE)
         })

names(mods_mse_oos) <- c("ARMA(3,3)-GARCH(1,1)", "GJR(1,1)", "EGARCH(1,1)", "ARMA(2,2)-GARCH(1,1)")

sort(round(mods_mse_oos, 5), decreasing = FALSE)

# Calcola l'errore quadratico medio (MSE)
mods_mse_oos <- 
  sapply(X = list(pred_arma33_EGARCH_11_cond_var,
                  pred_arma33_GJR_11_cond_var,
                  pred_arma33_garch_11_cond_var,
                  pred_arma22_garch_11_cond_var), 
         FUN = function(i) {
           mean((i - r2_vettore)^2) * 1000  # Calcolo dell'errore quadratico medio (MSE)
         })

# Aggiungi i nomi ai modelli e ordina per MSE
names(mods_mse_oos) <- c("ARMA(3,3)-GARCH(1,1)", "GJR(1,1)", "EGARCH(1,1)", "ARMA(2,2)-GARCH(1,1)")
sort(round(mods_mse_oos, 5), decreasing = FALSE)



############ RISK MEASURES

# parametri grafici di default
old.par <- par(no.readonly = TRUE)

# ARMA(3,3) GARCH(1,1) t student
#estrazione residui e varianza condizionata
a_arma33_garch <- residuals(fit_arma33_garch_t)
b_arma33_garch <- sigma(fit_arma33_garch_t)

z_arma33_garch <- a_arma33_garch/b_arma33_garch

# ARMA(2,2) GARCH(1,1) t student
#estrazione residui e varianza condizionata
a_arma22_garch <- residuals(fit_arma22_garch_t)
b_arma22_garch <- sigma(fit_arma22_garch_t)

z_arma22_garch <- a_arma22_garch/b_arma22_garch

# ARMA(3,3) GJR(1,1)
#estrazione residui e varianza condizionata
a_arma33_gjr <- residuals(fit_arma33_gjr)
b_arma33_gjr <- sigma(fit_arma33_gjr)

z_arma33_gjr <- a_arma33_gjr/b_arma33_gjr

# ARMA(3,3)   EGARCH(1,1)
#estrazione residui e varianza condizionata
a_arma33_egarch <- residuals(fit_arma33_egarch)
b_arma33_egarch <- sigma(fit_arma33_egarch)

z_arma33_egarch <- a_arma33_egarch/b_arma33_egarch


#####################ARMA(3,3) GARCH(1,1) t student

#######p-value 0,05
z05_arma33_garch <- quantile(z_arma33_garch, 0.05)
z05_arma33_garch

c05_arma33_garch <- mean(z_arma33_garch[z_arma33_garch < z05_arma33_garch])
c05_arma33_garch

# VaR al 95%
VaR05_arma33_garch <- pred_arma33_garch_11_cond_mean + z05_arma33_garch * pred_arma33_garch_11_cond_var

# ES al 95%
ES05_arma33_garch <- pred_arma33_garch_11_cond_mean + c05_arma33_garch * pred_arma33_garch_11_cond_var

#######p-value 0,01
z01_arma33_garch <- quantile(z_arma33_garch, 0.01)
z01_arma33_garch

# Media dei residui standardizzati al di sotto del quantile al 1%
c01_arma33_garch <- mean(z_arma33_garch[z_arma33_garch < z01_arma33_garch])
c01_arma33_garch

# VaR al 99%
VaR01_arma33_garch <- pred_arma33_garch_11_cond_mean + z01_arma33_garch * pred_arma33_garch_11_cond_var

# ES al 99%
ES01_arma33_garch <- pred_arma33_garch_11_cond_mean + c01_arma33_garch * pred_arma33_garch_11_cond_var

# grafico VaR ed ES oos
plot(y = DAX_OOS,
     x = times[(n_IS + 1) : n],
     ylim = c(-0.09, 0.06),
     type ='l', col = "dark gray",
     ylab = "values",
     xlab = "time",
     main = paste(asset, "Out-of-Sample Returns and\nARMA(3,3)GARCH(1,1) Estimated VaR at 0.05 and 0.01"))
lines(y = VaR05_arma33_garch,
      x = times[(n_IS + 1) : n],
      col = "red")
lines(y = VaR01_arma33_garch,
      x = times[(n_IS + 1) : n],
      col = "dark red")
legend('bottomleft',
       c("Returns", "VaR 0.05", "VaR 0.01"),
       lty = 1,
       col = c("dark gray", "red", "dark red"), cex = 0.7)

#################ARMA(2,2) GARCH(1,1) t-student

#############p-value 0,05
z05_arma22_garch <- quantile(z_arma22_garch, 0.05)
z05_arma22_garch

c05_arma22_garch <- mean(z_arma22_garch[z_arma22_garch < z05_arma22_garch])
c05_arma22_garch

# VaR al 95%
VaR05_arma22_garch <- pred_arma22_garch_11_cond_mean + z05_arma22_garch * pred_arma22_garch_11_cond_var

# ES al 95%
ES05_arma22_garch <- pred_arma22_garch_11_cond_mean + c05_arma22_garch * pred_arma22_garch_11_cond_var

##############p-value 0,01
z01_arma22_garch <- quantile(z_arma22_garch, 0.01)
z01_arma22_garch

# Media dei residui standardizzati al di sotto del quantile al 1%
c01_arma22_garch <- mean(z_arma22_garch[z_arma22_garch < z01_arma22_garch])
c01_arma22_garch

# VaR al 99%
VaR01_arma22_garch <- pred_arma22_garch_11_cond_mean + z01_arma22_garch * pred_arma22_garch_11_cond_var

# ES al 99%
ES01_arma22_garch <- pred_arma22_garch_11_cond_mean + c01_arma22_garch * pred_arma22_garch_11_cond_var

##Grafico VaR e ES
plot(y = DAX_OOS,
     x = times[(n_IS + 1) : n],
     ylim = c(-0.09, 0.06),
     type ='l', col = "dark gray",
     ylab = "values",
     xlab = "time",
     main = paste(asset, "Out-of-Sample Returns and\nARMA(2,2)GARCH(1,1) Estimated VaR at 0.05 and 0.01"))
lines(y = VaR05_arma22_garch,
      x = times[(n_IS + 1) : n],
      col = "red")
lines(y = VaR01_arma22_garch,
      x = times[(n_IS + 1) : n],
      col = "dark red")
legend('bottomleft',
       c("Returns", "VaR 0.05", "VaR 0.01"),
       lty = 1,
       col = c("dark gray", "red", "dark red"), cex = 0.7)

################ARMA(3,3) GJR-GARCH(1,1)


###########p-value 0,05
z05_arma33_gjr <- quantile(z_arma33_gjr, 0.05)
z05_arma33_gjr

#Media dei residui standardizzati al di sotto del quantile al 5%
c05_arma33_gjr <- mean(z_arma33_gjr[z_arma33_gjr < z05_arma33_gjr])
c05_arma33_gjr

#VaR
VaR05_arma33_gjr <- pred_arma33_GJR_11_cond_mean + z05_arma33_gjr * pred_arma33_GJR_11_cond_var

#ES
ES05_arma33_gjr <- pred_arma33_GJR_11_cond_mean + c05_arma33_gjr * pred_arma33_GJR_11_cond_var

############p-value 0,01
z01_arma33_gjr <- quantile(z_arma33_gjr, 0.01)
z01_arma33_gjr

# Media dei residui standardizzati al di sotto del quantile al 1%
c01_arma33_gjr <- mean(z_arma33_gjr[z_arma33_gjr < z01_arma33_gjr])
c01_arma33_gjr

# VaR al 99%
VaR01_arma33_gjr <- pred_arma33_GJR_11_cond_mean + z01_arma33_gjr * pred_arma33_GJR_11_cond_var

# ES al 99%
ES01_arma33_gjr <- pred_arma33_GJR_11_cond_mean + c01_arma33_gjr * pred_arma33_GJR_11_cond_var

##Grafico VaR e ES
plot(y = DAX_OOS,
     x = times[(n_IS + 1) : n],
     ylim = c(-0.09, 0.06),
     type ='l', col = "dark gray",
     ylab = "values",
     xlab = "time",
     main = paste(asset, "Out-of-Sample Returns and\nARMA(3,3)GJR(1,1) Estimated VaR at 0.05 and 0.01"))
lines(y = VaR05_arma33_gjr,
      x = times[(n_IS + 1) : n],
      col = "red")
lines(y = VaR01_arma33_gjr,
      x = times[(n_IS + 1) : n],
      col = "dark red")
legend('bottomleft',
       c("Returns", "VaR 0.05", "VaR 0.01"),
       lty = 1,
       col = c("dark gray", "red", "dark red"), cex = 0.7)

###########################ARMA(3,3) EGARCH(1,1)

########### p-value 0,05 
z05_arma33_egarch <- quantile(z_arma33_egarch, 0.05)
z05_arma33_egarch

# Media dei residui standardizzati al di sotto del quantile al 5%
c05_arma33_egarch <- mean(z_arma33_egarch[z_arma33_egarch < z05_arma33_egarch])
c05_arma33_egarch

# VaR al 95%
VaR05_arma33_egarch <- pred_arma33_EGARCH_11_cond_mean + z05_arma33_egarch * pred_arma33_EGARCH_11_cond_var

# ES al 95%
ES05_arma33_egarch <- pred_arma33_EGARCH_11_cond_mean + c05_arma33_egarch * pred_arma33_EGARCH_11_cond_var

############ p-value 0,01
z01_arma33_egarch <- quantile(z_arma33_egarch, 0.01)
z01_arma33_egarch

# Media dei residui standardizzati al di sotto del quantile al 1%
c01_arma33_egarch <- mean(z_arma33_egarch[z_arma33_egarch < z01_arma33_egarch])
c01_arma33_egarch

# VaR al 99%
VaR01_arma33_egarch <- pred_arma33_EGARCH_11_cond_mean + z01_arma33_egarch * pred_arma33_EGARCH_11_cond_var

# ES al 99%
ES01_arma33_egarch <- pred_arma33_EGARCH_11_cond_mean + c01_arma33_egarch * pred_arma33_EGARCH_11_cond_var

##Grafico VaR e ES
plot(y = DAX_OOS,
     x = times[(n_IS + 1) : n],
     ylim = c(-0.09, 0.06),
     type ='l', col = "dark gray",
     ylab = "values",
     xlab = "time",
     main = paste(asset, "Out-of-Sample Returns and\nARMA(3,3)EGARCH(1,1) Estimated VaR at 0.05 and 0.01"))
lines(y = VaR05_arma33_egarch,
      x = times[(n_IS + 1) : n],
      col = "red")
lines(y = VaR01_arma33_egarch,
      x = times[(n_IS + 1) : n],
      col = "dark red")
legend('bottomleft',
       c("Returns", "VaR 0.05", "VaR 0.01"),
       lty = 1,
       col = c("dark gray", "red", "dark red"), cex = 0.7)


###########################BACKTESTING
# p = 0.05......................................................................
DAX_OOS_n<-as.numeric(DAX_OOS)
# numero violazioni
nviol_05 <-
  c(length(DAX_OOS[DAX_OOS_n < VaR05_arma33_garch]),
    length(DAX_OOS[DAX_OOS_n < VaR05_arma22_garch]),
    length(DAX_OOS[DAX_OOS_n < VaR05_arma33_gjr]),
    length(DAX_OOS[DAX_OOS_n < VaR05_arma33_egarch]))
names(nviol_05) <- c("arma33_GARCH11", "arma22_GARCH11", "arma33_GJR","arma33_EGARCH")
nviol_05

# copertura empirica
phat_05 <- nviol_05/n_OOS
round(phat_05, 4)*100 # valori in %

###Riuscimo a dimostrare che tutti i modelli offrono un VaR stimato correttamente,
###dentro i livelli di confidenza considerati.

# p = 0.01......................................................................

nviol_01 <-
  c(length(DAX_OOS[DAX_OOS_n < VaR01_arma33_garch]),
    length(DAX_OOS[DAX_OOS_n < VaR01_arma22_garch]),
    length(DAX_OOS[DAX_OOS_n < VaR01_arma33_gjr]),
    length(DAX_OOS[DAX_OOS_n < VaR01_arma33_egarch]))
names(nviol_01) <- c("arma33_GARCH11", "arma22_GARCH11", "arma33_GJR","arma33_EGARCH")
nviol_01

# copertura empirica
phat_01 <- nviol_01/n_OOS
round(phat_01, 4)*100 # valori in %

#Dimostriamo anche ad un intervallo di confidenza più elevato (99%) che tutti i 
#modelli calcolano un VaR corretto, tranne l'ARMA(2,2) GARCH(1,1)

##Introduciamo alcune funzioni necessarie per i calcoli successivi
#-------------------------------------------------------------------------------
# Funzione per il calcolo della percentuale di violazioni
#-------------------------------------------------------------------------------

phat <-
  function(x, VaR){
    N = length(x)
    phat = length(x[x < VaR])/N
    phat
  }


#-------------------------------------------------------------------------------
# Funzione per eseguire il test di corretta copertura marginale
# (unconditional coverage test - Kupiec, 1995)
#-------------------------------------------------------------------------------

lrt_uc <-
  function(x, VaR, p0){
    
    # Kupiec's test: returns stat and pval
    
    N = length(x)
    ph = phat(x, VaR)
    ne = ph*N
    
    num = ne*log(p0) + (N-ne)*log(1-p0)
    den = ne*log(ph)+(N-ne)*log(1-ph)
    
    stat = -2*(num - den)
    pval = 1 - pchisq(stat, 1)
    
    stats = c(stat, pval)
    stats = data.frame(stats, row.names = c('statCM', 'pvalCM'))
    stats
  }


#-------------------------------------------------------------------------------
# Funzione per eseguire il test di indipendenza e il test di corretta copertura
# condizionata (conditional coverage test) - Christoffersen, 1998
#-------------------------------------------------------------------------------

lrt_ind <-
  function(x, VaR, stat_uc){
    
    # Indipendence test and Conditional Coverage test:
    # returns stat_ind, pval_ind, stat_cc, pval_cc
    
    hor = length(x)
    check = rep(0, hor)
    count00 = rep(0, hor)
    count01 = rep(0, hor)
    count10 = rep(0, hor)
    count11 = rep(0, hor)
    
    check[x<VaR] = 1
    for (i in (1:(hor-1))){
      if (check[i] == 0 && check[i+1] == 1){
        count01[i] = 1
      } else {
        count01[i] = 0
      }
    }
    n01 = sum(count01)
    
    for (i in (1:(hor-1))){
      if (check[i] == 1 && check[i+1] == 1){
        count11[i] = 1
      } else {
        count11[i] = 0
      }  
    }
    n11 = sum(count11)
    
    for (i in (1:(hor-1))){
      if (check[i] == 0 && check[i+1] == 0){
        count00[i] = 1
      } else {
        count00[i] = 0
      }  
    }
    n00 = sum(count00)
    
    for (i in (1:(hor-1))){
      if (check[i] == 1 && check[i+1] == 0){
        count10[i] = 1
      } else {
        count10[i] = 0
      }  
    }
    n10 = sum(count10)
    
    if (n00 == 0){n00 = 10^-6}
    if (n01 == 0){n01 = 10^-6}
    if (n10 == 0){n10 = 10^-6}
    if (n11 == 0){n11 = 10^-6}
    
    p01 = n01/(n00 + n01)
    p11 = n11/(n10 + n11)
    p2 = (n01 + n11) / (n00 + n01 + n10 + n11)
    
    num = ((1 - p2)^(n00 + n10)) * (p2^(n01 + n11))
    den = ((1 - p01)^(n00)) * (p01^(n01)) * ((1-p11)^(n10)) * (p11^n11)
    
    lnum = (log(1 - p2) * (n00 + n10)) + (log(p2) * (n01 + n11))
    lden =
      (log(1 - p01) * (n00)) + 
      log(p01) * (n01) +
      log(1 - p11) * (n10) +
      log(p11) * n11
    
    lrt_ind = -2 * (lnum - lden)
    
    pvalue1 = 1 - pchisq(lrt_ind,1)
    stat1 = c(lrt_ind, pvalue1)
    
    lrt_cond = lrt_ind + stat_uc
    
    pvalue2 = 1 - pchisq(lrt_cond, 2)
    stat2 = c(lrt_cond, pvalue2)
    
    statID = stat1[1]
    pvalID = stat1[2]
    
    statCC = stat2[1]
    pvalCC = stat2[2]
    
    stats = c(n00, n01, n10, n11, statID, pvalID, statCC, pvalCC)
    
    stats =
      data.frame(stats,
                 row.names = c('n00', 'n01', 'n10', 'n11',
                               'statID', 'pvalID',
                               'statCC', 'pvalCC'))
    
    stats
  }


#-------------------------------------------------------------------------------
# Funzione per il calcolo del VaR to Loss Ratio
#-------------------------------------------------------------------------------

var_to_loss <-
  function(x, VaR){
    #returns mean and minimum var to loss ratio
    
    
    excx = x[x < VaR]
    excvar = VaR[x < VaR]
    
    mexc = mean(excvar/excx)
    minexc = min(excvar/excx)
    
    stats = c(mexc, minexc)
    stats = data.frame(stats,
                       row.names = c('rapp. medio', 'rapp. minimo'))
    
    stats
  }


#####

###Procediamo con i calcoli:

# Unconditional Coverage Test di Kupiec (1995)----------------------------------

# p = 0.05......................................................................

test_uc_05 <- 
  lapply(X = list(VaR05_arma33_garch,
                  VaR05_arma22_garch,
                  VaR05_arma33_gjr,
                  VaR05_arma33_egarch),
         FUN = function(i){
           lrt_uc(x = DAX_OOS_n, VaR = i, p0 = 0.05)
         })
names(test_uc_05) <- c("arma33_garch", "arma22_garch", "arma33_gjr","arma33_egarch")
lapply(test_uc_05, round, 4)

# p = 0.01......................................................................

test_uc_01 <- 
  lapply(X = list(VaR01_arma33_garch,
                  VaR01_arma22_garch,
                  VaR01_arma33_gjr,
                  VaR01_arma33_egarch),
         FUN = function(i){
           lrt_uc(x = DAX_OOS_n, VaR = i, p0 = 0.01)
         })
names(test_uc_01) <- c("arma33_garch", "arma22_garch", "arma33_gjr","arma33_egarch")
lapply(test_uc_01, round, 4)

###Confermiamo che le perdite che superano il VaR rispettano il livello di confidenza
###stimato per il 95% e 99% corrispondentemente, quindi la frequenza delle violazioni
###osservate è compatibile con quella attesa.

# Conditional Coverage Test di Christoffersen (1998)----------------------------

# p = 0.05......................................................................

test_cc_05 <- 
  lapply(X = list(VaR05_arma33_garch,
                  VaR05_arma22_garch,
                  VaR05_arma33_gjr,
                  VaR05_arma33_egarch),
         FUN = function(i){
           lrt_ind(x = DAX_OOS_n , VaR = i, stat_uc = 0.05)
         })
names(test_cc_05) <- c("arma33_garch", "arma22_garch", "arma33_gjr","arma33_egarch")
lapply(test_cc_05, round, 4)


# p = 0.01......................................................................

test_cc_01 <- 
  lapply(X = list(VaR01_arma33_garch,
                  VaR01_arma22_garch,
                  VaR01_arma33_gjr,
                  VaR01_arma33_egarch),
         FUN = function(i){
           lrt_ind(x = DAX_OOS_n, VaR = i, stat_uc = 0.01)
         })
names(test_cc_01) <- c("arma33_garch", "arma22_garch", "arma33_gjr","arma33_egarch")
lapply(test_cc_01, round, 4)

###I valori pvalID e pvalCC per ogni modello sono alti,pertanto, non vi sono evidenze
###significative di violazioni raggruppate nel tempo per entrambi i livelli di confidenza

# Rapporti VaR/perdita non coperta----------------------------------------------

# 0.05
vtl05 <-
  lapply(X = list(VaR05_arma33_garch,
                  VaR05_arma22_garch,
                  VaR05_arma33_gjr,
                  VaR05_arma33_egarch),
         FUN = function(i){
           var_to_loss(x = DAX_OOS_n, VaR = i)
         })
names(vtl05) <- c("arma33_garch", "arma22_garch", "arma33_gjr","arma33_egarch")
lapply(vtl05, round, 4)

# 0.01
vtl01 <-
  lapply(X = list(VaR01_arma33_garch,
                  VaR01_arma22_garch,
                  VaR01_arma33_gjr,
                  VaR01_arma33_egarch),
         FUN = function(i){
           var_to_loss(x = DAX_OOS_n, VaR = i)
         })
names(vtl01) <- c("arma33_garch", "arma22_garch", "arma33_gjr","arma33_egarch")
lapply(vtl01, round, 4)

###I modelli stimati forniscono stime conservative e prudenti, con valori medi del rapporto VaR/perdita
###che garantiscono un margine di sicurezza; nei giorni più estremi, il modello di VaR al livello 5% 
###talvolta sottostima il rischio, mentre il livello di confidenza all' 1% sembra offrire una copertura più robusta.


# plot VaR----------------------------------------------------------------------

plot(y = DAX_OOS, x = times[(n_IS + 1):n], type = 'l', ylim = c(-0.12, 0.06), col = 'dark grey',
     main = 'Value at Risk - 5%, 1% - ARMA (2,2) GARCH, ARMA (3,3) GARCH, GJR, EGARCH')
lines(y = VaR05_arma33_garch, x = times[(n_IS + 1):n], col = 'blue', lty = 1)
lines(y = VaR01_arma33_garch, x = times[(n_IS + 1):n], col = 'blue', lty = 2)
lines(y = VaR05_arma22_garch, x = times[(n_IS + 1):n], col = 'green', lty = 1)
lines(y = VaR01_arma22_garch, x = times[(n_IS + 1):n], col = 'green', lty = 2)
lines(y = VaR05_arma33_gjr, x = times[(n_IS + 1):n], col = 'purple', lty = 1)
lines(y = VaR01_arma33_gjr, x = times[(n_IS + 1):n], col = 'purple', lty = 2)
lines(y = VaR05_arma33_egarch, x = times[(n_IS + 1):n], col = 'orange', lty = 1)
lines(y = VaR01_arma33_egarch, x = times[(n_IS + 1):n], col = 'orange', lty = 2)
legend('bottomleft',
       legend = c('Rendimenti',
                  'VaR 5% ARMA(2,2)-GARCH', 'VaR 1% ARMA(2,2)-GARCH',
                  'VaR 5% ARMA(3,3)-GARCH', 'VaR 1% ARMA(3,3)-GARCH',
                  'VaR 5% GJR-GARCH', 'VaR 1% GJR-GARCH',
                  'VaR 5% EGARCH', 'VaR 1% EGARCH'),
       col = c('dark grey', 'green', 'green', 'blue', 'blue', 'purple', 'purple', 'orange', 'orange'),
       lty = c(1, 1, 2, 1, 2, 1, 2, 1, 2),
       ncol = 2,
       cex = 0.55)

# plot ES-----------------------------------------------------------------------

plot(y = DAX_OOS, x = times[(n_IS + 1):n], type = 'l', ylim = c(-0.12, 0.06), col = 'dark grey',
     main = 'Expected Shortfall - 5%, 1%\nARMA(2,2) GARCH, ARMA(3,3) GARCH, GJR, EGARCH')
lines(y = ES05_arma33_garch, x = times[(n_IS + 1):n], col = 'blue', lty = 1)
lines(y = ES01_arma33_garch, x = times[(n_IS + 1):n], col = 'blue', lty = 2)
lines(y = ES05_arma22_garch, x = times[(n_IS + 1):n], col = 'green', lty = 1)
lines(y = ES01_arma22_garch, x = times[(n_IS + 1):n], col = 'green', lty = 2)
lines(y = ES05_arma33_gjr, x = times[(n_IS + 1):n], col = 'purple', lty = 1)
lines(y = ES01_arma33_gjr, x = times[(n_IS + 1):n], col = 'purple', lty = 2)
lines(y = ES05_arma33_egarch, x = times[(n_IS + 1):n], col = 'orange', lty = 1)
lines(y = ES01_arma33_egarch, x = times[(n_IS + 1):n], col = 'orange', lty = 2)
legend('bottomleft',
       legend = c('Rendimenti',
                  'ES 5% ARMA(2,2)-GARCH', 'ES 1% ARMA(2,2)-GARCH',
                  'ES 5% ARMA(3,3)-GARCH', 'ES 1% ARMA(3,3)-GARCH',
                  'ES 5% GJR-GARCH', 'ES 1% GJR-GARCH',
                  'ES 5% EGARCH', 'ES 1% EGARCH'),
       col = c('dark grey', 'green', 'green', 'blue', 'blue', 'purple', 'purple', 'orange', 'orange'),
       lty = c(1, 1, 2, 1, 2, 1, 2, 1, 2),
       ncol = 2,
       cex = 0.45)

