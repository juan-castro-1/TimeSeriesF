# Librerias #
library(readxl)
library(boot)
library(dplyr)
library(zoo)
library(lmtest)
library(dyn)
library(lubridate)
library(ggplot2)
library(lmtest)
library(dyn)
library(forecast) 
library(dplyr)
library(hrbrthemes)
library(plotly)
library(patchwork)
library(babynames)
library(viridis)
library(urca)
library(fastDummies)
library(vars)
library(tseries)
library(normtest)
library(fGarch)
library(aTSA)
library(rmgarch)
library(rugarch)
library(dynlm)
library(egcm)
library(gets)

library(tseries)
library(normtest)
library(fGarch)
library(aTSA)
library(rmgarch)
library(rugarch)
library(psych)


library(tidyverse)

##########################################################################################################################
################################################# Final Series de Tiempo #################################################
##########################################################################################################################
#----------------------------------------------------> Juan Castro

################################################################################################
#-----------------------------------------------------------------------------------------------
#------------------------------------------ PUNTO UNO ------------------------------------------
#-----------------------------------------------------------------------------------------------
################################################################################################
setwd("C://Users//juan_//Desktop//Time-Series//Final_11.08")
dir()
base <- read_xlsx("Base_M1.xlsx")
colnames(base)
options(scipen = 999)
#summary(base) 
M_1 = ts(base$M_1, frequency=12, start=c(2005,1)  , end=c(2020,5) )
M_2 = ts(base$M_2, frequency=12, start=c(2005,1)  , end=c(2020,5) )
Tasa = ts(base$Tasa, frequency=12, start=c(2005,1), end=c(2020,5) )
TC   = ts(base$TC, frequency=12, start=c(2005,1), end=c(2020,5) )



#------------------------------------
#---------- Punto 1.a ---------------
#------------------------------------
pM1 = plot(M_1, main="M1")
pM2 = plot.ts(M_2, main="M2")  
plot(Tasa)

summary(base)
## plot the obs ts, trend & seasonal effect
plot.ts(cbind(M_1,  diff(log(M_1))),  main = "M1", yax.flip = TRUE)
plot(cbind(M_2,  diff(log(M_2))),  main = "M2", yax.flip = TRUE)
plot(cbind(Tasa, diff(log(Tasa))), main = "Tasa de Interes", yax.flip = TRUE)
#plot.ts(M_1, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")

in_sample_M1 <- M_1[1:120]    #[1:185]
in_sample_M1 <- ts(in_sample_M1, start=c(2005,1), end=c(2014,12), frequency = 12)
plot(in_sample_M1)

out_of_sample_M1 <- M_1[120:185] #arranqué un mes antes porque después tomo diferencias
out_of_sample_M1 <- ts(out_of_sample_M1, start=c(2015,1), end=c(2020,5), frequency = 12)
plot(out_of_sample_M1)

SFGfadfg<- out_of_sample_M1[1:65]-M_1[121:185]

in_sample_M2 <- M_2[1:120]    #[1:185]
in_sample_M2 <- ts(in_sample_M2, start=c(2005,1), end=c(2014,12), frequency = 12)
plot(in_sample_M2)

out_of_sample_M2 <- M_2[120:185] #arranqué un mes antes porque después tomo diferencias
out_of_sample_M2 <- ts(out_of_sample_M2, start=c(2015,1), end=c(2020,5), frequency = 12)
plot(out_of_sample_M2)

in_sample_Tasa <- Tasa[1:120]    #[1:185]
in_sample_Tasa <- ts(in_sample_Tasa, start=c(2005,1), end=c(2014,12), frequency = 12)
plot(in_sample_Tasa)

out_of_sample_Tasa <- Tasa[120:185] #arranqué un mes antes porque después tomo diferencias
out_of_sample_Tasa <- ts(out_of_sample_Tasa, start=c(2015,1), end=c(2020,5), frequency = 12)
plot(out_of_sample_Tasa)

#in_sample_TC <- TC[1:120]    #[1:185]
#in_sample_TC <- ts(in_sample_TC, start=c(2005,1), end=c(2014,12), frequency = 12)
#plot(in_sample_TC)
#
#out_of_sample_TC <- TC[120:185] #arranqué un mes antes porque después tomo diferencias
#out_of_sample_TC <- ts(out_of_sample_TC, start=c(2015,1), end=c(2020,5), frequency = 12)
#plot(out_of_sample_TC)



#------------------------------------
#---------- Punto 1.b ---------------
#------------------------------------
#test de integracion/estacionalidad

#----------------
#---- ADickyFuller
#----------------

#------
#---M1
#------

df2<-ur.df(log(M_1),type="drift",selectlags="BIC")
summary(df2)
df3<-ur.df(log(M_1),type="trend",selectlags="BIC")
summary(df3)

d.df2<-ur.df(diff(log(M_1)),type="drift",selectlags="BIC")
summary(d.df2)
d.df3<-ur.df(diff(log(M_1)),type="trend",selectlags="BIC")
summary(d.df3)

#====> M1 es I(1)


#-----
#---M2
#-----

df2_m2<-ur.df(log(M_2),type="drift",selectlags="BIC")
summary(df2_m2)
df3_m2<-ur.df(log(M_2),type="trend",selectlags="BIC")
summary(df3_m2)

d.df2_m2<-ur.df(diff(log(M_2)),type="drift",selectlags="BIC")
summary(d.df2_m2)
d.df3_m2<-ur.df(diff(log(M_2)),type="trend",selectlags="BIC")
summary(d.df3_m2)

#====> M2 es I(1)


#--------
#---Tasa
#--------
plot(log(Tasa))

df2_tasa<-ur.df(log(Tasa),type="drift",selectlags="BIC")
summary(df2_tasa)
df3_tasa<-ur.df(log(Tasa),type="trend",selectlags="BIC")
summary(df3_tasa)

d.df2_tasa<-ur.df(diff(log(Tasa)),type="drift",selectlags="BIC")
summary(d.df2_tasa)
d.df3_tasa<-ur.df(diff(log(Tasa)),type="trend",selectlags="BIC")
summary(d.df3_tasa)

#====> M2 es I(1)

#-------------------
#-----PhillipsPerron
#-------------------
#si hay tiempo hacer mas#



############################################################################
#---------------------------------------------------------------------------
############################################################################
#----------------------------- ARMA(p,q) -----------------------------------
############################################################################
#---------------------------------------------------------------------------
############################################################################
# MODELO
dM_1 = diff(log(in_sample_M1))*100
plot(in_sample_M1)
plot(dM_1)
acf(dM_1)   
pacf(dM_1)  


# DOS MODELOS ----> # modelo1 (0,0,1)(0,0,1)[12] BIC
                    # modelo2 (2,0,2)(0,0,1)[12] AIC


#########################################################
########################## BIC ##########################
#########################################################
#--> https://robjhyndman.com/hyndsight/out-of-sample-one-step-forecasts/
?predict
#-------------------------------
#-> FORECAST ESQUEMA FIJO ------
#-------------------------------

dl_in_M1 <- diff(log(M_1[1:120]))*100
plot.ts(dl_in_M1)
dl_out_M1 <- diff(log(M_1[120:185]))*100
plot.ts(dl_out_M1)
dl_out_M1 <- ts(dl_out_M1, start=c(2015,2), end=c(2020,5), frequency = 12)
dl_in_M1 <-  ts(dl_in_M1, start=c(2005,2), end=c(2014,12), frequency = 12)

acf( dl_in_M1, main="ACF M1")   
pacf(dl_in_M1, main="PACF M1")  

?auto.arima
mod1 = auto.arima(dl_in_M1, ic=c("bic")); mod1
modelo1 = arima(dl_in_M1, order=c(0,0,1), seasonal=list(order=c(0,1,1), period=12)); modelo1
library(stargazer)
stargazer(modelo1, title = "Modelo 1", type = "latex")
Box.test(modelo1$residuals, lag=12, type="Ljung-Box")
summary(modelo1)

fit_X <- Arima(dl_in_M1,order=c(0,0,1),seasonal=list(order=c(0,1,1),period=12))
fit2_X <- Arima(dl_out_M1, model=fit_X)
onestep_X <- fitted(fit2_X)
plot(onestep_X)

par(mar= c(2, 2, 1, 2))
plot.ts(onestep_X, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(dl_out_M1, col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Diferencia Logaritmica")
legend("topleft",c("M1","ARMA Bic Fijo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

fcst_fit_fijo_log_dif_bic <- diff(log(M_1[121:185]))+onestep_X
fcst_fit_fijo_log_dif_bic <- fcst_fit_fijo_log_dif_bic/100
fcst_fit_fijo_log_bic <- fcst_fit_fijo_log_dif_bic + log(out_of_sample_M1)

fcst_fit_fijo_bic <- exp(fcst_fit_fijo_log_bic)
fcst_fit_fijo_bic <- ts(fcst_fit_fijo_bic,frequency=12,start=c(2015,2))#, end = c(2020,4))

par(mar= c(2, 2, 1, 2))
plot.ts(fcst_fit_fijo_bic, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(out_of_sample_M1[3:65], col="red", bty = "n", xlab = "periodos", ylab = "Pesos",bty="n",main="Nivel")
legend("topleft",c("M1","ARMA Bic Fijo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))


hist(mod1$residuals)

#-------------------------------
#---> EVALUO SESGO BIC----------
#-------------------------------

error.bic <- diff(log(M_1[121:185]))*100 - onestep_X
plot(error.bic)
summary(lm(error.bic~1)) #no hay evidencia de sesgo sistemático
isat(error.bic, iis=TRUE, sis=FALSE, tis=FALSE, t.pval=1/length(error.bic), plot = TRUE) #aplico IIS con un target size de 1/29
isat(error.bic, iis=FALSE, sis=TRUE, tis=FALSE, t.pval=1/length(error.bic), plot = TRUE) #applico SIS con un target size de 1/29
isat(error.bic, iis=FALSE, sis=FALSE, tis=TRUE, t.pval=1/(length(error.bic)), plot = TRUE) #applico IIS+SIS con un target size de 1/(2*29)

?isat

#-------------------------------
#-> FORECAST ESQUEMA ADAP ------ (recursivo)
#-------------------------------
#-->  https://robjhyndman.com/hyndsight/rolling-forecasts/
fcst_roll_bic <- matrix(0, nrow=64, ncol=1)
for(i in 1:64)
{  
  x_bic <- window(diff(log(M_1)),  end=2014.99 + (i-1)/12)
  refit_bic <- Arima(x_bic, order=c(0,0,1),seasonal=list(order=c(0,1,1),period=12))
  fcst_roll_bic[i,] <- forecast(refit_bic, h=1)$mean
}

par(mar= c(2, 2, 1, 2))
plot.ts(fcst_roll_bic, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(diff(log(M_1[120:185]))*100, col="red", bty = "n", xlab = "", ylab = "",bty="n", main ="Diferencia Logaritmica")
legend("topleft",c("M1","ARMA Bic Recursivo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))


fcst_fit_roll_log_dif_bic <- diff(log(M_1[121:185]))*100+fcst_roll_bic
fcst_fit_roll_log_dif_bic <- fcst_fit_roll_log_dif_bic/100
fcst_fit_roll_log_bic <- fcst_fit_roll_log_dif_bic + log(out_of_sample_M1[2:65])

fcst_fit_roll_bic <- exp(fcst_fit_roll_log_bic)
fcst_fit_roll_bic <- ts(fcst_fit_roll_bic,frequency=12,start=c(2015,2))#, end = c(2020,3))

par(mar= c(2, 2, 1, 2))
plot.ts(fcst_fit_roll_bic, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(out_of_sample_M1[4:65], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel")
legend("topleft",c("M1","ARMA Bic Recursivo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))



#-------------------------------
#---> EVALUO SESGO BIC---------- (recursivo)
#-------------------------------

error.bic.roll <- diff(log(M_1[121:185]))*100 - fcst_roll_bic
plot.ts(error.bic.roll)
summary(lm(error.bic.roll~1)) #no hay evidencia de sesgo sistemático
isat(error.bic.roll, iis=TRUE, sis=FALSE, tis=FALSE, t.pval=1/length( error.bic.roll), plot = TRUE) #aplico IIS con un target size de 1/29
isat(error.bic.roll, iis=FALSE, sis=TRUE, tis=FALSE, t.pval=1/length( error.bic.roll), plot = TRUE) #applico SIS con un target size de 1/29
isat(error.bic.roll, iis=FALSE, sis=FALSE, tis=TRUE, t.pval=1/(length(error.bic.roll)), plot = TRUE) #applico IIS+SIS con un target size de 1/(2*29)



#########################################################
########################## AIC ##########################
#########################################################

#-------------------------------
#-> FORECAST ESQUEMA FIJO ------
#-------------------------------

mod2 = auto.arima(dl_in_M1, ic=c("aic")); mod2
modelo2 = Arima(dl_in_M1, order=c(2,0,2), seasonal=list(order=c(0,1,1), period=12)); modelo2
library(stargazer)
summary(modelo2)
stargazer(modelo2, title = "Modelo 2", type = "latex")
Box.test(modelo2$residuals, lag=12, type="Ljung-Box")
summary(modelo2)

fit_2 <- Arima(dl_in_M1, order=c(2,0,2),seasonal=list(order=c(0,1,1),period=12))
fit2_2 <- Arima(dl_out_M1, model=fit_2)
onestep_2 <- fitted(fit2_2)

par(mar= c(2, 2, 1, 2))
plot.ts(onestep_2, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(dl_out_M1, col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Diferencia Logaritmica")
legend("topleft",c("M1","ARMA Aic Fijo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))


fcst_fit_fijo_log_dif_aic <- diff(log(M_1[121:185]))*100+onestep_2
fcst_fit_fijo_log_dif_aic <- fcst_fit_fijo_log_dif_aic/100
fcst_fit_fijo_log_aic <- fcst_fit_fijo_log_dif_aic + log(out_of_sample_M1)
  
fcst_fit_fijo_aic <- exp(fcst_fit_fijo_log_aic)
fcst_fit_fijo_aic <- ts(fcst_fit_fijo_aic,frequency=12,start=c(2015,2))#, end = c(2020,4))

par(mar= c(2, 2, 1, 2))
plot.ts(fcst_fit_fijo_aic, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(out_of_sample_M1[2:65], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel")
legend("topleft",c("M1","ARMA Aic Fijo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

#-------------------------------
#---> EVALUO SESGO AIC----------
#-------------------------------

error.aic <- dl_out_M1-onestep_2
plot(error.aic)
summary(lm(error.bic~1)) #no hay evidencia de sesgo sistemático
isat(error.aic, iis=TRUE, sis=FALSE, tis=FALSE, t.pval=1/length(error.aic), plot = TRUE) #aplico IIS con un target size de 1/29
isat(error.aic, iis=FALSE, sis=TRUE, tis=FALSE, t.pval=1/length(error.aic), plot = TRUE) #applico SIS con un target size de 1/29
isat(error.aic, iis=FALSE, sis=TRUE, tis=FALSE, t.pval=1/(2*length(error.aic)), plot = TRUE) #applico IIS+SIS con un target size de 1/(2*29)



#-------------------------------
#-> FORECAST ESQUEMA ADAP ------ (recursivo)
#-------------------------------

fcst_roll_aic <- matrix(0, nrow=65, ncol=1)
for(i in 1:65)
{  
  x_aic <- window(diff(log(M_1)),  end=2014.99 + (i-1)/12)
  refit_aic <- Arima(x_aic, order=c(2,0,2),seasonal=list(order=c(0,1,1),period=12))
  fcst_roll_aic[i,] <- forecast(refit_aic, h=1)$mean
}

par(mar= c(2, 2, 1, 2))
plot.ts(fcst_roll_aic, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(diff(log(M_1[120:185])), col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Diferencia Logaritmica")
legend("topleft",c("M1","ARMA Aic Recursivo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))


fcst_fit_roll_log_dif_aic <- diff(log(M_1[120:185]))+fcst_roll_aic
fcst_fit_roll_log_dif_aic <- fcst_fit_roll_log_dif_aic/100
fcst_fit_roll_log_aic <- fcst_fit_roll_log_dif_aic + log(out_of_sample_M1)

fcst_fit_roll_aic <- exp(fcst_fit_roll_log_aic)
fcst_fit_roll_aic <- ts(fcst_fit_roll_aic,frequency=12,start=c(2015,2))

par(mar= c(2, 2, 1, 2))
plot.ts(fcst_fit_roll_aic, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(out_of_sample_M1[3:65], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel")
legend("topleft",c("M1","ARMA Aic Recursivo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

#-------------------------------
#---> EVALUO SESGO AIC---------- (recursivo)
#-------------------------------

error.aic.roll <- diff(log(M_1[120:185]))-fcst_roll_aic
plot.ts(error.aic.roll)
summary(lm(error.aic.roll~1)) #no hay evidencia de sesgo sistemático
isat(error.aic.roll, iis=TRUE, sis=FALSE, tis=FALSE,   t.pval=1/length( error.aic.roll), plot = TRUE) #aplico IIS con un target size de 1/29
isat(error.aic.roll, iis=FALSE, sis=TRUE, tis=FALSE,   t.pval=1/length( error.aic.roll), plot = TRUE) #applico SIS con un target size de 1/29
isat(error.aic.roll, iis=FALSE, sis=TRUE, tis=FALSE, t.pval=1/(2*length(error.aic.roll)), plot = TRUE) #applico IIS+SIS con un target size de 1/(2*29)




############################################################################
#---------------------------------------------------------------------------
############################################################################
#----------------------------- ADL -----------------------------------------
############################################################################
#---------------------------------------------------------------------------
############################################################################
library(dynlm)
plot(in_sample_M1)
adlM_1 = diff(log(in_sample_M1))*100
plot(adlM_1)
adlM_2 = diff(log(in_sample_M2))*100
plot(adlM_2)
adlTasa = diff(log(in_sample_Tasa))*100
plot(adlTasa)
acf(adlM_1)
pacf(adlM_1)

?dynlm

ADL <- dynlm(adlM_1 ~ L(adlM_1, 1) + L(adlM_2) + L(adlM_1, 12) + dummies) ; ADL
ADL <- dynlm(adlM_1 ~ L(adlM_1, 1) + L(adlM_2) + L(adlM_1, 12) + L(adlM_2, 12) + dummies) ; ADL

stargazer(ADL, title = "Modelo 3", type = "latex")

resid_ADL <- resid(ADL)
plot.ts(resid_ADL, xlab=", ylab=")
acf(resid_ADL)
pacf(resid_ADL)
summary(egcm(M_1[1:120], M_2[1:120]))#, Tasa[2:120]))


adl_fcst_fijo <- ADL$coefficients[1] + ADL$coefficients[2]*lag(dl_out_M1, 1) + ADL$coefficients[3]*lag(diff(log(out_of_sample_M2))*100, 1) + ADL$coefficients[4]*lag(diff(log(out_of_sample_Tasa))*100, 1) + ADL$coefficients[5]*lag(diff(log(out_of_sample_M1))*100, 12)

plot(adl_fcst_fijo)


fcst_fit_fijo_log_dif_adl <- diff(log(M_1[120:173]))*100+adl_fcst_fijo
fcst_fit_fijo_log_dif_adl <- fcst_fit_fijo_log_dif_adl/100
fcst_fit_fijo_log_adl <- fcst_fit_fijo_log_dif_adl + log(out_of_sample_M1)

fcst_fit_fijo_adl <- exp(fcst_fit_fijo_log_adl )
fcst_fit_fijo_adl <- ts(fcst_fit_fijo_adl,frequency=12,start=c(2015,1))

par(mar= c(2, 2, 1, 2))
plot.ts(fcst_fit_fijo_adl, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(M_1[121:174], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel")
legend("topleft",c("M1","ADL Fijo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

#-------------------------------
#---> EVALUO SESGO AIC---------- 
#-------------------------------

error.adl.fijo <- M_1[122:174]-fcst_fit_fijo_adl
plot.ts(error.adl.fijo )
summary(lm(error.adl.fijo ~1)) #no hay evidencia de sesgo sistemático
isat(error.adl.fijo , iis=TRUE, sis=FALSE, tis=FALSE,   t.pval=1/length( error.adl.fijo ), plot = TRUE) #aplico IIS con un target size de 1/29
isat(error.adl.fijo , iis=FALSE, sis=TRUE, tis=FALSE,   t.pval=1/length( error.adl.fijo ), plot = TRUE) #applico SIS con un target size de 1/29
isat(error.adl.fijo , iis=FALSE, sis=TRUE, tis=FALSE, t.pval=1/(2*length(error.adl.fijo )), plot = TRUE) #applico IIS+SIS con un target size de 1/(2*29)




# FORECAST ESQUEMA ADAPTATIVO
# recursivo
dl_out_M2 <- diff(log(out_of_sample_M2))*100
dl_out_Tasa <- diff(log(out_of_sample_Tasa))*100

newdata <- data.frame(dl_out_M1, dl_out_M2, dl_out_Tasa)
cv <- predict(ADL, newdata)
cv<- ts(cv,frequency=12,start=c(2015,2), end=c(2020,5))
plot(cv)
par(mar= c(2, 2, 1, 2))
plot.ts(cv, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(dl_out_M1, col="red", bty = "n", xlab = "", ylab = "",bty="n")

fcst_fit_rec_log_dif_adl <- diff(log(M_1[121:185]))*100+cv
fcst_fit_rec_log_dif_adl <- fcst_fit_rec_log_dif_adl/100
fcst_fit_rec_log_adl <- fcst_fit_rec_log_dif_adl + log(out_of_sample_M1)

fcst_fit_rec_adl <- exp(fcst_fit_rec_log_adl )
fcst_fit_rec_adl <- ts(fcst_fit_rec_adl,frequency=12,start=c(2015,2))  #, end = c(2020,4))

par(mar= c(2, 2, 1, 2))
plot.ts(fcst_fit_rec_adl, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(M_1[122:185], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel")
legend("topleft",c("M1","ADL Recursivo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))


#-------------------------------
#---> EVALUO SESGO AIC---------- 
#-------------------------------

error.adl.rec <- M_1[122:185]-fcst_fit_rec_adl
plot.ts(error.adl.rec )
summary(lm(error.adl.rec ~1)) #no hay evidencia de sesgo sistemático
isat(error.adl.rec , iis=TRUE, sis=FALSE, tis=FALSE,   t.pval=1/length( error.adl.rec ), plot = TRUE) #aplico IIS con un target size de 1/29
isat(error.adl.rec , iis=FALSE, sis=TRUE, tis=FALSE,   t.pval=1/length( error.adl.rec ), plot = TRUE) #applico SIS con un target size de 1/29
isat(error.adl.rec , iis=FALSE, sis=TRUE, tis=FALSE, t.pval=1/(2*length(error.adl.rec )), plot = TRUE) #applico IIS+SIS con un target size de 1/(2*29)




 
# rolling
?isat

#-------------------------------
#---> EVALUO SESGO -------------
#-------------------------------



############################################################################
#---------------------------------------------------------------------------
############################################################################
#----------------------------- VAR -----------------------------------------
############################################################################
#---------------------------------------------------------------------------
############################################################################

# MODELO
#Genero un vector "y" de variables endógenas (asumiendo que son estacionarias):
y_2 <- cbind(VAR_M1=diff(log(M_1[1:120]))*100,
             VAR_M2=diff(log(M_2[1:120]))*100,
             VAR_Tasa=diff(log(Tasa[1:120]))*100)

###########################
dl_in_M2   <- diff(log(in_sample_M2))*100
dl_in_Tasa <- diff(log(in_sample_Tasa))*100
#dl_in_TC   <- diff(log(in_sample_TC))*100
isat(dl_in_Tasa, iis=TRUE, sis=FALSE, tis=FALSE, t.pval = 1/length(diff(log(M_1[1:120]))*100), plot=TRUE)
isat(dl_in_Tasa, iis=FALSE, sis=TRUE, tis=FALSE, t.pval = 1/length(diff(log(M_1[1:120]))*100), plot=TRUE)
isat(dl_in_Tasa, iis=FALSE, sis=FALSE, tis=TRUE, t.pval = 1/length(diff(log(M_1[1:120]))*100), plot=TRUE)

library(tstools) #útil para crear dummies temporales
d1 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2009,2 ), dummy_end = c(2009,2 ), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d2 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2007,8 ), dummy_end = c(2007,8 ), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d3 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2007,12), dummy_end = c(2007,12), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d4 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2008,12), dummy_end = c(2008,12), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d5 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2009,12), dummy_end = c(2009,12), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d6 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2010,12), dummy_end = c(2010,12), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d7 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2011,12), dummy_end = c(2011,12), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d8 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2012,12), dummy_end = c(2012,12), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d9 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2013,12), dummy_end = c(2013,12), sp = TRUE, start_basic = c(2005,2), frequency = 12)

d11 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2011,1) , dummy_end = c(2011,1), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d12 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2011,2) , dummy_end = c(2011,2), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d13 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2012,1) , dummy_end = c(2012,1), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d14 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2012,2) , dummy_end = c(2012,2), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d15 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2013,1) , dummy_end = c(2013,1), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d16 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2013,2) , dummy_end = c(2013,2), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d17 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2014,1) , dummy_end = c(2014,1), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d18 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2014,2) , dummy_end = c(2014,2), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d19 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2014,5) , dummy_end = c(2014,5), sp = TRUE, start_basic = c(2005,2), frequency = 12)

d26 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2008,5 ) , dummy_end = c(2008,5 ), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d27 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2008,6 ) , dummy_end = c(2008,6 ), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d28 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2008,8 ) , dummy_end = c(2008,8 ), sp = TRUE, start_basic = c(2005,2), frequency = 12)
d29 <-create_dummy_ts(end_basic = c(2014,12), dummy_start = c(2008,11) , dummy_end = c(2008,11), sp = TRUE, start_basic = c(2005,2), frequency = 12)

dummies <- cbind(d1 =d1 ,
                 d2 =d2 ,
                 d3 =d3 ,
                 d4 =d4 ,
                 d5 =d5 ,
                 d6 =d6 ,
                 d7 =d7 ,
                 d8 =d8 ,
                 d9 =d9 ,
                 d11 =d11 ,
                 d12 =d12 ,
                 d13 =d13 ,
                 d14 =d14 ,
                 d15 =d15 ,
                 d16 =d16 ,
                 d17 =d17 ,
                 d18 =d18 ,
                 d19 =d19 ,
                 d26 =d26 ,
                 d27 =d27 ,
                 d28 =d28 ,
                 d29 =d29 
                 )


#Selección de longitud de rezagos óptima
VARselect(y_2, type = "const", exogen = dummies)

#VARs
var1 <- VAR(y_2, p=1, type = "const",  exogen = dummies)
summary(var1)
library(stargazer)
stargazer(var1, title = "Modelo Var", type = "text")
serial.test(var1, lags.pt = 12, type = "PT.asymptotic")  # H0: error es W.N
normality.test(var1, multivariate.only=TRUE) 

var6_2 <- VAR(y_2, p=6, type = "const",  exogen = dummies)
summary(var6_2)
serial.test(var6_2, lags.pt = 12, type = "PT.asymptotic")  # H0: error es W.N
normality.test(var6_2, multivariate.only=TRUE)


######################


?predict
# FORECAST ESQUEMA ADAPTATIVO
var6_sin_d <- VAR(y_2, p=1, type = "const")
fcst_VAR_2 <- predict(var6_sin_d, n.ahead = 12); fcst_VAR_2
plot(fcst_VAR_2)
fanchart(fcst_VAR_2, plot.type = c("multiple"))


#matriz cov-corr
sum_var6 = summary(var6_2)
sum_var6$covres
sum_var6$corres
#Transformación de Choleski:
t(chol(sum_var6$corres))


#Funciones Impulso-Respuesta:
fir1 <-  irf(var6_sin_d, impulse = "VAR_M2",   response = c("VAR_M1", "VAR_Tasa" ), n.ahead = 8, ortho = TRUE, runs = 1000)
fir2 <- irf(var6_sin_d, impulse = "VAR_Tasa", response = "VAR_M1", n.ahead = 8,  ortho =  TRUE, runs = 1000)
fir3 <- irf(var6_sin_d, impulse = "VAR_Tasa", response = "VAR_M1", n.ahead = 8,  ortho =  TRUE, runs = 1000)
fir4 <- irf(var6_sin_d, impulse = "VAR_Tasa", response = "VAR_M2", n.ahead = 8,  ortho =  TRUE, runs = 1000)

plot(fir1)

plot(fir1, main="FIR de M1, shock de M2", ylab="M1")

plot(fir3, main="FIR de M1, shock de Tasa", ylab="M1")

plot(fir2, main="FIR de Tasa, shock de M2", ylab="Tasa")
plot(fir4, main="FIR de M2, shock de Tasa", ylab="M2")

#Causalidad en sentido de Granger
#causality(var6, cause = "VAR_dM_1")  
#causality(var6, cause = "VAR_dM_2")  
causality(var6_2, cause = c("VAR_Tasa","VAR_M2" )) 
causality(var1, cause = c("VAR_Tasa","VAR_M2" )) 


efe <- predict(var6_sin_d, diff(log(out_sample_M1))*100 , n.ahead = 64)
plot(efe)
fanchart(efe)


fcst_VAR_M1 <- data.frame(efe[["fcst"]][["VAR_M1"]])
asdf <- fcst_VAR_M1$fcst
plot.ts(asdf)


fcst_fit_log_dif_var <- diff(log(M_1[121:185]))+asdf
fcst_fit_log_dif_var <- fcst_fit_log_dif_var/100
fcst_fit_log_var <- fcst_fit_log_dif_var + log(out_of_sample_M1[2:65])

fcst_fit_var <- exp(fcst_fit_log_var)
fcst_fit_var <- ts(fcst_fit_var,frequency=12,start=c(2015,2))#, end = c(2020,4))

par(mar= c(2, 2, 1, 2))
plot.ts(fcst_fit_var, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(M_1[122:185], col="red", bty = "n", xlab = "", ylab = "",bty="n")


#-------------------------------
#---> EVALUO SESGO -------------
#-------------------------------

error.var<- M_1[122:185]-fcst_fit_var
plot(error.var)
summary(lm(error.var~1)) #no hay evidencia de sesgo sistemático
isat(error.var, iis=TRUE, sis=FALSE, tis=FALSE,   t.pval=1/length(error.var), plot = TRUE) #aplico IIS con un target size de 1/29
isat(error.var, iis=FALSE, sis=TRUE, tis=FALSE,   t.pval=1/length( error.var), plot = TRUE) #applico SIS con un target size de 1/29
isat(error.var, iis=FALSE, sis=TRUE, tis=FALSE, t.pval=1/(2*length(error.var)), plot = TRUE) #applico IIS+SIS con un target size de 1/(2*29)




############################################################################
#---------------------------------------------------------------------------
############################################################################
#------------------------------ RW -----------------------------------------
############################################################################
#---------------------------------------------------------------------------
############################################################################
library(gets) #tienen que instalarse esta librería para aplicar el algoritmo.

#Grafico los pronósticos del RW:
fcst_rw <- matrix(0, nrow=64, ncol=1) 
fcst_rw <- M_1[121:185]
fcst_rw<-ts(fcst_rw, frequency=12, start=c(2015,2))#, end = c(2020,4))

par(mar= c(2, 2, 1, 2))
plot.ts(fcst_rw, xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
plot.ts(M_1[121:185], xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")

par(new=TRUE)
plot.ts(M_1[122:185], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel")
legend("topleft",c("Forecast RW","M1"),cex=0.8,col=c("black","red"), lty=1, lwd=c(2.5,2.5))



#-------------------------------
#---> EVALUO SESGO -------------
#-------------------------------

error_rw <- ddM_1_out-diff(diff(log(fcst_rw)) , lag=12)*100
plot.ts(error_rw)
summary(lm(error_rw ~ 1))
isat(error_rw, iis=TRUE, sis=FALSE, tis=FALSE, t.pval=1/length(error_rw), plot = TRUE) #aplico IIS con un target size de 1/29
isat(error_rw, iis=FALSE, sis=TRUE, tis=FALSE, t.pval=1/length(error_rw), plot = TRUE) #applico SIS con un target size de 1/29
isat(error_rw, iis=TRUE, sis=TRUE, tis=FALSE, t.pval=1/(2*length(error_rw)), plot = TRUE) #applico IIS+SIS con un target size de 1/(2*29)




############################################################################
#---------------------------------------------------------------------------
############################################################################
#------------------------------ EVALUCION ----------------------------------
############################################################################
#---------------------------------------------------------------------------
############################################################################

# FALTAN LOS ROLLL

dd_fcst_rw <- diff(log(fcst_rw))*100

#predict_VAR_M_1 <- predict(var6, n.head=64)
#plot.ts(predict_VAR_M_1[VAR_dM])
#plot.ts(predict_VAR_M_1[["fcst"]][["VAR_dM_1"]])

fcst_data <- cbind(onestep_X,
                   onestep_2,
                   diff(log(fcst_fit_var))*100,
                   dd_fcst_rw)


par(mar= c(3, 2, 1, 2))
plot.ts(onestep_X, col="orange", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(onestep_2, col="red", bty = "n", xlab = "", ylab = "",bty="n" )
par(new=TRUE)
plot.ts(dd_fcst_rw, col="blue", bty = "n", xlab = "", ylab = "",bty="n" ,xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(ddM_1_out, col="black", bty = "n", xlab = "", ylab = "",bty="n" ,xaxt = "n",yaxt = "n")


nivel_fcst_data <- cbind(fcst_fit_fijo_aic,
                        fcst_fit_fijo_bic,
                        fcst_fit_var,
                        fcst_fit_roll_aic,
                        fcst_fit_roll_bic,
                        fcst_rw)
    
  

?plot.ts
par(mar= c(5, 2, 1, 2))
plot.ts(fcst_fit_fijo_aic, col="black", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(M_1[122:185], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )
legend("topleft",c("M1","ARMA AIC Fijo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

par(mar= c(5, 2, 1, 2))
plot.ts(fcst_fit_roll_aic, col="black", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(M_1[120:185], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )
legend("topleft",c("M1","ARMA AIC Recursivo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

par(mar= c(5, 2, 1, 2))
plot.ts(fcst_fit_fijo_bic, col="red", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(M_1[121:184], col="black", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )
legend("topleft",c("M1","ARMA BIC Fijo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

par(mar= c(5, 2, 1, 2))
plot.ts(fcst_fit_roll_bic, col="red", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(M_1[121:184], col="black", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )
legend("topleft",c("M1","ARMA BIC Recursivo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

par(mar= c(5, 2, 1, 2))
plot.ts(fcst_fit_var, col="black", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(out_of_sample_M1[2:65], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )
legend("topleft",c("M1","VAR Recursivo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

par(mar= c(5, 2, 1, 2))
plot.ts(M_1[120:185], col="black", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(M_1[121:185], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )
legend("topleft",c("M1","Random Walk"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

#-------------------------------------------------------------------------------------------------

par(mar= c(5, 2, 1, 2))
plot.ts(fcst_fit_fijo_aic, col="black", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(out_of_sample_M1[3:65], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )
legend("topleft",c("M1","ARMA AIC Fijo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

accuracy(fcst_fit_fijo_aic, out_of_sample_M1[3:65])


par(mar= c(5, 2, 1, 2))
plot.ts(fcst_fit_roll_aic, col="black", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(out_of_sample_M1[2:65], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )
legend("topleft",c("M1","ARMA AIC Recursivo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

accuracy(fcst_fit_roll_aic, out_of_sample_M1[2:65])


par(mar= c(5, 2, 1, 2))
plot.ts(fcst_fit_fijo_bic, col="black", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(out_of_sample_M1[3:65], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )
legend("topleft",c("M1","ARMA BIC Fijo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

accuracy(fcst_fit_fijo_bic, out_of_sample_M1[3:65])

par(mar= c(5, 2, 1, 2))
plot.ts(fcst_fit_roll_bic, col="black", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(out_of_sample_M1[3:65], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )
legend("topleft",c("M1","ARMA BIC Recursivo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

accuracy(fcst_fit_roll_bic, out_of_sample_M1[4:65])


par(mar= c(5, 2, 1, 2))
plot.ts(fcst_fit_var, col="black", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(out_of_sample_M1[2:65], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )
legend("topleft",c("M1","VAR Recursivo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

accuracy(fcst_fit_var, out_of_sample_M1[3:65])

par(mar= c(5, 2, 1, 2))
plot.ts(fcst_rw, col="black", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(out_of_sample_M1[3:65], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )
legend("topleft",c("M1","Random Walk"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))


accuracy(fcst_rw, out_of_sample_M1[3:65])

par(mar= c(5, 2, 1, 2))
plot.ts(fcst_fit_fijo_adl, col="black", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(out_of_sample_M1[1:53], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )

accuracy(fcst_fit_fijo_adl, out_of_sample_M1[1:53])

par(mar= c(5, 2, 1, 2))
plot.ts(fcst_fit_rec_adl, col="black", xlab="", ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(out_of_sample_M1[3:65], col="red", bty = "n", xlab = "", ylab = "",bty="n", main="Nivel" )
legend("topleft",c("M1","ADL Recursivo"),cex=0.8,col=c("red","black"), lty=1, lwd=c(2.5,2.5))

accuracy(fcst_fit_rec_adl, out_of_sample_M1[3:64])


#-------------------------------
#---> DESEMPEÑO MEDIO ----------
#-------------------------------

?accuracy #---> hay qe mirar el ME
accuracy(onestep_X,    diff(log(M_1[121:185]))*100  ) #bic
accuracy(onestep_2,  diff(log(M_1[121:185]))*100  ) #aic
accuracy(dd_fcst_rw, diff(log(M_1[121:185]))*100  ) #---> RANDOM WALK es benchmark

?accuracy #---> hay qe mirar el ME
accuracy(fcst_fit_fijo_aic, M_1[122:184])
accuracy(fcst_fit_fijo_bic, M_1[122:184])
accuracy(fcst_fit_var, M_1[122:184])

accuracy(fcst_fit_fijo_adl, out_of_sample_M1[13:65])
accuracy(fcst_fit_rec_adl, out_of_sample_M1[3:65])


accuracy(fcst_fit_roll_aic, M_1[121:184])
accuracy(fcst_fit_roll_bic, M_1[124:185])

accuracy(fcst_rw, M_1[122:184]) #---> RANDOM WALK es benchmark

?accuracy



################################################################################################
#-----------------------------------------------------------------------------------------------
#------------------------------------------ PUNTO DOS ------------------------------------------
#-----------------------------------------------------------------------------------------------
################################################################################################
# SOLO FUNIONA CN ESTOS PACKETES
library(tseries)

library(normtest)

library(fGarch)
library(aTSA)
library(rmgarch)
library(rugarch)
library(readxl)

setwd("C://Users//juan_//Desktop//Time-Series//Final_11.08")
dir()
serieTC <- read_xlsx("serieTC.xlsx")
colnames(serieTC)
#serieTC = drop_na(serieTC)
options(scipen = 999)

attach(serieTC)
dolar<-ts(TC, frequency = )
plot(dolar)





#------------------------------------
#---------- Punto 2.a ---------------
#------------------------------------

plot.ts(diff(log(dolar))*100, main="Variación del Tipo de Cambio")
?plot.ts

#------------------------------------
#---------- Punto 2.b ---------------
#------------------------------------

retorno = diff(log(dolar))*100
retorno2 = retorno^{2}
plot(retorno)
plot(retorno2)
acf(retorno2)
pacf(retorno2)
retorno<- ts(retorno)
retorno2<-ts(retorno2)


#Estimo por MV:

xxx <- auto.arima(Ddata$retorno);xxx

Ddata <- data.frame(retorno)
xx <- auto.arima(retorno);xx

fit_arch <- arima(retorno, order=c(4,0,1)) 

arch.test(fit_arch)

spec      <- ugarchspec(variance.model=list(garchOrder=c(1,1)),mean.model=list(armaOrder=c(4,0)), distribution.model="std")
                    
garch11 <- ugarchfit(spec = spec, data = Ddata$retorno, external.regressors=TRUE )

garch11
plot(garch11)

plot.ts(garch11@fit[["z"]])
plot.ts(garch11@fit[["residuals"]])
plot.ts(retorno)

par(mar= c(2, 2, 1, 2))
plot.ts(garch11@fit[["residuals"]], xlab="",ylab="",bty="n",xaxt = "n",yaxt = "n")
par(new=TRUE)
plot.ts(garch11@fit[["z"]], col="red", bty = "n", xlab = "", ylab = "",bty="n")
plot.ts(retorno, col="red", bty = "n", xlab = "", ylab = "",bty="n")


garch_res <- garch11@fit$residuals
jb.norm.test(garch_res)
plot.ts(garch_res)
hist(garch_res, breaks=120, main="Histograma del residuo estandarizado")
kurtosis.norm.test(garch_res)
skewness.norm.test(garch_res)


dev.off()
par(mar= c(4, 4, 2, 4))
plot(retorno^{2}, type="l", ylab="Retorno cuadrático (Negro)", col="black")
par(new=TRUE)
plot(garch11@fit$var, type="l", xaxt="n", yaxt="n", ylab="", xlab="", col="red")
axis(side=4)
mtext("Var. Condicional del GARCH (Rojo)", side=4, line=3)



#------------------------------------
#---------- Punto 2.c ---------------
#------------------------------------

#plot(dolar)
#difdolar <- diff(log(dolar))*100 #retorno, como diferencia logarítmica porcentual
#
#hist(difdolar, breaks=100, freq=F, main="Histograma de la variación del Dolar", xlim=c(-10,15))
#lines(density(difdolar), col="blue", lwd=2)
#jb.norm.test(difdolar[3:1827])
#describe(difdolar)







#Estimo un GARCH-in-mean:
garchSpec <- ugarchspec(
  variance.model=list(model="fGARCH",
                      garchOrder=c(1,1),
                      submodel="APARCH"),
  mean.model=list(armaOrder=c(0,0),
                  include.mean=TRUE,
                  archm=TRUE,
                  archpow=2
  ),
  distribution.model="std")
garchmFit <- ugarchfit(spec=garchSpec, data=retorno) #va a tardar!!!

coef(garchmFit) #el coeficiente lambda es el que corresponde a la prima de riesgo
garchmFit@fit$se.coef
garchmFit #output completo  mirar robyst eta me dice si hay o no ef asimetricos
#library(stargazer)
#stargazer(garchmFit@fit, type = "text")

# Engle-Ng sign bias test



par(mfrow=c(2,1),mar= c(2, 2, 2, 0.5),oma=c(0,0,0,0))
plot.ts(garchmFit@fit$fitted.values, main="Retorno Fitted values")
plot.ts(garchmFit@fit$var, main="Varianza condicional estimada")








################################################################################################
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
################################################################################################

