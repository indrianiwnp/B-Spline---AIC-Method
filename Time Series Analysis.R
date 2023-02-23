library(fpp)
library(dplyr)

Data_Kurs=read_excel("Nilai Tukar Rupiah.xlsx", sheet = "Nilai Tukar Rupiah" )
Data_Kurs

#Sorting Data KursKKData_Data_
Kurs_bulan_tahun = Data_Kurs %>% select(Month,Year,Kurs) %>% group_by(Month,Year) %>% summarise(Kurs)
print(Kurs_bulan_tahun, n=61)
Kurs_sorting=arrange(select(Kurs_bulan_tahun,Month,Year,Kurs),Year)
print(Kurs_sorting, n=61)

#Data Time Series Kurs
Kurs_ts=ts(Kurs_sorting$Kurs,start = c(2018,1),end=c(2023,1), frequency = 12)
Kurs_ts

#Plot Time Series Data Kurs
plot(Kurs_ts, ylim=c(13000,16000),xlab = "Waktu", ylab = "Kurs", type="o",pch=16,col="#29648A",lwd=2.5)
library(TSstudio)
ts_plot(Kurs_ts, title = "Harga Nilai Tukar Rupiah (IDR) terhadap Dollar Amerika (USD) Periode 2014-2022", 
        Xtitle = "Waktu", Ytitle = "Kurs", line.mode = "lines+markers")

#Uji Stasioneritas
library(tseries)
adf.test(Kurs_ts)
kpss.test(Kurs_ts)

#Differencing Data Kurs
Kurs_diff=diff(Kurs_ts, differences = 1)
Kurs_diff

#Plot Stasioneritas
ts_plot(Kurs_diff, title = "Grafik Diferensi Data Kurs", Xtitle = "Waktu", 
        Ytitle = "Kurs")

#Plot ACF dan PACF
library(astsa)
acf(Kurs_diff, lag.max = 20)
pacf(Kurs_diff, lag.max = 20)

#Model ARIMA
Model1=arima(Kurs_diff, order = c(2,1,0))
Model1
Model2=arima(Kurs_diff, order = c(0,1,2))
Model2
Model3=arima(Kurs_diff, order = c(2,1,2))
Model3

#Model ARIMA Terbaik
aic.model=data.frame(Model=c("ARIMA(2,1,0)","ARIMA(0,1,2)", "ARIMA(2,1,2)"), 
                     AIC=c(Model1$aic, Model2$aic, Model3$aic))
aic.model

#Uji Asumsi Model ARIMA
shapiro.test(Model3$residuals)
adf.test(Model3$residuals)
Box.test(Model3$residuals, type = "Ljung")
jarque.bera.test(Model3$residuals)

#Plot Linear
plot(Data_Kurs$Kurs,Data_Kurs$Y, xlab = "x", ylab = "y")
