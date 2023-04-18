library(fpp)
library(dplyr)
library(car)

Data_Kurs=read.csv("Nilai Tukar Rupiah.csv", header = TRUE, sep = ";" )
Data_Kurs
model = lm(Kurs ~ Y, data = Data_Kurs)
model
durbinWatsonTest(model)

#Sorting Data Kurs
Kurs_bulan_tahun = Data_Kurs %>% select(Month,Year,Kurs) %>% group_by(Month,Year) %>% summarise(Kurs)
print(Kurs_bulan_tahun, n=61)
Kurs_sorting=arrange(select(Kurs_bulan_tahun,Month,Year,Kurs),Year)
print(Kurs_sorting, n=61)

#Data Time Series Kurs
Kurs_ts=ts(Kurs_sorting$Kurs,start = c(2018,1),end=c(2023,1), frequency = 12)
Kurs_ts

#Plot Time Series Data Kurs
plot(Kurs_ts,xlab = "Waktu", ylab = "Kurs", col="#29648A",lwd=2.5, ylim=c(13300,16000))
library(TSstudio)
ts_plot(Kurs_ts, title = "Harga Nilai Tukar Rupiah (IDR) terhadap Dollar Amerika (USD) Periode 2014-2022", 
        Xtitle = "Waktu", Ytitle = "Kurs", line.mode = "lines")

#Uji Stasioneritas
library(tseries)
adf.test(Kurs_ts)
BoxCox.lambda(Kurs_ts)


#Differencing Data Kurs
Kurs_diff=diff(Kurs_ts, differences = 1)
Kurs_diff
adf.test(Kurs_diff)
BoxCox.lambda(Kurs_diff)
acf(Kurs_diff, lag.max = 20)
pacf(Kurs_diff, lag.max = 20)

Model=auto.arima(Kurs_ts)
Model
Modelarima=arima(Kurs_ts,order=c(0,1,2))
Modelarima
summary(Modelarima)
Modelforecast=forecast(Modelarima, h=12)
Modelforecast
plot(forecast(Modelforecast))

#Uji Asumsi Model ARIMA
Box.test(Modelarima$residuals, type = "Ljung-Box")
shapiro.test(Modelarima$residuals)
