### import the data
library(ggplot2)
library(tseries)
library(forecast)

dt<- read.csv(file.choose(),header = T)
class(dt)
head(dt)
str(dt)
View(dt)
frequency(dt)

### deseasonlise data

count_ma <- ts(na.omit(dt$sales),frequency = 12, start = c(1995,3), end = c(2013,4))
start(count_ma)
class(count_ma)
View(count_ma)
### decompose the data

##other type of decomposing
decomp <- stl(count_ma,s.window = "periodic")
plot(decomp)
deseasonal_cnt <- seasadj(decomp)

plot(decomp)

plot(count_ma)
plot(deseasonal_cnt)

##ADF test, alternate to acf
adf.test(deseasonal_cnt,alternative = "stationary") ## p value should be near to 0.05

##### since the data is not stationary, we will do differencing

count_d1<- diff(deseasonal_cnt,differences = 1)

plot(count_d1)

adf.test(count_d1,alternative = "stationary") ##since p value is less than 0.05, data becomes stationary


##### auto corelation

acf(count_d1,main="acf plot for the series")
pacf(count_d1,main="pacf plot for the series")

a<- auto.arima(deseasonal_cnt)
a

###fit

fit_sales<-  arima(deseasonal_cnt,c(0,1,1),
                  seasonal =list(order=c(0,1,1),period=12))

### predict

fcast<- forecast(fit_sales,h=12)

plot(fcast)


#### since the forecast is flatlined, we need to include the seasonality which we removed in the first place

fit_seas<- auto.arima(deseasonal_cnt,seasonal = TRUE)


seas_fcast <- forecast(fit_seas,h=12)

plot(seas_fcast)

## for accuracy check MAPE
accuracy(fcast)


export<-as.data.frame(fcast)
export1 <- round(export, digits = 0)
write.csv(export1,"Forecast_Values.csv")
getwd()

##to check residual correlation
Box.test(fit_sales$residuals, lag=10, type="Ljung-Box")
