
# Analyze Ice Cream Search Interest in United States from (Jan-2004 -- April 2021),
# Data used after December, 2020 will be used to compare to the prediction results

library(readr)
library(MASS)
library(TSstudio)
library(forecast)
library(tseries)

fullData <- read_csv("iceCreamData.csv",col_types = cols(Month = col_date(format = "%Y-%m")))

iceCream <- ts(fullData[1:204, 2], freq=12, start=c(2004, 1))
iceCream.full <- ts(fullData[, 2], freq=12, start=c(2004, 1))
ts.plot(iceCream)

# Plotting acf and pacf before data manipulation
par(mfrow=c(2,1))
acf(iceCream)
pacf(iceCream)

# Decomposing series
iceCream.stl <- stl(iceCream[, 1], "periodic")
iceCream.new.stl <- iceCream %>% mstl()
iceCream.full.stl <- stl(iceCream.full[, 1], "periodic")

# Removing Seasonality
iceCream.series <- iceCream.stl$time.series[,2] + iceCream.stl$time.series[, 3]
iceCream.new.series <- iceCream.new.stl[,2] + iceCream.new.stl[, 4]

# Plotting acf and pacf after removing seasonality
par(mfrow=c(2,1))
acf(iceCream.series, lag.max = length(iceCream.series))
pacf(iceCream.series, lag.max = length(iceCream.series))

# Determining best ARIMA Model for the Time Series by comparing AIC values
aicMatrix <- matrix(nrow = 4, ncol = 4)
for (r in 1:4) {
  for (l in 1:4) {
    #print(paste("(",r,",",l,")"))
    aicMatrix[r,l] <- AIC(arima(iceCream.new.series, order=c(r,0,l)))
    #print(aicMatrix[r,l]) 
  }
}

aicTable <- as.table(aicMatrix)
colnames(aicTable) <- c(1,2,3,4)
rownames(aicTable) <- c(1,2,3,4)
aicTable



# We will use ARIMA(2,0,4) because that gave us the lowest AIC
iceCream.series.arma1 <- arima(iceCream.series, order=c(2,0,4))
iceCream.new.series.arma1 <- arima(iceCream.new.series, order=c(4,0,4))


# Prediction of the series
iceCream.pred <- predict(iceCream.series.arma1, n.ahead = 4)
test.pres <- predict(test.arma, n.ahead = 4)
# Actual Data
iceCream.future <- ts(fullData[205:208,2], frequency = 12, start = c(2021,1))
# Seasonality of actual data
iceCream.future.seasonality <- iceCream.full.stl$time.series[205:208,1]
# Final prediction data
finalPred <- iceCream.pred$pred + iceCream.pred$se
testFinalPred <- test.pres$pred + test.pres$se
# Deseasonalized actual data
ActualDeseasonalized <- iceCream.future - iceCream.future.seasonality


finalPred
testFinalPred
ActualDeseasonalized


