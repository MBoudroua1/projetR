# Importing House Price Indice Data
HPIDataCSV <- read.csv("C:/Users/Windows10/Documents/R/House-index-canberra.csv", header=F, sep=",")

# conversion date format

values <- HPIDataCSV$V2
HPIData <- ts(values,frequency = 12, start = c(1990,1))

# House Price Indice Table Exploration 
length(HPIData)
print(HPIData)
plot(HPIData)
str(HPIData)
attributes(HPIData)

# Trend and Seasonal Components of HPI
HPIDecomp <- stats::decompose(HPIData)
# seasonal figures
HPIDecomp$figure

# 3 figures arranged in 3 rows and 1 columns
attach(mtcars)
par(mfrow=c(3,1))
plot(HPIDecomp$trend, type="b", xaxt="n", xlab="")
plot(HPIDecomp$seasonal, type="b", xaxt="n", xlab="")
plot(HPIDecomp$figure, type="b", xaxt="n", xlab="")
# get names of 12 months in English words
monthNames <- months(ISOdate(2011,1:12,1))
# label x-axis with month names
# las is set to 2 for vertical label orientation
axis(1, at=1:12, labels=monthNames, las=2)

plot(HPIDecomp)

# HPI Forecasting
fit <- arima(HPIData, order=c(1,0,0), list(order=c(2,1,0), period=12))
fore <- predict(fit, n.ahead=24)
# error bounds at 95% confidence level
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(HPIData, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4), lty=c(1,1,2))


# The Estimated Price of a Property
estimatePrice <- function(year, month){
  
  # compute time
  periode <- month-01 + 12*(year-2011)
  
  fit <- arima(HPIData, order=c(1,0,0), list(order=c(2,1,0), period=12))
  fore <- predict(fit, n.ahead=periode)
  
  # error bounds at 95% confidence degree
  U <- fore$pred + 2*fore$se
  L <- fore$pred - 2*fore$se
  
  # plot forecast
  ts.plot(HPIData, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
  legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4), lty=c(1,1,2))
  
  return(tail(fore$pred,1))
}

