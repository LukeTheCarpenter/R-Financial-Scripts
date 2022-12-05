#install.packages("quantmod")
#install.packages("ShellChron")
library(quantmod)
#library(ShellChron)

#PARAMETERS

business_cycle_days <- 1000
ticker <- "^GDAXI"

# Find a proxy for annual growth over history

s_and_p_data <-quantmod::getSymbols("^GSPC", auto.assign = FALSE, from ="01-01-1800")
start_dataset <- as.numeric(s_and_p_data[1,6])
end_dataset <- as.numeric(s_and_p_data[length(s_and_p_data[,6]), 6])
proxy_length_years <- length(annualReturn(s_and_p_data))
proxy_annual <- ((end_dataset / start_dataset)^(1/proxy_length_years)-1)


AEX <- quantmod::getSymbols(ticker, auto.assign = FALSE, from ="01-01-1900")
AEX <- na.approx(AEX)

mean_price <- mean(AEX[,6])

mean_price
trading_days <- length(AEX[,6])

half_days <- trading_days / 2.0



# calculate the minimum and maximum by using averages and a growth factor assumption

min_daily_growth <- proxy_annual / 365.0
min_end_price <- mean_price * ((1 + min_daily_growth)^half_days)
min_begin_price <- mean_price * ((1 - min_daily_growth)^half_days)


AEX$returns <- dailyReturn(AEX)


t = seq(min_begin_price, min_end_price, (min_end_price - min_begin_price) / as.numeric(trading_days))
y = sin(t)

#AEX$tidal_wave <- seq(0,0,length.out=length(AEX[,6]))

#sinreg(AEX[,6], seq(0,length(AEX[,6]),length.out=length(AEX[,6])))

sinus_values <- y[-length(y)]


AEX$tidal_wave <- sinus_values / 100




chart_Series(AEX)
segments(0,  min_begin_price,length(AEX[,6]), min_end_price )

tail(AEX)

tail(t)
tail(length(t))
tail(y)
tail(length(y))

