install.packages("quantmod")
library("quantmod")

ticker <- "^AEX"

#PLUG Accrued dividends of companies here
# ACCOUNT for withdrawals, AKA even-money. First is inflation
min_annual_growth <- 0.02 #inflation
max_annual_growth <- 0.06 - 0.02 #average ROI - dividends

#_________________#



sym <- getSymbols(ticker, from="1990-01-01", src="yahoo", auto.assign = FALSE)
sym <- na.approx(sym)
head(sym)

# take average
avg <- mean(sym[,6])


print(avg)
# contruct natural growth by iterating and comparing to growth rate
# Before half of the chart a negative growth rate is applied
# In the second half a positive growth rate is applied
# Based on average

halfway_x <- length(sym[,6]) / 2
daily_rate_min <- (min_annual_growth + 1)^(1/365)
daily_rate_max <- (max_annual_growth + 1)^(1/365)

print(daily_rate_max)
print(daily_rate_min)


growth_min <- c()
growth_max <- c()
for(x in 1:length(sym[,6])){
  growth_rate_min <-  daily_rate_min^(daily_rate_min * (x-halfway_x))
  adjusted_growth_min <- avg * growth_rate_min
  growth_min <- c(growth_min, adjusted_growth_min)
  
  growth_rate_max <-  daily_rate_max^(daily_rate_max * (x-halfway_x))
  adjusted_growth_max <- avg * growth_rate_max
  #print(adjusted_growth_min)
  growth_max <- c(growth_max, adjusted_growth_max )
  #
  #sym["AEX.MinGrowth", x] <- adjusted_growth_min
}
sym$GrowthMin <- growth_min
sym$GrowthMax <- growth_max
plot(sym[,6:8], type="l", col = c("green", "blue", "red"))

print(tail(growth_min))
print(tail(growth_max))
