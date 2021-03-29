#ALTCOIN SWING TRADING WITH FUTURES
#install.packages("httr")
#install.packages("jsonlite")
#install.packages("readr")
#install.packages("lubridate")
library(httr)
library(readr)
library(jsonlite)
library(lubridate)
base_pair <- "BTC"
denoted <- "USDT"
minutedata_days <- 2
hourlydata_days <- 5
dailydata_days <- 200

json_files <- c()
prices <- c()
timestamp <-999999999999
for(x in 1:minutedata_days){
  sample <- 1440
  
  key <- "81d353e0b1944c2840f9668016633e6456f1bd09708f30d82356d019803ba96f"
  url <- paste0("https://min-api.cryptocompare.com/data/v2/histominute?fsym=", base_pair,
    "&tsym=", denoted , "&limit=" , as.character(sample) , "&toTs=", as.character(timestamp), "&api_key=", key)
  
  response<- GET(url)
  print(url)
  jsonRespText<-content(response,as="text") 
  json_file <- fromJSON(jsonRespText)
  prices <- c(prices, rev(json_file$Data$Data['close']$close))
  latest_stamp <- json_file$Data$Data['time']$time[1][length(json_file$Data$Data['time'][1])]
  timestamp <- latest_stamp
}


frame_minute <- data.frame(seq(1, length(prices)),  prices )

avg_minute <- mean(rev(prices))
print(avg_minute)
#MINUTE DATA




#plot(frame_past, type="l", col="green")

plot(frame_minute, type="l", col="green")

abline(0,0, h=avg_minute, col="blue")




