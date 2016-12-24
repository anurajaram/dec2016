# author - Anupama Rajaram
# Date - Dec 15, 2016
# Description - Code for pricing analysis.
#               Analyze facebook stock price and create linear regression model.



# Start by cleaning up memory of current R session:
rm(list=ls(all=TRUE))

# suppress scientific notation e.g.: 2.4e5 instead 2,400,367 
options(scipen = 999)

# decimal places - only 4
options(digits = 4)

# load required library packages:
library(data.table)   # for fread() 
library(sqldf)

library(plotly)
library('ggplot2')    # plotting data

library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series 
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings



# ====================================
# load original dataset
# ====================================

fbkdf = data.frame(fread("fb_2004_to_2016.csv", stringsAsFactors = FALSE))
#fbkdf$ticker = "FB"

sp500df = data.frame(fread("sp500_2004_to_2016.csv", stringsAsFactors = FALSE))
sp5k = sp500df[,c(1:2)] 
colnames(sp5k) = c("Date", "SP500_price")

# merge sp500 and fbkdf to benchmark against S&P500.
fbknew = merge(fbkdf, sp5k, by = "Date", all.x = TRUE)

rm(sp500df, sp5k)



# ====================================
# Create derived variables
# ====================================

# calculate daily volatility
fbknew$prc_change = fbknew$Close - fbknew$Open

# date calculation
fbknew$Date2 = as.Date(fbknew$Date, "%Y-%m-%d")
fbknew$mthyr = paste(as.numeric(format(fbknew$Date2, "%m")), "-", 
                     as.numeric(format(fbknew$Date2, "%Y")), sep = "")
fbknew$mth = as.numeric(format(fbknew$Date2, "%m"))
fbknew$year = as.numeric(format(fbknew$Date2, "%Y"))   


# calculate volatility from prev day:
# first order by date
fbknew <- fbknew[order(fbknew$Date2),]

# add a factor for volatility to show change from previous day
fbknew$volt_chg = 0

for( i in 2:nrow(fbknew)){
  fbknew[i, "volt_chg"] = fbknew[i, "Open"] - fbknew[i-1, "Open"]
}



# ====================================
# Some visualizations
# ====================================
fbkmthdata = sqldf("select avg(Open) as 'Open', 
                   avg(High) as 'High', avg(Low) as 'Low' ,
                   avg(Close) as 'Close' , avg(Volume) as 'Volume',
                   mthyr, year, mth
                   from fbknew 
                   group by mthyr")


# graph for Facebook Stock Price by Volume
plot(fbkmthdata$Volume, fbkmthdata$Open, xlab = "Volume", 
     ylab = "Opening Stock Price", main = "Facebook Stock Price Analysis")
lines(lowess(fbkmthdata$Volume, fbkmthdata$Open), col="blue")



data1 = subset(fbkmthdata, Volume <= 120000000)
# graph for Facebook Stock Price by Volume
plot(data1$Volume, data1$Open, xlab = "Volume", 
     ylab = "Opening Stock Price", main = "Facebook Stock Price Analysis")
lines(lowess(data1$Volume, data1$Open), col="blue")



# graph for Stock Price variation by Month and Year
ggplot(fbkmthdata, aes(fbkmthdata$mth, fbkmthdata$Open, 
                       color = fbkmthdata$year))+geom_point() +
  labs(title = "Facebook Stock Price") + xlab("Month") +
  ylab( "Opening Stock Price")




# ====================================
# Linear Regression Model
# ====================================
lmodelfb = lm(Open ~ High + Volume + SP500_price + prc_change +
                mthyr + mth + year + volt_chg, 
              data = fbknew)

summary(lmodelfb)

plot(fbknew$SP500_price, fbknew$Open, 
     xlab = "Stock price S&P500",
     ylab = "Stock price Facebook (open)",
     main = "Relationship between Stock Price of Facebook versus S&P500")
lines(lowess(fbknew$SP500_price, fbknew$Open), col="red")


plot(fbknew$year, fbknew$Open,
     xlab = "Year",
     ylab = "Stock price Facebook (open)",
     main = "Stock Price of Facebook by year")
lines(lowess(fbknew$year, fbknew$Open), col="red")

plot(fbknew$mth, fbknew$Open,
     xlab = "Month",
     ylab = "Stock price Facebook (open)",
     main = "Stock Price of Facebook by month")


library(car)
scatterplot(fbknew$mth ~ fbknew$Open | fbknew$year,
            xlab="Month", ylab="Facebook Stock Price (opening amount)", 
            main="Enhanced Scatter Plot")


plot(fbknew$volt_chg, fbknew$Open)



plot(data1$High, data1$Open, xlab = "Daily High", 
     ylab = "Opening Stock Price", main = "Facebook Stock Price Analysis")
lines(lowess(data1$High, data1$Open), col="blue")





