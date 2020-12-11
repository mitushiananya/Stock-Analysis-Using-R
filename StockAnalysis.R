library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)

# Load the company stock 
getSymbols("AMZN",from="2010-12-01",to="2020-12-01")
getSymbols("FB",from="2010-12-01",to="2020-12-01")
getSymbols("TSLA",from="2010-12-01",to="2020-12-01")
getSymbols("AAPL",from="2010-12-01",to="2020-12-01")
getSymbols("GOOGL",from="2010-12-01",to="2020-12-01")

# Stock returns in log
AMZN_log_returns<-AMZN%>%Ad()%>%dailyReturn(type='log')
AMZN_log_returns
FB_log_returns<-FB%>%Ad()%>%dailyReturn(type='log')
FB_log_returns
TSLA_log_returns<-TSLA%>%Ad()%>%dailyReturn(type='log')
TSLA_log_returns
AAPL_log_returns<-AAPL%>%Ad()%>%dailyReturn(type='log')
AAPL_log_returns
GOOGL_log_returns<-GOOGL%>%Ad()%>%dailyReturn(type='log')
GOOGL_log_returns

# Mean of log stock returns 
print("Mean of Amazon, Facebook, Tesla, Apple and Google respectively: ")
AMZN_mean_log<-mean(AMZN_log_returns)
AMZN_mean_log
FB_mean_log<-mean(FB_log_returns)
FB_mean_log
TSLA_mean_log<-mean(TSLA_log_returns)
TSLA_mean_log
AAPL_mean_log<-mean(AAPL_log_returns)
AAPL_mean_log
GOOGL_mean_log<-mean(GOOGL_log_returns)
GOOGL_mean_log

# Round the mean to 4 decimal places
print("Rounding off the mean of stocks to 4 decimal places: ")
mean_log<-c(AMZN_mean_log,FB_mean_log,TSLA_mean_log,AAPL_mean_log,GOOGL_mean_log)
mean_log<-round(mean_log,4)
mean_log

# Standard deviation of log stock returns
print("Standard Deviation of Amazon, Facebook, Tesla, Apple and Google respectively: ")
AMZN_sd_log<-sd(AMZN_log_returns)
AMZN_sd_log
FB_sd_log<-sd(FB_log_returns)
FB_sd_log
TSLA_sd_log<-sd(TSLA_log_returns)
TSLA_sd_log
AAPL_sd_log<-sd(AAPL_log_returns)
AAPL_sd_log
GOOGL_sd_Log<-sd(GOOGL_log_returns)
GOOGL_sd_Log

# Round the standard deviation to 4 decimal places 
print("Rounding off the standard deviation to 4 decimal places: ")
sd_log<-c(AMZN_sd_log,FB_sd_log,TSLA_sd_log,AAPL_sd_log,GOOGL_sd_Log)
sd_log<-round(sd_log,4)
sd_log

# Data frame
graphic1<-data.frame(rbind(c("AMZN",AMZN_mean_log,AMZN_sd_log),c("FB",FB_mean_log,FB_sd_log),c("TSLA",TSLA_mean_log,TSLA_sd_log),c("AAPL",AAPL_mean_log,AAPL_sd_log),c("GOOGL",GOOGL_mean_log,GOOGL_sd_Log)),stringsAsFactors = FALSE)
graphic1<-data.frame(mean_log,sd_log)
rownames(graphic1)<-c("AMZN","FB","TSLA","AAPL","GOOGL")
colnames(graphic1)<-c("Mean_Log_Return", "Sd_Log_Return")

# Visualization of each stock's risk vs reward. 
# Risk: standard deviation of log returns
# Reward: mean of log returns
xlab<-list(title="Reward")
ylab<-list(title="Risk")
RiskReward <- plot_ly(x=graphic1[,1],y=graphic1[,2],text=rownames(graphic1),type='scatter',mode="markers",marker=list(color=c("black","darkblue","orange","grey","darkgreen")))%>%layout(title="Risk v Reward",xaxis=xlab,yaxis=ylab)
RiskReward

# Chartseries to show the stock prices 
AMZN%>%Ad()%>%chartSeries(name = "Amazon Price: ", theme = chartTheme("white", up.col = "orange"))
AMZN%>%chartSeries(name = "Amazon Bollinger Band chart",TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020', theme = chartTheme("white"))

FB%>%Ad()%>%chartSeries(name= "Facebook Price: ", theme = chartTheme("white", up.col = "purple"))
FB%>%chartSeries(name = "Facebook Bollinger Band chart", TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020', theme = chartTheme("white"))

TSLA%>%Ad()%>%chartSeries(name = "Tesla Price: ", theme = chartTheme("white", up.col ="darkblue"))
TSLA%>%chartSeries(name = "Tesla Bollinger Band chart", TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020', theme = chartTheme("white"))

AAPL%>%Ad()%>%chartSeries(name = "Apple Price: ", theme = chartTheme("white", up.col ="magenta"))
AAPL%>%chartSeries(name = "Apple Bollinger Band chart", TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020', theme = chartTheme("white"))

GOOGL%>%Ad()%>%chartSeries(name = "Google Price: ", theme = chartTheme("white", up.col= "darkgreen"))
GOOGL%>%chartSeries(name = "Google Bollinger Band chart", TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020', theme = chartTheme("white"))

# Average stock daily return
probs<-c(0.005,0.025,0.25,0.5,0.75,0.975,0.995)
AMZN_dist<-AMZN_log_returns%>%quantile(probs=probs,na.rm=TRUE)
AMZN_dist
AMZN_mean<-mean(AMZN_log_returns,na.rm=TRUE)
AMZN_mean
AMZN_sd<-sd(AMZN_log_returns,na.rm=TRUE)
AMZN_sd
AMZN_mean%>%exp() 

FB_dist<-FB_log_returns%>%quantile(probs=probs,na.rm=TRUE)
FB_dist
FB_mean<-mean(FB_log_returns,na.rm=TRUE)
FB_mean
FB_sd<-sd(FB_log_returns,na.rm=TRUE)
FB_sd
FB_mean%>%exp() 

TSLA_dist<-TSLA_log_returns%>%quantile(probs=probs,na.rm=TRUE)
TSLA_dist
TSLA_mean<-mean(TSLA_log_returns,na.rm=TRUE)
TSLA_mean
TSLA_sd<-sd(TSLA_log_returns,na.rm=TRUE)
TSLA_sd
TSLA_mean%>%exp() 

AAPL_dist<-AAPL_log_returns%>%quantile(probs=probs,na.rm=TRUE)
AAPL_dist
AAPL_mean<-mean(AAPL_log_returns,na.rm=TRUE)
AAPL_mean
AAPL_sd<-sd(AAPL_log_returns,na.rm=TRUE)
AAPL_sd
AAPL_mean%>%exp() 

GOOGL_dist<-GOOGL_log_returns%>%quantile(probs=probs,na.rm=TRUE)
GOOGL_dist
GOOGL_mean<-mean(GOOGL_log_returns,na.rm=TRUE)
GOOGL_mean
GOOGL_sd<-sd(GOOGL_log_returns,na.rm=TRUE)
GOOGL_sd
GOOGL_mean%>%exp() 

# Correlation of 4 stocks: Tesla, Facebook, Google, Amazon
data<-cbind(diff(log(Cl(AMZN))),diff(log(Cl(GOOGL))),diff(log(Cl(AAPL))),diff(log(Cl(FB))))
chart.Correlation(data)

# PRICE PREDICTION OF STOCKS
# Random walk
mu<-AMZN_mean_log # Amazon's Mean 
sig<-AMZN_sd_log # Amazon's Standard Deviation 
testsim<-rep(NA,1000) # generate random daily exponent increase rate using AMZN's mean and sd log returns

# One year has 252 trading days
# 4*252 trading days thus means 4 years of trading
price<-rep(NA,252*4)

# Most recent price of Amazon
price[1]<-as.numeric(AMZN$AMZN.Adjusted[length(AMZN$AMZN.Adjusted),])
price[1]

# Simulation of prices
for(i in 2:length(testsim)){
  price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
}
random_data<-cbind(price,1:(252*4))
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)
random_data%>%ggplot(aes(Day,Price))+geom_line()+labs(title="Amazon price simulation for 4 years using Random Walk")+theme_bw()

# Predicting the price of stock Amazon using Percentile method
final_mat[500,-1]%>%as.numeric()%>%quantile(probs=probs) # Amazon (AMZN)’s stock may reach the price of $18184.670 in four years time or crash to a $1985.563 low


