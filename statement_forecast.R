library(pRattle)
library(lubridate)
library(forecast)
library(xts)

cutoffdate<-"2016-10-24"
statement<-T

df<-get_scores('frc', agg.level='daily', email="<your_username>", pwd="<your_password>")

dates<-as.Date(as.Date("1990-01-01"):as.Date(cutoffdate), origin="1970-01-01")

empty<-xts(,order.by=dates)

df<-merge(empty,df,join="left")
df<-na.locf(df)

df2<-get_scores('frc', type='raw', email="bill@prattle.co", pwd="prattle15")
df_orig<-df2
df2<-subset(df2, speaker %in% c("FOMC Statement", "Minutes FRC"))
df2$statement[df2$speaker=="FOMC Statement"]<-1
df2$minutes[df2$speaker=="Minutes FRC"]<-1

df2$date<-as.Date(df2$date)

df2<-xts(df2[,c('statement', 'minutes')], order.by=df2$date)

ff<-merge(df, df2, join='left')

names(ff)[1]<-'score'
ff<-na.fill(ff, 0)
ff<-ff["19980101/"]

fit<-auto.arima(ff$score, xreg=ff[,c("statement", "minutes")])

if(statement==T){
  xdata<-data.frame(statement=c(0,0,0,0,0,0,1), minutes=c(0,0,0,0,0,0,0))  
} else {
  xdata<-data.frame(statement=c(0,0,0,0,0,0,0), minutes=c(0,0,0,0,0,0,1))  
}

sfore<-forecast(fit, h=7, xreg=xdata)
sfore

formean<-subset(df_orig, date<=ymd(cutoffdate)&date>=ymd(cutoffdate)-months(12))
if(statement==T){
  formean<-subset(formean, speaker=='FOMC Statement')
} else {
  formean<-subset(formean, speaker=='Minutes FRC')
}


baseline<-mean(formean$score)

print("Point estimate")
sfore$mean[7]
print("Residual estimate")
sfore$mean[7] - baseline

