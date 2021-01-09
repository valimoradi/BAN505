library(forecast)

ride.df = read.csv("bicup2006.csv", header = TRUE)
ride.ts = ts(ride.df$DEMAND, start= c(0), end= c(24), frequency = 63)

#creating weekend dummy
ride.df$dummy =  rep(0, length(ride.df))
ride.df$dummy[which(ride.df$DATE >= "5-Mar-01" & ride.df$DATE <= "7-Mar-01") ] = 1
ride.df$dummy[which(ride.df$DATE >= "12-Mar-01" & ride.df$DATE <= "14-Mar-01") ] = 1
ride.df$dummy[which(ride.df$DATE >= "19-Mar-01" & ride.df$DATE <= "21-Mar-01") ] = 1

#creating three data sets
train.data = ride.df[1:882, ]
valid.data= ride.df[883:1323, ]
validandtest.data= ride.df[883:1512, ]
test.data =ride.df[1323:1512, ]
nValid = 441 #63 observation * 7 days                             
nTraining = 882 #63 observation * 14 days
ntest= 1512 - nValid-nTraining 

#creating three time series data
train.ts = window(ride.ts, start= c(0), end = c(0,nTraining))
valid.ts = window(ride.ts, start = c(0, nTraining+1), end = c(0,nTraining+nValid))
test.ts = window(ride.ts, start = c(0, nTraining+nValid+1), end = c(0,1512))

#decomposition of the training set into trend, seasonal, and random
decompose_train.ts = decompose(train.ts, "additive")
plot(as.ts(decompose_train.ts$seasonal), main = "seasonal", ylab = "number of passengers")
plot(as.ts(decompose_train.ts$trend), main = "trend", ylab = "number of passengers")
plot(as.ts(decompose_train.ts$random), main = "randomness", ylab = "number of passengers")
plot(decompose_train.ts)

#simple model
train.lm.trend.season = tslm(train.ts ~ trend + season)
train.trend.season.pred = forecast(train.lm.trend.season, h = nValid )


#polynomial model
train.lm.trend.season.poly = tslm(train.ts ~ trend+ I(trend^2) + season)
train.trend.season.pred.poly = forecast(train.lm.trend.season.poly, h = nValid )


#weekend dummy
train.lm.trend.season.dummy = tslm(train.ts ~ trend + dummy+ season, data = train.data)
train.trend.season.pred.dummy = forecast(train.lm.trend.season.dummy, newdata =valid.data )


#accuracy test of the three models
accuracy(train.trend.season.pred,valid.ts)
accuracy(train.trend.season.pred.poly,valid.ts)
accuracy(train.trend.season.pred.dummy,valid.ts)


#best model(weekend dummy)
train.trend.season.pred.test.dummy = forecast(train.lm.trend.season.dummy,level = 0 ,newdata = test.data)


#the number of passengers predicted for the test set
print(train.trend.season.pred.test.dummy$mean[c(1:189)])

#plotting the prediction 
train.trend.season.pred.validandtest.dummy = forecast(train.lm.trend.season.dummy,level = 0 ,newdata = validandtest.data)
plot(train.trend.season.pred.validandtest.dummy,ylim = c(0, 160), ylab = "Passengers", bty= "l", xaxt = "n",                                
     xlim= c(0,24), main = "2005 Public Transportation Demand", cex= 2, flty = 2, fcol = "blue")
axis(1, at = seq(0,23),labels = format(as.factor(unique(ride.df$DATE))), las=2)

#Training Line
lines(train.lm.trend.season.dummy$fitted.values, lwd= 2, col="blue")

#Validation Line
lines(valid.ts)

#adding explanatory information to the plot
abline(v=14, col="red", lwd=3)       
abline(v=21, col="red",lwd=3)    
text(7,155,"Training", cex = 1.25)
text(17.5,155,"Validation", cex = 1.25)
text(22.7,155,"Test", cex = 1.25)
arrows(0, 145, 13, 145, code = 3, length = 0.1, lwd = 1, angle = 30) 
arrows(15, 145, 20, 145, code = 3, length = 0.1, lwd = 1, angle = 30) 
arrows(21.5, 145, 23.5, 145, code = 3, length = 0.1, lwd = 1, angle = 30) 
