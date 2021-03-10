## Preparation
#####################################################################

# Open Data Set and Initial Data Preparation

rides = read.csv("rides.csv")
str(rides)
rides$RatecodeID = NULL
rides$payment_type = NULL
rides$pickup_timeOfDay = NULL
rides$cond = NULL
rides$weekday = NULL

#Frage 2: Es handelt sich um einen Dataframe
# Der Unterschiede zu eine Matrix liegt daran, dass in einen Dataframe nicht 
# alle Variablen den gleichen Type sein müssen.

#Frage 3
mean(rides$total_amount) #Mean = 15.59
median(rides$total_amount) #Median = 11.16
sd(rides$total_amount) #SD = 13.47
# Diese Werte zeigen dass es viel kleinere Beträgen gibt, weil der Mdian tiefer als der Durschnitt liegt
# Damit sind die wenig grossen Beträge weiter entfernt der Median als die kleine Werte
# Die Standardabweichung bestätigt diese Hypothese, weil die SA grösser ist als der Median Wert 
# und es keinen negativen Werten gibt

#Frage 4
hist(rides$total_amount)
# Die Verteilung bestätigt die vorherige Hypothese, dass es weniger grossen Beträge gibt, aber diese extrem hoch sein können
# Die meisten Beträge befinden sich zwischen 0 und 20

#Frage 5
ncol(rides)
rides[1:18] <- lapply(rides[1:18],as.numeric)
cor(rides, rides$total_amount)
rides$trip_distance_miles= NULL
rides$trip_distance = NULL
rides$trip_duration = NULL
# Am meisten korreliert ist "Total_Amount" mit "trip_distance" und "trip_distance_miles", was Sinn ergibt da beide Variablen das gleiche Aussagen
# Weiter ist die Korrelation mit "trip_duration" und "speed" relativ stark
# Die Temperatur bewirkt auch eine negative Korrelation, was bedeutet dass Leuten wenn es kalt ist auch Taxis nehmen für kürzeren Strecken.
# Alle Werte, welche > 0,8 Korrelation aufwiesen wurden rausgenommen, um die späteren Regressionen nicht zu schaden


#Frage 6
plot(rides$dropoff_latitude,rides$dropoff_longitude, pch=16, col="blue")
corrected_rides <- rides[!(rides$dropoff_latitude == 0 & rides$dropoff_longitude == 0),]
plot(corrected_rides$dropoff_latitude,corrected_rides$dropoff_longitude, pch=16, col="blue")
# Die meisten Daten befinden sich zwischen die Longitude -74 und -73.8, sowie zwischen die Latitude 40.65 und 40.85

#Frage 7
# Splitting the rides Data set into a training and test set
# Beide sets werden ohne Zeilennummern abgespeichert
nrow(corrected_rides)
training_rides <- corrected_rides[1:3000,]
test_rides <- corrected_rides[3001:4942,]
write.csv(training_rides, "training_rides.csv", row.names = FALSE)
write.csv(test_rides, "test_rides.csv", row.names = FALSE)

#Frage 8
m1 <- lm(total_amount ~ passenger_count+pickup_hour+speed+temp+hum+vis+fog+rain+gasprice+pickup_longitude
         +pickup_latitude+dropoff_longitude+dropoff_latitude, 
         data = training_rides)
summary(m1)

predictions = predict(m1, newdata=training_rides)
predictions

#Frage 9
# Wichtigsten Variablen:
RMSE = sqrt(mean((training_rides$total_amount - predictions)^2))
RMSE


predictions_test = predict(m1, newdata=test_rides)
predictions_test
RMSE_test = sqrt(mean((test_rides$total_amount - predictions_test)^2))
RMSE_test

#Frage 10
SSE = sum((training_rides$total_amount - predictions)^2)
SSE 
SSE_test = sum((test_rides$total_amount - predictions_test)^2)
SSE_test


#Frage 11
set.seed(101)
plot(predictions,training_rides$total_amount,pch=16, col="blue", ylab="Actual Values", xlab="Predicted Values", main="Actual vs. Predicted Values")
abline(a=0, b=1, lwd=2.75,col="green4")

plot(predictions_test,test_rides$total_amount,pch=16, col="blue", ylab="Actual Values", xlab="Predicted Values", main="Actual vs. Predicted Values")
abline(a=0, b=1, lwd=2.75,col="green4")

#Frage 12
m2 <- lm(total_amount ~ pickup_hour + speed + temp + rain + hum +pickup_longitude
         +pickup_latitude+dropoff_longitude+dropoff_latitude, data = training_rides)
summary(m2)
opt_pred = predict(m2, newdata = training_rides)
opt_pred
opt_RMSE = sqrt(mean((training_rides$total_amount - opt_pred)^2))
opt_RMSE
opt_SSE = sum((training_rides$total_amount - opt_pred)^2)
opt_SSE
plot(opt_pred,training_rides$total_amount,pch=16, col="blue", ylab="Actual Values", xlab="Predicted Values", main="Actual vs. Predicted Values")
abline(a=0, b=1, lwd=2.75,col="green4")
#The model isn't performing better by removing the insignificant variables, it gets slightly worse
