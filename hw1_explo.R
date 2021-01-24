#clean data
t<-read.table(file = "household_power_consumption.txt",header = TRUE, sep = ";", na.strings = "?" )
t$Date<-as.Date(t[,1], format = "%d/%m/%Y")

t<-subset(t, t[,1]>=as.Date("2007-02-01") & t[,1]<=as.Date("2007-02-02") )
colnames(t)<- c("Date","Time","Global Active Power", "Global Reactive Power","Voltage", "Global Intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
library(chron)
t$Time<-chron(times. = t$Time)
Datetime<-paste(t$Date, t$Time)
t<-cbind(t,Datetime)
t$Datetime<-as.POSIXct(Datetime)

#Plot1
par(mfcol = c(1,1), mfrow= c(1,1))
hist(t$`Global Active Power`, freq = TRUE, col = "RED", xlab = "Global Active Power (Kilowatts)", main = "Global Active Power")

dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()

#Plot2
library(lubridate)
wday(t$Datetime, label=TRUE)
plot(t$`Global Active Power` ~ t$Datetime , type ="l", xlab = "", ylab = "Global Active Power (kilowatts)")
dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()

#Plot 3
with(t, plot(Sub_metering_1~ Datetime, type = "l", ylab = "Global Active Power (Kilowatts)", xlab =""))
lines(t$Sub_metering_2~ t$Datetime, type = "l", col = "RED")
lines(t$Sub_metering_3~ t$Datetime, type = "l", col = "BLUE")
legend("topright", col = c("black", "red", "blue"), lwd = c(1,1,1), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()

#plot 4 
par(mfcol = c(2,2), mar = c(4,4,2,1), oma = c(0,0,2,0))
plot(t$`Global Active Power` ~ t$Datetime , type ="l", xlab = "", ylab = "Global Active Power (kilowatts)")
with(t, plot(Sub_metering_1~ Datetime, type = "l", ylab = "Global Active Power (Kilowatts)", xlab =""))
lines(t$Sub_metering_2~ t$Datetime, type = "l", col = "RED")
lines(t$Sub_metering_3~ t$Datetime, type = "l", col = "BLUE")
legend("topright", col = c("black", "red", "blue"),lwd = 2, lty = 1, c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
plot(t$Voltage~t$Datetime, type = "l", xlab = '', ylab = "Voltage")
plot(t$`Global Reactive Power`~ t$Datetime, xlab = "", ylab = "Global Reactive Power (kilowatts)", type = "l")

dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()
