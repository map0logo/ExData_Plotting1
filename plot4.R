setwd("/data/mapologo/Dropbox/MOOCs/Exploratory Data Analysis")
data <- read.csv(unz("household_power_consumption.zip", "household_power_consumption.txt"),
                 sep=";",
                 as.is=TRUE)
data$Date <- as.Date(data$Date, format="%d/%m/%Y")
sub_data <- data[data$Date == "2007-02-01" | data$Date == "2007-02-02", ]
calc_time <- function(x){
  x <- as.numeric(x)
  x[1]*60 + x[2] + x[3] / 60
}
sub_data$Time <- sapply(strsplit(as.character(sub_data$Time), ":"), calc_time)
sub_data$Time[sub_data$Date == "2007-02-02"] <- sub_data$Time[sub_data$Date == "2007-02-02"] + 24 * 60
sub_data$Global_active_power <- as.numeric(sub_data$Global_active_power)
sub_data$Global_reactive_power <- as.numeric(sub_data$Global_reactive_power)
sub_data$Voltage <- as.numeric(sub_data$Voltage)
sub_data$Global_intensity <- as.numeric(sub_data$Global_intensity)
sub_data$Sub_metering_1 <- as.numeric(sub_data$Sub_metering_1)
sub_data$Sub_metering_2 <- as.numeric(sub_data$Sub_metering_2)
sub_data$Sub_metering_3 <- as.numeric(sub_data$Sub_metering_3)

png("plot4.png",width=480,height=480)
par(mfrow = c(2, 2))
with(sub_data,{
  plot(Global_active_power~Time,
       xlab="",
       ylab="Global Active Power",
       type="l",
       xaxt = "n")
  axis(1, at=c(0, 1440, 2880),labels=c("Thu", "Fri", "Sat"))
  plot(Voltage~Time,
       xlab="datetime",
       ylab="Voltage",
       type="l",
       xaxt = "n")
  axis(1, at=c(0, 1440, 2880),labels=c("Thu", "Fri", "Sat"))
  plot(Sub_metering_1~Time,
       xlab="",
       ylab="Energy sub metering",
       type="l",
       xaxt = "n")
  lines(Sub_metering_2, col="red")
  lines(Sub_metering_3, col="blue")
  legend("topright",
         legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         col=c("black","red","blue"),
         lty=c(1,1,1),
         bty="n")
  axis(1, at=c(0, 1440, 2880),labels=c("Thu", "Fri", "Sat"))
  plot(Global_reactive_power~Time,
       xlab="datetime",
       ylab="Global Reactive Power",
       type="l",
       xaxt = "n")
  axis(1, at=c(0, 1440, 2880),labels=c("Thu", "Fri", "Sat"))
})


dev.off()
