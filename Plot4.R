library(here)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(data.table)
library(dplyr)
library(skimr)

# Creating a temporary file to store the data set.  extracting the dataset to a data.table
#extracting the data set household_power_consumption.txt into a data table called dt_hpc
url <- paste0("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip")
tempDir<- tempdir()
tempFile <- tempfile()
download.file(url,tempFile,quiet=TRUE,mode="wb")

dt_hpc <- data.table::fread(unzip(file.path(tempFile), exdir = tempDir),na.strings="?")

#Removing the temp file 
file.remove(tempFile)
unlink(tempDir, recursive = TRUE)                                  

# mutating the date to Date format and Selecting only the dates are needed. 
#then converting the Time to class hms

dt_hpc<- dt_hpc %>% 
        dplyr::mutate(Date= dmy(Date)) %>% 
        dplyr::filter(Date>= "2007-02-01" & Date<="2007-02-02") %>% 
        dplyr::mutate(Time = hms::as_hms(Time)) %>% 
        drop_na()

#Exploring the data
str(dt_hpc)

#using base R
plot2<- dt_hpc %>% 
        unite(DateTime, Date:Time, sep = "-", 
              remove = TRUE, na.rm = FALSE) %>% 
        mutate(DateTime = ymd_hms(DateTime))



png("Plot4.png",width = 480, height = 480, units = "px")
par(mfrow=c(2,2))
# plot 1- 4
plot(plot2$Global_active_power~ plot2$DateTime, type = "l", 
     ylab = "Global Active Power (kilowatts)", xlab = "") 

# plot 2 - 4
plot(plot2$Voltage~ plot2$DateTime, type = "l", 
     ylab = "Voltage", xlab = "",  lwd=1)    

# plot 3 - 4
plot(plot2$Sub_metering_1 ~plot2$DateTime , type = "l", 
     ylab = "Energy sub metering", xlab = "")
lines(plot2$Sub_metering_2  ~ plot2$DateTime, type = "l", col = "red")
lines(plot2$Sub_metering_3  ~ plot2$DateTime, type = "l", col = "blue")
legend("topright", pch ="-",col=c("black","blue", "red"), legend = colnames(plot2[6:8]))


# plot 4 - 4
plot(plot2$Global_reactive_power~ plot2$DateTime, type = "l", 
     ylab = "Global_reactive_power", xlab = "", lwd=0.7)    


dev.off()

par(mfrow=c(1,1))

# using ggplot2

plot2 %>% 
        ggplot(aes(DateTime, Voltage))+
        geom_line(lwd = 0.8)+
        theme_bw()+
        labs(y="Voltage")

plot2 %>% 
        ggplot(aes(DateTime, Global_reactive_power))+
        geom_line(lwd = 0.8)+
        theme_bw()+
        labs(y="Global_reactive_power")