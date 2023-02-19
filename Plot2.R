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

png("Plot2.png",width = 480, height = 480, units = "px")
plot(plot2$Global_active_power~ plot2$DateTime, type = "l", 
     ylab = "Global Active Power (kilowatts)", xlab = "")    
dev.off()

# using ggplot2
plot2 %>% 
        ggplot(aes(DateTime, Global_active_power))+
        geom_line()+
        theme_bw()+
        labs(y="Global Active Power (kilowatts)")


