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

#Creating plot1 using base R
png("Plot1.png",width = 480, height = 480, units = "px")
hist(dt_hpc$Global_active_power, col = "red", 
     xlab = "Global Active Power(kilowatts)", 
     main="Global Active Power")
dev.off()


# using ggplot2

plot1<- dt_hpc %>% 
        ggplot(aes(Global_active_power))+
        geom_histogram(binwidth = 0.44, 
                       fill = "red",
                       color= "darkred",
                       linewidth= 1,
                       just= 1)+
        labs(title = "Global Active Power", 
             x = "Global Active Power(kilowatts)",
             y = "Frequency")+
        theme_bw()+
        theme(plot.title = element_text(hjust = 0.5))

plot1



