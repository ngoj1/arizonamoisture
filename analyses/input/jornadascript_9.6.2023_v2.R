# <============================================================================>
# Arizona precipitation and soil moisture visualizations
# Temporal Ecology Lab
# Author: Justin Ngo
# <============================================================================>

# getwd()
# azmoist_directory <- file.path("")

# loadng in packages
library(dplyr)
library(tidyverse)
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)
library(RColorBrewer)

#reading data into RStudio and adjusting headers
jornada <- read.csv("jornada_data.csv",header=F 
                    ,skip=4
                    ,sep=","  
                    ,quot='"' 
                    , col.names=c(
                      "Date",     
                      "Year",     
                      "YearDay",     
                      "Hours",     
                      "RECORD",     
                      "Flag_RECORD",     
                      "Sitename",     
                      "VWC_Avg_301_10cm",     
                      "Flag_VWC_Avg_301_10cm",     
                      "EC_Avg_301_10cm",     
                      "Flag_EC_Avg_301_10cm",     
                      "Soil_Temp_Avg_301_10cm",     
                      "Flag_Soil_Temp_Avg_301_10cm",     
                      "P_Avg_301_10cm",     
                      "Flag_P_Avg_301_10cm",     
                      "Period_Avg_301_10cm",     
                      "Flag_Period_Avg_301_10cm",     
                      "Voltage_Ratio_Avg_301_10cm",     
                      "Flag_Voltage_Ratio_Avg_301_10cm",     
                      "VWC_Avg_301_20cm",     
                      "Flag_VWC_Avg_301_20cm",     
                      "EC_Avg_301_20cm",     
                      "Flag_EC_Avg_301_20cm",     
                      "Soil_Temp_Avg_301_20cm",     
                      "Flag_Soil_Temp_Avg_301_20cm",     
                      "P_Avg_301_20cm",     
                      "Flag_P_Avg_301_20cm",     
                      "Period_Avg_301_20cm",     
                      "Flag_Period_Avg_301_20cm",     
                      "Voltage_Ratio_Avg_301_20cm",     
                      "Flag_Voltage_Ratio_Avg_301_20cm",     
                      "VWC_Avg_301_30cm",     
                      "Flag_VWC_Avg_301_30cm",     
                      "EC_Avg_301_30cm",     
                      "Flag_EC_Avg_301_30cm",     
                      "Soil_Temp_Avg_301_30cm",     
                      "Flag_Soil_Temp_Avg_301_30cm",     
                      "P_Avg_301_30cm",     
                      "Flag_P_Avg_301_30cm",     
                      "Period_Avg_301_30cm",     
                      "Flag_Period_Avg_301_30cm",     
                      "Voltage_Ratio_Avg_301_30cm",     
                      "Flag_Voltage_Ratio_Avg_301_30cm",     
                      "VWC_Avg_302_10cm",     
                      "Flag_VWC_Avg_302_10cm",     
                      "EC_Avg_302_10cm",     
                      "Flag_EC_Avg_302_10cm",     
                      "Soil_Temp_Avg_302_10cm",     
                      "Flag_Soil_Temp_Avg_302_10cm",     
                      "P_Avg_302_10cm",     
                      "Flag_P_Avg_302_10cm",     
                      "Period_Avg_302_10cm",     
                      "Flag_Period_Avg_302_10cm",     
                      "Voltage_Ratio_Avg_302_10cm",     
                      "Flag_Voltage_Ratio_Avg_302_10cm",     
                      "VWC_Avg_302_20cm",     
                      "Flag_VWC_Avg_302_20cm",     
                      "EC_Avg_302_20cm",     
                      "Flag_EC_Avg_302_20cm",     
                      "Soil_Temp_Avg_302_20cm",     
                      "Flag_Soil_Temp_Avg_302_20cm",     
                      "P_Avg_302_20cm",     
                      "Flag_P_Avg_302_20cm",     
                      "Period_Avg_302_20cm",     
                      "Flag_Period_Avg_302_20cm",     
                      "Voltage_Ratio_Avg_302_20cm",     
                      "Flag_Voltage_Ratio_Avg_302_20cm",     
                      "VWC_Avg_302_30cm",     
                      "Flag_VWC_Avg_302_30cm",     
                      "EC_Avg_302_30cm",     
                      "Flag_EC_Avg_302_30cm",     
                      "Soil_Temp_Avg_302_30cm",     
                      "Flag_Soil_Temp_Avg_302_30cm",     
                      "P_Avg_302_30cm",     
                      "Flag_P_Avg_302_30cm",     
                      "Period_Avg_302_30cm",     
                      "Flag_Period_Avg_302_30cm",     
                      "Voltage_Ratio_Avg_302_30cm",     
                      "Flag_Voltage_Ratio_Avg_302_30cm",     
                      "VwcCorr_Avg_301_10cm",     
                      "Flag_VwcCorr_Avg_301_10cm",     
                      "VwcCorr_Avg_301_20cm",     
                      "Flag_VwcCorr_Avg_301_20cm",     
                      "VwcCorr_Avg_301_30cm",     
                      "Flag_VwcCorr_Avg_301_30cm",     
                      "VwcCorr_Avg_302_10cm",     
                      "Flag_VwcCorr_Avg_302_10cm",     
                      "VwcCorr_Avg_302_20cm",     
                      "Flag_VwcCorr_Avg_302_20cm",     
                      "VwcCorr_Avg_302_30cm",     
                      "Flag_VwcCorr_Avg_302_30cm"    ), check.names=TRUE)

#converting dates into accessible format
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(jornada$Date,format=tmpDateFormat)
View(jornada)

#checking for proper conversion
if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){jornada$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}
rm(tmpDateFormat,tmp1Date)

if (class(jornada$Year)=="factor") dt1$Year <-as.numeric(levels(dt1$Year))[as.integer(dt1$Year) ]
if (class(jornada$Year)=="character") dt1$Year <-as.numeric(dt1$Year)
if (class(jornada$YearDay)=="factor") jornada$YearDay <-as.numeric(levels(jornada$YearDay))[as.integer(jornada$YearDay) ]               
if (class(jornada$YearDay)=="character") jornada$YearDay <-as.numeric(jornada$YearDay)
if (class(jornada$Hours)=="factor") jornada$Hours <-as.numeric(levels(jornada$Hours))[as.integer(jornada$Hours) ]               
if (class(jornada$Hours)=="character") jornada$Hours <-as.numeric(jornada$Hours)
if (class(jornada$RECORD)=="factor") jornada$RECORD <-as.numeric(levels(jornada$RECORD))[as.integer(jornada$RECORD) ]               
if (class(jornada$RECORD)=="character") jornada$RECORD <-as.numeric(jornada$RECORD)
if (class(jornada$Flag_RECORD)!="factor") jornada$Flag_RECORD<- as.factor(jornada$Flag_RECORD)
if (class(jornada$VWC_Avg_301_10cm)=="factor") jornada$VWC_Avg_301_10cm <-as.numeric(levels(jornada$VWC_Avg_301_10cm))[as.integer(jornada$VWC_Avg_301_10cm) ]               
if (class(jornada$VWC_Avg_301_10cm)=="character") jornada$VWC_Avg_301_10cm <-as.numeric(jornada$VWC_Avg_301_10cm)
if (class(jornada$Flag_VWC_Avg_301_10cm)!="factor") jornada$Flag_VWC_Avg_301_10cm<- as.factor(jornada$Flag_VWC_Avg_301_10cm)
if (class(jornada$EC_Avg_301_10cm)=="factor") jornada$EC_Avg_301_10cm <-as.numeric(levels(jornada$EC_Avg_301_10cm))[as.integer(jornada$EC_Avg_301_10cm) ]               
if (class(jornada$EC_Avg_301_10cm)=="character") jornada$EC_Avg_301_10cm <-as.numeric(jornada$EC_Avg_301_10cm)
if (class(jornada$Flag_EC_Avg_301_10cm)!="factor") jornada$Flag_EC_Avg_301_10cm<- as.factor(jornada$Flag_EC_Avg_301_10cm)
if (class(jornada$Soil_Temp_Avg_301_10cm)=="factor") jornada$Soil_Temp_Avg_301_10cm <-as.numeric(levels(jornada$Soil_Temp_Avg_301_10cm))[as.integer(jornada$Soil_Temp_Avg_301_10cm) ]               
if (class(jornada$Soil_Temp_Avg_301_10cm)=="character") jornada$Soil_Temp_Avg_301_10cm <-as.numeric(jornada$Soil_Temp_Avg_301_10cm)
if (class(jornada$Flag_Soil_Temp_Avg_301_10cm)!="factor") jornada$Flag_Soil_Temp_Avg_301_10cm<- as.factor(jornada$Flag_Soil_Temp_Avg_301_10cm)
if (class(jornada$P_Avg_301_10cm)=="factor") jornada$P_Avg_301_10cm <-as.numeric(levels(jornada$P_Avg_301_10cm))[as.integer(jornada$P_Avg_301_10cm) ]               
if (class(jornada$P_Avg_301_10cm)=="character") jornada$P_Avg_301_10cm <-as.numeric(jornada$P_Avg_301_10cm)
if (class(jornada$Flag_P_Avg_301_10cm)!="factor") jornada$Flag_P_Avg_301_10cm<- as.factor(jornada$Flag_P_Avg_301_10cm)
if (class(jornada$Period_Avg_301_10cm)=="factor") jornada$Period_Avg_301_10cm <-as.numeric(levels(jornada$Period_Avg_301_10cm))[as.integer(jornada$Period_Avg_301_10cm) ]               
if (class(jornada$Period_Avg_301_10cm)=="character") jornada$Period_Avg_301_10cm <-as.numeric(jornada$Period_Avg_301_10cm)
if (class(jornada$Flag_Period_Avg_301_10cm)!="factor") jornada$Flag_Period_Avg_301_10cm<- as.factor(jornada$Flag_Period_Avg_301_10cm)
if (class(jornada$Voltage_Ratio_Avg_301_10cm)=="factor") jornada$Voltage_Ratio_Avg_301_10cm <-as.numeric(levels(jornada$Voltage_Ratio_Avg_301_10cm))[as.integer(jornada$Voltage_Ratio_Avg_301_10cm) ]               
if (class(jornada$Voltage_Ratio_Avg_301_10cm)=="character") jornada$Voltage_Ratio_Avg_301_10cm <-as.numeric(jornada$Voltage_Ratio_Avg_301_10cm)
if (class(jornada$Flag_Voltage_Ratio_Avg_301_10cm)!="factor") jornada$Flag_Voltage_Ratio_Avg_301_10cm<- as.factor(jornada$Flag_Voltage_Ratio_Avg_301_10cm)
if (class(jornada$VWC_Avg_301_20cm)=="factor") jornada$VWC_Avg_301_20cm <-as.numeric(levels(jornada$VWC_Avg_301_20cm))[as.integer(jornada$VWC_Avg_301_20cm) ]               
if (class(jornada$VWC_Avg_301_20cm)=="character") jornada$VWC_Avg_301_20cm <-as.numeric(jornada$VWC_Avg_301_20cm)
if (class(jornada$Flag_VWC_Avg_301_20cm)!="factor") jornada$Flag_VWC_Avg_301_20cm<- as.factor(jornada$Flag_VWC_Avg_301_20cm)
if (class(jornada$EC_Avg_301_20cm)=="factor") jornada$EC_Avg_301_20cm <-as.numeric(levels(jornada$EC_Avg_301_20cm))[as.integer(jornada$EC_Avg_301_20cm) ]               
if (class(jornada$EC_Avg_301_20cm)=="character") jornada$EC_Avg_301_20cm <-as.numeric(jornada$EC_Avg_301_20cm)
if (class(jornada$Flag_EC_Avg_301_20cm)!="factor") jornada$Flag_EC_Avg_301_20cm<- as.factor(jornada$Flag_EC_Avg_301_20cm)
if (class(jornada$Soil_Temp_Avg_301_20cm)=="factor") jornada$Soil_Temp_Avg_301_20cm <-as.numeric(levels(jornada$Soil_Temp_Avg_301_20cm))[as.integer(jornada$Soil_Temp_Avg_301_20cm) ]               
if (class(jornada$Soil_Temp_Avg_301_20cm)=="character") jornada$Soil_Temp_Avg_301_20cm <-as.numeric(jornada$Soil_Temp_Avg_301_20cm)
if (class(jornada$Flag_Soil_Temp_Avg_301_20cm)!="factor") jornada$Flag_Soil_Temp_Avg_301_20cm<- as.factor(jornada$Flag_Soil_Temp_Avg_301_20cm)
if (class(jornada$P_Avg_301_20cm)=="factor") jornada$P_Avg_301_20cm <-as.numeric(levels(jornada$P_Avg_301_20cm))[as.integer(jornada$P_Avg_301_20cm) ]               
if (class(jornada$P_Avg_301_20cm)=="character") jornada$P_Avg_301_20cm <-as.numeric(jornada$P_Avg_301_20cm)
if (class(jornada$Flag_P_Avg_301_20cm)!="factor") jornada$Flag_P_Avg_301_20cm<- as.factor(jornada$Flag_P_Avg_301_20cm)
if (class(jornada$Period_Avg_301_20cm)=="factor") jornada$Period_Avg_301_20cm <-as.numeric(levels(jornada$Period_Avg_301_20cm))[as.integer(jornada$Period_Avg_301_20cm) ]               
if (class(jornada$Period_Avg_301_20cm)=="character") jornada$Period_Avg_301_20cm <-as.numeric(jornada$Period_Avg_301_20cm)
if (class(jornada$Flag_Period_Avg_301_20cm)!="factor") jornada$Flag_Period_Avg_301_20cm<- as.factor(jornada$Flag_Period_Avg_301_20cm)
if (class(jornada$Voltage_Ratio_Avg_301_20cm)=="factor") jornada$Voltage_Ratio_Avg_301_20cm <-as.numeric(levels(jornada$Voltage_Ratio_Avg_301_20cm))[as.integer(jornada$Voltage_Ratio_Avg_301_20cm) ]               
if (class(jornada$Voltage_Ratio_Avg_301_20cm)=="character") jornada$Voltage_Ratio_Avg_301_20cm <-as.numeric(jornada$Voltage_Ratio_Avg_301_20cm)
if (class(jornada$Flag_Voltage_Ratio_Avg_301_20cm)!="factor") jornada$Flag_Voltage_Ratio_Avg_301_20cm<- as.factor(jornada$Flag_Voltage_Ratio_Avg_301_20cm)
if (class(jornada$VWC_Avg_301_30cm)=="factor") jornada$VWC_Avg_301_30cm <-as.numeric(levels(jornada$VWC_Avg_301_30cm))[as.integer(jornada$VWC_Avg_301_30cm) ]               
if (class(jornada$VWC_Avg_301_30cm)=="character") jornada$VWC_Avg_301_30cm <-as.numeric(jornada$VWC_Avg_301_30cm)
if (class(jornada$Flag_VWC_Avg_301_30cm)!="factor") jornada$Flag_VWC_Avg_301_30cm<- as.factor(jornada$Flag_VWC_Avg_301_30cm)
if (class(jornada$EC_Avg_301_30cm)=="factor") jornada$EC_Avg_301_30cm <-as.numeric(levels(jornada$EC_Avg_301_30cm))[as.integer(jornada$EC_Avg_301_30cm) ]               
if (class(jornada$EC_Avg_301_30cm)=="character") jornada$EC_Avg_301_30cm <-as.numeric(jornada$EC_Avg_301_30cm)
if (class(jornada$Flag_EC_Avg_301_30cm)!="factor") jornada$Flag_EC_Avg_301_30cm<- as.factor(jornada$Flag_EC_Avg_301_30cm)
if (class(jornada$Soil_Temp_Avg_301_30cm)=="factor") jornada$Soil_Temp_Avg_301_30cm <-as.numeric(levels(jornada$Soil_Temp_Avg_301_30cm))[as.integer(jornada$Soil_Temp_Avg_301_30cm) ]               
if (class(jornada$Soil_Temp_Avg_301_30cm)=="character") jornada$Soil_Temp_Avg_301_30cm <-as.numeric(jornada$Soil_Temp_Avg_301_30cm)
if (class(jornada$Flag_Soil_Temp_Avg_301_30cm)!="factor") jornada$Flag_Soil_Temp_Avg_301_30cm<- as.factor(jornada$Flag_Soil_Temp_Avg_301_30cm)
if (class(jornada$P_Avg_301_30cm)=="factor") jornada$P_Avg_301_30cm <-as.numeric(levels(jornada$P_Avg_301_30cm))[as.integer(jornada$P_Avg_301_30cm) ]               
if (class(jornada$P_Avg_301_30cm)=="character") jornada$P_Avg_301_30cm <-as.numeric(jornada$P_Avg_301_30cm)
if (class(jornada$Flag_P_Avg_301_30cm)!="factor") jornada$Flag_P_Avg_301_30cm<- as.factor(jornada$Flag_P_Avg_301_30cm)
if (class(jornada$Period_Avg_301_30cm)=="factor") jornada$Period_Avg_301_30cm <-as.numeric(levels(jornada$Period_Avg_301_30cm))[as.integer(jornada$Period_Avg_301_30cm) ]               
if (class(jornada$Period_Avg_301_30cm)=="character") jornada$Period_Avg_301_30cm <-as.numeric(jornada$Period_Avg_301_30cm)
if (class(jornada$Flag_Period_Avg_301_30cm)!="factor") jornada$Flag_Period_Avg_301_30cm<- as.factor(jornada$Flag_Period_Avg_301_30cm)
if (class(jornada$Voltage_Ratio_Avg_301_30cm)=="factor") jornada$Voltage_Ratio_Avg_301_30cm <-as.numeric(levels(jornada$Voltage_Ratio_Avg_301_30cm))[as.integer(jornada$Voltage_Ratio_Avg_301_30cm) ]               
if (class(jornada$Voltage_Ratio_Avg_301_30cm)=="character") jornada$Voltage_Ratio_Avg_301_30cm <-as.numeric(jornada$Voltage_Ratio_Avg_301_30cm)
if (class(jornada$Flag_Voltage_Ratio_Avg_301_30cm)!="factor") jornada$Flag_Voltage_Ratio_Avg_301_30cm<- as.factor(jornada$Flag_Voltage_Ratio_Avg_301_30cm)
if (class(jornada$VWC_Avg_302_10cm)=="factor") jornada$VWC_Avg_302_10cm <-as.numeric(levels(jornada$VWC_Avg_302_10cm))[as.integer(jornada$VWC_Avg_302_10cm) ]               
if (class(jornada$VWC_Avg_302_10cm)=="character") jornada$VWC_Avg_302_10cm <-as.numeric(jornada$VWC_Avg_302_10cm)
if (class(jornada$Flag_VWC_Avg_302_10cm)!="factor") jornada$Flag_VWC_Avg_302_10cm<- as.factor(jornada$Flag_VWC_Avg_302_10cm)
if (class(jornada$EC_Avg_302_10cm)=="factor") jornada$EC_Avg_302_10cm <-as.numeric(levels(jornada$EC_Avg_302_10cm))[as.integer(jornada$EC_Avg_302_10cm) ]               
if (class(jornada$EC_Avg_302_10cm)=="character") jornada$EC_Avg_302_10cm <-as.numeric(jornada$EC_Avg_302_10cm)
if (class(jornada$Flag_EC_Avg_302_10cm)!="factor") jornada$Flag_EC_Avg_302_10cm<- as.factor(jornada$Flag_EC_Avg_302_10cm)
if (class(jornada$Soil_Temp_Avg_302_10cm)=="factor") jornada$Soil_Temp_Avg_302_10cm <-as.numeric(levels(jornada$Soil_Temp_Avg_302_10cm))[as.integer(jornada$Soil_Temp_Avg_302_10cm) ]               
if (class(jornada$Soil_Temp_Avg_302_10cm)=="character") jornada$Soil_Temp_Avg_302_10cm <-as.numeric(jornada$Soil_Temp_Avg_302_10cm)
if (class(jornada$Flag_Soil_Temp_Avg_302_10cm)!="factor") jornada$Flag_Soil_Temp_Avg_302_10cm<- as.factor(jornada$Flag_Soil_Temp_Avg_302_10cm)
if (class(jornada$P_Avg_302_10cm)=="factor") jornada$P_Avg_302_10cm <-as.numeric(levels(jornada$P_Avg_302_10cm))[as.integer(jornada$P_Avg_302_10cm) ]               
if (class(jornada$P_Avg_302_10cm)=="character") jornada$P_Avg_302_10cm <-as.numeric(jornada$P_Avg_302_10cm)
if (class(jornada$Flag_P_Avg_302_10cm)!="factor") jornada$Flag_P_Avg_302_10cm<- as.factor(jornada$Flag_P_Avg_302_10cm)
if (class(jornada$Period_Avg_302_10cm)=="factor") jornada$Period_Avg_302_10cm <-as.numeric(levels(jornada$Period_Avg_302_10cm))[as.integer(jornada$Period_Avg_302_10cm) ]               
if (class(jornada$Period_Avg_302_10cm)=="character") jornada$Period_Avg_302_10cm <-as.numeric(jornada$Period_Avg_302_10cm)
if (class(jornada$Flag_Period_Avg_302_10cm)!="factor") jornada$Flag_Period_Avg_302_10cm<- as.factor(jornada$Flag_Period_Avg_302_10cm)
if (class(jornada$Voltage_Ratio_Avg_302_10cm)=="factor") jornada$Voltage_Ratio_Avg_302_10cm <-as.numeric(levels(jornada$Voltage_Ratio_Avg_302_10cm))[as.integer(jornada$Voltage_Ratio_Avg_302_10cm) ]               
if (class(jornada$Voltage_Ratio_Avg_302_10cm)=="character") jornada$Voltage_Ratio_Avg_302_10cm <-as.numeric(jornada$Voltage_Ratio_Avg_302_10cm)
if (class(jornada$Flag_Voltage_Ratio_Avg_302_10cm)!="factor") jornada$Flag_Voltage_Ratio_Avg_302_10cm<- as.factor(jornada$Flag_Voltage_Ratio_Avg_302_10cm)
if (class(jornada$VWC_Avg_302_20cm)=="factor") jornada$VWC_Avg_302_20cm <-as.numeric(levels(jornada$VWC_Avg_302_20cm))[as.integer(jornada$VWC_Avg_302_20cm) ]               
if (class(jornada$VWC_Avg_302_20cm)=="character") jornada$VWC_Avg_302_20cm <-as.numeric(jornada$VWC_Avg_302_20cm)
if (class(jornada$Flag_VWC_Avg_302_20cm)!="factor") jornada$Flag_VWC_Avg_302_20cm<- as.factor(jornada$Flag_VWC_Avg_302_20cm)
if (class(jornada$EC_Avg_302_20cm)=="factor") jornada$EC_Avg_302_20cm <-as.numeric(levels(jornada$EC_Avg_302_20cm))[as.integer(jornada$EC_Avg_302_20cm) ]               
if (class(jornada$EC_Avg_302_20cm)=="character") jornada$EC_Avg_302_20cm <-as.numeric(jornada$EC_Avg_302_20cm)
if (class(jornada$Flag_EC_Avg_302_20cm)!="factor") jornada$Flag_EC_Avg_302_20cm<- as.factor(jornada$Flag_EC_Avg_302_20cm)
if (class(jornada$Soil_Temp_Avg_302_20cm)=="factor") jornada$Soil_Temp_Avg_302_20cm <-as.numeric(levels(jornada$Soil_Temp_Avg_302_20cm))[as.integer(jornada$Soil_Temp_Avg_302_20cm) ]               
if (class(jornada$Soil_Temp_Avg_302_20cm)=="character") jornada$Soil_Temp_Avg_302_20cm <-as.numeric(jornada$Soil_Temp_Avg_302_20cm)
if (class(jornada$Flag_Soil_Temp_Avg_302_20cm)!="factor") jornada$Flag_Soil_Temp_Avg_302_20cm<- as.factor(jornada$Flag_Soil_Temp_Avg_302_20cm)
if (class(jornada$P_Avg_302_20cm)=="factor") jornada$P_Avg_302_20cm <-as.numeric(levels(jornada$P_Avg_302_20cm))[as.integer(jornada$P_Avg_302_20cm) ]               
if (class(jornada$P_Avg_302_20cm)=="character") jornada$P_Avg_302_20cm <-as.numeric(jornada$P_Avg_302_20cm)
if (class(jornada$Flag_P_Avg_302_20cm)!="factor") jornada$Flag_P_Avg_302_20cm<- as.factor(jornada$Flag_P_Avg_302_20cm)
if (class(jornada$Period_Avg_302_20cm)=="factor") jornada$Period_Avg_302_20cm <-as.numeric(levels(jornada$Period_Avg_302_20cm))[as.integer(jornada$Period_Avg_302_20cm) ]               
if (class(jornada$Period_Avg_302_20cm)=="character") jornada$Period_Avg_302_20cm <-as.numeric(jornada$Period_Avg_302_20cm)
if (class(jornada$Flag_Period_Avg_302_20cm)!="factor") jornada$Flag_Period_Avg_302_20cm<- as.factor(jornada$Flag_Period_Avg_302_20cm)
if (class(jornada$Voltage_Ratio_Avg_302_20cm)=="factor") jornada$Voltage_Ratio_Avg_302_20cm <-as.numeric(levels(jornada$Voltage_Ratio_Avg_302_20cm))[as.integer(jornada$Voltage_Ratio_Avg_302_20cm) ]               
if (class(jornada$Voltage_Ratio_Avg_302_20cm)=="character") jornada$Voltage_Ratio_Avg_302_20cm <-as.numeric(jornada$Voltage_Ratio_Avg_302_20cm)
if (class(jornada$Flag_Voltage_Ratio_Avg_302_20cm)!="factor") jornada$Flag_Voltage_Ratio_Avg_302_20cm<- as.factor(jornada$Flag_Voltage_Ratio_Avg_302_20cm)
if (class(jornada$VWC_Avg_302_30cm)=="factor") jornada$VWC_Avg_302_30cm <-as.numeric(levels(jornada$VWC_Avg_302_30cm))[as.integer(jornada$VWC_Avg_302_30cm) ]               
if (class(jornada$VWC_Avg_302_30cm)=="character") jornada$VWC_Avg_302_30cm <-as.numeric(jornada$VWC_Avg_302_30cm)
if (class(jornada$Flag_VWC_Avg_302_30cm)!="factor") jornada$Flag_VWC_Avg_302_30cm<- as.factor(jornada$Flag_VWC_Avg_302_30cm)
if (class(jornada$EC_Avg_302_30cm)=="factor") jornada$EC_Avg_302_30cm <-as.numeric(levels(jornada$EC_Avg_302_30cm))[as.integer(jornada$EC_Avg_302_30cm) ]               
if (class(jornada$EC_Avg_302_30cm)=="character") jornada$EC_Avg_302_30cm <-as.numeric(jornada$EC_Avg_302_30cm)
if (class(jornada$Flag_EC_Avg_302_30cm)!="factor") jornada$Flag_EC_Avg_302_30cm<- as.factor(jornada$Flag_EC_Avg_302_30cm)
if (class(jornada$Soil_Temp_Avg_302_30cm)=="factor") jornada$Soil_Temp_Avg_302_30cm <-as.numeric(levels(jornada$Soil_Temp_Avg_302_30cm))[as.integer(jornada$Soil_Temp_Avg_302_30cm) ]               
if (class(jornada$Soil_Temp_Avg_302_30cm)=="character") jornada$Soil_Temp_Avg_302_30cm <-as.numeric(jornada$Soil_Temp_Avg_302_30cm)
if (class(jornada$Flag_Soil_Temp_Avg_302_30cm)!="factor") jornada$Flag_Soil_Temp_Avg_302_30cm<- as.factor(jornada$Flag_Soil_Temp_Avg_302_30cm)
if (class(jornada$P_Avg_302_30cm)=="factor") jornada$P_Avg_302_30cm <-as.numeric(levels(jornada$P_Avg_302_30cm))[as.integer(jornada$P_Avg_302_30cm) ]               
if (class(jornada$P_Avg_302_30cm)=="character") jornada$P_Avg_302_30cm <-as.numeric(jornada$P_Avg_302_30cm)
if (class(jornada$Flag_P_Avg_302_30cm)!="factor") jornada$Flag_P_Avg_302_30cm<- as.factor(jornada$Flag_P_Avg_302_30cm)
if (class(jornada$Period_Avg_302_30cm)=="factor") jornada$Period_Avg_302_30cm <-as.numeric(levels(jornada$Period_Avg_302_30cm))[as.integer(jornada$Period_Avg_302_30cm) ]               
if (class(jornada$Period_Avg_302_30cm)=="character") jornada$Period_Avg_302_30cm <-as.numeric(jornada$Period_Avg_302_30cm)
if (class(jornada$Flag_Period_Avg_302_30cm)!="factor") jornada$Flag_Period_Avg_302_30cm<- as.factor(jornada$Flag_Period_Avg_302_30cm)
if (class(jornada$Voltage_Ratio_Avg_302_30cm)=="factor") jornada$Voltage_Ratio_Avg_302_30cm <-as.numeric(levels(jornada$Voltage_Ratio_Avg_302_30cm))[as.integer(jornada$Voltage_Ratio_Avg_302_30cm) ]               
if (class(jornada$Voltage_Ratio_Avg_302_30cm)=="character") jornada$Voltage_Ratio_Avg_302_30cm <-as.numeric(jornada$Voltage_Ratio_Avg_302_30cm)
if (class(jornada$Flag_Voltage_Ratio_Avg_302_30cm)!="factor") jornada$Flag_Voltage_Ratio_Avg_302_30cm<- as.factor(jornada$Flag_Voltage_Ratio_Avg_302_30cm)
if (class(jornada$VwcCorr_Avg_301_10cm)=="factor") jornada$VwcCorr_Avg_301_10cm <-as.numeric(levels(jornada$VwcCorr_Avg_301_10cm))[as.integer(jornada$VwcCorr_Avg_301_10cm) ]               
if (class(jornada$VwcCorr_Avg_301_10cm)=="character") jornada$VwcCorr_Avg_301_10cm <-as.numeric(jornada$VwcCorr_Avg_301_10cm)
if (class(jornada$Flag_VwcCorr_Avg_301_10cm)!="factor") jornada$Flag_VwcCorr_Avg_301_10cm<- as.factor(jornada$Flag_VwcCorr_Avg_301_10cm)
if (class(jornada$VwcCorr_Avg_301_20cm)=="factor") jornada$VwcCorr_Avg_301_20cm <-as.numeric(levels(jornada$VwcCorr_Avg_301_20cm))[as.integer(jornada$VwcCorr_Avg_301_20cm) ]               
if (class(jornada$VwcCorr_Avg_301_20cm)=="character") jornada$VwcCorr_Avg_301_20cm <-as.numeric(jornada$VwcCorr_Avg_301_20cm)
if (class(jornada$Flag_VwcCorr_Avg_301_20cm)!="factor") jornada$Flag_VwcCorr_Avg_301_20cm<- as.factor(jornada$Flag_VwcCorr_Avg_301_20cm)
if (class(jornada$VwcCorr_Avg_301_30cm)=="factor") jornada$VwcCorr_Avg_301_30cm <-as.numeric(levels(jornada$VwcCorr_Avg_301_30cm))[as.integer(jornada$VwcCorr_Avg_301_30cm) ]               
if (class(jornada$VwcCorr_Avg_301_30cm)=="character") jornada$VwcCorr_Avg_301_30cm <-as.numeric(jornada$VwcCorr_Avg_301_30cm)
if (class(jornada$Flag_VwcCorr_Avg_301_30cm)!="factor") jornada$Flag_VwcCorr_Avg_301_30cm<- as.factor(jornada$Flag_VwcCorr_Avg_301_30cm)
if (class(jornada$VwcCorr_Avg_302_10cm)=="factor") jornada$VwcCorr_Avg_302_10cm <-as.numeric(levels(jornada$VwcCorr_Avg_302_10cm))[as.integer(jornada$VwcCorr_Avg_302_10cm) ]               
if (class(jornada$VwcCorr_Avg_302_10cm)=="character") jornada$VwcCorr_Avg_302_10cm <-as.numeric(jornada$VwcCorr_Avg_302_10cm)
if (class(jornada$Flag_VwcCorr_Avg_302_10cm)!="factor") jornada$Flag_VwcCorr_Avg_302_10cm<- as.factor(jornada$Flag_VwcCorr_Avg_302_10cm)
if (class(jornada$VwcCorr_Avg_302_20cm)=="factor") jornada$VwcCorr_Avg_302_20cm <-as.numeric(levels(jornada$VwcCorr_Avg_302_20cm))[as.integer(jornada$VwcCorr_Avg_302_20cm) ]               
if (class(jornada$VwcCorr_Avg_302_20cm)=="character") jornada$VwcCorr_Avg_302_20cm <-as.numeric(jornada$VwcCorr_Avg_302_20cm)
if (class(jornada$Flag_VwcCorr_Avg_302_20cm)!="factor") jornada$Flag_VwcCorr_Avg_302_20cm<- as.factor(jornada$Flag_VwcCorr_Avg_302_20cm)
if (class(jornada$VwcCorr_Avg_302_30cm)=="factor") jornada$VwcCorr_Avg_302_30cm <-as.numeric(levels(jornada$VwcCorr_Avg_302_30cm))[as.integer(jornada$VwcCorr_Avg_302_30cm) ]               
if (class(jornada$VwcCorr_Avg_302_30cm)=="character") jornada$VwcCorr_Avg_302_30cm <-as.numeric(jornada$VwcCorr_Avg_302_30cm)
if (class(jornada$Flag_VwcCorr_Avg_302_30cm)!="factor") jornada$Flag_VwcCorr_Avg_302_30cm<- as.factor(jornada$Flag_VwcCorr_Avg_302_30cm)

# Convert Missing Values to NA for non-dates

jornada$Year <- ifelse((trimws(as.character(jornada$Year))==trimws("NaN")),NA,jornada$Year)               
suppressWarnings(jornada$Year <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Year))==as.character(as.numeric("NaN"))),NA,jornada$Year))
jornada$YearDay <- ifelse((trimws(as.character(jornada$YearDay))==trimws("NaN")),NA,jornada$YearDay)               
suppressWarnings(jornada$YearDay <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$YearDay))==as.character(as.numeric("NaN"))),NA,jornada$YearDay))
jornada$Hours <- ifelse((trimws(as.character(jornada$Hours))==trimws("NaN")),NA,jornada$Hours)               
suppressWarnings(jornada$Hours <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Hours))==as.character(as.numeric("NaN"))),NA,jornada$Hours))
jornada$RECORD <- ifelse((trimws(as.character(jornada$RECORD))==trimws("NaN")),NA,jornada$RECORD)               
suppressWarnings(jornada$RECORD <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$RECORD))==as.character(as.numeric("NaN"))),NA,jornada$RECORD))
jornada$VWC_Avg_301_10cm <- ifelse((trimws(as.character(jornada$VWC_Avg_301_10cm))==trimws("NaN")),NA,jornada$VWC_Avg_301_10cm)               
suppressWarnings(jornada$VWC_Avg_301_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$VWC_Avg_301_10cm))==as.character(as.numeric("NaN"))),NA,jornada$VWC_Avg_301_10cm))
jornada$EC_Avg_301_10cm <- ifelse((trimws(as.character(jornada$EC_Avg_301_10cm))==trimws("NaN")),NA,jornada$EC_Avg_301_10cm)               
suppressWarnings(jornada$EC_Avg_301_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$EC_Avg_301_10cm))==as.character(as.numeric("NaN"))),NA,jornada$EC_Avg_301_10cm))
jornada$Soil_Temp_Avg_301_10cm <- ifelse((trimws(as.character(jornada$Soil_Temp_Avg_301_10cm))==trimws("NaN")),NA,jornada$Soil_Temp_Avg_301_10cm)               
suppressWarnings(jornada$Soil_Temp_Avg_301_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Soil_Temp_Avg_301_10cm))==as.character(as.numeric("NaN"))),NA,jornada$Soil_Temp_Avg_301_10cm))
jornada$P_Avg_301_10cm <- ifelse((trimws(as.character(jornada$P_Avg_301_10cm))==trimws("NaN")),NA,jornada$P_Avg_301_10cm)               
suppressWarnings(jornada$P_Avg_301_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$P_Avg_301_10cm))==as.character(as.numeric("NaN"))),NA,jornada$P_Avg_301_10cm))
jornada$Period_Avg_301_10cm <- ifelse((trimws(as.character(jornada$Period_Avg_301_10cm))==trimws("NaN")),NA,jornada$Period_Avg_301_10cm)               
suppressWarnings(jornada$Period_Avg_301_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Period_Avg_301_10cm))==as.character(as.numeric("NaN"))),NA,jornada$Period_Avg_301_10cm))
jornada$Voltage_Ratio_Avg_301_10cm <- ifelse((trimws(as.character(jornada$Voltage_Ratio_Avg_301_10cm))==trimws("NaN")),NA,jornada$Voltage_Ratio_Avg_301_10cm)               
suppressWarnings(jornada$Voltage_Ratio_Avg_301_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Voltage_Ratio_Avg_301_10cm))==as.character(as.numeric("NaN"))),NA,jornada$Voltage_Ratio_Avg_301_10cm))
jornada$VWC_Avg_301_20cm <- ifelse((trimws(as.character(jornada$VWC_Avg_301_20cm))==trimws("NaN")),NA,jornada$VWC_Avg_301_20cm)               
suppressWarnings(jornada$VWC_Avg_301_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$VWC_Avg_301_20cm))==as.character(as.numeric("NaN"))),NA,jornada$VWC_Avg_301_20cm))
jornada$EC_Avg_301_20cm <- ifelse((trimws(as.character(jornada$EC_Avg_301_20cm))==trimws("NaN")),NA,jornada$EC_Avg_301_20cm)               
suppressWarnings(jornada$EC_Avg_301_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$EC_Avg_301_20cm))==as.character(as.numeric("NaN"))),NA,jornada$EC_Avg_301_20cm))
jornada$Soil_Temp_Avg_301_20cm <- ifelse((trimws(as.character(jornada$Soil_Temp_Avg_301_20cm))==trimws("NaN")),NA,jornada$Soil_Temp_Avg_301_20cm)               
suppressWarnings(jornada$Soil_Temp_Avg_301_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Soil_Temp_Avg_301_20cm))==as.character(as.numeric("NaN"))),NA,jornada$Soil_Temp_Avg_301_20cm))
jornada$P_Avg_301_20cm <- ifelse((trimws(as.character(jornada$P_Avg_301_20cm))==trimws("NaN")),NA,jornada$P_Avg_301_20cm)               
suppressWarnings(jornada$P_Avg_301_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$P_Avg_301_20cm))==as.character(as.numeric("NaN"))),NA,jornada$P_Avg_301_20cm))
jornada$Period_Avg_301_20cm <- ifelse((trimws(as.character(jornada$Period_Avg_301_20cm))==trimws("NaN")),NA,jornada$Period_Avg_301_20cm)               
suppressWarnings(jornada$Period_Avg_301_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Period_Avg_301_20cm))==as.character(as.numeric("NaN"))),NA,jornada$Period_Avg_301_20cm))
jornada$Voltage_Ratio_Avg_301_20cm <- ifelse((trimws(as.character(jornada$Voltage_Ratio_Avg_301_20cm))==trimws("NaN")),NA,jornada$Voltage_Ratio_Avg_301_20cm)               
suppressWarnings(jornada$Voltage_Ratio_Avg_301_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Voltage_Ratio_Avg_301_20cm))==as.character(as.numeric("NaN"))),NA,jornada$Voltage_Ratio_Avg_301_20cm))
jornada$VWC_Avg_301_30cm <- ifelse((trimws(as.character(jornada$VWC_Avg_301_30cm))==trimws("NaN")),NA,jornada$VWC_Avg_301_30cm)               
suppressWarnings(jornada$VWC_Avg_301_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$VWC_Avg_301_30cm))==as.character(as.numeric("NaN"))),NA,jornada$VWC_Avg_301_30cm))
jornada$EC_Avg_301_30cm <- ifelse((trimws(as.character(jornada$EC_Avg_301_30cm))==trimws("NaN")),NA,jornada$EC_Avg_301_30cm)               
suppressWarnings(jornada$EC_Avg_301_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$EC_Avg_301_30cm))==as.character(as.numeric("NaN"))),NA,jornada$EC_Avg_301_30cm))
jornada$Soil_Temp_Avg_301_30cm <- ifelse((trimws(as.character(jornada$Soil_Temp_Avg_301_30cm))==trimws("NaN")),NA,jornada$Soil_Temp_Avg_301_30cm)               
suppressWarnings(jornada$Soil_Temp_Avg_301_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Soil_Temp_Avg_301_30cm))==as.character(as.numeric("NaN"))),NA,jornada$Soil_Temp_Avg_301_30cm))
jornada$P_Avg_301_30cm <- ifelse((trimws(as.character(jornada$P_Avg_301_30cm))==trimws("NaN")),NA,jornada$P_Avg_301_30cm)               
suppressWarnings(jornada$P_Avg_301_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$P_Avg_301_30cm))==as.character(as.numeric("NaN"))),NA,jornada$P_Avg_301_30cm))
jornada$Period_Avg_301_30cm <- ifelse((trimws(as.character(jornada$Period_Avg_301_30cm))==trimws("NaN")),NA,jornada$Period_Avg_301_30cm)               
suppressWarnings(jornada$Period_Avg_301_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Period_Avg_301_30cm))==as.character(as.numeric("NaN"))),NA,jornada$Period_Avg_301_30cm))
jornada$Voltage_Ratio_Avg_301_30cm <- ifelse((trimws(as.character(jornada$Voltage_Ratio_Avg_301_30cm))==trimws("NaN")),NA,jornada$Voltage_Ratio_Avg_301_30cm)               
suppressWarnings(jornada$Voltage_Ratio_Avg_301_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Voltage_Ratio_Avg_301_30cm))==as.character(as.numeric("NaN"))),NA,jornada$Voltage_Ratio_Avg_301_30cm))
jornada$VWC_Avg_302_10cm <- ifelse((trimws(as.character(jornada$VWC_Avg_302_10cm))==trimws("NaN")),NA,jornada$VWC_Avg_302_10cm)               
suppressWarnings(jornada$VWC_Avg_302_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$VWC_Avg_302_10cm))==as.character(as.numeric("NaN"))),NA,jornada$VWC_Avg_302_10cm))
jornada$EC_Avg_302_10cm <- ifelse((trimws(as.character(jornada$EC_Avg_302_10cm))==trimws("NaN")),NA,jornada$EC_Avg_302_10cm)               
suppressWarnings(jornada$EC_Avg_302_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$EC_Avg_302_10cm))==as.character(as.numeric("NaN"))),NA,jornada$EC_Avg_302_10cm))
jornada$Soil_Temp_Avg_302_10cm <- ifelse((trimws(as.character(jornada$Soil_Temp_Avg_302_10cm))==trimws("NaN")),NA,jornada$Soil_Temp_Avg_302_10cm)               
suppressWarnings(jornada$Soil_Temp_Avg_302_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Soil_Temp_Avg_302_10cm))==as.character(as.numeric("NaN"))),NA,jornada$Soil_Temp_Avg_302_10cm))
jornada$P_Avg_302_10cm <- ifelse((trimws(as.character(jornada$P_Avg_302_10cm))==trimws("NaN")),NA,jornada$P_Avg_302_10cm)               
suppressWarnings(jornada$P_Avg_302_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$P_Avg_302_10cm))==as.character(as.numeric("NaN"))),NA,jornada$P_Avg_302_10cm))
jornada$Period_Avg_302_10cm <- ifelse((trimws(as.character(jornada$Period_Avg_302_10cm))==trimws("NaN")),NA,jornada$Period_Avg_302_10cm)               
suppressWarnings(jornada$Period_Avg_302_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Period_Avg_302_10cm))==as.character(as.numeric("NaN"))),NA,jornada$Period_Avg_302_10cm))
jornada$Voltage_Ratio_Avg_302_10cm <- ifelse((trimws(as.character(jornada$Voltage_Ratio_Avg_302_10cm))==trimws("NaN")),NA,jornada$Voltage_Ratio_Avg_302_10cm)               
suppressWarnings(jornada$Voltage_Ratio_Avg_302_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Voltage_Ratio_Avg_302_10cm))==as.character(as.numeric("NaN"))),NA,jornada$Voltage_Ratio_Avg_302_10cm))
jornada$VWC_Avg_302_20cm <- ifelse((trimws(as.character(jornada$VWC_Avg_302_20cm))==trimws("NaN")),NA,jornada$VWC_Avg_302_20cm)               
suppressWarnings(jornada$VWC_Avg_302_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$VWC_Avg_302_20cm))==as.character(as.numeric("NaN"))),NA,jornada$VWC_Avg_302_20cm))
jornada$EC_Avg_302_20cm <- ifelse((trimws(as.character(jornada$EC_Avg_302_20cm))==trimws("NaN")),NA,jornada$EC_Avg_302_20cm)               
suppressWarnings(jornada$EC_Avg_302_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$EC_Avg_302_20cm))==as.character(as.numeric("NaN"))),NA,jornada$EC_Avg_302_20cm))
jornada$Soil_Temp_Avg_302_20cm <- ifelse((trimws(as.character(jornada$Soil_Temp_Avg_302_20cm))==trimws("NaN")),NA,jornada$Soil_Temp_Avg_302_20cm)               
suppressWarnings(jornada$Soil_Temp_Avg_302_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Soil_Temp_Avg_302_20cm))==as.character(as.numeric("NaN"))),NA,jornada$Soil_Temp_Avg_302_20cm))
jornada$P_Avg_302_20cm <- ifelse((trimws(as.character(jornada$P_Avg_302_20cm))==trimws("NaN")),NA,jornada$P_Avg_302_20cm)               
suppressWarnings(jornada$P_Avg_302_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$P_Avg_302_20cm))==as.character(as.numeric("NaN"))),NA,jornada$P_Avg_302_20cm))
jornada$Period_Avg_302_20cm <- ifelse((trimws(as.character(jornada$Period_Avg_302_20cm))==trimws("NaN")),NA,jornada$Period_Avg_302_20cm)               
suppressWarnings(jornada$Period_Avg_302_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Period_Avg_302_20cm))==as.character(as.numeric("NaN"))),NA,jornada$Period_Avg_302_20cm))
jornada$Voltage_Ratio_Avg_302_20cm <- ifelse((trimws(as.character(jornada$Voltage_Ratio_Avg_302_20cm))==trimws("NaN")),NA,jornada$Voltage_Ratio_Avg_302_20cm)               
suppressWarnings(jornada$Voltage_Ratio_Avg_302_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Voltage_Ratio_Avg_302_20cm))==as.character(as.numeric("NaN"))),NA,jornada$Voltage_Ratio_Avg_302_20cm))
jornada$VWC_Avg_302_30cm <- ifelse((trimws(as.character(jornada$VWC_Avg_302_30cm))==trimws("NaN")),NA,jornada$VWC_Avg_302_30cm)               
suppressWarnings(jornada$VWC_Avg_302_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$VWC_Avg_302_30cm))==as.character(as.numeric("NaN"))),NA,jornada$VWC_Avg_302_30cm))
jornada$EC_Avg_302_30cm <- ifelse((trimws(as.character(jornada$EC_Avg_302_30cm))==trimws("NaN")),NA,jornada$EC_Avg_302_30cm)               
suppressWarnings(jornada$EC_Avg_302_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$EC_Avg_302_30cm))==as.character(as.numeric("NaN"))),NA,jornada$EC_Avg_302_30cm))
jornada$Soil_Temp_Avg_302_30cm <- ifelse((trimws(as.character(jornada$Soil_Temp_Avg_302_30cm))==trimws("NaN")),NA,jornada$Soil_Temp_Avg_302_30cm)               
suppressWarnings(jornada$Soil_Temp_Avg_302_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Soil_Temp_Avg_302_30cm))==as.character(as.numeric("NaN"))),NA,jornada$Soil_Temp_Avg_302_30cm))
jornada$P_Avg_302_30cm <- ifelse((trimws(as.character(jornada$P_Avg_302_30cm))==trimws("NaN")),NA,jornada$P_Avg_302_30cm)               
suppressWarnings(jornada$P_Avg_302_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$P_Avg_302_30cm))==as.character(as.numeric("NaN"))),NA,jornada$P_Avg_302_30cm))
jornada$Period_Avg_302_30cm <- ifelse((trimws(as.character(jornada$Period_Avg_302_30cm))==trimws("NaN")),NA,jornada$Period_Avg_302_30cm)               
suppressWarnings(jornada$Period_Avg_302_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Period_Avg_302_30cm))==as.character(as.numeric("NaN"))),NA,jornada$Period_Avg_302_30cm))
jornada$Voltage_Ratio_Avg_302_30cm <- ifelse((trimws(as.character(jornada$Voltage_Ratio_Avg_302_30cm))==trimws("NaN")),NA,jornada$Voltage_Ratio_Avg_302_30cm)               
suppressWarnings(jornada$Voltage_Ratio_Avg_302_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$Voltage_Ratio_Avg_302_30cm))==as.character(as.numeric("NaN"))),NA,jornada$Voltage_Ratio_Avg_302_30cm))
jornada$VwcCorr_Avg_301_10cm <- ifelse((trimws(as.character(jornada$VwcCorr_Avg_301_10cm))==trimws("NaN")),NA,jornada$VwcCorr_Avg_301_10cm)               
suppressWarnings(jornada$VwcCorr_Avg_301_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$VwcCorr_Avg_301_10cm))==as.character(as.numeric("NaN"))),NA,jornada$VwcCorr_Avg_301_10cm))
jornada$VwcCorr_Avg_301_20cm <- ifelse((trimws(as.character(jornada$VwcCorr_Avg_301_20cm))==trimws("NaN")),NA,jornada$VwcCorr_Avg_301_20cm)               
suppressWarnings(jornada$VwcCorr_Avg_301_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$VwcCorr_Avg_301_20cm))==as.character(as.numeric("NaN"))),NA,jornada$VwcCorr_Avg_301_20cm))
jornada$VwcCorr_Avg_301_30cm <- ifelse((trimws(as.character(jornada$VwcCorr_Avg_301_30cm))==trimws("NaN")),NA,jornada$VwcCorr_Avg_301_30cm)               
suppressWarnings(jornada$VwcCorr_Avg_301_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$VwcCorr_Avg_301_30cm))==as.character(as.numeric("NaN"))),NA,jornada$VwcCorr_Avg_301_30cm))
jornada$VwcCorr_Avg_302_10cm <- ifelse((trimws(as.character(jornada$VwcCorr_Avg_302_10cm))==trimws("NaN")),NA,jornada$VwcCorr_Avg_302_10cm)               
suppressWarnings(jornada$VwcCorr_Avg_302_10cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$VwcCorr_Avg_302_10cm))==as.character(as.numeric("NaN"))),NA,jornada$VwcCorr_Avg_302_10cm))
jornada$VwcCorr_Avg_302_20cm <- ifelse((trimws(as.character(jornada$VwcCorr_Avg_302_20cm))==trimws("NaN")),NA,jornada$VwcCorr_Avg_302_20cm)               
suppressWarnings(jornada$VwcCorr_Avg_302_20cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$VwcCorr_Avg_302_20cm))==as.character(as.numeric("NaN"))),NA,jornada$VwcCorr_Avg_302_20cm))
jornada$VwcCorr_Avg_302_30cm <- ifelse((trimws(as.character(jornada$VwcCorr_Avg_302_30cm))==trimws("NaN")),NA,jornada$VwcCorr_Avg_302_30cm)               
suppressWarnings(jornada$VwcCorr_Avg_302_30cm <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(jornada$VwcCorr_Avg_302_30cm))==as.character(as.numeric("NaN"))),NA,jornada$VwcCorr_Avg_302_30cm))

#Selecting for only VWC, volumetric water content
jornadaVWC <- jornada %>%
  select(Date,
         Year,
         YearDay,
         RECORD,
         Sitename,
         VWC_Avg_301_10cm,
         VWC_Avg_301_20cm,
         VWC_Avg_301_30cm,
         VWC_Avg_302_10cm,
         VWC_Avg_302_20cm,
         VWC_Avg_302_30cm)
View(jornadaVWC)

#making the data longer so I can plot multiple series on one graph
jorVWClong <- jornadaVWC %>%
  pivot_longer(cols = c(VWC_Avg_301_10cm,
                        VWC_Avg_301_20cm,
                        VWC_Avg_301_30cm,
                        VWC_Avg_302_10cm,
                        VWC_Avg_302_20cm,
                        VWC_Avg_302_30cm),
               names_to = "Depth",
               values_to = "VWC_avg")
View(jorVWClong)



#Filtering for specific years
jornada2014 <- jorVWClong %>%
  filter(Year == 2014)
jornada2015 <- jorVWClong %>%
  filter(Year == 2015)
jornada2016 <- jorVWClong %>%
  filter(Year == 2016)
jornada2017 <- jorVWClong %>%
  filter(Year == 2017)
jornada2018 <- jorVWClong %>%
  filter(Year == 2018)
jornada2019 <- jorVWClong %>%
  filter(Year == 2019)
jornada2020 <- jorVWClong %>%
  filter(Year == 2020)
jornada2021 <- jorVWClong %>%
  filter(Year == 2021)
jornada2022 <- jorVWClong %>%
  filter(Year == 2022)

#separating 301 and 302
jornada2014a <- jornada2014 %>%
  filter(grepl("301",Depth))
jornada2015a <- jornada2015 %>%
  filter(grepl("301",Depth))
jornada2016a <- jornada2016 %>%
  filter(grepl("301",Depth))
jornada2017a <- jornada2017 %>%
  filter(grepl("301",Depth))
jornada2018a <- jornada2018 %>%
  filter(grepl("301",Depth))
jornada2019a <- jornada2019 %>%
  filter(grepl("301",Depth))
jornada2020a <- jornada2020 %>%
  filter(grepl("301",Depth))
jornada2021a <- jornada2021 %>%
  filter(grepl("301",Depth))
jornada2022a <- jornada2022 %>%
  filter(grepl("301",Depth))

jornada2014 <- jorVWClong %>%
  filter(Year == 2014)
jornada2015 <- jorVWClong %>%
  filter(Year == 2015)
jornada2016 <- jorVWClong %>%
  filter(Year == 2016)
jornada2017 <- jorVWClong %>%
  filter(Year == 2017)
jornada2018 <- jorVWClong %>%
  filter(Year == 2018)
jornada2019 <- jorVWClong %>%
  filter(Year == 2019)
jornada2020 <- jorVWClong %>%
  filter(Year == 2020)
jornada2021 <- jorVWClong %>%
  filter(Year == 2021)
jornada2022 <- jorVWClong %>%
  filter(Year == 2022)

jornada2014b <- jornada2014 %>%
  filter(grepl("302",Depth))
jornada2015b <- jornada2015 %>%
  filter(grepl("302",Depth))
jornada2016b <- jornada2016 %>%
  filter(grepl("302",Depth))
jornada2017b <- jornada2017 %>%
  filter(grepl("302",Depth))
jornada2018b <- jornada2018 %>%
  filter(grepl("302",Depth))
jornada2019b <- jornada2019 %>%
  filter(grepl("302",Depth))
jornada2020b <- jornada2020 %>%
  filter(grepl("302",Depth))
jornada2021b <- jornada2021 %>%
  filter(grepl("302",Depth))
jornada2022b <- jornada2022 %>%
  filter(grepl("302",Depth))

#visualization
# jornada 2014, 301 depths
jor14a_plot <- jornada2014a %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2014)",
       y="Average volumetric water content",
       colour ="301 Depth (cm)",
       shape = "301 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_301_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_301_20cm" = "#20A16F",
                                 "VWC_Avg_301_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor14a_plot
ggsave("plots/jornada_2014_301.png",plot=jor14a_plot,scale=1,dpi=600)

# jornada 2014, 302 depths
jor14b_plot <- jornada2014b %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2014)",
       y="Average volumetric water content",
       colour ="302 Depth (cm)",
       shape = "302 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_302_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_302_20cm" = "#20A16F",
                                 "VWC_Avg_302_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor14b_plot
ggsave("plots/jornada_2014_302.png",plot=jor14b_plot,scale=1,dpi=600)

# jornada 2015, 301 depths
jor15a_plot <- jornada2015a %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2015)",
       y="Average volumetric water content",
       colour ="301 Depth (cm)",
       shape = "301 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_301_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_301_20cm" = "#20A16F",
                                 "VWC_Avg_301_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor15a_plot
ggsave("plots/jornada_2015_301.png",plot=jor15a_plot,scale=1,dpi=600)

# jornada 2015, 302 depths
jor15b_plot <- jornada2015b %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2015)",
       y="Average volumetric water content",
       colour ="302 Depth (cm)",
       shape = "302 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_302_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_302_20cm" = "#20A16F",
                                 "VWC_Avg_302_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor15b_plot
ggsave("plots/jornada_2015_302.png",plot=jor15b_plot,scale=1,dpi=600)

# jornada 2016, 301 depths
jor16a_plot <- jornada2016a %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2016)",
       y="Average volumetric water content",
       colour ="301 Depth (cm)",
       shape = "301 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_301_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_301_20cm" = "#20A16F",
                                 "VWC_Avg_301_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor16a_plot
ggsave("plots/jornada_2016_301.png",plot=jor16a_plot,scale=1,dpi=600)

# jornada 2016, 302 depths
jor16b_plot <- jornada2016b %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2016)",
       y="Average volumetric water content",
       colour ="302 Depth (cm)",
       shape = "302 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_302_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_302_20cm" = "#20A16F",
                                 "VWC_Avg_302_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor16b_plot
ggsave("plots/jornada_2016_302.png",plot=jor16b_plot,scale=1,dpi=600)

# jornada 2017, 301 depths
jor17a_plot <- jornada2017a %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2017)",
       y="Average volumetric water content",
       colour ="301 Depth (cm)",
       shape = "301 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_301_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_301_20cm" = "#20A16F",
                                 "VWC_Avg_301_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor17a_plot
ggsave("plots/jornada_2017_301.png",plot=jor17a_plot,scale=1,dpi=600)

# jornada 2017, 302 depths
jor17b_plot <- jornada2017b %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2017)",
       y="Average volumetric water content",
       colour ="302 Depth (cm)",
       shape = "302 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_302_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_302_20cm" = "#20A16F",
                                 "VWC_Avg_302_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor17b_plot
ggsave("plots/jornada_2017_302.png",plot=jor17b_plot,scale=1,dpi=600)

# jornada 2018, 301 depths
jor18a_plot <- jornada2018a %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2018)",
       y="Average volumetric water content",
       colour ="301 Depth (cm)",
       shape = "301 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_301_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_301_20cm" = "#20A16F",
                                 "VWC_Avg_301_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor18a_plot
ggsave("plots/jornada_2018_301.png",plot=jor18a_plot,scale=1,dpi=600)

# jornada 2018, 302 depths
jor18b_plot <- jornada2018b %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2018)",
       y="Average volumetric water content",
       colour ="302 Depth (cm)",
       shape = "302 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_302_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_302_20cm" = "#20A16F",
                                 "VWC_Avg_302_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor18b_plot
ggsave("plots/jornada_2018_302.png",plot=jor18b_plot,scale=1,dpi=600)

# jornada 2019, 301 depths
jor19a_plot <- jornada2019a %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2019)",
       y="Average volumetric water content",
       colour ="301 Depth (cm)",
       shape = "301 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_301_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_301_20cm" = "#20A16F",
                                 "VWC_Avg_301_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor19a_plot
ggsave("plots/jornada_2019_301.png",plot=jor19a_plot,scale=1,dpi=600)

# jornada 2019, 302 depths
jor19b_plot <- jornada2019b %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2019)",
       y="Average volumetric water content",
       colour ="302 Depth (cm)",
       shape = "302 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_302_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_302_20cm" = "#20A16F",
                                 "VWC_Avg_302_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor19b_plot
ggsave("plots/jornada_2019_302.png",plot=jor19b_plot,scale=1,dpi=600)

# jornada 2020, 301 depths
jor20a_plot <- jornada2020a %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2020)",
       y="Average volumetric water content",
       colour ="301 Depth (cm)",
       shape = "301 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_301_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_301_20cm" = "#20A16F",
                                 "VWC_Avg_301_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor20a_plot
ggsave("plots/jornada_2020_301.png",plot=jor20a_plot,scale=1,dpi=600)

# jornada 2020, 302 depths
jor20b_plot <- jornada2020b %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2020)",
       y="Average volumetric water content",
       colour ="302 Depth (cm)",
       shape = "302 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_302_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_302_20cm" = "#20A16F",
                                 "VWC_Avg_302_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor20b_plot
ggsave("plots/jornada_2020_302.png",plot=jor20b_plot,scale=1,dpi=600)

# jornada 2021, 301 depths
jor21a_plot <- jornada2021a %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2021)",
       y="Average volumetric water content",
       colour ="301 Depth (cm)",
       shape = "301 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_301_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_301_20cm" = "#20A16F",
                                 "VWC_Avg_301_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor21a_plot
ggsave("plots/jornada_2021_301.png",plot=jor21a_plot,scale=1,dpi=600)

# jornada 2021, 302 depths
jor21b_plot <- jornada2021b %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2021)",
       y="Average volumetric water content",
       colour ="302 Depth (cm)",
       shape = "302 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_302_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_302_20cm" = "#20A16F",
                                 "VWC_Avg_302_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor21b_plot
ggsave("plots/jornada_2021_302.png",plot=jor21b_plot,scale=1,dpi=600)

# jornada 2022, 301 depths
jor22a_plot <- jornada2022a %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2022)",
       y="Average volumetric water content",
       colour ="301 Depth (cm)",
       shape = "301 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_301_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_301_20cm" = "#20A16F",
                                 "VWC_Avg_301_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor22a_plot
ggsave("plots/jornada_2022_301.png",plot=jor22a_plot,scale=1,dpi=600)

# jornada 2022, 302 depths
jor22b_plot <- jornada2022b %>%
  ggplot(aes(x = YearDay,y = VWC_avg,colour = Depth,shape = Depth)) + 
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) + #when black/white, shapes will show through
  labs(x="Day of Year (2022)",
       y="Average volumetric water content",
       colour ="302 Depth (cm)",
       shape = "302 Depth (cm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("VWC_Avg_302_10cm" = "#FFD524", #colour-blind friendly hues
                                 "VWC_Avg_302_20cm" = "#20A16F",
                                 "VWC_Avg_302_30cm" = "#366EC4"),
                      labels = c(10,20,30)) +
  scale_shape_manual(values = c("circle","square","triangle"),
                     labels = c(10,20,30))
jor22b_plot
ggsave("plots/jornada_2022_302.png",plot=jor22b_plot,scale=1,dpi=600)

