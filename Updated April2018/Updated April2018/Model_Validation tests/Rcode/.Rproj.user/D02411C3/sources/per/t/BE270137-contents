#Load and plot  Model validation results from Excel and Gdx files

#
# Moazzam Ali Rind
# created: July 4, 2019
# updated: July 26, 2019
# moazzamalirind@gmail.com

# Model Files and Code Source: https://github.com/moazzamalirind/-Bug-flow-

###Set working directory (as per your Favorable location to access easily)
setwd("E:\\Project_Bugflow\\Montly_Models\\April_2018\\Model_Validation tests")

rm(list = ls())  #Clear history

# Load all packages and libraries

if (!require(reshape2)) { 
  install.packages("reshape2", repos="http://cran.r-project.org") 
  library(reshape2) 
}

if (!require(gdxrrw)) { 
  download.file("http://support.gams.com/lib/exe/fetch.php?media=gdxrrw:gdxrrw_1.0.4_r351.zip","gdxrrw_1.0.4.zip") 
  install.packages("gdxrrw_1.0.4.zip",repos=NULL) 
  library(gdxrrw) 
}

if (!require(RColorBrewer)) { 
  install.packages("RColorBrewer",repos="http://cran.r-project.org") 
  library(RColorBrewer) 
}


if (!require(ggplot2)) { 
  install.packages("ggplot2",repos="http://cran.r-project.org") 
  library(ggplot2) 
}

if (!require(dplyr)) { 
  install.packages("dplyr",repos="http://cran.r-project.org") 
  library(dplyr) 
}


if (!require(tidyr)) { 
  install.packages("tidyr",repos="http://cran.r-project.org") 
  library(tidyr) 
}


# Check the gams gdx interface is working
igdx("") 

###Set working directory (as per your Favorable location to access easily)
setwd("E:\\Project_Bugflow\\Montly_Models\\April_2018")


# Load igdx directory from your computer 
igdx("C:\\GAMS\\win64\\24.2");

igdx();



# Installing Package for reading Excel Files
install.packages("readxl")
install.packages("janitor")
install.packages("tibble")

# Load required packages.
library("readxl")
library(tibble)
library(janitor)
library(tidyr)
library(dplyr)
library(ggplot2) 
library(reshape2)
library(scales)
 

# Reading xlsx file
Energy<- read_excel("Model_Validation_April2018 (Rough).xlsx", sheet = 2)
#sorting the required columns
Obs_Energy <- Energy [,c(1,2,3,5,7)]
#Removing Extra Rows
Obs_Energy<-Obs_Energy[-(31:39),]

#Changing the date format
Obs_Energy$Date<- excel_numeric_to_date(as.numeric(as.character(Obs_Energy$Date)), date_system = "modern") 

#Changing Names of the columns
names(Obs_Energy) <- c("Date","Obs_Energy","EngSim_9to22and22to9","EngSim_4to9and9to4","EngSim_4periods")

# Plotting using ggplot 2
ggplot(Obs_Energy, aes(x = Date))+
  geom_line( aes(y = as.numeric(Obs_Energy), color ="red" ), size=2)+
  geom_line( aes(y = as.numeric(EngSim_9to22and22to9), color ="blue"), size=2)+
  geom_line(aes(y = as.numeric(EngSim_4to9and9to4), color ="green" ), size=2)+
  geom_line( aes(y = as.numeric(EngSim_4periods), color ="yelow" ), size=2)+
  xlab("Date") + ylab("Energy Generated (MWh)")+
  theme(axis.text.x = element_text( color="black", size=20,angle=90, hjust =0.8, vjust =1),
        axis.text.y = element_text( color="black", size=20),
        axis.title.x = element_text(color="blue", size=22, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=22, face="bold", vjust = 2))+ 
  scale_x_date(labels = date_format("%d/%m/%Y"),date_breaks = "1 days",limits = as.Date(c('2018-04-01','2018-04-30')))+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=18,face="bold"))+
  ggtitle("Model Performance vs Observed Data for Energy (April 2018)")+
  theme(plot.title = element_text(size=24,hjust =0.5, vjust = 2,face="bold"),legend.justification=c(1,0),legend.position=c(0.99, 0.01))
dev.copy(png,"Energy_validation.png", width =5000, height =2000,res=200)
dev.off() 



