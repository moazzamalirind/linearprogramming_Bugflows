#Load and plot Bug Flow Model results
#
# Moazzam Ali Rind
# created: July 4, 2019
# updated: May 15, 2020
# moazzamalirind@gmail.com

# Model Files and Code Source: https://github.com/moazzamalirind/-Bug-flow-

# 1st method for required gdx Package Installation:
# Go here and download the package: http://support.gams.com/doku.php?id=gdxrrw:interfacing_gams_and_r

# Windows installation: In the R environment, navigate to the directory containing the binary package file 
#(see download links above), and, substituting the version number for a.b.c, type: install.packages("gdxrrw_a.b.c.zip") or, 
# depending on the version of R, install.packages("gdxrrw_a.b.c.zip",repos=NULL)


#Some help and examples here: http://ftp.gamsworld.org/presentations/informs2012_gdxrrw.pdf

# or
# 2ndmethod for required gdx Package Installation:
# start here:

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

###Set working directory (as per your Favorable location to a  1.0MAFess easily)
setwd("E:\Project_Bugflow\Montly_Models\June_2018\7day_try_dailyissue\Results")


# Load igdx directory from your computer 
igdx("C:\\GAMS\\win64\\24.2");

igdx();

###############################################
## Read in Deterministic Model Results
###############################################

gdxfile <- "Junel8_trial.gdx"

gdxfile

# Output summary of gdx file contents to console
#gdxInfo(gdxName = gdxfile, dump=FALSE, returnList=TRUE, returnDF=TRUE)

#  Objective functions values under all senarios with all equations intact.
Objval_alleqs <- rgdx(gdxfile, list(name="Fstore", form = "full"))$val

#  Objective functions values under all senarios with all equations intact.
Objval_nosim <- rgdx(gdxfile, list(name="FStore2", form = "full"))$val

# Volume senario values
monthlyVol_values <- rgdx(gdxfile, list(name="Vol_monthlyrelease", form = "full"))$val

#Release values under extreme points with respect to different senarios and all equations intact
Extpoint_rel_alleqs <- rgdx(gdxfile, list(name="RStore", form = "full"))$val

#Release values under extreme points with respect to different senarios and without simulation equation intact
Extpoint_rel_nosim <- rgdx(gdxfile, list(name="RStore2", form = "full"))$val

#Energy Generated values under extreme points with respect to different senarios and all equations intact
Extpoint_EnergyGen_alleqs<- rgdx(gdxfile, list(name="XStore", form = "full"))$val

#Energy Generated values under extreme points with respect to different senarios and without simulation equations intact
Extpoint_EnergyGen_nosim<- rgdx(gdxfile, list(name="XStore2", form = "full"))$val



#Convert to dataframes
dfmonthvol<- as.data.frame.table(monthlyVol_values)
dfobj_alleqs <- as.data.frame.table(Objval_alleqs)
dfobj_nosim <- as.data.frame.table(Objval_nosim)
dfExtRel_alleqs<- as.data.frame.table(Extpoint_rel_alleqs)
dfExtRel_nosim<- as.data.frame.table(Extpoint_rel_nosim)
dfExtEnergy_Bugflow<- as.data.frame.table(Extpoint_EnergyGen_alleqs)
dfExtEnergy_optimization<- as.data.frame.table(Extpoint_EnergyGen_nosim)



# Make Variable for Each Desired Item
dfobj_alleqs$Bugflow <- dfobj_alleqs$Freq
dfobj_nosim$optimization <- dfobj_nosim$Freq
dfExtRel_alleqs$ExtRel_Bugflow<-dfExtRel_alleqs$Freq
dfExtRel_nosim$ExtRel_optimization <- dfExtRel_nosim$Freq 
dfExtEnergy_Bugflow$ExtEnergy_Bugflow <- dfExtEnergy_Bugflow$Freq 
dfExtEnergy_optimization$ExtEnergy_optimization <-dfExtEnergy_optimization$Freq 


#Merge the related dataframes into one

#Objective values
dfResults_obj <- merge(dfobj_alleqs, dfobj_nosim[,c("f2","f","tot_vol","optimization")],by=c("f2","f","tot_vol"), all.x=TRUE, sort=TRUE)

#Release Extreme points 
dfResults_ExtRel <- merge(dfExtRel_alleqs,dfExtRel_nosim[,c("f2","d","tot_vol","p","ExtRel_optimization")],by=c("f2","d","tot_vol","p"), all.x=TRUE, sort=TRUE)

#Energy Extreme points
dfResults_ExtEnergy <- merge(dfExtEnergy_Bugflow,dfExtEnergy_optimization[,c("f2","d","tot_vol","p","ExtEnergy_optimization")],by=c("f2","d","tot_vol","p"), all.x=TRUE, sort=TRUE)



#############################################
#Rearranging and Plotting Section......
#############################################
library(ggplot2) 
library(reshape2)
library(dplyr) 
library(plyr) 
library(RColorBrewer)
library(plotly)
#############################################

#melting both cases (model with all Eqaution and Without Simulation eq) into one column
Objective_Melt <- melt(dfResults_obj, id=c("f2","f","tot_vol","Freq"))

#filtering the data frame with respect to Objective
# Filtering the data frame for only BugIndex Objective
Obj_Bug<- Objective_Melt[Objective_Melt[, "f"]=='BugIndex', ]
Obj_Bug<- Obj_Bug[with(Obj_Bug,order(tot_vol)),]

# Filtering the data frame for only Hydro Objective
Obj_Hydro<- Objective_Melt[Objective_Melt[, "f"]=='Hydro', ]
Obj_Hydro$Hydro_Obj<- Obj_Hydro$value
Obj_Hydro<- Obj_Hydro[with(Obj_Hydro,order(tot_vol)),]

#Merging both filtered data frame in one
Obj_Bug$Hydro_Obj<-Obj_Hydro$Hydro_Obj

#Removing Freq Column bcz its extra and confusing here.
Obj_Bug$Freq<- NULL
Obj_Bug$f2<- NULL

#melting both cases (model with all Eqaution and Without Simulation eq) into one column
Obj_Melt <- melt(Obj_Bug, id=c("tot_vol","f","Hydro_Obj","variable","value"))

Obj_Melt<- Obj_Melt[with(Obj_Melt,order(tot_vol)),]

#Making copy of Obj-Melt data frame inorder to further filtering.
Obj2 <- Obj_Melt

#Removing the columns from the data frame which are not needed for plot.
Obj2$f <- NULL
names(Obj2)[4]<-"BugIndex"


#Creating New Column for linkage with total volume data frame.
Obj2$Tot_Vol_AF<-Obj2$tot_vol

#Replacing values of current data frame with values of total monthly volume used in Gams model
Obj2$Tot_Vol_AF<- ifelse(Obj2$tot_vol== "V1",dfmonthvol[1,]$Freq ,
                ifelse(Obj2$tot_vol== "V2",dfmonthvol[2,]$Freq ,
                ifelse(Obj2$tot_vol== "V3",dfmonthvol[3,]$Freq ,
                ifelse(Obj2$tot_vol== "V4",dfmonthvol[4,]$Freq ,
                ifelse(Obj2$tot_vol== "V5",dfmonthvol[5,]$Freq , "Observed")))))

#converting the observed flow number to the nearest decimal
Obj2$Tot_Vol_AF<- as.factor(replace(Obj2$Tot_Vol_AF,Obj2$Tot_Vol_AF=='2e+05','200000'))


#Renaming some value in column variable
Obj2$variable <- as.character(Obj2$variable)
Obj2$variable[Obj2$variable=="Bugflow"] <- "Bugflow_2days"

#Coverting it into character
Obj2$tot_vol <- as.character(Obj2$tot_vol)
Obj2$Tot_Vol_AF <- as.character(Obj2$Tot_Vol_AF)

#ideal point
Obj2[nrow(Obj2) + 1,] <- c("Ideal_Point",max(Obj2$Hydro_Obj),"Ideal_Point",min(Obj2$BugIndex),"Ideal_Point")

Obj2$Hydro_Obj <-as.numeric(Obj2$Hydro_Obj)
Obj2$BugIndex <-as.numeric(Obj2$BugIndex)
Obj2$Hydro_Obj <-Obj2$Hydro_Obj/10^6


#####################################################
# Plotting Objective Values (Pareto-optimal Curve)
#####################################################


# With blue Palette color
ggplot(Obj2, aes(Hydro_Obj,BugIndex,color = Tot_Vol_AF, shape=variable,linetype = variable))+
  geom_point(stat = "identity", size=6)+
  geom_path(size=1.5)+
  xlab("Hydro-Power Revenue Generated(Million $)") + ylab("Hydro Peaking Index")+
  theme_gray(base_size = 16)+
  theme(panel.background = element_rect(fill = "gray80", colour = "#6D9EC1", size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"))+
  theme(axis.text.x = element_text( color="black", size=20, angle=45, hjust =0.8, vjust =1),
        axis.text.y = element_text( color="black", size=20),
        axis.title.x = element_text(color="blue", size=24, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=24, face="bold", vjust = 2))+ 
  scale_x_continuous(breaks = seq(0,40,1))+
  scale_y_continuous(breaks = seq(0,0.5,0.1))+
  theme(legend.title=element_text(size=22,face="bold"))+
  theme(legend.text=element_text(size=22,face="bold"))+
  ggtitle("Pareto-Optimal Curve with 10000 cfs BugFlow (June-2018)")+
  theme(plot.title = element_text(size=30,hjust =0.5, vjust = 2,face="bold"),legend.justification=c(1,0))
#scale_color_manual(name="Vol_Ac-ft",values=c( "Ideal_Point"= "red","Observed"="black","V1" = "aliceblue", "V2" = "slategray1", "V3" = "steelblue2", "V4" = "royalblue2","V5" = "midnightblue" ))
dev.copy(png,"Pareto-optimal(Bluecolor).png", width =5000, height =2500,res=200)
dev.off()



#####################################################
# Plotting Release
#####################################################
#Plotting Release for Extreme points for BugIndex (Lowest Extreme Value)and Hydro Power Objective (Highest End) with total Montly Release of 1.0 MAF.
#filtering the data frame with respect to Objective and Volume Senario

#melting both cases (model with all Eqaution and Without Simulation eq) into one column
Ext_RelMelt <- melt(dfResults_ExtRel, id=c("f2","d","tot_vol","p","Freq"))

#Renaming some value in column variable
Ext_RelMelt$variable<- as.character(Ext_RelMelt$variable)
Ext_RelMelt$variable[Ext_RelMelt$variable=="ExtRel_Bugflow"] <- "ExtRel_Bugflow_2days"


#################
#BugIndex and V1
#################
# Filtering the data frame for only V1 Senario
Ext_Rel1<- Ext_RelMelt[Ext_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only BugIndex
Ext_Rel2 <- Ext_Rel1[Ext_Rel1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to Periods squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Rel2$new <- paste(Ext_Rel2$d,Ext_Rel2$p)
#Turn your 'new' column into a character vector
Ext_Rel2$new <- as.character(Ext_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Rel2$new <- factor(Ext_Rel2$new, levels=unique(Ext_Rel2$new))

 # Plotting using ggplot 2
ggplot(data = Ext_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2)+
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text( color="black", size=12, face="bold",angle=90, hjust =-0.3,vjust =0),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(6000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Lowest Extreme BugIndex value for V1  Total Volume")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Extreme_bugindex_Release_V1.png", width =2000, height = 1000,res=150)
dev.off() 


#################
#BugIndex and V2
#################
# Filtering the data frame for only V2 Senario
Ext_Rel1<- Ext_RelMelt[Ext_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only BugIndex
Ext_Rel2 <- Ext_Rel1[Ext_Rel1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to Periods squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Rel2$new <- paste(Ext_Rel2$d,Ext_Rel2$p)
#Turn your 'new' column into a character vector
Ext_Rel2$new <- as.character(Ext_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Rel2$new <- factor(Ext_Rel2$new, levels=unique(Ext_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Ext_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2)+
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text( color="black", size=12, face="bold",angle=90, hjust =-0.3,vjust =0),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(6000,40000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Lowest Extreme BugIndex value for V2 Total Volume")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Extreme_bugindex_Release_V2.png", width =2000, height = 1000,res=150)
dev.off()



#################
#BugIndex and V3
#################
# Filtering the data frame for only V3 Senario
Ext_Rel1<- Ext_RelMelt[Ext_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only BugIndex
Ext_Rel2 <- Ext_Rel1[Ext_Rel1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to Periods squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Rel2$new <- paste(Ext_Rel2$d,Ext_Rel2$p)
#Turn your 'new' column into a character vector
Ext_Rel2$new <- as.character(Ext_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Rel2$new <- factor(Ext_Rel2$new, levels=unique(Ext_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Ext_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2)+
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text( color="black", size=12, face="bold",angle=90, hjust =-0.3,vjust =0),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(6000,40000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Lowest Extreme BugIndex value for V3 Total Volume")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Extreme_bugindex_Release_V3.png", width =2000, height = 1000,res=150)
dev.off()


#################
#BugIndex and V4
#################
# Filtering the data frame for only V4 Senario
Ext_Rel1<- Ext_RelMelt[Ext_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only BugIndex
Ext_Rel2 <- Ext_Rel1[Ext_Rel1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to Periods squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Rel2$new <- paste(Ext_Rel2$d,Ext_Rel2$p)
#Turn your 'new' column into a character vector
Ext_Rel2$new <- as.character(Ext_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Rel2$new <- factor(Ext_Rel2$new, levels=unique(Ext_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Ext_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2)+
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text( color="black", size=12, face="bold",angle=90, hjust =-0.3,vjust =0),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(8000,40000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Lowest Extreme BugIndex value for V4 Total Volume")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Extreme_bugindex_Release_V4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#BugIndex and V5
#################
# Filtering the data frame for only V5 Senario
Ext_Rel1<- Ext_RelMelt[Ext_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only BugIndex
Ext_Rel2 <- Ext_Rel1[Ext_Rel1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to Periods squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Rel2$new <- paste(Ext_Rel2$d,Ext_Rel2$p)
#Turn your 'new' column into a character vector
Ext_Rel2$new <- as.character(Ext_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Rel2$new <- factor(Ext_Rel2$new, levels=unique(Ext_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Ext_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2)+
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text( color="black", size=12, face="bold",angle=90, hjust =-0.3,vjust =0),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(8000,40000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Lowest Extreme BugIndex value for V5 Total Volume")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Extreme_bugindex_Release_V5.png", width =2000, height = 1000,res=150)
dev.off()

##########################
#HydroPowerobjective and V1
##########################
# Filtering the data frame for only V1 Senario
Ext_Rel1<- Ext_RelMelt[Ext_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for onlyHydro
Ext_Rel2 <- Ext_Rel1[Ext_Rel1[, "f2"]=='Hydro', ]
# Reording the dataframe with respect to Periods squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Rel2$new <- paste(Ext_Rel2$d,Ext_Rel2$p)
#Turn your 'new' column into a character vector
Ext_Rel2$new <- as.character(Ext_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Rel2$new <- factor(Ext_Rel2$new, levels=unique(Ext_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Ext_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Extreme Hydro value for V1 Total Volume")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Extreme_Hydro_Release_V1.png", width =2000, height = 1000,res=150)
dev.off()


##########################
#HydroPowerobjective and V2
##########################
# Filtering the data frame for only V2 Senario
Ext_Rel1<- Ext_RelMelt[Ext_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for onlyHydro
Ext_Rel2 <- Ext_Rel1[Ext_Rel1[, "f2"]=='Hydro', ]
# Reording the dataframe with respect to Periods squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Rel2$new <- paste(Ext_Rel2$d,Ext_Rel2$p)
#Turn your 'new' column into a character vector
Ext_Rel2$new <- as.character(Ext_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Rel2$new <- factor(Ext_Rel2$new, levels=unique(Ext_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Ext_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Extreme Hydro value for V2 Total Volume")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Extreme_Hydro_Release_V2.png", width =2000, height = 1000,res=150)
dev.off()


##########################
#HydroPowerobjective and V3
##########################
# Filtering the data frame for only V3 Senario
Ext_Rel1<- Ext_RelMelt[Ext_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for onlyHydro
Ext_Rel2 <- Ext_Rel1[Ext_Rel1[, "f2"]=='Hydro', ]
# Reording the dataframe with respect to Periods squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Rel2$new <- paste(Ext_Rel2$d,Ext_Rel2$p)
#Turn your 'new' column into a character vector
Ext_Rel2$new <- as.character(Ext_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Rel2$new <- factor(Ext_Rel2$new, levels=unique(Ext_Rel2$new))


# Plotting using ggplot 2
ggplot(data = Ext_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Extreme Hydro value for V3 Total Volume")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Extreme_Hydro_Release_V3.png", width =2000, height = 1000,res=150)
dev.off()



##########################
#HydroPowerobjective and V4
##########################
# Filtering the data frame for only V4 Senario
Ext_Rel1<- Ext_RelMelt[Ext_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for onlyHydro
Ext_Rel2 <- Ext_Rel1[Ext_Rel1[, "f2"]=='Hydro', ]
# Reording the dataframe with respect to Periods squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Rel2$new <- paste(Ext_Rel2$d,Ext_Rel2$p)
#Turn your 'new' column into a character vector
Ext_Rel2$new <- as.character(Ext_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Rel2$new <- factor(Ext_Rel2$new, levels=unique(Ext_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Ext_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Extreme Hydro value for V4 Total Volume")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Extreme_Hydro_Release_V4.png", width =2000, height = 1000,res=150)
dev.off()

##########################
#HydroPowerobjective and V5
##########################
# Filtering the data frame for only V5 Senario
Ext_Rel1<- Ext_RelMelt[Ext_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for onlyHydro
Ext_Rel2 <- Ext_Rel1[Ext_Rel1[, "f2"]=='Hydro', ]
# Reording the dataframe with respect to Periods squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Rel2$new <- paste(Ext_Rel2$d,Ext_Rel2$p)
#Turn your 'new' column into a character vector
Ext_Rel2$new <- as.character(Ext_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Rel2$new <- factor(Ext_Rel2$new, levels=unique(Ext_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Ext_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Extreme Hydro value for V5 Total Volume")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Extreme_Hydro_Release_V5.png", width =2000, height = 1000,res=150)
dev.off()
