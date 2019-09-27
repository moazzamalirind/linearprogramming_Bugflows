#Load and plot Bug Flow Model results
#
# Moazzam Ali Rind
# created: July 4, 2019
# updated: July 26, 2019
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

###Set working directory (as per your Favorable location to access easily)
setwd("E:\\Project_Bugflow\\Montly_Models\\April_2018")


# Load igdx directory from your computer 
igdx("C:\\GAMS\\win64\\24.2");

igdx();

###############################################
## Read in Deterministic Model Results
###############################################

gdxfile <- "GamsResult_Apri1l8.gdx"

gdxfile

# Output summary of gdx file contents to console
#gdxInfo(gdxName = gdxfile, dump=FALSE, returnList=TRUE, returnDF=TRUE)

#  Objective functions values under all senarios with all equations intact.
Objval_alleqs <- rgdx(gdxfile, list(name="Obj_Save", form = "full"))$val

#  Objective functions values under all senarios with all equations intact.
Objval_nosim <- rgdx(gdxfile, list(name="Obj_Save2", form = "full"))$val

# Volume senario values
monthlyVol_values <- rgdx(gdxfile, list(name="Vol_monthlyrelease", form = "full"))$val

#Release values under extreme points with respect to different senarios and all equations intact
Extpoint_rel_alleqs <- rgdx(gdxfile, list(name="RStore", form = "full"))$val

#Release values under extreme points with respect to different senarios and without simulation equation intact
Extpoint_rel_nosim <- rgdx(gdxfile, list(name="RStore2", form = "full"))$val

#Release values with all equations intact under all the different senarios
Release_alleqs <- rgdx(gdxfile, list(name="ReleaseSave", form = "full"))$val

#Release values without simulation equation intact under all the different senarios
Release_nosim <- rgdx(gdxfile, list(name="ReleaseSave2", form = "full"))$val

#Energy Generated values under extreme points with respect to different senarios and all equations intact
Extpoint_EnergyGen_alleqs<- rgdx(gdxfile, list(name="XStore", form = "full"))$val

#Energy Generated values under extreme points with respect to different senarios and without simulation equations intact
Extpoint_EnergyGen_nosim<- rgdx(gdxfile, list(name="XStore2", form = "full"))$val

#Energy Generated values with all equations intact under all the different senarios
EnergyGen_alleqs <- rgdx(gdxfile, list(name="EnergyGen_save", form = "full"))$val

#Energy Generated values without simulation equation intact under all the different senarios
EnergyGen_nosim <- rgdx(gdxfile, list(name= "EnergyGen_save2", form = "full"))$val

#Storage values under extreme points with respect to different senarios and all equations intact
Extpoint_storage_alleqs<- rgdx(gdxfile, list(name="Sstore", form = "full"))$val

#Storage values under extreme points with respect to different senarios and without simulation equation intact
Extpoint_storage_nosim<- rgdx(gdxfile, list(name="Sstore2", form = "full"))$val

#Storage values with all equations intact under all the different senarios
Storage_alleqs<- rgdx(gdxfile, list(name="Storage_Save", form = "full"))$val

#Storage values without simulation equation intact under all the different senarios
Storage_nosim<- rgdx(gdxfile, list(name="Storage_Save2", form = "full"))$val

#Convert to dataframes
dfmonthvol<- as.data.frame.table(monthlyVol_values)
dfobj_alleqs <- as.data.frame.table(Objval_alleqs)
dfobj_nosim <- as.data.frame.table(Objval_nosim)
dfExtRel_alleqs<- as.data.frame.table(Extpoint_rel_alleqs)
dfExtRel_nosim<- as.data.frame.table(Extpoint_rel_nosim)
dfRel_alleqs<- as.data.frame.table(Release_alleqs)
dfRel_nosim<- as.data.frame.table(Release_nosim)
dfExtEnergy_alleqs<- as.data.frame.table(Extpoint_EnergyGen_alleqs)
dfExtEnergy_nosim<- as.data.frame.table(Extpoint_EnergyGen_nosim)
dfEnergy_alleqs<- as.data.frame.table(EnergyGen_alleqs)
dfEnergy_nosim<- as.data.frame.table(EnergyGen_nosim)
dfExtstor_alleqs<- as.data.frame.table(Extpoint_storage_alleqs)
dfExtstor_nosim<- as.data.frame.table(Extpoint_storage_nosim)
dfstor_alleqs<- as.data.frame.table(Storage_alleqs)
dfstor_nosim<- as.data.frame.table(Storage_nosim)

# Make Variable for Each Desired Item
dfobj_alleqs$obj_alleqs <- dfobj_alleqs$Freq
dfobj_nosim$obj_nosim <- dfobj_nosim$Freq
dfExtRel_alleqs$ExtRel_alleqs<-dfExtRel_alleqs$Freq
dfRel_alleqs$Rel_alleqs <- dfRel_alleqs$Freq
dfExtRel_nosim$ExtRel_nosim <- dfExtRel_nosim$Freq 
dfRel_nosim$Rel_nosim <- dfRel_nosim$Freq
dfExtEnergy_alleqs$ExtEnergy_alleqs <- dfExtEnergy_alleqs$Freq 
dfEnergy_alleqs$Energy_alleqs <- dfEnergy_alleqs$Freq
dfExtEnergy_nosim$ExtEnergy_nosim <-dfExtEnergy_nosim$Freq 
dfEnergy_nosim$Energy_nosim <- dfEnergy_nosim$Freq
dfExtstor_alleqs$Extstor_alleqs <- dfExtstor_alleqs$Freq 
dfstor_alleqs$stor_alleqs <- dfstor_alleqs$Freq
dfExtstor_nosim$Extstor_nosim <- dfExtstor_nosim$Freq 
dfstor_nosim$stor_nosim  <- dfstor_nosim$Freq

#Merge the related dataframes into one

#Objectives With senarios
dfResults_obj <- merge(dfobj_alleqs, dfobj_nosim[,c("f","All_cases","tot_vol","obj_nosim")],by=c("f","All_cases","tot_vol"), all.x=TRUE, sort=TRUE)

#Release Extreme points 
dfResults_ExtRel <- merge(dfExtRel_alleqs,dfExtRel_nosim[,c("f2","d","tot_vol","p","ExtRel_nosim")],by=c("f2","d","tot_vol","p"), all.x=TRUE, sort=TRUE)

#Release With senarios
dfResults_ScenRel <- merge(dfRel_alleqs,dfRel_nosim[,c("tot_vol","Scen","d","p","Rel_nosim")],by=c("tot_vol","Scen","d","p"), all.x=TRUE, sort=TRUE)

#Energy Extreme points
dfResults_ExtEnergy <- merge(dfExtEnergy_alleqs,dfExtEnergy_nosim[,c("f2","d","tot_vol","p","ExtEnergy_nosim")],by=c("f2","d","tot_vol","p"), all.x=TRUE, sort=TRUE)

#Energy With senarios
dfResults_ScenEnergy <- merge(dfEnergy_alleqs,dfEnergy_nosim[,c("tot_vol","Scen","d","p","Energy_nosim")],by=c("tot_vol","Scen","d","p"), all.x=TRUE, sort=TRUE)

#Storage Extreme points
dfResults_Extstorage <- merge(dfExtstor_alleqs,dfExtstor_nosim[,c("f2","d","tot_vol","Extstor_nosim")],by=c("f2","d","tot_vol"), all.x=TRUE, sort=TRUE)

#storage With senarios
dfResults_Scenstorage <- merge(dfstor_alleqs,dfstor_nosim[,c("tot_vol","Scen","d","stor_nosim")],by=c("tot_vol","Scen","d"), all.x=TRUE, sort=TRUE)


#############################################
#Rearranging and Plotting Section......
#############################################
library(ggplot2) 
library(reshape2)
library(dplyr) 
#############################################

#melting both cases (model with all Eqaution and Without Simulation eq) into one column
Objective_Melt <- melt(dfResults_obj, id=c("f","tot_vol","All_cases","Freq"))

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

#melting both cases (model with all Eqaution and Without Simulation eq) into one column
Obj_Melt <- melt(Obj_Bug, id=c("tot_vol","All_cases","f","Hydro_Obj","variable","value"))

Obj_Melt<- Obj_Melt[with(Obj_Melt,order(tot_vol)),]

#Making copy of Obj-Melt data frame inorder to further filtering.
Obj2 <- Obj_Melt

#Removing the columns from the data frame which are not needed for plot.
Obj2$All_cases<- NULL
Obj2$f <- NULL


#############################################
#Reading the validation resuls 
#############################################
gdxfile2 <- "Validation_April2018.gdx"
gdxfile2

#  Objective functions from validation run
val_Objective <- rgdx(gdxfile2, list(name="FStore", form = "full"))$val
dfobj_validation <- as.data.frame.table(val_Objective)
Obj_val<- dfobj_validation[dfobj_validation[, "f2"]=='Hydro', ]
final_df <- as.data.frame(t(Obj_val))
names(final_df)[1]<-"BugIndex"
names(final_df)[2]<-"Hydro"
Obj<-final_df[-c(1,2), ]

########################################################

#Creating a new data frame for validation results inorder to merge them with the previous data frame.
O2<- data.frame(tot_vol="Observed",Hydro_Obj= as.numeric(as.character(Obj$Hydro)), variable="Observed", value=as.numeric(as.character(Obj$BugIndex)))

#Binding Data frames
Obj2 <- rbind(Obj2, O2)

#Creating New Column for linkage with total volume data frame.
Obj2$Tot_Vol_AF<-Obj2$tot_vol

#Replacing values of current data frame with values of total monthly volume used in Gams model
Obj2$Tot_Vol_AF<- ifelse(Obj2$tot_vol== "V1",dfmonthvol[1,]$Freq ,
                ifelse(Obj2$tot_vol== "V2",dfmonthvol[2,]$Freq ,
                ifelse(Obj2$tot_vol== "V3",dfmonthvol[3,]$Freq ,
                ifelse(Obj2$tot_vol== "V4",dfmonthvol[4,]$Freq ,
                ifelse(Obj2$tot_vol== "V5",dfmonthvol[5,]$Freq , "Observed")))))

#####################################################
# Plotting Objective Values (Pareto-optimal Curve)
#####################################################
ggplot(Obj2, aes(Hydro_Obj,value,color=Tot_Vol_AF, shape=variable,linetype = variable))+
  geom_point(stat = "identity", size=6)+
  geom_line(size=1.5)+
  xlab("Hydro-Power Revenue Generated($)") + ylab("Hydro Peaking Index")+
  theme(axis.text.x = element_text( color="black", size=20, angle=45, hjust =0.8, vjust =1),
        axis.text.y = element_text( color="black", size=20),
        axis.title.x = element_text(color="blue", size=24, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=24, face="bold", vjust = 2))+ 
  scale_x_continuous(breaks = seq(10000000,24000000,1000000))+
  scale_y_continuous(breaks = seq(0,0.7,0.1))+
  theme(legend.title=element_text(size=22,face="bold"))+
  theme(legend.text=element_text(size=22,face="bold"))+
  ggtitle("Pareto-Optimal Curve (April-2018)")+
  theme(plot.title = element_text(size=30,hjust =0.5, vjust = 2,face="bold"),legend.justification=c(1,0),legend.position=c(0.99, 0.01))
dev.copy(png,"Pareto-optimal.png", width =5000, height =2500,res=200)
dev.off() 


#####################################################
# Plotting Release
#####################################################
#Plotting Release for Extreme points for BugIndex (Lowest Extreme Value)and Hydro Power Objective (Highest End) with total Montly Release of 0.8MAF.
#filtering the data frame with respect to Objective and Volume Senario

#melting both cases (model with all Eqaution and Without Simulation eq) into one column
Ext_RelMelt <- melt(dfResults_ExtRel, id=c("f2","d","tot_vol","p","Freq"))

#################
#BugIndex and V1
#################
# Filtering the data frame for only V1 Senario
Ext_Rel1<- Ext_RelMelt[Ext_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only BugIndex
Ext_Rel2 <- Ext_Rel1[Ext_Rel1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to Days squence.
Ext_Rel2 <- Ext_Rel2[with(Ext_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Rel2$new <- paste(Ext_Rel2$d,Ext_Rel2$p)
#Turn your 'new' column into a character vector
Ext_Rel2$new <- as.character(Ext_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Rel2$new <- factor(Ext_Rel2$new, levels=unique(Ext_Rel2$new))

#geom_line(aes(linetype = variable), size=1.2) +
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
  scale_y_continuous(breaks = seq(8000,16000,500))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.865, 0.01))+
  ggtitle("Releases under Lowest Extreme BugIndex value for 0.5MAF senario")+
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
  scale_y_continuous(breaks = seq(8000,16000,500))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.865, 0.01))+
  ggtitle("Releases under Lowest Extreme BugIndex value for 0.6MAF senario")+
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
  scale_y_continuous(breaks = seq(8000,16000,500))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.865, 0.01))+
  ggtitle("Releases under Lowest Extreme BugIndex value for 0.7MAF senario")+
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
  scale_y_continuous(breaks = seq(8000,18000,500))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.865, 0.01))+
  ggtitle("Releases under Lowest Extreme BugIndex value for 0.8MAF senario")+
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
  scale_y_continuous(breaks = seq(8000,22000,500))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.865, 0.01))+
  ggtitle("Releases under Lowest Extreme BugIndex value for 0.9MAF senario")+
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
  scale_y_continuous(breaks = seq(3000,280000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.75, 0.7))+
  ggtitle("Releases under Extreme Hydro value for 0.5MAF senario")+
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
  scale_y_continuous(breaks = seq(3000,280000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.75, 0.7))+
  ggtitle("Releases under Extreme Hydro value for 0.6MAF senario")+
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
  scale_y_continuous(breaks = seq(3000,280000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.64, 0.7))+
  ggtitle("Releases under Extreme Hydro value for 0.7MAF senario")+
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
  scale_y_continuous(breaks = seq(3000,280000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Extreme Hydro value for 0.8MAF senario")+
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
  scale_y_continuous(breaks = seq(3000,280000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Extreme Hydro value for 0.9MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Extreme_Hydro_Release_V5.png", width =2000, height = 1000,res=150)
dev.off()

#Plotting Release for Scenarios of  BugIndex  with various total Montly Release senarios.
#filtering the data frame with respect to Objective and Volume Senario

#melting both cases (model with all Eqaution and Without Simulation eq) into one column
Scen_RelMelt <- melt(dfResults_ScenRel, id=c("tot_vol","Scen","d","p","Freq"))

#################
#Scen1 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen1
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc1', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))


# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,500))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen1 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen1_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen1 and V2
#################
# Filtering the data frame for only V2 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen1
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc1', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))


# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,500))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen1 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen1_v2.png", width =2000, height = 1000,res=150)
dev.off()



#################
#Scen1 and V3
#################
# Filtering the data frame for only V3 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen1
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc1', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))


# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,500))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen1 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen1_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen1 and V4
#################
# Filtering the data frame for only V4 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen1
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc1', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen1 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen1_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen1 and V5
#################
# Filtering the data frame for only V5 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen1
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc1', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen1 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen1_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen2 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen2
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc2', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen2 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen2_v1.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen2 and V2
#################
# Filtering the data frame for only V2 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen2
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc2', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen2 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen2_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen2 and V3
#################
# Filtering the data frame for only V3 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen2
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc2', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen2 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen2_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen2 and V4
#################
# Filtering the data frame for only V4 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen2
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc2', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen2 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen2_v4.png", width =2000, height = 1000,res=150)
dev.off()
#################
#Scen2 and V5
#################
# Filtering the data frame for only V5 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen2
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc2', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen2 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen2_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen3 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen3
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc3', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen3 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen3_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen3 and V2
#################
# Filtering the data frame for only V2 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen3
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc3', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))


# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen3 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen3_v2.png", width =2000, height = 1000,res=150)
dev.off() 

#################
#Scen3 and V3
#################
# Filtering the data frame for only V3 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen3
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc3', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))



# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen3 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen3_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen3 and V4
#################
# Filtering the data frame for only V4 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen3
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc3', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen3 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen3_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen3 and V5
#################
# Filtering the data frame for only V5 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen3
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc3', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen3 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen3_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen4 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen4
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc4', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen4 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen4_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen4 and V2
#################
# Filtering the data frame for only V2 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen4
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc4', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen4 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen4_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen4 and V3
#################
# Filtering the data frame for only V3 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen4
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc4', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen4 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen4_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen4 and V4
#################
# Filtering the data frame for only V4 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen4
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc4', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen4 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen4_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen4 and V5
#################
# Filtering the data frame for only V5 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen4
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc4', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen4 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen4_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen5 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen5
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc5', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen5 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen5_v1.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen5 and V2
#################
# Filtering the data frame for only V2 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen5
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc5', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen5 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen5_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen5 and V3
#################
# Filtering the data frame for only V3 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen5
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc5', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen5 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen5_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen5 and V4
#################
# Filtering the data frame for only V4 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen5
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc5', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen5 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen5_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen5 and V5
#################
# Filtering the data frame for only V5 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen5
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc5', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen5 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen5_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen6 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen6
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc6', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen5 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen6_v1.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen6 and V2
#################
# Filtering the data frame for only V2 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen6
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc6', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen5 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen6_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen6 and V3
#################
# Filtering the data frame for only V3 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen6
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc6', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen5 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen6_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen6 and V4
#################
# Filtering the data frame for only V4 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen6
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc6', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen5 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen6_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen6 and V5
#################
# Filtering the data frame for only V5 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen6
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc6', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen5 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen6_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen7 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen7
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc7', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen7 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen7_v1.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen7 and V2
#################
# Filtering the data frame for only V2 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen7
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc7', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen7 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen7_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen7 and V3
#################
# Filtering the data frame for only V3 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen7
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc7', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen7 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen7_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen7 and V4
#################
# Filtering the data frame for only V4 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen7
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc7', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen7 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen7_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen7 and V5
#################
# Filtering the data frame for only V5 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen7
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc7', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen7 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen7_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen8 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen8
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc8', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen8 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen8_v1.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen8 and V2
#################
# Filtering the data frame for only V2 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen8
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc8', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen8 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen8_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen8 and V3
#################
# Filtering the data frame for only V3 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen8
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc8', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen8 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen8_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen8 and V4
#################
# Filtering the data frame for only V4 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen8
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc8', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen8 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen8_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen8 and V5
#################
# Filtering the data frame for only V5 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen8
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc8', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen8 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen8_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen9 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen9
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc9', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen9 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen9_v1.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen9 and V2
#################
# Filtering the data frame for only V2 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen9
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc9', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen9 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen9_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen9 and V3
#################
# Filtering the data frame for only V3 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen9
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc9', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen9 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen9_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen9 and V4
#################
# Filtering the data frame for only V4 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen9
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc9', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen9 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen9_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen9 and V5
#################
# Filtering the data frame for only V5 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen9
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc9', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen9 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen9_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen10 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen10
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc10', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen10 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen10_v1.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen10 and V2
#################
# Filtering the data frame for only V2 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen10
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc10', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen10 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen10_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen10 and V3
#################
# Filtering the data frame for only V3 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen10
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc10', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen10 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen10_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen10 and V4
#################
# Filtering the data frame for only V4 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen10
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc10', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen10 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen10_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen10 and V5
#################
# Filtering the data frame for only V5 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen10
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc10', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen10 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen10_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen11 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen11
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc11', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen11 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen11_v1.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen11 and V2
#################
# Filtering the data frame for only V2 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen11
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc11', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen11 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen11_v2.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen11 and V3
#################
# Filtering the data frame for only V3 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen11
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc11', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen11 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen11_v3.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen11 and V4
#################
# Filtering the data frame for only V4 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen11
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc11', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen11 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen11_v4.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen11 and V5
#################
# Filtering the data frame for only V5 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen11
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc11', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen11 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen11_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen12 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen12
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc12', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen12 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen12_v1.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen12 and V2
#################
# Filtering the data frame for only V2 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen12
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc12', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen12 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen12_v2.png", width =2000, height = 1000,res=150)
dev.off() 

#################
#Scen12 and V3
#################
# Filtering the data frame for only V3 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen12
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc12', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen12 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen12_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen12 and V4
#################
# Filtering the data frame for only V4 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen12
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc12', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen12 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen12_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen12 and V5
#################
# Filtering the data frame for only V5 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen12
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc12', ]
# Reording the dataframe with respect to Days squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Rel2$new <- paste(Scen_Rel2$d,Scen_Rel2$p)
#Turn your 'new' column into a character vector
Scen_Rel2$new <- as.character(Scen_Rel2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Rel2$new <- factor(Scen_Rel2$new, levels=unique(Scen_Rel2$new))

# Plotting using ggplot 2
ggplot(data = Scen_Rel2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Release (cfs)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust =-0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(3000,26000,1000))+ 
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.7))+
  ggtitle("Releases under Scen12 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen12_v5.png", width =2000, height = 1000,res=150)
dev.off()


#######################################################
############Hydropower Generation (MWH) Extreme Values
######################################################
library(ggplot2)
library(gridExtra)
library(magrittr)
library(dplyr)
library(grid)

#Plotting Release for Extreme points for BugIndex (Lowest Extreme Value)and Hydro Power Objective (Highest End) with total Montly Releases.
#filtering the data frame with respect to Objective and Volume Senario

#melting both cases (model with all Eqaution and Without Simulation eq) into one column
Ext_EnergyMelt <- melt(dfResults_ExtEnergy, id=c("f2","d","tot_vol","p","Freq"))
observed_TotMonthly_MW <- 318194

#################
#BugIndex and V1
#################
# Filtering the data frame for only V1 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only BugIndex
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)


#adding Statistics to the plot
Stats <- as.data.frame(c(Tot_Monthlysim_MW = Totmonthly_Power_alleqs,Tot_Monthlynosim_MW = Totmonthly_Power_nosim,Observed_MW =observed_TotMonthly_MW ))
Stats$Total<-c('Tot_Monthlysim_MW','Tot_Monthlynosim_MW','Observed_MW')
names(Stats)[1]<- "Value (MW)"
Statistics <-as.data.frame(t(Stats))
Statistics <-Statistics [-c(2),]

# Plotting using ggplot 2
ggplot(data =Ext_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=1.2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
    scale_y_continuous(breaks = seq(1000,10000,200))+
  theme(legend.text=element_text(size=10,face="bold"),legend.title = element_text(size=10),legend.justification=c(1,0),legend.position=c(0.4,0.01))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  ggtitle("Energy under Extreme BugIndex value for 0.5 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) +
  annotate("text",x=36,y=3000, label=Statistics$Tot_Monthlysim_MW,colour = "red",size=5)+                        
  annotate("text",x=36,y=3100, label="TotMonthly_simulatedPower(MW)",size=5)+
  annotate("text",x=52,y=3000, label=Statistics$Observed_MW,colour = "red",size=5)+                        
  annotate("text",x=52,y=3100, label="TotMonthly_ObservedPower(MW)",size=5)

#Saving the Png of the plot
dev.copy(png,"Extreme_bugindex_Energy_V1.png", width =2000, height = 1000,res=150)
dev.off() 

#################
#BugIndex and V2
#################
# Filtering the data frame for only V2 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only BugIndex
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))


# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)


#adding Statistics to the plot
Stats <- as.data.frame(c(Tot_Monthlysim_MW = Totmonthly_Power_alleqs,Tot_Monthlynosim_MW = Totmonthly_Power_nosim,Observed_MW =observed_TotMonthly_MW ))
Stats$Total<-c('Tot_Monthlysim_MW','Tot_Monthlynosim_MW','Observed_MW')
names(Stats)[1]<- "Value (MW)"
Statistics <-as.data.frame(t(Stats))
Statistics <-Statistics [-c(2),]

# Plotting using ggplot 2
ggplot(data = Ext_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=1.2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,10000,400))+
  theme(legend.text=element_text(size=10,face="bold"),legend.title = element_text(size=10),legend.justification=c(1,0),legend.position=c(0.43,0.01))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  ggtitle("Energy under Extreme BugIndex value for 0.6 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))+
  annotate("text",x=36,y=3120, label=Statistics$Tot_Monthlysim_MW,colour = "red",size=5)+                        
  annotate("text",x=36,y=3000, label="TotMonthly_simulatedPower(MW)",size=5)+
  annotate("text",x=52,y=3120, label=Statistics$Observed_MW,colour = "red",size=5)+                        
  annotate("text",x=52,y=3000, label="TotMonthly_ObservedPower(MW)",size=5)

dev.copy(png,"Extreme_bugindex_Energy_V2.png", width =2000, height = 1000,res=150)
dev.off() 

#################
#BugIndex and V3
#################
# Filtering the data frame for only V3 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only BugIndex
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)


#adding Statistics to the plot
Stats <- as.data.frame(c(Tot_Monthlysim_MW = Totmonthly_Power_alleqs,Tot_Monthlynosim_MW = Totmonthly_Power_nosim,Observed_MW =observed_TotMonthly_MW ))
Stats$Total<-c('Tot_Monthlysim_MW','Tot_Monthlynosim_MW','Observed_MW')
names(Stats)[1]<- "Value (MW)"
Statistics <-as.data.frame(t(Stats))
Statistics <-Statistics [-c(2),]

# Plotting using ggplot 2
ggplot(data = Ext_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=1.2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,10000,500))+
  theme(legend.text=element_text(size=10,face="bold"),legend.title = element_text(size=10),legend.justification=c(1,0),legend.position=c(0.42,0.01))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  ggtitle("Energy under Extreme BugIndex value for 0.7 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))+
  annotate("text",x=36,y=3200, label=Statistics$Tot_Monthlysim_MW,colour = "red",size=5)+                        
  annotate("text",x=36,y=3000, label="TotMonthly_simulatedPower(MW)",size=5)+
  annotate("text",x=52,y=3200, label=Statistics$Observed_MW,colour = "red",size=5)+                        
  annotate("text",x=52,y=3000, label="TotMonthly_ObservedPower(MW)",size=5)

dev.copy(png,"Extreme_bugindex_Energy_V3.png", width =2000, height = 1000,res=150)
dev.off() 


#################
#BugIndex and V4
#################
# Filtering the data frame for only V4 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only BugIndex
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))


# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)


#adding Statistics to the plot
Stats <- as.data.frame(c(Tot_Monthlysim_MW = Totmonthly_Power_alleqs,Tot_Monthlynosim_MW = Totmonthly_Power_nosim,Observed_MW =observed_TotMonthly_MW ))
Stats$Total<-c('Tot_Monthlysim_MW','Tot_Monthlynosim_MW','Observed_MW')
names(Stats)[1]<- "Value (MW)"
Statistics <-as.data.frame(t(Stats))
Statistics <-Statistics [-c(2),]

# Plotting using ggplot 2
ggplot(data = Ext_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=1.2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,10000,500))+
  theme(legend.text=element_text(size=10,face="bold"),legend.title = element_text(size=10),legend.justification=c(1,0),legend.position=c(0.42,0.01))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  ggtitle("Energy under Extreme BugIndex value for 0.8 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))+
  annotate("text",x=36,y=3200, label=Statistics$Tot_Monthlysim_MW,colour = "red",size=5)+                        
  annotate("text",x=36,y=3000, label="TotMonthly_simulatedPower(MW)",size=5)+
  annotate("text",x=52,y=3200, label=Statistics$Observed_MW,colour = "red",size=5)+                        
  annotate("text",x=52,y=3000, label="TotMonthly_ObservedPower(MW)",size=5)
  
dev.copy(png,"Extreme_bugindex_Energy_V4.png", width =2000, height = 1000,res=150)
dev.off() 


#################
#BugIndex and V5
#################
# Filtering the data frame for only V5 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only BugIndex
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)


#adding Statistics to the plot
Stats <- as.data.frame(c(Tot_Monthlysim_MW = Totmonthly_Power_alleqs,Tot_Monthlynosim_MW = Totmonthly_Power_nosim,Observed_MW =observed_TotMonthly_MW ))
Stats$Total<-c('Tot_Monthlysim_MW','Tot_Monthlynosim_MW','Observed_MW')
names(Stats)[1]<- "Value (MW)"
Statistics <-as.data.frame(t(Stats))
Statistics <-Statistics [-c(2),]

# Plotting using ggplot 2
ggplot(data = Ext_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=1.2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,10000,500))+
  theme(legend.text=element_text(size=10,face="bold"),legend.title = element_text(size=10),legend.justification=c(1,0),legend.position=c(0.42,0.01))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  ggtitle("Energy under Extreme BugIndex value for 0.9 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))+
  annotate("text",x=36,y=3300, label=Statistics$Tot_Monthlysim_MW,colour = "red",size=5)+                        
  annotate("text",x=36,y=3000, label="TotMonthly_simulatedPower(MW)",size=5)+
  annotate("text",x=52,y=3300, label=Statistics$Observed_MW,colour = "red",size=5)+                        
  annotate("text",x=52,y=3000, label="TotMonthly_ObservedPower(MW)",size=5)

dev.copy(png,"Extreme_bugindex_Energy_V5.png", width =2000, height = 1000,res=150)
dev.off() 

#################
#Hydropower and V1
#################
# Filtering the data frame for only V1 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Hydro
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='Hydro', ]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))


# Plotting using ggplot 2
ggplot(data = Ext_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=1.2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.2,0.6))+
  ggtitle("Energy under Extreme Hydro value for 0.5 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Extreme_Hydro_Energy_V1.png", width =2000, height = 1000,res=150)
dev.off() 



#################
#Hydropower and V2
#################
# Filtering the data frame for only V2 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Hydro
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='Hydro', ]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))



# Plotting using ggplot 2

ggplot(data = Ext_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=1.2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.7,0.6))+
  ggtitle("Energy under Extreme Hydro value for 0.6 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Extreme_Hydro_Energy_V2.png", width =2000, height = 1000,res=150)
dev.off() 



#################
#Hydropower and V3
#################
# Filtering the data frame for only V3 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Hydro
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='Hydro', ]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))


# Plotting using ggplot 2

ggplot(data = Ext_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=1.2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.6))+
  ggtitle("Energy under Extreme Hydro value for 0.7 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Extreme_Hydro_Energy_V3.png", width =2000, height = 1000,res=150)
dev.off() 



#################
#Hydropower and V4
#################
# Filtering the data frame for only V4 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Hydro
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='Hydro', ]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))


# Plotting using ggplot 2

ggplot(data = Ext_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=1.2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.6))+
  ggtitle("Energy under Extreme Hydro value for 0.8 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Extreme_Hydro_Energy_V4.png", width =2000, height = 1000,res=150)
dev.off() 


#################
#Hydropower and V5
#################
# Filtering the data frame for only V5 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Hydro
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='Hydro', ]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))



# Plotting using ggplot 2

ggplot(data = Ext_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=1.2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+
  theme(legend.text=element_text(size=10,face="bold"),legend.justification=c(1,0),legend.position=c(0.15,0.6))+
  ggtitle("Energy under Extreme Hydro value for 0.9 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Extreme_Hydro_Energy_V5.png", width =2000, height = 1000,res=150)
dev.off() 




#################################################
############Hydropower Generation (MW) Scenarios
#################################################

#Plotting Release for Scenarios of  BugIndex  with various total Montly Release senarios.
#filtering the data frame with respect to Objective and Volume Senario

#melting both cases (model with all Eqaution and Without Simulation eq) into one column
Scen_EnergyMelt <- melt(dfResults_ScenEnergy, id=c("tot_vol", "Scen","d","p","Freq"))

#################
#Scen1 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen1
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc1', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,300))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen1 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen1_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen1 and V2
#################

# Filtering the data frame for only V2 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen1
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc1', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen1 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen1_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen1 and V3
#################

# Filtering the data frame for only V3 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen1
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc1', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen1 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen1_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen1 and V4
#################

# Filtering the data frame for only V4 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen1
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc1', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2
ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen1 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen1_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen1 and V5
#################

# Filtering the data frame for only V5 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen1
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc1', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2
ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen1 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen1_v5.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen2 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen2
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc2', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen2 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen2_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen2 and V2
#################

# Filtering the data frame for only V2 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen2
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc2', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen2 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen2_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen2 and V3
#################

# Filtering the data frame for only V3 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen2
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc2', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen2 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen2_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen2 and V4
#################

# Filtering the data frame for only V4 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen2
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc2', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen2 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen2_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen2 and V5
#################

# Filtering the data frame for only V5 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen2
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc2', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2
ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen2 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen2_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen3 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen3
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc3', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen3 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen3_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen3 and V2
#################

# Filtering the data frame for only V2 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen3
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc3', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen3 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen3_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen3 and V3
#################

# Filtering the data frame for only V3 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen3
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc3', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen3 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen3_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen3 and V4
#################

# Filtering the data frame for only V4 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen3
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc3', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen3 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen3_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen3 and V5
#################

# Filtering the data frame for only V5 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen3
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc3', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2
ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen3 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen3_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen4 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen4
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc4', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen4 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen4_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen4 and V2
#################

# Filtering the data frame for only V2 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen4
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc4', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen4 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen4_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen4 and V3
#################

# Filtering the data frame for only V3 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen4
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc4', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen4 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen4_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen4 and V4
#################

# Filtering the data frame for only V4 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen4
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc4', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen4 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen4_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen4 and V5
#################

# Filtering the data frame for only V5 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen4
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc4', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2
ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen4 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen4_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen5 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen5
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc5', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen5 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen5_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen5 and V2
#################

# Filtering the data frame for only V2 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen5
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc5', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen5 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen5_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen5 and V3
#################

# Filtering the data frame for only V3 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen5
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc5', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen5 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen5_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen5 and V4
#################

# Filtering the data frame for only V4 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen5
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc5', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen5 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen5_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen5 and V5
#################

# Filtering the data frame for only V5 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen5
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc5', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2
ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen5 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen5_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen6 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen6
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc6', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen6 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen6_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen6 and V2
#################

# Filtering the data frame for only V2 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen6
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc6', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen6 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen6_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen6 and V3
#################

# Filtering the data frame for only V3 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen6
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc6', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen6 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen6_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen6 and V4
#################

# Filtering the data frame for only V4 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen6
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc6', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen6 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen6_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen6 and V5
#################

# Filtering the data frame for only V5 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen6
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc6', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2
ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen6 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen6_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen7 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen7
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc7', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen7 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen7_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen7 and V2
#################

# Filtering the data frame for only V2 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen7
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc7', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen7 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen7_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen7 and V3
#################

# Filtering the data frame for only V3 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen7
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc7', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen7 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen7_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen7 and V4
#################

# Filtering the data frame for only V4 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen7
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc7', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen7 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen7_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen7 and V5
#################

# Filtering the data frame for only V5 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen7
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc7', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2
ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen7 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen7_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen8 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen8
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc8', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen8 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen8_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen8 and V2
#################

# Filtering the data frame for only V2 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen8
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc8', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen8 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen8_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen8 and V3
#################

# Filtering the data frame for only V3 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen8
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc8', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen8 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen8_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen8 and V4
#################

# Filtering the data frame for only V4 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen8
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc8', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen8 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen8_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen8 and V5
#################

# Filtering the data frame for only V5 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen8
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc8', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2
ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen8 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen8_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen9 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen9
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc9', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen9 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen9_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen9 and V2
#################

# Filtering the data frame for only V2 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen9
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc9', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen9 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen9_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen9 and V3
#################

# Filtering the data frame for only V3 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen9
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc9', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen9 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen9_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen9 and V4
#################

# Filtering the data frame for only V4 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen9
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc9', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen9 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen9_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen9 and V5
#################

# Filtering the data frame for only V5 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen9
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc9', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2
ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen9 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen9_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen10 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen10
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc10', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen10 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen10_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen10 and V2
#################

# Filtering the data frame for only V2 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen10
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc10', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen10 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen10_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen10 and V3
#################

# Filtering the data frame for only V3 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen10
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc10', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen10 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen10_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen10 and V4
#################

# Filtering the data frame for only V4 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen10
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc10', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen10 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen10_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen10 and V5
#################

# Filtering the data frame for only V5 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen10
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc10', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2
ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen10 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen10_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen11 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen11
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc11', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen11 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen11_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen11 and V2
#################

# Filtering the data frame for only V2 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen11
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc11', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen11 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen11_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen11 and V3
#################

# Filtering the data frame for only V3 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen11
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc11',]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen11 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen11_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen11 and V4
#################

# Filtering the data frame for only V4 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen11
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc11', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen11 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen11_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen11 and V5
#################

# Filtering the data frame for only V5 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen11
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc11', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2
ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen11 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen11_v5.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen12 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen12
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc12', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen12 with 0.5MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen12_v1.png", width =2000, height = 1000,res=150)
dev.off()


#################
#Scen12 and V2
#################

# Filtering the data frame for only V2 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen12
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc12', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen12 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen12_v2.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen12 and V3
#################

# Filtering the data frame for only V3 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only Scen12
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc12', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen12 with 0.7MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen12_v3.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen12 and V4
#################

# Filtering the data frame for only V4 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only Scen12
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc12', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2

ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen12 with 0.8MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen12_v4.png", width =2000, height = 1000,res=150)
dev.off()

#################
#Scen12 and V5
#################

# Filtering the data frame for only V5 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only Scen12
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc12', ]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_alleqs case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_alleqs',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_nosim case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_nosim',]
Totmonthly_Power_nosim<-sum(filtter_nosim$value)

# Plotting using ggplot 2
ggplot(data = Scen_Eng2, aes(x = new, y = value, group = variable, color = variable, shape = variable)) + 
  geom_step(aes(linetype = variable), size=1.2) + 
  geom_point(fill = "white", size=2) + 
  scale_shape_manual(values = c(22, 21)) + 
  xlab("Daily Timesteps") +ylab("Power Generated in Timestep (MW)")+
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=90, hjust =-0.3),
        axis.text.y = element_text( color="black", size=12, face="bold"),
        axis.title.x = element_text(color="blue", size=16, face="bold", vjust = -0.3),
        axis.title.y = element_text(color="blue", size=16, face="bold", vjust = 2))+ 
  scale_y_continuous(breaks = seq(1000,15000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen12 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen12_v5.png", width =2000, height = 1000,res=150)
dev.off()

