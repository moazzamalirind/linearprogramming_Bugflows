#Load and plot Bug Flow Model results
#
# Moazzam Ali Rind
# created: July 4, 2019
# updated: August 17 , 2019
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
setwd("E:\\Project_Bugflow\\Montly_Models\\April_2018\\Results_new")


# Load igdx directory from your computer 
igdx("C:\\GAMS\\win64\\24.2");

igdx();

###############################################
## Read in Deterministic Model Results
###############################################

gdxfile <- "Apri1l8_2periods_0to8.gdx"

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
dfExtEnergy_Bugflow<- as.data.frame.table(Extpoint_EnergyGen_alleqs)
dfExtEnergy_optimization<- as.data.frame.table(Extpoint_EnergyGen_nosim)
dfEnergy_Bugflow<- as.data.frame.table(EnergyGen_alleqs)
dfEnergy_optimization<- as.data.frame.table(EnergyGen_nosim)
dfExtstor_alleqs<- as.data.frame.table(Extpoint_storage_alleqs)
dfExtstor_nosim<- as.data.frame.table(Extpoint_storage_nosim)
dfstor_alleqs<- as.data.frame.table(Storage_alleqs)
dfstor_nosim<- as.data.frame.table(Storage_nosim)

# Make Variable for Each Desired Item
dfobj_alleqs$Bugflow <- dfobj_alleqs$Freq
dfobj_nosim$optimization <- dfobj_nosim$Freq
dfExtRel_alleqs$ExtRel_Bugflow<-dfExtRel_alleqs$Freq
dfRel_alleqs$Rel_Bugflow <- dfRel_alleqs$Freq
dfExtRel_nosim$ExtRel_optimization <- dfExtRel_nosim$Freq 
dfRel_nosim$Rel_optimization <- dfRel_nosim$Freq
dfExtEnergy_Bugflow$ExtEnergy_Bugflow <- dfExtEnergy_Bugflow$Freq 
dfEnergy_Bugflow$Energy_Bugflow <- dfEnergy_Bugflow$Freq
dfExtEnergy_optimization$ExtEnergy_optimization <-dfExtEnergy_optimization$Freq 
dfEnergy_optimization$Energy_optimization <- dfEnergy_optimization$Freq
dfExtstor_alleqs$Extstor_Bugflow <- dfExtstor_alleqs$Freq 
dfstor_alleqs$stor_Bugflow <- dfstor_alleqs$Freq
dfExtstor_nosim$Extstor_optimization <- dfExtstor_nosim$Freq 
dfstor_nosim$stor_optimization <- dfstor_nosim$Freq

#Merge the related dataframes into one

#Objectives With senarios
dfResults_obj <- merge(dfobj_alleqs, dfobj_nosim[,c("f","All_cases","tot_vol","optimization")],by=c("f","All_cases","tot_vol"), all.x=TRUE, sort=TRUE)

#Release Extreme points 
dfResults_ExtRel <- merge(dfExtRel_alleqs,dfExtRel_nosim[,c("f2","d","tot_vol","p","ExtRel_optimization")],by=c("f2","d","tot_vol","p"), all.x=TRUE, sort=TRUE)

#Release With senarios
dfResults_ScenRel <- merge(dfRel_alleqs,dfRel_nosim[,c("tot_vol","Scen","d","p","Rel_optimization")],by=c("tot_vol","Scen","d","p"), all.x=TRUE, sort=TRUE)

#Energy Extreme points
dfResults_ExtEnergy <- merge(dfExtEnergy_Bugflow,dfExtEnergy_optimization[,c("f2","d","tot_vol","p","ExtEnergy_optimization")],by=c("f2","d","tot_vol","p"), all.x=TRUE, sort=TRUE)

#Energy With senarios
dfResults_ScenEnergy <- merge(dfEnergy_Bugflow,dfEnergy_optimization[,c("tot_vol","Scen","d","p","Energy_optimization")],by=c("tot_vol","Scen","d","p"), all.x=TRUE, sort=TRUE)

#Storage Extreme points
dfResults_Extstorage <- merge(dfExtstor_alleqs,dfExtstor_nosim[,c("f2","d","tot_vol","Extstor_optimization")],by=c("f2","d","tot_vol"), all.x=TRUE, sort=TRUE)

#storage With senarios
dfResults_Scenstorage <- merge(dfstor_alleqs,dfstor_nosim[,c("tot_vol","Scen","d","stor_optimization")],by=c("tot_vol","Scen","d"), all.x=TRUE, sort=TRUE)


#############################################
#Rearranging and Plotting Section......
#############################################
library(ggplot2) 
library(reshape2)
library(dplyr) 
library(plyr) 
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
#Obj2$All_cases<- NULL
Obj2$f <- NULL


#############################################
#Reading the validation results 
#############################################
gdxfile2 <- "Validation_April2018_0to8.gdx"
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
O2<- data.frame(tot_vol="Observed",Hydro_Obj= as.numeric(as.character(Obj$Hydro)), variable="Observed",All_cases="validation",value=as.numeric(as.character(Obj$BugIndex)))

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

#converting the observed flow number to the nearest decimal
Obj2$Tot_Vol_AF<- replace(Obj2$Tot_Vol_AF,Obj2$Tot_Vol_AF=='749449.05','7.49e+05')

#odering the data set.
Obj2<-Obj2[with(Obj2,order(value)),]  
Obj2<-Obj2[with(Obj2,order(tot_vol)),] 
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
  theme(plot.title = element_text(size=30,hjust =0.5, vjust = 2,face="bold"),legend.justification=c(1,0),legend.position=c(0.998, 0.01))
dev.copy(png,"Pareto-optimal.png", width =5000, height =2500,res=200)
dev.off() 


#############################################
#Reading the 3BugFlow day results 
#############################################
gdxfile3 <- "Apri1l8_3bugflowdays.gdx"

#  Objective functions with bugflow incorporated in run
Objective_3bugflow <- rgdx(gdxfile3, list(name="obj_save", form = "full"))$val
dfObjective_3bugflow <- as.data.frame.table(Objective_3bugflow)

#adding new columns 
dfObjective_3bugflow$Bugflow_3days <- dfObjective_3bugflow$Freq

#odering both data sets dfobjectives_3bugflow and dfResults_obj
dfObjective_3bugflow<- dfObjective_3bugflow[with(dfObjective_3bugflow,order(tot_vol)),]
dfObjective_3bugflow<- dfObjective_3bugflow[with(dfObjective_3bugflow,order(All_cases)),]
dfResults_obj<- dfResults_obj[with(dfResults_obj,order(tot_vol)),]
dfResults_obj<- dfResults_obj[with(dfResults_obj,order(All_cases)),]

#creating a copy of dfresults_obj
obj_3bugflow <-dfResults_obj

#now adding Bugflow_3days column to data frame obj_3bugflow
obj_3bugflow$Bugflow_3days <- dfObjective_3bugflow$Bugflow_3days

#now melt the obj_3bugflow
obj_3bugflow_melt <- melt(obj_3bugflow, id=c("f","tot_vol","All_cases","Freq"))

#Creating New Column for linkage with total volume data frame.
obj_3bugflow_melt$Tot_Vol_AF<-obj_3bugflow_melt$tot_vol

#Replacing values of current data frame with values of total monthly volume used in Gams model
obj_3bugflow_melt$Tot_Vol_AF<- ifelse(obj_3bugflow_melt$tot_vol== "V1",dfmonthvol[1,]$Freq ,
                            ifelse(obj_3bugflow_melt$tot_vol== "V2",dfmonthvol[2,]$Freq ,
                            ifelse(obj_3bugflow_melt$tot_vol== "V3",dfmonthvol[3,]$Freq ,
                            ifelse(obj_3bugflow_melt$tot_vol== "V4",dfmonthvol[4,]$Freq ,
                            ifelse(obj_3bugflow_melt$tot_vol== "V5",dfmonthvol[5,]$Freq, "Observed")))))

#converting the observed flow number to the nearest decimal
obj_3bugflow_melt$Tot_Vol_AF<- replace(obj_3bugflow_melt$Tot_Vol_AF,obj_3bugflow_melt$Tot_Vol_AF=='749449.05','7.49e+05')


#filtering the data frame with respect to Objective
# Filtering the data frame for only BugIndex Objective
Obj_3Bug<- obj_3bugflow_melt[obj_3bugflow_melt[, "f"]=='BugIndex', ]
Obj_3Bug<- Obj_3Bug[with(Obj_3Bug,order(tot_vol)),]

# Filtering the data frame for only Hydro Objective
Obj_3Hydro<-obj_3bugflow_melt[obj_3bugflow_melt[, "f"]=='Hydro', ]
Obj_3Hydro<-Obj_3Hydro[with(Obj_3Hydro,order(tot_vol)),]

#renaming columns 
names(Obj_3Hydro)[6]<-"Hydro"
names(Obj_3Bug)[6]<-"BugIndex"

#Merging both filtered data frame in one
Obj_3Bug$Hydro<-Obj_3Hydro$Hydro

#Removing Freq Column bcz its extra and confusing here.
Obj_3Bug$Freq<- NULL
Obj_3Bug$f<- NULL
Obj_3Bug$tot_vol<- NULL

#Adding the observed value point
#reordering the data frame O2 having observed data objective
O2<-O2[,c(4,3,5,1,2)]

#renaming the columns
names(O2)[3]<-"BugIndex"
names(O2)[4]<-"Tot_Vol_AF"
names(O2)[5]<-"Hydro"

#Binding Data frames
Obj3<- rbind(Obj_3Bug,O2)

#Renaming some value in column variable
Obj3$variable <- as.character(Obj3$variable)
Obj3$variable[Obj3$variable=="Bugflow"] <- "Bugflow_2days"

#####################################################
# Plotting Objective Values (Pareto-optimal Curve with 3days Bugflow curve too)
#####################################################
ggplot(Obj3, aes(Hydro,BugIndex,color=Tot_Vol_AF, shape=variable,linetype = variable))+
  geom_point(stat = "identity", size=5)+
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
  theme(plot.title = element_text(size=30,hjust =0.5, vjust = 2,face="bold"),legend.justification=c(1,0),legend.position=c(0.998, 0.01))

dev.copy(png,"3bugflowday.png", width =5000, height =2500,res=200)
dev.off() 


#plot without marker

ggplot(Obj3, aes(Hydro,BugIndex,color=Tot_Vol_AF, shape=variable,linetype = variable))+
  geom_line(size=2)+
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
  theme(plot.title = element_text(size=30,hjust =0.5, vjust = 2,face="bold"),legend.justification=c(1,0),legend.position=c(0.998, 0.01))

dev.copy(png,"3bugflowday_withoutmarks.png", width =5000, height =2500,res=200)
dev.off() 



#############################################
#Reading the 4BugFlow day results 
#############################################
gdxfile4 <- "Apri1l8_4bugflowdays.gdx"

#  Objective functions with bugflow incorporated in run
Objective_4bugflow <- rgdx(gdxfile4, list(name="obj_save", form = "full"))$val
dfObjective_4bugflow <- as.data.frame.table(Objective_4bugflow)

#adding new columns 
dfObjective_4bugflow$Bugflow_4days <- dfObjective_4bugflow$Freq

#odering both data sets dfobjectives_4bugflow with respect to obj_3bugflow
dfObjective_4bugflow<- dfObjective_4bugflow[with(dfObjective_4bugflow,order(tot_vol)),]
dfObjective_4bugflow<- dfObjective_4bugflow[with(dfObjective_4bugflow,order(All_cases)),]

#creating a copy of dfresults_obj
obj_4bugflow <-obj_3bugflow

#now adding Bugflow_3days column to data frame obj_3bugflow
obj_4bugflow$Bugflow_4days <- dfObjective_4bugflow$Bugflow_4days

#now melt the obj_3bugflow
obj_4bugflow_melt <- melt(obj_4bugflow, id=c("f","tot_vol","All_cases","Freq"))

#Creating New Column for linkage with total volume data frame.
obj_4bugflow_melt$Tot_Vol_AF<-obj_4bugflow_melt$tot_vol

#Replacing values of current data frame with values of total monthly volume used in Gams model
obj_4bugflow_melt$Tot_Vol_AF<- ifelse(obj_4bugflow_melt$tot_vol== "V1",dfmonthvol[1,]$Freq ,
                                      ifelse(obj_4bugflow_melt$tot_vol== "V2",dfmonthvol[2,]$Freq ,
                                             ifelse(obj_4bugflow_melt$tot_vol== "V3",dfmonthvol[3,]$Freq ,
                                                    ifelse(obj_4bugflow_melt$tot_vol== "V4",dfmonthvol[4,]$Freq ,
                                                           ifelse(obj_4bugflow_melt$tot_vol== "V5",dfmonthvol[5,]$Freq, "Observed")))))

#converting the observed flow number to the nearest decimal
obj_4bugflow_melt$Tot_Vol_AF<- replace(obj_4bugflow_melt$Tot_Vol_AF,obj_4bugflow_melt$Tot_Vol_AF=='749449.05','7.49e+05')


#filtering the data frame with respect to Objective
# Filtering the data frame for only BugIndex Objective
Obj_4Bug<- obj_4bugflow_melt[obj_4bugflow_melt[, "f"]=='BugIndex', ]
Obj_4Bug<- Obj_4Bug[with(Obj_4Bug,order(tot_vol)),]

# Filtering the data frame for only Hydro Objective
Obj_4Hydro<-obj_4bugflow_melt[obj_4bugflow_melt[, "f"]=='Hydro', ]
Obj_4Hydro<-Obj_4Hydro[with(Obj_4Hydro,order(tot_vol)),]

#renaming columns 
names(Obj_4Hydro)[6]<-"Hydro"
names(Obj_4Bug)[6]<-"BugIndex"

#Merging both filtered data frame in one
Obj_4Bug$Hydro<-Obj_4Hydro$Hydro

#Removing Freq Column bcz its extra and confusing here.
Obj_4Bug$Freq<- NULL
Obj_4Bug$f<- NULL
Obj_4Bug$tot_vol<- NULL


#Binding Data frames
Obj4<- rbind(Obj_4Bug,O2)

#Renaming some value in column variable
Obj4$variable <- as.character(Obj4$variable)
Obj4$variable[Obj4$variable=="Bugflow"] <- "Bugflow_2days"

#####################################################
# Plotting Objective Values (Pareto-optimal Curve with 4days Bugflow curve too)
#####################################################
ggplot(Obj4, aes(Hydro,BugIndex,color=Tot_Vol_AF, shape=variable,linetype = variable))+
  geom_point(stat = "identity", size=5)+
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
  theme(plot.title = element_text(size=30,hjust =0.5, vjust = 2,face="bold"),legend.justification=c(1,0),legend.position=c(0.998, 0.01))

dev.copy(png,"4bugflowday.png", width =5000, height =2500,res=200)
dev.off() 


#plot without marker

ggplot(Obj4, aes(Hydro,BugIndex,color=Tot_Vol_AF, shape=variable,linetype = variable))+
  geom_line(size=2)+
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
  theme(plot.title = element_text(size=30,hjust =0.5, vjust = 2,face="bold"),legend.justification=c(1,0),legend.position=c(0.998, 0.01))

dev.copy(png,"4bugflowday_withoutmarks.png", width =5000, height =2500,res=200)
dev.off() 


#####################################################
# Plotting Release
#####################################################
#Plotting Release for Extreme points for BugIndex (Lowest Extreme Value)and Hydro Power Objective (Highest End) with total Montly Release of 0.8MAF.
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
  scale_y_continuous(breaks = seq(1000,12000,20))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Lowest Extreme BugIndex value for 0.6MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Extreme_bugindex_Release_V1.png", width =2000, height = 1000,res=150)
dev.off() 

#
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
  scale_y_continuous(breaks = seq(8000,16000,225))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Lowest Extreme BugIndex value for 0.7MAF senario")+
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
  scale_y_continuous(breaks = seq(8000,16000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Lowest Extreme BugIndex value for 0.749MAF senario")+
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
  scale_y_continuous(breaks = seq(8000,30000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
  scale_y_continuous(breaks = seq(8000,32000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
  ggtitle("Releases under Extreme Hydro value for 0.6MAF senario")+
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
  ggtitle("Releases under Extreme Hydro value for 0.7MAF senario")+
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
  ggtitle("Releases under Extreme Hydro value for 0.749MAF senario")+
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
  ggtitle("Releases under Extreme Hydro value for 0.9MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Extreme_Hydro_Release_V5.png", width =2000, height = 1000,res=150)
dev.off()

######################################################################################
#Plotting Release for Scenarios of  BugIndex  with various total Montly Release senarios.
#filtering the data frame with respect to Objective and Volume Senario

#melting both cases (model with all Eqaution and Without Simulation eq) into one column
Scen_RelMelt <- melt(dfResults_ScenRel, id=c("tot_vol","Scen","d","p","Freq"))

#Renaming some value in column variable
Scen_RelMelt$variable<- as.character(Scen_RelMelt$variable)
Scen_RelMelt$variable[Scen_RelMelt$variable=="Rel_Bugflow"] <- "Rel_Bugflow_2days"

#################
#Scen1 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen1
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc1', ]
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,100))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen1 with 0.6MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) 
dev.copy(png,"Release_Scen1_v1.png", width =2000, height = 1000,res=150)
dev.off()

#

#################
#Scen1 and V2
#################
# Filtering the data frame for only V2 Senario
Scen_Rel1<- Scen_RelMelt[Scen_RelMelt[, "tot_vol"]=='V2', ]
# Filtering the data frame for only Scen1
Scen_Rel2 <- Scen_Rel1[Scen_Rel1[, "Scen"]=='sc1', ]
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen1 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen1 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,100))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen2 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen2 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen2 with 0.749MAF Total Monthly Release")+
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen3 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen3 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen3 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen4 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen4 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen4 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen5 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen5 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen5 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen6 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen5 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen5 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen7 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen7 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen7 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen8 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen8 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen8 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen9 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen9 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen9 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,500))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen10 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen10 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen10 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen11 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen11 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen11 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen12 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen12 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Releases under Scen12 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to Periods squence.
Scen_Rel2 <- Scen_Rel2[with(Scen_Rel2,order(p)),]
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
  scale_y_continuous(breaks = seq(4000,36000,2000))+ 
  theme(legend.text=element_text(size=12,face="bold"))+
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

#Renaming some value in column variable
Ext_EnergyMelt$variable<- as.character(Ext_EnergyMelt$variable)
Ext_EnergyMelt$variable[Ext_EnergyMelt$variable=="ExtEnergy_Bugflow"] <- "ExtEnergy_Bugflow_2days"

#Observed Energy value
observed_TotMonthly_MW <- 318194

#################
#BugIndex and V1
#################
# Filtering the data frame for only V1 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only BugIndex
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to periods squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_Optimization',]
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
    scale_y_continuous(breaks = seq(1000,10000,500))+
  theme(legend.text=element_text(size=12,face="bold"),legend.title = element_text(size=10))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  ggtitle("Energy under Extreme BugIndex value for 0.6 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold")) +
  annotate("text",x=36,y=2650, label=Statistics$Tot_Monthlysim_MW,colour = "red",size=4)+                        
  annotate("text",x=36,y=2800, label="TotMonthly_simulated(MW)",size=4)+
  annotate("text",x=52,y=2650, label=Statistics$Observed_MW,colour = "red",size=4)+                        
  annotate("text",x=52,y=2800, label="TotMonthly_Observed(MW)",size=4)

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
# Reording the dataframe with respect to periods squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))


# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_Optimization',]
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
  theme(legend.text=element_text(size=12,face="bold"),legend.title = element_text(size=10))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  ggtitle("Energy under Extreme BugIndex value for 0.7 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))+
  annotate("text",x=36,y=2700, label=Statistics$Tot_Monthlysim_MW,colour = "red",size=4)+                        
  annotate("text",x=36,y=2870, label="TotMonthly_simulated(MW)",size=4)+
  annotate("text",x=52,y=2700, label=Statistics$Observed_MW,colour = "red",size=4)+                        
  annotate("text",x=52,y=2870, label="TotMonthly_Observed(MW)",size=4)

dev.copy(png,"Extreme_bugindex_Energy_V2.png", width =2000, height = 1000,res=150)
dev.off() 

#################
#BugIndex and V3
#################
# Filtering the data frame for only V3 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V3', ]
# Filtering the data frame for only BugIndex
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to periods squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_Optimization',]
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
  theme(legend.text=element_text(size=12,face="bold"),legend.title = element_text(size=10))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  ggtitle("Energy under Extreme BugIndex value for 0.749 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))+
  annotate("text",x=36,y=2600, label=Statistics$Tot_Monthlysim_MW,colour = "red",size=4)+                        
  annotate("text",x=36,y=2800, label="TotMonthly_simulated(MW)",size=4)+
  annotate("text",x=52,y=2600, label=Statistics$Observed_MW,colour = "red",size=4)+                        
  annotate("text",x=52,y=2800, label="TotMonthly_Observed(MW)",size=4)

dev.copy(png,"Extreme_bugindex_Energy_V3.png", width =2000, height = 1000,res=150)
dev.off() 


#################
#BugIndex and V4
#################
# Filtering the data frame for only V4 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V4', ]
# Filtering the data frame for only BugIndex
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to periods squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))


# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_Optimization',]
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
  theme(legend.text=element_text(size=12,face="bold"),legend.title = element_text(size=10))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  ggtitle("Energy under Extreme BugIndex value for 0.8 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))+
  annotate("text",x=36,y=2550, label=Statistics$Tot_Monthlysim_MW,colour = "red",size=4)+                        
  annotate("text",x=36,y=2800, label="TotMonthly_simulated(MW)",size=4)+
  annotate("text",x=52,y=2550, label=Statistics$Observed_MW,colour = "red",size=4)+                        
  annotate("text",x=52,y=2800, label="TotMonthly_Observed(MW)",size=4)
  
dev.copy(png,"Extreme_bugindex_Energy_V4.png", width =2000, height = 1000,res=150)
dev.off() 


#################
#BugIndex and V5
#################
# Filtering the data frame for only V5 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V5', ]
# Filtering the data frame for only BugIndex
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='BugIndex', ]
# Reording the dataframe with respect to periods squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Ext_Eng2$new <- paste(Ext_Eng2$d,Ext_Eng2$p)
#Turn your 'new' column into a character vector
Ext_Eng2$new <- as.character(Ext_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Ext_Eng2$new <- factor(Ext_Eng2$new, levels=unique(Ext_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Ext_Eng2[Ext_Eng2$variable == 'ExtEnergy_Optimization',]
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
  scale_y_continuous(breaks = seq(1000,11000,500))+
  theme(legend.text=element_text(size=10,face="bold"),legend.title = element_text(size=10))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  ggtitle("Energy under Extreme BugIndex value for 0.9 MAF senario")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))+
  annotate("text",x=36,y=2530, label=Statistics$Tot_Monthlysim_MW,colour = "red",size=4)+                        
  annotate("text",x=36,y=2800, label="TotMonthly_simulated(MW)",size=4)+
  annotate("text",x=52,y=2530, label=Statistics$Observed_MW,colour = "red",size=4)+                        
  annotate("text",x=52,y=2800, label="TotMonthly_Observed(MW)",size=4)

dev.copy(png,"Extreme_bugindex_Energy_V5.png", width =2000, height = 1000,res=150)
dev.off() 

#################
#Hydropower and V1
#################
# Filtering the data frame for only V1 Senario
Ext_Eng1<- Ext_EnergyMelt[Ext_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Hydro
Ext_Eng2 <- Ext_Eng1[Ext_Eng1[, "f2"]=='Hydro', ]
# Reording the dataframe with respect to periods squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(p)),]
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
  scale_y_continuous(breaks = seq(1000,50000,1000))+
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Energy under Extreme Hydro value for 0.6 MAF senario")+
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
# Reording the dataframe with respect to periods squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(p)),]
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
  scale_y_continuous(breaks = seq(1000,50000,1000))+
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Energy under Extreme Hydro value for 0.7 MAF senario")+
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
# Reording the dataframe with respect to periods squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(p)),]
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
  scale_y_continuous(breaks = seq(1000,50000,1000))+
  theme(legend.text=element_text(size=12,face="bold"))+
  ggtitle("Energy under Extreme Hydro value for 0.749 MAF senario")+
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
# Reording the dataframe with respect to periods squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(p)),]
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
  scale_y_continuous(breaks = seq(1000,50000,1000))+
  theme(legend.text=element_text(size=12,face="bold"))+
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
# Reording the dataframe with respect to periods squence.
Ext_Eng2 <- Ext_Eng2[with(Ext_Eng2,order(p)),]
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
  scale_y_continuous(breaks = seq(1000,50000,1000))+
  theme(legend.text=element_text(size=12,face="bold"))+
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

#Renaming some value in column variable
Scen_EnergyMelt$variable<- as.character(Scen_EnergyMelt$variable)
Scen_EnergyMelt$variable[Scen_EnergyMelt$variable=="Energy_Bugflow"] <- "Energy_Bugflow_2days"

#################
#Scen1 and V1
#################
# Filtering the data frame for only V1 Senario
Scen_Eng1<- Scen_EnergyMelt[Scen_EnergyMelt[, "tot_vol"]=='V1', ]
# Filtering the data frame for only Scen1
Scen_Eng2 <- Scen_Eng1[Scen_Eng1[, "Scen"]=='sc1', ]
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,300))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen1 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen1 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen1 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen2 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen2 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen2 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen3 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen3 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen3 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen4 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen4 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen4 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen5 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen5 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen5 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen6 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen6 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen6 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen7 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen7 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen7 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen8 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen8 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen8 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen9 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen9 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen9 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen10 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen10 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen10 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen11 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen11 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen11 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen12 with 0.6MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen12 with 0.7MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen12 with 0.749MAF Total Monthly Release")+
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
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
# Reording the dataframe with respect to periods squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(p)),]
# Reording the dataframe with respect to Days squence.
Scen_Eng2 <- Scen_Eng2[with(Scen_Eng2,order(d)),]
#Merging 2 Columns into one inorder to meet the sub daily representation case.
Scen_Eng2$new <- paste(Scen_Eng2$d,Scen_Eng2$p)
#Turn your 'new' column into a character vector
Scen_Eng2$new <- as.character(Scen_Eng2$new)
#Then turn it back into a factor with the levels in the correct order
Scen_Eng2$new <- factor(Scen_Eng2$new, levels=unique(Scen_Eng2$new))

# Filtering the Data frame for only Energy_Bugflow case
filtter_alleqs <-Scen_Eng2[Scen_Eng2$variable == 'Energy_Bugflow',]
Totmonthly_Power_alleqs<-sum(filtter_alleqs$value)
# Filtering the Data frame for only Energy_optimization case
filtter_nosim <-Scen_Eng2[Scen_Eng2$variable == 'Energy_optimization',]
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
  scale_y_continuous(breaks = seq(1000,25000,1000))+ 
  theme(legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=14,face="bold"))+
  ggtitle("Energy under Scen12 with 0.9MAF Total Monthly Release")+
  theme(plot.title = element_text(size=18,hjust =0.5, vjust = 2,face="bold"))
dev.copy(png,"Energy_Scen12_v5.png", width =2000, height = 1000,res=150)
dev.off()

