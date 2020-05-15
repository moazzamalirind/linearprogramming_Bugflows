
$ontext
Title Optimization model for Glen Canyon Dam releases to favor Bugs population. (June 2018)

###################################
Created By: Moazzam Ali Rind
Email: moazzamalirind@gmail.com

Created : 4/24/2019
Last updated: 7/24/2019

Description: Daily High flow & low flow release concept with an aim to minimize the difference between two daily flows.
             Whereas, the overall objective of the model is to get the daily flow release outer bounds which is not only favourable for bugs
             but also accounts the hydropower objective to its best. Conversely, the total monthly release amount is maintained same as per colorado river compact or Constrainted amount.

######################################

$offtext

****Model code:

Set
          d                             days in June
          p                             time period during a day /pLow "Low flow period",pHigh "High flow period"/
          f                             objective functions/BugIndex "Bug Flow objective", Hydro "Hydropower Objective"/
*          All_cases                     Joining All senarios in one set i.e. Extreme points cases and intermediate point cases/Min,sc1 * sc6,Max/
          tot_vol                       Total montly release volume (acre-ft)/V1*V5/
*          Scen(All_cases)               objective function scenario values /sc1 * sc6/
          modpar                        Saving model parameter for each of the solutions for each of the scenario/ ModStat "Model Statistics", SolStat "solve Statistics"/
          Ext_points                    Defining Extreme Points/Min, Max/
;

*Defining a second name for the set f as f2.
Alias (f,f2);


*======================================
*Parameters
*======================================

PARAMETERS

FtoUse(f)                               Objective functions to use (1=yes 0=no)
FLevel(f)                               Right hand side of constraint when the objective is constrained

FStore(f2,f,tot_vol)                    Storing objective function values over different scenarios of f with all equations intact
XStore(f2,d,tot_vol,p)                  Store Energy Generated at extreme values over different scenarios with all equations intact(MWH)
RStore(f2,d,tot_vol,p)                  Store Release values at extreme values over different scenarios with all equations intact(cfs)
Sstore(f2,d,tot_vol)                    Store Storage Value at extreme values over different scenarios with all equations intact(ac-ft)

FStore2(f2,f,tot_vol)                   Storing objective function values over different scenarios of f without simulation equation intact
XStore2(f2,d,tot_vol,p)                 Store Energy Generated at extreme values over different scenarios  without simulation equation intact(MWH)
RStore2(f2,d,tot_vol,p)                 Store Release values at extreme values over different scenarios  without simulation equation intact(cfs)
Sstore2(f2,d,tot_vol)                   Store Storage Value at extreme values over different scenarios with all equations intact(ac-ft)

$ontext
Scen_store(f,Scen,tot_vol)             Store objective function values under different senario values with all equations intact
Scen_store2(f,Scen,tot_vol)            Store objective function values under different senario values without simulation equation intact
ReleaseSave(tot_vol,Scen,d,p)          Saving release from each of the senario having all equations (cfs)
ReleaseSave2(tot_vol,Scen,d,p)         Saving release from each of the senario without simulation equation (cfs)
EnergyGen_save(tot_vol,Scen,d,p)       Saving Energy Generated from each of the senario having all equations (MWH)
EnergyGen_save2(tot_vol,Scen,d,p)      Saving Energy Generated from each of the senario without simulation equation (MWH)
Storage_Save(tot_vol,Scen,d)           Saving Storage values from each of the senario having all equations (ac-ft)
Storage_Save2(tot_vol,Scen,d)          Saving Storage values from each of the senariowithout simulation equation(ac-ft)

$offtext

ExtModeL_Stat(tot_vol,f2,modpar)       Extreme Points Model Results for the scenarios having all equations
ExtModeL_Stat2(tot_vol,f2,modpar)      Extreme Points Model Results for the scenarios without simulation equation
*ModelResults(tot_vol,Scen,modpar)      Model Results for the scenarios having all equations
*ModelResults2(tot_vol,Scen,modpar)     Model Results for the scenarios without simulation equation

$ontext
MinVal(tot_vol,f)                      Extracting the Minimum objectives values --Partial Optimization
MaxVal(tot_vol,f)                      Extracting the Maximum objectives values--Partial Optimization
MinVal2(tot_vol,f)                     Extracting the Minimum objectives values -- Full Optimization
MaxVal2(tot_vol,f)                     Extracting the Maximum objectives values -- Full Optimization

MinVal_a(tot_vol)                      Extracting the Minimum objective value (BugIndex objective in this case)--Partial Optimization
MaxVal_a(tot_vol)                      Extracting the Maximum objective value (BugIndex objective in this case)--Partial Optimization
MinVal2_a(tot_vol)                     Extracting the Minimum objective value (BugIndex objective in this case) -- Full Optimization
MaxVal2_a(tot_vol)                     Extracting the Maximum objective value (BugIndex objective in this case) -- Full Optimization
$offtext

initstorage                           Initial reservoir storage 1st June 2018 (acre-ft)
maxstorage                            Reservoir capacity (acre-ft)
minstorage                            Minimum reservoir storage to maintain hydropower level(acre-ft)
Inflow(d)                             Inflow to reservoir (cfs)
maxRel                                Maximum release in a day d at any timeperiod p(cfs)
minRel                                Minimum release in a day d at any timeperiod p(cfs)
evap                                  evaporation (ac-ft per day Considered constant throughout the month
EnergyRate(p)                         Energy revenue ($ per MWH) /pLow 24.56, pHigh 62.21/
Obj_dir(f)                            To set objective directions inorder to set the maximization and minimization of the objectives /BugIndex 1,Hydro -1/
Duration(p)                           Duration of period (hours)
Rel_vals(d,p)                         Defined release value for simulation with respect to day and period of day (cfs)
Use_Sim(d,p)                          Binary parameter which will specify simulation flow for day and period (1=yes 0=no)

*Levels(f,tot_vol,Scen)                Selected objective function levels with all equations(i.e. BugIndex)"Partial Optimization"
*Levels2(f,tot_vol,Scen)               Selected objective function levels without simulation equation(i.e. BugIndex) "Full Optimization"

Vol_monthlyrelease(tot_vol)           Different Total volumes of water to be released in the month i.e. June2018 in presented case (acre-ft)/V1 122000,V2 160000,V3 200000,V4 250000,V5 260000/
TotMonth_volume                       Total monthly volume (acre-ft)

;


Duration("pLow")= 8;
* low period weightage in a day(08 Hours or 11 by 24 i.e:0.33 of day)

Duration("pHigh")= 16;
*  High period weightage in a day( 13 Hours or 13 by 24 i.e:0.67 of day)

*===================================================
* Read data from Excel
*===================================================
*$CALL GDXXRW.EXE input=Input_June_2018.xlsx output= Junel8_0to8_dailycv.gdx set=d rng=day!A1 Rdim=1  par=Inflow rng=inflow!A1 Rdim=1  par=initstorage rng=initstorage!A1 Rdim=0  par=maxstorage rng=maxstorage!A1 Rdim=0   par=minstorage rng=minstorage!A1 Rdim=0  par=maxRel rng=maxRel!A1 Rdim=0 par=minRel rng=minRel!A1 Rdim=0  par=evap rng=evap!A1 Rdim=0

$CALL GDXXRW.EXE input=Input_June_2018.xls output= Junel8_trial.gdx set=d rng=day_week!A1 Rdim=1  par=Inflow rng=inflow!A1 Rdim=1  par=initstorage rng=initstorage!A1 Rdim=0  par=maxstorage rng=maxstorage!A1 Rdim=0   par=minstorage rng=minstorage!A1 Rdim=0  par=maxRel rng=maxRel!A1 Rdim=0 par=minRel rng=minRel!A1 Rdim=0  par=evap rng=evap!A1 Rdim=0
*Note please go back to original file with original name.

*Write the input Data into a GDX file
$GDXIN Junel8_trial.gdx

* parameters and input data from the GDX file into the model
$LOAD d
$LOAD inflow
$LOAD initstorage
$LOAD maxstorage
$LOAD minstorage
$LOAD maxRel
$LOAD minRel
$LOAD evap

*Close the GDX file
$GDXIN

Display d,inflow, initstorage, maxstorage, minstorage, maxRel, minRel, evap, p, Duration;
*===============================================
SCALAR
conver                        conversion factor from cfs to ac-ft per day /1.98348/
factor_foracftperHr           conversion factor from cfs to ac-ft per hour (0.0014*60)/0.084/
*factor_HptoKWH                conversion factor from Horse Power to KWH (0.746 by 550)/0.00098/
*KWHtoMWH_factor               conversion factor from KWH to MWH /0.001/
*Unitweight_Water              Specific weight of Water(Lb per Ft3)/62.43/
*factor_powerMW                Factor required to get results in MW with English Units /11810/
Numdays                       Number of days in month/7/
*Elev_Head                     Elevation Head at Glen Canyon Dam /432.54/
*Efficiency                    Efficieny of power turbines at GCD /0.9/
Num_of_timesteps              Total Number of timesteps used /14/
Daily_Ramprate                Allowable daily ramp rate (cfs)/8000/


VARIABLES

ObjectiveVal(f)               Objective functions calculation
CombineObjective              Combine objective functions for each senario

Positive Variables
storage(d)                    reservoir storage on any day d (acre-ft)
release(d,p)                  reservoir release on any day d in any period p (cfs)
Avgrelease(d)                 Average release value for the whole month (cfs)
standarddev(d)                Standard deviation of releases over the month
cv(d)                         coefficient of variance for a day d
Energyrate_vari(d,p)          Rate of hydropower with respect to day and period of day ($ per MWH)
Energy_Gen(d,p)               Hydropower Generated at a each time step (MWH)
ReleaseVol(d,p)               volume of water released per time step(acre-ft)

;

*Initialize
Use_Sim(d,p) = 0;
Rel_vals(d,p)= 0;
*Set weekend days to 1
Use_Sim(d,p) $((ord(d)>= 2  and ord(d)<= 3))= 1;

*                    OR(ord(d)>= 9 and ord(d)<= 10)
*                   OR (ord(d)>=16 and ord(d)<=17)
*                  OR (ord(d)>=23 and ord(d)<=24)
*                  OR (ord(d)>=30 and ord(d)<=30)) = 1;
*Set weekend flows to 10000 cfs.
Rel_vals(d,p)$Use_Sim(d,p) = 10000;


EQUATIONS
*AND CONSTRAINTS
EQ1__ResMassBal(d)           Reservoir mass balance (acre-ft)
EQ2__reqpowerstorage(d)      The minimum storage equivalent to reservoir level required for hydropower generation (acre-ft)
EQ3__maxstor(d)              Reservoir storage max (acre-ft)
EQ4__MaxR(d,p)               Max Release (cfs)
EQ5__MinR(d,p)               Min Release  (cfs)
*EQ6_Energyrate(d,p)          Defination of Energy rate as per period of day and day of week ($ per MWH)
EQ7_Rampup_rate(d,p)         Constraining the daily ramp up rate between the timesteps(cfs) ..(with in same day)
EQ7a_Rampdown_rate(d,p)      Constraining the daily ramp down rate between the timesteps(cfs) ..(with in same day)
EQ7b_Rampup_ratenext(d,p)    Constraining the daily ramp up rate between the last timestep of current day and next timestep for next day(cfs)
EQ7c_Rampdown_ratenext(d,p)  Constraining the daily ramp down rate between the last timestep of current day and next timestep for next day(cfs)
EQ7d_FlowVolume(d,p)         volume of water released per time step (acre-ft)
EQ8__Monthtlyrel             Constraining Total monthly volume of water released in "June" as per WAPA information(acre-ft)
EQ9_Avgrelease(d)            Average daily release (cfs)
EQ10_Standarddev(d)          Standard Devation over a day
EQ11_daliyCV(d)              CV for day d
EQ12_Hydropeakingindex(f)    Hydropeaking index value over month.
EQ13_EnergyGen(d,p)          Amount of energy generated in each time step (MWH)
EQ13a_EnergyGen_Max(d,p)     Maximum Energy Generation Limit of the Glen Caynon Dam(MW)for low Period
EQ13b_EnergyGen_Max(d,p)     Maximum Energy Generation Limit of the Glen Caynon Dam(MW) for high Period
EQ14_EnergyRevenue(f)        Total monthly Hydropower Revenue generated ($)
EQ15_CombinedObjectives      Defining all objective in single equation
EQ16_ReleaseSim(d,p)         Setting release values as predefined for simulation(cfs)
;

*------------------------------------------------------------------------------*

EQ1__ResMassBal(d)..         storage(d) =e= initstorage$(ord(d)eq 1)+ storage(d-1)$(ord(d)gt 1)+ (inflow(d)*conver)- sum(p,ReleaseVol(d,p))-evap;
EQ2__reqpowerstorage(d)..    storage(d) =g= minstorage;
EQ3__maxstor(d)..            storage(d)=l= maxstorage;
EQ4__MaxR(d,p)..             release(d,p)=l= maxRel ;
EQ5__MinR(d,p)..             release(d,p)=g= minRel;
*EQ6_Energyrate(d,p)..        Energyrate_vari(d,p)=e= EnergyRate(p);
*Equation 6 is just making the energy rate same for all days. However in future we can change it as per different rates for different days.

EQ7_Rampup_rate(d,p)..          release(d,"pHigh")-release(d,"pLow")=l=Daily_Ramprate;
EQ7a_Rampdown_rate(d,p)..       release(d,"pHigh")-release(d,"pLow")=g= -1*Daily_Ramprate ;
EQ7b_Rampup_ratenext(d,p)$(ord(d) lt Numdays)..     release(d,"pHigh")-release(d+1,"pLow")=g=-1*Daily_Ramprate;
EQ7c_Rampdown_ratenext(d,p)$(ord(d) lt Numdays)..   release(d,"pHigh")-release(d+1,"pLow")=l= Daily_Ramprate;

EQ7d_FlowVolume(d,p)..       ReleaseVol(d,p) =e= release(d,p)*factor_foracftperHr*Duration(p);
EQ8__Monthtlyrel..           sum(d,sum(p,ReleaseVol(d,p)))=e=TotMonth_volume;
*EQ8_  constraining the overall monthly released volume..
EQ9_Avgrelease(d)..            Avgrelease(d)=e= sum(p,release(d,p)*Duration(p))/24;
* Equation 9 is calculating the monlthy average release from the reservior. (Mathematical details of RHS: First summing daily two aggregated values and dividing 24(hours in day)-for average- and then summing values for all days and dividing by total number of days i.e: 30 in June.
EQ10_Standarddev(d)..           standarddev(d)=e= sqrt(sum(p, Duration(p)*power(release(d,p)-Avgrelease(d),2))/12);
* Equation 10 is calculating the daily weighted standard devation. (Mathematical details of RHS:  sqrt((summation weightage i.e (duration at period p) *(release at period p and day d - weighted agverage)^2.. and in denominator firstly normalizing with weights i.e. ((Number of weights -1)/Number of weights) and then multiply by summation of total amount weights.
EQ11_daliyCV(d)..                cv(d)=e= standarddev(d)/Avgrelease(d);
*  Equation 11 is calculating the daily Coefficient of variance.
EQ12_Hydropeakingindex(f)$(ord(f) eq 1)..     ObjectiveVal(f)=e= Obj_dir(f)*(sum(d,cv(d))/Numdays);
*EQ 12 is calculating hydropeaking index value for the whole month. Whereas, the -1 is added to switch the direction of objective (i.e. to minimize the Bug objective rather miximizing it).

*EQ12_EnergyGen(d,p)..                         Energy_Gen(d,p)=e= (release(d,p)*Unitweight_Water*Elev_Head *Efficiency*factor_HptoKWH*KWHtoMWH_factor)*Duration(p);
*https://www.quora.com/What-is-formula-of-hydroelectric-power-generation

*EQ12_EnergyGen(d,p)..                         Energy_Gen(d,p)=e= ((release(d,p)*Elev_Head *Efficiency)/factor_powerMW)*Duration(p);
*formula Source http://rivers.bee.oregonstate.edu/book/export/html/6

EQ13_EnergyGen(d,p)..                         Energy_Gen(d,p)=e= release(d,p)*Duration(p)*0.03715;
* Energy generation formula used in wapa Execl model..

EQ13a_EnergyGen_Max(d,p)$(ord(p)eq 1)..                    Energy_Gen(d,p)=l= 1320*Duration("pLow");
EQ13b_EnergyGen_Max(d,p)$(ord(p)eq 2)..                    Energy_Gen(d,p)=l= 1320*Duration("pHigh");
*Maximum Energy Generation capacity of GCD (MWH).. Source https://www.usbr.gov/uc/rm/crsp/gc/

EQ14_EnergyRevenue(f)$(ord(f) eq 2)..         ObjectiveVal(f)=e= Obj_dir(f)* sum((d,p),Energy_Gen(d,p)*EnergyRate(p));
**EQ14_HyrdroPower objective

EQ15_CombinedObjectives..                     CombineObjective=e= sum(f,FtoUse(f)*ObjectiveVal(f));

***************************************************
******Simulation Model
***************************************************
*------------------------------------------------------------------------------*
*Eqauation 14 is introducing the steady bug flow on weekneds only. while allowing the model to calculate release for other days as per formulation.
*Assuming the month of may 2018 (i.e. starting day will be tuesday and month ends on thursday).
EQ16_ReleaseSim(d,p)$Use_Sim(d,p)..           release(d,p)=e=Rel_vals(d,p);

*------------------------------------------------------------------------------*
*Initial values just to run simulation (Random Pick)... One might need to pick other numbers if he/she founds that some of the runs are locally infeasbile on their intial values.
release.L(d,p) = 10000;
Avgrelease.L(d) = 10000;


******************************************************
*****Model to disclose the Binding and Shadow Values
******************************************************

PARAMETERS
*For shadow values Only, these parameters are introduced ...

*For Partail Simulation Portion
EQ1__ResMassBal_m(d,tot_vol)           Reservoir mass balance (acre-ft)
EQ2__reqpowerstorage_m(d,tot_vol)      The minimum storage equivalent to reservoir level required for hydropower generation (acre-ft)
EQ3__maxstor_m(d,tot_vol)              res storage max (acre-ft)
EQ4__MaxR_m(d,p,tot_vol)               Max Release (cfs)
EQ5__MinR_m(d,p,tot_vol)               Min Release  (cfs)
*EQ6_Energyrate_m(d,p,tot_vol)          Defination of Energy rate as per period of day and day of week ($ per MWH)
EQ7_Rampup_rate_m(d,p,tot_vol)         Constraining the daily ramp up rate between the timesteps(cfs) ..(with in same day)
EQ7a_Rampdown_rate_m(d,p,tot_vol)      Constraining the daily ramp down rate between the timesteps(cfs) ..(with in same day)
EQ7b_Rampup_ratenext_m(d,p,tot_vol)    Constraining the daily ramp up rate between the last timestep of current day and next timestep for next day(cfs)
EQ7c_Rampdown_ratenext_m(d,p,tot_vol)  Constraining the daily ramp down rate between the last timestep of current day and next timestep for next day(cfs)
EQ7d_FlowVolume_m(d,p,tot_vol)         volume of water released per time step (acre-ft)
EQ8__Monthtlyrel_m(tot_vol)            Constraining Total monthly volume of water released in "June" as per WAPA information(acre-ft)
EQ9_Avgrelease_m(d,tot_vol)            Average daily release (cfs)
EQ10_Standarddev_m(d,tot_vol)          Standard Devation over a day d.
EQ11_daliyCV_m(d,tot_vol)              CV for day d
EQ12_Hydropeakingindex_m(f,tot_vol)    Hydropeaking index value over month.
EQ13_EnergyGen_m(d,p,tot_vol)          Amount of energy generated in each time step (MWH)
EQ13a_EnergyGen_Max_m(d,p,tot_vol)     Maximum Energy Generation Limit of the Glen Caynon Dam(MW)for High Period
EQ13b_EnergyGen_Max_m(d,p,tot_vol)     Maximum Energy Generation Limit of the Glen Caynon Dam(MW) for Low Period)
EQ14_EnergyRevenue_m(f,tot_vol)        Total monthly Hydropower Revenue generated ($)
EQ15_CombinedObjectives_m(tot_vol)     Defining all objective in single equation
EQ16_ReleaseSim_m(d,p,tot_vol)         Setting release values as predefined for simulation(cfs)


*For full Simulation Portion
EQ1__ResMassBal_m2(d,tot_vol)           Reservoir mass balance (acre-ft)
EQ2__reqpowerstorage_m2(d,tot_vol)      The minimum storage equivalent to reservoir level required for hydropower generation (acre-ft)
EQ3__maxstor_m2(d,tot_vol)              res storage max (acre-ft)
EQ4__MaxR_m2(d,p,tot_vol)               Max Release (cfs)
EQ5__MinR_m2(d,p,tot_vol)               Min Release  (cfs)
*EQ6_Energyrate_m2(d,p,tot_vol)          Defination of Energy rate as per period of day and day of week ($ per MWH)
EQ7_Rampup_rate_m2(d,p,tot_vol)         Constraining the daily ramp up rate between the timesteps(cfs) ..(with in same day)
EQ7a_Rampdown_rate_m2(d,p,tot_vol)      Constraining the daily ramp down rate between the timesteps(cfs) ..(with in same day)
EQ7b_Rampup_ratenext_m2(d,p,tot_vol)    Constraining the daily ramp up rate between the last timestep of current day and next timestep for next day(cfs)
EQ7c_Rampdown_ratenext_m2(d,p,tot_vol)  Constraining the daily ramp down rate between the last timestep of current day and next timestep for next day(cfs)
EQ7d_FlowVolume_m2(d,p,tot_vol)         volume of water released per time step (acre-ft)
EQ8__Monthtlyrel_m2(tot_vol)            Constraining Total monthly volume of water released in "June" as per WAPA information(acre-ft)
EQ9_Avgrelease_m2(d,tot_vol)            Average daily release (cfs)
EQ10_Standarddev_m2(d,tot_vol)          Standard Devation over day d.
EQ11_daliyCV_m2(d,tot_vol)              CV for day d
EQ12_Hydropeakingindex_m2(f,tot_vol)    Hydropeaking index value over month.
EQ13_EnergyGen_m2(d,p,tot_vol)          Amount of energy generated in each time step (MWH)
EQ13a_EnergyGen_Max_m2(d,p,tot_vol)     Maximum Energy Generation Limit of the Glen Caynon Dam(MW)for High Period
EQ13b_EnergyGen_Max_m2(d,p,tot_vol)     Maximum Energy Generation Limit of the Glen Caynon Dam(MW) for Low Period
EQ14_EnergyRevenue_m2(f,tot_vol)        Total monthly Hydropower Revenue generated ($)
EQ15_CombinedObjectives_m2(tot_vol)     Defining all objective in single equation

;



$ontext
*------------------------------------------------------------------------------*    *------------------------------------------------------------------------------*
* Merging Extreme and other senarios Results for better/easier post processing.
*------------------------------------------------------------------------------*    *------------------------------------------------------------------------------*
Parameters
Extreme_Points(f,tot_vol,Ext_points)    Saving Extreme points for All Volume Senarios (Partial Optimization)
Obj_Save(f,tot_vol,All_cases)           Saving Objectives values for all senarios(Partial Optimization)

Extreme_points2(f,tot_vol,Ext_points)   Saving Extreme points for All Volume Senarios (Full Optimization)
Obj_Save2(f,tot_vol,All_cases)          Saving Objectives values for all senarios(Full Optimization)
;
$offtext

***************************************************
******Extremepoints Model
****************************************** ********

*MODEL ExtremePt Finding extreme points using the NLP under each of the total Volume searios with simulation part (Partial Optimization).

MODEL ExtremePt Find extreme points by using NLP/ALL/ ;

loop((tot_vol,f2),
*Ignore all the objectives
   FtoUse(f) = 0;
*  Only consider the current objective
   FtoUse(f2) = 1;
   Display FtoUse;

option  reslim=10000000000;
option  iterlim = 2000000;
*release.L(d,p) = 12000;
*Avgrelease.L(d) = 12000;
*option  NumLoc= -1;
*MaxExecError = 100000;
*option sys12 = 1;

option Threads=6;
option optcr=0.00001;
option NLP= BARON;
*option nodLim=2000000;

   TotMonth_volume = Vol_monthlyrelease(tot_vol);
   SOLVE ExtremePt USING NLP Minimize CombineObjective;
*SOLVE ExtremePt USING NLP MAXIMIGING CombineObjective;
   FStore(f2,f,tot_vol)= ObjectiveVal.L(f)*Obj_dir(f)+ EPS;
   XStore(f2,d,tot_vol,p) = Energy_Gen.L(d,p);
   RStore(f2,d,tot_vol,p)=release.L(d,p);
   Sstore(f2,d,tot_vol)=storage.L(d);
   ExtModeL_Stat(tot_vol,f2,"SolStat") = ExtremePt.solvestat;
   ExtModeL_Stat(tot_vol,f2,"ModStat") = ExtremePt.modelstat;
   DISPLAY FStore,XStore,RStore,Sstore;

* Just disclosing the Binding and Shadow Values
EQ1__ResMassBal_m(d,tot_vol)= EQ1__ResMassBal.m(d);
EQ2__reqpowerstorage_m(d,tot_vol)= EQ2__reqpowerstorage.m(d);
EQ3__maxstor_m(d,tot_vol)= EQ3__maxstor.m(d);
EQ4__MaxR_m(d,p,tot_vol)= EQ4__MaxR.m(d,p);
EQ5__MinR_m(d,p,tot_vol)= EQ5__MinR.m(d,p);
*EQ6_Energyrate_m(d,p,tot_vol)= EQ6_Energyrate.m(d,p);
EQ7_Rampup_rate_m(d,p,tot_vol)= EQ7_Rampup_rate.m(d,p);
EQ7a_Rampdown_rate_m(d,p,tot_vol)= EQ7a_Rampdown_rate.m(d,p);
EQ7b_Rampup_ratenext_m(d,p,tot_vol)= EQ7b_Rampup_ratenext.m(d,p)$(ord(d) lt Numdays);
EQ7c_Rampdown_ratenext_m(d,p,tot_vol)= EQ7c_Rampdown_ratenext.m(d,p)$(ord(d) lt Numdays);
EQ7d_FlowVolume_m(d,p,tot_vol)= EQ7d_FlowVolume.m(d,p);
EQ8__Monthtlyrel_m(tot_vol) = EQ8__Monthtlyrel.m;
EQ9_Avgrelease_m(d,tot_vol)= EQ9_Avgrelease.m(d) ;
EQ10_Standarddev_m(d,tot_vol) = EQ10_Standarddev.m(d);
EQ11_daliyCV_m(d,tot_vol)= EQ11_daliyCV.m(d);
EQ12_Hydropeakingindex_m(f,tot_vol)=EQ12_Hydropeakingindex.m(f)$(ord(f) eq 1) ;
EQ13_EnergyGen_m(d,p,tot_vol)= EQ13_EnergyGen.m(d,p);
EQ13a_EnergyGen_Max_m(d,p,tot_vol)= EQ13a_EnergyGen_Max.m(d,p)$(ord(p)eq 1);
EQ13b_EnergyGen_Max_m(d,p,tot_vol)= EQ13b_EnergyGen_Max.m(d,p)$(ord(p)eq 2);
EQ14_EnergyRevenue_m(f,tot_vol)= EQ14_EnergyRevenue.m(f)$(ord(f) eq 2);
EQ15_CombinedObjectives_m(tot_vol) = EQ15_CombinedObjectives.m ;
EQ16_ReleaseSim_m(d,p,tot_vol)= EQ16_ReleaseSim.m(d,p)$Use_Sim(d,p);

option clear=CombineObjective,clear=ObjectiveVal,clear=release;
);



$ontext
*------------------------------------------------------------------------------*
*Finding values of scenarios (Level Parameter) for the All equation active (Partial optimization) Part.
loop((tot_vol,f),
*Saving both objectives extreme values
MinVal(tot_vol,f) = smin(f2,FStore(f2,f,tot_vol));
MaxVal(tot_vol,f) = smax(f2,FStore(f2,f,tot_vol));

*Merging both Extreme point in one Parameter for easier results post processing.
Extreme_Points(f,tot_vol,"Min")= MinVal(tot_vol,f);
Extreme_Points(f,tot_vol,"Max")=MaxVal(tot_vol,f);

Extreme_Points(f,tot_vol,EXT_points) = Extreme_Points(f,tot_vol,EXT_points) + EPS;

*Sorting just the values for Bugindex objective because in this presented model Bugindex objective is taken as constrain. However, one can easily get values for both functions by replacing BugIndex with "f" Or if only values of other objective is required just replace BugIndex with Hydro in Levels equation.
MinVal_a(tot_vol)= MinVal(tot_vol,"BugIndex");
MaxVal_a(tot_vol)= MaxVal(tot_vol,"BugIndex");

Display MinVal,MaxVal,MinVal_a,MaxVal_a,Extreme_points;

*Finding the intermediate levels between Extreme points
Levels("BugIndex",tot_vol,Scen)= MinVal_a(tot_vol)+((MaxVal_a(tot_vol)-MinVal_a(tot_vol))*(ord(scen)/(card(scen)+1)));
);

$offtext
*------------------------------------------------------------------------------*

*MODEL ExtremePt Finding extreme points using the NLP under each of the total Volume searios without simulation part(Full Optimization).

MODEL ExtremePt_minus_simulation Find extreme points by using NLP without simulation part /EQ1__ResMassBal,EQ2__reqpowerstorage,EQ3__maxstor,EQ4__MaxR,EQ5__MinR,EQ7_Rampup_rate,EQ7a_Rampdown_rate,EQ7b_Rampup_ratenext,EQ7c_Rampdown_ratenext,EQ7d_FlowVolume,EQ8__Monthtlyrel,EQ9_Avgrelease,EQ10_Standarddev,EQ11_daliyCV,EQ12_Hydropeakingindex, EQ13_EnergyGen,EQ13a_EnergyGen_Max,EQ13b_EnergyGen_Max,EQ14_EnergyRevenue,EQ15_CombinedObjectives/;
*MODEL ExtremePt_minus_simulation Find extreme points by using NLP without simulation part /EQ1__ResMassBal,EQ2__reqpowerstorage,EQ3__maxstor,EQ4__MaxR,EQ5__MinR,EQ7_Rampup_rate,EQ7a_Rampdown_rate,EQ7b_Rampup_ratenext,EQ7c_Rampdown_ratenext,EQ7d_FlowVolume,EQ8__Monthtlyrel,EQ9_Avgrelease,EQ10_Standarddev,EQ11_daliyCV,EQ12_Hydropeakingindex, EQ13_EnergyGen,EQ13a_EnergyGen_Max,EQ13b_EnergyGen_Max,EQ14_EnergyRevenue,EQ15_CombinedObjectives/;


loop((tot_vol,f2),
*Ignore all the objectives
   FtoUse(f) = 0;
*  Only consider the current objective
   FtoUse(f2) = 1;
   Display FtoUse;

option  reslim=10000000000;
option  iterlim = 2000000;
*option  NumLoc= -1;
*MaxExecError = 100000;
*option sys12 = 1;
option Threads=6;
option optcr=0.000001;
*option nodLim=2000000;
option NLP= BARON;

   TotMonth_volume = Vol_monthlyrelease(tot_vol);
  SOLVE ExtremePt_minus_simulation USING NLP minimize CombineObjective;
*SOLVE ExtremePt_minus_simulation USING NLP MAXIMIGING CombineObjective;
   FStore2(f2,f,tot_vol)= ObjectiveVal.L(f)*Obj_dir(f)+ EPS;
   XStore2(f2,d,tot_vol,p) =Energy_Gen.L(d,p);
   RStore2(f2,d,tot_vol,p)=release.L(d,p);
   Sstore2(f2,d,tot_vol)=storage.L(d);
   ExtModeL_Stat2(tot_vol,f2,"SolStat") = ExtremePt_minus_simulation.solvestat;
   ExtModeL_Stat2(tot_vol,f2,"ModStat") = ExtremePt_minus_simulation.modelstat;
   DISPLAY FStore2,XStore2,RStore2,Sstore2;

* Just disclosing the Binding and Shadow Values
EQ1__ResMassBal_m2(d,tot_vol)= EQ1__ResMassBal.m(d);
EQ2__reqpowerstorage_m2(d,tot_vol)= EQ2__reqpowerstorage.m(d);
EQ3__maxstor_m2(d,tot_vol)= EQ3__maxstor.m(d);
EQ4__MaxR_m2(d,p,tot_vol)= EQ4__MaxR.m(d,p);
EQ5__MinR_m2(d,p,tot_vol)= EQ5__MinR.m(d,p);
*EQ6_Energyrate_m2(d,p,tot_vol)= EQ6_Energyrate.m(d,p);
EQ7_Rampup_rate_m2(d,p,tot_vol)= EQ7_Rampup_rate.m(d,p);
EQ7a_Rampdown_rate_m2(d,p,tot_vol)= EQ7a_Rampdown_rate.m(d,p);
EQ7b_Rampup_ratenext_m2(d,p,tot_vol)= EQ7b_Rampup_ratenext.m(d,p)$(ord(d) lt Numdays);
EQ7c_Rampdown_ratenext_m2(d,p,tot_vol)= EQ7c_Rampdown_ratenext.m(d,p)$(ord(d) lt Numdays);
EQ7d_FlowVolume_m2(d,p,tot_vol)= EQ7d_FlowVolume.m(d,p);
EQ8__Monthtlyrel_m2(tot_vol) = EQ8__Monthtlyrel.m;
EQ9_Avgrelease_m2(d,tot_vol)= EQ9_Avgrelease.m(d);
EQ10_Standarddev_m2(d,tot_vol) = EQ10_Standarddev.m(d);
EQ11_daliyCV_m2(d,tot_vol)= EQ11_daliyCV.m(d);
EQ12_Hydropeakingindex_m2(f,tot_vol)=EQ12_Hydropeakingindex.m(f)$(ord(f) eq 1) ;
EQ13_EnergyGen_m2(d,p,tot_vol)= EQ13_EnergyGen.m(d,p);
EQ13a_EnergyGen_Max_m2(d,p,tot_vol)= EQ13a_EnergyGen_Max.m(d,p)$(ord(p)eq 1);
EQ13b_EnergyGen_Max_m2(d,p,tot_vol)= EQ13b_EnergyGen_Max.m(d,p)$(ord(p)eq 2);
EQ14_EnergyRevenue_m2(f,tot_vol)= EQ14_EnergyRevenue.m(f)$(ord(f) eq 2);
EQ15_CombinedObjectives_m2(tot_vol) = EQ15_CombinedObjectives.m;

option clear=CombineObjective,clear=ObjectiveVal,clear=release;
);


$ontext
*------------------------------------------------------------------------------*
*Finding values of scenarios (Level Parameter) for the Without simulation equation  (full optimization) Part.
loop((tot_vol,f),
*Saving both objectives extreme values
MinVal2(tot_vol,f) = smin(f2,FStore2(f2,f,tot_vol));
MaxVal2(tot_vol,f) = smax(f2,FStore2(f2,f,tot_vol));

*Merging both Extreme point in one Parameter for easier results post processing.
Extreme_Points2(f,tot_vol,"Min")= MinVal2(tot_vol,f);
Extreme_Points2(f,tot_vol,"Max")=MaxVal2(tot_vol,f);

Extreme_Points2(f,tot_vol,EXT_points) = Extreme_Points2(f,tot_vol,EXT_points) + EPS;


*Sorting just the values for Bugindex objective because in this presented model Bugindex objective is taken as constrain. However, one can easily get values for both functions by replacing BugIndex with "f" Or if only values of other objective is required just replace BugIndex with Hydro in Levels equation.
MinVal2_a(tot_vol)= MinVal2(tot_vol,"BugIndex");
MaxVal2_a(tot_vol)= MaxVal2(tot_vol,"BugIndex");

Display MinVal2,MaxVal2,MinVal2_a,MaxVal2_a,Extreme_points2;

*Finding the intermediate levels between Extreme points
Levels2("BugIndex",tot_vol,Scen)= MinVal2_a(tot_vol)+((MaxVal2_a(tot_vol)-MinVal2_a(tot_vol))*(ord(scen)/(card(scen)+1)));
);

*------------------------------------------------------------------------------*  *------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*  *------------------------------------------------------------------------------*

*This section constrains one objective to a certain defined level
EQUATION
ObjAsCon(f)          Objective function as constraint f(x) = FLevel;

*The objective as constraint is greater or less or equal than the level set for that objective
ObjAsCon(f)$(1 - FtoUse(f))..        ObjectiveVal(f)=e=FLevel(f);

MODEL ObjAsConstraint Single-objective model with other objectives constrained /ALL/;

MODEL ObjAsConstraint__minus_simulation Single-objective model with other objectives constrained /EQ1__ResMassBal,EQ2__reqpowerstorage,EQ3__maxstor,EQ4__MaxR,EQ5__MinR,EQ7_Rampup_rate,EQ7a_Rampdown_rate,EQ7b_Rampup_ratenext,EQ7c_Rampdown_ratenext,EQ7d_FlowVolume,EQ8__Monthtlyrel,EQ9_Avgrelease,EQ10_Standarddev,EQ11_daliyCV,EQ12_Hydropeakingindex, EQ13_EnergyGen,EQ13a_EnergyGen_Max,EQ13b_EnergyGen_Max,EQ14_EnergyRevenue,EQ15_CombinedObjectives,ObjAsCon/;
*MODEL ObjAsConstraint__minus_simulation Single-objective model with other objectives constrained /EQ1__ResMassBal,EQ2__reqpowerstorage,EQ3__maxstor,EQ4__MaxR,EQ5__MinR,EQ7_Rampup_rate,EQ7a_Rampdown_rate,EQ7b_Rampup_ratenext,EQ7c_Rampdown_ratenext,EQ7d_FlowVolume,EQ8__Monthtlyrel,EQ9_Avgrelease,EQ10_Standarddev,EQ11_daliyCV,EQ12_Hydropeakingindex, EQ13_EnergyGen,EQ13a_EnergyGen_Max,EQ13b_EnergyGen_Max,EQ14_EnergyRevenue,EQ15_CombinedObjectives,ObjAsCon/;


*6. Solve the models.
*First, solve the single objective formulation(minimize BugFlow_objective)
*FtoUse(f)=1;

* Step B. Constrain one objective function value and maximize the other objective

**Minimize the bugindex objective, constrain the hydropower objective
*FToUse(f) = 0;
*FtoUse("BugIndex") = 1;
*Constrain the irrigation objective
*Choose a value between the extreme points for the irrigation objective identified above
*FLevel("Hydro") =7877694.725;

*Alternatively
*Maximize the hydropower objective, constrain the  Bug objective

*For Partial Optimization Part (with Simulation Eqaution)
loop((Scen,tot_vol),
     FtoUse(f) = 0;
     FtoUse("Hydro") =1;
      TotMonth_volume = Vol_monthlyrelease(tot_vol);

*Set a level for the Bugindex objective
     FLevel(f)=Levels(f,tot_vol,Scen)* Obj_dir(f);

option  reslim=10000000000;
option  iterlim = 2000000;
*option  NumLoc= -1;
*release.L(d,p) = 12000;
*Avgrelease.L(d) = 12000;
*MaxExecError = 100000;
*option sys12 = 1;
option Threads=6;
option optcr=0.001;
*option nodLim=2000000;
option NLP= BARON;

     SOLVE ObjAsConstraint USING NLP minimize CombineObjective;
*SOLVE ObjAsConstraint USING NLP MAXIMIGING CombineObjective;
     Scen_store(f,Scen,tot_vol)= Obj_dir(f)*ObjectiveVal.L(f);
     ReleaseSave(tot_vol,Scen,d,p)= release.L(d,p);
     EnergyGen_save(tot_vol,Scen,d,p)= Energy_Gen.L(d,p);
     Storage_Save(tot_vol,Scen,d)=storage.L(d);
     ModelResults(tot_vol,Scen,"SolStat") = ObjAsConstraint.solvestat;
     ModelResults(tot_vol,Scen,"ModStat") = ObjAsConstraint.modelstat;
     DISPLAY Scen_store, ReleaseSave,EnergyGen_save,Storage_Save;


* Trying to Merge All objectives results in one parameter.
     Obj_Save(f,tot_vol,"sc1")= Scen_store(f,"sc1",tot_vol);
     Obj_Save(f,tot_vol,"sc2")= Scen_store(f,"sc2",tot_vol);
     Obj_Save(f,tot_vol,"sc3")= Scen_store(f,"sc3",tot_vol);
     Obj_Save(f,tot_vol,"sc4")= Scen_store(f,"sc4",tot_vol);
     Obj_Save(f,tot_vol,"sc5")= Scen_store(f,"sc5",tot_vol);
     Obj_Save(f,tot_vol,"sc6")= Scen_store(f,"sc6",tot_vol);
$ontext
     Obj_Save(f,tot_vol,"sc7")= Scen_store(f,"sc7",tot_vol);
     Obj_Save(f,tot_vol,"sc8")= Scen_store(f,"sc8",tot_vol);
     Obj_Save(f,tot_vol,"sc9")= Scen_store(f,"sc9",tot_vol);
     Obj_Save(f,tot_vol,"sc10")= Scen_store(f,"sc10",tot_vol);
     Obj_Save(f,tot_vol,"sc11")= Scen_store(f,"sc11",tot_vol);
     Obj_Save(f,tot_vol,"sc12")= Scen_store(f,"sc12",tot_vol);
*$offtext
     Obj_Save("BugIndex",tot_vol,"Min")= Extreme_Points("BugIndex",tot_vol,"Min");
     Obj_Save("BugIndex",tot_vol,"Max")=Extreme_Points("BugIndex",tot_vol,"Max");
     Obj_Save("Hydro",tot_vol,"Min")= Extreme_Points("Hydro",tot_vol,"Min");
     Obj_Save("Hydro",tot_vol,"Max")= Extreme_Points("Hydro",tot_vol,"Max");

option clear=CombineObjective,clear=ObjectiveVal,clear=release;
);


*For full Optimization Part (without Simulation Eqaution)
loop((Scen,tot_vol),
     FtoUse(f) = 0;
     FtoUse("Hydro") =1;
     TotMonth_volume = Vol_monthlyrelease(tot_vol);

*Set a level for the Bugindex objective
     FLevel(f)=Levels2(f,tot_vol,Scen)*Obj_dir(f);

option  reslim=10000000000;
option  iterlim = 2000000;
*release.L(d,p) = 12000;
*Avgrelease.L(d) = 12000;
*option  NumLoc= -1;
*MaxExecError = 100000;
*option sys12 = 1;
option Threads= 6;
option optcr=0.01;
*option nodLim=2000000;
option NLP= BARON;

     SOLVE ObjAsConstraint__minus_simulation USING NLP minimize CombineObjective;
*SOLVE ObjAsConstraint__minus_simulation USING NLP MAXIMIGING CombineObjective;
     Scen_store2(f,Scen,tot_vol)= ObjectiveVal.L(f)*Obj_dir(f);

     ReleaseSave2(tot_vol,Scen,d,p)= release.L(d,p);
     EnergyGen_save2(tot_vol,Scen,d,p)= Energy_Gen.L(d,p);
     Storage_Save2(tot_vol,Scen,d)=storage.L(d);
     ModelResults2(tot_vol,Scen,"SolStat") =ObjAsConstraint__minus_simulation.solvestat;
     ModelResults2(tot_vol,Scen,"ModStat") =ObjAsConstraint__minus_simulation.modelstat;
     DISPLAY Scen_store2,ReleaseSave2, EnergyGen_save2,Storage_Save2;

* Trying to Merge All objectives results in one parameter.
     Obj_Save2(f,tot_vol,"sc1")= Scen_store2(f,"sc1",tot_vol);
     Obj_Save2(f,tot_vol,"sc2")= Scen_store2(f,"sc2",tot_vol);
     Obj_Save2(f,tot_vol,"sc3")= Scen_store2(f,"sc3",tot_vol);
     Obj_Save2(f,tot_vol,"sc4")= Scen_store2(f,"sc4",tot_vol);
     Obj_Save2(f,tot_vol,"sc5")= Scen_store2(f,"sc5",tot_vol);
     Obj_Save2(f,tot_vol,"sc6")= Scen_store2(f,"sc6",tot_vol);
$ontext
     Obj_Save2(f,tot_vol,"sc7")= Scen_store2(f,"sc7",tot_vol);
     Obj_Save2(f,tot_vol,"sc8")= Scen_store2(f,"sc8",tot_vol);
     Obj_Save2(f,tot_vol,"sc9")= Scen_store2(f,"sc9",tot_vol);
     Obj_Save2(f,tot_vol,"sc10")= Scen_store2(f,"sc10",tot_vol);
     Obj_Save2(f,tot_vol,"sc11")= Scen_store2(f,"sc11",tot_vol);
     Obj_Save2(f,tot_vol,"sc12")= Scen_store2(f,"sc12",tot_vol);
*$offtext
     Obj_Save2("BugIndex",tot_vol,"Min")= Extreme_Points2("BugIndex",tot_vol,"Min");
     Obj_Save2("BugIndex",tot_vol,"Max")=Extreme_Points2("BugIndex",tot_vol,"Max");
     Obj_Save2("Hydro",tot_vol,"Min")= Extreme_Points2("Hydro",tot_vol,"Min");
     Obj_Save2("Hydro",tot_vol,"Max")= Extreme_Points2("Hydro",tot_vol,"Max");

option clear=CombineObjective,clear=ObjectiveVal,clear=release;

);
$offtext
*------------------------------------------------------------------------------*
* Dump all input data and results to a GAMS gdx file
Execute_Unload "Junel8_trial.gdx";
* Dump the gdx file to an Excel workbook
Execute "gdx2xls Junel8_trial.gdx"


*For BARON Solver in case of any issue with number of error itterations.
*MaxExecError = 100000;
*option sys12 = 1;



