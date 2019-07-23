
$ontext
Title Optimization model for Glen Canyon Dam releases to favor Bugs population.

###################################
Created By: Moazzam Ali Rind
Email: moazzamalirind@gmail.com

Created : 4/24/2019
Last updated: 7/20/2019

Description: Daily High flow & low flow release concept with an aim to minimize the difference between two daily flows.
             Whereas, the overall objective of the model is to get the daily flow release outer bound which is not only favourable for bugs
             but also accounts the hydropower objective to its best. Conversely, the total monthly release amount is maintained same as per colorado river compact .

######################################

$offtext

****Model code:

Set

          d                             days in May
          p                             time period during a day /pHigh "High flow period",pLow "Low flow period"/
          f                             objective functions/BugIndex "Bug Flow objective", Hydro "Hydropower Objective"/
          All_cases                     Joining All senarios in one set i.e. Extreme points cases and intermediate point cases/Min,sc1 * sc12,Max/
          tot_vol                       Total montly release volume (acre-ft)/V1*V5/
          Scen(All_cases)               objective function scenario values /sc1 * sc12/
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

Scen_store(f,Scen,tot_vol)             Store objective function values under different senario values with all equations intact
Scen_store2(f,Scen,tot_vol)            Store objective function values under different senario values without simulation equation intact
ReleaseSave(tot_vol,Scen,d,p)          Saving release from each of the senario having all equations (cfs)
ReleaseSave2(tot_vol,Scen,d,p)         Saving release from each of the senario without simulation equation (cfs)
EnergyGen_save(tot_vol,Scen,d,p)       Saving Energy Generated from each of the senario having all equations (MWH)
EnergyGen_save2(tot_vol,Scen,d,p)      Saving Energy Generated from each of the senario without simulation equation (MWH)
Storage_Save(tot_vol,Scen,d)           Saving Storage values from each of the senario having all equations (ac-ft)
Storage_Save2(tot_vol,Scen,d)          Saving Storage values from each of the senariowithout simulation equation(ac-ft)

ExtModeL_Stat(tot_vol,f2,modpar)       Extreme Points Model Results for the scenarios having all equations
ExtModeL_Stat2(tot_vol,f2,modpar)      Extreme Points Model Results for the scenarios without simulation equation
ModelResults(tot_vol,Scen,modpar)      Model Results for the scenarios having all equations
ModelResults2(tot_vol,Scen,modpar)     Model Results for the scenarios without simulation equation


MinVal(tot_vol,f)                      Extracting the Minimum objectives values --Partial Optimization
MaxVal(tot_vol,f)                      Extracting the Maximum objectives values--Partial Optimization
MinVal2(tot_vol,f)                     Extracting the Minimum objectives values -- Full Optimization
MaxVal2(tot_vol,f)                     Extracting the Maximum objectives values -- Full Optimization

MinVal_a(tot_vol)                      Extracting the Minimum objective value (BugIndex objective in this case)--Partial Optimization
MaxVal_a(tot_vol)                      Extracting the Maximum objective value (BugIndex objective in this case)--Partial Optimization
MinVal2_a(tot_vol)                     Extracting the Minimum objective value (BugIndex objective in this case) -- Full Optimization
MaxVal2_a(tot_vol)                     Extracting the Maximum objective value (BugIndex objective in this case) -- Full Optimization


initstorage                           Initial reservoir storage on apr 30th 2018 (acre-ft)
maxstorage                            Reservoir capacity (acre-ft)
minstorage                            Minimum reservoir storage to maintain hydropower level(acre-ft)
Inflow(d)                             Inflow to reservoir (cfs)
maxRel                                Maximum release in a day d at any timeperiod p(cfs)
minRel                                Minimum release in a day d at any timeperiod p(cfs)
evap                                  evaporation (ac-ft per day Considered constant throughout the month
EnergyRate(p)                         Energy revenue ($ per MWH) /pHigh 60, pLow 25/
Obj_dir(f)                            To set objective directions inorder to set the maximization and minimization of the objectives /BugIndex -1,Hydro 1/
Duration(p)                           Duration of period (hours)
Rel_vals(d,p)                         Defined release value for simulation with respect to day and period of day (cfs)
Use_Sim(d,p)                          Binary parameter which will specify simulation flow for day and period (1=yes 0=no)

Levels(f,tot_vol,Scen)                Selected objective function levels with all equations(i.e. BugIndex)"Partial Optimization"
Levels2(f,tot_vol,Scen)               Selected objective function levels without simulation equation(i.e. BugIndex) "Full Optimization"

Vol_monthlyrelease(tot_vol)           Different Total volumes of water to be released in the month i.e. may 2018 in presented case (acre-ft)/V1 800000,V2 900000,V3 1000000,V4 1100000,V5 1200000/
TotMonth_volume                       To represent total monthly volume (acre-ft)

;

Duration("pHigh")= 13;
*  High period weightage in a day( 13 Hours or 13 by 24 i.e:0.542 of day)
Duration("pLow")= 11;
* low period weightage in a day(11 Hours or 11 by 24 i.e:0.458 of day)


*===================================================
* Read data from Excel
*===================================================
$CALL GDXXRW.EXE input=Input.xlsx output= Bugflow_releases.gdx set=d rng=day!A1 Rdim=1  par=Inflow rng=inflow!A1 Rdim=1  par=initstorage rng=initstorage!A1 Rdim=0  par=maxstorage rng=maxstorage!A1 Rdim=0   par=minstorage rng=minstorage!A1 Rdim=0  par=maxRel rng=maxRel!A1 Rdim=0 par=minRel rng=minRel!A1 Rdim=0  par=evap rng=evap!A1 Rdim=0

*Write the input Data into a GDX file
$GDXIN Bugflow_releases.gdx

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

Display d,inflow, initstorage, maxstorage, minstorage, maxRel, minRel, evap, p, Duration, Scen;
*===============================================
SCALAR
conver                        conversion factor from cfs to ac-ft per day /1.98348/
factor_foracftperHr           conversion factor from cfs to ac-ft per hour /0.08/
*factor_HptoKWH                conversion factor from Horse Power to KWH (0.746 by 550)/0.00098/
*KWHtoMWH_factor               conversion factor from KWH to MWH /0.001/
*Unitweight_Water              Specific weight of Water(Lb per Ft3)/62.43/
*factor_powerMW                Factor required to get results in MW with English Units /11810/
Numdays                       Number of days in month/31/
*Elev_Head                     Elevation Head at Glen Canyon Dam /432.54/
*Efficiency                    Efficieny of power turbines at GCD /0.9/
Num_of_timesteps              Total Number of timesteps used /62/


VARIABLES

ObjectiveVal(f)               Objective functions calculation
CombineObjective              Combine objective functions for each senario

Positive Variables
storage(d)                    reservoir storage on any day d (acre-ft)
release(d,p)                  reservoir release on any day d in any period p (cfs)
Avgrelease                    Average release value for the whole month (cfs)
standarddev                   Standard deviation of releases over the month
Energyrate_vari(d,p)          Rate of hydropower with respect to day and period of day ($ per MWH)
Energy_Gen(d,p)               Hydropower Generated at a each time step (MWH)
ReleaseVol(d,p)               volume of water released per time step(acre-ft)

;


*Initialize
Use_Sim(d,p) = 0;
Rel_vals(d,p)= 0;
*Set weekend days to 1
Use_Sim(d,p) $((ord(d)>= 5  and ord(d)<= 6)
                    OR(ord(d)>= 12 and ord(d)<= 13)
                   OR (ord(d)>=19 and ord(d)<=20)
                  OR (ord(d)>=26 and ord(d)<=28)) = 1;
*Set weekend flows to 9000 cfs + 28 may which is also a holiday because of memorial day.
Rel_vals(d,p)$Use_Sim(d,p) = 9000;


EQUATIONS
*AND CONSTRAINTS

EQ1__ResMassBal(d)           Reservoir mass balance (acre-ft)
EQ2__reqpowerstorage(d)      The minimum storage equivalent to reservoir level required for hydropower generation (acre-ft)
EQ3__maxstor(d)              res storage max (acre-ft)
EQ4__MaxR(d,p)               Max Release (cfs)
EQ5__MinR(d,p)               Min Release  (cfs)
EQ6_Energyrate(d,p)          Defination of Energy rate as per period of day and day of week ($ per MWH)
EQ7_FlowVolume(d,p)          volume of water released per time step (acre-ft)
EQ8__Monthtlyrel             Constraining Total monthly volume of water released in "May" as per WAPA information(acre-ft)
EQ9_Avgrelease               Average Monthly release (cfs)
EQ10_Standarddev             Standard Devation over month.
EQ11_Hydropeakingindex(f)    Hydropeaking index value over month.
EQ12_EnergyGen(d,p)          Amount of energy generated in each time step (MWH)
EQ13_EnergyRevenue(f)        Total monthly Hydropower Revenue generated ($)
EQ14_ReleaseSim(d,p)         Setting release values as predefined for simulation(cfs)
EQ15_CombinedObjectives      Defining all objective in single equation
;

*------------------------------------------------------------------------------*

EQ1__ResMassBal(d)..         storage(d) =e= initstorage$(ord(d)eq 1)+ storage(d-1)$(ord(d)gt 1)+ (inflow(d)*conver)- sum(p,ReleaseVol(d,p))-evap;
EQ2__reqpowerstorage(d)..    storage(d) =g= minstorage;
EQ3__maxstor(d)..            storage(d)=l= maxstorage;
EQ4__MaxR(d,p)..             release(d,p)=l= maxRel ;
EQ5__MinR(d,p)..             release(d,p)=g= minRel;
EQ6_Energyrate(d,p)..        Energyrate_vari(d,p)=e= EnergyRate(p);
*Equation 6 is just making the energy rate same for all days. However in future we can change it as per different rates for different days.

EQ7_FlowVolume(d,p)..        ReleaseVol(d,p) =e= release(d,p)*factor_foracftperHr*Duration(p);
EQ8__Monthtlyrel..           sum(d,sum(p,ReleaseVol(d,p)))=e=TotMonth_volume;
*EQ8_  constraining the overall monthly released volume..
EQ9_Avgrelease..             Avgrelease=e= sum(d,sum(p,release(d,p))/2)/Numdays;
* Equation 9 is calculating the monlthy average release from the reservior. (Mathematical details of RHS: First summing daily two values and dividing by 2-for average- and then summing values for all days and dividing by total number of days i.e: 31 in May.
EQ10_Standarddev..           standarddev=e= sqrt[sum(d,sum{p,power(release(d,p)- Avgrelease,2)})/Num_of_timesteps];
* Equation 10 is calculating the monlthy average standard devation. (Mathematical details of RHS:   as per formula of standard dev i.e: sqrt((summation (value - average)^2)/number of values).. So same is applied here with the help of power function for squaring.

EQ11_Hydropeakingindex(f)$(ord(f) eq 1)..     ObjectiveVal(f)=e= Obj_dir(f)*(standarddev/Avgrelease);
*EQ 11 is calculating hydropeaking index value for the whole month. Whereas, the -1 is added to switch the direction of objective (i.e. to minimize the Bug objective rather miximizing it).

*EQ12_EnergyGen(d,p)..                         Energy_Gen(d,p)=e= (release(d,p)*Unitweight_Water*Elev_Head *Efficiency*factor_HptoKWH*KWHtoMWH_factor)*Duration(p);
*https://www.quora.com/What-is-formula-of-hydroelectric-power-generation

*EQ12_EnergyGen(d,p)..                         Energy_Gen(d,p)=e= ((release(d,p)*Elev_Head *Efficiency)/factor_powerMW)*Duration(p);
*formula Source http://rivers.bee.oregonstate.edu/book/export/html/6

EQ12_EnergyGen(d,p)..                         Energy_Gen(d,p)=e= release(d,p)*Duration(p)*0.03715;
* Energy generation formula used in wapa Execl model..

EQ13_EnergyRevenue(f)$(ord(f) eq 2)..         ObjectiveVal(f)=e= Obj_dir(f)* sum((d,p),Energy_Gen(d,p)*EnergyRate(p));
**EQ13_HyrdroPower objective

EQ15_CombinedObjectives..                     CombineObjective=e= sum(f,FtoUse(f)*ObjectiveVal(f));


***************************************************
******Simulation Model
***************************************************
*------------------------------------------------------------------------------*
*Eqauation 14 is introducing the steady bug flow on weekneds only. while allowing the model to calculate release for other days as per formulation.
*Assuming the month of may 2018 (i.e. starting day will be tuesday and month ends on thursday).
EQ14_ReleaseSim(d,p)$Use_Sim(d,p)..           release(d,p)=e=Rel_vals(d,p);

*------------------------------------------------------------------------------*
*Initial values just to run simulation (Random Pick)... One might need to pick other numbers if he/she founds that some of the runs are locally infeasbile on their intial values.
release.L(d,p) = 23000;
Avgrelease.L = 23000;



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
EQ6_Energyrate_m(d,p,tot_vol)          Defination of Energy rate as per period of day and day of week ($ per MWH)
EQ7_FlowVolume_m(d,p,tot_vol)          volume of water released per time step (acre-ft)
EQ8__Monthtlyrel_m(tot_vol)            Constraining Total monthly volume of water released in "May" as per WAPA information(acre-ft)
EQ9_Avgrelease_m(tot_vol)              Average Monthly release (cfs)
EQ10_Standarddev_m(tot_vol)            Standard Devation over month.
EQ11_Hydropeakingindex_m(f,tot_vol)    Hydropeaking index value over month.
EQ12_EnergyGen_m(d,p,tot_vol)          Amount of energy generated in each time step (MWH)
EQ13_EnergyRevenue_m(f,tot_vol)        Total monthly Hydropower Revenue generated ($)
EQ14_ReleaseSim_m(d,p,tot_vol)         Setting release values as predefined for simulation(cfs)
EQ15_CombinedObjectives_m(tot_vol)     Defining all objective in single equation


*For full Simulation Portion
EQ1__ResMassBal_m2(d,tot_vol)           Reservoir mass balance (acre-ft)
EQ2__reqpowerstorage_m2(d,tot_vol)      The minimum storage equivalent to reservoir level required for hydropower generation (acre-ft)
EQ3__maxstor_m2(d,tot_vol)              res storage max (acre-ft)
EQ4__MaxR_m2(d,p,tot_vol)               Max Release (cfs)
EQ5__MinR_m2(d,p,tot_vol)               Min Release  (cfs)
EQ6_Energyrate_m2(d,p,tot_vol)          Defination of Energy rate as per period of day and day of week ($ per MWH)
EQ7_FlowVolume_m2(d,p,tot_vol)          volume of water released per time step (acre-ft)
EQ8__Monthtlyrel_m2(tot_vol)            Constraining Total monthly volume of water released in "May" as per WAPA information(acre-ft)
EQ9_Avgrelease_m2(tot_vol)              Average Monthly release (cfs)
EQ10_Standarddev_m2(tot_vol)            Standard Devation over month.
EQ11_Hydropeakingindex_m2(f,tot_vol)    Hydropeaking index value over month.
EQ12_EnergyGen_m2(d,p,tot_vol)          Amount of energy generated in each time step (MWH)
EQ13_EnergyRevenue_m2(f,tot_vol)        Total monthly Hydropower Revenue generated ($)
EQ14_ReleaseSim_m2(d,p,tot_vol)         Setting release values as predefined for simulation(cfs)
EQ15_CombinedObjectives_m2(tot_vol)     Defining all objective in single equation
;

*------------------------------------------------------------------------------*    *------------------------------------------------------------------------------*
* Merging Extreme and other senarios Results for better/easier post processing.
*------------------------------------------------------------------------------*    *------------------------------------------------------------------------------*
Parameters
Extreme_Points(f,tot_vol,Ext_points)    Saving Extreme points for All Volume Senarios (Partial Optimization)
Obj_Save(f,All_cases,tot_vol)           Saving Objectives values for all senarios(Partial Optimization)
*Rel_Save(All_cases,tot_vol,d,p)           Saving Release values for all senarios(Partial Optimization)
*Stor_Save((All_cases,tot_vol,d)           Saving storage values for all senarios(Partial Optimization)
*EnergyGen_save(All_cases,tot_vol,d,p)     Saving Energy Generated values for all senarios(Partial Optimization)
*Modelstat_Save(All_cases,tot_vol,modpar)  Saving Model Statistics for all senarios(Partial Optimization)
*Levels_Merge(f,tot_vol,All_cases)         Merging All values together i.e. Extreme points and Intermediate points for constrained objective(Partial Optimization).

Extreme_points2(f,tot_vol,Ext_points)   Saving Extreme points for All Volume Senarios (Full Optimization)
Obj_Save2(f,All_cases,tot_vol)          Saving Objectives values for all senarios(Full Optimization)
*Rel_Save2(All_tot_vol,d,p)                 Saving Release values for all senarios(Full Optimization)
*Stor_Save2(All_cases,tot_vol,d)            Saving storage values for all senarios(Full Optimization)
*EnergyGen_save2(All_cases,tot_vol,d,p)     Saving Energy Generated values for all senarios(Full Optimization)
*Modelstat_Save2(All_cases,tot_vol,modpar)  Saving Model Statistics for all senarios (Full Optimization)
*Levels_Merge(f,tot_vol,All_cases)          Merging All values together i.e. Extreme points and Intermediate points for constrained objective(Full Optimization).



***************************************************
******Extremepoints Model
****************************************** ********

*MODEL ExtremePt Finding extreme points using the NLP under each of the total Volume searios with simulation part (Partial Optimization).

MODEL ExtremePt Find extreme points by using  NLP/ALL/ ;

loop((tot_vol,f2),
*Ignore all the objectives
   FtoUse(f) = 0;
*  Only consider the current objective
   FtoUse(f2) = 1;
   Display FtoUse;
   TotMonth_volume=Vol_monthlyrelease(tot_vol);
   SOLVE ExtremePt USING NLP MAXIMIGING CombineObjective;
   FStore(f2,f,tot_vol)= ObjectiveVal.L(f)*Obj_dir(f);
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
EQ6_Energyrate_m(d,p,tot_vol)= EQ6_Energyrate.m(d,p);
EQ7_FlowVolume_m(d,p,tot_vol)= EQ7_FlowVolume.m(d,p);
EQ8__Monthtlyrel_m(tot_vol) = EQ8__Monthtlyrel.m;
EQ9_Avgrelease_m(tot_vol)= EQ9_Avgrelease.m ;
EQ10_Standarddev_m(tot_vol) = EQ10_Standarddev.m ;
EQ11_Hydropeakingindex_m(f,tot_vol)=EQ11_Hydropeakingindex.m(f)$(ord(f) eq 1) ;
EQ12_EnergyGen_m(d,p,tot_vol)= EQ12_EnergyGen.m(d,p);
EQ13_EnergyRevenue_m(f,tot_vol)= EQ13_EnergyRevenue.m(f)$(ord(f) eq 2);
EQ14_ReleaseSim_m(d,p,tot_vol)= EQ14_ReleaseSim.m(d,p)$Use_Sim(d,p);
EQ15_CombinedObjectives_m(tot_vol) = EQ15_CombinedObjectives.m ;
);

*------------------------------------------------------------------------------*
*Finding values of scenarios (Level Parameter) for the All equation active (Partial optimization) Part.
loop((tot_vol,f),
*Saving both objectives extreme values
MinVal(tot_vol,f) = smin(f2,FStore(f2,f,tot_vol));
MaxVal(tot_vol,f) = smax(f2,FStore(f2,f,tot_vol));

*Merging both Extreme point in one Parameter for easier results post processing.
Extreme_Points(f,tot_vol,"Min")= MinVal(tot_vol,f);
Extreme_Points(f,tot_vol,"Max")=MaxVal(tot_vol,f);

*Sorting just the values for Bugindex objective because in this presented model Bugindex objective is taken as constrain. However, one can easily get values for both functions by replacing BugIndex with "f" Or if only values of other objective is required just replace BugIndex with Hydro in Levels equation.
MinVal_a(tot_vol)= MinVal(tot_vol,"BugIndex");
MaxVal_a(tot_vol)= MaxVal(tot_vol,"BugIndex");

Display MinVal,MaxVal,MinVal_a,MaxVal_a,Extreme_points;

*Find the intermediate levels between Extreme points
Levels("BugIndex",tot_vol,Scen)= MinVal_a(tot_vol)+((MaxVal_a(tot_vol)-MinVal_a(tot_vol))*(ord(scen)/(card(scen)+1)));
);

*------------------------------------------------------------------------------*

*MODEL ExtremePt Finding extreme points using the NLP under each of the total Volume searios without simulation part(Full Optimization).

MODEL ExtremePt_minus_simulation Find extreme points by using  NLP without simulation part /EQ1__ResMassBal,EQ2__reqpowerstorage,EQ3__maxstor,EQ4__MaxR,EQ5__MinR,EQ6_Energyrate,EQ7_FlowVolume,EQ8__Monthtlyrel,EQ9_Avgrelease,EQ10_Standarddev, EQ11_Hydropeakingindex, EQ12_EnergyGen,EQ13_EnergyRevenue,EQ15_CombinedObjectives/;

loop((tot_vol,f2),
*Ignore all the objectives
   FtoUse(f) = 0;
*  Only consider the current objective
   FtoUse(f2) = 1;
   Display FtoUse;
   TotMonth_volume=Vol_monthlyrelease(tot_vol);
   SOLVE ExtremePt_minus_simulation USING NLP MAXIMIGING CombineObjective;
   FStore2(f2,f,tot_vol)= ObjectiveVal.L(f)*Obj_dir(f);
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
EQ6_Energyrate_m2(d,p,tot_vol)= EQ6_Energyrate.m(d,p);
EQ7_FlowVolume_m2(d,p,tot_vol)= EQ7_FlowVolume.m(d,p);
EQ8__Monthtlyrel_m2(tot_vol) = EQ8__Monthtlyrel.m;
EQ9_Avgrelease_m2(tot_vol)= EQ9_Avgrelease.m ;
EQ10_Standarddev_m2(tot_vol) = EQ10_Standarddev.m ;
EQ11_Hydropeakingindex_m2(f,tot_vol)=EQ11_Hydropeakingindex.m(f)$(ord(f) eq 1) ;
EQ12_EnergyGen_m2(d,p,tot_vol)= EQ12_EnergyGen.m(d,p);
EQ13_EnergyRevenue_m2(f,tot_vol)= EQ13_EnergyRevenue.m(f)$(ord(f) eq 2);
EQ14_ReleaseSim_m2(d,p,tot_vol)= EQ14_ReleaseSim.m(d,p)$Use_Sim(d,p);
EQ15_CombinedObjectives_m2(tot_vol) = EQ15_CombinedObjectives.m ;
);

*------------------------------------------------------------------------------*
*Finding values of scenarios (Level Parameter) for the Without simulation equation  (full optimization) Part.
loop((tot_vol,f),
*Saving both objectives extreme values
MinVal2(tot_vol,f) = smin(f2,FStore2(f2,f,tot_vol));
MaxVal2(tot_vol,f) = smax(f2,FStore2(f2,f,tot_vol));

*Merging both Extreme point in one Parameter for easier results post processing.
Extreme_Points2(f,tot_vol,"Min")= MinVal2(tot_vol,f);
Extreme_Points2(f,tot_vol,"Max")=MaxVal2(tot_vol,f);

*Sorting just the values for Bugindex objective because in this presented model Bugindex objective is taken as constrain. However, one can easily get values for both functions by replacing BugIndex with "f" Or if only values of other objective is required just replace BugIndex with Hydro in Levels equation.
MinVal2_a(tot_vol)= MinVal2(tot_vol,"BugIndex");
MaxVal2_a(tot_vol)= MaxVal2(tot_vol,"BugIndex");

Display MinVal2,MaxVal2,MinVal2_a,MaxVal2_a,Extreme_points2;

*Find the intermediate levels between Extreme points
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

MODEL ObjAsConstraint__minus_simulation Single-objective model with other objectives constrained /EQ1__ResMassBal,EQ2__reqpowerstorage,EQ3__maxstor,EQ4__MaxR,EQ5__MinR,EQ6_Energyrate,EQ7_FlowVolume,EQ8__Monthtlyrel,EQ9_Avgrelease,EQ10_Standarddev, EQ11_Hydropeakingindex, EQ12_EnergyGen,EQ13_EnergyRevenue,EQ15_CombinedObjectives,ObjAsCon/;

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
     TotMonth_volume=Vol_monthlyrelease(tot_vol);
*Set a level for the Bugindex objective
     FLevel(f)=Levels(f,tot_vol,Scen)*Obj_dir(f);
     SOLVE ObjAsConstraint USING NLP MAXIMIGING CombineObjective;
     Scen_store(f,Scen,tot_vol)= Obj_dir(f)*ObjectiveVal.L(f);

* Trying to Merge All objectives results in one parameter.
     Obj_Save(f,All_cases,tot_vol)$(ord (All_cases) gt 1)= Scen_store(f,Scen,tot_vol);
     Obj_Save(f,All_cases,tot_vol)$(ord (All_cases) lt 14)= Scen_store(f,Scen,tot_vol);
     Obj_Save("BugIndex","Min",tot_vol)= Extreme_Points("BugIndex",tot_vol,"Min");
     Obj_Save("BugIndex","Max",tot_vol)=Extreme_Points("BugIndex",tot_vol,"Max");
     Obj_Save("Hydro","Min",tot_vol)= Extreme_Points("Hydro",tot_vol,"Min");
     Obj_Save("Hydro","Max",tot_vol)= Extreme_Points("Hydro",tot_vol,"Max");


     ReleaseSave(tot_vol,Scen,d,p)= release.L(d,p);
     EnergyGen_save(tot_vol,Scen,d,p)= Energy_Gen.L(d,p);
     Storage_Save(tot_vol,Scen,d)=storage.L(d);
     ModelResults(tot_vol,Scen,"SolStat") = ObjAsConstraint.solvestat;
     ModelResults(tot_vol,Scen,"ModStat") = ObjAsConstraint.modelstat;
     DISPLAY Scen_store, ReleaseSave,EnergyGen_save,Storage_Save;

);



*For full Optimization Part (without Simulation Eqaution)
loop((Scen,tot_vol),
     FtoUse(f) = 0;
     FtoUse("Hydro") =1;
     TotMonth_volume=Vol_monthlyrelease(tot_vol);
*Set a level for the Bugindex objective
     FLevel(f)=Levels2(f,tot_vol,Scen)*Obj_dir(f);
     SOLVE ObjAsConstraint__minus_simulation USING NLP MAXIMIGING CombineObjective;
     Scen_store2(f,Scen,tot_vol)= ObjectiveVal.L(f)*Obj_dir(f);

* Trying to Merge All objectives results in one parameter.
     Obj_Save2(f,All_cases,tot_vol)$(ord (All_cases) gt 1)= Scen_store2(f,Scen,tot_vol);
     Obj_Save2(f,All_cases,tot_vol)$(ord (All_cases) lt 14)= Scen_store2(f,Scen,tot_vol);
     Obj_Save2("BugIndex","Min",tot_vol)= Extreme_Points2("BugIndex",tot_vol,"Min");
     Obj_Save2("BugIndex","Max",tot_vol)=Extreme_Points2("BugIndex",tot_vol,"Max");
     Obj_Save2("Hydro","Min",tot_vol)= Extreme_Points2("Hydro",tot_vol,"Min");
     Obj_Save2("Hydro","Max",tot_vol)= Extreme_Points2("Hydro",tot_vol,"Max");


     ReleaseSave2(tot_vol,Scen,d,p)= release.L(d,p);
     EnergyGen_save2(tot_vol,Scen,d,p)= Energy_Gen.L(d,p);
     Storage_Save2(tot_vol,Scen,d)=storage.L(d);
     ModelResults2(tot_vol,Scen,"SolStat") =ObjAsConstraint__minus_simulation.solvestat;
     ModelResults2(tot_vol,Scen,"ModStat") =ObjAsConstraint__minus_simulation.modelstat;
     DISPLAY Scen_store2,ReleaseSave2, EnergyGen_save2,Storage_Save2;
);



$ontext
*------------------------------------------------------------------------------*
* Merging Extreme and other senarios Results for better/easier post processing.

Parameters
Obj_Save(f,All_cases,tot_vol)             Saving Objectives values for all senarios(Partail Optimization)
Rel_Save(All_cases,tot_vol,d,p)           Saving Release values for all senarios(Partail Optimization)
Stor_Save((All_cases,tot_vol,d)           Saving storage values for all senarios(Partail Optimization)
EnergyGen_save(All_cases,tot_vol,d,p)     Saving Energy Generated values for all senarios(Partail Optimization)
Modelstat_Save(All_cases,tot_vol,modpar)  Saving Model Statistics for all senarios(Partail Optimization)
Levels_Merge(f,tot_vol,All_cases)         Merging All values together i.e. Extreme points and Intermediate points for constrained objective(Partail Optimization).


Obj_Save2(f,All_cases,tot_vol)             Saving Objectives values for all senarios(Full Optimization)
Rel_Save2(All_tot_vol,d,p)                 Saving Release values for all senarios(Full Optimization)
Stor_Save2(All_cases,tot_vol,d)            Saving storage values for all senarios(Full Optimization)
EnergyGen_save2(All_cases,tot_vol,d,p)     Saving Energy Generated values for all senarios(Full Optimization)
Modelstat_Save2(All_cases,tot_vol,modpar)  Saving Model Statistics for all senarios (Full Optimization)
Levels_Merge(f,tot_vol,All_cases)          Merging All values together i.e. Extreme points and Intermediate points for constrained objective(Full Optimization).
;



* Partail Optimization


Loop((All_cases,tot_vol)

Levels_Merge(f,tot_vol,All_cases)=

FtoUse(f) = 0;
FtoUse("Hydro") =1;
FLevel(f)=Levels(f,tot_vol,Scen)*Obj_dir(f);
SOLVE ObjAsConstraint USING NLP MAXIMIGING CombineObjective;
Obj_Save(f,All_cases,tot_vol)= ObjectiveVal.L(f)*Obj_dir(f)



* Full Optimization
Loop((All_cases,Scen)


ReleaseSave(tot_vol,Scen,d,p)= release.L(d,p);
EnergyGen_save(tot_vol,Scen,d,p)= Energy_Gen.L(d,p);
Storage_Save(tot_vol,Scen,d)=storage.L(d);
ModelResults(tot_vol,Scen,"SolStat") = ObjAsConstraint.solvestat;
ModelResults(tot_vol,Scen,"ModStat") = ObjAsConstraint.modelstat;


$offtext
*------------------------------------------------------------------------------*
* Dump all input data and results to a GAMS gdx file
Execute_Unload "Bug_simulation_Triallevel.gdx";
* Dump the gdx file to an Excel workbook
Execute "gdx2xls Bug_simulation_Triallevel.gdx"



