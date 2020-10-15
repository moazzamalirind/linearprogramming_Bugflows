
$ontext
Title Optimization model for Glen Canyon Dam releases to favor Bugs population. (June 2018)

###################################
Created By: Moazzam Ali Rind
Email: moazzamalirind@gmail.com

Created : 6/15/2020
Last updated: 10/13/2020

Description: This model was developed to qaunitfy the trade-off between number of steady low flow days and hydropower revenue objectives.
            The model has 2 periods per day (i.e. pHigh and plow) and runs for a month. we have used linear programming to solve the problem.
            All the structural and operational constraints applied here are uptodate.

######################################
Pricing Scenarios
*#####################################
$offtext

Set

          d                             days in June
          p                             time period during a day /pLow "Low flow period",pHigh "High flow period"/
          tot_vol                       Total montly release volume (acre-ft)/V1*V5/
          modpar                        Saving model status for each of the scenario solution/ ModStat "Model Status", SolStat "Solve Status"/
          case                          Defining constrainted cases for number of low flow steady days /case1*case12/
          scen                          pricing scenarios tested /scen1 "$0/MWh differential", scen2 "$12.91/MWh differential",scen3 "$25.82/MWh differential"/
;


*======================================
*Parameters
*======================================

PARAMETERS

FStore(tot_vol,case,scen)                     Storing objective function values over different scenarios

XStore_steady(tot_vol,case,scen)                   Store Energy Generated during steady flow days over different cases (MWH)
XStore_unsteady(tot_vol,case,scen)                 Store Energy Generated during unsteady flow days over different cases (MWH)

RStore_steady(tot_vol,case,p,scen)                 Store Release values during steady flow days over different cases(cfs)
RStore_unsteady(tot_vol,case,p,scen)               Store Release values during unsteady flow days over different cases(cfs)

Sstore(tot_vol,case,scen)                          Store Storage Values over different cases(ac-ft)


ModelResults(tot_vol,case,modpar,scen)             Store solution status of the scenarios i.e. whether the solution found is optimal or not?


initstorage                           Initial reservoir storage 1st June 2018 (acre-ft)
maxstorage                            Reservoir capacity (acre-ft)
minstorage                            Minimum reservoir storage to maintain hydropower level(acre-ft)
Inflow(d)                             Inflow to reservoir (cfs)
maxRel                                Maximum release in a day d at any timeperiod p(cfs)
minRel                                Minimum release in a day d at any timeperiod p(cfs)
evap                                  evaporation (ac-ft per day Considered constant throughout the month
WeekdayRate(p)                        Energy prices on weekdays ($ per MWH)
weekendRate(p)                        Energy prices on weekends ($ per MWH) /pLow 37.70, pHigh 37.70/
Duration(p)                           Duration of period (hours)
Vol_monthlyrelease(tot_vol)           Different Total volumes of water to be released in the month i.e. June2018 in presented case (acre-ft)/V1 700000,V2 800000,V3 900000,V4 1000000,V5 1100000/
TotMonth_volume                       To represent total monthly volume (acre-ft)
Steady_Days                           To represent the number of steady low flow days
Num_steady(case)                      Number of steady low flow days/case1 0, case2 4,case3 6, case4 7, case5 8, case6 9,case7 10,case8 12,case9 15,case10 20,case11 25,case12 30/
Mar_Ramp(tot_vol,case,p,scen)         To save margninal values associted with the ramp rate
;

TABLE EnergyRate(scen,p)              Energy prices on weekdays ($ per MWH)
         pLow     pHigh
scen1    37.70    37.70
scen2    37.70    50.61
scen3    37.70    63.52
;


Duration("pLow")= 8;
* low period weightage in a day(08 Hours or 8 by 24 i.e:0.33 of day)

Duration("pHigh")= 16;
*  High period weightage in a day( 16 Hours or 16 by 24 i.e:0.67 of day)

*===================================================
* Read data from Excel
*===================================================
$CALL GDXXRW.EXE input=June2018.xls output=PriceScen_June18.gdx set=d rng=day!A1 Rdim=1  par=Inflow rng=inflow!A1 Rdim=1  par=initstorage rng=initstorage!A1 Rdim=0  par=maxstorage rng=maxstorage!A1 Rdim=0   par=minstorage rng=minstorage!A1 Rdim=0  par=maxRel rng=maxRel!A1 Rdim=0 par=minRel rng=minRel!A1 Rdim=0  par=evap rng=evap!A1 Rdim=0

*Write the input Data into a GDX file
$GDXIN PriceScen_June18.gdx

* loading parameters and input data from the GDX file into the model
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
Convert                       conversion factor from cfs to ac-ft per hour (0.0014*60)/0.083/
Totaldays                      Total number of days in the month/30/
weekdays                      Number of days in month/22/
weekends                       Number of weekend days in the month /8/
Num_of_timesteps              Total Number of timesteps used /60/
Daily_Ramprate                Allowable daily ramp rate (cfs)/8000/


VARIABLES

ObjectiveVal                   Objective functions calculation
storage                        reservoir storage at the end of the month (acre-ft)
release(p)                     reservoir release on any unsteady day in any period p (cfs)
Energy_Gen(p)                  Hydropower Generated for each timestep during unsteady flow days (MWH)
SteadyEn_Gen(p)                Hydropower Generated for each timestep during steady flow days (MWH)
Steady_Release                 Minimum release value of the hydrograph

Avail_Water                    Total Water available in the system during the month (June)
steady_Outflow                 Volume of water released in the steady low flow days (acre-ft)
unsteady_Outflow               Volume of water released in the unsteay low flow days (acre-ft)

;



EQUATIONS
*AND CONSTRAINTS

EQa_Inflow                   Total volume of Water available in the system during the month (acre-ft)
EQb_SteadyOutflow            Volume of Water released during the steady days (acre-ft)
EQc_UnSteadyOutflow          Volume of Water released during the unsteady days (acre-ft)

EQ1__ResMassBal              Reservoir mass balance (acre-ft)
EQ2__reqpowerstorage         The minimum storage equivalent to reservoir level required for hydropower generation (acre-ft)
EQ3__maxstor                 res storage max (acre-ft)
EQ4__MaxR(p)                 Max Release (cfs)
EQ5__MinR(p)                 Min Release  (cfs)
EQ6_Rampup_rate(p)           Constraining the daily ramp up rate between the timesteps(cfs)with in same day
EQ7__Monthtlyrel             Constraining Total monthly volume of water released in "June" as per WAPA information(acre-ft)
EQ8_Steadyflow               Minimun release value within the hydrograph(cfs)
EQ9_SteadyEnergy(p)          Energy generated in each time step during steady flow days (MWH)
EQ10_EnergyGen(p)            Energy generated in each time step during unsteady flow days (MWH)
EQ11_EnergyGen_Max(p)        Maximum Energy Generation Limit of the Glen Caynon Dam(MW)
EQ12_EnergyRevenue           Total monthly Hydropower Revenue generated when number of steady low flow day are greater than number of weekend days($)
EQ13_Revenue                 Total monthly Hydropower Renvenue generated if number of steady flow days are equal or less than weekend days ($)
;

*------------------------------------------------------------------------------*


EQa_Inflow..                 Avail_Water =e= initstorage + sum(d,inflow(d)*Convert*sum(p,Duration(p)));
EQb_SteadyOutflow..          steady_Outflow =e= Steady_Release*sum(p,Duration(p))*Convert*Steady_Days;
EQc_UnSteadyOutflow..        unsteady_Outflow =e= sum(p,release(p)*Convert*Duration(p))*(Totaldays-Steady_Days);


EQ1__ResMassBal..           storage =e= Avail_Water-steady_Outflow-unsteady_Outflow-(evap*Totaldays);
EQ2__reqpowerstorage..      storage =g= minstorage;
EQ3__maxstor..              storage =l= maxstorage;

EQ4__MaxR(p)..              release(p)=l= maxRel;
EQ5__MinR(p)..              release(p)=g= minRel;
EQ6_Rampup_rate(p)..        release("pHigh")-release("pLow")=l=Daily_Ramprate;

*EQ7_  constraining the overall monthly released volume..
EQ7__Monthtlyrel..                         TotMonth_volume=e= steady_Outflow + unsteady_Outflow;

EQ8_Steadyflow..                           Steady_Release=e=release("pLow");
* EQ8_  finds the minimimum release value from the hydrograph. This equation also takes care of ramprate bewtween unsteady high release and steady release.

EQ9_SteadyEnergy(p)..                      SteadyEn_Gen(p)=e= Steady_Release*Duration(p)*0.03715;
* Energy generation formula used in wapa Execl model.

EQ10_EnergyGen(p)..                         Energy_Gen(p)=e= release(p)*Duration(p)*0.03715;
* Energy generation formula used in wapa Execl model..

EQ11_EnergyGen_Max(p)..                    Energy_Gen(p)=l= 1320*Duration(p);
*Maximum Energy Generation capacity of GCD (MWH).. Source https://www.usbr.gov/uc/rm/crsp/gc/

*
EQ12_EnergyRevenue..                         ObjectiveVal=e= sum(p,SteadyEn_Gen(p)*weekendRate(p))*weekends + sum(p,SteadyEn_Gen(p)*WeekdayRate(p))*(Steady_Days-weekends)+ sum(p,Energy_Gen(p)*WeekdayRate(p))*(Totaldays-Steady_Days);
* *This equation works for number of steady days greater than number of weekend days.

EQ13_Revenue..                             ObjectiveVal=e= sum(p,SteadyEn_Gen(p)*weekendRate(p))*(Steady_Days) + sum(p,Energy_Gen(p)*weekendRate(p))*(weekends - Steady_Days)+ sum(p,Energy_Gen(p)*WeekdayRate(p))*(Totaldays-weekends);
*This equation works for number of steady days either equal or less than weekend days.  This equation price all the weekend days by weekned rate irrespective of whether its steady or hydropeaking day.


*------------------------------------------------------------------------------*

***************************************************
******Linearization Model
****************************************** ********

MODEL Model1 Find value of hydropower revenue using LP/EQa_Inflow,EQb_SteadyOutflow,EQc_UnSteadyOutflow,EQ1__ResMassBal,EQ2__reqpowerstorage,EQ3__maxstor,EQ4__MaxR,EQ5__MinR,EQ6_Rampup_rate,EQ7__Monthtlyrel,EQ8_Steadyflow,EQ9_SteadyEnergy,EQ10_EnergyGen,EQ11_EnergyGen_Max,EQ12_EnergyRevenue/;
*this Model will work under scenarios when number of steady low bug flow days are greater than number of weekned days.

*This line provides additional information in the .lst file. If you not aware of option file please comment it out or remove before running the model
Model1.optfile = 1;

Model Model2 Find value of hydropower revenue using LP /EQa_Inflow,EQb_SteadyOutflow,EQc_UnSteadyOutflow,EQ1__ResMassBal,EQ2__reqpowerstorage,EQ3__maxstor,EQ4__MaxR,EQ5__MinR,EQ6_Rampup_rate,EQ7__Monthtlyrel,EQ8_Steadyflow,EQ9_SteadyEnergy,EQ10_EnergyGen,EQ11_EnergyGen_Max,EQ13_Revenue/;
*this Model will work under scenarios when number of steady low bug flow days are less than or equal to number of weekned days.

*This line provides additional information in the .lst file. If you not aware of option file please comment it out or remove before running the model
Model2.optfile = 1;


*Looping over total volume and number of steay days scenarios
loop((tot_vol,case,scen),

*limiting some of the parameter of the solver
option reslim=20000;
option Threads=6;
option optcr=0.001;
*selecting the solver to be used
option LP= CPLEX;

*initializing the variables
release.L(p) = 10000;
Steady_Release.L= 8000;

*setting the total monthly volume equal to the mentioned volumes of the scenarios
TotMonth_volume = Vol_monthlyrelease(tot_vol);

*setting number of steady low flow days
Steady_Days = Num_steady(case)+ EPS;

*Setting the weekday energy pricing as per scen.
WeekdayRate(p)= EnergyRate(scen,p);

*solve statement
if  (Steady_Days > weekends,
     SOLVE Model1 USING LP maximize ObjectiveVal;
else SOLVE Model2 USING LP maximize ObjectiveVal;
);


* All the following lines of code are saving values for different parameters
   FStore(tot_vol,case,scen)= ObjectiveVal.L;
   XStore_steady(tot_vol,case,scen)= (Steady_Release.L*sum(p,Duration(p))*0.03715)*Num_steady(case);
   XStore_unsteady(tot_vol,case,scen)= sum(p,release.L(p)*Duration(p)*0.03715)*(Totaldays-Num_steady(case));
   RStore_steady(tot_vol,"case1",p,scen)= 0 + EPS;
*************************************************
   RStore_steady(tot_vol,case,p,scen)= Steady_Release.L;

   RStore_unsteady(tot_vol,case,p,scen)= release.L(p);
*********************************************************************

    RStore_unsteady(tot_vol,"case12",p,scen)= 0+ EPS ;

   Sstore(tot_vol,case,scen)=storage.L;

   ModelResults(tot_vol,"case1","SolStat",scen)= Model2.solvestat;
   ModelResults(tot_vol,"case1","ModStat",scen)= Model2.modelstat;
   ModelResults(tot_vol,"case2","SolStat",scen)= Model2.solvestat;
   ModelResults(tot_vol,"case2","ModStat",scen)= Model2.modelstat;
   ModelResults(tot_vol,"case3","SolStat",scen)= Model2.solvestat;
   ModelResults(tot_vol,"case3","ModStat",scen)= Model2.modelstat;
   ModelResults(tot_vol,"case4","SolStat",scen)= Model2.solvestat;
   ModelResults(tot_vol,"case4","ModStat",scen)= Model2.modelstat;
   ModelResults(tot_vol,"case5","SolStat",scen)= Model2.solvestat;
   ModelResults(tot_vol,"case5","ModStat",scen)= Model2.modelstat;
   ModelResults(tot_vol,case,"SolStat",scen)= Model1.solvestat;
   ModelResults(tot_vol,case,"ModStat",scen)= Model1.modelstat;

   Mar_Ramp(tot_vol,case,p,scen) = EQ6_Rampup_rate.M(p)+ EPS;

*clearing the cache memory before next itteration
   option clear=ObjectiveVal,clear=release,clear=Steady_Release;
);

   DISPLAY FStore,XStore_steady,XStore_unsteady,RStore_steady,RStore_unsteady,Sstore;

*------------------------------------------------------------------------------*
* Dump all input data and results to a GAMS gdx file
Execute_Unload "PriceScen_June18.gdx";
* Dump the gdx file to an Excel workbook
Execute "gdx2xls PriceScen_June18.gdx"

