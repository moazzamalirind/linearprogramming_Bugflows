
$ontext
Title Optimization model for Glen Canyon Dam releases to favor Bugs population. (June 2018)

###################################
Created By: Moazzam Ali Rind
Email: moazzamalirind@gmail.com

Created : 4/24/2019
Last updated: 6/15/2020

Description: Daily High flow & low flow release concept with an aim to minimize the difference between two daily flows.
             Whereas, the overall objective of the model is to get the daily flow release outer bounds which is not only favourable for bugs
             but also accounts the hydropower objective to its best. Conversely, the total monthly release amount is maintained same as per colorado river compact or Constrainted amount.

######################################

#new Metric for Bug Suitability

$offtext

****Model code:

Set

          d                             days in June
          p                             time period during a day /pLow "Low flow period",pHigh "High flow period"/
          tot_vol                       Total montly release volume (acre-ft)/V1*V5/
          modpar                        Saving model parameter for each of the solutions for each of the scenario/ ModStat "Model Statistics", SolStat "Solve Statistics"/
          case                          Defining constrainted cases for number of low flow steady days /case1,case2,case3,case4/
;


*======================================
*Parameters
*======================================

PARAMETERS


FStore(tot_vol,case)                          Storing objective function values over different scenarios

XStore_steady(tot_vol,case)                   Store Energy Generated during steady flow days over different cases (MWH)
XStore_unsteady(tot_vol,case)                 Store Energy Generated during unsteady flow days over different cases (MWH)

RStore_steady(tot_vol,case,p)                 Store Release values during steady flow days over different cases(cfs)
RStore_unsteady(tot_vol,case,p)               Store Release values during unsteady flow days over different cases(cfs)

Sstore(tot_vol,case)                          Store Storage Values over different cases(ac-ft)

ModelResults(tot_vol,case,modpar)             Model Results for the scenarios


initstorage                           Initial reservoir storage 1st June 2018 (acre-ft)
maxstorage                            Reservoir capacity (acre-ft)
minstorage                            Minimum reservoir storage to maintain hydropower level(acre-ft)
Inflow(d)                             Inflow to reservoir (cfs)
Avail_Water                           Total Water available in the system during the month (June)
maxRel                                Maximum release in a day d at any timeperiod p(cfs)
minRel                                Minimum release in a day d at any timeperiod p(cfs)
evap                                  evaporation (ac-ft per day Considered constant throughout the month
EnergyRate(p)                         Energy revenue ($ per MWH) /pLow 24.56, pHigh 62.21/

Duration(p)                           Duration of period (hours)
Vol_monthlyrelease(tot_vol)           Different Total volumes of water to be released in the month i.e. June2018 in presented case (acre-ft)/V1 700000,V2 793857.12,V3 900000,V4 1000000,V5 1100000/
TotMonth_volume                       To represent total monthly volume (acre-ft)

Steady_Days                           To represent the number of steady low flow days

Num_steady(case)                      Number of steady low flow days/case1 0,case2 10, case3 20, case4 30/

steady_Outflow                        Volume of water released in the steay low flow days (acre-ft)
unsteady_Outflow                      Volume of water released in the unsteay low flow days (acre-ft)

;


Duration("pLow")= 8;
* low period weightage in a day(08 Hours or 8 by 24 i.e:0.33 of day)

Duration("pHigh")= 16;
*  High period weightage in a day( 16 Hours or 16 by 24 i.e:0.67 of day)

*===================================================
* Read data from Excel
*===================================================
$CALL GDXXRW.EXE input=June2018.xls output=Linear_June18.gdx set=d rng=day!A1 Rdim=1  par=Inflow rng=inflow!A1 Rdim=1  par=initstorage rng=initstorage!A1 Rdim=0  par=maxstorage rng=maxstorage!A1 Rdim=0   par=minstorage rng=minstorage!A1 Rdim=0  par=maxRel rng=maxRel!A1 Rdim=0 par=minRel rng=minRel!A1 Rdim=0  par=evap rng=evap!A1 Rdim=0

*Write the input Data into a GDX file
$GDXIN Linear_June18.gdx
*$GDXIN V1_check.gdx


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
conver                        conversion factor from cfs to ac-ft per day /1.983/
factor_foracftperHr           conversion factor from cfs to ac-ft per hour (0.0014*60)/0.083/
Numdays                       Number of days in month/30/
Num_of_timesteps              Total Number of timesteps used /60/
Daily_Ramprate                Allowable daily ramp rate (cfs)/8000/


VARIABLES

ObjectiveVal                   Objective functions calculation
storage                        reservoir storage on any day d (acre-ft)
release(p)                     reservoir release on any unsteady day in any period p (cfs)
Energy_Gen(p)                  Hydropower Generated at a each time step (MWH)
Steady_Release                 Minimum release value of the hydrograph

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
*EQ7_Rampdown_rate(p)        Constraining the daily ramp down rate between the timesteps(cfs) with in same day
EQ8__Monthtlyrel             Constraining Total monthly volume of water released in "June" as per WAPA information(acre-ft)
EQ9_Steadyflow               Minimun release value within the hydrograph(cfs)
EQ10_EnergyGen(p)            Amount of energy generated in each time step (MWH)
EQ11_EnergyGen_Max(p)        Maximum Energy Generation Limit of the Glen Caynon Dam(MW)for Low Period
EQ12_EnergyRevenue           Total monthly Hydropower Revenue generated ($)

;

*------------------------------------------------------------------------------*


EQa_Inflow..                 Avail_Water =e= initstorage + sum(d,inflow(d)*conver);
EQb_SteadyOutflow..          steady_Outflow =e= Steady_Release*conver*Steady_Days;
EQc_UnSteadyOutflow..        unsteady_Outflow =e= sum(p,release(p)*factor_foracftperHr* Duration(p))*(Numdays-Steady_Days);


EQ1__ResMassBal..           storage =e= Avail_Water-steady_Outflow-unsteady_Outflow-(evap*Numdays);
EQ2__reqpowerstorage..      storage =g= minstorage;
EQ3__maxstor..              storage =l= maxstorage;

EQ4__MaxR(p)..              release(p)=l= maxRel;
EQ5__MinR(p)..              release(p)=g= minRel;
EQ6_Rampup_rate(p)..           release("pHigh")-release("pLow")=l=Daily_Ramprate;
*EQ7_Rampdown_rate(p)..        release("pHigh")-release("pLow")=g= -1*Daily_Ramprate ;

*EQ8_  constraining the overall monthly released volume..
EQ8__Monthtlyrel..                         TotMonth_volume=e= steady_Outflow + unsteady_Outflow;

EQ9_Steadyflow..                           Steady_Release=e=release("pLow");
*EQ9_Threshold..                           Steady_Release=l=smin(p,release(p));
* EQ9_  finds the minimimum release value from the hydrograph.

EQ10_EnergyGen(p)..                         Energy_Gen(p)=e= release(p)*Duration(p)*0.03715;
* Energy generation formula used in wapa Execl model..


EQ11_EnergyGen_Max(p)..                    Energy_Gen(p)=l= 1320*Duration(p);
*Maximum Energy Generation capacity of GCD (MWH).. Source https://www.usbr.gov/uc/rm/crsp/gc/

EQ12_EnergyRevenue..                         ObjectiveVal=e= sum(p,Energy_Gen(p)*EnergyRate(p))*(Numdays-Steady_Days)+ (sum(p,Steady_Release*Duration(p)*0.03715*EnergyRate(p)))*Steady_Days;
**EQ12_HyrdroPower objective                                                                                                   Energy generated from the period of low steady flow multiply by the energy rate and summed over the day (2 periods) in a day and multiply by the number of steady low flow days.


***************************************************
******
***************************************************
*------------------------------------------------------------------------------*

***************************************************
******Linearization Model
****************************************** ********

MODEL Linearization Find extreme points by using lp/ALL/ ;

loop((tot_vol,case),

option reslim=20000;
option Threads=6;
option optcr=0.001;
option LP= BARON;
release.L(p) = 10000;
Steady_Release.L= 8000;
   TotMonth_volume = Vol_monthlyrelease(tot_vol);
   Steady_Days = Num_steady(case);
   SOLVE Linearization USING LP maximize ObjectiveVal;

   FStore(tot_vol,case)= ObjectiveVal.L;

   XStore_steady(tot_vol,case)= (Steady_Release.L*sum(p,Duration(p))*0.03715)*Num_steady(case);
   XStore_unsteady(tot_vol,case)= sum(p,release.L(p)*Duration(p)*0.03715)*(Numdays-Num_steady(case));

   RStore_steady(tot_vol,case,p)=release.L(p);
   RStore_unsteady(tot_vol,case,p)= Steady_Release.L;

   Sstore(tot_vol,case)=storage.L;

   ModelResults(tot_vol,case,"SolStat")= Linearization.solvestat;
   ModelResults(tot_vol,case,"ModStat")= Linearization.modelstat;
   DISPLAY FStore,XStore_steady,XStore_unsteady,RStore_steady,RStore_unsteady,Sstore;
   option clear=ObjectiveVal,clear=release,clear=Steady_Release;

);


*------------------------------------------------------------------------------*
* Dump all input data and results to a GAMS gdx file
Execute_Unload "Linear_June18.gdx";
*Execute_Unload "V1_check.gdx";
* Dump the gdx file to an Excel workbook
Execute "gdx2xls Linear_June18.gdx"

