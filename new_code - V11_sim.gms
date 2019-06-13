
$ontext
Title Optimization model for Glen Canyon Dam releases to favor Bugs population




###################################
Created By: Moazzam Ali Rind
Email: moazzamalirind@gmail.com

Created : 4/24/2019
Last updated: 5/25/2019

Description: Daily High flow & low flow release concept with an aim to minimize the difference between two daily flows.
             Whereas, the overall objective of the model is to get the daily flow release outer bound which is not only favourable for bugs
             but also accounts the hydropower objective to its best. Conversely, the total monthly release amount is maintained same as per colorado river compact .

######################################

$offtext

****Model code:

Set

          d             days in May
          p             time period during a day /pHigh "High flow period",pLow "Low flow period"/
          f             objective functions/BugIndex "Bug Flow objective", Hydro "Hydropower Objective"/;

*Define a second name for the set f -- f2.
Alias (f,f2);

*======================================
*Parameters
*======================================

PARAMETERS

FtoUse(f)                    Objective functions to use (1=yes 0=no)*
FLevel(f)                    Right hand side of constraint when the objective is constrained
FStore(f2,f)                 Storing objective function values over different scenarios of f
XStore(f2,d)                 Store decision variable values over different scenarios of f
initstorage                  Initial reservoir storage on apr 30th 2018 (acre-ft)
maxstorage                   Reservoir capacity (acre-ft)
minstorage                   Minimum reservoir storage to maintain hydropower level(acre-ft)
Inflow(d)                    Inflow to reservoir (cfs)
maxRel                       Maximum release in a day d at any timeperiod p(cfs)
minRel                       Minimum release in a day d at any timeperiod p(cfs)
evap                         evaporation (ac-ft per day Considered constant throughout the month
EnergyRate(p)                Energy revenue ($ per MWH) /pHigh 60, pLow 25/
Duration(p)                  Duration of period (hours);

Duration("pHigh")= 13;
*phigh_weightage/duration              High period weightage in a day(13 by 24 hrs. i.e:0.542 of day)
Duration("pLow")= 11;
*plow_weightage/duration               low period weightage in a day(11 by 24 hrs. i.e:0.458 of day)

*===================================================
* Read data from Excel
*===================================================
$CALL GDXXRW.EXE input=Input.xlsx output= Bugflow_releases.gdx set=d rng=day!A1 Rdim=1  par=Inflow rng=inflow!A1 Rdim=1  par=initstorage rng=initstorage!A1 Rdim=0  par=maxstorage rng=maxstorage!A1 Rdim=0   par=minstorage rng=minstorage!A1 Rdim=0  par=maxRel rng=maxRel!A1 Rdim=0 par=minRel rng=minRel!A1 Rdim=0  par=evap rng=evap!A1 Rdim=0

*Write the input Data into a GDX file
$GDXIN E:\Project_Bugflow\Bugflow_releases.gdx


*****ALL GDXXRW input=New_Input.xlsx output=Bugflow.gdx set=d rng=day!A1 Rdim=1 set=r(rh,rl) rng=releases!A1 Rdim=1 par=inflow rng=inflow!A1:A2 Rdim=2 Cdim=1 par=initstorage rng=initstorage!A1 Rdim=1 par=maxstorage rng=maxstorage!A1 Cdim=1  par=minstorage rng=minstorage!A1 Cdim=1  par=maxRel rng=maxrelease!A1 Rdim=1 par=minRel rng=minrelease!A1 Rdim=1  par=evap rng=evap!A1 Rdim=1  par=hydro rng=hydrolevel!A1 Rdim=1  GDXIN Input.gdx*****

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

Display d,inflow, initstorage, maxstorage, minstorage, maxRel, minRel, evap,p,Duration;
*===============================================
SCALAR
conver                        conversion factor from cfs to ac-ft per day /1.98348/
factor_foracftperHr           conversion factor from cfs to ac-ft per hour /0.08/
factor_HptoKWH                conversion factor from Horse Power to KWH (0.746 by 550)/0.00098/
KWHtoMWH_factor               conversion factor from KWH to MWH /0.001/
Density_Water                 Density of Water(Lb per Ft3)/62.43/
Numdays                       Number of days in month/31/
Elev_Head                     Elevation Head at Glen Canyon Dam /432.54/
Efficiency                    Efficieny of power turbines at GCD /0.66/
Num_of_timesteps              Total Number of timesteps used /62/
Rel_vals                      Defined release value for simulation/9000/

VARIABLES
BugIndex                      Minimize the monthly hydropeaking index value
Hydro                         Maximize the overall monthly hydropower revenue($)

Positive Variables
storage(d)                    reservoir storage on any day d (acre-ft)
release(d,p)                  reservoir release on any day d in any period p (cfs)
Avgrelease                    Average release value for the whole month (cfs)
standarddev                   Standard deviation of releases over the month
Energyrate_vari(d,p)          Rate of hydropower with respect to day and period of day ($ per MWH)
FlowVol(d,p)                  volume of water released per time step(acre-ft)
;

EQUATIONS
*AND CONSTRAINTS

EQ1__ResMassBal(d)           Reservoir mass balance (acre-ft)
EQ2__reqpowerstorage(d)      The minimum storage equivalent to reservoir level required for hydropower generation (acre-ft)
EQ3__maxstor(d)              res storage max (acre-ft)
EQ4__MaxR(d,p)               Max Release (cfs)
EQ5__MinR(d,p)               Min Release  (cfs)
EQ6_Energyrate(d,p)          Defination of Energy rate as per period of day and day of week ($ per MWH)
EQ7_FlowVolume(d,p)          volume of water released per time step (acre-ft)
EQ8__Mayrel                  Constraining Total monthly volume of water released in "May" as per WAPA information(acre-ft)
EQ9_Avgrelease               Average Monthly release (cfs)
EQ10_Standarddev             Standard Devation over month.
EQ11_Hydropeakingindex(f)    Hydropeaking index value over month.
EQ12_EnergyRevenue(f)        Total monthly Hydropower Revenue generated ($)
EQ13_ReleaseSim(d,p)           Setting release values as predefined for simulation(cfs)
;


*------------------------------------------------------------------------------*

EQ1__ResMassBal(d)..         storage(d) =e= initstorage$(ord(d)eq 1)+ storage(d-1)$(ord(d)gt 1)+ (inflow(d)*conver)- sum(p,FlowVol(d,p))-evap;
EQ2__reqpowerstorage(d)..    storage(d) =g= minstorage;
EQ3__maxstor(d)..            storage(d)=l= maxstorage;
EQ4__MaxR(d,p)..             release(d,p)=l= maxRel ;
EQ5__MinR(d,p)..             release(d,p)=g= minRel;
EQ6_Energyrate(d,p)..        Energyrate_vari(d,p)=e= EnergyRate(p);
*Equation 6 is just making the energy rate same for all days. However in future we can change it as per senarios.

EQ7_FlowVolume(d,p)..        FlowVol(d,p) =e= release(d,p)*factor_foracftperHr*Duration(p);
EQ8__Mayrel..                sum(d,sum(p,FlowVol(d,p)))=e=800000;
*EQ8b_  trying to constrain the overall monthly released volume..
EQ9_Avgrelease..             Avgrelease=e= sum(d,sum(p,release(d,p))/2)/Numdays;
* Equation 9 is calculating the monlthy average release from the reservior. (Mathematical details of RHS: First summing daily two values and dividing by 2-for average- and then summing values for all days and dividing by total number of days i.e: 31 in May.
EQ10_Standarddev..           standarddev=e= sqrt[sum(d,sum{p,power(release(d,p)- Avgrelease,2)})/Num_of_timesteps];
* Equation 10 is calculating the monlthy average standard devation. (Mathematical details of RHS:   as per formula of standard dev i.e: sqrt((summation (value - average)^2)/number of values).. So same is applied here with the help of power function for squaring.

EQ11_Hydropeakingindex(f)$(ord(f) eq 1)..      BugIndex=e= standarddev/Avgrelease;
*EQ 11 is calculating hydropeaking index value for the whole month.
EQ12_EnergyRevenue(f)$(ord(f) eq 2)..           Hydro=e= sum((d,p), release(d,p)*Density_Water*Elev_Head *Efficiency*factor_HptoKWH*KWHtoMWH_factor*EnergyRate(p)*Duration(p));
**EQ12_HyrdroPower objective





*SOLVE HI minimize BugIndex using NLP;
*SOLVE HI maximize Hydro using NLP;
*display release.l,storage.l,Avgrelease.l,standarddev.l,BugIndex.l,FlowVol.l, Hydro.l;

***************************************************
******Simulation Model
****************************************** ********
*------------------------------------------------------------------------------*
*Eqauation 13 is introducing the steady bug flow on weekneds only while allowing the model to calculate release for other days as per formulation.
*Assuming the month of may 2018 (i.e. starting day will be tuesday and month ends on thursday).
EQ13_ReleaseSim(d,p)$((ord(d)>= 5  and ord(d)<= 6)
                    OR(ord(d)>= 12 and ord(d)<= 13)
                   OR (ord(d)>=19 and ord(d)<=20)
                  OR (ord(d)>=26 and ord(d)<=27))..               release(d,p)=e=Rel_vals;


 Model   HI /ALL/;
release.L(d,p) = 10;
Avgrelease.L = 10;


MODEL Simulation Find release values for Maximizing hydro /All/;


*MODEL ExtremePt Find an extreme point of the NLP /EQ11_Hydropeakingindex,EQ12_EnergyRevenue,EQ7_FlowVolume,EQ1__ResMassBal,EQ4__MaxR/

MODEL ExtremePt Find an extreme point of the NLP /ALL/;

*This section constrains one objective to be greater than a level
EQUATION
ObjAsCon(f)          Objective function as constraint f(x) = FLevel;

*The objective as constraint is greater or less or equal than the level set for that objective
ObjAsCon(f)$(1 - FtoUse(f))..        Hydro=e=FLevel(f);

MODEL ObjAsConstraint Single-objective model with other objectives constrained /ALL/;

*6. Solve the models.
*First, solve the single objective formulation(minimize BugFlow_objective)
FtoUse(f)=1;

*Solve as a single-objective nonlinear programming formulation
SOLVE ExtremePt USING NLP  MINIMIZING BugIndex;


* Solve for the extrement points in sequence
* Step A. First find the extreme points associated with objective #1. Ignore all other objectives
*   i.e., Max fi(X) s.t. aX <= b;
* Then move to objective #2

Loop(f2,
*  Ignore all the objectives
   FtoUse(f) = 0;
*  Only consider the current objective
   FtoUse(f) = 1;

   Display FtoUse;

*  Solve the model
   SOLVE ExtremePt USING NLP MINIMIZING BugIndex;

   DISPLAY release.L,storage.L,BugIndex.L,FlowVol.L,Hydro.L;
*Also save the results for later use
   FStore(f2,f)= BugIndex.L;
   XStore(f2,d) =sum(p,FlowVol.L(d,p));
);

DISPLAY FStore;


* Step B. Constrain one objective function value and maximize the other objective

**Minimize the bugindex objective, constrain the hydropower objective
FToUse(f) = 0;
FtoUse("BugIndex") = 1;
*Constrain the irrigation objective
*Choose a value between the extreme points for the irrigation objective identified above
FLevel("Hydro") =7877694.725;

*Alternatively
*Maximum the hydropower objective, constraint the  Bug objective

*FtoUse(f) = 0;
*FtoUse("Hydro") =1;
*Set a level for the Hydropower objective
*FLevel("BugIndex")= 0.659;

SOLVE ObjAsConstraint USING NLP MINIMIZING BugIndex;

DISPLAY release.L,storage.L,BugIndex.L,FlowVol.L, Hydro.L;

SOLVE Simulation USING NLP MINIMIZING BugIndex;
DISPLAY release.L,storage.L,BugIndex.L,FlowVol.L, Hydro.L;

* Dump all input data and results to a GAMS gdx file
Execute_Unload "Bug_simulation.gdx";
* Dump the gdx file to an Excel workbook
Execute "gdx2xls Bug_simulation.gdx"



