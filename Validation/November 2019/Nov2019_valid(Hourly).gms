$ontext
Title Optimization model for Glen Canyon Dam releases to favor Bugs population. (November 2019)

###################################
Created By: Moazzam Ali Rind
Email: moazzamalirind@gmail.com

Created : 10/16/2020
Last updated: 10/17/2020

Description: This model was developed to qaunitfy the trade-off between number of steady low flow days and hydropower revenue objectives.
            The model has 2 periods per day (i.e. pHigh and plow) and runs for a month. we have used linear programming to solve the problem.
            All the structural and operational constraints applied here are uptodate.

######################################

$offtext


****Model code:

Set

          d             days in November
          p             time period during a day /p1*p24/;


*======================================
*Parameters
*======================================

PARAMETERS
FStore                       Storing objective function value
initstorage                  Initial reservoir storage.. Storage observed on 31 october 2019 (acre-ft)
maxstorage                   Reservoir capacity (acre-ft)
minstorage                   Minimum reservoir storage to maintain hydropower level(acre-ft)
Inflow(d)                    Inflow to reservoir (cfs)
observed_release(d,p)        observed release at November 2019 averaged over hourly timesteps (cfs)
maxRel                       Maximum release in a day d at any timeperiod p(cfs)
minRel                       Minimum release in a day d at any timeperiod p(cfs)
evap                         evaporation (ac-ft per day) Considered constant throughout the month
EnergyRate(p)                Energy revenue ($ per MWH)

;



*===================================================
* Read data from Excel
*===================================================
$CALL GDXXRW.EXE input=Input_Nov2019.xlsx output= Nov19_Valid(Hr).gdx  par=observed_release rng=Hourly_Rel!A1:Y32 par=EnergyRate rng=Rates!A1 Rdim=1  set=d rng=day!A1 Rdim=1  par=Inflow rng=inflow!A1 Rdim=1  par=initstorage rng=initstorage!A1 Rdim=0  par=maxstorage rng=maxstorage!A1 Rdim=0   par=minstorage rng=minstorage!A1 Rdim=0  par=maxRel rng=maxRel!A1 Rdim=0 par=minRel rng=minRel!A1 Rdim=0  par=evap rng=evap!A1 Rdim=0

*Write the input Data into a GDX file
$GDXIN Nov19_Valid(Hr).gdx

* parameters and input data from the GDX file into the model
$LOAD d
$LOAD inflow
$LOAD initstorage
$LOAD maxstorage
$LOAD minstorage
$LOAD maxRel
$LOAD minRel
$LOAD evap
$LOAD observed_release
$LOAD EnergyRate
*Close the GDX file
$GDXIN

Display d,inflow, initstorage, maxstorage, minstorage, maxRel, minRel, evap,p, EnergyRate, observed_release;
*===============================================
SCALAR
Convert                        conversion factor from cfs to ac-ft per hour (0.0014*60)/0.084/
Num_of_timesteps               Total Number of timesteps used /720/
Daily_Ramprate                 Allowable daily ramp rate (cfs)/8000/

VARIABLES
ObjectiveVal                   Objective function value

Positive Variables
storage(d)                    reservoir storage on any day d (acre-ft)
release(d,p)                  reservoir release on any day d in any period p (cfs)
Energy_Gen(d,p)               Hydropower Generated at a each time step (MWH)
ReleaseVol(d,p)               volume of water released per month(acre-ft)
Tot_Energy                   Total Hydropower Generated  (MWH)
Tot_vol                      Total water released per month(acre-ft)
;


EQUATIONS
*AND CONSTRAINTS

EQ1__ResMassBal(d)           Reservoir mass balance (acre-ft)
EQ2__reqpowerstorage(d)      The minimum storage equivalent to reservoir level required for hydropower generation (acre-ft)
EQ3__maxstor(d)              Reservoir storage max (acre-ft)
EQ4__MaxR(d,p)               Max Release (cfs)
EQ5__MinR(d,p)               Min Release  (cfs)
EQ7_FlowVolume              volume of water released per timestep (acre-ft)
EQ7a_TotVolume              volume of water released per month (acre-ft)
EQ8_EnergyGen                Amount of energy generated per timestep (MWH)
EQ8a_TotEnergy               Amount of energy generated total per month (MWH)
EQ9_EnergyRevenue            Total monthly Hydropower Revenue generated ($)
EQ10_ReleaseSim(d,p)         Setting release values as predefined for simulation(cfs)

;


*------------------------------------------------------------------------------*

EQ1__ResMassBal(d)..         storage(d) =e= initstorage$(ord(d)eq 1)+ storage(d-1)$(ord(d)gt 1)+ (inflow(d)*Convert*24)- sum(p,ReleaseVol(d,p))-evap;
EQ2__reqpowerstorage(d)..    storage(d) =g= minstorage;
EQ3__maxstor(d)..            storage(d)=l= maxstorage;
EQ4__MaxR(d,p)..             release(d,p)=l= maxRel ;
EQ5__MinR(d,p)..             release(d,p)=g= minRel;

EQ7_FlowVolume(d,p)..        ReleaseVol(d,p)=e= release(d,p)*Convert;
EQ7a_TotVolume..             Tot_vol=e= sum ((d,p),ReleaseVol(d,p));

EQ8_EnergyGen(d,p)..          Energy_Gen(d,p)=e= release(d,p)*0.03715;
* Energy generation formula used in wapa Execl model..
EQ8a_TotEnergy..             Tot_Energy=e= sum ((d,p),Energy_Gen(d,p));

**HyrdroPower objective
EQ9_EnergyRevenue..                           ObjectiveVal=e=  sum((d,p),Energy_Gen(d,p)*EnergyRate(p));

***************************************************
******Simulation Model
****************************************** ********
*------------------------------------------------------------------------------*

* Constraining the releases to the observed releases.
EQ10_ReleaseSim(d,p)..                 release(d,p)=e=observed_release(d,p);

* Define MODEL validation

MODEL validation Find ObjectiveValue using  LP /ALL/;
*Solve the model
SOLVE validation USING LP MAXIMIGING ObjectiveVal;
FStore = ObjectiveVal.L;

DISPLAY FStore,Tot_Energy.l,Tot_Vol.l;

* Dump all input data and results to a GAMS gdx file
Execute_Unload "Nov19_Valid(Hr).gdx";
* Dump the gdx file to an Excel workbook
Execute "gdx2xls Nov19_Valid(Hr).gdx"



