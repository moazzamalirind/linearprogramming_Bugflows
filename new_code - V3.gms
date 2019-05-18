
$ontext
Title Optimization model for Glen Canyon Dam releases to favor Bugs population




###################################
Created By: Moazzam Ali Rind
Email: moazzamalirind@gmail.com

Created : 4/24/2019
Last updated: 5/14/2019

Description: Daily High flow & low flow release concept with an aim to minimize the difference between two daily flows.
             Whereas, the overall objective of the model is to get the daily flow release outer bound which is not only favourable for bugs
             but also accounts the hydropower objective to its best. Conversely, the total monthly release amount is maintained same as per colorado river compact .

######################################

$offtext

****Model code:

Set

          d             days in May
          p             time period during a day /pHigh "High flow period",pLow "Low flow period"/
;


*======================================
*Parameters
*======================================

PARAMETERS

initstorage                  Initial reservoir storage on apr 30th 2018 (acre-ft)
maxstorage                   Reservoir capacity (acre-ft)
minstorage                   Minimum reservoir storage to maintain hydropower level(acre-ft)
Inflow(d)                    Inflow to reservoir (cfs)
maxRel                       Maximum release in a day d at any timeperiod p(cfs)
minRel                       Minimum release in a day d at any timeperiod p(cfs)
evap                         evaporation (ac-ft per day. Considered constant throughout the month.
Duration(p)                  Duration of period (hours);

*phigh_weightage/duration              High period weightage in a day(13 by 24 hrs. i.e:0.542 of day)
*plow_weightage/duration               low period weightage in a day(11 by 24 hrs. i.e:0.458 of day)

Duration("pHigh")= 0.542*24;
Duration("pLow")= 0.458*24;

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

VARIABLES
Bugflow_Objective             Minimize the monthly hydropeaking index value

hydropower_Objective          Increase the overall monthly hydropower revenue ($)
;

nonnegative variable

storage(d)                    reservoir storage on any day d (acre-ft)
release(d,p)                  reservoir release on any day d in any period p (cfs)
Avgrelease                    Average release value for the whole month (cfs)
standarddev                   Standard deviation of releases over the month


EQUATIONS
*AND CONSTRAINTS

EQ1__ResMassBal(d)           Reservoir mass balance (acre-ft)
EQ2__reqpowerstorage(d)      The minimum storage equivalent to reservoir level required for hydropower generation (acre-ft)
EQ3__maxstor(d)              res storage max (acre-ft)
EQ4__MaxR(d,p)               Max Release (cfs)
EQ5__MinR(d,p)               Min Release  (cfs)
*EQ6__Mayrel                  total release for "May" from WAPA file (acre-ft)
EQ7_Avgrelease               Average Monthly release (cfs)
EQ8_Standarddev              Standard Devation over month.
EQ9_Hydropeakingindex        Hydropeaking index value over month.
;


*------------------------------------------------------------------------------*

EQ1__ResMassBal(d)..         storage(d) =e= initstorage$(ord(d)eq 1)+ storage(d-1)$(ord(d)gt 1)+ (inflow(d)*conver)- sum(p,release(d,p)*conver)-evap ;
EQ2__reqpowerstorage(d)..    storage(d) =g= minstorage;
EQ3__maxstor(d)..            storage(d)=l= maxstorage;
EQ4__MaxR(d,p)..             release(d,p)=l= maxRel;
EQ5__MinR(d,p)..             release(d,p)=g= minRel;
*EQ6__Mayrel ..               sum(d,release(d,p))=e=800000;
*EQ6_ is not needed for this stage because its just trying to constrain the overall monthly released volume... I will definately involve this equation in my next trails. The sturcutur of this equation needs further thoughts before applications.
EQ7_Avgrelease..             Avgrelease=e= sum(d,sum(p,release(d,p))/2)/31;
* Equation 7 is calculating the monlthy average release from the reservior. (Mathematical details of RHS: First summing daily two values and dividing by 2-for average- and then summing values for all days and dividing by total number of days i.e: 31 in May.
EQ8_Standarddev..            standarddev=e= sqrt[sum(d,sum{p,power(release(d,p)- Avgrelease,2)})/62];
* Equation 8 is calculating the monlthy average standard devation. (Mathematical details of RHS:   as per formula of standard dev i.e: sqrt((summation (value - average)^2)/number of values).. So same is applied here with the help of power function for squaring.
EQ9_Hydropeakingindex..      Bugflow_Objective=e= standarddev/Avgrelease;
*EQ 9 is calculating hydropeaking index value for the whole month.
Model    HI /ALL/;

*Solve
SOLVE HI minimize Bugflow_Objective using NLP;

display release.l,storage.l,Avgrelease.l,standarddev.l;











