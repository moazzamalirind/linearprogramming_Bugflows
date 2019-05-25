
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

*hydropower_Objective          Increase the overall monthly hydropower revenue ($)
Daily_hp
;

nonnegative variable

storage(d)                    reservoir storage on any day d (acre-ft)
release(d,p)                  reservoir release on any day d in any period p (cfs)
Avgrelease                    Average release value for the whole month (cfs)
standarddev                   Standard deviation of releases over the month
Highvol(d)                    Daily volume released by High flows (acre-ft)
Lowvol (d)                    Daily monthly volume released by low flows  (acre-ft)
hp1                           Daily Hydropower Revenue generated from High flow ($)
hp2                           Daily Hydropower Revenue generated from low flow ($
*Daily_hp(d)                   Total Daily Hydropower Revenue generated ($)

EQUATIONS
*AND CONSTRAINTS

EQ1__ResMassBal(d)           Reservoir mass balance (acre-ft)
EQ2__reqpowerstorage(d)      The minimum storage equivalent to reservoir level required for hydropower generation (acre-ft)
EQ3__maxstor(d)              res storage max (acre-ft)
EQ4__MaxR(d,p)               Max Release (cfs)
EQ5__MinR(d,p)               Min Release  (cfs)
EQ6__flowHigh(d,p)           High flow release condition or defination (cfs)
EQ7__flowLow(d,p)            Low flow release condition or defination(cfs)
EQ8a_Highflowvol(d,p)        Total monthly volume of water released by High flows (acre-ft)
EQ8b_lowflowvol(d,p)         Total monthly volume of water released by low flows (acre-ft)
EQ8__Mayrel                  Constraining Total monthly volume of water released in "May" as per WAPA information(acre-ft)
EQ9_Avgrelease               Average Monthly release (cfs)
EQ10_Standarddev             Standard Devation over month.
EQ11_Hydropeakingindex       Hydropeaking index value over month.
EQ12_highhydro               Daily Hydropower Revenue generated from High flow ($)
EQ13_lowhydro                Daily Hydropower Revenue generated from low flow ($)
Hydro_calculation            Total Daily Hydropower Revenue generated ($)
*hydropower_max               Hydropower revenue generated over the month ($)
;


*------------------------------------------------------------------------------*

EQ1__ResMassBal(d)..         storage(d) =e= initstorage$(ord(d)eq 1)+ storage(d-1)$(ord(d)gt 1)+ (inflow(d)*conver)- sum(p,release(d,p)*conver)-evap ;
EQ2__reqpowerstorage(d)..    storage(d) =g= minstorage;
EQ3__maxstor(d)..            storage(d)=l= maxstorage;
EQ4__MaxR(d,p)..             release(d,p)=l= maxRel;
EQ5__MinR(d,p)..             release(d,p)=g= minRel;
EQ6__flowHigh(d,p)..         release(d,p)$(ord(p) eq 1)=g= 11000;
*Equation 6 is defining the lower bound of High Flow release.
EQ7__flowLow(d,p)..          release(d,p)$(ord(p) eq 2)=l= 10000;
*Equation 7 is defining the upper bound of low Flow release.
EQ8a_Highflowvol(d,p)..      Highvol(d)=e= release(d,p)$(ord(p) eq 1)*0.08*13;
* Equation 8a is calculating total monthly volume of water released due to constant high flow for certian period of each day. Whereas, that period of day for high flow is 13 (from 9 A.M to 10 P.M) hours as per WAPA documents and 0.08 in equation is a conversion factor from CFS to acre-ft per hour.
EQ8b_Lowflowvol(d,p)..       Lowvol(d)=e= release(d,p)$(ord(p) eq 2)*0.08*11;
* Equation 8b is calculating total monthly volume of water released due to constant low flow for certian period of each day. Whereas, that period of day for high flow is 11(from 10 P.M to 9 A.M) hours as per WAPA documents and 0.08 in equation is a conversion factor from CFS to acre-ft per hour.
EQ8__Mayrel ..               sum(d,Highvol(d)+Lowvol(d))=e=800000;
*EQ8_ is not needed for this stage because its just trying to constrain the overall monthly released volume... I will definately involve this equation in my next trails. The sturcutur of this equation needs further thoughts before applications.
EQ9_Avgrelease..             Avgrelease=e= sum(d,sum(p,release(d,p))/2)/31;
* Equation 9 is calculating the monlthy average release from the reservior. (Mathematical details of RHS: First summing daily two values and dividing by 2-for average- and then summing values for all days and dividing by total number of days i.e: 31 in May.
EQ10_Standarddev..           standarddev=e= sqrt[sum(d,sum{p,power(release(d,p)- Avgrelease,2)})/62];
* Equation 10 is calculating the monlthy average standard devation. (Mathematical details of RHS:   as per formula of standard dev i.e: sqrt((summation (value - average)^2)/number of values).. So same is applied here with the help of power function for squaring.
EQ11_Hydropeakingindex..     Bugflow_Objective=e= standarddev/Avgrelease;
*EQ 11 is calculating hydropeaking index value for the whole month.

**HyrdroPower objective
EQ12_highhydro..          hp1=e= sum((d,p),release(d,p)$(ord(p) eq 1)*62.43*432.54*0.66*(0.746/550)*0.001*60*13);
****                                                            *desity of water(lb/ft3)* Elevation head at GCD in ft* Efficiecny of Turbines*(0.746 kW = 1 hp/550 foot-lbs./sec. = 1 hp)*conversion factor from KWH to MWH* price per MWH * Number of high flow hours in a day.
EQ13_lowhydro..           hp2=e=sum((d,p),release(d,p)$(ord(p) eq 2)*62.43*432.54*0.66*(0.746/550)*0.001*25*11);
****                                                            *desity of water(lb/ft3)* Elevation head at GCD in ft* Efficiecny of Turbines*(0.746 kW = 1 hp/550 foot-lbs./sec. = 1 hp)*conversion factor from KWH to MWH* price per MWH * Number of low flow hours in a day.
Hydro_calculation..          Daily_hp=e= hp1+ hp2;
*hydropower_max..             hydropower_Objective=e= sum (d,Daily_hp(d));




Model    HI /ALL/;

*Solve
SOLVE HI minimize Bugflow_Objective using NLP;
SOLVE HI maximize Daily_hp using NLP;

display release.l,storage.l,Avgrelease.l,standarddev.l;











