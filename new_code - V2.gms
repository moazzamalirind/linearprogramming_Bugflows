
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

          d            days in May
*         p            time period during a day /pHigh,pLow/
          i            time period during day/1,2/
;

p('pHigh')= 0.542*d;
p('plow')= 0.458*d;

*======================================
*Parameters
*======================================

PARAMETERS

initstorage                  Initial reservoir storage on apr 30th 2018 (acre-ft)
maxstorage                   Reservoir capacity (acre-ft)
minstorage                   Minimum reservoir storage to maintain hydropower level(acre-ft)
Inflow(d)                    Inflow to reservoir (cfs)
maxRel                       Maximum release (cfs)
minRel                       Minimum release (cfs)
evap                         evaporation (ac-ft per day)
*phigh_weightage              High period weightage in a day(13 by 24 hrs. i.e:0.542 of day)
*plow_weightage               low period weightage in a day(11 by 24 hrs. i.e:0.458 of day)
*P(i);
*p(1)= 0.542;
*p(2)= 0.458;


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

Display d,inflow, initstorage, maxstorage, minstorage, maxRel, minRel, evap,p;

*===============================================
SCALAR
factor                       conversion factor from cfs to ac-ft /1.98348/


VARIABLES
bugflow_Objective             Minimize the difference between High and Low release value (cfs)

hydropower_Objective          Increase the overall hydropower revenue ($)
;

nonnegative variable
high(d)                       High release value (cfs)
low(d)                        Low release value (cfs)
storage(d)                    reservoir storage (acre-ft)
release(d)                    reservoir release (cfs)
diff(d)                       Difference between High and Low release value (cfs)
hp1(d)                        revenue from hydropower at high release value ($)
hp2(d)                        revenue from hydropower at low release value ($)

z1(d)                         outcome from release code ($)
z2(d)                         outcome from Hydropower code ($)
;

EQUATIONS
*AND CONSTRAINTS

EQ1__ResMassBal(d)           Reservoir mass balance (acre-ft)
EQ2__Hydrostorage(d)         The minimum storage equivalent to reservoir level needed for hydropower (acre-ft)
EQ3__maxstor(d)              res storage max (acre-ft)
EQ4__MaxR(d)                 Max Release (cfs)
EQ5__MinR(d)                 Min Release  (cfs)
EQ6__Highflow(d)             High Flow release (cfs)
EQ6a__Highflow(d)            High Flow release share (cfs)
EQ7__Lowflow(d)              Low Flow release (cfs)
EQ7a__Lowflow(d)             Low Flow release share (cfs)
EQ8__mayrel                  total release for "May" from WAPA file (acre-ft)
E8a_outcome(d)               Outcome calculation (acre-ft)
EQ9__Objective               Objective function (acre-ft)

hydropower_max              hydropower revenue ($)
Hydro_calculation(d)         Hydro calulation ($)
highhydro(d)                 Daily Revenue generated from High flow ($)
lowhydro(d)                  Daily Revenue generated from Low flow ($)

**eq_dif(d)
**highlowday(d)
**highlimit(d)
**lowlimit(d)
**hydropower(d)
;

*------------------------------------------------------------------------------*
EQ1__ResMassBal(d)..          storage(d) =e= initstorage$(ord(d)eq 1)+ storage(d-1)$(ord(d)gt 1)+ (inflow(d)*factor)- (release(d)*factor)-evap ;
EQ2__Hydrostorage(d)..        storage(d) =g= minstorage;
EQ3__maxstor(d)..             storage(d)=l= maxstorage;
EQ4__MaxR(d)..                release(d)=l= maxRel;
EQ5__MinR(d)..                release(d)=g= minRel;
EQ6__Highflow(d)..            high(d)=g= 10000;
EQ6a__Highflow(d)..           high(d)=e= 0.75*storage(d);
EQ7__Lowflow(d)..             low(d)=l=10000;
EQ7a__Lowflow(d)..            low(d)=e= 0.25*storage(d);
**Assuming that most of the release will be in the high flow time so 0.75 of daily water in high and 0.25 in low
**0.8 MAF total monthly water release as mentioned by WAPA File.
EQ8__mayrel..                 sum(d,storage(d))=e=800000;
E8a_outcome(d)..              z1(d)=e= (high(d)-low(d))*factor;
EQ9__Objective..              bugflow_Objective=e= sum (d,z1(d));
** Objective value will be in CFS.

highhydro(d)..                 hp1(d)=e=high(d)*62.43*432.54*0.66*60*13;
******************************                 *desity of water* Elevation head at GCD * assumed Efficiecny of Turbines
lowhydro(d)..                  hp2(d)=e=low(d)*62.43*432.54*0.66*25*9;
Hydro_calculation(d)..         z2(d)=e= hp1(d)+ hp2(d);
hydropower_max..               hydropower_Objective=e= sum (d,z2(d));

Model    HI /ALL/;

*Solve
SOLVE HI minimize bugflow_Objective using NLP;
SOLVE HI maximize hydropower_Objective using NLP;




