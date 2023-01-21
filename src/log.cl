+--------------------------------------------------------------------------------------------+
|  EMLA                                                                                      |
|  Evolutionary Machine Learning Agents                                                      |
|  Copyright (C) Yves Caseau 2016-2018                                                       |
+--------------------------------------------------------------------------------------------+

EDITED ON OCTOBER 21st

// February 29th 2016 ============== version 0.1 ==========================================

model.cl:  start

tseries :
- create the Time (date & calendar) utilities
- read the file produced by Knomee

// March 1st

model.cl: create a profile
tseries.cl: read file (add a profile as a parameter)

// ski trip
- display for time series
- load -> profiles
- algebra

// March 12th - move to home PC
// load files & fix syntax errors + nice date print
- distance, correlation

// March 19th move to laptop
algebra.cl :  file with all algebraic methods
  (1) lift: make<*>(t:TS) - approximation heuristic
  (2) project: TSerie!(t:TST) 
  (3) mutate : random variation
  (4) cross : crossing of two terms
  (5) optimize : local opt to better fit a goal
  
// march 23: foo.cl, to be merged with algebra
new algebra (A) !
A :: basic | composed | sum(A,derived[T])
basic :: trend | weekly | hourly | curve
composed :: mix | split
derived[T] :: corr | cumm | thresh 
 - defined depth & dist 
  
// march 25th - plane
(1) lift heuristics
(2) local opt

// March 27 home
- feed foo.cl into algebra.cl :)
- move to MacBook :)

// April 8th
(3) cross
(4) mutate
(5) two-opt = mutate(simple) + opt(simple)

// April 9th & 10th - end of v0.1 - on laptop
(5) EMLA methods  : pool management - done
- fix syntax (load)

// April 16th & 17th
makeBasic
makeDerived / makeSomb -> makeCorrelation

// April 30th & May 1st
- makeCummulative
- makeTreshold
- makeMix / MakeSplit
- makeTST = lift !
  - randomize

// May 5th to 8th
- 2 opt=> requires a dcopy
  WARNING : trying to use CLAIRE copy create a strange bug :eats all memory
  WARNING : mod has poor type inference :)  (x mod 7) -> (0 .. 6)

- May 6th: code is completed !
  compilation to clean up :)

  - create a PoolStrategy object
  - twoOpt without built-in repetition (too expensive)

//TODO!
   - implement ways to avoid using Split and Cycles (which are prone to overfit)
   - implement a flag to avoid Sums => pure data analysis = trend,H,W,Mix
   - test forecasting for T2, T3 and T4
   - experiment with all Data but 3 and see what is predicted

// TODO  (April / May ) - fun part

level 1 - test lift - done on May 1 st.
    create / random / measure distance / print
level 2 - test opt1 -done on May 4th
   run on list / best lift found
   look for fixed point
level3 - test twoOpt (requires mutate)
       - test mutate (single? et multiple?)
       - test Xover
level 4 - test pool with 10 terms - done on May 6th
- create optimization problems
- play with different parameters
   - number of mutations
   - pool versus local opt
 NOTE  : right now, EMLA works worse than TOST(1000) :)

 May 7th : freeze for a  while ... start Knomee v1.2!

 // June 12th : unfreeze => KnoZero v1.2 is complete :)
 // TODO:
 (1) add two new test files
 (2) resume

 // JUne 19th - weekeend when EMLA must work
 - step 1: create a test file that is interpreted (usual trick)
 - step 2: run some first experiments tost versus emal
 done !!! now emla works very well  (fixed bug in cross => dcopy)
 + fixbug in two opt

level 5 - smart evaluation
  - start simple forecasting (remove the last data) => done
    forecast(Yves,i) -> ...
    CONCLUSION: need to train EMLA on quality (remove 3 points and check the distance)

// June 25th
  - learn from other trackers => simple mode (NOSUM)
  - KEN: do not use all data (but 3 points) and check forecast => good quality indicator
  - produce ken(...) => random sample
    and ken2( ...)  => deterministic samples of 3 points
  - added variants of distance => see what works best = square + weight
      => try different distances: see what is the best training performance to get quality !
         - sum of abs
         - sum of square

// July 2nd
  - look at the impact of complexity on quality
      => produce simpler terms (NOSUM) or (MAX COMPLEX) Impact of term size
      => remove split
      => tost versus EMLA
      => Impact of length of training emla(30), emla(100), EMLA (200)

  There is no clear-cut conclusion ... we need more data samples :)

  - try ken2 as a substitute for ken + try ken(50) +   ken/1

// July 3rd
  - on hold => move to CTM

// July 14th
  - CIC : use randopt + tabu (either an index or 0 for no hour and -1 for no cycle)
  - need pattern(t,i) : tells if the pattern is there ...  (avoid stupid stuff)
  - IGNOBLE bug in the hyperbolic approximation (Trend) : add a contraint = alpha x (m - M) < 0

// August 1st / 2nd
  - enforce m < M
  - create hard bounds pb.trendMin/Max
  - enfore Trend coherence (ov - lv) * alpha
  - fixed a bug in term distance when m and M are the same
  => works well but slow !
  (resume when back on PC during the 12-24)




TODO (v0.1 : m1)


(a) additional test: ken1 and ken2(non random), ken3 (predict the end value)

(b) use our Measure object !! to get both the average score and the stdev :)

(c) forecasting
  - define a sample
    create the sampling methods
  - define the interface to emla
  - simple evaluation with sub-algebra  (measure the contribution of a factor)
  - Richer forecast : forecasting of factors


(d) CIC : try simple positive randopt

(e) close and move to v0.2 (September)



// v 0.2 ===================================================================================
// December 6th

main goals

(1) get fast methods for easy integration into iOS
(2) simplify code as much as possible
(3)  implement incremental methods for ken (iKen)
(4)  simple first version of CCI : Causality Contribution Insight

First steps:
(a) clean up code that is not used (taylor to iPhone) - done
(b) step by step  CLAIRE-to-SWIFT coherence check
    algebra - done => bugs in swift !!
	tserie - done => bugs in swift !!
(c) write better mutate ! (train trip)
(d) run on data samples - serious work at home !
(d) write first incremental algo : RIES, the MVP of EMLA !
    key words : incremental randomized evolutionary/optimized/hillclimbding
	RIES: Randomized Incremental Evolutionary Search !
 

// January 8th - resume !
code ITP :)

January 14 th : ITP works
 - compile
 - need to industrialize : exp(test file number, method number, iteration)  => create a log file
 - create a go file to make scripts : run all

January 15 th : complied & experiments
  /!\ : attention - we need to regroup key design parameters : hDist, maxDepth
  THE DATA SHOWS that trying to get a better match does not make sense.

February 4 th - 10 days marathon towards an answer (forecast is possible ?)
==========================================================================

   ABSOLUTE KEY POINT one should seperate
   - use EMLA to get the best match as a way to explain the data => to be used in CIC
   - use EMLA to forecast the next step

   Obviously if we can do both it is better ...

Questions list
==============

(1) importance of forecast (using a tracker correlate)
    - forecast = reality (test it)
    - forecast = average (seems ugly - check that it is trulyu that bad)
    - forecast = last few measures
    - (harder to do, but what we do on the phone) = use other formulas

    Answer: this is not the issue !! cf. FMODE results => saturday 3/2


(2) can we do better than average ?
    YES ! much better => simple constant + tracker gets better result (20%)
    what is important : pick best tracker + reduce the stdev
    clearly this is at the expense of fitting precision


(3) importance of distance heuristics (a) for learning (b) for evaluating
    - for evaluating : should we avoid the regression to the mean benefit
    - see if putting a penalty on excess deviation helps to get better forecast    

    /!\ we need to train to get solution that are as good as what one gets by hand

    to start : try to get solution => yves  / we need to reduce the variability by hand
    :) we get actually worse results (40%) on E1a
    BUT ... the first data set is very small

    methods: (a) penalty on stdev
             (b) weight differently errors if they are far from mean
             (c) penalty for overshooting



(4) run nice tables and store the result here !
    run (Exporiment 1 to 7, emla : (1:randopt, 2:yves, 3: trendit, 4:yves), n: param)
    with different distances => combination of method (hDist1 to 4) plus regret

ATTENTION : these are run with MAXDEPTH = 5 !

randopt (10)
      d = 3     d=5     d=6(50%)     d=7(300%)
E1    46%
E2    16%
E3    17%
E5    16%
E6
E7

table1-*
randopt(100)  [m = 1]
        d=3     d=5[98]  d=6(50)   d=7(300)
E1      32      45      49         46
E2      22      20      22         21
E3      19      18      18         18
E5      15      19      13         20
E6      27      26      15         23
E7      25      24      16         26

randopt(200) [m = 1]  / table1-200
        d=3     d=5[98]  d=6(50)   d=7(300)


E1      60%     53      47      53
E2      21      21      23      22
E3      17      17      20      16
E5      21      21      19      22
E6      24      24      15      24
E7      25      23      23      23





table2-*
Ries (100) [m = 2]
        d=3             d=5     d=6(50) d=7(300)
E1      37              38      30      30
E2      22              19      22      19
E3      15              14      17      18
E5      21              24      17      21
E6      20              22      17      20
E7      24 (0.029)      27      15      25


Ries (200) [m = 2]
        d=3             d=5     d=6(50) d=7
E1      48%             40      32      42
E2      20              20      20      22
E3      23              23      18      17
E5      23              20      16      21
E6      21              26      17      22
E7      24              18      20      21


table3 - comparison with Yves (m = 4) ================================
Yves = trendIt + simple corr
        d=3
        f     d      stdev
E1      26    0.10      7%
E2      19    0.05      6%
E3      15    0.04      3%
E5      13
E6      20
E7      23
============ this is quite good / make sure that Ries is not much worse =====


table4 : variations using Ries(100)
Note: this should be re-open this summer with more data
        d=6(30)                         d = 6(80)
        f       d       stdev
E1      30      0.04    9
E2      16      0.03    7
E3      17      0.01    8
E5      21
E6      16
E7      25      0.03    6


        d=7(200)
E1      30
E2      16
E3      17
E5      21
E6      21
E7      22
Bottom line : first sample is small => do not overplay

// maxDepth = 9

table1
41 / 19 / 18 / 17 / 22 / 22  avec d3
35 / 23 / 18 / 17 / 17 / 24  avec d6
39 / 21 / 10 / 16 / 22 / 20  avec d7

table2
46 /21 /29

// maxDepth = 7 (G*b)

conclusion (1) depth = 5 works best
           (2) we generate crazy terms
           (3) dist6 is a good candidate

2/12/2017
Hence
 (a) growLift creates a nice controled term
 (b) forget about cycles => creates over fitting - to be tuned later on
     we should only support cycles when they are very significant !
 (c) start with a simple version that could be enriched
 (d) randomized version of Yves


/!\ the DATA VIZ work in March will be the opportunity to better understand

r�sultats dans log
(1) growOpt & growLift marchent ... mais pas tr�s bien
    par ailleurs ils sont tr�s consommateurs en GC => � creuser
    cela reste une bonne piste, mais apr�s la DataViz

(2) pour l'instant, le mieux est Ries(100) avec une LIFTBOUND � 5 !
    utiliser la distance6 avec DEVFACTOR = 1.0 et DEVSTART � 0.02

ceci clot EMLA 0.2 le 12 F�vrier � 17 h 45 :)    


// EMLA v0.3 ========================================================================

// reopen on Saturday Jne 9th, 2018

saturday : import the new data files
sunday : change readFile @ tserie => read both new (CSV) and old format

// June 16th : implement a linear regression
- make a Matrix from a profile
- computes the linear regression on the matrix
- display the results
- compute some distance
create a file gauge.cl

// todo
- create claire code
- gather all example files
- rewrite the loader

// June 24th: implement tp (training protocol) + k-clustering
- done: in the file gauge.cl

// July 29th : time to get back and extend the algebra
- implemented wAverage and Combine in model.cl
- two methods for TSeries in algebra.cl

// August 4th
- completed CTP => with compilation and script
  avg perf of LR is 22.8%, WA is 20%, WA is 19.2%  !

Start ATP !
- created run + irun
- validated on Constant & Trend (not much to do)
- created fitness

// August 7th : start on Mac !
- fix test2.cl (bad file)

// August 11 - 12th
- implemented lift & fit for Hourly, weekly, average
- drop cycle from the code
- implemented lcut
decided to drop combine -> save for EMLA 0.4
implemented control(TS,i) -> visual inspection

// August 13th
- lift & score for cummulative & threshold -> poor result so far

// August 14th
- make Expriments follow the same logResult format and same protocole
- add a TFILTER variable to play with other trackers

// August 15th :
- change run(E*,i)
- creer irun
- change logResult
- compiler

// August 16th : move to MAC
... c'est parti on relance les exp�riences
A verifier : m�me r�glages gagnants que Emla 0.2
:) compile sans probl�me -> creer ries E* (batch) ->
   m3 -s 6 6 -f run E* i 100
   attention : bg stupide dans ld() => need a put(pb, fparams, ...)
   a regarder si on a du temps

// August 18th - August 26th (trip)
- implement trun(exp,i) -> use i-th tracker to learn
  KEY IDEA: for basic (Hour, Weekly, Avg) => we have 4 x 17 series
  create a  arun(exp,i)
-  Normalize fit between 0% and 100%
  implement various improvement on fit
     - standardized fit = dist6(lift*) versus liftConstant
- checked that all generic methods from algebra.cl are exended for both
- define the parametric test of one algebraic test, with simple lift, and with optim
- implement lamark = growLift
     (1) new random lift that only pick good operators
- discovered HUGE CLAIRE bugs with MacOS version caused by float slots

// August 28th
test de makeCorrelationNew => pas de difference (18.1 / 18.5)
//August 29th - Sept 5th
play with DEVSTART and DEVFACTOR => 1.0 and 0.02 seem the best

// Septembre => see written notes => Lamark does not work
what is left to explore (in a week)
  - tuning algebra distribution better than 40% Basic 30% Mix _(30) Derived
  - see if Ries is worth keeping
  - see if MAXDEPTH = 7 could work with another distance (DEVSTART / DEVFACTOR)

// resume on October 7th
play with 35/65 (E1) and 35/75

// October 20th - ROADEF paper week-end
// THIS IS IT : closure of EMLA v0.3
// WE HAVE OBTAINED GOOD RESULTS => CF PAPER THAT WILL BE SENT
// RANDOP IS THE WINNER

// January 29th  =============== START v0.4 ========================================
// this version will be used for the presentation to ROADEF
// we clean the Lamarck mess, remove unused code, and clean up
// we need to get results as least as good as before :)
// this version is the EMLA core for Knomee 2.0

// starts on January 29th : creates the directory

February 1-2 : create the code structure
- new agebra terms
- cleanup liftmode + variations

- tserie : fix the weekOfDay :)
- algebra : pickTracker checked.

- model : re-introduce Cycle and Split

- name change : distance = dist6 => evaluate
                dist3 => wDistance
                dist3 without weigth => distance


- introduce WEIGHTED option
    - key : WEIGHTED is an option when learning, but not clear when we do the final evaluation
    - testing WEIGHTED vs non-WEIGHTED

simplification : kill LiftMode + s=>simple

// february 2 : end of new code :) close PC / move to MAC

// 2/4/2019
gauge.cl - update LR compared to datathon
complete set of experiments done

// 2/5/2019
added findex in logResult(E,i,...) for filtering
fixed evol.cl -> optimize(Mix/Sum) so that the code with s?=false is run

// 2/7/2019
work on makeCycle - much better version
(1) subsample lox (local extremum) with a RATIO
(2) pick the ratio that gives the best distance
(3) show lox ! (debug function)
(4) need to estimate the distance without creating the algebra term
    cycleDistance(t,period, halfPeriod, lowDate, minValue, maxValue)
(5) write a function that minimize CD for a list of RATIOS
cf. written notes : works well on problem 5, ....

// 2/9-10
work on makeSplit
attention : simply adding a random in makeComposed reduces the performance
- introducing cycle reduces perf

// 2/11/2019
m40 works ! freeze it ... copy it to m43 !!!

// TODO for v0.4 - remaining 5 days
(0) cleanup m40 code for randopt and see that it still works :)
(1) run 24 samples
   - greart on E1 16.9 versus 17.5 on m40 et m41
   - E5 (DEPTH = 6) and E4 (DEPTH = 4) are good but not as good (17.00)
(2) play with cycle and split on 24 cycles
   E1c : split = 5% 17.33
   E1 : spit = 0%   17.05
(3) retry Ries
  - E2 : actually excellent
  - E200 / E201 : good but not as good (17.5/17)
(4) try a new tDistance for TERM x TSerie (same as triangle distance)
 - E1 is mediocre:
(5) play with kRandOpt
   - E3
(6) play with mutate => variation of kRandopt
(7) close it until end of 2018 :)
- r�cup�rer les variantes (m41) et les garder en comments

// 2/17/2019 : closure !

// December 24th 2022 : restart EMLA : move to CLAIRE4 ========================================

first step: get it to work ...

// idea stack for v0.5 ========================================================================

GENIAL : create simple profiles for other trackers and apply ML algos
=> this gives 80 time series to play with

HORRIBLE : there is a crazy bug in wDistance(Algebra.cl)
   the span is NOT the sum of the weights, fixed in Knomee

le paradoxe a r�soudre est de savoir comment utiliser les facteurs ...
pour l'instant les termes les plus robustes n'utilisent pas vraiment ces facteurs
Lamark a �chou� ... parce que trop brutal
On pourrait imaginer une solution en deux �tapes:
(1) RANDOPT => produit un terme de base
(2) ajouter des facteurs tres l�gers (avec une penalit� forte � l'�cart type)

TODO for next tests :
--------------------

- randomize the random generation (with a seed) - it seems quite sensitive


list of possible improvements (none of them seems so great in Knomee context)
=============================================================================


- work on mutate
  This would make more sense if we had a function to optimize to deliver robust terms !
  currently, any "deep optimization" produces terms which are not robust

- and : combination of two factors (logical and) - the or is implicit with linear algebra

- discounted time (precision matters less in the past)

- positive CIC (faster & simpler)
  measure the quality of the lift when we use the pattern
  however, this gives too much false positive

 - add derivative (inverse of cummulative) - strong variation as a trigger => left aside
 
 - find a way to introduce Cycle and Split when it makes sense

 - // Combines intoduces the AND combination of two tracker time serie as a possible explanation with a possible delay
Combine <: TSDerived(
   coeff:float,
   delay:Time,
   from2:TSConstant)

[nth(t:Combine,x:Time) : float
  -> let y := max(TimeOrigin,x - t.delay),
         ts1 := t.from.value, z1 := ts1[y],
         avg1 := ts1.avgValue,
         ts2 := t.from2.value, z2 := ts2[y],
         avg2 := ts2.avgValue in
       (//[5] nth(~S) -> ~A / ~A; nth(~S) -> ~A / ~A (* ~A) // ts1,z1,avg1,ts2,z2,avg2,t.coeff,
        t.coeff * (z1 - avg1) * (z2 - avg2)) ]

self_print(t:Combine) : void
  -> printf("AND[~F2](~SX~S+~A)",t.coeff,t.from,t.from2,t.delay)

