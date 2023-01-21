// +--------------------------------------------------------------------------------------------+
// |  EMLA                                                                                      |
// |  Evolutionary Machine Learning Agents                                                      |
// |  Copyright (C) Yves Caseau 2016-2019                                                       |
// +--------------------------------------------------------------------------------------------+


// *********************************************************************************************
// *  Table of content                                                                         *
// *   1. Tseries                                                                              *
// *   2. Profiles                                                                             *
// *   3. Algebra                                                                              *
// *   4. EMLA Pool                                                                            *
// *   5. Utilities                                                                            *
// *********************************************************************************************


// we measure all time from 
Time :: integer
TimeOrigin:Time :: 0                // setup when we read the time series
Version:float :: 0.4                      // last update on January 29th, 2019

// trace variable => set to 5 to get better perfs !
DEBUG:integer :: 5
TRACE:integer :: 5
CHECK:boolean :: false              // one simple way to cut all assertion checking

/* v0.4 macro option (look at v0.41 for execution)
WEIGHTED:boolean :: true
SIMPLE:boolean :: false         // simple means that TSerie distance is Euclidean
weightSum(a:Time, b:Time, n:integer) : float  => (#if WEIGHTED float!(b - a) else float!(n))
*/

// new in v0.4 : play with the introduction of two terms
SPLIT%:integer :: 0
CYCLE%:integer :: 5

// e.options can mean anything - we print a reminder string
[oString(i:integer) : string 
    ->  "CYCLE%=" /+ string!(i) ]

// *********************************************************************************************
// *   1. Tseries                                                                              *
// *********************************************************************************************

// a time serie
// dates is an ordered list
TSerie <: ephemeral_object(
  count:integer = 0,
  dates:list<Time>,
  values:list<float>,
  // computed slots
  avgValue:float = 0.0,
  minDate:Time = 0,
  minValue:float = 0.0,
  maxDate:Time = 0,
  maxValue:float = 0.0)

[self_print(x:TSerie) : void  
  -> printf("<TSerie:~A>",x.count)]

[close(t:TSerie) : TSerie
  -> minMax(t), t]

// insert a measure into a time serie, does not assume order
[insert(t:TSerie,d:Time,x:float) : void
  -> let i := 1, n := t.count in
       (for i in (1 .. n)
         (if (t.dates[i] > d) (nth+(t.dates,i,d),nth+(t.values,i,x), t.count :+ 1, break())),
        if (n = t.count)
           (nth+(t.dates,n + 1,d), nth+(t.values,n + 1,x), t.count :+ 1)) ]


// min max analysis
[minMax(t:TSerie) : void
  -> let m := 1e9, M := -1e9, tm := 0, tM := 0, s := 0.0 in
       (for i in (1 .. t.count)
          let v := t.values[i], d := t.dates[i] in
            (s :+ v,
             if (v < m) (m := v, tm := d),
             if (v > M) (M := v, tM := d)),
        t.avgValue := s / float!(t.count),
        t.minValue := m, t.minDate := tm,
        t.maxValue := M, t.maxDate := tM) ]


// computes the value of the TSerie at any point (linear interpolation)
[nth(t:TSerie,x:Time) : float
  -> let i := 2, n := t.count in
       (if (x <= t.dates[1]) t.values[1]
        else if (x >= t.dates[n]) t.values[n]
        else (while (x > t.dates[i])
                    (i :+ 1,
                     if (i > n) return(t.values[n])),
              if (t.dates[i] = x) t.values[i]
              else let d1 := t.dates[i - 1], d2 := t.dates[i],
                       v1 := t.values[i - 1], v2 := t.values[i] in
                     (v1 + (v2 - v1) * float!(x - d1) / float!(d2 - d1)))) ]

// other methods are found in the TSeries.cl file

// *********************************************************************************************
// *   2. Profiles                                                                             *
// *********************************************************************************************

// a profile is what we work on
// 2 to 4 TSeries
// list of tracker names
Profile <: thing(
   target:TSerie,
   trackers:list<TSerie>,
   tags:list<string>)

// instantiation (default values)
[close(p:Profile) : Profile
  -> p.target := TSerie(),
     p.trackers  := list<TSerie>(),
     for i in (2 .. length(p.tags)) p.trackers :add TSerie(),
     p ]




// *********************************************************************************************
// *   3. Algebra                                                                              *
// *********************************************************************************************

// v0.3 new algebra

// TSTerm is a the generic class, but instanciation follows this grammar:
// TSTerm :: TSBasic | TSComposed | Sum(TSTerm, TSDerived)
// TSBasic :: Trend | Weekly | Hourly | WAverage
// TSComposed :: Mix(TSTerm, TSTerm)
// TSDerived   :: Correlate | Cummulative | Threshold


// root of the Algebra of terms that represent a time serie
// Assumption: all terms use the same time sampling
// each term is defined through the nth method T[x] = value of T at point x
TSTerm <: ephemeral_object()

// constant time series may be introduced
TSConstant <: TSTerm(
   tag:string,             // a designation (for instance a profile tracker)
   index:integer = 0,
   value:TSerie)

[nth(t:TSConstant,x:Time) : float
  -> t.value[x]]

self_print(t:TSConstant) : void -> print(t.tag)

// a common root for basic terms
TSBasic <: TSTerm()

// the first TST is a trend : linear approximation with a max (hyberbola)
Trend <: TSBasic(
   originValue:float,            // value at time origin
   limitValue:float,             // asymptotic value
   alpha:float)                  // derivative at 0

// this hyperbolic approximation uses (1/ 1 + x) as the base (start at 1, limit = 0, derivative = -1)
// watch out: when automatic tuning, cannot change the signs
// m > M => alpha negative
// this garantees that the function is bounded (between original value and limitValue)
[nth(t:Trend,x:Time) : float
   -> let y := max(0.0, float!(x - TimeOrigin)),
          m := t.originValue, M := t.limitValue, delta := M - m in
        (// check(t,"nth"),                 // debug : remove later
         if (delta = 0.0) M
         else (M - (delta / (1.0 + ((y * t.alpha) / delta))))) ]

self_print(t:Trend) : void 
  -> printf("T[~F2-~F2/~F2]",t.originValue,t.limitValue, t.alpha)

// check the constraint
[check(t:Trend,s:string) : void
  -> let m := min(t,1), M := max(t,1), v := t.originValue in
       (if (v < m | v > M) error("origin(~S)= ~A out of (~A - ~A) in ~A",t,v,m,M,s)),
     let m := min(t,1), M := max(t,1), v := t.limitValue in
       (if (v < m | v > M) error("limit(~S)= ~A out of (~A - ~A) in ~A",t,v,m,M,s)),
     if (t.alpha * (t.originValue - t.limitValue) > 0.0)
        error("~S is not a possible Trend in ~A",t,s) ]

// hourly is a periodic pattern based on the hour (6 values for 4hours period)
SLICE :: 6
Hourly <: TSBasic(
    values:list<float>,
    slice:integer = SLICE)

// to which slice is assigned a time	
[slice(t:Hourly,x:Time) : integer
  -> let h := hour(x) / (24 / t.slice) in 1 + h ]
[nth(t:Hourly,x:Time) : float -> t.values[slice(t,x)] ]

self_print(t:Hourly) : void -> printf("H(~F2-~F2)",t.values[1],t.values[3])
// debug: self_print(t:Hourly) : void -> printf("H(~A)",t.values)

// weekly is a periodic pattern based on the day of the week (7 values)
Weekly <: TSBasic(
    values:list<float>)

[slice(t:Weekly,x:Time) : (1 .. 7) -> weekDay(x) + 1]
[nth(t:Weekly,x:Time) : float -> t.values[slice(t,x)]]

self_print(t:Weekly) : void -> printf("W(~F2)",t.values[1])

// weighted average
// replaces Cycle in EMLA 0.3 ! much simpler ...
// wAv with small history are useful for slow variations (e.g. weight)
Average <: TSBasic(
        from:TSConstant,                // always from the main tracker (target = first from profile)
        history:integer = 3,                // max number of measures from the past
        coeff:float = 1.0)              // discount in the past

[nth(t:Average,x:Time) : float
   -> wAvgValue(t.from.value,t.history,t.coeff,x) ]

self_print(t:Average) : void
    -> printf("wAvg[~S](~A,~F2)",t.from,t.history,t.coeff)

// Biorythm cycles linear approximation (not necessarily regular => two points)
// re introduced in v0.4 (not necessarily used)
Cycle <: TSBasic(
     period:Time,           // period in day
     lowDate:Time,          // time 
     halfPeriod:Time,       // distance between highDate and lowDate
     minValue:float,        // min value at low dates
     maxValue:float)        // max value at high dates

[nth(t:Cycle,x:Time) : float
   -> let p := t.period, p1 := t.halfPeriod, p2 := p - p1,
          m := t.minValue, M := t.maxValue, y := cycleMod(x,p) in
        (if (y < p1) m + (M - m) * (float!(y) / float!(p1))
         else M + (m - M) * (float!(y - p1) / float!(p2))) ]


self_print(t:Cycle) : void 
    -> printf("C[~A](~F2,~F2)",day(t.period),t.minValue,t.maxValue)

// Composed terms -------------------------------------	

TSComposed <: TSTerm(
   t1:TSTerm,
   t2:TSTerm)
   
// Split is an horizontal combination of two TS terms with a cutoff date
// v0.4 Split is reintroduced but only used when t1 is a constant (or equivalent)
// should be used sparingly because the risk of overfitting is obvious
Split <: TSComposed(
   cutoff:Time)

[nth(t:Split,x:Time) : float
  -> (if (x <= t.cutoff) t.t1[x] else t.t2[x])]

self_print(t:Split) : void
  -> printf("Split[~A](~S,~S)",dateString(t.cutoff),t.t1,t.t2)


// linear combination of two terms
Mix <: TSComposed(
   coeff:float)

[nth(t:Mix,x:Time) : float
  -> t.coeff * t.t1[x] + (1.0 - t.coeff) * t.t2[x]]

self_print(t:Mix) : void
  -> printf("Mix[~F2](~S,~S)",t.coeff,t.t1,t.t2)

// special terms that injects constant time series into the algebra ----------------------------
// TSderived terms are obtained from a TSerie that represents a tracker
// they are implicitly normalized so that their average is 0.0
TSDerived <: TSTerm(
   from:TSConstant)            // all derived from a Tserie : from


// summing a TSTerm and a correction which is a derived
Sum <: TSTerm(
	t1:TSTerm,
	t2:TSDerived)
	
[nth(t:Sum,x:Time) : float -> t.t1[x] + t.t2[x]]

self_print(t:Sum) : void -> printf("(~S + ~S)",t.t1,t.t2)

// Correlate intoduces a tracker time serie as a possible explanation with a possible delay
Correlate <: TSDerived(
   coeff:float,
   delay:Time)

[nth(t:Correlate,x:Time) : float
  -> let y := max(TimeOrigin,x - t.delay),
         ts := t.from.value, z := ts[y],  
         avg := ts.avgValue in
       (//[5] nth(~S) -> ~A / ~A (* ~A) // ts,z,avg,t.coeff,
        t.coeff * (z - avg)) ]

self_print(t:Correlate) : void
  -> printf("Cor[~F2](~S+~A)",t.coeff,t.from,t.delay)

// this is more elaborate : use the integral of the input time serie as the possible explanation
// memory is the span over which we interate, 
// in v0.1, we forget about delay (take a longer period if needed :))
Cummulative <: TSDerived(   
   memory:Time,             // time span for cummulative integration
   coeff:float)            // multiplicative factor

[nth(t:Cummulative,x:Time) : float
  -> let y := max(0,x - t.memory),
         ts := t.from.value,
         avg := ts.avgValue in
       (t.coeff * (integral(ts, y, x) - avg * (x - y))) ]
       
self_print(t:Cummulative) : void
  -> printf("Cum[~F2](~Ss~A)",t.coeff,t.from,t.memory)

// this is a treshold function (0 or 1 according to a level)
Threshold <: TSDerived(
    coeff:float,
    level:float,         // relative treshold from minValue
    delay:Time)          // time delay between cause and effect

[nth(t:Threshold,x:Time) : float
  -> let y := max(TimeOrigin,x - t.delay),
         ts := t.from.value, z := ts[y],
         avg := ts.avgValue,
         tv := t.level * max(ts.maxValue -  avg, avg - ts.minValue) in
       (if (abs(z - avg) > tv) (z - avg) * t.coeff else 0.0) ]

self_print(t:Threshold) : void
  -> printf("Thresh[>~F1*~F2](~S+~A)",t.level,t.coeff,t.from,t.delay)


/*  // create a TSeries from a TSTerm (following a profile)
[serie!(p:Profile,t:TSTerm) : TSerie
  -> let l1 := p.target.dates in
       TSeries(count = length(l1),
               dates = l1,
               values = list<float>{t[x] | x in l1}) ] */

// ignoble - debug
[ck(a:TSTerm,s:string) : void
 -> if CHECK
       case a (Trend check(a,s),
               TSComposed (ck(a.t1,s), ck(a.t2,s)),
               Sum ck(a.t1,s),
               any false) ]


// check the min < Max constraint
[ck2(a:TSTerm,m:float,M:float,p:integer,s:string) : void
  -> if (CHECK & m > M)
      error("m=~A > M=~A for ~S:~A in ~A",m,M,a,p,s) ]
            
// *********************************************************************************************
// *   4. EMLA Manager                                                                         *
// *********************************************************************************************

// EMLA is the global object that represents the EMLA meta parameter for search
EMLA <: thing(
  maxDepth:integer = 5,     // avoid overfitting with small terms !
  diversity:float = 0.0,    // to be tuned: add a bonus for diversity in pool
  twoLoops:integer = 3,     // number of twoOpt loops
  oneLoops:integer = 1,     // number of oneOpt loops
  liftLoops:integer = 10)   // reinforce lift

// experiments - associated to a result file
// to enrich :)
Experiment <: thing(
  findex:integer = 1,           // index of the data file
  option:integer = 0,           // option parameter that can mean anything (e.g. CYCLE%)
  fparam:float = 0.0,           // float parameter used for WAvg and makeCorrelation (0.3 seems the best)
  avgDist:float,                // average distance (term to serie)
  avgDelta:float,               // average forecast error
  avgVolatility:float,          // for comparison: avg dist to previous average
  avgDev:float,                  // average standard deviation of term
  avgDepth:float,
  runTime:integer,              // run time per iteration
  count:float = 0.0)            // used when loading an experiment file



// control training experiments
CExperiment <: Experiment(
        control:integer = 3,    // 1 (LR), 2 (cluster), 3 (weighted average), 4 (ARMA)
        param:integer = 1       // parameter that may be passed to the ago (e.g. size of cluster)
        )


// algebra training experiments
AExperiment <: Experiment(
        control:integer = 1,      // tell which algebra
        nOpt:integer = 0,         // add Optim loops after lift
        avgFit:float              // average fitness score
        )

// iterative training experiments
IExperiment <: Experiment(
        emla:integer,                 // which emla to run (should be an object)
        maxDepth:integer = 5,         // max size of terms
        nOpt:integer = 3,             // nOpt dans boucle interne (randOpt)
        nLoop:integer = 100,          // number of optim loop (NEW)
        fcut:float = 1.0              // global control over cut parameters
    )

CUTFACTOR:float := 1.0       // avoids the huge GC BUG

// our usual suspect : key object with global variable
Problem <: thing(
  emla:EMLA,                    // vector of EMLA parameters
  experiment:Experiment,        // the current experiment if one is run
  profile:Profile,              // this is the KNOMEE profile, can be updated innternally
  timeScale:list<float>,        // shared => keep it once (float values for printing)
  timeList:list<Time>,          // the x_i point (dates) are shared for all terms & series
  fparam:float = 0.0 ,          // a global constant that may be used by LIFT (for tuning purposes)
  fnames:list<string>,          // list of test file names
  origin:Profile,               // original profile from the test file
  // EMLA variables
  target:TSerie,                // target TSerie
  optTerm:TSTerm,               // the TS term that is being optimized
  cursat:float,                 // the distance between the time serie and its algebraic approximation
//  fcut:float = 1.0,             // global control over cut parameters
  // used in lift: random term generation
  tabu:integer = 0,             // -2 for W, -1 for H, and i>0 for a tracker(index)
  trendMin:float = 0.0,         // when we read a TSerie, we set hard bounds for trend generation
  trendMax:float = 0.0,         //
  // Used by mutation alorithm : make it defeasible + tabu on OPT
  muteT:TSTerm,
  muteI:integer,
  muteV:float,
  fcut:float = 1.0)            // CLAITE BIG APPLE BUG: where I put the float slots seem to matter a lot

// classical CLAIRE pattern - pb is our problem representation objetc
pb :: Problem()


// *********************************************************************************************
// *   5. Utilities                                                                            *
// *********************************************************************************************

// fib test on mac & pc
[fib(n:integer) : integer
  -> if (n < 2) 1 else fib(n - 1) + fib(n - 2) ]

[g(n:integer)
  -> time_set(),
     let m := fib(n) in
       (time_show(),
        printf("fib(~A) = ~A\n",n,m)) ]


// random between two floats
[random(a:float,b:float) : float
  -> a + (b - a) * (float!(random(1000000))/ 1000000.0) ]

[random?(n:integer) : boolean => (n < random(100)) ]

// cycle mod a is true modulo
[cycleMod(x:integer,p:integer) : integer
  -> let y := (x mod p) in (if (y < 0) y + p else y) ]


// computes the variation a short list
[stdeviation(l:list<float>) : float
  -> let sum := 0.0, sum2 := 0.0, n := length(l) in
       (for i in (1 .. n)
           (sum :+ l[i],
            sum2 :+ l[i] ^ 2.0),
        let mean := sum / float!(n),
            var := (sum2 / float!(n)) - (mean ^ 2.0) in
              (sqrt(var) / mean)) ]



// ======= linear regression ==============================================  v0.6

// <start>  code fragment, origine = CGS, version = 1.0, date = Avril 2010
// stolen from mms v0.1

// input = lists of Xi and Yi, returns a triplet (slope, constant factor, deviation)
[linearRegression(lx:list<float>,ly:list<float>) : list<float>
  -> let sx := 0.0, sy := 0.0, ssx := 0.0, n := length(lx), sxy := 0.0,
         a := 0.0, b := 0.0, sv := 0.0, av_x := 0.0, av_y := 0.0, av_xy := 0.0 in
       (assert(length(ly) = n),
        for i in (1 .. n)
          let x := lx[i], y := ly[i] in
             (sx :+ x, ssx :+ (x * x), sy :+ y, sxy :+ (x * y)),
        av_x := sx / n, av_y := sy / n, av_xy := sxy / n,
        a :=  (av_xy - av_x * av_y) / ((ssx / n) - av_x * av_x),
        b := av_y - a * av_x,
        for i in (1 .. n)
          let x := lx[i], y := ly[i], v := sqr(y - (a * x + b)) in (sv :+ v),
        sv :/ (n - 2),             // ref: Wikipedia on linear regression
        list<float>(a,b,sqrt(sv))) ]

// pseudo equal for floats
== :: operation(precedence = precedence(=))
==(x:float,y:float) : boolean
  -> (abs(x - y) < (abs(x) + abs(y) + 1.0) * 1e-2)

[testReg()
 -> let l := linearRegression(list<float>(1.0,2.0,3.0), list<float>(0.0,0.0,0.0)) in
        assert(l[1] == 0.0 & l[2] == 0.0 & l[3] == 0.0),
    let l := linearRegression(list<float>(1.0,2.0,3.0), list<float>(0.0,1.0,2.0)) in
        assert(l[1] == 1.0 & l[2] == -1.0 & l[3] == 0.0),
    let l := linearRegression(list<float>(1.0,2.0,3.0), list<float>(3.0,5.0,7.0)) in
        assert(l[1] == 2.0 & l[2] == 1.0) ]

// <end>

