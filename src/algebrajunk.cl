// +--------------------------------------------------------------------------------------------+
// |  EMLA                                                                                      |
// |  Evolutionary Machine Learning Agents                                                      |
// |  algebra.cl                                                                                |
// |  Copyright (C) Yves Caseau 2016-2018                                                       |
// +--------------------------------------------------------------------------------------------+

// algebraic operations : methods that are defined for each term of the algebra

// *********************************************************************************************
// *  Table of content                                                                         *
// *     Part 1: dCopy, distance & depth                                                       *
// *     Part 2: algebra term modification interface                                           *
// *     Part 3: lift & project                                                                *
// *     Part 4: mutate                                                                        *
// *     Part 5: cross                                                                         *
// *********************************************************************************************


// *********************************************************************************************
// *  Table of content                                                                         *
// *     Part 1: dCopy, distance & depth                                                       *
// *     Part 2: algebra term modification interface                                           *
// *     Part 3: lift & project                                                                *
// *********************************************************************************************


// ********************************************************************************************* 
// *     Part 1: dCopy, distrance & depth                                                      *
// ********************************************************************************************* 

// deepCopy
[dCopy(a:TSTerm) : TSTerm
  -> case a
      (Monthly  Monthly(slice = a.slice,values = copy(a.values)),
       Weekly   Weekly(values = copy(a.values)),
       Trend    Trend(originValue = a.originValue, limitValue = a.limitValue, alpha = a.alpha),
       Average          Average( from = a.from, coeff = a.coeff, history = a.history),
       Correlate        Correlate( from = a.from, coeff = a.coeff, delay = a.delay),
       Mix      Mix( coeff = a.coeff, t1 = dCopy(a.t1), t2 = dCopy(a.t2)),
       Sum      Sum(t1 = dCopy(a.t1), t2 = dCopy(a.t2)),
       any error("copy not implemented for ~S", a)) ]

// distance between two TSTerms
[dist(a:TSTerm, b:TSTerm) : float
 -> case a
     (TSBasic  case b
		    (TSBasic slotDist(a,b),
			 any dist(b,a)),
      Mix      case b
		   (Mix (a.coeff * dist(a.t1,b.t1) + (1.0 - a.coeff) * dist(a.t2,b.t2)),
		    any (a.coeff * dist(a.t1,b) + (1.0 - a.coeff) * dist(a.t2,b))),
      Sum      case b
		   (Sum (dist(a.t1,b.t1) + slotDist(a.t2,b.t2)),
		    any 0.5 + dist(a.t1,b)),
      any 1000.0)]
			
// when a & b are of the same class,we use parametric slot access 
// for basic & derived term (sum of parameter distances)
[slotDist(a:TSTerm,b:TSTerm) : float
  ->  if (a.isa = b.isa)
        let d := 0.0 in
	    (for i in (1 .. nSlot(a))
		   let v1 := read(a,i), v2 := read(b,i) in
		     d :+ (abs(v1 - v2) / (0.01 + abs(min(a,i) - max(a,i)))),
		 d / float!(nSlot(a)))
	   else 1.0 ]

// this is a simple complexity measure
[depth(t:TSTerm) : integer
  -> case t (TSBasic 1,
             TSComposed 1 + depth(t.t1) + depth(t.t2),
	     Sum 2 + depth(t.t1),
             any 1000) ]

// ---------------------------- critical : distance to TS ------------------------

// distance between a term (forecast) and the target tserie
// THIS IS THE BEST DEFAULT SOLUTION
[distanceSquare(a:TSTerm,t:TSerie) : float
  -> let n := t.count, ld := t.dates, s := 0.0,
         span := ld[n] - ld[1], delta := t.maxValue - t.minValue in
       (if (delta = 0.0) delta := 1.0,         // avoid error in stupid case
        for i in (1 .. n)
            let w := (if (i = 1) (ld[2] - ld[1]) else ld[i] - ld[i - 1]) in
            s :+ w * sqr((a[ld[i]] - t.values[i]) / delta),
        s / span)  ]


// distance 6 adds a penalty for large deviation
// this is called re-normalisation
DEVFACTOR:float :: 0.0 // was 1.0
DEVSTART:float :: 0.02
[distance(a:TSTerm,t:TSerie) : float
  -> distanceSquare(a,t) + DEVFACTOR * max(0.0,stdev%(a) - DEVSTART) ]



// *********************************************************************************************
// *     Part 2: algebra term modification interface                                           *
// *********************************************************************************************

// this part gives a generic access to terms numerical slots that may be tuned through local
// optimization - this is taken from GTES code :)
// nSlot(t/TST) : number of slot
// min(t/TST, i) : min value that is accepter
// max(t/TST, i) : max value that is accepted
// read(t,i) : reads the value
// write(t,i,v) : writes the value (float between min & max)

TAG :: integer

// Trend term :  originValue, limitValue, alpha
// notice that we enforce the constraint origin > limit => alpha < 0 &
//                                       origin < limit =>  alpha > 0
TrendSlots :: list<property>(originValue, limitValue, alpha)
[nSlot(t:Trend) : TAG ->  3]

// cf code of hyperbolic approximation in model.cl .. constraint ensures boundedness
[min(t:Trend, i:TAG) : float
  -> if (i = 1) (if (t.alpha < 0.0) t.limitValue else pb.trendMin)
     else if (i = 2) (if (t.alpha > 0.0) t.originValue else pb.trendMin)
     else (if (t.originValue < t.limitValue) 0.0 else -1.0)]

[max(t:Trend, i:TAG) : float
  -> if (i = 1) (if (t.alpha > 0.0) t.limitValue else pb.trendMax)
     else if (i = 2) (if (t.alpha < 0.0) t.originValue else pb.trendMax)
     else (if  (t.originValue > t.limitValue) 0.0 else 1.0)]

[read(t:Trend,i:TAG) : float -> (read(TrendSlots[i],t) as float)]
[write(t:Trend,i:TAG,v:float) : void -> write(TrendSlots[i],t,v)] 
[label(t:Trend,i:TAG) : symbol -> TrendSlots[i].name]
 
// Monthly term :  SLICE values
[nSlot(t:Monthly) : TAG ->  SLICE]
[min(t:Monthly, i:TAG) : float -> pb.target.minValue ]
[max(t:Monthly, i:TAG) : float -> pb.target.maxValue ]
[read(t:Monthly,i:TAG) : float -> t.values[i]]
[write(t:Monthly,i:TAG,v:float) : void -> t.values[i] := v]
[label(t:Monthly,i:TAG) : string -> string!(i)]
 
// Weekly term :  7 values
[nSlot(t:Weekly) : TAG ->  7]
[min(t:Weekly, i:TAG) : float -> pb.target.minValue ]
[max(t:Weekly, i:TAG) : float -> pb.target.maxValue ]
[read(t:Weekly,i:TAG) : float -> t.values[i]]
[write(t:Weekly,i:TAG,v:float) : void -> t.values[i] := v]
[label(t:Weekly,i:TAG) : string -> string!(i)]
 
// Average : period, lowDate, halfPeriod,minValue,MaxValue
AverageSlots :: list<property>(history,coeff)
[nSlot(t:Average) : TAG ->  2]
[min(t:Average, i:TAG) : float
  -> if (i = 1) 3.0
    else 0.5]
[max(t:Average, i:TAG) : float
  -> if (i = 1 ) 10.0
     else 1.0]
[read(t:Average,i:TAG) : float
   -> let v := read(AverageSlots[i],t) in (if (i = 1) float!(v) else v) ]
[write(t:Average,i:TAG,v:float) : void
   -> write(AverageSlots[i],t,(if (i = 1) integer!(v) else v)) ]
[label(t:Average,i:TAG) : symbol -> AverageSlots[i].name]

// Sum, Mix are special  -> call optimize on the inner thing
// which implies that it is a parametric optimize (a piece of the tree while we evaluate on the 
// whole term)
// we could do it with a dynamic renumering of inner slots ... but too heavy versus recursive opt*

// Mix : coeff, 
[nSlot(t:Mix) : TAG ->  1]
[min(t:Mix, i:TAG) : float -> 0.0]
[max(t:Mix, i:TAG) : float -> 1.0]
[read(t:Mix,i:TAG) : float -> t.coeff]
[write(t:Mix,i:TAG,v:float) : void -> t.coeff := v]
[label(t:Mix,i:TAG) : symbol -> coeff.name]


// ------------------- generic OPT kit for derived
// Corr : coeff, delay
CorrSlots :: list<property>(coeff,delay)
[nSlot(t:Correlate) : TAG ->  2]
[min(t:Correlate, i:TAG) : float -> 0.0]
[max(t:Correlate, i:TAG) : float
  -> if (i = 1) 1.0 else float!(5 * DAY) ]
[read(t:Correlate,i:TAG) : float
    -> let v := read(CorrSlots[i],t) in (if (i = 1) (v as float) else float!(v)) ]
[write(t:Correlate,i:TAG,v:float) : void
    -> write(CorrSlots[i],t, (if (i = 2) integer!(v) else v)) ]
[label(t:Correlate,i:TAG) : symbol -> CorrSlots[i].name]


// *********************************************************************************************
// *     Part 3: lift & project                                                                *
// *********************************************************************************************

// LIFT DEBUG
LDB:integer :: 5

// projection : produce a TSerie
[tserie(a:TSTerm) : TSerie
  -> TSerie(count = length(pb.timeList),
            dates = pb.timeList,
	    values = list<float>{ a[x] | x in pb.timeList}) ]

// lift is implemented with makeX(T) method
// makeX(T) returns a X TST that is similar to T (a TSerie)
// we follow the algebra
	// TSTerm :: TSBasic | TSComposed | Sum(TSTerm, TSDerived)
// TSBasic :: Trend | Monthly | Hourly | Average
// TSComposed :: Mix(TSTerm, TSTerm)
// TSDerived   :: Corr

// lift(T,n) is the external method that is exposed to the rest of the module
// makeTST, make<X> are the internal spepiaclized methods
			 
// (1) lift a basic term ------------------------------		 


// dumb : create a constant [DEBUG]
[makeConstant(t:TSerie) : Trend
  -> let v := t.avgValue in
        Trend(originValue = v,  limitValue = v, alpha = 0.0) ]


// create a Trend from a TS
// do a linear regression, then an hyperbolic correction
// constraint is satisfied by construction
[makeTrend(t:TSerie) : Trend
  -> let lr := linearRegression(t),
         ov := lr[2] + pb.timeScale[1] * lr[1],
         lv := (if (lr[1] > 0.0) t.maxValue else t.minValue),
         m := pb.trendMin, M := pb.trendMax in
       Trend(originValue = min(M,max(m,ov)),
             limitValue = min(M,max(m,lv)),
             alpha = lr[1]) ]

// fitWfitTrend(t) : is there a global trend in the data
// could be a test on alpha
[fitTrend(t:TSerie) : float
  -> let a := makeTrend(t),
         b := makeConstant(t) in
    (distance1(a,t) / distance1(b, t)) ]

// lift a hourly : create a table => collect stats => average value
[makeHourly(t:TSerie) : Hourly
  -> let table := list<float>{0.0 |i in (1 .. SLICE)},
         cnt := list<integer>{0 |i in (1 .. SLICE)},
         avg := t.avgValue,
         a := Hourly(slice = SLICE) in
        (for i in (1 .. t.count)
            let j := slice(a,t.dates[i]) in
                (table[j] :+ t.values[i],
                 cnt[j] :+ 1),
         a.values := list<float>{deduce(cnt[i],table[i],t.count,avg) |i in (1 .. SLICE)},
         a) ]
				 
// useful heuristic for an approximate average (with samples that may be too small)
// n values => sigma out of m samples with average avg
HCUT:integer := 10
[deduce(n:integer,sigma:float,m:integer,avg:float) : float
  -> if (n = 0) avg
     else if (n > HCUT) (sigma / n)
     else (sigma + (float!(m) * avg / float!(HCUT))) / (float!(n) + (float!(m) / float!(HCUT))) ]


// fitHourly(t) : combination of umber of values and stdev
FH:integer :: 1
[fitHourly(t:TSerie) : float
  -> let a := makeHourly(t),
         b := makeConstant(t) in
      distance1(a,t) / distance1(b, t) ]


// lift a Weekly : very simular to Hourly   -----------------------------------------
[makeWeekly(t:TSerie) : Weekly
  -> let table := list<float>{0.0 |i in (1 .. 7)},
         cnt := list<integer>{0 |i in (1 .. 7)},
	    avg := t.avgValue,
         a := Weekly() in
        (for i in (1 .. t.count)
            let j := slice(a,t.dates[i]) in
                (table[j] :+ t.values[i],
                 cnt[j] :+ 1),
		 a.values := list<float>{deduce(cnt[i],table[i],t.count,avg) |i in (1 .. 7)},
         a) ]

// fitWeekly(t) : combination of umber of values and stdev
[fitWeekly(t:TSerie) : float
  -> let a := makeWeekly(t),
         b := makeConstant(t) in
    (distance1(a,t) / distance1(b, t)) ]  //  * smooth(a) ]


// lift a weighted average - simply tries 3,6,10
LIST_AVERAGE :: list<integer>(3,6,10)
// (2,3,4,6,10) : worse ! better correlation but no prediction robustness
[makeAverage(t:TSerie) : Average
  -> let best_d := 0, best_v := 0.0 in
       (for d in LIST_AVERAGE
           let v := matchSerie(t,wAvg(t,d,1.0)) in
             (//[5] d=~A gives ~A // d,v,
              if (v > best_v) (best_v := v, best_d := d)),
        //[LDB] -- best history length = ~A mins gives match ~A // best_d, best_v,
        Average(from = TSConstant(tag = pb.profile.tags[1], index = 0, value = t),
                history = best_d,
                coeff = 1.0)) ]

// fit is simple (smaller the better)
[fitAverage(t:TSerie) : float
  -> let a := makeAverage(t),
         b := makeConstant(t) in
      (distance1(a,t) / distance1(b, t)) ]


// (2) lift a composed term --------------------------------

// we use a smart approach : the target for b is not t but (t - la) / (1 - l)
// thus the error made by t -> a may be compensated
[makeMix(t:TSerie, n:integer, liftMode:integer) : Mix
  -> let l := random(0.4,0.8),
         a := makeTST(t, (n - 1) / 2, liftMode),
	 nt := multiply(difference(t,multiply(tserie(a),l)),1.0 / (1.0 - l)),
	 b := makeTST(nt, (n - 1) / 2, liftMode) in
     Mix(coeff = l, t1 = a, t2 = b) ]



// (3) lift a derived term ------------------------------		 

// make a Sum : list a TST then lift a derived for the difference
[makeSum(t:TSerie,n:integer,liftMode:integer) : Sum
  -> let a := makeTST(t,n - 2,liftMode),
         b := makeDerived(difference(t,a),liftMode) in
	 Sum(t1 = a, t2 = b) ]

// make a derived : 50% Corr, 25% Cum, 25% 
[makeDerived(t:TSerie,liftMode:integer) : TSDerived
  -> let n := random(100) in
       (if (n < 40) makeCorrelation(t,liftMode)
        else if (n < 80) makeCummulative(t,liftMode)
        else makeThreshold(t,liftMode)) ]

// controlled version : n tells which algebra term should be used
CORR_CUT:float := 0.0
CORR_TRH:float := 10000.0
[controlSum(t:TSerie,n:integer) : TSTerm
 -> if (bestMatchValue(t) < CORR_CUT) makeConstant(t)
    else if (n = 3 & fitThreshold(t) > CORR_TRH) makeConstant(t)
    else let a := makeConstant(t),
            b := (if (n = 1) makeCorrelation(difference(t,a),0)
                  else if (n = 2) makeCummulative(difference(t,a),0)
                  else makeThreshold(difference(t,a),0)) in
        Sum(t1 = a, t2 = b) ]

// make a Correlation term
// we would like to explain t (target serie, average is expected to be zero) with the 
// normalized version of a tracker which we pick randomly
LIST_DELAY :: list<Time>(0,60,180,360,720,1440)   // 0, 1h,3h,6h,12h
[makeCorrelationNew(t:TSerie,liftMode:integer) : TSTerm
  ->  let tk := pickTrackerNew(t,liftMode), best_d := 0, best_v := 0.0 in
         (//[LDB] -- try to use tracker ~S to explain the ts // tk,
          for d in LIST_DELAY
            let v := matchSerie(t,shift(tk.value,d)) in
               (if (v > best_v) (best_v := v, best_d := d)),
         //[LDB] -- best delay = ~A mins gives correlation ~A // best_d, best_v,
         Correlate(from = tk,
                   coeff = (if (corSerie(t,tk.value) > 0.0) 1.0 else -1.0) * (stdev(t) / stdev(tk.value)) * pb.fparam,
                   delay = best_d)) ]

// pick a tracker randomly but only if the match is good enough
[pickTrackerNew(t:TSerie,liftMode:integer) : TSConstant
  -> let lt := pb.profile.trackers, n := length(lt), i := 0, k := 0 in
    (if (liftMode >= 5) k := liftMode - 5                     // we impose the tracker!
     else let  lprob := list<integer>{0 | i in (1 .. n)} in
        (for i in (1 .. n)
           let v := matchSerie(t,lt[i])  in
             (if (v > CORR_CUT & i != pb.tabu) lprob[i] := 0
              else lprob[i] := sqr(integer!(v * 100.0))),
         //[LDB] list of tracker correlations : ~A // lprob,
         for i in (2 .. n) lprob[i] :+ lprob[i - 1],   // transformation into partial sums
         let j := random(lprob[n]) in
            k := (some(i in (1 .. n) | j <= lprob[i]) as integer),
         TSConstant(tag = pb.profile.tags[k + 1], index = k, value = lt[k]))) ]

// this is the code that was used befire
[makeCorrelation(t:TSerie,liftMode:integer) : Correlate
  -> let tk := pickTracker(t,liftMode), best_d := 0, best_v := 0.0 in
       (//[LDB] -- try to use tracker ~S to explain the ts // tk,
        for d in LIST_DELAY
		   let v := matchSerie(t,shift(tk.value,d)) in
		     (if (v > best_v) (best_v := v, best_d := d)),
        //[LDB] -- best delay = ~A mins gives correlation ~A // best_d, best_v,
        Correlate(from = tk,
		  coeff = (if (corSerie(t,tk.value) > 0.0) 1.0 else -1.0) * (stdev(t) / stdev(tk.value)) * pb.fparam,
          delay = best_d)) ]

// pickTracker pick a tracker with a probability proportional to its correlation
// liftMode : how to pick (used for CIC)
[pickTracker(t:TSerie,liftMode:integer) : TSConstant
  -> let lt := pb.profile.trackers, n := length(lt), i := 0, k := 0 in
        (if (liftMode >= 5) k := liftMode - 5                     // we impose the tracker!
         else let  lprob := list<integer>{0 | i in (1 .. n)} in
	    (for i in (1 .. n)
                 (lprob[i] := integer!(matchSerie(t,lt[i]) * 100.0),
                  if (i = pb.tabu) lprob[i] := 0),            // tabu mode = do not use tracker i
             //[LDB] list of tracker correlations : ~A // lprob,
	     for i in (2 .. n) lprob[i] :+ lprob[i - 1],   // transformation into partial sums
	     let j := random(lprob[n]) in
         	k := some(i in (1 .. n) | j <= lprob[i]) as integer,
       TSConstant(tag = pb.profile.tags[k + 1], index = k, value = lt[k]))) ]
     
// to see the match we compute the correlation, which is insensitive to  constant value, so
// there is no need to normalize
[matchSerie(t1:TSerie,t2:TSerie) : float -> abs(corSerie(t1,t2)) ]
[corSerie(t1:TSerie,t2:TSerie) : float
  -> if (t1.count = t2.count) corr(t1,t2)
     else corr(t1,project(t2,t1))  ]

// this is dumber : pick the best tracker !
[pickBestTracker(t:TSerie) : integer
  -> let lt := pb.profile.trackers, n := length(lt),  best_i := 0, best_v := 0.0 in
	(for i in (1 .. n)
            let v := matchSerie(t,lt[i])  in
               (if (v > best_v) (best_v := v, best_i := i)),
         best_i) ]

// this is dumber : pick the best tracker !
[bestMatchValue(t:TSerie) : float
  -> let lt := pb.profile.trackers, n := length(lt),  best_i := 0, best_v := 0.0 in
    (for i in (1 .. n)
            let v := matchSerie(t,lt[i])  in
               (if (v > best_v) (best_v := v, best_i := i)),
         best_v) ]

// [DEBUG] useful for hand tuning
// this version uses the exact
[makeSumCorr(t:TSerie,liftMode:integer) : TSTerm
  -> if (bestMatchValue(t) < CORR_CUT) makeConstant(t)
    else let a :=  makeConstant(t),  // makeTrend(t) is already more precise and less robust
         best_t := pickBestTracker(t),
         b := makeCorrelation2(difference(t,a),5 + best_t) in
	 Sum(t1 = a, t2 = b) ]

// fit for Correlation
// fit returns the best matching tracker
[fitCorrelation(t:TSerie) : float
  -> let lt := pb.profile.trackers, n := length(lt),  best_i := 0, best_v := 0.0 in
        (for i in (1 .. n)
            let v := matchSerie(t,lt[i])  in
               (if (v > best_v) (best_v := v, best_i := i)),
            1.0 - best_v) ]

LIST_DELAY2 :: list<Time>(0,60,180,360,720,1440)   // 0, 1h,3h,6h,12h
// (0,60,120,180,240,300,360,420,720,1440,2000, 2500, 3000)
//
// debug variant to control the deviation
[makeCorrelation2(t:TSerie,liftMode:integer) : Correlate
  -> let tk := pickTrackerOld(t,liftMode), best_d := 0, best_v := 0.0 in
       (//[LDB] -- try to use tracker ~S to explain the ts // tk,
        for d in LIST_DELAY2
		   let v := matchSerie(t,shift(tk.value,d)) in
		     (if (v > best_v) (best_v := v, best_d := d)),
        //[LDB] -- best delay = ~A mins gives correlation ~A // best_d, best_v,
        Correlate(from = tk,
		  coeff = (if (corSerie(t,tk.value) > 0.0) 1.0 else -1.0) *
                          (stdev(t) / stdev(tk.value)) * pb.fparam,
                  delay = best_d)) ]




// (4) lift a generic term ------------------------------	

// make a basic term with 40% Trend, 20% Hour, 20% Weekly,
[makeBasic(t:TSerie,liftMode:integer) : TSBasic
  -> let n := random(100) in
       (if (n < 40) makeAverage(t)
        else if (n < 60 & hourOK(liftMode)) makeHourly(t)
    else if (n < 80 & weekOK(liftMode)) makeWeekly(t)
    else makeTrend(t))]

// This is the key method, controlled by a meta-parameter (liftMode) which tells which part of the Algebra 
    // 0:   use everything
    // 1:   use everything but trackers
    // 2:   crude = trend, cycle and Mix
    // 3:   crude + H
    // 4:   crude + W
    // 5+i : crude + tracker
// create a term with depth less than n 
[makeTST(t:TSerie,n:integer, liftMode:integer) : TSTerm
  -> let m := random(100) in
        (if (n < 3 | m < 40) makeBasic(t, liftMode)
	 else if (m < 70 | (liftMode >= 1 & liftMode <= 4)) makeComposed(t,n,liftMode)
	 else makeSum(t,n,liftMode)) ]                       // sum of a term and a derived

// readable macros
hourOK(liftMode:integer) : boolean -> (liftMode <= 1 | liftMode = 3)
weekOK(liftMode:integer) : boolean -> (liftMode <= 1 | liftMode = 4)
trackOK(liftMode:integer,i:integer) : boolean -> (liftMode <= 0 | liftMode = 5 + i)
 
// LIFTBOUND avoids the creation of terms that are too large
LIFTBOUND:integer :: 5
[lift(t:TSerie,liftMode:integer) : TSTerm // -> makeTST(t,LIFTBOUND) + trace/check
   -> let a := makeTST(t,LIFTBOUND,liftMode) in
        (if (depth(a) > LIFTBOUND) error("long term ~S -> ~A",a,depth(a)),
         ck(a,"lift"),
         a) ]

// create a composed term with depth less than n
[makeComposed(t:TSerie,n:integer,liftMode:integer) : TSTerm
  ->  makeMix(t,n,liftMode) ]


// useful utility ?
// best lift out of n
[lift(n:integer,t:TSerie) : TSTerm
   -> let best_d := 1e8, best_t := unknown in
        (//[DEBUG] lift(~A) // n,
         for i in (1 .. n)
           let a := lift(t) in
             (if (distance(a,t) < best_d)
                 (best_d := distance(a,t), best_t := a)),
         best_t as TSTerm) ]


// (5) controlled lift, type-constained ------------------------------

// filter
FITCUT:float :: 1e9

// create a term of a specific type, useful for debug
[makeControl(t:TSerie,i:integer) : TSTerm
  -> if (i = 0) makeConstant(t)
     else if (i = 1) makeTrend(t)
     else if (i = 2)
        (if (fitHourly(t) < FITCUT) makeHourly(t)
         else makeConstant(t))
     else if (i = 3) makeWeekly(t)
     else if (i = 4) makeAverage(t)
     else if (i = 15) makeSumCorr(t,0)   // debug method for comparison
     else if (i >= 5) controlSum(t,i - 4) ]

// debug version that shows the result and store it in TST1
AA:TSTerm := unknown
[control(t:TSerie,i:integer) : TSTerm
  -> let a := makeControl(t,i) in
        (display(tserie(a),t),
         AA := a,
         a) ]

// call the fit heuristic
[fitControl(t:TSerie,i:integer) : float
  -> if (i = 1) fitTrend(t)
     else if (i = 2) fitHourly(t)
     else if (i = 3) fitWeekly(t)
     else if (i = 4) fitAverage(t)
     else if (i = 5 | i = 15) fitCorrelation(t)
     else if (i = 6) fitCummulative(t)
     else if (i = 7) fitThreshold(t)
     else 0.0]


