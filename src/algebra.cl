// +--------------------------------------------------------------------------------------------+
// |  EMLA                                                                                      |
// |  Evolutionary Machine Learning Agents                                                      |
// |  algebra.cl                                                                                |
// |  Copyright (C) Yves Caseau 2016-2019                                                       |
// +--------------------------------------------------------------------------------------------+

// algebraic operations : methods that are defined for each term of the algebra
// v0.4

// *********************************************************************************************
// *  Table of content                                                                         *
// *     Part 1: dCopy, distance & depth                                                       *
// *     Part 2: algebra term modification interface                                           *
// *     Part 3: lift & project                                                                *
// *     Part 4: mutate                                                                        *
// *********************************************************************************************


// *********************************************************************************************
// *     Part 1: dCopy, distrance & depth                                                      *
// *********************************************************************************************

// deepCopy
[dCopy(a:TSTerm) : TSTerm
  -> case a
      (Hourly   Hourly(slice = a.slice,values = copy(a.values)),
       Weekly   Weekly(values = copy(a.values)),
       Trend    Trend(originValue = a.originValue, limitValue = a.limitValue, alpha = a.alpha),
       Cycle    Cycle(period = a.period, lowDate = a.lowDate, halfPeriod = a.halfPeriod,
                      minValue = a.minValue, maxValue = a.maxValue),
       Average          Average( from = a.from, coeff = a.coeff, history = a.history),
       Correlate        Correlate( from = a.from, coeff = a.coeff, delay = a.delay),
       Cummulative      Cummulative( from = a.from, coeff = a.coeff, memory = a.memory),
       Threshold        Threshold( from = a.from, coeff = a.coeff, delay = a.delay,
                                   level = a.level),
       Mix      Mix( coeff = a.coeff, t1 = dCopy(a.t1), t2 = dCopy(a.t2)),
       Sum      Sum(t1 = dCopy(a.t1), t2 = dCopy(a.t2)),
       Split    Split( cutoff = a.cutoff, t1 = dCopy(a.t1), t2 = dCopy(a.t2)),
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
      Split    case b
		   (Split (dist(a.t1,b.t1) + dist(a.t2,b.t2)),
		    any (dist(a.t1,b) + dist(a.t2,b))),
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

// this is the simplest distance (used with Knomee)
[distance(a:TSTerm,t:TSerie) : float
  -> let n := t.count, ld := t.dates, s := 0.0,
         span := ld[n] - ld[1], delta := t.maxValue - t.minValue in
       (if (delta = 0.0) delta := 1.0,         // avoid error in stupid case
        for i in (1 .. n) s :+ sqr((a[ld[i]] - t.values[i]) / delta),
        s / float!(n))  ]


// other variante - weight by the distance to other points
// THIS IS THE BEST DEFAULT SOLUTION
// ARGL !!! there is a stupid bug ! span is less than the sum of weigths => to be fixed in EMLA 0.5
// TODO : fix the ignoble bug (done for Knomee)
[wDistance(a:TSTerm,t:TSerie) : float
  -> let n := t.count, ld := t.dates, s := 0.0,
         span := ld[n] - ld[1], delta := t.maxValue - t.minValue in
       (if (delta = 0.0) delta := 1.0,         // avoid error in stupid case
        for i in (1 .. n)
            let w := (if (i = 1) (ld[2] - ld[1]) else ld[i] - ld[i - 1]) in
            s :+ w * sqr((a[ld[i]] - t.values[i]) / delta),
        s / span)  ]


/* triangle distance - does not work
[tDistance(at:TSTerm,t:TSerie) : float
  -> let sum := 0.0, n := t.count, a := t.dates[1], b := t.dates[n] in
       (for i in (2 .. n)                                           // intern part - each of [xi,xi+1]
           let x1 := t.dates[i - 1], x2 := t.dates[i],
               v1 := at[x1], v2 := at[x2],
               w1 := t.values[i - 1], w2 := t.values[i] in
             (sum :+ (x2 - x1) * absInt(v1 - w1, v2 - w2)),
        sum / (b - a)) ] */

// evaluate is the "meta-distance" used for learning
// this is called re-normalisation
DEVFACTOR:float :: 1.0
DEVSTART:float :: 0.02
[evaluate(a:TSTerm,t:TSerie) : float
  -> wDistance(a,t) + DEVFACTOR * max(0.0,stdev%(a) - DEVSTART) ]

// this is a test ! should be wDistance

//TBUG  -> (if WEIGHTED wDistance(a,t) else distance(a,t)) + DEVFACTOR * max(0.0,stdev%(a) - DEVSTART) ]

// new in v0.4 (surprising) : distance to mean (a kind of weighted stdev)
[evaluateMean(t:TSerie) : float
 -> let n := t.count, ld := t.dates, s := 0.0,
         span := ld[n] - ld[1], delta := t.maxValue - t.minValue in
       (if (delta = 0.0) delta := 1.0,         // avoid error in stupid case
        for i in (1 .. n)
            let w := (if (i = 1) (ld[2] - ld[1]) else ld[i] - ld[i - 1]) in
            s :+ w * sqr((t.avgValue - t.values[i]) / delta),
        s / span)  ]


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
[nSlot(t:Trend) : TAG 
 ->  3]

// cf code of hyperbolic approximation in model.cl .. constraint ensures boundedness
[min(t:Trend, i:TAG) : float
  -> if (i = 1) (if (t.alpha < 0.0) t.limitValue else pb.trendMin)
     else if (i = 2) (if (t.alpha > 0.0) t.originValue else pb.trendMin)
     else (if (t.originValue < t.limitValue) 0.0 else -1.0)]

[max(t:Trend, i:TAG) : float
  -> if (i = 1) (if (t.alpha > 0.0) t.limitValue else pb.trendMax)
     else if (i = 2) (if (t.alpha < 0.0) t.originValue else pb.trendMax)
     else (if  (t.originValue > t.limitValue) 0.0 else 1.0)]

[read(t:Trend,i:TAG) : float 
   -> (read(TrendSlots[i],t) as float)]
[write(t:Trend,i:TAG,v:float) : void 
   -> write(TrendSlots[i],t,v)]
[label(t:Trend,i:TAG) : symbol 
   -> TrendSlots[i].name]

// Hourly term :  SLICE values
[nSlot(t:Hourly) : TAG 
  ->  SLICE]
[min(t:Hourly, i:TAG) : float 
  -> pb.target.minValue ]
[max(t:Hourly, i:TAG) : float 
  -> pb.target.maxValue ]
[read(t:Hourly,i:TAG) : float 
  -> t.values[i]]
[write(t:Hourly,i:TAG,v:float) : void 
  -> t.values[i] := v]
[label(t:Hourly,i:TAG) : string 
  -> string!(i)]

// Weekly term :  7 values
[nSlot(t:Weekly) : TAG 
  ->  7]
[min(t:Weekly, i:TAG) : float 
  -> pb.target.minValue ]
[max(t:Weekly, i:TAG) : float 
  -> pb.target.maxValue ]
[read(t:Weekly,i:TAG) : float 
  -> t.values[i]]
[write(t:Weekly,i:TAG,v:float) : void 
  -> t.values[i] := v]
[label(t:Weekly,i:TAG) : string 
  -> string!(i)]

// Average : period, lowDate, halfPeriod,minValue,MaxValue
AverageSlots :: list<property>(history,coeff)
[nSlot(t:Average) : TAG 
  ->  2]
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
[label(t:Average,i:TAG) : symbol 
  -> AverageSlots[i].name]


// Cycle : period, lowDate, halfPeriod,minValue,MaxValue
CycleSlots :: list<property>(period, halfPeriod, lowDate, minValue, maxValue)
[nSlot(t:Cycle) : TAG 
  ->  5]
[min(t:Cycle, i:TAG) : float
  -> if (i = 1 | i = 2) 0.0 
     else if (i = 3) float!(pb.timeList[1])
	 else pb.target.minValue]
[max(t:Cycle, i:TAG) : float
  -> if (i = 1 | i = 2) float!(20 * DAY)
     else if (i = 3) float!(pb.timeList[1] + 20 * DAY)      // should look at max value BUG
     else pb.target.maxValue]
[read(t:Cycle,i:TAG) : float
   -> let v := read(CycleSlots[i],t) in (if (i <= 3) float!(v) else v) ]
[write(t:Cycle,i:TAG,v:float) : void
   -> write(CycleSlots[i],t,(if (i <= 3) integer!(v) else v)) ]
[label(t:Cycle,i:TAG) : symbol -> CycleSlots[i].name]


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

// Split : cutoff,
[nSlot(t:Split) : TAG ->  1]
[min(t:Split, i:TAG) : float -> pb.timeScale[1]]
[max(t:Split, i:TAG) : float -> last(pb.timeScale)]
[read(t:Split,i:TAG) : float -> float!(t.cutoff)]
[write(t:Split,i:TAG,v:float) : void -> t.cutoff := integer!(v)]
[label(t:Split,i:TAG) : symbol -> cutoff.name]


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

// Cummulative : coeff, memory
CummSlots :: list<property>(coeff,memory)
[nSlot(t:Cummulative) : TAG ->  2]
[min(t:Cummulative, i:TAG) : float -> 0.0]
[max(t:Cummulative, i:TAG) : float
  -> if (i = 1) 1.0 else float!(5 * DAY) ]
[read(t:Cummulative,i:TAG) : float
    -> let v := read(CummSlots[i],t) in (if (i = 1) (v as float) else float!(v as integer)) ]
[write(t:Cummulative,i:TAG,v:float) : void
    -> write(CummSlots[i],t, (if (i = 2) integer!(v) else v))  ]
[label(t:Cummulative,i:TAG) : symbol -> CummSlots[i].name]

// Threshold : coeff, level, delay
ThreshSlots :: list<property>(coeff,level,delay)
[nSlot(t:Threshold) : TAG ->  3]
[min(t:Threshold, i:TAG) : float -> 0.0]
[max(t:Threshold, i:TAG) : float
  -> if (i < 3) 1.0 else float!(5 * DAY) ]
[read(t:Threshold,i:TAG) : float
    -> let v := read(ThreshSlots[i],t) in
          (if (i <= 2) (v as float) else float!(v as integer)) ]
[write(t:Threshold,i:TAG,v:float) : void
    -> write(ThreshSlots[i],t, (if (i = 3) integer!(v) else v)) ]
[label(t:Threshold,i:TAG) : symbol -> ThreshSlots[i].name]

// *********************************************************************************************
// *     Part 3: lift & project                                                                *
// *********************************************************************************************

// LIFT DEBUG
LDB:integer :: 5

// we introduce a control variables
NOSUM:boolean :: false

// projection : produce a TSerie
[tserie(a:TSTerm) : TSerie
  -> TSerie(count = length(pb.timeList),
            dates = pb.timeList,
        values = list<float>{ a[x] | x in pb.timeList}) ]

// lift is implemented with makeX(T) method
// makeX(T) returns a X TST that is similar to T (a TSerie)
// we follow the algebra
    // TSTerm :: TSBasic | TSComposed | Sum(TSTerm, TSDerived)
// TSBasic :: Trend | Weekly | Hourly | Average | Cycle
// TSComposed :: Mix(TSTerm, TSTerm) | Split(Constant, TSTerm)
// TSDerived   :: Corr | Cumm | Thresh


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



// lift a Cycle: try to find a periodic pattern in a Time Serie by looking at the
// list of local extremums (lox is the list of indices [1-n] of l.e.)
// (1) => extract period
// (2) => find average position of low values according to this pattern
// create corresponding Cycle
// v0.4: create a filter that only keeps the best local extremum
LIST_RATIO:list<float> := list<float>(0.1,0.15,0.20,0.25,0.3,0.35,0.4,0.45,0.5)
[makeCycle(t:TSerie) : TSBasic
  -> let best_c := unknown, best_v := 1e9 in
       (for r in LIST_RATIO
         let a := makeCycle(t,r),
             v := evaluate(a,t) * (if (a % Cycle) 1.0 else 4.0) in
             (//[5] ===== ratio = ~A => ~S at ~A =====// r,a,v,
              if (best_v > v)
                  (best_v := v, best_c := a)),
        best_c)]

// find a Cycle for a given ration (filter only true extremums)
[makeCycle(t:TSerie, ratio:float) : TSBasic
  -> let lox := localExtrem(t,ratio), n := length(lox) in
      (if (n < 4) makeTrend(t) // not a TS for a periodic pattern !
	   else let firstMin? := (sign(t,lox) = 1),
	            P := extractPeriod(t,lox,firstMin?),
	            H := extractHalfPeriod(t,lox,firstMin?),
                mDate := t.dates[lox[(if firstMin? 1 else 2)]],
		        sMin := 0.0, ctMin := 0, mShift := 0,
		        sMax := 0.0, ctMax := 0 in
               (//[LDB] computes the average min and average max, P = ~A, H = ~A (~S)// P,H,firstMin?,
	            for i in (1 .. n)
	           let val := t.values[lox[i]] in
		      (if (((i mod 2) = 1) = firstMin?)     // firstMin => t[1] is min
			             (sMin :+ val, ctMin :+ 1,
                          mShift :+ cycleMod(t.dates[lox[i]],P))
		       else (sMax :+ val, ctMax :+ 1)),
            //[5] period is ~A days, min@~A // P / MinPerDay,mDate + (mShift / ctMax),
	        Cycle(period = P,                               // period in day
                  halfPeriod = H,                           // distance between lowDate & High
		          lowDate =  mDate + (mShift / ctMax),      // try to find the best reference
                  minValue = sMin / float!(ctMin),          // average of min values
                  maxValue = sMax / float!(ctMax)))) ]      // average of max

// returns the list of local extremums (exclude bounds !) -> through their index (sublist of 2 .. n-1)
// monotonic function returns empty list
// new in v0.4 : LE = 3 conditios
//   (a) change of sign (local extremum)
//   (b) close to min/max modulo ratio
//   (c) alternate list of local min and local max
[localExtrem(t:TSerie,ratio:float) : list<integer>
  -> let sign := 0, prev_v := t.values[1], span := t.maxValue - t.minValue,
         max_v := t.maxValue - ratio * span,
         min_v := t.minValue + ratio * span,
         alternate := 0,                       // 1 if previous was a max, -1 is prev lox was a min
         lox := list<Time>() in
    (for i in (2 .. t.count)
	  let x := t.dates[i], v := t.values[i] in
	    (//[LDB] ~A:~A v = ~A, prev = ~A sign = ~A // i,x,v,prev_v,sign,
         if (sign = 1 & v < prev_v & prev_v > max_v & alternate < 1)
             (// found a local max
              lox :add (i - 1),
              alternate := 1)
         else if  (sign = -1 & v > prev_v & prev_v < min_v & alternate > -1)
             (// found a local min extremum value[i - 1] = prev_v
               lox :add (i - 1),
               alternate := -1),
         if (v > prev_v) sign := 1              // sign tells if increasing or decreasing
	     else if (v < prev_v) sign := -1,
         prev_v := v),
         // printf("extremums of ~S is ~I\n",t, showLox(t,lox)),
         //[5] lox -> ~A values with ratio=~A // size(lox), ratio,
         lox) ]

// good for debug
[showLox(t:TSerie,lox:list<integer>) : void
  -> for x in lox printf("~F2@~A ",t.values[x],dateString(t.dates[x])) ]
	 
// sign of a local extremums list: +1 means that first is a low value, -1 high, 0 means that
// the list is empty
[sign(t:TSerie,lox:list<integer>) : integer
 -> if (length(lox) = 0 ) 0
    else sign(t,lox[1]) ]
[sign(t:TSerie,i:integer) : integer
  -> if (t.values[i - 1] > t.values[i]) 1 else -1 ]
  
// using the lox, finds the average period
// look at all even (minimal) dates
[extractPeriod(t:TSerie,lox:list<integer>,odd?:boolean) : Time
  -> let s := 0, previ := 0, i := (if odd? 1 else 2), 
         ct := 0, n := length(lox) in
        (while (i <= n)  
		   (if (previ != 0) 
		        (s :+ (t.dates[lox[i]] - t.dates[previ]), ct :+ 1),
		    previ := lox[i],
		    i :+ 2),
		 s / ct) ]
		 
// using the lox, finds the average half period
// look at all even (minimal) dates
[extractHalfPeriod(t:TSerie,lox:list<integer>,odd?:boolean) : Time
  -> let s := 0, i := (if odd? 2 else 3), 
         ct := 0, n := length(lox) in
        (while (i <= n)  
		   (s :+ (t.dates[lox[i]] - t.dates[lox[i - 1]]),
		    ct :+ 1,
		    i :+ 2),
		 s / ct) ]


// (2) lift a composed term --------------------------------

// we use a smart approach : the target for b is not t but (t - la) / (1 - l)
// thus the error made by t -> a may be compensated
[makeMix(t:TSerie, n:integer) : Mix
  -> let l := random(0.4,0.8),
         a := makeTST(t, (n - 1) / 2),
     nt := multiply(difference(t,multiply(tserie(a),l)),1.0 / (1.0 - l)),
     b := makeTST(nt, (n - 1) / 2) in
     Mix(coeff = l, t1 = a, t2 = b) ]


// making a split is a strange idea actually
// it requires to split the target t into t_1 and t_2		  
[makeSplit(t:TSerie, m:integer) : Split
  -> let n := random(t.count / 3, t.count * 2 / 3),
         t_1 := split(t,1,n), t_2 := split(t,n, t.count),
	 a := makeConstant(t_1), b := makeTST(t_2, (m - 1) / 2) in
	Split(cutoff = t.dates[n], t1 = a, t2 = b)]


// (3) lift a derived term ------------------------------

// make a Sum : list a TST then lift a derived for the difference
[makeSum(t:TSerie,n:integer) : Sum
  -> let a := makeTST(t,n - 2),
         b := makeDerived(difference(t,a)) in
     Sum(t1 = a, t2 = b) ]

// make a derived : 50% Corr, 25% Cum, 25%
[makeDerived(t:TSerie) : TSDerived
  -> let n := random(100) in
       (if (n < 40) makeCorrelation(t)
        else if (n < 80) makeCummulative(t)
        else makeThreshold(t)) ]

// this is the code that was used before
LIST_DELAY :: list<Time>(0,60,180,360,720,1440)   // 0, 1h,3h,6h,12h
[makeCorrelation(t:TSerie) : Correlate
  -> let tk := pickTracker(t), best_d := 0, best_v := 0.0 in
       (//[LDB] -- try to use tracker ~S to explain the ts // tk,
        for d in LIST_DELAY
           let v := matchSerie(t,shift(tk.value,d)) in
             (if (v > best_v) (best_v := v, best_d := d)),
        //[LDB] -- best delay = ~A mins gives correlation ~A // best_d, best_v,
        Correlate(from = tk,
          coeff = (if (corSerie(t,tk.value) > 0.0) 1.0 else -1.0) * (stdev(t) / stdev(tk.value)) * pb.fparam,
          delay = best_d)) ]

// pickTracker pick a tracker with a probability proportional to its correlation
[pickTracker(t:TSerie) : TSConstant
  -> let lt := pb.profile.trackers, n := length(lt), i := 0, k := 0,
         lprob := list<integer>{0 | i in (1 .. n)} in
        (for i in (1 .. n)
                 (lprob[i] := integer!(matchSerie(t,lt[i]) * 100.0),
                  if (i = pb.tabu) lprob[i] := 0),            // tabu mode = do not use tracker i
             //[LDB] list of tracker correlations : ~A // lprob,
         for i in (2 .. n) lprob[i] :+ lprob[i - 1],   // transformation into partial sums
         let j := random(lprob[n]) in
             k := some(i in (1 .. n) | j <= lprob[i]) as integer,
         TSConstant(tag = pb.profile.tags[k + 1], index = k, value = lt[k])) ]

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



// make a Cummulative term
// here we try different cummulative DELAYS
DAY :: 1440
LIST_PERIOD :: list<Time>(DAY, 2 * DAY, 4 * DAY, 6 * DAY)
[makeCummulative(t:TSerie) : Cummulative
  -> let tk := pickTracker(t), best_d := 0, best_v := 0.0 in
        (for d in LIST_PERIOD
           let v := matchSerie(t,cummulative(tk.value,d)) in
             (if (v > best_v) (best_v := v, best_d := d)),
         //[LDB] -- best duration = ~A mins gives correlation ~A // best_d, best_v,
         Cummulative(from = tk,
             coeff = (if (corSerie(t,tk.value) > 0.0) 1.0 else -1.0) *
                             (stdev(t) / stdev(cummulative(tk.value,best_d))) * pb.fparam,
             memory = best_d)) ]



// make a Treshold term
// here we try 3 relative levels 30,50,80%
// we will let local optimization play with delays
LIST_LEVEL :: list<float>(0.2, 0.3,0.5,0.7)
[makeThreshold(t:TSerie) : Threshold
  ->  let tk := pickTracker(t), best_l := 0.0, best_v := 0.0 in
        (for l in LIST_LEVEL
           let v := matchSerie(t,threshold(tk.value,l)) in
        (if (v > best_v) (best_v := v, best_l := l)),
        //[LDB] -- best treshold = ~A mins gives correlation ~A // best_l, best_v,
        Threshold(from = tk,
               delay = 0,
              coeff = (if (corSerie(t,tk.value) > 0.0) 1.0 else -1.0) *
                           (stdev(t) / stdev(threshold(tk.value,best_l))) * pb.fparam,
              level = best_l)) ]


// (4) lift a generic term ------------------------------

// make a basic term with 40% Trend, 20% Hour, 20% Weekly,
[makeBasic(t:TSerie) : TSBasic
  -> let n := random(100) in
       (if (n < 40) makeAverage(t)
        else if (n < 60) makeHourly(t)
    else if (n < 80) makeWeekly(t)
//    else if (n < 80 + CYCLE%) makeCycle(t)
    else makeTrend(t))]

// This is the key method, controlled by a meta-parameter (liftMode) which tells which part of the Algebra
    // 0:   use everything
    // 1:   use everything but trackers
    // 2:   crude = trend, cycle and Mix
    // 3:   crude + H
    // 4:   crude + W
    // 5+i : crude + tracker
// create a term with depth less than n
[makeTST(t:TSerie,n:integer) : TSTerm
  -> let m := random(100) in
        (if (n < 3 | m < 40) makeBasic(t)
     else if (m < 70) makeComposed(t,n)
     else makeSum(t,n)) ]                       // sum of a term and a derived


// LIFTBOUND avoids the creation of terms that are too large
LIFTBOUND:integer :: 5
[lift(t:TSerie) : TSTerm // -> makeTST(t,LIFTBOUND) + trace/check
   -> let a := makeTST(t,LIFTBOUND) in
        (if (depth(a) > LIFTBOUND) error("long term ~S -> ~A",a,depth(a)),
         ck(a,"lift"),
         a) ]

// create a composed term with depth less than n
// v0.4 : should we introduce a Split ?
[makeComposed(t:TSerie,n:integer) : TSTerm
  -> makeMix(t,n) ]
/* terrible !
  ->  let m := random(100) in
       (if (m < SPLIT%) makeSplit(t,n - 2)
        else makeMix(t,n)) ] */


// useful utility ?
// best lift out of n
[lift(n:integer,t:TSerie) : TSTerm
   -> let best_d := 1e8, best_t := unknown in
        (//[DEBUG] lift(~A) // n,
         for i in (1 .. n)
           let a := lift(t) in
             (if (evaluate(a,t) < best_d)
                 (best_d := evaluate(a,t), best_t := a)),
         best_t as TSTerm) ]


// (5) controlled lift, type-constained ------------------------------

// create a term of a specific type, useful for debug
[makeControl(t:TSerie,i:integer) : TSTerm
  -> if (i = 0) makeConstant(t)
     else if (i = 1) makeTrend(t)
     else if (i = 2) makeHourly(t)
     else if (i = 3) makeWeekly(t)
     else if (i = 4) makeAverage(t)
     else if (i = 5) makeCycle(t)
     else if (i = 6) makeSplit(t,5)
     else if (i >= 10) controlSum(t,i - 9) ]

// controlled version : n tells which algebra term should be used
[controlSum(t:TSerie,n:integer) : TSTerm
 -> let a := makeConstant(t),
        b := (if (n = 1) makeCorrelation(difference(t,a))
              else if (n = 2) makeCummulative(difference(t,a))
              else makeThreshold(difference(t,a))) in
        Sum(t1 = a, t2 = b) ]

// debug version that shows the result and store it in TST1
AA:TSTerm := unknown
[control(t:TSerie,i:integer) : TSTerm
  -> let a := makeControl(t,i) in
        (display(tserie(a),t),
         printf("distance to t is ~F2, versus meant to t = ~F2\n",
                wDistance(a,t), evaluateMean(t)),
         AA := a,
         a) ]



// *********************************************************************************************
// *     Part 4: mutate                                                                        *
// *********************************************************************************************

// v0.2: redo the mutation with two parameters
//  - beta: (0 - 1)  tells how wide the change is
//  - delta (0 - 1)  tells how deep the change is

// this is a random change - similar to local optimization
// we do not change the structure of the tree, simply small mutations of some
// slots

// TSBasic :: Trend | Weekly | Hourly | Cycle                => use the generic OPT
// TSComposed :: Mix(TSTerm, TSTerm)                         => structure mutate
// TSDerived   :: Corr | Cumm | Thresh                       => generic OPT on derived

// TSterm mutation is simple - use the OPTIMIZE interface
[mutate1(a:TSTerm,p%:integer) : void
  -> //[5] mutate1 ~S,~A // a,p%,
     let i := random(1,nSlot(a)),
         f := random(-(p%),p%),                // percentage of change (play with it)
         m := min(a,i), M := max(a,i),
         v := read(a,i) * (1.0 + (float!(f) / 100.0)) in
       (ck2(a,m,M,i,"mutate"),
        v := min(M,max(m,v)),
        pb.muteT := a, pb.muteI := i, pb.muteV := read(a,i),
    //[5] end mutate1 ~S,~A with ~S // a,i,v,
        write(a,i,v)) ]

// basic
[mutate(a:TSBasic,s?:boolean) : void
  ->  mutate1(a,30),
      if not(s?) mutate1(a,60) ]

// Mutation of a mix or a split
[mutate(a:TSComposed,s?:boolean) : void
  -> //[5] mutate composed ~S,~S // a,s?,
     let b := random?(50) in
       (mutate((if b a.t1 else a.t2), s?),
        if not(s?) mutate((if b a.t2 else a.t1), s?)) ]

// Mutation of a Sum
[mutate(a:Sum,s?:boolean) : void
  -> mutate1(a.t2,30),
     if not(s?) mutate(a.t1,s?) ]

// mutate of a derived
[mutate(a:TSDerived,s?:boolean) : void
  -> mutate1(a,30)]

// a special version that is used in cross and returns the mutated term
[mutate(a:TSTerm) : TSTerm
 -> //[DEBUG] mutate ~S // a,
    mutate(a,false),
    ck(a,"mutate"),
    //[DEBUG] end mutate ~S // a,
    a]

// mutate and opt (to repair)
[mutateOpt(a:TSTerm,t:TSerie) : TSTerm
  -> opt1(mutate(a),t,false,3) ]




