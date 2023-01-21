// +--------------------------------------------------------------------------------------------+
// |  EMLA                                                                                      |
// |  Evolutionary Machine Learning Agents                                                      |
// |  algebra Experiments .cl                                                                   |
// |  Copyright (C) Yves Caseau 2016-2018                                                       |
// +--------------------------------------------------------------------------------------------+

// algebraic operations : methods that are defined for each term of the algebra
// this file contains lots of variations / some could be reused later


// *********************************************************************************************
// *  Table of content                                                                         *
// *     Part 1: dCopy, distance & depth                                                       *
// *     Part 2: algebra term modification interface                                           *
// *     Part 3: lift & project                                                                *
// *     Part 4: mutate                                                                        *
// *     Part 5: cross                                                                         *
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
       Average          Average( from = a.from, coeff = a.coeff, history = a.history),
       Correlate        Correlate( from = a.from, coeff = a.coeff, delay = a.delay),
       Cummulative      Cummulative( from = a.from, coeff = a.coeff, memory = a.memory),
       Threshold        Threshold( from = a.from, coeff = a.coeff, delay = a.delay,
                                   level = a.level),
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
VAR:integer :: 6   // try different variants of distance to see what works best
[distance(a:TSTerm,t:TSerie) : float
  -> if (VAR = 1) distance1(a,t)
     else if (VAR = 2) distance2(a,t)
     else if (VAR = 3) distance3(a,t)
     else if (VAR = 4) distance4(a,t)
     else if (VAR = 5) distance5(a,t)
     else if (VAR = 6) distance6(a,t)
     else if (VAR = 7) distance7(a,t)
     else error("heuristic distance ~A not defined", VAR)]

// distance between a TSTerm and a TSerie
// simple sum of squares, could become more subtle in the future
[distance1(a:TSTerm,t:TSerie) : float
  -> let n := t.count, ld := t.dates, s := 0.0, delta := t.maxValue - t.minValue in
       (if (delta = 0.0) delta := 1.0,         // avoid error in stupid case
        for i in (1 .. n) s :+ sqr((a[ld[i]] - t.values[i]) / delta),
        s / n)  ]

//debug
[dd(a:TSTerm,t:TSerie) : float
  -> let n := t.count, ld := t.dates, s := 0.0, delta := t.maxValue - t.minValue in
       (if (delta = 0.0) delta := 1.0,         // avoid error in stupid case
        for i in (1 .. n)
           (//[0][~A] a @ ~A -> ~S // i, ld[i], a[ld[i]],
             s :+ sqr((a[ld[i]] - t.values[i]) / delta)),
        if (n = 0) error("what the fuck"),
        //[0] s = ~S, n = ~A // s, n,
        s / n)  ]


// variante: replace square by absolute values [not very interesting ]
[distance2(a:TSTerm,t:TSerie) : float
  -> let n := t.count, ld := t.dates, s := 0.0, delta := t.maxValue - t.minValue in
       (if (delta = 0.0) delta := 1.0,         // avoid error in stupid case
        for i in (1 .. n) s :+ abs((a[ld[i]] - t.values[i]) / delta),
        s / n)  ]

// other variante - weight by the distance to other points
// THIS IS THE BEST DEFAULT SOLUTION
[distance3(a:TSTerm,t:TSerie) : float
  -> let n := t.count, ld := t.dates, s := 0.0,
         span := ld[n] - ld[1], delta := t.maxValue - t.minValue in
       (if (delta = 0.0) delta := 1.0,         // avoid error in stupid case
        for i in (1 .. n)
            let w := (if (i = 1) (ld[2] - ld[1]) else ld[i] - ld[i - 1]) in
            s :+ w * sqr((a[ld[i]] - t.values[i]) / delta),
        s / span)  ]


// add a discount to error when there is a cross!
// THIS SHOULD BE BETTER, to be confirmed
[distance4(a:TSTerm,t:TSerie) : float
  -> let n := t.count, ld := t.dates, s := 0.0, span := ld[n] - ld[1],
         delta := t.maxValue - t.minValue, prev := 0.0 in
       (if (delta = 0.0) delta := 1.0,         // avoid error in stupid case
        for i in (1 .. n)
            let w := (if (i = 1) (ld[2] - ld[1]) else ld[i] - ld[i - 1]),
                cross := 1.0, diff := a[ld[i]] - t.values[i] in
              (if (diff * prev < 0.0) cross := 0.5,            // 0.5 discount if crossing
               s :+ (w * cross * sqr(diff)) / delta,
               prev := diff),
        s / span)  ]

// discount the past (based on distance 3)
TimeDiscount:float :: 0.98
[distance5(a:TSTerm,t:TSerie) : float
  -> let n := t.count, ld := t.dates, s := 0.0, span := ld[n] - ld[1],
         delta := t.maxValue - t.minValue in
       (if (delta = 0.0) delta := 1.0,         // avoid error in stupid case
        for i in (1 .. n)
            let w := (if (i = 1) (ld[2] - ld[1]) else ld[i] - ld[i - 1]),
                w2 := TimeDiscount ^ float!(n - i) in
            s :+ w * w2 * sqr((a[ld[i]] - t.values[i]) / delta),
        s / span)  ]

// distance 6 adds a penalty for large deviation
DEVFACTOR:float :: 1.0
DEVSTART:float :: 0.02
[distance6(a:TSTerm,t:TSerie) : float
  -> distance3(a,t) + DEVFACTOR * max(0.0,stdev%(a) - DEVSTART) ]

// distance 7 adds a penalty when the error is outside
OUTFACTOR:float :: 3.0
[distance7(a:TSTerm,t:TSerie) : float
  -> let n := t.count, ld := t.dates, s := 0.0, avg := t.avgValue,
         span := ld[n] - ld[1], delta := t.maxValue - t.minValue in
       (if (delta = 0.0) delta := 1.0,         // avoid error in stupid case
        for i in (1 .. n)
            let  val := a[ld[i]], ref := t.values[i],
                 w := float!((if (i = 1) (ld[2] - ld[1]) else ld[i] - ld[i - 1])) in
           (if ((val - ref) * (ref - avg) > 0.0) w :* OUTFACTOR,     // penalty for overshooting
            s :+ w * sqr((val - ref) / delta)),
        s / span)  ]

// this is the parameter tuning : we use a % (integer)
[setHPar(n:integer) : void
 -> if (VAR = 5) TimeDiscount := float!(n) / 100.0
    else if (VAR = 6) DEVFACTOR := float!(n) / 100.0
    else if (VAR = 7) OUTFACTOR := float!(n) / 100.0
    else nil
    ]

[getHPar() : integer
  -> if (VAR = 5) integer!(TimeDiscount * 100.0)
     else if (VAR = 6) integer!(DEVFACTOR * 100.0)
     else if (VAR = 7) integer!(OUTFACTOR * 100.0)
     else 0 ]

// finds if a pattern is used in a term
[pattern?(a:TSTerm,i:integer) : boolean
  -> case a
      (TSConstant (a.index = i),
       Hourly (i = -1),
       Weekly (i = -2),
       TSComposed (pattern?(a.t1,i) | pattern?(a.t2,i)),
       Sum (pattern?(a.t1,i) | pattern?(a.t2,i)),
       TSDerived pattern?(a.from,i),
       any false) ]

// tag for the pattern
[ppatern(y:Profile,i:integer) : string
  -> if (i = -2) "Weekly"
     else if (i = -1) "Hourly"
     else y.tags[i + 1]]


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
 
// Hourly term :  SLICE values
[nSlot(t:Hourly) : TAG ->  SLICE]
[min(t:Hourly, i:TAG) : float -> pb.target.minValue ]
[max(t:Hourly, i:TAG) : float -> pb.target.maxValue ]
[read(t:Hourly,i:TAG) : float -> t.values[i]]
[write(t:Hourly,i:TAG,v:float) : void -> t.values[i] := v]
[label(t:Hourly,i:TAG) : string -> string!(i)]
 
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
// TSBasic :: Trend | Weekly | Hourly | Average
// TSComposed :: Mix(TSTerm, TSTerm)
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


// new idea : use a laplace formula
[deduce2(n:integer,sigma:float,m:integer,avg:float) : float
 -> (sigma + avg) / (n + 1) ]


// fitHourly(t) : combination of umber of values and stdev
FH:integer :: 1
[fitHourly(t:TSerie) : float
  -> let a := makeHourly(t),
         b := makeConstant(t),
         n := float!(t.count),
         nFactor := n / ( 30.0 + n) in
      (if (FH = 1) distance1(a,t) / distance1(b, t)
       else if (FH = 2) distance6(a,t) / distance6(b, t)
       else if (FH = 3) 1.0 -  nFactor * stdeviation(a.values)
       else if (FH = 4) distance3(a,t) / distance3(b, t)
       else 1.0 - stdeviation(a.values))]

// complex version that sums the local dev and normalize
// does not work => clean and kill
[fitHourly2(t:TSerie) : float
 -> let table := list<float>{0.0 |i in (1 .. SLICE)},
        cnt := list<integer>{0 |i in (1 .. SLICE)},
        table2 := list<float>{0 |i in (1 .. SLICE)},
        a := Hourly(slice = SLICE),
        avg := t.avgValue, stdev := stdeviation(t.values),
        sumdev : = 0.0 in
        (for i in (1 .. t.count)
            let j := slice(a,t.dates[i]) in
                (table[j] :+ t.values[i],
                 table2[j] :+ sqr(t.values[i]),
                 cnt[j] :+ 1),
         for j in (1 .. SLICE)
           let mean := (if (cnt[j] > 0) (table[j] / cnt[j]) else avg),
               var := (if (cnt[j] > 0) ((table2[j] / cnt[j]) - sqr(mean)) else 0.0) in
               sumdev :+ (sqrt(var) / mean),
         if (stdev > 0.0) (sumdev / (SLICE * stdev)) else 1.0) ]


// debug : analysis
[fitTalk(t:TSerie)
  -> let table := list<float>{0.0 |i in (1 .. SLICE)},
         square := list<float>{0.0 |i in (1 .. SLICE)},
         cnt := list<integer>{0 |i in (1 .. SLICE)},
         avg := t.avgValue,
         a := Hourly(slice = SLICE) in
        (for i in (1 .. t.count)
            let j := slice(a,t.dates[i]) in
                (table[j] :+ t.values[i],
                 square[j] :+ sqr(t.values[i]),
                 cnt[j] :+ 1),
         for i in (1 .. SLICE)
            let m := float!(cnt[i]) in
              printf("~A: ~A values -> ~A dev=~A\n",i,cnt[i],
                                       (if (m > 0.0) table[i] / m else 0),
                                       (if (m > 0.0) sqrt((square[i] / m) - sqr(table[i] / m)) else 0))
         )]


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

// smoothness of a time series version 1 : no interest
[smooth(l:list<float>) : float
  -> let s := 0.0, m := l[1], M := l[1], n := length(l) in
      (for i in (2 .. n)
        (m :min l[i],
         M :max l[i],
         s :+ abs(l[i] - l[i - 1])),
        if (m = M) 1.0
        else (s / (M - m)))]
[smooth(a:Weekly) : float -> smooth(a.values)]

// regularness : computes the sum of the stdevs !
// TO KILL
[fitWeekly2(t:TSerie) : float
 -> let table := list<float>{0.0 |i in (1 .. 7)},
        cnt := list<integer>{0 |i in (1 .. 7)},
        table2 := list<float>{0 |i in (1 .. 7)},
        a := Weekly(),
        avg := t.avgValue, stdev := stdeviation(t.values),
        sumdev : = 0.0 in
        (for i in (1 .. t.count)
            let j := slice(a,t.dates[i]) in
                (table[j] :+ t.values[i],
                 table2[j] :+ sqr(t.values[i]),
                 cnt[j] :+ 1),
         for j in (1 .. 7)
           let mean := (if (cnt[j] > 0) (table[j] / cnt[j]) else avg),
               var := (if (cnt[j] > 0) ((table2[j] / cnt[j]) - sqr(mean)) else 0.0) in
               sumdev :+ (sqrt(var) / mean),
         if (stdev > 0.0) (sumdev / (7 * stdev)) else 1.0) ]


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

// experiments with match series
// t1 is the refernce, t2 is the proposal
// here we add a penalty for too much deviation
// TOKILL
DEVFACTOR2:float :: 0.3
[matchSerie2(t1:TSerie,t2:TSerie) : float
   -> dist(t1,t2) + DEVFACTOR2 * max(0.0,stdev%(t2) - DEVSTART)]


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
       (if (n < 50) makeCorrelation(t,liftMode)
        else if (n < 75) makeCummulative(t,liftMode)
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
[makeCorrelation(t:TSerie,liftMode:integer) : TSTerm
  ->  let tk := pickTracker(t,liftMode), best_d := 0, best_v := 0.0 in
         (//[LDB] -- try to use tracker ~S to explain the ts // tk,
          for d in LIST_DELAY
            let v := matchSerie(t,shift(tk.value,d)) in
               (if (v > best_v) (best_v := v, best_d := d)),
         //[LDB] -- best delay = ~A mins gives correlation ~A // best_d, best_v,
         Correlate(from = tk,
                   coeff = (if (corSerie(t,tk.value) > 0.0) 1.0 else -1.0) * (stdev(t) / stdev(tk.value)) * pb.fparam,
                   delay = best_d)) ]

// pick a tracker randomly but only if the match is good enough
[pickTracker(t:TSerie,liftMode:integer) : TSConstant
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
             k := some(i in (1 .. n) | j <= lprob[i])),
       TSConstant(tag = pb.profile.tags[k + 1], index = k, value = lt[k])) ]


[makeCorrelationOld(t:TSerie,liftMode:integer) : Correlate
  -> let tk := pickTrackerOld(t,liftMode), best_d := 0, best_v := 0.0 in
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
[pickTrackerOld(t:TSerie,liftMode:integer) : TSConstant
  -> let lt := pb.profile.trackers, n := length(lt), i := 0, k := 0 in
        (if (liftMode >= 5) k := liftMode - 5                     // we impose the tracker!
         else let  lprob := list<integer>{0 | i in (1 .. n)} in
	    (for i in (1 .. n)
                 (lprob[i] := integer!(matchSerie(t,lt[i]) * 100.0),
                  if (i = pb.tabu) lprob[i] := 0),            // tabu mode = do not use tracker i
             //[LDB] list of tracker correlations : ~A // lprob,
	     for i in (2 .. n) lprob[i] :+ lprob[i - 1],   // transformation into partial sums
	     let j := random(lprob[n]) in
         	k := some(i in (1 .. n) | j <= lprob[i])),
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


// [DEBUG] useful for hand tuning
// this version uses the exact
[makeSumCorr(t:TSerie,liftMode:integer) : TSTerm
  -> if (bestMatchValue(t) < CORR_CUT) makeConstant(t)
    else let a :=  makeConstant(t),  // makeTrend(t) is already more precise and less robust
         best_t := pickBestTracker(t),
         b := makeCorrelation2(difference(t,a),5 + best_t) in
	 Sum(t1 = a, t2 = b) ]

// fit for Correlation
FC:integer := 2
[fitCorrelation(t:TSerie) : float
  -> if (FC = 1)
        let a := makeSumCorr(t,0),
            b := makeConstant(t) in
        (distance1(a,t) / distance1(b, t))
     else fitCorrelation2(t) ]

// fit returns the best matching tracker
[fitCorrelation2(t:TSerie) : float
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


// make a Cummulative term
// here we try different cummulative DELAYS
DAY :: 1440
LIST_PERIOD :: list<Time>(DAY, 2 * DAY, 4 * DAY, 6 * DAY)
LIST_PERIOD2 :: list<Time>(500,1000,1500,2000,2500,3000,4000,5000,7000,8000,9000,10000,11000)
[makeCummulative(t:TSerie,liftMode:integer) : Cummulative
  -> let tk := pickTracker(t,liftMode), best_d := 0, best_v := 0.0 in
        (for d in LIST_PERIOD
		   let v := matchSerie(t,cummulative(tk.value,d)) in
		     (if (v > best_v) (best_v := v, best_d := d)), 
         //[LDB] -- best duration = ~A mins gives correlation ~A // best_d, best_v,
         Cummulative(from = tk,
		     coeff = (if (corSerie(t,tk.value) > 0.0) 1.0 else -1.0) *
                             (stdev(t) / stdev(cummulative(tk.value,best_d))) * pb.fparam,
             memory = best_d)) ]
             
// try a simple generic fit fit with a twist
// use distance6
[fitCummulative(t:TSerie) : float
  -> fitCorrelation2(t) ]
             

// make a Treshold term
// here we try 3 relative levels 30,50,80% 
// we will let local optimization play with delays
LIST_LEVEL :: list<float>(0.2, 0.3,0.5,0.7)
[makeThreshold(t:TSerie,liftMode:integer) : Threshold
  ->  let tk := pickTracker(t,liftMode), best_l := 0.0, best_v := 0.0 in
        (for l in LIST_LEVEL 
	       let v := matchSerie(t,threshold(tk.value,l)) in
		(if (v > best_v) (best_v := v, best_l := l)),
        //[LDB] -- best treshold = ~A mins gives correlation ~A // best_l, best_v,
        Threshold(from = tk,
               delay = 0,
	          coeff = (if (corSerie(t,tk.value) > 0.0) 1.0 else -1.0) *
                           (stdev(t) / stdev(threshold(tk.value,best_l))) * pb.fparam,
              level = best_l)) ]

// try a simple generic fit fit with a twist
// use distance6
[fitThreshold(t:TSerie) : float
  -> let a := makeThreshold(t,0),
         b := makeConstant(t) in
      (distance1(a,t) / distance1(b, t)) ]
 
  //fitCorrelation2(t)  ]


// (4) lift a generic term ------------------------------	

// make a basic term with 40% Trend, 20% Hour, 20% Weekly,
[makeBasic(t:TSerie,liftMode:integer) : TSBasic
  -> let n := random(100) in
       (if (n < 20) makeAverage(t)
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


// growLift creates a special term
//   Sum ( gBasic, gDerived) | Sum (Sum(gBasic, gDerived), gDerivedBest)
//   gBasic = Mixo( Trend, Mixo(Weekly, Hourly))
//   gDerived = corr | tresh | mix

// top of tree
[growLift(t:TSerie,liftMode:integer) : TSTerm
  -> let a := growSum(t,liftMode),
         i := pickBestTracker(t) in
        (if (a.t2.from.index = i | random(100) < 60) a
         else let b := makeDerived(difference(t,a), 5 + i) in
               Sum(t1 = a, t2 = b)) ]

// standard Sum
[growSum(t:TSerie,liftMode:integer) : Sum
  -> let a := growBasic(t),
         b := makeDerived(difference(t,a), liftMode) in
        Sum(t1 = a, t2 = b) ]

// basic : start with a Trend
[growBasic(t:TSerie) : TSTerm
  -> let a := makeTrend(t) in
       (if (random(100) < 20) a
        else  let l := random(0.3,0.8),
                  nt := multiply(difference(t,multiply(tserie(a),l)),1.0 / (1.0 - l)),
	          b := growHourly(nt) in
           Mix(coeff = l, t1 = a, t2 = b)) ]

// optional hourly
// TODO : we should discard it based on its contribution
[growHourly(t:TSerie) : TSTerm
  -> if (random(100) < 40) makeWeekly(t)
     else let a := makeHourly(t) in
           (if (random(100) < 40) a
            else let l := random(0.4,0.8),
                     nt := multiply(difference(t,multiply(tserie(a),l)),1.0 / (1.0 - l)),
                     b := makeWeekly(nt) in
                     Mix(coeff = l, t1 = a, t2 = b)) ]


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



// *********************************************************************************************
// *     Part 5: Algebra Training Protocol                                                     *
// *********************************************************************************************

P2 :: Profile()
TT1:TSTerm := unknown
TT2:TSTerm := unknown
TT3:TSTerm := unknown
TT4:TSTerm := unknown

// ATP uses a special WHICH variable which tells which tracker we want to use
WHICH:integer := 1
[which(p:Profile) : TSerie
  -> if (WHICH = 1) p.target
     else if (WHICH = 2) p.trackers[1]
     else if (WHICH = 3) p.trackers[2]
     else  p.trackers[3]]

// hand tuning for each algebra operator (tune lift and fitness)
// this is similar to ctp and atp
// im is (1 to 9) : which algrebraic constructor we want to solve
// m tells how many opt loops we want to do.
// f is a float parameter that may also be used by the ML method
[atp(p:Profile,im:integer,m:integer, f:float)
  -> let mt := which(p),                             // master time-serie / reference for time
         n := p.target.count, ns := (2 * n / 3),     // debug (2 * n / 3),
         prev := unknown,              // previous term (for RIES)
         sum_d := 0.0, sum_f := 0.0, sum_c := 0.0, sum_dv := 0.0, sum_fit := 0.0 in
      (project(p,ns,P2),
       time_set(),
       for i in (ns .. (n - 1))   // usually n / 2
         let ms := measure!(p,min(n,i + 1)) in    // tuple of values
           (//[5] <<~A>> ======== start a tp at time  ~A // i, dateF(p.target.dates[i]),
            init(P2),
            let a := (if (im < 0) TT4 else liftAndOpt(P2,im,m)),
                fit := fitControl(which(P2),im),
                v := forecast(a,mt.dates[min(n,i + 1)]),
                d := distance3(a, which(P2)),                  // important : evaluate with h3
                c := delta(mt,which(P2).avgValue, ms[WHICH]),
                f := delta(mt,v, ms[WHICH]),
                dv := stdev%(a) in
              (//[0] <<~A>> ========== Algebra Test yields ~S:~A (~A/~A) // i, a, fit,v,ms[1],
               //[0]   {~A}  d=~A & f=~A (~A/~A) & std = ~A // dateF(mt.dates[min(n,i + 1)]),d,f,v,ms[1],dv,
               if (i = n) TT3 := a
               else (
               if (i = ns) TT1 := a
               else if (i = n - 1) TT2 := a,
               sum_dv :+ dv,
               sum_c :+ c,
               sum_d :+ d,
               sum_fit :+ fit,
               sum_f :+ f),
             if (i < n) addMeasure(P2,mt.dates[i + 1], ms))),
        let e := pb.experiment, k := n - ns in
           (e.count := float!(n),
            e.avgDist := sum_d / k,
            e.avgDelta := sum_f / k,
            e.avgVolatility := sum_c / k,
            e.avgDev := sum_dv / k,
            e.runTime := time_read() / k,
            e.avgFit := sum_fit / k,
            printf("ATP: average d = ~F3, average forecast delta= ~F% [volatility:~F%], emla time: ~Ams {~F%}\n",
                  e.avgDist,e.avgDelta, e.avgVolatility,e.runTime,e.avgDev),
            printf("average fit : ~F2 \n", e.avgFit),
            time_show())) ]

// debug
[atp(p:Profile, a:TSTerm)
  -> TT4 := a,
     atp(p,-1,100,0.0) ]

// lift and Opt
[liftAndOpt(p:Profile,i:integer,n:integer) : TSTerm
  ->  let t := which(p), a := makeControl(t,i) in
       (for j in (1 .. n)
          (a := opt1(a,t,false,3)),
        a) ]


// run the from the experiment
// creates a data file associated to the experiment
[run(e:AExperiment,i:integer)
  -> close(pb.origin),             // resets the profile
     loadFile(e,i),                 // read the data
     atp(pb.origin,e.control,e.nOpt,e.fparam),
     logResult(e) ]

// run 14 experiments
[irun(e:AExperiment)
  -> for i in (4 .. length(pb.fnames)) run(e,i) ]
 
// plays with the WHICH filter
[trun(e:AExperiment)
  -> for i in (4 .. length(pb.fnames))
       for k in (1 .. 4)
         (WHICH := k,
          run(e,i)),
     WHICH := 1]
 

// show the results
// load an experiment
[load(e:AExperiment) : void
  ->  e.avgFit := 0.0,
      load@Experiment(e),
      printf("=> average Fit = ~F3\n", e.avgFit / e.count)]

// save the results in a data file
// log a result in a log file - reusable pattern
[logResult(e:AExperiment) : void
  -> let p := fopen(Id(*where*) / "data" / "log" /+ string!(name(e)),"a") in
       (use_as_output(p),
        printf("//~S ~A {~A} :~A [~A] avgDelta, avgDist, avgVolatility, runtime, fitness\n",
               e, e.findex, WHICH, e.count, substring(date!(0),1,19)),
        printf("(logResult(~S,~F3,~F3,~F3,~F2,~F3))\n\n",
               e, e.avgDelta, e.avgDist, e.avgVolatility, e.runTime, e.avgFit),
        fclose(p)) ]

// store average Fit
SHOWLD:integer := 2
[logResult(e:AExperiment,v1:float,v2:float,v3:float,v4:integer,v5:float) : void
  -> if (v5 <= LCUT)
        (//[SHOWLD]  add(~S) avg delta = ~A, avg dev = ~A // e,v1,v3,
         logResult@Experiment(e,v1,v2,v3,v4),
         e.avgFit :+ v5) ]



