// +--------------------------------------------------------------------------------------------+
// |  EMLA                                                                                      |
// |  Evolutionary Machine Learning Agents                                                      |
// |  test.cl                                                                                   |
// |  Copyright (C) Yves Caseau 2016 - 2017                                                     |
// +--------------------------------------------------------------------------------------------+

(printf("------------- Interpeted Script Test files for EMLA version ~A -------------\n", Version))

// *********************************************************************************************
// *  Table of content                                                                         *
// *     Part 1: Data sets and EMLA settings                                                   *                  *
// *     Part 2: CIC                                                                           *
// *     Part 3: Ken                                                                           *
// *     Part 4: Forecast                                                                      *
// *     Part 5: test                                                                          *
// *********************************************************************************************


/*
Tutorial (how to use this code)   - this is a good practice that should always be followed
- the files are defined in Part 1
- go(i) loads file #i
- go1(n) runs randopt



*/



// *********************************************************************************************
// *     Part 1: Data sets and EMLA settings                                                   *                  *
// *********************************************************************************************

// testing
FNAME1 :: Id(*where* / "data" / "DefaultJan2016.txt")
FNAME2 :: Id(*where* / "data" / "DefaultMar2016.txt")
FNAME3 :: Id(*where* / "data" /  "EnergyMay2016.txt")
FNAME4 :: Id(*where* / "data" / "LHMay2016.txt")


// EMLA settings (useful to compare)
E1 :: EMLA( diversity = 0.01, twoLoops = 3, oneLoops = 3, liftLoops = 10, maxDepth = 5)
E2 :: EMLA( diversity = 0.01, twoLoops = 3, oneLoops = 3, liftLoops = 10, maxDepth = 9)
E3 :: EMLA( diversity = 0.01, twoLoops = 3, oneLoops = 3, liftLoops = 20, maxDepth = 9)
E4 :: EMLA( diversity = 0.005, twoLoops = 3, oneLoops = 3, liftLoops = 10, maxDepth = 9)
E5 :: EMLA( diversity = 0.01, twoLoops = 3, oneLoops = 2, liftLoops = 10, maxDepth = 9)
E6 :: EMLA( diversity = 0.01, twoLoops = 10, oneLoops = 3, liftLoops = 10, maxDepth = 9)
E7 :: EMLA( diversity = 0.002, twoLoops = 10, oneLoops = 2, liftLoops = 10, maxDepth = 9)
E7a :: EMLA( diversity = 0.002, twoLoops = 10, oneLoops = 2, liftLoops = 10, maxDepth = 5)
E7b :: EMLA( diversity = 0.002, twoLoops = 10, oneLoops = 2, liftLoops = 10, maxDepth = 7)
E7c :: EMLA( diversity = 0.002, twoLoops = 10, oneLoops = 2, liftLoops = 10, maxDepth = 11)
E7d :: EMLA( diversity = 0.002, twoLoops = 10, oneLoops = 2, liftLoops = 10, maxDepth = 13)
E7e :: EMLA( diversity = 0.002, twoLoops = 10, oneLoops = 2, liftLoops = 10, maxDepth = 15)
(pb.emla := E7e)

// create a profile
Yves :: Profile(tags = list<string>("fitness","gluten","fat","sleep"))
T1:TSerie := unknown
T2:TSerie := unknown
T3:TSerie := unknown
T4:TSerie := unknown

// go : reads a file
[go(f:string)
  -> 
  //[0] welcome to CLAIRE, Yves ! will look at ~S // f,
  NOSPLIT := true,
  VAR := 3,                     // new best distance
  readFile(Yves,f),
  T1 := Yves.target,
  T2 := Yves.trackers[1],
  T3 := Yves.trackers[2],
  T4 := Yves.trackers[3],
  init(Yves),
  display(T1) ]

// load one file + prepare the pool
// then one can run test(*), tost(*), t*st or emla(n)
[go(i:integer)
  -> go(list(FNAME1,FNAME2,FNAME3,FNAME4)[i]),
     pb.sampleIndex := i,
     pool(T1) ]

AA:TSTerm :: unknown

// learning from other trackers
[go(i:integer,j:(1 .. 4))
  -> go(list(FNAME1,FNAME2,FNAME3,FNAME4)[i]),
     NOSUM := true,
     pool((if (j = 1) Yves.target else Yves.trackers[j - 1])),
     emla(Yves,100),
     printf("NOSUM emla produce ~S for ~A[~A]\n",pb.scores[1],Yves.tags[j],j) ]

// *********************************************************************************************
// *     Part 2: CIC                                                                           *
// *********************************************************************************************

// TODO : implement a simple CIC that is the difference between nothing (trend) and nothing plus X
// X being
// use randOpt with a special filter

// ----------------------- testing the CIC score ---------------------------------------------

//  to be tuned ...
[ecic(y:Profile, i:integer, n:integer) : float
  -> emla(y,n),
     let a1 := pb.pool[1], v1 := distance(a1,y.target) in
       (pb.tabu := i,
        pool(y.target),
        emla(y,n),
        let a2 := pb.pool[1], v2 := distance(a2,y.target) in
           (// v2 is expected to be more than v1 (worse match)
            pb.tabu := 0,   // back to default value
            trace(0,"without ~A, ~S->~A\nwith ~S->~A \n",i,a2,v2,a1,v1),
            100.0 * (v2 - v1) / v1)) ]
                      

// *********************************************************************************************
// *     Part 3: Ken                                                                           *
// *********************************************************************************************

// quality assessment (KEN) ---------------------------------------------

PROF:any :: unknown              // debug code : keep a pointer to the shifted profile
RANDOPT:boolean :: false         // debug code, to be removed => compare with randopt

// KEN is a quality assessment score, based on the classical dual testing
// this is the randomized version - call n time the kenLoop (which returns the score)
// shows the average score
[ken(y:Profile) -> ken(y,20,100) ]
[ken(y:Profile,n:integer,m:integer)
 -> let v := 0.0 in
       (time_set(),
       for i in (1 .. n)
         let ks := kenLoop(i,y,m) in
           (v :+ ks),
       v :/ float!(n),
       time_show(),
       printf("===ken score [~A x ~A] on sample~Ax~S -> ~F% {~I}\n",
               n,m, pb.sampleIndex,pb.emla, v, eProfile()))  ]


// remove 3 data points and see how we do find their values
// kenList returs the list of points to remove
// kenShit creates new profile (y2) with smaller data sets
[kenLoop(i:integer,y:Profile,m:integer) : float
  -> let l := kenList(y.target.count), y2 := kenShift(y,l) in
        (//[0] KEN test for ~S by removing ~A // y, l,
         kenLoop(i,y,y2,m)) ]

[kenLoop(i:integer,y:Profile,y2:Profile,m:integer) : float
  -> let v := 0.0, best_t := unknown in
       (random!(0),                        // improves repeatability
        init(y2),
        PROF := y2,
        pool(y2.target),
        if RANDOPT best_t := randopt(y2,m)
        else (emla(y2,m), best_t := pb.pool[1]),
        v := kenScore(best_t,y2.target,y.target),
        printf("[~A] ken score for ~S -> ~F% (vs ~F%)\n",i,best_t,v,kenTest(best_t,y2.target,y.target)),
        v)]



// create a tabu list of 3 index that should be excluded
KENMODE:integer :: 1        // variants
[kenList(n:integer) : list<integer>
  -> if (KENMODE = 1)
      list<integer>(random(2,n / 3), random(n / 3 + 1, (2 * n) / 3), random((2 * n) / 3 + 1, n - 1))
     else if (KENMODE  = 2)
      list<integer>(n / 3, (2 * n) / 3 , n)
     else if (KENMODE = 3)  list<integer>(n)
     else if (KENMODE = 4)  list<integer>(random(2,n)) ]

// create a alternate profile
[kenShift(y:Profile,l:list<integer>) : Profile
  -> let y2 := Profile(tags = y.tags) in
        (y2.target := kenShift(y.target,l),
         y2.trackers := list<TSerie>{kenShift(x,l) | x in y.trackers},
         y2) ]

// create a smaller tracker
[kenShift(t:TSerie,l:list<integer>) : TSerie
 -> let n := t.count - length(l), l1 := list<integer>(), l2 := list<float>() in
       (for i in (1 .. t.count)
          (if not(i % l)
              (l1 :add t.dates[i], l2 :add t.values[i])),
        TSerie(count = n, dates = l1, values = l2)) ]

// test measure : finds the values that are expected for t2 based on t1
[kenScore(a:TSTerm,t1:TSerie,t2:TSerie) : float
  -> let n := 0.0, v := 0.0 in
       (for d in t2.dates
          (if not(d % t1.dates)
             (n :+ 1.0,
              //[0] @~A: ~A vs ~A  // d, a[d], t2[d],
              if (a[d] < t1.minValue * 0.5 | v > t1.maxValue * 2.0)
                 error("out of range (~A - ~A)", t1.minValue,t1.maxValue ),
              v :+ abs(a[d] - t2[d])/ range(t2))),
        if (n > 0.0)  (v / n)
        else 0.0) ]


// test measure : finds the values that are expected for t2 based on t1
// the idea is to apply the same formula for the data points in the training set
[kenTest(a:TSTerm,t1:TSerie,t2:TSerie) : float
  -> let n := 0.0, v := 0.0 in
       (for d in t1.dates
             (n :+ 1.0,
              v :+ abs(a[d] - t2[d])/ range(t2)),
        if (n > 0.0)  (v / n)
        else 0.0) ]


// show the emlaProfile  (very useful when comparing EMLA strategies)
[eProfile()
  -> printf("DEPTH:~A SPLIT:~A  SUM:~A VAR:~A",pb.emla.maxDepth,(if NOSPLIT "NO" else "YES"),
            (if NOSUM "NO" else "YES"), VAR) ]


// debug with a special list
// this is useful to redo a special experiment
[kenTest(y:Profile,l:list<integer>,m:integer)
  -> let y2 := kenShift(y,l) in kenLoop(1,y,y2,m) ]

// test by removing one data point only
[ken1(y:Profile)
  -> KENMODE := 4,
     ken(y,10,100) ]

// this is the non random version (faster, assume N <=5)
[ken2(y:Profile)  -> ken2(y,5,100) ]
[ken2(y:Profile,n:integer,m:integer)
 -> let v := 0 in
      (for i in (1 .. n)
         let ks := kenLoop2(i,y,m) in
           (v :+ ks),
       v :/ float!(n),
       printf("===ken score 2 [~A] on sample~A -> ~F% {~I}\n",n, pb.sampleIndex, v, eProfile()))  ]

// remove 3 data points and see how we do find their values
[kenLoop2(i:integer,y:Profile,m:integer) : float
  -> let l := kenList2(y.target.count,i), y2 := kenShift(y,l) in
       kenLoop(i,y,y2,m) ]


// create a tabu list of 3 index that should be excluded
// SHIFT = i(iteration index) - 1  (start with 0)
[kenList2(n:integer,i:integer) : list<integer>
  ->  let k := i - 1 in list<integer>(n / 3 - k, (2 * n) / 3 - k, n - k)]


// ken3 is the "forecast the end point game" !
[ken3(y:Profile)
  -> KENMODE := 3,
     ken(y,20,100) ]

// gros bug
[bug()
  -> kenTest(Yves,list<integer>(6,15,16),100) ]

[bug2()
  -> kenTest(Yves,list<integer>(5,19,38),100) ]


// *********************************************************************************************
// *     Part 4: Forecast                                                                      *
// *********************************************************************************************


// create a alternate profile
[cutLast(y:Profile,k:integer) : Profile
  -> let y2 := Profile(tags = y.tags) in
        (y2.target := shift(y.target,k),
         y2.trackers := list<TSerie>{shift(x,k) | x in y.trackers},
         y2) ]

[cutLast(t:TSerie,k:integer) : TSerie
 -> let n := t.count - k in
      TSerie(count = n,
             dates = list<integer>{t.dates[i] | i in (1 .. n)},
             values = list<float>{t.values[i] | i in (1 .. n)})]

// ------------------------------------------ new code --------------------------------

/ our first forecast method
// take a profile and cut the last data points
[forecast(y:Profile,i:integer)
  -> let y2 := cutLast(y,i), n := pb.target.count + 1,
         d := y.target.dates[n - i], v := y.target.values[n - i] in
       (//[0] forcast for ~S on ~A measures // y, n - i,
        init(y2),
        pool(y2.target),
        emla(y2,100),
        AA := pb.pool[1],
        printf("AA predicts ~S versus ~S (~F%)\n",AA[d],v,abs(AA[d] - v)/ range(y.target))) ]

[range(t:TSerie) : float -> t.maxValue - t.minValue]


// compute a forecast estimate for each target
[estimate(y:Profile,i:integer)
  -> let y2 := y.target[i] in
       (//[0] estimate a forecast for ~S // i, 
        pool(y2),
        emla(y2,30),
        pb.forecasts[i] := pb.pool[1],
        printf("estimate traker[%A] (~F%)\n",i,pb.pool[1],pb.scores[1])) ]


// NEW code written during trip :)
; ajouter un champs   forecasts:list<TSTerm>
;,ajouter un champs index a TConstant


// global method for forecasting
[forecast(x:Time) : float 
  -> let y := pb.profile, n := ...
       (for i in (1 .. n) estimate(y,i)
	    let a := emla(y,50) in forecast(a,x)) ]



// how to compute a forecast
[forecast(a:TSTerm,x:Time) : float
  -> case a 
       (TSBasic a[x],
	    Split (if (x <= t.cutoff) forecast(t.t1,x) else forecast(t.t2,x)),
		Mix t.coeff * forecast(t.t1,x) + (1.0 - t.coeff) * forecast(t.t2,x),
		Sum forecast(t.t1,x) + forecast(t.t2,x),
		Cummulative a[x],                     // TODO : think about a better solution
		TConstant pb.forecast[a.index][x],
		Correlate let y := max(TimeOrigin,x - t.delay),
                       ts := t.from.value, z := forecast(t.from,y),  
                       avg := ts.avgValue in
                     (t.coeff * (z - avg)) ]
		Treshold let y := max(TimeOrigin,x - t.delay),
                      ts := t.from.value, z := forecast(t.from,y),
                      avg := ts.avgValue,
                      tv := t.level * max(ts.maxValue -  avg, avg - ts.minValue) in
                   (if (abs(z - avg) > tv) (z - avg) * t.coeff else 0.0)) ]

// *********************************************************************************************
// *     Part 5: test results                                                                  *
// *********************************************************************************************


// --------------------------- results ---------------------------------------------

// (go(FNAME2))

// step 1 : try lift
//                         FNAME 1                            FNAME2
// test(makeTrend,T1)   -> works  d= 0.10; corr = 0.277
// test(makeHourly,T1)  -> works! d =0.09; corr = 0.7
// test(makeWeekly,T1)  -> works! d= 0.09; corr = 0.49
// test(makeCycle,T1)   ->

// May 1st - test(N,T1) using lift
//              FNAME1                     FNAME2
//  200,5       d=0.053 corr=0.84        d=0.03, c=0.78
//  2000,5      d=0.047 corr=0.84        d=0.029 c=0.785

//  200,8       d=0.04  corr=0.865
//  2000,8      d=0.041 corr=0.865

// June 19th
// tests sur go(1)
// emla(E2) ->
//  PTYPE1  10 -> 0.04 en 4s; 100 -> 0.0105 en 44s; 0.0104 en 216s
//  PTYPE2                     100 -> 0.011 en 36s; 500 -> 0.011 en 216s
//  PTYPE3  10 -> 0.028 en 6 s; 100 -> 0.019 en 36s, 500 -> 0.011 en 200s
//  PTYPE4                      100 -> 0.022 en 34s, 500 -> 0.014 en 175s
//  PTYPE5                      100 -> 0.017 en 39s; 500 -> 0.0103 en 160s
// tost: 100 -> 0.028 en 1.7s ; 1000) -> 0.020104 en 18s; tost(10000) -> 0.0168 en 180s
// tast: 1000,3 -> 0.028 en 3s (no gain)
//       1000, 10 -> 0.027 en 57s
//       1000, 30 ->
// tust:  1000 ->


// tests sur go(2)
// emla(E2) ->
//    PTYPE 1 10:  0.015 en 6s   ; 100: 0.0144 en 94s ; 500 -> 0.144 en 500s
//    PTYPE 2 10:     ; 100: 0.0128 en 79s, 500 -> 0.009 en 400s
//    PTYPE 5         ; 100  0.0119 en 85s

// tost(1000) ->  0.0149 en 30s; 10000 -> 0.0149 en 300s
// tast:   1000,X ->

// tests sur go(3)
// emla(E2)
// PTYPE 1 -> 10  ;    100 :0.0004 en 41 s !
// PTYPE 5   100:0.00158 e, 37s
// tost : 100 -> 0.0033 en 3 s;   1000 -> 0.0031 en 30s
// emla beats random+ opt but a great factor !!


// test sur les facteurs de EMLA  (PTYPE1)
//                                              0.0105             0.0144              0.0004
// liftLoop = 20 (E3)       emla(100)   go(1):  0.011      go(2):  0.0168       go(3): 0.0007
// diversity = 0.02 (E4)    emla(100)           0.022              0.011               0.011
// diversity = 0.005                            0.0118             0.095               0.0002
// oneLoops = 5 (E5)        emla(100)   go(1):  0.019              0.012               0.0022
// oneLoops = 2                                 0.017              0.012               0.002
// twoLoops = 10 (E6)       emla(100)   go(1):  0.0104 44s         0.016               0.011
// combination (E7)                             0.008 30s          0.0078              0.001

// --------------- July 2/3 experiments :KEN score -----------------------------------------

// we use ken(Yves,10,100) as a basis (n = 20 gives more precise results)
// default profile = E7 - depth = 9
// tost experiments => set TOST to true
//                         go(1)                go(2)                  go(3)
//  emla(100)               28.7%               20.26%                  21.7%
//  emla(30)               19%                  23%                     23%
//  emla(200)              38% (overfitting)    19%                     22.9%
//  tost(100)              33%                  10%                     23%
//  tost(200) x 20         43%                  17%                     14.8%
//  emla(100) x 20         30.8%                20.8%                   15%
//  depth = 5 /7a          44%                  13%                     15%
//  depth = 7 /7b
//  depth = 11 /7c         29.7%                24.3                    19%
//  depth = 13 /7d         30.8%                19.9                    15%
//  depth = 15 /7e         36%                  16%                     17%
//  with SPLIT             31.2                 21                      16%
//  with SPLIT(7d)
//  NOSUM                  39%                  19%                     42%

// test avec ken(50)
// emla(100)  x E7e             37%             15.7                   15.2%
// emla(100)  x E7              31%             21.2                   22%
// emla(50)
// tost(100)

// July 14 : redo experiment with clean code (in the compiled module)
// WARNING : the ken code works when interpreted ... so GC bugs would be very bad news
// SHORT solution : put the code back in test
// ken(Yves,10, *)
// ken(Yves,20,100) set x E7
//                              go(1)           go(2)           go(3)
// emla(100)                    28%             20.6            19.6%
//                              430s           700s             37s
// avec E7e


// =====  still todo !!! ==============================================================


// test avec ken1  (1 versus 3)


// test avec ken2




