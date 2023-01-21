// +--------------------------------------------------------------------------------------------+
// |  EMLA                                                                                      |
// |  Evolutionary Machine Learning Agents                                                      |
// |  evol.cl                                                                                   |
// |  Copyright (C) Yves Caseau 2016-2019                                                       |
// +--------------------------------------------------------------------------------------------+

// algebraic operations : methods that are defined for each term of the algebra
// edited on Octobre 21st

// *********************************************************************************************
// *  Table of content                                                                         *
// *     Part 1: Optimize                                                                      *
// *     Part 2: evolutionary algorithm                                                        * 
// *     Part 3: control methods                                                               *
// *     Part 4: Forcasting, CIC and Ken                                                       *
// *     Part 5: Algebra Training Protocol (atp & itp)                                         *
// *     Part 6: Experiment management                                                         *
// *********************************************************************************************

// v0.4

// *********************************************************************************************
// *     Part 1: optimize                                                                      *
// *********************************************************************************************
 
// in v0.1 with implement simple, one slot at a time, dichotomous hill-climbing
// in v0.1 we add a simple form of two opt: mutate + fix.
[opt1(a:TSTerm,t:TSerie,simple?:boolean,n:integer) : TSTerm
  -> setGoal(t),
     opt1(a,simple?,n),
     a]

// this is the key to evaluate
[setGoal(t:TSerie) : void
  -> pb.target := t]

// we declare the term that we want to optimize (will enable sub-term optimization)  
[setTerm(a:TSTerm) : void
  -> pb.optTerm := a]

// this is called runLoop as a reminder of the GTES heritage :)
// simply computes the distance between the optimized term and the target TSerie
[runLoop() : float
  -> pb.cursat := evaluate(pb.optTerm,pb.target),
     pb.cursat]

// version for 2opt
[runLoop(a:TSTerm) : float -> setTerm(a), runLoop()]

// opt1 is our simple hill-climbing borrowed from mms/socc
// we use the five abstract methods defined in part I
OPTI:integer :: TRACE        // trace parameter for optimization functions
ODB:integer :: TRACE         // debug trace level
NUM1:integer :: 7            // number of dichotomic steps (to be tuned)

// local opt main loop (Hill-climbing) -----------------------------------------------

// debug ugly  = remove, but only when everything is broken
OPC:integer :: 0
OPE:integer :: 100000000
OPX:any :: unknown

// optimise a TSTerm with hillclimbing for each of the slot
// opt1 is the method that we call from EMLA
[opt1(a:TSTerm,simple?:boolean,n:integer) : void
   -> pb.muteI := 0,        // avoid tabu-blockage
      OPC :+ 1,
      if (OPC >= OPE) (//[0] here it comes =============================//,
                       OPX := a, OPTI := 0, ODB := 0),
      setTerm(a),
      let scStart := runLoop(a) in
         (for i in (1 .. n) optimize(a,simple?),
          if (runLoop(a) > scStart) error("opt is broken for ~S",a)),
      ck(a,"opt1") ]
      
	  
// this is the internal version that is called directly by twoOpt	  
// this works as is for Tbasic (and will be called for TDerived)
[optimize(a:TSTerm,simple?:boolean)
  -> let v1 := runLoop() in              // used to reset cursat
        (for p in (1 .. nSlot(a))
	   (if (pb.muteI = p) nil  // tabu
            else (if not(simple?) optimize2(a,p),
                  optimize(a,p))),
         ck(a,"optimize 1"),
         trace(OPTI,"--- end optimize(~S) -> ~A [from ~A]\n",a,pb.cursat,v1)) ]

// first approach : relative steps (=> does not cross the 0 boundary, keeps the sign) ----------

// optimize a given slot in a set of two dichotomic steps
[optimize(a:TSTerm,p:TAG)
  -> for i in (1 .. NUM1) optimize(a,p,float!(2 ^ (i - 1))),
     ck(a,"optimize 2"),
     trace(OPTI,"best ~A for ~S is ~A => ~A\n",
           label(a,p),a,read(a,p), pb.cursat) ]

// try to add / retract a (multiplying) increment
// check that dynamic min/max create a true window !
[optimize(a:TSTerm,p:TAG,r:float)
   -> //[ODB] ..... start optimize(~S) : ~A @ ~A // a,label(a,p),r,
      OPX := a,
      let m := min(a,p) as float, M := max(a,p) as float,
	  vp := read(a,p), vr := pb.cursat, val := 0.0,
          v1 := vp / (1.0 +  (1.0 / r)), v2 := vp * (1.0 + (1.0 / r)) in
        (ck2(a,m,M,p,"optimize"),
         v1 := min(M,max(m,v1)),
         //[ODB] try ~A(~S)= ~A in (~A -- ~A) // label(a,p),a,v1,m,M,
	 write(a,p,v1),
         val := runLoop(),
         //[ODB] try ~A (vs.~A) for ~A(~S) -> ~A (vs. ~A)// v1,vp,label(a,p),a,val,vr,
         if (val < vr) (vp := v1, vr := val),
         v2 := min(M,max(m,v2)),
	 write(a,p,v2),
         val := runLoop(),
         //[ODB] try ~A for ~A(~S) -> ~A // v2,label(a,p),a,val,
         if (val < vr) (vp := v2, vr := val),
         write(a,p,vp),
         pb.cursat := vr) ]


// absolute variant --------------------------------------------------------------------

// optimize a given slot in a set of two dichotomic steps
[optimize2(a:TSTerm,p:TAG)
  -> for i in (1 .. NUM1) optimize2(a,p,float!(2 ^ (i - 1))),
     trace(OPTI,"[2] best ~A for ~S is ~A => ~A\n",
           label(a,p),a,read(a,p), pb.cursat) ]

SEED:float :: 0.1
[optimize2(a:TSTerm,p:TAG,r:float)
   -> //[ODB] ..... start optimize2(~S) : ~A @ ~A // a,label(a,p),r,
      let m := min(a,p) as float, M := max(a,p) as float,
	      vp := read(a,p), vr := pb.cursat, val := 0.0,
          v1 := vp +  (SEED / r), v2 := vp - (SEED / r) in
        (ck2(a,m,M,p,"optimize2"),
         v1 := min(M,max(m,v1)),
	 write(a,p,v1),
         val := runLoop(),
         //[ODB] try ~A (vs.~A) for ~S(~S) -> ~A (vs. ~A)// v1,vp,label(a,p),a,val,vr,
         if (val < vr) (vp := v1, vr := val),
         v2 := min(M,max(m,v2)),
	 write(a,p,v2),
         val := runLoop(),
         //[ODB] try ~A for ~S(~S) -> ~A // v2,label(a,p),a,val,
         if (val < vr) (vp := v2, vr := val),
         write(a,p,vp),
         pb.cursat := vr) ]
		 
// extensions for composed terms
// v0.4 : simpler => there was a bug in v0.3
[optimize(a:Mix,simple?:boolean) : void
  -> optimize(a.t1,simple?),
     optimize(a.t2,simple?),
     optimize(a,1) ]

[optimize(a:Sum,simple?:boolean) : void
  -> optimize(a.t1,simple?),
     optimize(a.t2,simple?) ]


[optimize(a:Split,simple?:boolean) : void
  -> optimize(a.t1,simple?),
     optimize(a.t2,simple?),
     if not(simple?) optimize(a,1) ]


// *********************************************************************************************
// *     Part 2: RIES: evolutionary algorithm                                                  * 
// *********************************************************************************************

// RIES : Randomized Incremental Evolutionary Search
// todo: add mutation
[ries(y:Profile,n:integer,prevA:TSTerm) : TSTerm
  -> let t := y.target, best_d := 1e8, best_t := unknown in
    (//[0] === RIES : first part: tune the previous term ==== //,
     let val1 := evaluate(prevA,t),
         a := opt1(prevA,t, true,pb.experiment.nOpt) in
       (//[0] first step opt(prev) from ~A to ~A // val1,evaluate(a,t),
        best_t := a,
        best_d := evaluate(a,t)),
      //[0] second step : randopt to get another term //,
      let b := randopt(y,n - 1) in
         (//[0] randopt returns d = ~A // evaluate(b,t),
          if (evaluate(b,t) < best_d)
             (//[0] second step yields an improvement ! //,
              best_d := evaluate(b,t), best_t := b)),
      //[0] last iteration (should use mutate) @ ~A // best_d,
      let c := opt1(best_t,t,true,pb.experiment.nOpt) in
        (//[0] opt returns d = ~A // evaluate(c,t),
          if (evaluate(c,t) < best_d)
             (//[0] last optimization was useful @ ~A // evaluate(c,t),
              best_d := evaluate(c,t), best_t := c)),
        best_t as TSTerm) ]


// run n times a loop of random creation (lift) followed by an optimization term
// returns the best found term
// v0.4 DEBUG
[randopt(y:Profile,n:integer) : TSTerm
   -> let t := y.target, best_d := 1e8, best_t := unknown,
          simple? := (pb.experiment.nOpt < 10),
          nop := (pb.experiment.nOpt mod 10) in
        (OPTI := 5,
         for i in (1 .. n)
           let a := lift(t), b := opt1(a,t,false,nop) in
             (//[1] [~A] ~S {~A} => dis=~A, cor=~A// i, a, evaluate(a,t), evaluate(b,t), corr(tserie(a),t),
              if (evaluate(b,t) < best_d)
                 (best_d := evaluate(b,t), best_t := b)),
         best_t as TSTerm) ]

// add a second deeper optim pass at the end
[rand2opt(y:Profile,n:integer) : TSTerm
  -> let t := y.target, a := randopt(y,n),
         b := opt1(a,t,false,pb.experiment.nOpt) in
    (printf("--- rand2Opt from ~A to ~A\n",evaluate(a,t),evaluate(b,t)),
     b)]


// SWIFT version (current Knomeecode - used with 3 extra loops
// todo : see if simple? should be true or false
[kRandopt(y:Profile,n:integer) : TSTerm
   -> let n := pb.experiment.nOpt in
        (pb.experiment.nOpt := 1,
         opt1(randopt(y,n),y.target,false,n)) ]


// display the best term
[display(a:TSTerm) : void
 -> let t2 := tserie(a), t := pb.target in
     (//[0] EMLA produces ~S{~A} at evaluate ~A (corr = ~A)// a, depth(a), evaluate(a,t), corr(t2,t),
      display(t2,t)) ]

      
// *********************************************************************************************
// *     Part 3: control methods                                                               *
// *********************************************************************************************

// init the pool from a profile
[init(p:Profile)
  -> pb.profile := p,
     pb.timeList := p.target.dates,  // this is the reference list of date (do not modify)
     TimeOrigin := car(pb.timeList),
     minMax(p.target),
     for ts in p.trackers minMax(ts),
     // set up boundaries for trend generation
     pb.trendMin := 0.7 * p.target.minValue,
     pb.trendMax := 1.3 * p.target.maxValue,
     // create a timeScale
     pb.timeScale := list<float>{float!(x) | x in pb.timeList} ]

// display a profile (always nice)
[see(p:Profile) : void
  -> printf("=== display Target : ~A =============\n~I\n\n", p.tags[1],display(p.target)),
     printf("=== display tracker1 : ~A (corr : ~F%) =============\n~I\n\n",
            p.tags[2],corr(p.target,p.trackers[1]),display(p.trackers[1])),
     printf("=== display tracker1 : ~A (corr : ~F%) =============\n~I\n\n",
            p.tags[3],corr(p.target,p.trackers[2]),display(p.trackers[2])),
     printf("display tracker1 : ~A (corr : ~F%) =============\n~I\n",
            p.tags[4],corr(p.target,p.trackers[3]),display(p.trackers[3]))]
    

// *********************************************************************************************
// *     Part 4: CCI and iKen                                                                  *
// *********************************************************************************************


// *********************************************************************************************
// *     Part 5: Algebra Training Protocol (atp & itp)                                         *
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
         sum_d := 0.0, sum_f := 0.0, sum_c := 0.0, sum_dv := 0.0 in
      (project(p,ns,P2),
       time_set(),
       for i in (ns .. (n - 1))   // usually n / 2
         let ms := measure!(p,min(n,i + 1)) in    // tuple of values
           (//[5] <<~A>> ======== start a tp at time  ~A // i, dateF(p.target.dates[i]),
            init(P2),
            let a := (if (im < 0) TT4 else liftAndOpt(P2,im,m)),
                v := forecast(a,mt.dates[min(n,i + 1)]),
                d := wDistance(a, which(P2)),                  // important : evaluate with h3
                c := delta(mt,which(P2).avgValue, ms[WHICH]),
                f := delta(mt,v, ms[WHICH]),
                dv := stdev%(a) in
              (//[0] <<~A>> ========== Algebra Test yields ~S (~A/~A) // i, a, v,ms[1],
               //[0]   {~A}  d=~A & f=~A (~A/~A) & std = ~A // dateF(mt.dates[min(n,i + 1)]),d,f,v,ms[1],dv,
               if (i = n) TT3 := a
               else (
               if (i = ns) TT1 := a
               else if (i = n - 1) TT2 := a,
               sum_dv :+ dv,
               sum_c :+ c,
               sum_d :+ d,
               sum_f :+ f),
             if (i < n) addMeasure(P2,mt.dates[i + 1], ms))),
        let e := pb.experiment, k := n - ns in
           (e.count := float!(n),
            e.avgDist := sum_d / k,
            e.avgDelta := sum_f / k,
            e.avgVolatility := sum_c / k,
            e.avgDev := sum_dv / k,
            e.runTime := time_read() / k,
             printf("ATP: average d = ~F3, average forecast delta= ~F% [volatility:~F%], emla time: ~Ams {~F%}\n",
                  e.avgDist,e.avgDelta, e.avgVolatility,e.runTime,e.avgDev),
            time_show())) ]

// debug : run atp on a given term
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
[run(e:AExperiment,i:integer,k:integer)
  -> close(pb.origin),             // resets the profile
     loadFile(e,i),                 // read the data
     atp(pb.origin,e.control,e.nOpt,e.fparam),
     logResult(e) ]


// show the results
// load an experiment
[load(e:AExperiment) : void
  ->  e.avgFit := 0.0,
      load@Experiment(e),
      printf("=> average Fit = ~F3\n", e.avgFit / e.count)]

// save the results in a data file
// log a result in a log file - reusable pattern
[logResult(e:AExperiment) : void
  -> let p := fopen(Id(*where*) / "results" / "log" /+ string!(name(e)),"a") in
       (use_as_output(p),
        printf("//~S ~A {~A} :~A [~A] avgDelta, avgDist, avgVolatility, runtime, fitness\n",
               e, e.findex, WHICH, e.count, substring(date!(0),1,19)),
        printf("(logResult(~S,~A,~F3,~F3,~F3,~F2,~F3))\n\n",
               e, e.findex, e.avgDelta, e.avgDist, e.avgVolatility, e.runTime, e.avgFit),
        fclose(p)) ]

// store average Fit
SHOWLD:integer := 2
[logResult(e:AExperiment,v1:float,v2:float,v3:float,v4:integer,v5:float) : void
  -> if (v5 <= LCUT)
        (//[SHOWLD]  add(~S) avg delta = ~A, avg dev = ~A // e,v1,v3,
         logResult@Experiment(e,v1,v2,v3,v4),
         e.avgFit :+ v5) ]

[logResult(e:AExperiment,i:integer,v1:float,v2:float,v3:float,v4:integer,v5:float) : void
  -> logResult(e,v1,v2,v3,v4,v5) ]



// this is is the key part ! incremental testing protocol (ITP)

TST1:TSTerm :: unknown          // new : run ITP protocol on a basic term
TST2:TSTerm :: unknown          // stores the last infered term into a constant

// this is the major contribution of v2 : ITP : Incremental Testing Protocol
// im: emla indice 1: randopt, 2:Ries 3:kRandOpt
// m is a parameter that is passed to the EMLA method (here randopt)
[itp(p:Profile,im:integer,m:integer)
  -> let mt := p.target,               // master time-serie / reference for time
         n := p.target.count,  ns := (2 * n / 3), // max(_, n - 30),    // simple cut for large files
         prev := unknown,              // previous term (for RIES)
         sum_d := 0.0, sum_f := 0.0, sum_c := 0.0, sum_dv := 0.0, sum_dp := 0 in
      (project(p,ns,P2),
       time_set(),
       m := pb.experiment.nLoop,           // NEW : support experiment control
       for i in (ns .. (n - 1))
         let ms := measure!(p,i + 1) in    // tuple of values
           (//[0] <<~A>> ======== start itp at time  ~A // i, dateF(p.target.dates[i]),
            init(P2),
            let a := (if (im = 0) TST1
                      else if (im = 1 | (im = 2 & prev = unknown)) randopt(P2, m)
                      else if (im = 2)  ries(P2, m, prev)
                      else if (im = 3 ) kRandopt(P2,m)
                      else rand2opt(P2,m)),
                v := forecast(a,mt.dates[i + 1]),
                d := dist(tserie(a),P2.target),
                // d := wDistance(a, p.target),                  // important : evaluate with h3
                c := delta(mt,P2.target.avgValue, ms[1]),
                f := delta(mt,v, ms[1]),
                dv := stdev%(a) in
              (//[0] <<~A>> ========== end of EMLA yields ~S // i, a,
               //[0]   {~A}  d=~A & f=~A & std = ~A (depth ~A) // dateF(mt.dates[i + 1]),d,f,dv,depth(a),
               TST2 := a,
               prev := a,
               sum_dv :+ dv,
               sum_c :+ c,
               sum_d :+ d,
               sum_dp :+ depth(a),
               sum_f :+ f),
            addMeasure(P2,mt.dates[i + 1], ms)),
        let e := pb.experiment, k := n - ns in
           (e.count := float!(n),
            e.avgDist := sum_d / k,
            e.avgDelta := sum_f / k,
            e.avgVolatility := sum_c / k,
            e.runTime := time_read() / k,
            e.avgDev := sum_dv / k,
            e.avgDepth := float!(sum_dp / k),
            printf("ITP: average d = ~F3, average forecast delta= ~F% [vol:~F%], emla time: ~Ams\n",
                  e.avgDist,e.avgDelta, e.avgVolatility,e.runTime),
            printf("END result : d = ~F2, stdev =~F% (vf ~F%),  d= ~F2\n",
                  evaluate(TST2, P2.target),e.avgDev, stdev%(mt), e.avgDepth),
            time_show())) ]


// project a profile onto another one
[project(p:Profile, n:integer, p2:Profile) : void
  -> p2.target := project(p.target, n),
     p2.trackers := list<TSerie>{ project(t,n) | t in p.trackers},
     p2.tags := p.tags  ]


// *********************************************************************************************
// *     Part 6: Experiment management                                                         *
// *********************************************************************************************



Measure :: list<float>
// reads a measure from a profile
[measure!(p:Profile, i:integer) : Measure
  -> cons(which(p).values[i], list<float>{ t.values[i] | t in p.trackers}) ]


// we add a measure (tuple of values) to a profile
[addMeasure(p:Profile, x:Time, m:Measure) : void
  -> addValue(p.target, x, m[1]),
     for i in (1 .. length(p.trackers)) addValue(p.trackers[i],x, m[i + 1])  ]

// add a value to a TSerie
[addValue(ts:TSerie, x:Time, v:float) : void
  -> ts.count :+ 1,
     ts.dates :add x,
     ts.values :add v,
     minMax(ts)]


// how to compute a forecast
[forecast(a:TSTerm,x:Time) : float
  -> case a 
       (TSBasic a[x],
	    Mix a.coeff * forecast(a.t1,x) + (1.0 - a.coeff) * forecast(a.t2,x),
    	Split (if (x <= a.cutoff) forecast(a.t1,x) else forecast(a.t2,x)),
	    Sum forecast(a.t1,x) + forecast(a.t2,x),
	    Cummulative a[x],                     // TODO : think about a better solution
	    TSConstant trackCast(a.index,a.value,x),       // use a dumb version from the TSerie
	    Correlate let y := max(TimeOrigin,x - a.delay),
                      ts := a.from.value, z := forecast(a.from,y),
                      avg := ts.avgValue in
                     (a.coeff * (z - avg)),
	    Threshold let y := max(TimeOrigin,x - a.delay),
                      ts := a.from.value, z := forecast(a.from,y),
                      avg := ts.avgValue,
                      tv := a.level * max(ts.maxValue -  avg, avg - ts.minValue) in
                   (if (abs(z - avg) > tv) (z - avg) * a.coeff else 0.0),
        any 0.0) ]


// this is the dumb version
FMODE:integer :: 1    // 1 : previous value, 2: average, 3: exact value(debug)
CHEAT:list<TSerie> :: list<TSerie>()
[trackCast(i:integer,t:TSerie,x:Time) : float
  -> if (FMODE = 1)  t[x]
     else if (FMODE = 2) t.avgValue
     else CHEAT[i][x] ]

// mesure the relative error of a forecast
// big change in EMLA 0.3 : relative to maxValue and not  delta(min,max)
[delta(ts:TSerie, x:float, y:float) : float
  -> abs(x - y) / (ts.maxValue - ts.minValue) ]


// default profile
P1 :: Profile(tags = list<string>("target","track1","track2","track3"))

// run an experiment
// TODO: change with two object
// E1 : defines the experience
// em : defines the algo 1: randomp, 2: ries, 3:full emla
[run(e:IExperiment,i:integer,n:integer)
  ->  LIFTBOUND := e.maxDepth,
      readFile(P1,pb.fnames[i]),
      e.findex := i,
      SPLIT% := e.option,         // v0.4 - we can use e.option for whatever
      init(P1),
      CHEAT := P1.trackers,
      display(P1.target),
      pb.experiment := e,
      pb.fcut := e.fcut,
      //[0] start running ~S with EMLA:~A intensity ~A // e,e.emla,n,
      itp(P1,e.emla,n),
      logResult(e,n) ]

// default version
[run(e:IExperiment, i:integer) -> run(e,i,100)]

// run 13 experiments  (better done with a bash batch)
[irun(e:IExperiment)
  -> for i in (4 .. length(pb.fnames)) run(e,i) ]

// save the results in a data file
// log a result in a log file - reusable pattern
// here we also note the number of iteration (that may be overloaded)
[logResult(e:IExperiment,n:integer) : void
  -> let p := fopen(Id(*where*) / "results" / "log" /+ string!(name(e)),"a") in
       (use_as_output(p),
        printf("//~S ~A :~A [~A] emla=~A, nIter=~A, ~A, maxDepth=~A/~A, runtime=~As\n",
               e, e.findex, e.count, substring(date!(0),1,19),e.emla,n,oString(e.option),
               e.avgDepth,LIFTBOUND, e.runTime / 1000),
        printf("(logResult(~S,~A,~F3,~F3,~F3,~F2,~F3))\n\n",
               e, e.findex, e.avgDelta, e.avgDist, e.avgVolatility, e.runTime, e.avgDev),
        fclose(p)) ]

// store average Fit
[logResult(e:IExperiment,v1:float,v2:float,v3:float,v4:integer,v5:float) : void
  -> logResult@Experiment(e,v1,v2,v3,v4),
     e.avgDev :+ v5 ]

[logResult(e:IExperiment,i:integer,v1:float,v2:float,v3:float,v4:integer,v5:float) : void
  -> logResult(e,v1,v2,v3,v4,v5)]

// ------------------------- our reusable trick -------------------------

[ld() : void -> load(Id(*src* / "emlav" /+ string!(Version) / "test2")) ]
[sld() : void -> sload(Id(*src* / "emlav" /+ string!(Version) / "test2")) ]

// we load a file of interpreted code which contains the program description
(#if (compiler.active? = false | compiler.loading? = true) (try ld() catch any nil) else nil)
// (ld())

