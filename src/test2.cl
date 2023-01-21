// +--------------------------------------------------------------------------------------------+
// |  EMLA                                                                                      |
// |  Evolutionary Machine Learning Agents                                                      |
// |  test.cl                                                                                   |
// |  Copyright (C) Yves Caseau 2016 - 2019                                                     |
// +--------------------------------------------------------------------------------------------+

(printf("------------- Interpeted Script Test2 files for EMLA version ~A = 0.4 -------------\n", Version))

// *********************************************************************************************
// *  Table of content                                                                         *
// *     Part 1: Data sets and EMLA settings                                                   *
// *     Part 2: CTP - Control Test Protocol                                                   *
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
// *     Part 1: Data sets and EMLA settings                                                   *
// *********************************************************************************************

DATADIR :: (".." / "data")

// testing
// FNAME1 :: Id(*where* / "data" / "DefaultJan2016.txt")
// note that the first 3 are irrelevant (not enough data)
FNAME1 :: (DATADIR / "DefaultJan2016.txt")
FNAME2 :: (DATADIR / "LHMay2016.txt")
FNAME3 :: (DATADIR /  "EnergyMay2016.txt")
FNAME4 :: (DATADIR / "DefaultMar2016.txt")
FNAME5 :: (DATADIR / "PaleoNov2016.txt")
FNAME6 :: (DATADIR / "EnergyNov2016.txt")
FNAME7 :: (DATADIR /  "MoodNov2016.txt")

FNAME8 :: (DATADIR / "EnergyMarch2017.txt")
FNAME9 :: (DATADIR / "ENERGYNov2017.txt")
FNAME10 :: (DATADIR /  "ENERGYSept2017.txt")
FNAME11 :: (DATADIR /  "INFLJuly2017.txt")
FNAME12 :: (DATADIR /  "MOODJune2017.txt")
FNAME13 :: (DATADIR /  "MOODMarch2017.txt")
FNAME14 :: (DATADIR /  "MOODSept2017.txt")
FNAME15 :: (DATADIR /  "PaleoJune2017.txt")
FNAME16 :: (DATADIR /  "PALEOSept2017.txt")
FNAME17 :: (DATADIR / "LHJuly2017.txt")

// added after the 2018 summer
FNAME18 :: (DATADIR /  "PALEOFeb2018.txt")
FNAME19 :: (DATADIR /  "MOODFeb2018.txt")
FNAME20 :: (DATADIR /  "ENERGYFeb2018.txt")

// added on January 13rd, 2019
FNAME21 :: (DATADIR /  "PALEOJune2018.txt")
FNAME22 :: (DATADIR /  "MOODJune2018.txt")
FNAME23 :: (DATADIR /  "RESTSept2018.txt")
FNAME24 :: (DATADIR /  "HEARTSept2018.txt")

// list of all files
(put(fnames,pb, list<string>( FNAME1,FNAME2,FNAME3,FNAME4,FNAME5,FNAME6,FNAME7,FNAME8,FNAME9,
                              FNAME10,FNAME11,FNAME12,FNAME13,FNAME14,FNAME15,FNAME16,FNAME17,
                              FNAME18,FNAME19,FNAME20,FNAME21,FNAME22,FNAME23,FNAME24)))

// there is a crazy bug with the MAC version .... not clear
// (a) what it is (we need a put vs write because the instantiation failed)
// (b) if this problem will hit me elsewhere
// (c) looks like an alignment bug(because of 64 bits)
// (d) to look into with care with a lot of debug print

// create a profile
Yves :: Profile(tags = list<string>("fitness","gluten","fat","sleep"))
(pb.origin := Yves)
T1:TSerie := unknown
T2:TSerie := unknown
T3:TSerie := unknown
T4:TSerie := unknown

// reads a knomee file  (debug)
[go(f:string)
  -> 
  //[0] welcome to CLAIRE, Yves ! will look at ~S // f,
  readFile(pb.origin,f),
  //[0] read file ~A // f,
  T1 := pb.origin.target,
  T2 := pb.origin.trackers[1],
  T3 := pb.origin.trackers[2],
  T4 := pb.origin.trackers[3],
  init(pb.origin),
  pb.experiment := E1,                 // default
  display(T1) ]


// load one file + prepare the pool
// then one can run test(*), tost(*), t*st or emla(n)
[go(i:integer)  -> go(pb.fnames[i])]


// reload (useful)
[rego(i:integer)
  -> close(pb.origin),
     go(i)]

// test depth of makeTST
[TD(n:integer, m:integer)
 -> pb.experiment.maxDepth := m,
    LIFTBOUND := m,
    let t := pb.origin.target, l := set<integer>() in
     (for i in (1 .. n)
       let a := lift(t,0), d := depth(a) in
        (printf("[~A/~A] ~S\n",d,m,a),
         l :add d),
      l) ]


// *********************************************************************************************
// *     Part 2: CTP - Control test protocol                                                   *
// *********************************************************************************************

// linear regression experiment
CE1 :: CExperiment(control = 1, param = 2000)

// k-means clustering
CE2 :: CExperiment(control = 2, param = 6)
CE3 :: CExperiment(control = 2, param = 10)
CE4 :: CExperiment(control = 2, param = 14)

// weighted Average
CE6 :: CExperiment(control = 3, param = 4, fparam = 1.0)
CE7 :: CExperiment(control = 3, param = 8, fparam = 1.0)
CE8 :: CExperiment(control = 3, param = 12, fparam = 1.0)
CE9 :: CExperiment(control = 3, param = 16, fparam = 1.0)
CE10 :: CExperiment(control = 3, param = 16, fparam = 0.95)
CE11 :: CExperiment(control = 3, param = 16, fparam = 0.9)
CE12 :: CExperiment(control = 3, param = 16, fparam = 0.6)
CE13 :: CExperiment(control = 3, param = 16, fparam = 0.6)

// ARMA experiments
CE20 :: CExperiment(control = 4, param = 2000)

// *********************************************************************************************
// *     Part 3: ATP - Algebra Test Protocol                                                   *
// *********************************************************************************************

// check all algebra lift
AE0 :: AExperiment(control = 0, nOpt = 0) // constant  => sets the benchmark at 18.6
AF0 :: AExperiment(control = 0, nOpt = 3) // 18.62

AE1 :: AExperiment(control = 1, nOpt = 0) // trend => 18.6
AF1 :: AExperiment(control = 1, nOpt = 3)  // 18.42

AE2 :: AExperiment(control = 2, nOpt = 0) // hourly - 18.99
AF2 :: AExperiment(control = 2, nOpt = 3) // 18.64

AE3 :: AExperiment(control = 3, nOpt = 0) // Weekly - 18.8
AF3 :: AExperiment(control = 3, nOpt = 3)  // 18.49

AE4 :: AExperiment(control = 4, nOpt = 0) // Average
AF4 :: AExperiment(control = 4, nOpt = 3)

AE5 :: AExperiment(control = 5, nOpt = 0) // Cycle 20.5
AF5 :: AExperiment(control = 5, nOpt = 3)  // 18.8

AE6 :: AExperiment(control = 6, nOpt = 0) // Cycle
AF6 :: AExperiment(control = 6, nOpt = 3)


// for the Constant + Correlation experiments, fparams tells what is the best variation
AE10 :: AExperiment(control = 10, nOpt = 0, fparam = 0.5) // Sum (Constant+ Correlation)
AE10a :: AExperiment(control = 10, nOpt = 0, fparam = 0.3) // Sum (Constant+ Correlation)
AE5b :: AExperiment(control = 10, nOpt = 0, fparam = 0.2) // Sum (Constant+ Correlation)

AE11 :: AExperiment(control = 11, nOpt = 0, fparam = 0.3) // Sum (Constant+ Cummul)


AE12 :: AExperiment(control = 12, nOpt = 0, fparam = 0.3) // Sum (Constant+ Threshold)



// *********************************************************************************************
// *     Part 4: ITP - Iterative Test Protocol                                                 *
// *********************************************************************************************

// experiments with makeControl
// to be continued to see if all lift versions work
// July 16th, 2018 - restart EMLA
// February 6th, 2019 -
E1 :: IExperiment(emla = 1, maxDepth = 5, fparam = 0.3, nOpt = 3, nLoop = 100)  // 18.1
E101 :: IExperiment(emla = 1, maxDepth = 5, fparam = 0.3, nOpt = 1, nLoop = 100)
E102 :: IExperiment(emla = 1, maxDepth = 5, fparam = 0.3, nOpt = 5, nLoop = 100)
// nOpt > 10 forces simple? to false
E103 :: IExperiment(emla = 1, maxDepth = 5, fparam = 0.3, nOpt = 13, nLoop = 100)

// play with SCORE%
E1a :: IExperiment(emla = 1, maxDepth = 5, fparam = 0.03, nOpt = 3, nLoop = 100, option = 3)
E1b :: IExperiment(emla = 1, maxDepth = 5, fparam = 0.03, nOpt = 3, nLoop = 100, option = 5)
E1c :: IExperiment(emla = 1, maxDepth = 5, fparam = 0.03, nOpt = 3, nLoop = 100, option = 10)

// RIES
E2 :: IExperiment(emla = 2, maxDepth = 5, fparam = 0.3, nOpt = 3, nLoop = 100)
E200 :: IExperiment(emla = 2,  maxDepth = 5, fparam = 0.3, nOpt = 2, nLoop = 150)
E201 :: IExperiment(emla = 2,  maxDepth = 5, fparam = 0.3, nOpt = 4, nLoop = 70)
E201 :: IExperiment(emla = 2,  maxDepth = 5, fparam = 0.3, nOpt = 5, nLoop = 50)

// playing with MAXDEPTH
E3 :: IExperiment(emla = 1, maxDepth = 7, fparam = 0.3,nOpt = 3, nLoop = 100)   // worse ! 18.46
E4 :: IExperiment(emla = 1, maxDepth = 9, fparam = 0.3,nOpt = 3, nLoop = 100)
E5 :: IExperiment(emla = 1, maxDepth = 6, fparam = 0.3,nOpt = 3, nLoop = 100)
E6 :: IExperiment(emla = 1, maxDepth = 4, fparam = 0.3,nOpt = 3, nLoop = 100)

// new experiments kRandOpt (copied from Knomee)
E11 :: IExperiment(emla = 3, maxDepth = 5, fparam = 0.3, fcut = 1000.0, nOpt = 3, nLoop = 100)



// play with dist6
// (DEVFACTOR := 0.5)
// E1  -> 18.1 (attention pas tres pur)
// E20 ->  18.1
// (DEVFACTOR := 1.5)
// E1 -> 18.2
// E20 -> 18.2
// (DEVFACTOR := 0.8) 18.2

// (DEVSTART := 0.01) // FALSE -> 18.2 and 18.2 // redo : 18.1
// (DEVSTART := 0.0) // FALSE -> 18.3 / 18.3 // redo: 18.4 and 18.3 for E21
// (DEVSTART := 0.03)

AA:TSTerm :: unknown

// learning from other trackers
[go(i:integer,j:(1 .. 4))
  -> go(i),
     NOSUM := true,
     pool((if (j = 1) Yves.target else Yves.trackers[j - 1])),
     emla(Yves,100),
     printf("NOSUM emla produce ~S for ~A[~A]\n",pb.scores[1],Yves.tags[j],j) ]


// simple: lauch RandOpt
[go1() -> go1(10)]

[go1(i:integer)
  -> let a := randopt(Yves,i) in
       (display(a))  ]


// first prototype of ITP
[go2(n:integer,m:integer)
  -> itp(Yves,n,m) ]

// go3(i) runs make-control on the main profile
[goControl(i:integer)
  -> control(Yves.target,i) ]

(printf("---  loaded without problem ---- \n"))





