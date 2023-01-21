// +--------------------------------------------------------------------------------------------+
// |  EMLA                                                                                      |
// |  Evolutionary Machine Learning Agents                                                      |
// |  gauge.cl                                                                                  |
// |  Copyright (C) Yves Caseau 2016-2017                                                       |
// +--------------------------------------------------------------------------------------------+

// this file contains two basic ML methods to test  and use as a gauge of EMLA performance

// *********************************************************************************************
// *  Table of content                                                                         *
// *     Part 1: Profile to Matrix & Parameter Classes                                         *
// *     Part 2: Linear Regression with Gradient Descent                                       *
// *     Part 3: ARMA =  Auto-regressive Moving Average                                        *
// *     Part 4: k-Means                                                                       *
// *     Part 5: Results & ITP (Iterative Testing Protocol)                                    *
// *********************************************************************************************

// *********************************************************************************************
// *     Part 1: Profile to Matrix & Parameter Classes                                         *
// *********************************************************************************************

// here a vector is a list of floats
Vector :: list<float>

// simple matrix, M lines, N columns
Matrix <: object(
        n:integer,    // number of columns = number_examples
        m:integer,    // number of lines = number_factors
        values:list<Vector>)

[self_print(M:Matrix) -> printf("Matrix[~Ax~A]",M.m,M.n)]

// indexed access
[nth(M:Matrix,i:integer,j:integer) : float => M.values[i][j]]

// nice display on multiple lines
[display(M:Matrix)
  -> for i in (1 .. M.m)
       printf("~A:[ ~A]\n",i,M.values[i]) ]

// normalize the matrix so that the max value is 1
[normalize(M:Matrix) : Matrix
   -> for j in (1 .. M.n)
        let vmax := 0.0 in
           (for i in (1 .. M.m) vmax :max abs(M[i,j]),
            if (vmax > 0.0)
               (for i in (1 .. M.m) M.values[i][j] := (M.values[i][j] / vmax))),
       M ]

// simple version for a value from a tracker
[normalize(v:float,tr:TSerie) : float
  -> v / max(abs(tr.maxValue), abs(tr.minValue)) ]

// reverse
[denormalize(v:float,tr:TSerie) : float
  -> v * max(abs(tr.maxValue), abs(tr.minValue)) ]


// methods that are used
[dot(M:Matrix,V:Vector) : Vector
  -> let n := M.n in
       (if (length(V) != n) error("matrix x vector dimension error")
        else list<float>{sum(list<float>{ (M[i,j] * V[j]) | j in (1 .. n)}) | i in (1 .. M.m)}) ]

// transposition
[transpose(M:Matrix) : Matrix
  -> Matrix(m = M.n, n = M.m,
            values = list<Vector>{ list<float>{M[i,j] | i in (1 .. M.m)} |  j in (1 .. M.n)})]

// Vector operations
[-(v1:Vector,v2:Vector) : Vector
  -> let n := length(v1) in
      (if (length(v2) != n) error("vector difference dimension error")
       else list<float>{(v1[i] - v2[i]) | i in (1 .. n)}) ]

[*(v:Vector,z:float) : Vector  -> list<float>{ (x * z) | x in v} ]
[*(v1:Vector,v2:Vector) : float
   -> let n := length(v1) in
        (if (n != length(v2)) error("vector difference dimension error")
         else let s := 0.0 in (for i in (1 .. n) s :+ v1[i] * v2[i], s))  ]

[/(v:Vector,z:float) : Vector
  -> (if (z = 0.0) error("vector division bty zero error")
       else list<float>{ (x / z) | x in v}) ]

[ones(n:integer) : Vector -> list<float>{1.0 | i in (1 .. n)}]

// usual macro
[sum(l:list<float>) : float => let s := 0.0 in (for x in l s :+ x, s) ]


// extract a feature matrix from a profile (3 factor + hour + day)
[feature(p:Profile) : Matrix
   -> let nExamples := p.target.count,
          M :=  Matrix( n = nExamples, m = 4,
                 values = list<Vector>( ones(nExamples),
                                        p.trackers[1].values,
                                        p.trackers[2].values,
                                        p.trackers[3].values)) in
        normalize(transpose(M)) ]


// extract a target vector from a profile
[targetVector(p:Profile) : Vector -> p.target.values]



// weighted average
// TODO play with WAVG to see what works best
WAVG:float :: 2.0
[wAvg(l:Vector) : float
  -> let n := length(l), s := l[1] in
    (for i in (2 .. n)
       s := (s + WAVG * l[i]) / (1.0 + WAVG),
     s)]

// more sophisticated version that looks like what we use in the algebra
[wAvgValue(l:Vector,n:integer,d:float) : float
  -> let s1 := last(l), s2 := 1.0, m := length(l), f := d in
       (for i in (1 .. min(n - 1, m - 1))
           (s1 :+ f * l[m - i],
            s2 :+ f,
            f :* d),
        s1 / s2)]

// create a TSerie from a vector
[tSerie(p:Profile,v:Vector) : TSerie
  -> TSerie(    count = p.target.count,
                dates = p.target.dates,
                values = v) ]

// *********************************************************************************************
// *     Part 2: Linear Regression with Gradient Descent                                       *
// *********************************************************************************************

// smarter version with time info 4 + 6 + 7
// this code works both for LR and KM
[featureT(p:Profile, LR?:boolean) : Matrix
   -> let nExamples := p.target.count,
          M :=  Matrix( n = nExamples, m = 17,
                 values = list<Vector>( (if LR? ones(nExamples) else p.target.values),
                                        p.trackers[1].values,
                                        p.trackers[2].values,
                                        p.trackers[3].values,
                                        list<float>{(if (hour(t) % (0 .. 3)) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (hour(t) % (4 .. 7)) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (hour(t) % (8 .. 11)) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (hour(t) % (12 .. 15)) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (hour(t) % (16 .. 19)) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (hour(t) % (20 .. 23)) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 0) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 1) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 2) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 3) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 4) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 5) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 6) 1.0 else 0.0) | t in p.target.dates}
                                         )) in
        normalize(transpose(M)) ]

// this is tricky : extract a forecast vector in time
// this is used to forecast together with the LR thetas
[featureV(p:Profile,t:Time) : Vector
  -> list<float>(1.0,
                normalize(wAvg(p.trackers[1].values),p.trackers[1]),
                normalize(wAvg(p.trackers[2].values),p.trackers[2]),
                normalize(wAvg(p.trackers[3].values),p.trackers[3]),
                (if (hour(t) % (0 .. 3)) 1.0 else 0.0),
                (if (hour(t) % (4 .. 7)) 1.0 else 0.0),
                (if (hour(t) % (8 .. 11)) 1.0 else 0.0),
                (if (hour(t) % (12 .. 15)) 1.0 else 0.0),
                (if (hour(t) % (16 .. 19)) 1.0 else 0.0),
                (if (hour(t) % (20 .. 23)) 1.0 else 0.0),
                (if (weekDay(t) = 0) 1.0 else 0.0),
                (if (weekDay(t) = 1) 1.0 else 0.0),
                (if (weekDay(t) = 2) 1.0 else 0.0),
                (if (weekDay(t) = 3) 1.0 else 0.0),
                (if (weekDay(t) = 4) 1.0 else 0.0),
                (if (weekDay(t) = 5) 1.0 else 0.0),
                (if (weekDay(t) = 6) 1.0 else 0.0)) ]

// linear regression
// X is the input matrix, Y the desired outcome, nMax the max number of iteration,
// tolerance the min distance
[LR(X:Matrix, Y:Vector, nMax:integer, alpha:float, tolerance:float) : Vector
  -> let n := X.n, m := X.m,
         thetas := ones(n),
         M := transpose(X) in
        (for k in (1 .. nMax)
            let diffs := dot(X,thetas) - Y,
                cost := sum(list<float>{ (x * x) | x in diffs}) / float!(2 * m),
                gradient := dot(M, diffs) / float!(m) in
               (//[5] [~A] prediction is ~A // k, dot(X,thetas),
                //[5] [~A] diff is ~A // k, diffs,
                //[3] [~A] cost = ~A, theta = ~A // k, cost, thetas,
                //[5] gradient = ~A // gradient,
                thetas :- gradient * alpha,
                if (cost < tolerance) break()),
         thetas) ]

// *********************************************************************************************
// *     Part 3: Auto-regressive Moving Average                                                *
// *********************************************************************************************

// simple shift by k units
[shiftBy(t:TSerie,k:integer) : Vector
  -> let n := t.count in
       list<float>{ (if (i > k) t.values[i - k] else t.values[1])      |
                     i in (1 .. n)}]
[extractBy(t:TSerie,k:integer) : float
  -> let n := t.count in (if (n > k) t.values[n - k] else t.values[1])]


// ARMA adds a auto-regressive last part to the matrix
[armaT(p:Profile, LR?:boolean) : Matrix
   -> let nExamples := p.target.count,
          M :=  Matrix( n = nExamples, m = 24,
                 values = list<Vector>( (if LR? ones(nExamples) else p.target.values),
                                        p.trackers[1].values,
                                        p.trackers[2].values,
                                        p.trackers[3].values,
                                        list<float>{(if (hour(t) % (0 .. 3)) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (hour(t) % (4 .. 7)) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (hour(t) % (8 .. 11)) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (hour(t) % (12 .. 15)) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (hour(t) % (16 .. 19)) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (hour(t) % (20 .. 23)) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 0) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 1) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 2) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 3) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 4) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 5) 1.0 else 0.0) | t in p.target.dates},
                                        list<float>{(if (weekDay(t) = 6) 1.0 else 0.0) | t in p.target.dates},
                                        shiftBy(p.target,1),
                                        shiftBy(p.target,2),
                                        shiftBy(p.target,3),
                                        shiftBy(p.target,4),
                                        shiftBy(p.target,5),
                                        shiftBy(p.target,6),
                                        shiftBy(p.target,7)
                                         )) in
        normalize(transpose(M)) ]

// this is tricky : extract a forecast vector in time
// this is used to forecast together with the LR thetas
[armaV(p:Profile,t:Time) : Vector
  -> list<float>(1.0,
                normalize(wAvg(p.trackers[1].values),p.trackers[1]),
                normalize(wAvg(p.trackers[2].values),p.trackers[2]),
                normalize(wAvg(p.trackers[3].values),p.trackers[3]),
                (if (hour(t) % (0 .. 3)) 1.0 else 0.0),
                (if (hour(t) % (4 .. 7)) 1.0 else 0.0),
                (if (hour(t) % (8 .. 11)) 1.0 else 0.0),
                (if (hour(t) % (12 .. 15)) 1.0 else 0.0),
                (if (hour(t) % (16 .. 19)) 1.0 else 0.0),
                (if (hour(t) % (20 .. 23)) 1.0 else 0.0),
                (if (weekDay(t) = 0) 1.0 else 0.0),
                (if (weekDay(t) = 1) 1.0 else 0.0),
                (if (weekDay(t) = 2) 1.0 else 0.0),
                (if (weekDay(t) = 3) 1.0 else 0.0),
                (if (weekDay(t) = 4) 1.0 else 0.0),
                (if (weekDay(t) = 5) 1.0 else 0.0),
                (if (weekDay(t) = 6) 1.0 else 0.0),
                normalize(extractBy(p.target,1),p.target),
                normalize(extractBy(p.target,2),p.target),
                normalize(extractBy(p.target,3),p.target),
                normalize(extractBy(p.target,4),p.target),
                normalize(extractBy(p.target,5),p.target),
                normalize(extractBy(p.target,6),p.target),
                normalize(extractBy(p.target,7),p.target))]
              

// *********************************************************************************************
// *     Part 4: k-Means                                                                       *
// *********************************************************************************************

// using clustering for forecast is a bit strange .. the idea is to use what we have:
// time, day, previous values (3 previous measures = 15 value)  : total = 17 values

// L2 distance
[distL2(x:Vector,y:Vector) : float
  -> let s := 0.0, n := length(x) in
       (for i in (1 .. n) s :+ sqr(x[i] - y[i]),
        sqrt(s)) ]

// gets the list of Points and retun k (bounded by m)
// m points of size n, k centroids
[kMean(S:list<Vector>,k:integer,nMax:integer) : list<Vector>
  -> let m := length(S),
         k2 := min(m,k),
         lUsed := list<integer>{0 | i in (1 .. k2)},
         lAssign := list<integer>{0 | i in (1 .. m)},
         C := list<Vector>(),
         n := 1 in
       (// random assignment of first k2 centroids
        while (n <= k2)
          let j := random(1,m) in
             (if exists(i in (1 .. (n - 1)) | lUsed[i] = j) nil
              else (lUsed[n] := j,
                    C :add S[j],
                    n :+ 1)),
        // assign
        let w := 0.0  in   // total weight
          (for i in (1 .. m)
              (lAssign[i] := nearest(S[i],C),
               //[5] nearest(~A) is ~A // S[i],lAssign[i],
               w :+ distL2(S[i],C[lAssign[i]])),
           //[1] start with weight = ~A (assign:~A) // w / float!(m),lAssign,
           //kDisplay(k2,lAssign),
           for p in (1 .. nMax)
             (C := Centroids(S,lAssign,k2),
              let change := false in
                (w := 0.0,
                 for i in (1 .. m)
                    let j := nearest(S[i],C) in
                       (if (j != lAssign[i])
                           (change := true, lAssign[i] := j),
                        w :+ distL2(S[i],C[lAssign[i]])),
                 //[1] iteration[~A] w = ~A // p, w / float!(m),
                 if not(change) break())),
           //[1] k-means (dim ~A) ends at weight = ~A // length(S[1]), w / float!(m),
           // kDisplay(k2,lAssign),
           C)) ]

// useful for debug
[kDisplay(k:integer,lAssign:list<integer>)
  -> for i in (1 .. k) printf("C[~A] = ~A \n",i,list{j in (1 .. length(lAssign)) | lAssign[j] = i}) ]
                     
// creates a strange bug in CLAIRE 4 
//[bar(k:integer,lAssign:list<int>)
// -> for i in (1 .. k) printf("C[~A] = ~A \n",i,list{j in (1 .. length(lAssign)) | lAssign[j] = i}) ]

// computes the centroids
// m points of size n, k centroids
[Centroids(S:list<Vector>,lAssign:list[integer],k:integer) : list<Vector>
  -> let n := length(S[1]), m := length(S),
         C := list<Vector>{list<float>{0.0 | i in (1 .. n)} | j in (1 .. k)},
         count := list<float>{0.0 | j in (1 .. k)} in
       (//[1] start computing centroids C = ~A // C,
        for i in (1 .. m)
          let j := lAssign[i] in
             (count[j] :+ 1.0,
              //[5] update C[~A] = ~A // j, C[j],
              for w in (1 .. n) C[j][w] :+ S[i][w]),
        for i in (1 .. k)
          (if (count[i] > 0.0)
              (for w in (1 .. n) C[i][w] :/ count[i])),
        //[1] end updating Centroids C = ~A // C,
        C) ]



// returns the nearest centroid for a point
[nearest(x:Vector,C:list<Vector>) : integer
  -> let dmin := 1e10, bestk := 0 in
       (for i in (1 .. length(C))
          let d := distL2(x,C[i]) in (if (d < dmin) (dmin := d, bestk := i)),
        bestk) ]



// simple test
// k: number of centroid, nMax : number of iterations
[KM(p:Profile,k:integer,nMax:integer)
  -> let S := featureT(p,false).values,
         C := kMean(S,k,nMax) in
       printf("Clustering returns ~A @ weight ~A -> dist=~A\n",C, weight(S,C), dist(S,C)) ]

// debug : recompute weight
[weight(S:list<Vector>, C:list<Vector>) : float
  -> let s := 0.0 in
        (for x in S
           let dmin := 1e10 in
             (for i in (1 .. length(C))
               let d := distL2(x,C[i]) in (if (d < dmin) dmin := d),
              s :+ dmin),
         s / float!(length(S))) ]


// forecast is actually easy : find the best cluster
[forecastKM(p:Profile,t:Time,m:integer) : tuple(float,float)
   -> let S := featureT(p,false).values,         // without normalization
          C := kMean(S,m,20),
          vf := featureV(p,t),                  // timed vector -  first feature is 1.0 : ignored
          bestC := nearestBut(vf,C),
          fval := denormalize(C[bestC][1],p.target) in
       (//[0] Forecast => ~A x ~A => ~A // vf, bestC,  fval,
        tuple(dist(S,C),fval)) ]



// returns the nearest centroid for a point
[nearestBut(x:Vector,C:list<Vector>) : integer
  -> let dmax := 1e10, bestk := 0 in
       (//[0] nearestBut on X size ~A // length(x),
        for i in (1 .. length(C))
          let d := distBut(x,C[i]) in (if (d < dmax) (dmax := d, bestk := i)),
        bestk) ]

// L2 distance but first coordinate
[distBut(x:Vector,y:Vector) : float
  -> let s := 0.0, n := length(x) in
       (for i in (2 .. n) s :+ sqr(x[i] - y[i]),
        sqrt(s)) ]


// performance metric on fist feature
[dist(S:list<Vector>, C:list<Vector>) : float
  -> let s := 0.0 in
        (for x in S
           let dmin := 1e10, bestk := 0  in
             (for i in (1 .. length(C))
               let d := distL2(x,C[i]) in (if (d < dmin) (dmin := d,bestk := i)),
              s :+ abs(x[1] - C[bestk][1])),
         s / float!(length(S))) ]        

// *********************************************************************************************
// *     Part 4: Results & ITP (Iterative Testing Protocol)                                    *
// *********************************************************************************************

// simple test
[LR(p:Profile,n:integer)
  -> let X := featureT(p,true), Y := targetVector(p) in
      (//[0] here is the matrix //,
       display(X),
       //[0] Y = ~A // Y,
       let thetas := LR(X,Y,n,0.1,0.02),
           result := tSerie(p,dot(X,thetas)) in
         (printf("final distance is ~A\n",dist(p.target,result)),
          display(result,p.target))) ]

// debug values
THETAS:list<float> := list<float>()
RESULT:TSerie := unknown

// test with a point from the learning set
[LRtest(p:Profile,i:integer)
  -> let X := featureT(p,true), Y := targetVector(p) in
      (//[0] here is the matrix //,
       display(X),
       //[0] Y = ~A // Y,
       let thetas := LR(X,Y,2000,0.1,0.02),
           result := tSerie(p,dot(X,thetas)) in
         (printf("final distance is ~A\n",dist(p.target,result)),
          RESULT := result,
          THETAS := thetas,
          display(result,p.target),
          let val := p.target.values[i],
              vf := featureV(p,i) in
            printf("forecast[i] =>  ~A vs ~A \n", vf * thetas,val)))]


// forecast test : returns the training distance, the predicted value
[forecastLR(p:Profile,t:Time,m:integer) : tuple(float,float)
  -> let X := featureT(p,true), Y := targetVector(p),
         thetas := LR(X,Y,m,0.1,0.02),
         result := tSerie(p,dot(X,thetas)),
         vf := featureV(p,t) in
       (//[3] Forecast => ~A x ~A => ~A // vf, thetas,  vf * thetas,
        tuple(dist(p.target,result),vf * thetas)) ]

// ARMA variant
[forecastARMA(p:Profile,t:Time,m:integer) : tuple(float,float)
  -> let X := armaT(p,true), Y := targetVector(p),
         thetas := LR(X,Y,m,0.1,0.02),
         result := tSerie(p,dot(X,thetas)),
         vf := armaV(p,t) in
       (//[0] ARMA Forecast => ~A x ~A => ~A // vf, thetas,  vf * thetas,
        tuple(dist(p.target,result),vf * thetas)) ]


// trivial application of weighted average
[forecastWAVG(p:Profile,t:Time,m:integer,d:float) : tuple(float,float)
  -> tuple(0.0, wAvgValue(p.target.values,m,d)) ]


// here we want to measure two things : the value of the fit and the prediction
// this is equivalent version of itp
// we use the same P2 profile to produce subsets (projections) and predict the next value
// im is one of the standard (1:LR, 2:k-mean, 3:w-avg, 4: ARMA)
// m is the parameter passed to the ML method
// f is a float parameter that may also be used by the ML method
[ctp(p:Profile,im:integer,m:integer, f:float)
  -> let mt := p.target,               // master time-serie / reference for time
         n := p.target.count,
         ns := (2 * n / 3),            // v0.4 : test 2N/3 to N (faster tests)
         prev := unknown,              // previous term (for RIES)
         sum_d := 0.0, sum_f := 0.0, sum_c := 0.0 in
      (//[0] ********* start ctp: test ~A with ~A values // pb.experiment.findex, n,
       project(p,n / 2,P2),
       time_set(),
       for i in ((n / 2) .. (n - 1))
         let ms := measure!(p,i + 1) in    // tuple of values
           (//[0] <<~A>> ======== ctp at time  ~A // i, dateF(p.target.dates[i]),
            init(P2),
            let (d,v) :=
                 (if (im = 1) forecastLR(P2,mt.dates[i + 1],m)
                  else if (im = 2) forecastKM(P2,mt.dates[i + 1],m)
                  else if (im = 3) forecastWAVG(P2,mt.dates[i + 1],m,f)
                  else forecastARMA(P2,mt.dates[i + 1],m)) in
            let c := delta(mt,P2.target.avgValue, ms[1]),
                f := delta(mt,v, ms[1]) in
              (//[0] <<~A>> ========== LR Forecast yield ~A versus ~A (d=~A) // i, v, ms[1], d,
               sum_c :+ c,
               sum_d :+ d,
               sum_f :+ f),
             addMeasure(P2,mt.dates[i + 1], ms)),
        let e := pb.experiment, k := n - (n / 2) in
           (e.count := float!(n),
            e.avgDist := sum_d / k,
            e.avgDelta := sum_f / k,
            e.avgVolatility := sum_c / k,
            e.runTime := time_read() / k,
            printf("TP: average d = ~F3, average forecast delta= ~F% [volatility:~F%], emla time: ~Ams\n",
                  e.avgDist,e.avgDelta, e.avgVolatility,e.runTime),
            time_show())) ]

// run the from the experiment
// creates a data file associated to the experiment
// k is not used
[run(e:CExperiment,i:integer,k:integer)
  -> close(pb.origin),             // resets the profile
     loadFile(e,i),
     ctp(pb.origin,e.control,e.param,e.fparam),
     logResult(e) ]

     
// save the results in a data file
// log a result in a log file - reusable pattern
[logResult(e:CExperiment) : void
  -> let p := fopen(Id(*where*) / "results" / "log" /+ string!(name(e)),"a") in
       (use_as_output(p),
        printf("//~S ~A:~A [~A] avgDelta, avgDist, avgVolatility, runtime\n",
               e, e.findex, e.count, substring(date!(0),1,19)),
        printf("(logResult(~S,~A,~F3,~F3,~F3,~F2))\n\n",
               e, e.findex,e.avgDelta, e.avgDist, e.avgVolatility, e.runTime),
        fclose(p)) ]

// load an experiment
[load(e:Experiment) : void
  ->  e.count := 0.0,
      e.avgDelta := 0.0,
      e.avgDist := 0.0,
      e.avgVolatility := 0.0,
      e.runTime := 0,
      load(Id(*where*) / "results" / "log" /+ string!(name(e))),
      printf("=== Experiment ~S, ~A runs [cut:~F2] ===\n",e,e.count, LCUT),
      printf("=> average delta=~F2%, distance=~F3, stedv=~F2% time = ~Ams\n",
               (e.avgDelta / e.count), e.avgDist / e.count, e.avgVolatility / e.count,
               float!(e.runTime) / e.count)]

// load and filter according to a cut value
LCUT:float :: 1e9
[lcut(e:Experiment, f:float) : void
  ->  LCUT := f,
      load(e) ]

// when called, simply add to the stats
// i is the file number (useful for cuts)
[logResult(e:Experiment,i:integer,v1:float,v2:float,v3:float,v4:integer) : void
  -> e.count :+ 1.0,
     e.avgDelta :+ v1,
     e.avgDist :+ v2,
     e.avgVolatility :+ v3,
     e.runTime :+ v4]

// when called, simply add to the stats
// i is the file number (useful for cuts)
[logResult(e:Experiment,v1:float,v2:float,v3:float,v4:integer) : void
  -> e.count :+ 1.0,
     e.avgDelta :+ v1,
     e.avgDist :+ v2,
     e.avgVolatility :+ v3,
     e.runTime :+ v4]


