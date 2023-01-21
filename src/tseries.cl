// +--------------------------------------------------------------------------------------------+
// |  EMLA                                                                                      |
// |  Evolutionary Machine Learning Agents                                                      |
// |  TSeries.cl                                                                                |
// |  Copyright (C) Yves Caseau 2016                                                            |
// +--------------------------------------------------------------------------------------------+

// *********************************************************************************************
// *  Table of content                                                                         *
// *     Part 1: Date utilities                                                                *
// *     Part 2: Read Knomee files                                                             *
// *     Part 3: Display Time Series & Utilities                                               *
// *     Part 4: Distance and correlation                                                      *
// *     Part 5: Test code                                                                     *
// *********************************************************************************************

// *********************************************************************************************
// *     Part 1: Date utilities                                                                *
// *********************************************************************************************

// this is something that iOS does well :)

MinPerDay :: (24 * 60)
YearOrigin :: 2015

// bisextile years - add the complete rule later on ...
[bisextile?(y:integer) : boolean 
  -> (y mod 4 = 0)]

// number of days between first of this month and begining of the year
DaysInMonth :: list(31,28,31,30,31,30,31,31,30,31,30,31)
[dayStartMonth(m:integer,y:integer) : integer
 -> let n := 0 in
      (for i in (1 .. (m - 1)) n :+ DaysInMonth[i],
       if (bisextile?(y) & m > 2) n + 1 else n) ]

// number of days since the origin
[dayStartYear(y:integer) : integer
  -> let n := 0 in
      (for i in (YearOrigin .. (y - 1)) n :+ (if bisextile?(i) 366 else 365),
       n) ]

// create a date from  y,m,d
[date(y:integer,m:integer,d:integer) : Time
  -> (dayStartYear(y) + dayStartMonth(m,y) + (d - 1)) * MinPerDay]

// reverse function : get the year, month, day component from a date
[year(d:Time) : integer
   -> let y := YearOrigin + (d / (365 * MinPerDay)) in    // may overestimate
         (if (date(y,1,1) > d) (y - 1) else y)]

// computes the month: 12/5/2019 -> 5
[month(d:Time) : integer
  -> let y := year(d), m := 1 in
       (for x in (1 .. 12)
         let d2 := date(y,x,1) in
            (if (d2 > d) break()
             else m := x),
        m) ]

// extract the daily calendar : 12/5/2019 -> 12
[day(d:Time) : integer
  -> let y := year(d), m := month(d), d2 := date(y,m,1) in
        ((d - d2) / MinPerDay + 1) ]

[hour(d:Time) : integer
 -> let x := (d mod MinPerDay) in (x / 60) ]

[minute(d:Time)  : integer
 -> let x := (d mod MinPerDay) in (x mod 60)]

// weekDay  (fixed in v0.4 from datathon)
ReferenceSunday :: date(2015, 1,4)
daySinceOrigin(x:Time) : integer => (x / MinPerDay)
[weekDay(x:Time) :  integer // (0 .. 6) creates a stupid check_in
  -> let d1 := 7 + daySinceOrigin(x), d2 := daySinceOrigin(ReferenceSunday) in
      ((d1 - d2) mod 7)]
 //       (((d1 - d2) mod 7) as (0 .. 6))

// print a date
[dateString(x:Time) : string
  -> string!(year(x)) /+ "/" /+ string2!(month(x)) /+ "/" /+ string2!(day(x)) ]

// print the time of day
[timeString(x:Time) : string 
  -> string2!(hour(x)) /+ ":" /+ string2!(minute(x)) ]

// print with two digits
[string2!(x:integer) : string
  -> let s := string!(x) in (if (x < 10) ("0" /+ s) else s) ]

// TODO - to have a nice library
// currentTime() : Time -> ... from date!(0)

// full date
[dateF(x:Time) : string 
 -> dateString(x) /+ " " /+ timeString(x) ]

// useful for debug
[see(t:TSerie) : void
  -> for i in (1 .. t.count)
       printf("~A: ~F2\n",dateF(t.dates[i]),t.values[i]) ]

// *********************************************************************************************
// *     Part 2: Read Knomee files                                                             *
// *********************************************************************************************

// this is the old format that we want to read
// 2016/01/05 10:35 7,278 2,722 3,097 7,021
// the new format is CSV + full date in reverse format + long/lat that are ignored + dot for floats
// 11/28/17 16:00, 3.09949, 45.77654, 4.1, 7, 8030, 2

// read an integer (this is stupid since the code exists in CLAIRE)
// first get next char
PORT:port :: unknown
CHAR:char :: ' '
INTCHAR :: (integer!('0') .. integer!('9'))
EOL :: char!(10) // for windows
RET :: char!(13) // for UNIX

// three methods to read a char from the file - first one expects a char (EOF => error)
[nextChar() : void
   -> CHAR := getc(PORT),
      //[5] nextchar: ~A: ~A // CHAR,integer!(CHAR),
      if (CHAR = EOF) error("end of file while reading")]

// read the nextchar but ignores white spaces and CR
[skipChar() : char
   -> while (CHAR = ' ' | CHAR = RET ) CHAR := getc(PORT),
      //[5] skipchar: ~A: ~A // CHAR,integer!(CHAR),
      CHAR]

[getChar(): void
   -> CHAR := getc(PORT) ]
      

// read the next integer in the stream
[nextInt() : integer
  -> let num := 0 in
        (while not(integer!(CHAR) % INTCHAR) nextChar(),
         while (integer!(CHAR) % INTCHAR)
            (num := (integer!(CHAR) - integer!('0')) + num * 10,
             nextChar()),
         num)]

// useful to see that we get 
[checkChar(c:char) : void
  -> if (CHAR != c) error("missing ~S as expected",c)
     else nextChar() ]

// combo
[nextInt(c:char) : integer
  -> let x := nextInt() in
       (checkChar(c), x)]

// read a date (YYYY/MM/DD) or (MM/DD/YY)
// we compute the number of minutes elapsed since the reference time (Jan 1st, 0:00, YearOrigin)
// TODO : fix Knomee to generate a 4digit year !
[nextDate() : Time
  -> let year := nextInt('/'),
         month := nextInt('/') in
       (if (year > 2000)    // old format (nextInt = day)
           (MinPerDay * ((nextInt() - 1) + dayStartMonth(month,year) + dayStartYear(year)))
        else
          let day2 := month, month2 := year, year2 := nextInt() in
           (if (year2 < 2000) year2 :+ 2000,
            MinPerDay * ((day2 - 1) + dayStartMonth(month2,year2) + dayStartYear(year2)))) ]
      

// read a time  (XX:YY)
[nextTime() : Time
  -> let hour := nextInt(':') in
        ( nextInt() + 60 * hour) ]

// read a float in a fucking x,y format or x.y format
// a first comma is ignored because nextInt is a bully :)
// csv = true implies that
[nextFloat(csv:boolean) : float
  -> let x := float!(nextInt()) in
        (if ((CHAR = ',' & not(csv)) | CHAR = '.') x + fractional(nextInt())
         else x) ]


// read a fractional part
[fractional(x:integer) : float 
  -> let y := float!(x) in
       (while (y >= 1.0) y :/ 10.0,
        y)]


// read a line and produce a tuple (Date,hour,val1 to val 4)
[readLine(p:Profile)
  -> let csv := false,
         date := nextDate(),
         time := nextTime(),
         values := list<float>() in
            (if (CHAR = ',')  // new format  => setup csv
                (csv := true,
                 nextFloat(csv), nextFloat(csv)),  // ignore lat & long
             while (skipChar() != EOL) values :add nextFloat(csv),
             //[5] read so far ~A ~A : ~S // dateString(date), timeString(time), values,
             addValue(p,date + time,values),
             getChar()) ]

[readFile(p:Profile, file:string)
  -> PORT := fopen(file,"r"),
     CHAR := ' ',
     while (skipChar() != EOF) readLine(p),
     fclose(PORT),
     printf("read -> ~A time values\n",p.target.count) ]

// adds a lines of values into a profile's trackers
[addValue(p:Profile,d:Time,values:list<float>)
  -> if (length(values) != length(p.trackers) + 1) error("wrong data format for ~S",p),
     insert(p.target,d,values[1]),
     for i in (1 .. length(p.trackers)) insert(p.trackers[i],d,values[i + 1]) ]


// load test file - (note: debug equivalent is go(i))
[loadFile(e:Experiment,i:integer) : void
  -> readFile(pb.origin,pb.fnames[i]),
     init(pb.origin),
     pb.experiment := e,
     e.findex := i,
     pb.fparam := e.fparam ]
          
   
// *********************************************************************************************
// *     Part 3: Display Time Series & Utilities                                               *
// *********************************************************************************************

WIDTH :: 90         // size of CLAIRE windows
HEIGHT :: 20        // HEIGHT of 
GRID :: list<string>{make_string(WIDTH,' ') | i in (1 .. HEIGHT)}

// clear the pixel grid
[clearGrid()
  -> for i in (1 .. HEIGHT)
       for j in (1 .. WIDTH) GRID[i][j] := ' ']


// display a TSerie using the grid
[display(t:TSerie) : void
   -> minMax(t),
      clearGrid(),
      fillGrid(t,'*',t.minValue,t.maxValue),
      displayGrid(t)]

// display the grid and the legend at the bottom
[displayGrid(t:TSerie) : void
  ->  for i in (1 .. HEIGHT)
        printf("~I~A\n", (if (i = 1) princ("\n^") else princ('|')), GRID[HEIGHT + 1 - i]),
      printf("+~I>\n",(for i in (1 .. WIDTH) princ('-'))),
      printf(" ^~A~I~A^\n",dateString(t.dates[1]),
                           (for i in (1 .. (WIDTH - 22)) princ(' ')),
                           dateString(last(t.dates))),
      printf("* mean:~F3\tmin:~F3@~A\tmax:~F3@~A\n",mean(t), t.minValue,dateString(t.minDate),
                                           t.maxValue,dateString(t.maxDate)) ]


// normalizer : take a float value x between xMin and xMax and return an index between 1 and yMax
[normalizer(x:float,xMin:float,xMax:float,yMax:integer) : integer
  -> 1 + integer!(((x - xMin) / (xMax - xMin)) * (float!(yMax) - 1e-9)) ]

[normalizer(x:Time,xMin:Time,xMax:Time,yMax:integer) : integer
  -> 1 + integer!((float!(x - xMin) / float!(xMax - xMin)) * (float!(yMax) - 1e-9)) ]


// fill the grid with the TSerie curve
// (a) compute the average normalized distribution
// (b) fills each cell with the designated char (e.g., '*')
// m is the min value, M is the max value
[fillGrid(t:TSerie,c:char,m:float,M:float) : void
  -> let val := list<float>{0.0 | i in (1 .. WIDTH)},
         cnt := list<float>{0.0 | i in (1 .. WIDTH)} in
       (for i in (1 .. t.count)  // (a)
          let x := normalizer(t.dates[i],t.dates[1],t.dates[t.count],WIDTH) in
             (val[x] :+ t.values[i], cnt[x] :+ 1.0),
        for x in (1 .. WIDTH)    // (b)
          (if (cnt[x] > 0.0)
              let y := normalizer((val[x] / cnt[x]),m,M,HEIGHT) in
                 GRID[y][x] := c)) ]


// cute : double display : t1 is the one we are interested in, t2 is the target
[display(t1:TSerie,t2:TSerie) : void
   -> minMax(t1),
      minMax(t2),
     let m := min(t1.minValue,t2.minValue),
          M := max(t1.maxValue,t2.maxValue) in
        (clearGrid(),
         fillGrid(t2,'o',m,M),
         fillGrid(t1,'*',m,M),
         displayGrid(t1))]
                 

// other utilities -------------------------------------------------------------

// difference between two time series
[difference(t:TSerie,a:TSerie) : TSerie
  ->  TSerie(count = t.count,
             dates = t.dates,
	     values = list<float>{ (t.values[i] - a.values[i]) | i in (1 .. t.count)})]

// difference between a TS and a TST
[difference(t:TSerie,a:TSTerm) : TSerie
  ->  TSerie(count = t.count,
             dates = t.dates,
			values = list<float>{ (t.values[i] - a[t.dates[i]]) | i in (1 .. t.count)})]

// multiply a Time Serie by a scalar
[multiply(t:TSerie,a:float) : TSerie
  ->  TSerie(count = t.count,
             dates = t.dates,
			values = list<float>{ (t.values[i] * a ) | i in (1 .. t.count)})]

// shift a time series by a delay
[shift(t:TSerie,d:Time) : TSerie
  ->  TSerie(count = t.count,
             dates = t.dates,
	     values = list<float>{ t[t.dates[i] - d] | i in (1 .. t.count)})]

// this is a partial integration method : t,d -> t_d(x) = Int(from y = x - d to y = x, t(y)dy)
// notice that we only integrate (f(x) - avg(x)) 
[cummulative(t:TSerie,d:Time) : TSerie
  -> TSerie(count = t.count,
            dates = t.dates,
	    values = list<float>{ (let x := t.dates[i], y := max(0, x - d) in  // was t.dates[1]
                                     (integral(t,y,x) - t.avgValue * (x - y)))  | i in (1 .. t.count)})]


// this is a thresholding function : only keeps extremum values from average
[threshold(t:TSerie,percent:float) : TSerie
  -> let avg := t.avgValue,
         tv := percent * max(t.maxValue - avg, avg - t.minValue) in
     TSerie(	count = t.count,
				dates = t.dates,
				values = list<float>{ (let x := t.values[i]  in  // was t.dates[1]
                                     (if (abs(x - avg) > tv) (x - avg) else 0.0)) | i in (1 .. t.count)})]

// split a TS into a sub TS according to two indexes
[split(t:TSerie,a:integer,b:integer) : TSerie
  -> TSerie(count = b - a + 1,
            dates = list<Time>{ t.dates[i] | i in (a .. b)},
	    values = list<float>{ t.values[i] | i in (a .. b)}) ]

// project a TS onto the time domain of a smaller one (t2) - but t2.dates is a subset
[project(t1:TSerie,t2:TSerie) : TSerie
  -> let l := list<float>(), m := t2.dates[1], M := t2.dates[t2.count] in
       (for i in (1 .. t1.count)
         (if (t1.dates[i] >= m & t1.dates[i] <= M) l :add t1.values[i]),
        assert(length(l) = t2.count),
        TSerie(count = t2.count, dates = t2.dates,values = l)) ]

// project a TS onto (1 .. n) : keep the n first values
[project(t1:TSerie,n:integer) : TSerie
  -> let l1 := list<integer>(), l2 := list<float>() in
       (for i in (1 .. n)
            (l1 : add t1.dates[i], l2 :add t1.values[i]),
        TSerie(count = n, dates = l1, values = l2)) ]


// v0.3 : two new TSeries operation = weighted average and AND combination

// this is a smart implementation (incremental) - cf wAvgValue in model
// key: weighted average from the past!
// l2[j] = sum(i in 1 .. n | l[j - i] * d^(n - i)) / sum(i, d^(n - i))
[wAvg(ts:TSerie,n:integer,d:float) : TSerie
  -> if (ts.count < 2 | ts.count <= n) ts    // avoid stupid bugs
     else let  m := ts.count, s1 := ts.values[m - 1], s2 := 1.0,  f := 1.0,
          l := list<float>{0.0 | i in (1 .. m)}, j := m - 1 in
       (// initalize s1 and s2  - last value computation => fits in l2[m]
        for i in (1 .. min(n - 1, m - 1))
           (f :* d,                            // incremental compupation of d^m-j
            s1 :+ f * ts.values[m - (i + 1)],
            s2 :+ f),
        l[m] := (s1 / s2),
        while (j > 0)
          let cv := ts.values[j],               // last value of previous serie
              nv := ts.values[max(1, j - n)] in // first value of new serie
            (s1 := ((s1 - cv) / d) + (f * nv),
             l[j] := (s1 / s2),
             j :- 1),
        TSerie(count = m, dates = ts.dates, values = l))]

// discounted average (factor d) of n last measures of a time serie
// read at a given point of time -> from the pas
[wAvgValue(ts:TSerie,n:integer,d:float,x:Time) : float
  -> let  m := ts.count, past := m in
       (while (past > 0)
            (if (ts.dates[past] < x) break()
             else past :- 1),
        if (past = 0) ts.avgValue       // louche ... but avoids screwing the error measure when debug
        else let s1 := ts.values[past], s2 := 1.0, f := d in
            (for i in (1 .. min(n - 1, past - 1))
                (s1 :+ f * ts.values[past - i],
                s2 :+ f,
                f :* d),
            s1 / s2)) ]

// and is a simple multiplication of variance (value - avg)
[and(t1:TSerie,t2:TSerie) : TSerie
       -> let l := list<float>(), avg1 := t1.avgValue, avg2 := t2.avgValue in
       (for i in (1 .. t1.count)
          l :add (t1.values[i] - avg1) * (t2.values[i] - avg2),
        assert(length(l) = t2.count),
        TSerie(count = t2.count, dates = t2.dates,values = l)) ]

// *********************************************************************************************
// *     Part 4: Distance and correlation                                                      *
// *********************************************************************************************

// Note: this are not simple series, these are time series (the closer the points, the smaller their
// weights)

// integrate between two boundaries
// far from optimal !
[integral(t:TSerie,a:Time,b:Time) : float
  -> let sum := 0.0, n := t.count in
       (if (a < t.dates[1]) sum :+ t.values[1] * (min(b,t.dates[1]) - a),  // left part of the integral
        if (b > t.dates[n]) sum :+ t.values[n] * (b - max(a,t.dates[n])),  // right part
        for i in (2 .. n)                                           // intern part - each of [xi,xi+1]
           let x1 := max(t.dates[i - 1],a), x2 := min(t.dates[i],b) in
             (if (x1 < x2) sum :+ (x2 - x1) * (t[x1] + t[x2] ) / 2.0),
        sum) ]

// mean : similar to integral
[mean(t:TSerie) : float
  -> if (t.count = 1) t.values[1]
     else let sum := 0.0, n := t.count, a := t.dates[1], b := t.dates[n] in
       (for i in (2 .. n)                                           // intern part - each of [xi,xi+1]
           let x1 := t.dates[i - 1], x2 := t.dates[i] in
             (sum :+ (x2 - x1) * (t.values[i - 1] + t.values[i] ) / 2.0),
        sum / (b - a)) ]

// pseudo - variance (approximation)
// E(x^2) - E(x)^2
// note the max(0.0) because rounding error may produce negative results
[variance(t:TSerie) : float
  -> if (t.count = 1) 0.0
     else let sum := 0.0, sum2 := 0.0, n := t.count, a := t.dates[1], b := t.dates[n] in
       (for i in (2 .. n)                                           // intern part - each of [xi,xi+1]
           let x1 := t.dates[i - 1], x2 := t.dates[i] in
             (sum :+ (x2 - x1) * (t.values[i - 1] + t.values[i] ) / 2.0,
              sum2 :+ (x2 - x1) * (((t.values[i - 1] + t.values[i] ) / 2.0) ^ 2.0)),
        max(0.0, (sum2 / (b - a)) - ((sum / (b - a)) ^ 2.0))) ]

// stdev is the root of the variance
[stdev(t:TSerie) : float -> sqrt(variance(t)) ]
[stdev%(t:TSerie) : float -> sqrt(variance(t)) / mean(t) ]

// special computation for an algebaric term without creating the serie
// computes variance and mean
[stdev%(at:TSTerm) : float
  -> let  lDates := pb.timeList, n := length(lDates),
          sum := 0.0, sum2 := 0.0,  a := lDates[1], b := lDates[n] in
       (for i in (2 .. n)                                           // intern part - each of [xi,xi+1]
           let x1 := lDates[i - 1], x2 := lDates[i], avx := (at[x1] + at[x2] ) / 2.0 in
             (sum :+ (x2 - x1) * avx,
              sum2 :+ (x2 - x1) * (avx ^ 2.0)),
        let varian := (sum2 / (b - a)) - ((sum / (b - a)) ^ 2.0),  // E(X^2) - E(X)^2
            mean := (sum / (b - a)) in
          (if (varian < 0.0) 0.0
           else sqrt(varian) / mean)) ]


// distance between two series using the same time scale !
// v0.4 : test two modes => distTriangle is the best (cf ROADEF presentation)
[dist(t1:TSerie,t2:TSerie) : float
  -> if (t1.count = 1) abs(t1.values[1] - t2.values[1])
     else let sum := 0.0, n := t1.count, a := t1.dates[1], b := t1.dates[n] in
       (for i in (2 .. n)                                           // intern part - each of [xi,xi+1]
           let x1 := t1.dates[i - 1], x2 := t1.dates[i],
               v1 := t1.values[i - 1], v2 := t1.values[i],
               w1 := t2.values[i - 1], w2 := t2.values[i] in
             (sum :+ (x2 - x1) * absInt(v1 - w1, v2 - w2)),
        sum / (b - a)) ]

// we want the average abs value for a linear function that goes from a to b
// the tricky part is when the sign changes
[absInt(a:float,b:float) : float
  -> if (a * b >= 0.0) (abs(a) + abs(b)) / 2                // same sign : average value
     else (sqr(a) + sqr(b)) / (2.0 * (abs(b) + abs(b))) ]


// covariance between two series
// E(XY) - E(X)E(Y)
[covariance(t1:TSerie,t2:TSerie) : float
  -> if (t1.count = 1) 0.0
     else let sum1 := 0.0, sum2 := 0.0, sump := 0.0,
              n := t1.count, a := t1.dates[1], b := t1.dates[n] in
       (for i in (2 .. n)                                           // intern part - each of [xi,xi+1]
           let x1 := t1.dates[i - 1], x2 := t1.dates[i],
               v1 := t1.values[i - 1], v2 := t1.values[i],
               w1 := t2.values[i - 1], w2 := t2.values[i] in
             (sum1 :+ (x2 - x1) * (v1 + v2) / 2.0,
              sum2 :+ (x2 - x1) * (w1 + w2) / 2.0,
              sump :+ (x2 - x1) * ((v1 + v2) * (w1 + w2) / 4.0)),
        (sump / (b - a)) - ((sum1 / (b - a)) * (sum2 / (b - a)))) ]

// correlation
[corr(t1:TSerie,t2:TSerie) : float
  ->  assert(t1.count = t2.count),
      let ss := (stdev(t1) * stdev(t2)) in
        (if (ss = 0.0) 0.0
         else covariance(t1,t2) / (stdev(t1) * stdev(t2))) ]

// linear regression
[linearRegression(t:TSerie) : list<float>
   ->  if (length(t.values) = length(pb.timeScale)) linearRegression(pb.timeScale,t.values)
       else linearRegression(list<float>{float!(x) | x in t.dates}, t.values) ]

