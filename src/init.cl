(printf("-------------- init.cl file for EMLA on mac ------------------\n"))

*src* :: "/Users/ycaseau/Dropbox/src"

// usual stub to compile m1
(verbose() := 1,
 compiler.safety := 1)

// our emla V0.1 module
m1 :: module(source = *src* / "emlav0.1",
             uses = list(Core,Reader),
             made_of = list("model","tseries","algebra","evol"))

// 2018 version with Lamarck score-guided generation + extended algebra 
m3 :: module(source = *src* / "emlav0.3",
             uses = list(Core,Reader),
             made_of = list("model","tseries","algebra","gauge","evol"))

// 2019 version for ROADEF presentation
m40 :: module(source = *src* / "emlav0.4",
             uses = list(Core,Reader),
             made_of = list("model","tseries","algebra","gauge","evol"))


// m41 is just the same for playing with options
m41 :: module(source = *src* / "emlav0.41",
             uses = list(Core,Reader),
             made_of = list("model","tseries","algebra","gauge","evol"))

// comparison: emla 0.2 with full data
m42 :: module(source = *src* / "emlav0.42",
             uses = list(Core,Reader),
             made_of = list("model","tseries","algebra","evol"))


// m43 is m41 trying to become m40
m43 :: module(source = *src* / "emlav0.43",
             uses = list(Core,Reader),
             made_of = list("model","tseries","algebra","gauge","evol"))


// m5 is the m40 version ported to CLAIRE4  (version for ROADEF presentation)
m5 :: module(source = *src* / "emlav0.5",
             uses = list(Core,Reader),
             made_of = list("model","tseries","algebra","gauge","evol"))

// local environment variable : where to find the data files
*where* :: "/Users/ycaseau/proj/emla"




