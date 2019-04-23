{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields -O2 #-}

module Lib
  ( module System.Random
  , module Lib
  )
where

import Data.List (sortBy)
import Data.List.Split (chunksOf)
import System.IO
import System.Random
import System.Random.Mersenne.Pure64

--import Control.Monad.State
--import Control.Monad.Identity


-- ------------ CONFIGURATION ------------------------------------------------

-- Basis specification (drift is put in in simulation part)
data BS = BS {
     -- Basis protocol specification
     expiryT :: Double  -- expiry time (fixed)
   , numdT   :: Int     -- number of expiries (varies)
   , deltaT  :: Double  -- time between inf/deflations (= expiryT / numdT)
   , path    :: String  -- path to d dim Sobol numbers file for numT=d
     -- market parameters
   , vol     :: Double  -- basis price vol (variable)
   , rate    :: Double  -- risk free rate (always set to 0)
   , disc    :: Double  -- one period DF (derived = exp rate * deltaT =1 )
   } deriving Show

-- low vol:
bs_n8_v3 = BS {
     expiryT = 5.0, numdT   = 8, deltaT  = 0.625
   , path    = "data/sobol_d8_100,000_4.txt"

   , vol     = 0.03, rate    = 0.0, disc    = 1.0
   }

bs_n60_v3  = bs_n8_v3 { numdT  = 60,  deltaT = 5.0 / fromIntegral 60 
                      , path   = "data/sobol_d60_100,000_4.txt" }

bs_n260_v3 = bs_n8_v3 { numdT  = 260, deltaT = 5.0 / fromIntegral 260 
                      , path   = "data/sobol_d260_100,000_4.txt" }

bs_n800_v3 = bs_n8_v3 { numdT  = 800, deltaT = 5.0 / fromIntegral 800 
                      , path   = "data/sobol_d800_100,000_4.txt" }

-- medium vol
bs_n8_v10   = bs_n8_v3 { numdT = 8  , deltaT = 5.0 / fromIntegral 8  , vol = 0.1 }

bs_n60_v10  = bs_n8_v3 { numdT = 60 , deltaT = 5.0 / fromIntegral 60 , vol = 0.1 
                       , path   = "data/sobol_d60_100,000_4.txt" }

bs_n260_v10 = bs_n8_v3 { numdT = 260, deltaT = 5.0 / fromIntegral 260, vol = 0.1 
                      , path   = "data/sobol_d260_100,000_4.txt" }

bs_n800_v10 = bs_n8_v3 { numdT = 800, deltaT = 5.0 / fromIntegral 800, vol = 0.1 
                      , path   = "data/sobol_d800_100,000_4.txt" }

-- high vol
bs_n8_v30   = bs_n8_v3 { numdT = 8  , deltaT = 5.0 / fromIntegral 8  , vol = 0.3 }

bs_n60_v30  = bs_n8_v3 { numdT = 60 , deltaT = 5.0 / fromIntegral 60 , vol = 0.3 
                      , path   = "data/sobol_d60_100,000_4.txt" }

bs_n260_v30 = bs_n8_v3 { numdT = 260, deltaT = 5.0 / fromIntegral 260, vol = 0.3
                      , path   = "data/sobol_d260_100,000_4.txt" }

bs_n800_v30 = bs_n8_v3 { numdT = 800, deltaT = 5.0 / fromIntegral 800, vol = 0.3
                      , path   = "data/sobol_d800_100,000_4.txt" }

-- program configuration (hard coded constants, ugly but they're constant)
nSamps    = 40000 :: Int  -- number of MC samples for each option pricing
bTol      = 10.0^^(-14)   -- fitting function b parameter tolerance
oMQFactor = 0.05          -- places medium initial option amt (multiple of n)
oHQFactor = 10.0^20       -- places high initial option amt (multiple of n)
qLowTol   = 0.999         -- 1=strictly impossibly high target, or a bit lower
oSlopeTol = 0.005         -- minimum slope of option price vs amount
oPrTol    = 0.01          -- relative error tolerance of option price

-- ----- Types

-- Option pricing state, used to calculate option prices.
-- (A DIFFERENCE LIST for qs COULD BE BEST BECAUSE OF THE ++)
data Ostate = Ostate {
     n   :: Double    -- number of basis coins in existence
   , qs  :: [Double]  -- unexpired bonds amounts from oldest to newest
                      -- reduces in length with payoffs and expiry
                      -- no new bond at each step, last is the option
   , ix  :: Int       -- counter: head expires when it reaches zero
                      -- (allows removing leading zeros of qs for efficiency)
   , pay :: Double    -- contribution to option price
   } deriving Show

-- Basis system state, used in main simulation.
data Bstate = Bstate {
     nB    :: Double    -- number of basis coins
   , qsB   :: [Double]  -- unexpired bond amounts from oldest to newest
                        -- always numdT long
                        -- at each step head expires and new last one added
   } deriving Show

-- Option amount, price pairs.
data Point = Point { 
     x :: Double 
   , y :: Double
   } deriving Show


-- ------------ GENERATE RANDOM BASIS PRICES ---------------------------------

-- Inverse cumulative normal function.
-- Taken from http://home.online.no/~pjacklam/notes/invnorm/
-- Accurate to about 1e-9. Good for Sobol numbers
iNorm :: (Ord a, Floating a) => a -> a
iNorm p =
    let a1 = -3.969683028665376e+01 ; a2 =  2.209460984245205e+02
        a3 = -2.759285104469687e+02 ; a4 =  1.383577518672690e+02
        a5 = -3.066479806614716e+01 ; a6 =  2.506628277459239e+00

        b1 = -5.447609879822406e+01 ; b2 =  1.615858368580409e+02
        b3 = -1.556989798598866e+02 ; b4 =  6.680131188771972e+01
        b5 = -1.328068155288572e+01

        c1 = -7.784894002430293e-03 ; c2 = -3.223964580411365e-01
        c3 = -2.400758277161838e+00 ; c4 = -2.549732539343734e+00
        c5 =  4.374664141464968e+00 ; c6 =  2.938163982698783e+00

        d1 =  7.784695709041462e-03 ; d2 =  3.224671290700398e-01
        d3 =  2.445134137142996e+00 ; d4 =  3.754408661907416e+00

        pLow = 0.02425; nan = 0/0

    in  if p < 0 then
            nan
        else if p == 0 then
            -1/0
        else if p < pLow then
            let q = sqrt(-2*log(p))
            in  (((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6) /
                 ((((d1*q+d2)*q+d3)*q+d4)*q+1)
        else if p < 1 - pLow then
            let q = p - 0.5
                r = q*q
            in  (((((a1*r+a2)*r+a3)*r+a4)*r+a5)*r+a6)*q /
                (((((b1*r+b2)*r+b3)*r+b4)*r+b5)*r+1)
        else if p <= 1 then
            - iNorm (1 - p)
        else
            nan

-- Single period Basis Price, risk neutral (uses rate, not drift), use Sobol
rnBP :: BS      -- configuration
     -> Double  -- uniform [0,1] rv (from a Sobol sequence)
     -> Double  -- basis price
rnBP bs uv = exp $ ((rate bs) - (vol bs)^2/2)*(deltaT bs) + sn*(vol bs)*sqrt (deltaT bs)
    where sn = iNorm uv

-- Stream of Basis prices, risk neutral
rnBPs :: BS        -- configuration
      -> [Double]  -- uniform [0,1] rv (from a Sobol sequence)
      -> [Double]  -- basis price
rnBPs bs []       = []
rnBPs bs (uv:[])  = ( exp $ ((rate bs) - (vol bs)^2/2)*(deltaT bs) 
                      + (iNorm uv)*(vol bs)*sqrt (deltaT bs) ) : []
rnBPs bs (uv:uvs) = ( exp $ ((rate bs) - (vol bs)^2/2)*(deltaT bs) 
                      + (iNorm uv)*(vol bs)*sqrt (deltaT bs) ) : rnBPs bs uvs

-- Box-Muller: 2 standard nv's from 2 uv's [0,1], use Mersenne Twister
boxMul :: Double -> Double -> (Double, Double)
boxMul un1 un2 = ( r*sin (2*pi*un2), r*cos (2*pi*un2) )
  where r = sqrt $ (-2)*log un1  -- note log is base e (as it should be!)

-- Stream of basis prices
-- NOTE: THESE NEED FIXING UP WITH DRIFT APPLIED
oBPs :: BS        -- configuration
     -> [Double]  -- uniform rvs, infinite length or sufficient
     -> [Double]  -- basis prices
oBPs bs (u1:u2:us) = (rnBP bs sn1):(rnBP bs sn2):(oBPs bs us)
  where (sn1,sn2) = boxMul u1 u2


-- Change in amount of basis, inflate (if price>1) or deflate (if price<1).
dnFromP :: Double  -- basis price
        -> Double  -- current amount
        -> Double  -- amount to in/deflate
dnFromP p n = (p-1) * n


-- ------------ MONTE CARLO PRICING OF THE OPTION ----------------------------

-- One step deflation.
oneDef :: Double  -- price <=1
       -> Ostate  -- initial Ostate
       -> Ostate  -- new Ostate
oneDef p (Ostate n (q:qs)   0  _) = Ostate (n + dnFromP p n) qs 1  0
oneDef p (Ostate n qs       ix _) = Ostate (n + dnFromP p n) qs ix 0

-- One step inflation helper, paying down bonds and the option.
oneInfH :: ( [Double], Double, Int, Double ) -- bonds, inf dn>0, ix, pay
        -> ( [Double], Int, Double )         -- bonds, ix, pay
oneInfH ((q1:q2:qs), dn, 0 , _  ) = if q1 > dn
                                    then (q2:qs, 1, 0)
                                    else oneInfH (q2:qs, dn-q1, 1   , 0)
oneInfH ((q1:q2:qs), dn, ix, _  ) = if q1 > dn
                                    then ((q1-dn):q2:qs, ix, 0)
                                    else oneInfH (q2:qs, dn-q1, ix+1, 0)
oneInfH ((q:[])    , dn, 0 , pay) = ([], 1, pay + min dn q)
oneInfH ((q:[])    , dn, ix, pay) = if q > dn
                                    then ((q-dn):[], ix  , pay+dn)
                                    else ([]       , ix+1, pay+q )

-- One step inflation.
oneInf :: Double  -- price >1
       -> Ostate  -- initial Ostate
       -> Ostate  -- new Ostate
oneInf p (Ostate n qs ix _) = (Ostate (dn + n) newqs newix newpay)
  where
    dn = dnFromP p n
    (newqs, newix, newpay) = oneInfH (qs, dn, ix, 0)

-- One sample Option price, using multistep inflation and deflation.
oSample :: BS        -- Configuration
        -> [Double]  -- list of basis prices, (numT-1) long
        -> Ostate    -- initial Ostate
        -> Double    -- sample price
oSample _  _   (Ostate _ [] _ cont) = cont  -- no more option
oSample _  []  (Ostate _ _  _ cont) = cont  -- no more timesteps
oSample bs (p:ps) oSt = let newOSt  = if p <= 1 
                                   then oneDef p oSt 
                                   else oneInf p oSt
                            newpay  = pay newOSt
                            nextix  = (ix  newOSt) - 1
                            nextOSt = newOSt {ix = nextix, pay = 0}
                        in  (disc bs) * ( newpay + oSample bs ps nextOSt )

-- Monte Carlo price for a given amount of the option. (Total, not per unit.)
oPrice :: BS        -- Configuration
       -> Ostate    -- initial Ostate
       -> [Double]  -- list of basis prices
       -> Double    -- price
oPrice bs oSt ps = let prices   = chunksOf (numdT bs) ps
                       sampList = take nSamps $ map (\x -> oSample bs x oSt) prices
                   in  (sum sampList ) / fromIntegral nSamps


-- ------------ SOLVING FOR THE OPTION AMOUNT FOR A GIVEN DEFLATION AMOUNT ---

-- ----- Helper functions.

-- Replace last element of a (bond) list = amount of latest option.
-- Partial function (fails when qs empty)
replaceQ :: [a] -> a -> [a]
replaceQ qs qnew = (init qs) ++ [qnew]

-- Sort Points in ascending order of y.
sortPts :: [Point] -> [Point]
sortPts ~pts = sortBy func pts
  where
    func (Point x1 y1) (Point x2 y2) =
        let cmp = compare y1 y2
        in  if cmp == EQ then compare x1 x2 else cmp

-- 2 points closest to a target y (either side if possible).
-- Requires at least 2 ordered different points.
-- (Underused by feeding it short lists.)
twoBestPts :: Double -> [Point] -> [Point]
twoBestPts _  (pt1:pt2:[]) = [pt1,pt2]
twoBestPts yT (pt1:pt2:pts)
  | yT < (y pt2)           = [pt1,pt2]
  | otherwise              = twoBestPts yT (pt2:pts)
twoBestPts _  _            = error "error twoBestPoints: bad input. "

-- ----- 2 parameter fitting: y = a * tanh (b*x) .

-- Approximating function.
tFunc :: Double -> (Double, Double) -> Double
tFunc x (a, b) = a * tanh (b*x)

-- Inverse approximating function.
itFunc :: Double -> (Double, Double) -> Double
itFunc y (a, b) = atanh (y / a) / b

-- param b from 2 Points and a starting guess
findb :: (Double, Double) -> (Double, Double) -> Double -> Double
findb (x1, y1) (x2, y2) bGuess =  
    let 
      newbGuess = ( atanh( (y1/y2)*tanh(x2*bGuess) ) )/ x1
      err = abs (newbGuess - bGuess)
    in
      if err < bTol
        then newbGuess
        else findb (x1, y1) (x2, y2) newbGuess

-- The 2 parameters (a,b) when fitting to 2 points.
tParams :: Point -> Point -> (Double, Double)
tParams (Point x1 y1) (Point x2 y2)
  | y2/x2 > y1/x1 = error "error tParams bad input: y2/x2 > y1/x1.   "
  | x1 == x2      = error "error tParams bad input: same x's.   "
  | y1 == y2      = error "error tParams bad input: same y's.   "
  | otherwise     = (y1 / tanh (b*x1),  b)
  where
    b = findb (x1, y1) (x2, y2) 1.0  -- HARD CODED 1.0 is initial guess

-- Use fitting curve (from 2 points) to find a new q.
-- (Origin (0,0) is fitted by construction so should not be input.)
tFuncQ :: Double -> (Point, Point) -> Double
tFuncQ yT (pt1, pt2) = itFunc yT $ tParams pt1 pt2

-- ----- Solving (target price) = oPrice(q) for amount of option q.

-- 2 intial points using a simple method.
first2Pts :: BS        -- config
          -> Ostate    -- initial Ostate
          -> [Double]  -- list of basis prices (numdT*nSamps) long
          -> (Point, Point)
first2Pts bs oSt ps = let qm      = oMQFactor * (n oSt)
                          qh      = oHQFactor * (n oSt)
                          newqsm  = replaceQ (qs oSt) qm
                          newqsh  = replaceQ (qs oSt) qh
                          omPrice = oPrice bs oSt {qs = newqsm} ps
                          ohPrice = oPrice bs oSt {qs = newqsh} ps
                      in  ((Point qm omPrice), (Point qh ohPrice))

-- Finding a new point using the tanh fitting
findQ :: BS -> Ostate -> Double -> [Double] -> (Point, Point)-> Point
findQ bs oSt yT ps pts@( pt1, pt2@(Point x2 y2) ) =
    let -- calculate next point
        newq     = tFuncQ yT pts
        newqs    = replaceQ (qs oSt) newq
        newoSt   = oSt {qs = newqs}
        newPrice = oPrice bs newoSt ps
        newPt    = Point newq newPrice
        -- terminate if slope too small or within tolerance
        slope    = (newPrice - y2) / (newq - x2)
        relErr   = abs (newPrice - yT) / yT
-- warning if slope small???
    in  if relErr < oPrTol || slope < oSlopeTol
          then newPt
          else findQ bs newoSt yT ps $ toTuple $ twoBestPts yT $ sortPts [pt1, newPt, pt2]
              where toTuple [pt1, pt2] = (pt1, pt2)

-- Root finding yT = OptionPrice(q) .
tBestQ :: BS        -- config
       -> Ostate    -- initial Ostate
       -> Double    -- target price
       -> [Double]  -- list of basis prices
       -> Point     -- q solution
tBestQ bs oSt yT ps = let (ptm, pth) = first2Pts bs oSt ps
                          yh = y pth
                          isTooHigh = yT * qLowTol < yh
                      in  if isTooHigh 
                          then error "error tBestQ: impossibly high option amount. "
                          else findQ bs oSt yT ps (ptm, pth)


-- ----- Process Sobol files

-- Split Sobol file into list of list of Doubles (not string)
proc :: String -> [[Double]]
proc str =  (fmap . fmap) read $ fmap words $ lines str

-- Process Sobol data to Basis prices
toPricesS :: String -> BS -> [Double]
--toPricesS str bs = fmap (rnBP bs) $ concat $ proc str
toPricesS str bs = rnBPs bs $ concat $ proc str

-----------------

-- function that uses all elements of input list
funnyAv :: [Double] -> Double
funnyAv = foldr fun 0.0
    where fun l = (if l> 0.25 && l <=0.75 then (\x->x+l) else (\x->x-l))



-- test timings of file
tempy = do
    let bs = bs_n8_v30
    print bs
--    content <- readFile (path bs) -- "data/sobol_d8_100,000_4.txt"
    content <- readFile "data/sob_d260_4k.txt"
    let sps = toPricesS content bs
    print "here1"
    print $ last content
    print $ last sps
    let fAv = funnyAv sps
    print fAv
    let nAv = av sps (0,0,0)
    print nAv
--    print $ take 100 content
--    print $ take 10 sps


-- check sobols are uniform[0,1]
av (l:[]) (s,n,_) = (s+l,n+1, (s+l)/(n+1)) 
av (l:ls) (s,n,_) = av ls (s+l,n+1,0)


-- 8d option evaluation with Sobol
temp1 = do 
    let bs = bs_n8_v3
    print bs
    content <- readFile "data/sobol_d8_100,000_4.txt"
    let sps = toPricesS content bs
    putStrLn "  8-d option with Sobol, vol = 0.03"
    let prc1 = oPrice bs (Ostate 1000 [0,0,0,0,0.5,0.7,0,1] 0 0) sps
    putStr "1 "
    putStrLn $ show prc1
    let prc1 = oPrice bs (Ostate 1000 [0,0,0,0,0.5,0.7,0,10] 0 0) sps
    putStr "10 "
    putStrLn $ show prc1
    let prc1 = oPrice bs (Ostate 1000 [0,0,0,0,0.5,0.7,0,100] 0 0) sps
    putStr "100 "
    putStrLn $ show prc1
    let prc1 = oPrice bs (Ostate 1000 [0,0,0,0,0.5,0.7,0,1000] 0 0) sps
    putStr "1000 "
    putStrLn $ show prc1
    -- change configuration
    print " "
    let bs = bs_n8_v10
    print bs
    let sps = toPricesS content bs
    putStrLn "  8-d option with Sobol, vol= 0.10"
    let prc1 = oPrice bs (Ostate 1000 [0,0,0,0,0.5,0.7,0,1] 0 0) sps
    putStr "1 "
    putStrLn $ show prc1
    let prc1 = oPrice bs (Ostate 1000 [0,0,0,0,0.5,0.7,0,10] 0 0) sps
    putStr "10 "
    putStrLn $ show prc1
    let prc1 = oPrice bs (Ostate 1000 [0,0,0,0,0.5,0.7,0,100] 0 0) sps
    putStr "100 "
    putStrLn $ show prc1
    let prc1 = oPrice bs (Ostate 1000 [0,0,0,0,0.5,0.7,0,1000] 0 0) sps
    putStr "1000 "
    putStrLn $ show prc1

temp11 = do 
    let bs = bs_n8_v3
    print $ bs
--    let bs = bs_n60_v3
--    print $ bs

-- 800d option with sobol
temp2 = do
    let bs = bs_n800_v30
    content <- readFile "data/sobol_d800_100,000_2.txt"
    let sps = toPricesS content bs
    --
    let qs1    = (take 799 $ repeat 0.01) ++ [1]
    let qs10   = (take 799 $ repeat 0.01) ++ [10]
    let qs100  = (take 799 $ repeat 0.01) ++ [100]
    let qs1000 = (take 799 $ repeat 0.01) ++ [1000]
    let qs1000 = (take 799 $ repeat 0.01) ++ [10000000]
    --
    putStrLn "  800-d option with Sobol"
    let prc1 = oPrice bs (Ostate 1000 qs1 0 0) sps
    putStrLn "1 "
    putStrLn $ show prc1
    let prc1 = oPrice bs (Ostate 1000 qs10 0 0) sps
    putStrLn "10 "
    putStrLn $ show prc1
    let prc1 = oPrice bs (Ostate 1000 qs100 0 0) sps
    putStrLn "100 "
    putStrLn $ show prc1
    let prc1 = oPrice bs (Ostate 1000 qs1000 0 0) sps
    putStrLn "1000 "
    putStrLn $ show prc1
    let prc1 = oPrice bs (Ostate 1000 qs1000 0 0) sps
    putStrLn "10000000 "
    putStrLn $ show prc1









-- Mersenne Twister RNG
tempMT = do
    gen <- newPureMT
    putStrLn $ show $ take 10 (randoms gen :: [Double])
    putStrLn $ show $ randomDouble gen


-- ------------ MAIN SIMULATION, AND GATHERING STATISTICS --------------------

-- simulate new prices

-- evolve each step with inflation or deflation

-- do many times for different scenarios
-- will need time dependent drift

-- output proportion paths leading to DS











-- ---------------------------------------------------------------------------
-- UNUSED CODE - MAY BE USED IN FUTURE ******

-- ------------ BLACK SCHOLES FORMULAS ---------------------------------------
-- Unused in program, but useful for experiments.

-- ERF function, Abramowitz and Stegun formula 7.1.26
-- (From www.johndcook.com/blog/haskell-erf/)
erf :: Double -> Double
erf x = sign*y 
  where a1 =  0.254829592; a2 = -0.284496736; a3 =  1.421413741
        a4 = -1.453152027; a5 =  1.061405429; g  =  0.3275911
        sign = if x > 0 then 1 else -1
        t  =  1.0/(1.0 + g * abs x)
        y  =  1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x)

-- Black Scholes call price formula.
bsCall :: Double -> Double -> Double -> Double -> Double -> Double
bsCall s k t v r = s*cnorm d1 - k*exp(-r*t)*cnorm d2
  where d1 = ( log (s/k) + (r + 0.5*v*v)*t ) / (v * sqrt t)
        d2 = d1 - v * sqrt t
        cnorm x = 0.5*( 1 + erf (x/sqrt 2) )

-- Price of the call spread, any start price.
bsSpread :: Double -> Double -> Double -> Double -> Double -> Double -> Double
bsSpread n s q t v r 
    = n*( bsCall s 1 t v r  -  bsCall s (1+q/n) t v r )

-- Price of call spread with price = 1, i.e. at-the-money with lower strike.
atmBsSpread :: Double -> Double -> Double -> Double -> Double -> Double
atmBsSpread n q t v r = bsSpread n 1 q t v r 

-- Price per unit of the lower at-the-money call spread.
unitAtmBsSpread :: Double -> Double -> Double -> Double -> Double -> Double
unitAtmBsSpread n q t v r = atmBsSpread n q t v r  / q

-- --------------------------------
-- May be used if use quadratic fitting

-- 3 points closest to a target y (both sides if possible).
-- Requires at least 3 ordered different points.
-- (Possibly underused by feeding it short lists.)
threeBestPts :: Double -> [Point] -> [Point]
threeBestPts _  (pt1:pt2:pt3:[])        = [pt1,pt2,pt3]
threeBestPts yT (pt1:pt2:pt3:pt4:[])
  | yT      < (y pt2)                   = [pt1,pt2,pt3]
  | (y pt3) < yT                        = [pt2,pt3,pt4]
  | abs ((y pt1)-yT) < abs ((y pt4)-yT) = [pt1,pt2,pt3]
  | otherwise                           = [pt2,pt3,pt4]
threeBestPts yT (pt1:pt2:pt3:pt4:pts)
  | yT < (y pt2)                        = [pt1,pt2,pt3]
  | yT < (y pt3)                        = threeBestPts yT [pt1,pt2,pt3,pt4]
  | otherwise                           = threeBestPts yT (pt2:pt3:pt4:pts)
threeBestPts _  _                       = error "error threeBestPoints: bad input. "


-- ----- Quadratic 3 parameter fitting function: x = a*y^2 + b*x +c .

-- Approximating function. (Fails for x<-b/2a.)
-- Gives the higher solution of x in order to have cave convexity.
qFunc :: Double -> (Double, Double, Double) -> Double
qFunc x (a, b, c) = if x < (-b)/(2*a) 
                    then error "error qFunc: bad input x<-b/2a. "
                    else ( sqrt(b*b - 4*a*(c-x)) - b ) / (2*a)

-- Inverse approximating function.
iqFunc :: Double -> (Double, Double, Double) -> Double
iqFunc y (a, b, c) = a*y*y + b*y +c

-- The 3 paramaters (a,b,c) when fitting to 3 Points.
-- (Formulas found using Sagemath.)
qParams :: Point -> Point -> Point -> (Double, Double, Double)
qParams (Point x1 y1) (Point x2 y2) (Point x3 y3) = 
    let denom = (y1^two*y2 - y1*y2^two + (y1 - y2)*y3^two 
                                       - (y1^two - y2^two)*y3)
        a = (-((x2 - x3)*y1 - (x1 - x3)*y2 + (x1 - x2)*y3))          / denom
        b = ((x2 - x3)*y1^two - (x1 - x3)*y2^two + (x1 - x2)*y3^two) / denom
        c = (x3*y1^two*y2 - x3*y1*y2^two + (x2*y1 - x1*y2)*y3^two 
                                   - (x2*y1^two - x1*y2^two)*y3) / denom
        two = 2 :: Integer
    in  (a, b, c)

-- New q from 3 points and target y.
-- (The origin is a valid point and must be used when you only have 2 points.)
qFuncQ :: (Point, Point, Point) -> Double -> Double
qFuncQ (pt1, pt2, pt3) yT = iqFunc yT $ qParams pt1 pt2 pt3


-- computations random nums and op prices: --------------------------

-- 4d options with system RNG
temp_4d_SysRNG = do
    gen <- newPureMT  -- MersenneT, could use standard: getStdGen
    let bs = bs_n8_v3 -- wrong dimension for 4d, but just to work
    -- uses risk netral prices
    putStrLn $ show $  oPrice bs (Ostate 100 [0,25,0,150] 1 0) $ rnBPs bs $ (randoms gen :: [Double])
    putStrLn $ show $  oPrice bs (Ostate 100 [0,25,0,100] 1 0) $ rnBPs bs $ (randoms gen :: [Double])
    putStrLn $ show $  oPrice bs (Ostate 100 [0,25,0,50] 1 0) $ rnBPs bs $ (randoms gen :: [Double])
    putStrLn $ show $  oPrice bs (Ostate 100 [0,25,0,15] 1 0) $ rnBPs bs $ (randoms gen :: [Double])
    putStrLn $ show $  oPrice bs (Ostate 100 [0,25,0,10] 1 0) $ rnBPs bs $ (randoms gen :: [Double])
    putStrLn $ show $  oPrice bs (Ostate 100 [0,25,0,7] 1 0) $ rnBPs bs $ (randoms gen :: [Double])
    putStrLn $ show $  oPrice bs (Ostate 100 [0,25,0,4] 1 0) $ rnBPs bs $ (randoms gen :: [Double])
    putStrLn $ show $  oPrice bs (Ostate 100 [0,25,0,3] 1 0) $ rnBPs bs $ (randoms gen :: [Double])
    putStrLn $ show $  oPrice bs (Ostate 100 [0,25,0,2] 1 0) $ rnBPs bs $ (randoms gen :: [Double])


-- getting random number to work
oSt1 = Ostate 1000 [1,2,0,0,4,2,3,1.777] 8 0

test = do
    gen <- newPureMT  -- MersenneT, could use standard: getStdGen
    let bs = bs_n8_v3 -- wrong dimension for 4d, but just to work
    -- uses risk netral prices
    putStrLn $ show $ tBestQ bs oSt1 1.1 (rnBPs bs $ randoms gen :: [Double])






