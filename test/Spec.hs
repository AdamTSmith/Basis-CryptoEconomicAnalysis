import Test.Hspec
import Test.QuickCheck
import Lib              -- main program module


-- Wrapping function for tParams and tFunc .
test_tParams :: Point -> Point  -> [Double]
test_tParams pt1 pt2  = [x1,y1,x2,y2]
  where
    (a,b) = tParams pt1 pt2
    x1 = x pt1          ; x2 = x pt2
    y1 = tFunc x1 (a,b) ; y2 = tFunc x2 (a,b)

-- tParams and tFunc recover the original point.
-- (Strict limits apply to the inputs.)
prop_qPFRecover :: (Positive Double) -> (Positive Double) -> (Positive Double) ->Property
prop_qPFRecover = 
    (\(Positive x1) (Positive y1) (Positive dx) ->
       forAll (choose(0.00001, dx*y1/x1 - 0.00001)) $
           (\y2 -> (maximum $ zipWith (\u v -> abs(u-v)) 
                              (test_tParams (Point x1 y1) (Point (x1+dx) y2) )
                              [x1,y1,x1+dx,y2] )
            < 0.000001
           )
    )

-- Test qParams. quadratic not in use yet.
test_qParams ::
    Point -> Point -> Point -> 
    ((Double,Double), (Double,Double), (Double,Double), (Double,Double,Double))
test_qParams pt1 pt2 pt3 = ((x1,y1),(x2,y2),(x3,y3),(a,b,c))
  where
    (a,b,c) = qParams pt1 pt2 pt3
    x1 = x pt1              ; x2 = x pt2            ; x3 = x pt3
    y1 = qFunc x1 (a,b,c)   ; y2 = qFunc x2 (a,b,c) ; y3 = qFunc x3 (a,b,c)



main :: IO ()
main = hspec $ do
  describe "Discrete tests" $ do

    it "test dnFromP" $ do
      dnFromP 2 1 `shouldBe` 1.0

    it "bsSpread should give q for high price (above 1+q/n)" $ do
      (bsSpread 10.0 100 5 10 0.2 0) - 5.0 < 10^^(-10) `shouldBe` True


  describe "Quickcheck tests" $ do
      it "tParams/tFunc recover the original 2 points" $ property $
         prop_qPFRecover













