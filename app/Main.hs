{- **************************************************************************

Author   : Adam T Smith
Date     :
Contact  :
Version  :
Location :
License  : Proprietary (for the present)

Accompanying paper : 

Overall Description of the Program  :
This is a program to examine the robustness of the Basis stablecoin cryptocurrency protocol, as described in the whitepaper (June 2017). Execution time is the coding priority given the demands of the multiple levels of simulation described bellow. However many mathematical algorithms I have in mind that could help improve this are not implemented in this version.

Overall there are two simulations. The first prices an option given its' amount q. 

The second simulation evolves the system forward in time a potentially unlimited number of steps, using the first simulation to obtain the option amounts at each (deflation) step. This produces a single sample path of the evolution of the system.

The second simulation must be done multiple times to acquire probabilistic information, each desired set of system parameters.

(A language like C++ may be more traditional for demanding numerical processing, but Haskell offers quick development with perhaps potentially near C like efficiency. In reality though I chose Haskell because I wanted to learn it.)

************************************************************************** *-}

module Main where

import Lib

main :: IO ()
main =  do
    temp1
--    temp2




