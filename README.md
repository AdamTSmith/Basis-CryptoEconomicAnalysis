# Basis-CryptoEconomicAnalysis
A mathematical analysis and program to investigate the stability of the Basis stablecoin system


## License
This work is initially set up with No License. Essentially I'm delaying the choice of license until deciding on the best choice (even if this limits use and convenience for now).

## Description
This is a Haskell Stack project.

**/data** contains a single example sobol 8-dimensional quasi random sequence file, used for option pricing (the Basis Bond). In fact 500,000 points of 8, 60, 260, 800-dimensional data were prepared (using a Julia library), each split accross 5 files but this is an unwieldy amount of data. The reason the 4th file (i.e. points 300,001 to 400,000) is provided is to give a burn in.

**/test** contains some tests using the Hspec framework (both unit and property tests).

**/app** has a simple main source presently runnning example option prices (using the 8-d data).

**/src** has the main source file. Presently only really pricing the options. (Embedding this is a system simulation is easy but is one of the many changes to be made.)



