# Basis-CryptoEconomicAnalysis
A mathematical analysis and program to investigate the stability of the Basis stablecoin system


## License
This work is initially set up with No License. Essentially I'm delaying the choice of license until deciding on the best choice (even if this limits use and convenience for now).

## Description
This is a Haskell Stack project.

**/data** contains a single example sobol 8-dimensional quasi random sequence file, used for option pricing (the Basis Bond). In fact 500,000 points of 8, 60, 260, 800-dimensional data were prepared (using a Julia library), each split accross 5 files but this is an unwieldy amount of data. The reason the 4th file (i.e. points 300,001 to 400,000) is provided is to give a burn in. See bellow for how te sobol sequences were generated.

**/test** contains some tests using the Hspec framework (both unit and property tests).

**/app** has a simple main source presently runnning example option prices (using the 8-d data).

**/src** has the main source file. Presently only really pricing the options. (Embedding this is a system simulation is easy but is one of the many changes to be made.)


## Generation of Sobol sequences used in monte carlo option pricing

The library used was https://github.com/stevengj/Sobol.jl , and the docker image https://hub.docker.com/_/julia/ , as follows.

    docker run -it --rm -v "$PWD":/usr/myapp -w /usr/myapp julia

Doing this gave v1.0.2 version of the Sobol library for me. Then in the Julia REPL, setting dimension to 8, 60, .., as required:

    using Pkg
    Pkg.add("Sobol")

    using Sobol
    s = SobolSeq(dimension)

The function skip(s,n) to skip points didn't work for me, but it's useful to generate the full sequences to examine them anyway. Now produce the sequence: it's convenient to produce a batch (say of 100,000 points) and then write the batch to a file, then repeat for as many batches as required.

Create the data (next only 5 points here):

    thing = hcat([next!(s) for i = 1:5]...)'

Write data to a file:

    using DelimitedFiles
    writedlm("sobolDim8Batch1.txt", thing, " ")

