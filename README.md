# Basis-CryptoEconomicAnalysis
A mathematical analysis and program to investigate the stability of the Basis stablecoin system


## License
Initially No License, until deciding on the best choice (even if this limits use and convenience for now).

## Description
This is a Haskell Stack project.

**/data** Contains a single example Sobol 8-dimensional quasi random sequence file, used for option pricing (the Basis Bond). In reality 500,000 points of 8, 60, 260, 800-dimensional data were prepared (using a Julia library), each split across 5 files but this is an unwieldy amount of data. The reason the 4th file (i.e. points 300,001 to 400,000) is provided is to give a burn in. See bellow for how Sobol sequences were generated.

**/test** Contains some tests using the Hspec framework, both unit and property based tests.

**/app** Has a simple main source presently running example option prices (using the 8-d Sobol data).

**/src** Has the main source file.


## Generation of Sobol sequences used in Monte Carlo option pricing

The library used was https://github.com/stevengj/Sobol.jl , and the docker image https://hub.docker.com/_/julia/ , as follows.

    docker run -it --rm -v "$PWD":/usr/myapp -w /usr/myapp julia

In the Julia REPL, set dimension to 8, 60, .., as required:

    using Pkg
    Pkg.add("Sobol")

    using Sobol
    s = SobolSeq(dimension)

Produce the sequence: it's convenient to produce a batch (say of 100,000 points) and then write the batch to a file, then repeat for as many batches as required.

Create the data:

    thing = hcat([next!(s) for i = 1:100000]...)'

Write the sequence to a file:

    using DelimitedFiles
    writedlm("sobolDim8Batch1.txt", thing, " ")


## Mathematical Model

A complete explanation of the model is given in my paper, but here's a summary. Basis consists of three linked crypto coins: Basis, the coin that's supposed to be stable and worth 1 USD (US Dollar); Bond coins that are auctioned on chain for Basis in order to remove Basis from circulation (coin deflation); and Share coins. 

When new Basis coins are issued they are paid first to the oldest Bond until it's fully paid off, then to the the next oldest Bond coin and so on. When all Bonds are fully paid then the Share coins receive equal shares any newly issued Basis coins.

These inflation and deflation events occur at regularly intervals and are designed to maintain the peg price of 1 USD per Basis coin, according to whether Basis is overpriced (> 1 USD) or underpriced (< 1 USD) respectively. An Oracle is used to obtain the trading price.

My main assumptions are:

A1. The Basis price is inversely proportional to the number in circulation. (The same assumption is made in Robert Sams original paper in the field.) This fixes the amount of Basis to add or remove from circulation given the Basis price. The Basis price therefore makes a discontinuous jump back to 1 when the system is performing properly, when the number of coins changes.
A2. Bonds can always be sold for Basis, the market is liquid and never dries up, and the only question is the auction price.
A3. Aside from the control jumps the Basis price is continuous and a lognormal (geometric Brownian motion) stochastic process (continuous random process). Equivalently demand for Basis at 1 USD per coin is a lognormal process. A corollary is that risk neutral pricing (a standard financial derivatives method) can be used to price the Bonds in an auction.
A4. The interest rate is zero. This helps keep the Bond price up and is therefore good for the health of the system. We now have one less parameter to worry about and rates are very low these days anyway.

I argue that these are generous assumptions, in the sense that they decrease the chances of the system becoming unhealthy. We need to be veer on the generous side in order to be sure that problems seen in the model can be expected in reality, even though this will underestimate the probability.

Assumption A3 is actually rather profound since it cuts important feedback effects out of the model. These would be very strong just when the system is under stress, such as in a death spiral: in reality a low Bond price would further depress the Basis price, leading to more Bonds needing to be issued and an even lower Bond price. A vicious circle can be triggered.

The model still contains the far milder feedback loop of a large amount of Bonds outstanding depressing the price of new Bonds, due solely to the new Bond standing lower in the payout hierarchy. A cascade of Bond issuance that can't be maintained may therefore still occur without the impetus of direct feedback. Removing direct feedback is a very generous assumption, in fact likely to be greatly overgenerous, but makes leads to a much more tractable model.


Computation is demanding for the following reasons:

1. The dimension of the Bond pricing problem can be very high, essentially the number of coin control events in 5 years. Our 800 dimension corresponds to a rebalance every 2.2 days, though we may be able to extrapolate from lower dimensions. Quasi Monte-Carlo is used to speed this up. This part may well need to be computed on a GPU, and as a repeated evaluation of a fairly simple kernel function for different vector inputs is nicely parallelizable.
1. The price of the Bond depends on the quantity issued. This means that a root finding procedure is needed to pin down the the amount of Bond required to remove a given amount of Basis from circulation. (My tangent fitting method speeds this up, but quadratic fitting code used in an earlier version is left in there in case of use.)
1. There is a further overall simulation that needs to be repeated many times for scenario in order to obtain statistics about death spirals and failures.
