# covidm
Dynamic model of SARS-nCoV-2 transmission.

This model is under active development, and at the moment the documentation is sparse. There are some examples to get you started, see below.

Note that there are two versions of `covidm` here. Version 1 is the relatively more "stable" version, and it is the version that all the examples currently use. Version 2 is under more active development, and can run quite a bit faster because it shifts more of the computation into C++ rather than keeping it in R, especially in the context of model fitting. However, we are still in the process of making this user-friendly.

This repository is for development of the model itself. Several projects which use `covidm` come bundled with their own version of it, and consulting the code for these projects may help illuminate how the model can be used. These projects include:
* [COVID-19 scenario projections for the UK](https://github.com/cmmid/covid-uk) (dating from late February to early March 2020)

## Installation for Windows 10

Before using covidm, you will need to have `Rcpp` set up and install the `GNU Scientific Library`. Instructions for how to do so follow.

1. Install R packages Rcpp and RcppGSL.
2. Install Rtools (https://cran.r-project.org/bin/windows/Rtools/).
3. Download a prebuilt GSL library by downloading `localXXX.zip` from http://www.stats.ox.ac.uk/pub/Rtools/goodies/multilib/. As of May 2nd 2020 the latest version was `local323.zip`. Download this file, unzip, and paste the constituent folders `include` and `lib` into a folder called `local323` in your R directory – for example, the full path may be something like `C:\Program Files\R\R-4.0.0\local323` or `C:\Users\R-4.0.0\local323`. 
4. Go to C:\Users\<your username>, make folder `.R` if it does not exist. Make a new plain text file (e.g. in Notepad), paste the two lines below and save as `Makevars.win` in `.R` folder.
```
PKG_CPPFLAGS=-IC:/Program Files/R/R-4.0.0/local323/include -I../inst/include
PKG_LIBS=-LC:/Program Files/R/R-4.0.0/local323/lib/x64 -lgsl -lgslcblas
```
**Note: The path C:/Program Files/R/R-4.0.0/local323 in the two `Makevars.win` lines in step 4 should be the same path to your R directory as in step 3.**

## Installation for Mac OS X

You will need to install gfortran binaries from here: https://github.com/fxcoudert/gfortran-for-macOS/releases

Once installed, run `gcc --version` in terminal to get your current version, e.g. `Target: x86_64-apple-darwin18.8.2.0`. Then run below in terminal to add library path for R:

`cd ~
mkdir .R
cd .R
echo FLIBS=-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin18/8.2.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm >> Makevars
`

## Examples

These can be found in the `examples` folder. This includes the following R files:

* `examples/0-libraries.R` - Install required R libraries
* `examples/1-getting-started.R` - Compile code and run a basic simulation
* `examples/2-interventions.R` - Run intervention scenarios
* `examples/3-processes.R` - Set up an observation process (one way of calculating health burdens over time). 
* `examples/4-fitting.R` - Fitting model to data using MCMC. 
* `examples/5-observer.R` - Set up an observer to dynamically change parameters during a simulation. 

Model parameters are documented in `parameters_ref.txt`.
