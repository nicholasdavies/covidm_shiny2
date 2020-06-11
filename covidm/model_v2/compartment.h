//
// COMPARTMENT
//

#ifndef COMPARTMENT_H
#define COMPARTMENT_H

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppGSL)]]

#include <vector>
#include <iostream>
#include <fstream>
#include <stdexcept>
#include <algorithm>
#include <numeric>
#include <ctime>
#include <limits>
#include <omp.h>
using namespace std;

#include "randomizer.h"
#include "distribution.h"
#include "helper.h"
#include "process_spec.h"
#include "changes.h"
#include "parameters.h"
#include "compartment.h"
#include "reporter.h"

struct Parameters;
class Randomizer;

// A compartment containing individuals
class Compartment
{
public:
    // Construct the compartment
    Compartment();

    // Add n individuals to the compartment, with maturation times t
    // Note that t[0] means individuals who will be in the compartment for 0 time steps
    void Add(Parameters& P, Randomizer& Rand, double n, Discrete& mat);

    // Mature the compartment by one time step, returning the number of individuals who have left the compartment.
    double Mature();

    // Return the total number of individuals in the compartment
    double Size();
    
private:
    // Individuals in the compartment indexed by the number of days to maturation
    vector<double> contents;

    // Total number of individuals in the compartment
    double size;
};

#endif