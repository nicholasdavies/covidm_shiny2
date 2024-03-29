// Rcpp_interface.cpp

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
#include <Rcpp.h>
using namespace std;

#include "randomizer.h"
#include "distribution.h"
#include "helper.h"
#include "process_spec.h"
#include "changes.h"
#include "parameters.h"
#include "compartment.h"
#include "reporter.h"
#include "sim_compartment.h"
#include "sim_household.h"
#include "user_defined.h"
#include "Rcpp_interface.h"
#include "mcmc.h"


Reporter RunSimulation(Parameters& P, Randomizer& Rand)
{
    Reporter rep(P);

    if (P.model == "SEI3R")
    {
        Metapopulation mp(P);
        mp.Run(P, Rand, rep);
    } 
    else if (P.model == "household")
    {
        Households h(P);
        h.Run(P, Rand, rep);
    }
    else
    {
        throw std::logic_error("Unrecognized model type.");
    }

    return rep;
}


// [[Rcpp::export]]
Rcpp::List cm_backend_simulate_v2(Rcpp::List parameters, unsigned int n_run = 1, unsigned long int seed = 0)
{
    // Initialise parameters for this simulation
    Randomizer Rand(seed);
    Parameters covidm_parameters;
    SetParameters(covidm_parameters, parameters, Rand);

    Rcpp::List dynamics;
    Rcpp::List csvs;

    for (unsigned int r = 0; r < n_run; ++r)
    {
        // Run the simulation
        Parameters P = covidm_parameters;
        Reporter rep = RunSimulation(P, Rand);

        // TODO Dataframe construction -- copied from old reporter.cpp ----
        // TODO Can move some of this outside the loop...
    
        // Create times
        Rcpp::NumericVector t(rep.n_times * rep.n_populations * rep.n_age_groups, 0.);
        for (unsigned int it = 0; it < rep.n_times; ++it)
            for (unsigned int j = 0; j < rep.n_populations * rep.n_age_groups; ++j)
                t[it * rep.n_populations * rep.n_age_groups + j] = P.time0 + it * P.time_step * P.report_every;

        // Create identifier columns
        Rcpp::DataFrame dynamics_df = Rcpp::DataFrame::create(
            Rcpp::Named("t") = t,
            Rcpp::Named("population") = Rcpp::rep(Rcpp::rep_each(Rcpp::seq(1, rep.n_populations), rep.n_age_groups), rep.n_times),
            Rcpp::Named("group") = Rcpp::rep(Rcpp::seq(1, rep.n_age_groups), rep.n_times * rep.n_populations)
        );

        // Allocate all columns to the dataframe
        for (unsigned int c = 0; c < rep.col_names.size(); ++c)
            dynamics_df.push_back(rep.data[c], rep.col_names[c]);

        // Add observer columns
        for (unsigned int c = 0; c < rep.obs.size(); ++c)
            dynamics_df.push_back(rep.obs[c], "obs" + to_string(c));

        // Set dataframe as a data.table
        Rcpp::Function setDT("setDT"); 
        setDT(dynamics_df);

        dynamics.push_back(dynamics_df);
        csvs.push_back(rep.csv);
        // ----
    }

    return Rcpp::List::create(
        Rcpp::Named("dynamics") = dynamics,
        Rcpp::Named("csv") = csvs
    );
}

// [[Rcpp::export]]
Rcpp::DataFrame cm_evaluate_distribution_v2(string dist_code, unsigned int steps = 101, double xmin = 0, double xmax = -1)
{
    Distribution dist(dist_code);
    if (xmax < xmin)
    {
        xmin = dist.LowerBound();
        xmax = dist.UpperBound();
    }

    vector<double> x(steps, 0.);
    vector<double> p(steps, 0.);

    for (unsigned int s = 0; s < steps; ++s)
    {
        x[s] = xmin + ((xmax - xmin) / (steps - 1.)) * s;
        p[s] = exp(dist.LogProbability(x[s]));
    }

    Rcpp::DataFrame results = Rcpp::DataFrame::create(
        Rcpp::Named("x") = x,
        Rcpp::Named("p") = p
    );

    return results;
}

// [[Rcpp::export]]
Rcpp::DataFrame cm_backend_mcmc_test(Rcpp::List R_base_parameters, Rcpp::List params_priors, unsigned long int seed, 
    unsigned int burn_in, unsigned int iterations, unsigned int n_threads, bool classic_gamma)
{
    // Initialise parameters for this simulation
    // TODO Rand also used for setting parameters -- is it actually used? this may cause issues with seeds for sample fit etc
    Randomizer Rand(seed); // randomizer for fitting; randomizers for model runs are created in the Likelihood class.
    Parameters base_parameters;

    SetParameters(base_parameters, R_base_parameters, Rand);

    vector<string> param_names = Rcpp::as<vector<string>>(params_priors.names());
    vector<Distribution> priors;

    for (unsigned int i = 0; i < params_priors.size(); ++i)
        priors.push_back(Distribution(Rcpp::as<string>(params_priors[i])));

    ///---
    unsigned int n_chains = 2 * params_priors.size();
    bool verbose = true;
    bool reeval_likelihood = false;
    bool in_parallel = n_threads > 1;
    ///---

    Likelihood lik(base_parameters, seed);
    MCMCReporter rep(iterations, n_chains, param_names);

    DEMCMC_Priors(Rand, lik, rep, burn_in, iterations, n_chains, priors, verbose, param_names, 
        reeval_likelihood, in_parallel, n_threads, classic_gamma);

    // Get data.frame as a data.table and return
    Rcpp::DataFrame df = Rcpp::DataFrame::create();
    df.push_back(Rcpp::IntegerVector::import(rep.trial.begin(), rep.trial.end()), "trial");
    df.push_back(Rcpp::NumericVector::import(rep.lp.begin(), rep.lp.end()), "lp");
    df.push_back(Rcpp::IntegerVector::import(rep.chain.begin(), rep.chain.end()), "chain");
    df.push_back(Rcpp::NumericVector::import(rep.ll.begin(), rep.ll.end()), "ll");
    for (unsigned int d = 0; d < rep.theta.size(); ++d)
        df.push_back(Rcpp::NumericVector::import(rep.theta[d].begin(), rep.theta[d].end()), rep.pnames[d]);

    return df;
}

// [[Rcpp::export]]
Rcpp::DataFrame cm_backend_optimize_test(Rcpp::List R_base_parameters, Rcpp::List params_priors, 
    unsigned int maxeval, double ftol_abs, 
    unsigned long int seed, unsigned int n_threads)
{
    // Initialise parameters for this simulation
    // TODO Rand also used for setting parameters -- is it actually used? this may cause issues with seeds for sample fit etc
    Randomizer Rand(seed); // randomizer for fitting; randomizers for model runs are created in the Likelihood class.
    Parameters base_parameters;

    SetParameters(base_parameters, R_base_parameters, Rand);

    vector<string> param_names = Rcpp::as<vector<string>>(params_priors.names());
    vector<Distribution> priors;

    for (unsigned int i = 0; i < params_priors.size(); ++i)
        priors.push_back(Distribution(Rcpp::as<string>(params_priors[i])));

    Likelihood lik(base_parameters, seed);
    MCMCReporter rep(1, 1, param_names);

    Optimize_Priors(Rand, lik, rep, priors,
        maxeval, ftol_abs, true, n_threads > 1, n_threads);

    // Get data.frame as a data.table and return
    Rcpp::DataFrame df = Rcpp::DataFrame::create();
    df.push_back(Rcpp::IntegerVector::import(rep.trial.begin(), rep.trial.end()), "trial");
    df.push_back(Rcpp::NumericVector::import(rep.lp.begin(), rep.lp.end()), "lp");
    df.push_back(Rcpp::IntegerVector::import(rep.chain.begin(), rep.chain.end()), "chain");
    df.push_back(Rcpp::NumericVector::import(rep.ll.begin(), rep.ll.end()), "ll");
    for (unsigned int d = 0; d < rep.theta.size(); ++d)
        df.push_back(Rcpp::NumericVector::import(rep.theta[d].begin(), rep.theta[d].end()), rep.pnames[d]);

    return df;
}

// [[Rcpp::export]]
Rcpp::List cm_backend_sample_fit_test(Rcpp::List R_base_parameters, Rcpp::DataFrame posterior, unsigned int n, unsigned long int seed)
{
    // Initialise parameters for this simulation
    Randomizer Rand(seed); // randomizer for fitting; randomizers for model runs are created in the Likelihood class.
    Parameters base_parameters;

    SetParameters(base_parameters, R_base_parameters, Rand);

    Rcpp::List dynamics;

    for (unsigned int i = 0; i < n; ++i)
    {
        // TODO separate fitting and model seeds...
        Randomizer r(seed);
        Parameters P(base_parameters);

        unsigned int row = Rand.Discrete(posterior.nrows());
        vector<double> theta;
        for (unsigned int j = 4; j < posterior.size(); ++j)
            theta.push_back(Rcpp::as<Rcpp::NumericVector>(posterior[j])[row]);

        CppChanges(theta, P);

        Reporter rep = RunSimulation(P, r);

        // TODO Dataframe construction -- copied from old reporter.cpp ----
        // TODO Can move some of this outside the loop... and refactor with code above which is similar
    
        // Create times
        Rcpp::NumericVector t(rep.n_times * rep.n_populations * rep.n_age_groups, 0.);
        for (unsigned int it = 0; it < rep.n_times; ++it)
            for (unsigned int j = 0; j < rep.n_populations * rep.n_age_groups; ++j)
                t[it * rep.n_populations * rep.n_age_groups + j] = P.time0 + it * P.time_step * P.report_every;

        // Create identifier columns
        int run = i + 1;
        Rcpp::DataFrame dynamics_df = Rcpp::DataFrame::create(
            Rcpp::Named("run") = Rcpp::rep(run, rep.n_times * rep.n_populations * rep.n_age_groups),
            Rcpp::Named("t") = t,
            Rcpp::Named("population") = Rcpp::rep(Rcpp::rep_each(Rcpp::seq(1, rep.n_populations), rep.n_age_groups), rep.n_times),
            Rcpp::Named("group") = Rcpp::rep(Rcpp::seq(1, rep.n_age_groups), rep.n_times * rep.n_populations)
        );

        // Allocate all columns to the dataframe
        for (unsigned int c = 0; c < rep.col_names.size(); ++c)
            dynamics_df.push_back(rep.data[c], rep.col_names[c]);

        // Add observer columns
        for (unsigned int c = 0; c < rep.obs.size(); ++c)
            dynamics_df.push_back(rep.obs[c], "obs" + to_string(c));

        // Set dataframe as a data.table
        Rcpp::Function setDT("setDT"); 
        setDT(dynamics_df);

        dynamics.push_back(dynamics_df);
    }

    return dynamics;
}
