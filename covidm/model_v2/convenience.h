// convenience.h

#ifndef CONVENIENCE_H
#define CONVENIENCE_H

#include <vector>

struct Parameters;
class Reporter;

// TODO perform rigorous error checking on inputs

// Helper functions for user code
std::vector<double> seq(double x0, double x1, double by = 1);

// binomial log density
double binom(double x, double size, double prob);

// negative binomial log density
double nbinom(unsigned int x, double mean, double size);

// negative binomial log density with retrospective confirmation
double nbinom_gammaconf(unsigned int x, double mean, double size, double days_ago, double conf_delay_mean, double conf_delay_shape);

// construct a delay distribution following a gamma distribution with mean mu and shape parameter shape.
std::vector<double> delay_gamma(double mu, double shape, double t_max, double t_step, double mult = 1.);

// estimate the basic reproduction number
double estimate_R0(Parameters& P, double t, unsigned int p, unsigned int iter);

// estimate the effective reproduction number
double estimate_Rt(Parameters& P, Reporter& rep, double t, unsigned int p, unsigned int iter);


#endif