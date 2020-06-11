// convenience.cpp

// Helper functions
#include "convenience.h"
#include "parameters.h"
#include <gsl/gsl_sf_gamma.h>
#include <gsl/gsl_cdf.h>
#include <cmath>
#include <iostream>
using namespace std;

vector<double> seq(double x0, double x1, double by)
{
    int steps = int((x1 - x0) / by + by * 1e-3);
    if (steps >= 0)
    {
        vector<double> ret(steps);
        for (int i = 0; i < steps; ++i)
            ret[i] = x0 + by * i;
        return ret;
    }
    return vector<double>(0);
}

// binomial log density
double binom(double x, double size, double prob)
{
    return -log(size + 1.) - gsl_sf_lnbeta(size - x + 1., x + 1.) + x * log(prob) + (size - x) * log(1. - prob);
}

// negative binomial log density
double nbinom(unsigned int x, double mean, double size)
{
    double k = x;
    double p = size / (size + mean);
    double n = mean * p / (1 - p);

    return gsl_sf_lngamma(n + k) - gsl_sf_lngamma(k + 1) - gsl_sf_lngamma(n) + n * log(p) + k * log(1 - p);
}

// negative binomial log density with retrospective confirmation
double nbinom_gammaconf(unsigned int x, double mean, double size, double days_ago, double conf_delay_mean, double conf_delay_shape)
{
    double conf_delay_scale = conf_delay_mean / conf_delay_shape;
    double prop_confirmed = gsl_cdf_gamma_P(days_ago, conf_delay_shape, conf_delay_scale);
    return nbinom(x, mean * prop_confirmed, size);
}

// construct a delay distribution following a gamma distribution with mean mu and shape parameter shape.
vector<double> delay_gamma(double mu, double shape, double t_max, double t_step, double mult)
{
    double scale = mu / shape;
    vector<double> height;

    for (double t = 0.0; t < t_max + 0.5 * t_step; t += t_step)
        height.push_back(mult * (gsl_cdf_gamma_P(t + t_step/2, shape, scale) - 
            gsl_cdf_gamma_P(max(0.0, t - t_step/2), shape, scale)));
    return height;
}

// estimate the basic reproduction number
double estimate_R0(Parameters& P, double t, unsigned int p, unsigned int iter)
{
    vector<double> inf(P.pop[p].size.size(), 1.0);
    vector<double> inf2 = inf;

    double dIp = P.pop[p].dIp.Mean() * P.time_step;
    double dIs = P.pop[p].dIs.Mean() * P.time_step;
    double dIa = P.pop[p].dIa.Mean() * P.time_step;

    double n_inf = P.pop[p].size.size();
    double n_inf2 = 0;
    double R0 = 1;

    double seas = 1.0 + P.pop[p].season_A * cos(2. * M_PI * (t - P.pop[p].season_phi) / P.pop[p].season_T);

    for (unsigned int i = 0; i < iter; ++i)
    {
        n_inf2 = 0;
        for (unsigned int a = 0; a < inf.size(); ++a)
        {
            inf2[a] = 0;
            for (unsigned int b = 0; b < inf.size(); ++b)
            {
                inf2[a] += inf[b] * P.pop[p].cm(a, b) * P.pop[p].u[a] * seas * (
                    P.pop[p].y[b] * (P.pop[p].fIp[b] * dIp + P.pop[p].fIs[b] * dIs) +
                    (1 - P.pop[p].y[b]) * P.pop[p].fIa[b] * dIa
                );
            }
            n_inf2 += inf2[a];
        }

        R0 = n_inf2 / n_inf;

        swap(n_inf2, n_inf);
        swap(inf2, inf);
    }

    return R0;
}

// estimate the effective reproduction number
double estimate_Rt(Parameters& P, Reporter& dyn, double t, unsigned int p, unsigned int iter)
{
    vector<double> inf(P.pop[p].size.size(), 1.0);
    vector<double> inf2 = inf;
    vector<double> S(P.pop[p].size.size(), 0.0);

    for (unsigned int a = 0; a < S.size(); ++a)
        S[a] = dyn(t, p, a, 0) / P.pop[p].size[a];

    double dIp = P.pop[p].dIp.Mean() * P.time_step;
    double dIs = P.pop[p].dIs.Mean() * P.time_step;
    double dIa = P.pop[p].dIa.Mean() * P.time_step;

    double n_inf = P.pop[p].size.size();
    double n_inf2 = 0;
    double Rt = 1;

    double seas = 1.0 + P.pop[p].season_A * cos(2. * M_PI * (t - P.pop[p].season_phi) / P.pop[p].season_T);

    for (unsigned int i = 0; i < iter; ++i)
    {
        n_inf2 = 0;
        for (unsigned int a = 0; a < inf.size(); ++a)
        {
            inf2[a] = 0;
            for (unsigned int b = 0; b < inf.size(); ++b)
            {
                inf2[a] += S[a] * inf[b] * P.pop[p].cm(a, b) * P.pop[p].u[a] * seas * (
                    P.pop[p].y[b] * (P.pop[p].fIp[b] * dIp + P.pop[p].fIs[b] * dIs) +
                    (1 - P.pop[p].y[b]) * P.pop[p].fIa[b] * dIa
                );
            }
            n_inf2 += inf2[a];
        }

        Rt = n_inf2 / n_inf;

        swap(n_inf2, n_inf);
        swap(inf2, inf);
    }

    return Rt;
}

