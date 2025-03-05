#ifndef MOVING_OPTIONS
#define MOVING_OPTIONS

#include <Rcpp.h>

Rcpp::LogicalMatrix overlap_with_objects_rcpp(
    Rcpp::S4 agent, 
    Rcpp::S4 background, 
    Rcpp::NumericMatrix centers, 
    Rcpp::LogicalMatrix check,
    double space_between
);

Rcpp::LogicalMatrix moving_options_rcpp(
    Rcpp::S4 agent, 
    Rcpp::S4 state, 
    Rcpp::S4 background, 
    Rcpp::NumericMatrix centers
);

#endif