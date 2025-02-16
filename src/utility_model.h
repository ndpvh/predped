#ifndef UTILITY
#define UTILITY

#include <Rcpp.h>

Rcpp::NumericVector utility_rcpp(
    Rcpp::DataFrame data, 
    Rcpp::DataFrame parameters
);

Rcpp::NumericVector gc_utility_rcpp(
    double a_group_centroid, 
    double b_group_centroid, 
    double radius, 
    Rcpp::NumericVector cell_distances, 
    double stop_utility, 
    int nped
);

Rcpp::NumericVector vf_utility_rcpp(
    double b_visual_field, 
    Rcpp::NumericVector relative_angles
);

#endif