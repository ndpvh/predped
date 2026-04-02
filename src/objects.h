#ifndef OBJECTS
#define OBJECTS

#include <Rcpp.h>

Rcpp::NumericMatrix nodes_on_circumference_rcpp(
    Rcpp::S4 object,
    double space_between
);

Rcpp::LogicalVector in_object_rcpp(
    Rcpp::S4 object,
    Rcpp::NumericMatrix x
);

#endif