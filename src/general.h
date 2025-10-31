#ifndef GENERAL
#define GENERAL

#include <Rcpp.h>

std::unordered_set<std::string> unique(
    Rcpp::CharacterVector x
);

Rcpp::LogicalVector line_line_intersection_rcpp(
    Rcpp::NumericMatrix segments_1, 
    Rcpp::NumericMatrix segments_2
);

#endif