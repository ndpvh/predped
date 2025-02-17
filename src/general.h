#ifndef GENERAL
#define GENERAL

#include <Rcpp.h>

std::unordered_set<std::string> unique(
    Rcpp::CharacterVector x
);

#endif