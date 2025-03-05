#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
std::unordered_set<std::string> unique(CharacterVector x) {
    auto X = Rcpp::as<std::vector<std::string>>(x);
    return std::unordered_set<std::string>(X.begin(), X.end());
}