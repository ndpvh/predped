#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <algorithm>
#include "general.h"
#include "utility_model.h"

using namespace Rcpp;

Function subset("[.data.frame");

// [[Rcpp::depends(RcppArmadillo)]]

//' Compute the min-log-likelihood
//' 
//' Rcpp alternative to \code{\link[predped]{mll}}. Be wary: This version does 
//' not automatically add the motion variables to the data if not present in 
//' the data, nor does it do any of the other preprocessing. It is therefore 
//' better not used as an alternative to the R version, but rather as an 
//' extension of it (as done automatically in predped). 
//'
//' @param data List containing data.frames to use in the estimation procedure.
//' @param parameters List containing the parameters to be used.
//' Should be specified in the same order as specified in \code{"parameter_names"}. 
//' @param ids CharacterVector containing the names of the participants in the 
//' data set.
//' @param idx IntegerVector containing the index of the parameters to use to 
//' evaluate a given row in the data. Note that this index uses C++ convention.
//' Order should conform to the order in the list of the data.
//' @param cells IntegerVector denoting the cell to which a participant has 
//' moved at a given iteration. Order should conform to the order in the list of 
//' the data.
//' 
//' @return Min-log-likelihood per person in the dataset.
//' 
//' @export 
// [[Rcpp::export]]
NumericVector mll_rcpp(List data, 
                       List parameters,
                       CharacterVector ids,
                       IntegerVector idx,
                       IntegerVector cells) {

    // Loop over each row and do the actual computations
    NumericVector MLL(ids.length());
    NumericVector V(34);

    for(int i = 0; i < data.length(); i++) {
        // Compute the utility of the data under the parameters of the person.
        // Note that we need to transform the indices to +1 because we are using
        // an R function for subsetting rather than an Rcpp function.
        V = utility_rcpp(
            as<DataFrame>(data[i]),
            as<DataFrame>(parameters[idx[i]])
        );

        // Transform V to probabilities and afterwards to the min-log-likelihood.
        // Add the value of this likelihood to the MLL vector.
        V = Rcpp::exp(V - *std::max_element(V.begin(), V.end()));
        MLL[idx[i]] -= log(1 + V[cells[i]] / Rcpp::sum(V));
    }

    MLL.attr("names") = ids;
    return MLL;
}