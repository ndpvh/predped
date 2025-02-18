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
//' @param data Data.frame containing at least "id", "time", "x", "y", "goal_x",
//' "goal_y", and "goal_id". If it does not have the utility variables yet, these
//' will add them to the data.frame.
//' @param parameters Numeric vector or matrix containing the parameters to be 
//' used. Should be specified in the same order as specified in 
//' \code{"parameter_names"}. If a matrix, each row should contain parameters to 
//' be estimated for each instance of "id" separately.
//' @param parameter_names Character vector containing the parameters that you 
//' want to estimate. Defaults to all parameters defined in
//' \code{\link[predped]{params_from_csv}}. Whenever not all parameters are used,
//' the excluded parameters are assumed to have a value of 0.
//' @param transform Logical denoting whether to transform the provided parameters
//' from the real axis to the bounded scales imposed on the parameters within 
//' \code{predped}. Defaults to \code{TRUE}.
//' @param bounds Matrix containing the lower and upper bounds of the parameters
//' in its first and second column respectively. Additionally, rownames should 
//' denote for which parameter a certain pair represents the bounds. Only used 
//' when \code{transform = TRUE}. Defaults to the default bounds of \code{predped}.
//' @param ... Additional arguments passed on to \code{\link[predped]{add_motion_variables}}.
//' In a typical estimation situation, these motion variables should already be 
//' in \code{data}.
//' 
//' @return Min-log-likelihood per person in the dataset.
//' 
//' @export 
// [[Rcpp::export]]
NumericVector mll_rcpp(DataFrame data, 
                       DataFrame parameters,
                       CharacterVector parameter_names,
                       CharacterVector ids) {

    // Create an index saying which data belong to which participant
    CharacterVector id_column = data["id"];
    
    // Loop over each row and do the actual computations
    NumericVector MLL(ids.length());
    for(int i = 0; i < data.nrow(); i++) {
        // Get the index of the person who corresponds to the data
        int idx = 0;
        for(int j = 0; j < ids.length(); j++) {
            if(id_column[i] == ids[j]) {
                idx = j;
            }
        }

        // Compute the utility of the data under the parameters of the person.
        // Note that we need to transform the indices to +1 because we are using
        // an R function for subsetting rather than an Rcpp function.
        DataFrame data_i = subset(data, i + 1, R_MissingArg);
        DataFrame params_i = subset(parameters, idx + 1, R_MissingArg);
        
        NumericVector V = utility_rcpp(
            data_i, 
            params_i
        );

        // Transform V to probabilities and afterwards to the min-log-likelihood.
        // Add the value of this likelihood to the MLL vector.
        V = V - *std::max_element(V.begin(), V.end());
        V = Rcpp::exp(V);
        NumericVector P = V / Rcpp::sum(V);
        
        int cell = data_i["cell"];
        MLL[idx] -= log(1 + P[cell]);
    }

    MLL.attr("names") = ids;
    return MLL;
}