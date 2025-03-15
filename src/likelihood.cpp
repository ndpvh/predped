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
//' @param sizes IntegerVector containing the number of data points per person.
//' Ignored if \code{summed} is \code{TRUE}.
//' @param summed Boolean denoting whether to sum the min-log-likelihood to one
//' value per person. If \code{TRUE}, you get the resulting summed 
//' min-log-likelihood for each individual with a correction to avoid \code{-Inf}s.
//' If \code{FALSE}, the function will instead return a list of vectors containing
//' the raw likelihoods (not min-log-likelihoods!), allowing users to specify 
//' their own corrections (if needed).
//' 
//' @return Either named vector containing the summed min-log-likelihood 
//' (\code{summed = TRUE}) or named list with vectors of raw likelihoods
//' (\code{summed = FALSE}) per person in the dataset.
//' 
//' @export 
// [[Rcpp::export]]
List mll_rcpp(List data, 
              List parameters,
              CharacterVector ids,
              IntegerVector idx,
              IntegerVector cells,
              IntegerVector sizes,
              bool summed) {

    // Distinguish between summing the MLL for each iteration (summed = TRUE)
    // and not doing that. Requires different memory allocation in the beginning
    List MLL(ids.length());
    if(summed) {
        // Initialize an MLL list as a collection of doubles. If not done, then 
        // we cannot allocate the results to it
        for(int i = 0; i < MLL.length(); i++) {
            MLL[i] = 0.;
        }

        // Loop over each row and do the actual computations
        NumericVector V(34);
        double MLL_i = 0.;

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

            MLL_i = MLL[idx[i]]; 
            MLL_i -= log(V[cells[i]] / Rcpp::sum(V));
            MLL[idx[i]] = MLL_i;
        }

    } else {
        // Initialize an MLL list and an additional indexing vector that tells us 
        // how far along we are along the NumericVector of MLLs for each specific 
        // participant
        for(int i = 0; i < MLL.length(); i++) {
            MLL[i] = NumericVector(sizes[i]);
        }

        IntegerVector idx_participant(ids.length());

        // Loop over each iteration and assign the likelihood to the MLL list of the 
        // respective participant
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

            NumericVector MLL_i = MLL[idx[i]];
            MLL_i[idx_participant[idx[i]]] = V[cells[i]] / Rcpp::sum(V);
            MLL[idx[i]] = MLL_i;

            // Update the participant index
            idx_participant[idx[i]]++;
        }
    }

    MLL.attr("names") = ids;
    return MLL;
}