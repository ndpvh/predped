#ifndef UTILITY
#define UTILITY

#include <Rcpp.h>

Rcpp::DataFrame compute_utility_variables_rcpp(
    Rcpp::S4 agent,
    Rcpp::S4 state,
    Rcpp::S4 background,
    Rcpp::List agent_specifications,
    Rcpp::NumericMatrix centers,                    
    Rcpp::LogicalMatrix check
);

Rcpp::NumericVector utility_rcpp(
    Rcpp::DataFrame data, 
    Rcpp::DataFrame parameters
);

Rcpp::Nullable<Rcpp::NumericVector> distance_group_centroid_rcpp(
    Rcpp::NumericMatrix predictions, 
    Rcpp::NumericMatrix centers, 
    int number_agents
);

Rcpp::NumericVector gc_utility_rcpp(
    double a_group_centroid, 
    double b_group_centroid, 
    double radius, 
    Rcpp::NumericVector cell_distances, 
    double stop_utility, 
    int nped
);

Rcpp::Nullable<Rcpp::NumericVector> get_angles_rcpp(
    int agent_idx, 
    Rcpp::NumericVector agent_groups, 
    Rcpp::NumericVector position, 
    double orientation,  
    Rcpp::NumericMatrix predictions, 
    Rcpp::NumericMatrix centers,
    bool any_member = true
);

Rcpp::NumericVector vf_utility_rcpp(
    double b_visual_field, 
    Rcpp::NumericVector relative_angles
);

#endif