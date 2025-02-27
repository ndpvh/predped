#ifndef UPDATE
#define UPDATE

#include <Rcpp.h>

Rcpp::List create_agent_specifications_rcpp(
    Rcpp::List agent_list,
    bool stay_stopped = true, 
    double time_step = 0.5
);

#endif