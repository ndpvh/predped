#include <Rcpp.h>

using namespace Rcpp;

// Load M4MA. Script used to load all m4ma utility functions into predped, 
// allowing predped to use them within the cpp code
Environment m4ma = Environment::namespace_env("m4ma");

// [[Rcpp::export]]
NumericVector psUtility(double a_preferred_speed, 
                        double b_preferred_speed,
                        double preferred_speed,
                        double slowing_time, 
                        NumericVector current_speed,
                        NumericVector goal_distance) {

    Function f = m4ma["psUtility_rcpp"];
    return f(
        a_preferred_speed,
        b_preferred_speed,
        preferred_speed,
        slowing_time,
        current_speed,
        goal_distance
    );
} 

// [[Rcpp::export]]
NumericVector gaUtility(double b_goal_direction, 
                        double a_goal_direction,
                        NumericVector goal_angles) {

    Function f = m4ma["gaUtility_rcpp"];
    return f(
        b_goal_direction,
        a_goal_direction,
        goal_angles
    );
} 

// [[Rcpp::export]]
NumericVector caUtility(double b_current_direction, 
                        double a_current_direction,
                        double blr_current_direction) {

    Function f = m4ma["caUtility_rcpp"];
    return f(
        b_current_direction,
        a_current_direction,
        blr_current_direction
    );
} 

// [[Rcpp::export]]
NumericVector idUtility(double b_current_direction, 
                        double d_current_direction,
                        double a_current_direction,
                        LogicalVector id_ingroup,
                        LogicalMatrix id_check,
                        NumericMatrix id_distance,
                        NumericVector impossible_utility) {

    Function f = m4ma["idUtility_rcpp"];
    return f(
        b_current_direction, 
        d_current_direction,
        a_current_direction,
        id_ingroup,
        id_check,
        id_distance,
        impossible_utility
    );
} 

// [[Rcpp::export]]
NumericVector baUtility(double a_blocked, 
                        double b_blocked,
                        NumericVector ba_angle,
                        IntegerVector cone_id) {

    Function f = m4ma["baUtility_rcpp"];
    return f(
        a_blocked,
        b_blocked,
        ba_angle,
        cone_id
    );
} 

// [[Rcpp::export]]
NumericVector flUtility(double a_leader, 
                        double b_leader,
                        double d_leader,
                        NumericMatrix leaders,
                        NumericMatrix distances) {

    Function f = m4ma["flUtility_rcpp"];
    return f(
        a_leader,
        b_leader,
        d_leader,
        leaders,
        distances
    );
} 

// [[Rcpp::export]]
NumericVector wbUtility(double a_buddy, 
                        double b_buddy,
                        NumericMatrix buddies,
                        NumericMatrix distances) {

    Function f = m4ma["wbUtility_rcpp"];
    return f(
        a_buddy,
        b_buddy,
        buddies,
        distances
    );
} 