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

// [[Rcpp::export]]
NumericMatrix destinationAngle(double orientation, 
                               NumericMatrix agent_position,
                               NumericMatrix goal_position) {

    Function f = m4ma["destinationAngle_rcpp"];
    return f(
        orientation, 
        agent_position,
        goal_position
    );
} 

// [[Rcpp::export]] 
RObject predClose(int agent_idx,
                  NumericMatrix agent_position,
                  double orientation,
                  NumericMatrix others_position,
                  NumericVector radius,
                  NumericMatrix centers,
                  NumericMatrix predicted_positions,
                  List objects) {
    
    Function f = m4ma["predClose_rcpp"];
    Nullable<NumericMatrix> result = f(
        agent_idx,
        agent_position,
        orientation,
        others_position,
        radius,
        centers,
        predicted_positions,
        objects
    );

    return RObject(result);
}

// [[Rcpp::export]] 
NumericVector blockedAngle(NumericMatrix agent_position,
                           double orientation,
                           double speed,
                           NumericMatrix predictions_minus_agent,
                           NumericVector radii,
                           List objects) {
    
    Function f = m4ma["blockedAngle_rcpp"];
    return f(
        agent_position,
        orientation,
        speed,
        predictions_minus_agent,
        radii,
        objects
    );
}

RObject getLeaders(int agent_idx,
                   NumericMatrix positions,
                   NumericVector orientations,
                   NumericVector speeds,
                   NumericMatrix goal_position,
                   NumericVector groups,
                   NumericMatrix centers,
                   List objects) {
    
    Function f = m4ma["getLeaders_rcpp"];
    Nullable<List> result = f(
        agent_idx,
        positions,
        orientations,
        speeds,
        goal_position,
        groups,
        centers,
        objects
    );

    return RObject(result);
}

RObject getBuddy(int agent_idx,
                 NumericMatrix positions,
                 NumericVector speeds,
                 NumericVector groups,
                 NumericVector orientations,
                 NumericMatrix predictions,
                 NumericMatrix centers,
                 List objects,
                 bool pickBest) {
    
    Function f = m4ma["getBuddy_rcpp"];
    Nullable<List> result = f(
        agent_idx,
        positions,
        speeds,
        groups,
        orientations,
        predictions,
        centers,
        objects,
        pickBest
    );

    return RObject(result);
}

NumericVector dist1(NumericVector position, 
                    NumericMatrix goal_position) {

    Function f = m4ma["dist1_rcpp"];
    return f(
        position, 
        goal_position
    );
}

NumericVector dist(NumericMatrix matrix_1, 
                   NumericMatrix matrix_2) {

    Function f = m4ma["dist_rcpp"];
    return f(
        matrix_1, 
        matrix_2
    );
}

NumericMatrix c_vd(IntegerVector cells, 
                   NumericVector position,
                   double velocity,
                   double angle,
                   NumericMatrix velocities,
                   NumericMatrix angles,
                   double time_step) {
    
    Function f = m4ma["c_vd_rcpp"];
    return f(
        cells,
        position,
        velocity,
        angle,
        velocities,
        angles,
        time_step
    );
}

NumericVector aTOd(NumericVector angle) {

    Function f = m4ma["aTOd_rcpp"];
    return f(angle);
}

NumericVector scaleVel(NumericVector velocities,
                       double time_step) {

    Function f = m4ma["scaleVel_rcpp"];
    return f(
        velocities,
        time_step
    );
}

LogicalMatrix free_cells(S4 agent, 
                         S4 background, 
                         NumericMatrix centers) {
    
    Function f = m4ma["free_cells_rcpp"];
    return f(
        agent,
        background,
        centers
    );
}

LogicalVector seesGoalOK(int agent_idx, 
                         List objects, 
                         List m4ma_state,
                         NumericMatrix centers,
                         LogicalVector check) {
    
    Function f = m4ma["seesGoalOK_rcpp"];
    return f(
        agent_idx,
        objects,
        m4ma_state,
        centers,
        check
    );
}

// [[Rcpp::export]]
LogicalMatrix bodyObjectOK(double radius,
                                     NumericMatrix centers,
                                     List objects,
                                     LogicalVector check) {

    Function f = m4ma["bodyObjectOK_rcpp"];

    Nullable<LogicalMatrix> new_check = f(
        radius,
        centers,
        objects,
        check
    );

    if(!(new_check == R_NilValue)) {
        LogicalMatrix result = new_check.get(); 
        return result;
    }
    
    return LogicalMatrix(11, 3);
}
