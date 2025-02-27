#ifndef M4MA
#define M4MA

#include <Rcpp.h>

Rcpp::NumericVector psUtility(
    double a_preferred_speed, 
    double b_preferred_speed,
    double preferred_speed,
    double slowing_time, 
    Rcpp::NumericVector current_speed,
    Rcpp::NumericVector goal_distance
);

Rcpp::NumericVector gaUtility(
    double b_goal_direction, 
    double a_goal_direction,
    Rcpp::NumericVector goal_angles
);

Rcpp::NumericVector caUtility(
    double b_current_direction, 
    double a_current_direction,
    double blr_current_direction
);

Rcpp::NumericVector idUtility(
    double b_current_direction, 
    double d_current_direction,
    double a_current_direction,
    Rcpp::LogicalVector id_ingroup,
    Rcpp::LogicalMatrix id_check,
    Rcpp::NumericMatrix id_distance,
    Rcpp::NumericVector impossible_utility
); 

Rcpp::NumericVector baUtility(
    double a_blocked, 
    double b_blocked,
    Rcpp::NumericVector ba_angle,
    Rcpp::IntegerVector cone_id
);

Rcpp::NumericVector flUtility(
    double a_leader, 
    double b_leader,
    double d_leader,
    Rcpp::NumericMatrix leaders,
    Rcpp::NumericMatrix distances
);

Rcpp::NumericVector wbUtility(
    double a_buddy, 
    double b_buddy,
    Rcpp::NumericMatrix buddies,
    Rcpp::NumericMatrix distances
);

Rcpp::NumericMatrix destinationAngle(
    double orientation, 
    Rcpp::NumericMatrix agent_position,
    Rcpp::NumericMatrix goal_position
);

Rcpp::RObject predClose(
    int agent_idx,
    Rcpp::NumericMatrix agent_position,
    double orientation,
    Rcpp::NumericMatrix others_position,
    Rcpp::NumericVector radius,
    Rcpp::NumericMatrix centers,
    Rcpp::NumericMatrix predicted_positions,
    Rcpp::List objects
);

Rcpp::NumericVector blockedAngle(
    Rcpp::NumericMatrix agent_position,
    double orientation,
    double speed,
    Rcpp::NumericMatrix predictions_minus_agent,
    Rcpp::NumericVector radii,
    Rcpp::List objects
);

Rcpp::RObject getLeaders(
    int agent_idx,
    Rcpp::NumericMatrix positions,
    Rcpp::NumericVector orientations,
    Rcpp::NumericVector speeds,
    Rcpp::NumericMatrix goal_position,
    Rcpp::NumericVector groups,
    Rcpp::NumericMatrix centers,
    Rcpp::List objects
);

Rcpp::RObject getBuddy(
    int agent_idx,
    Rcpp::NumericMatrix positions,
    Rcpp::NumericVector speeds,
    Rcpp::NumericVector groups,
    Rcpp::NumericVector orientations,
    Rcpp::NumericMatrix predictions,
    Rcpp::NumericMatrix centers,
    Rcpp::List objects,
    bool pickBest
);

Rcpp::NumericVector dist1(
    Rcpp::NumericVector position, 
    Rcpp::NumericMatrix goal_position
);

Rcpp::NumericVector dist(
    Rcpp::NumericMatrix matrix_1, 
    Rcpp::NumericMatrix matrix_2
);

Rcpp::NumericMatrix c_vd(
    Rcpp::IntegerVector cells, 
    Rcpp::NumericVector position,
    double velocity,
    double angle,
    Rcpp::NumericMatrix velocities,
    Rcpp::NumericMatrix angles,
    double time_step
);

Rcpp::NumericMatrix aTOd(Rcpp::NumericVector angle);

Rcpp::NumericVector scaleVel(
    Rcpp::NumericVector velocities,
    double time_step
);

Rcpp::LogicalMatrix free_cells(
    Rcpp::S4 agent, 
    Rcpp::S4 background, 
    Rcpp::NumericMatrix centers
);

Rcpp::LogicalVector seesGoalOK(
    int agent_idx, 
    Rcpp::List objects, 
    Rcpp::List m4ma_state,
    Rcpp::NumericMatrix centers,
    Rcpp::LogicalVector check
);

Rcpp::LogicalMatrix bodyObjectOK(
    double radius,
    Rcpp::NumericMatrix centers,
    Rcpp::List objects,
    Rcpp::LogicalVector check
);

#endif