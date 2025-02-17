#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "m4ma.h"
#include <typeinfo>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]



//' Group centroid utility
//'
//' Rcpp alternative for the group centroid utility function. 
//' 
//' @param a_gc Numeric denoting the power to which to take the utility.
//' @param b_gc Numeric denoting the slope of the utility function.
//' @param radius Numeric denoting the radius of the agent.
//' @param cell_dist Numeric vector denoting the distance of each cell in the 
//' \code{centers} to the predicted group centroid.
//' @param stop_utility Numeric denoting the utility of stopping. Is used to 
//' ensure the agents do not freeze when they are too far away from each other. 
//' @param nped Numeric denoting the number of ingroup members. 
//' 
//' @return Numeric vector containing the group-centroid-related utility for each 
//' cell. 
//' 
//' @seealso 
//' \code{\link[predped]{distance_group_centroid}},
//' \code{\link[predped]{params_from_csv}},
//' \code{\link[predped]{utility}}
//' 
//' @rdname gc_utility_rcpp
//' 
//' @export
// [[Rcpp::export]]
NumericVector gc_utility_rcpp(double a_group_centroid, 
                              double b_group_centroid, 
                              double radius, 
                              NumericVector cell_distances, 
                              double stop_utility, 
                              int nped) {
    
    // Compute the optimal distance to keep from the group. Within the radius 
    // of this distance, all utilities are equal to 0. Otherwise, they are 
    // equal to the output of the group centroid utility function
    double optimal_distance = 1.5 * nped * radius;
    
    NumericVector V(cell_distances.length());
    LogicalVector check(cell_distances.length());
    for(int i = 0; i < cell_distances.length(); i++) {
        if(cell_distances[i] < optimal_distance) {
            V[i] = 0;
        } else {
            double difference = cell_distances[i] - optimal_distance;
            difference = std::abs(difference);

            V[i] = -1 * b_group_centroid * std::pow(difference, a_group_centroid);
        }

        check[i] = V[i] < stop_utility;
    }

    // Perform a check: If all utilities are lower than the stop utility, return
    // a vector with only zeros. This makes sure that agents are still able to 
    // walk around even when separated from their friends.
    if(is_true(all(check))) {
        NumericVector Zero(cell_distances.length());
        return Zero;
    }
    
    return V;
}

//' Discrete visual field utility
//' 
//' Rcpp alternative to the \code{vf_utility_discrete} function.
//' 
//' The idea of this utility function is that it doesn't matter at which angle 
//' you see a group member within the visual field, as long as you see them. 
//' This translates to a discrete added disutility whenever the group member 
//' falls inside the non-visual zone behind the agent.
//' 
//' @param b_vf Numeric denoting the slope of the utility function. 
//' @param rel_angles Numeric vector containing the relative angle from each cell 
//' center to the predicted positions of the group members. Typically output of 
//' \code{\link[predped]{get_angle}}. 
//' 
//' @return Numeric vector containing the utility attributed to keeping the 
//' group members within your visual field. Returns 0's if the agent does not 
//' have any additional group members.
//' 
//' @seealso 
//' \code{\link[predped]{get_angles}},
//' \code{\link[predped]{utility}},
//' \code{\link[predped]{vf_utility_continuous}}
//' 
//' @rdname vf_utility_rcpp
//' 
//' @export
// [[Rcpp::export]]
NumericVector vf_utility_rcpp(double b_visual_field, 
                              NumericVector relative_angles) {
     
    
    double lower_angle = 130 * M_PI / 180;
    double upper_angle = 2 * M_PI - lower_angle;

    NumericVector V(relative_angles.length());
    for(int i = 0; i < relative_angles.length(); i++) {
        if((relative_angles[i] > lower_angle) && (relative_angles[i] < upper_angle)) {
            V[i] = -1 * b_visual_field;
        } else {
            V[i] = 0.;
        }
    }

    return V;
}

//' Utility
//'
//' This function is the Rcpp equivalent of \code{\link[predped]{utility}}. It
//' takes in a dataframe containing all of the relevant values for computing the
//' utility, as well as a dataframe containing the parameters. Heavily depends 
//' on the \code{m4ma} package.
//' 
//' @param object Dataframe containing all of the needed information to compute 
//' the utilities. Typically output of the 
//' \code{\link[predped]{compute_utility_variables}} function.
//' @param parameters Dataframe containing the parameters of the agent. Should 
//' conform to the naming conventions mentioned in 
//' \code{\link[predped]{params_from_csv}}.
//' 
//' @return Numeric vector denoting the (dis)utility of moving to each of the 
//' potential cells.
//' 
//' @seealso 
//' \code{\link[predped]{simulate,predped-method}},
//' \code{\link[predped]{simulate,state-method}},
//' \code{\link[predped]{update,agent-method}},
//' \code{\link[predped]{update,state-method}},
//' \code{\link[predped]{utility,agent-method}},
//' \code{\link[predped]{compute_utility_variables}},
//' \code{\link[predped]{params_from_csv}},
//' \code{\link[predped]{update_position}}
//' 
//' @rdname utility_rcpp
//' 
// [[Rcpp::export]]
NumericVector utility_rcpp(DataFrame data, 
                           DataFrame parameters) {

    // Create an empty vector of the same size as needed for the computation
    List check_list = data["check"];
    LogicalMatrix check = check_list[0];
    int rows = check.nrow();
    int cols = check.ncol();

    NumericVector V(rows * cols);

    // Preferred speed utility: Check whether the distance to the goal is not 
    // NULL and, if not, compute the utility of deceleration, acceleration, or 
    // maintenance of speed
    if(!(data["ps_distance"] == R_NilValue)) {
        V += psUtility(
            parameters["a_preferred_speed"], 
            parameters["b_preferred_speed"], 
            parameters["preferred_speed"], 
            parameters["slowing_time"], 
            data["ps_speed"], 
            data["ps_distance"]
        );
    }

    // Goal direction utility: Check whether the angle to the goal is defined and,
    // if so, compute the utility of heading in a given direction relative to 
    // where the goal is located
    List gd_angle = data["gd_angle"];
    if(!(gd_angle[0] == R_NilValue)) {
        V += gaUtility(
            parameters["b_goal_direction"], 
            parameters["a_goal_direction"], 
            gd_angle[0]
        );
    }

    // Current direction utility: Compute the utility of heading in a given 
    // direction. No other variables needed for this.
    V += caUtility(
        parameters["a_current_direction"], 
        parameters["b_current_direction"], 
        parameters["blr_current_direction"]
    );

    // Interpersonal distance utility: Check whether the distance to other 
    // pedestrians is defined and, if so, compute the utility.
    NumericVector V_id(rows * cols);
    for(int i = 0; i < V_id.length(); i++) {
        if(check[i]) {
            V_id[i] = 0;
        } else {
            V_id[i] = -1 * arma::datum::inf;
        }
    }

    List id_distance = data["id_distance"];
    if(!(id_distance[0] == R_NilValue)) {
        List id_ingroup = data["id_ingroup"];

        V += idUtility(
            parameters["b_interpersonal"], 
            parameters["d_interpersonal"], 
            parameters["a_interpersonal"], 
            id_ingroup[0], 
            check,
            id_distance[0], 
            V_id
        );
    } else {
        V += V_id;
    }

    // Blocked angle utility: Check whether any of the angles are blocked in the 
    // first place, and if so, compute the utility
    List ba_angle_list = data["ba_angle"];
    if(!(ba_angle_list[0] == R_NilValue)) {
        // Retrieve the numbers of the cones that could be blocked on the long 
        // term. Additionally, make sure that all angles are either equal to 
        // or greater than 0.
        NumericVector ba_angle = ba_angle_list[0];

        List ba_cones_list = data["ba_cones"];
        IntegerVector ba_cones = ba_cones_list[0];

        // Adjust the index of the cones so that it corresponds to C++ instead
        // of R. Additionally make sure angles are either 0 or greater than 0.
        for(int i = 0; i < ba_angle.length(); i++) {
            ba_cones[i] -= 1;

            if(ba_angle[i] < 0) {
                ba_angle[i] = 0.;
            }
        }

        V += baUtility(
            parameters["a_blocked"], 
            parameters["b_blocked"],
            ba_angle, 
            ba_cones - 1
        );
    }

    // Follow the leader utility: Check whether there are any leaders in the first 
    // place and, if so, compute the utility
    List fl_leaders_list = data["fl_leaders"];
    if(!(fl_leaders_list[0] == R_NilValue)) {
        List fl_leaders = fl_leaders_list[0];

        V += flUtility(
            parameters["a_leader"], 
            parameters["b_leader"], 
            parameters["d_leader"], 
            fl_leaders["leaders"], 
            fl_leaders["dists"]
        );
    }

    // Walk beside utility: Check whether there are any buddies and, if so, 
    // compute the utility
    List wb_buddies_list = data["wb_buddies"];
    if(!(wb_buddies_list[0] == R_NilValue)) {
        List wb_buddies = wb_buddies_list[0];

        V += wbUtility(
            parameters["a_buddy"], 
            parameters["b_buddy"], 
            wb_buddies["buddies"], 
            wb_buddies["dists"]
        );
    }

    // Group centroid utility: Check whether people are walking in a group in 
    // the first place and, if so, compute the utility
    List gc_distance = data["gc_distance"];
    if(!(gc_distance[0] == R_NilValue)) {
        NumericVector distances = gc_distance[0];
        double stop_utility = parameters["stop_utility"];

        V += gc_utility_rcpp(
            parameters["a_group_centroid"],
            parameters["b_group_centroid"],
            data["gc_radius"],
            distances,
            -1 * stop_utility,
            data["gc_nped"]
        );
    }

    // Visual field utility: Check whether people are walking in a group and, if 
    // so, compute the utility
    List vf_angles = data["vf_angles"];
    if(!(vf_angles[0] == R_NilValue)) {
        V += vf_utility_rcpp(
            parameters["b_visual_field"],
            vf_angles[0]
        );
    }





    // Add the stopping utility to the vector and transform them according to the 
    // randomness parameter
    double stop_utility = parameters["stop_utility"];
    double randomness = parameters["randomness"];

    V.insert(V.begin(), -1 * stop_utility);
    for(int i = 0; i < V.length(); i++) {
        V[i] = V[i] / randomness;
    }

    return V;    
}






