#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <algorithm>
#include "general.h"
#include "m4ma.h"
#include "moving_options.h"
#include "update.h"
#include "utility_model.h"

using namespace Rcpp;

// // Import the moving_options function => At some point, it can become an Rcpp 
// // function, but now is not the time. Note, however, that this makes the 
// // functions that call this function will be slower because of this choice.
// LogicalMatrix imported_moving_options(S4 agent, 
//                                       S4 state,
//                                       S4 background,
//                                       NumericMatrix centers) {
    
//     Function f("moving_options"); 
//     return f(
//         agent,
//         state,
//         background,
//         centers
//     );
// }




////////////////////////////////////////////////////////////////////////////////
// STARTING WITH A TRACE

//' Transform trace to time-series
//'
//' Rcpp alternative for \code{\link[predped]{time_series}}.
//' 
//' @param trace List of objects of the \code{\link[predped]{state-class}}
//' @param time_step Numeric denoting the time between each iteration. Defaults 
//' to \code{0.5} (the same as in \code{\link[predped]{simulate,predped-method}}).
//' 
//' @examples
//' # This is my example
//'
//' @rdname time_series_rcpp
//' 
//' @export
// [[Rcpp::export]]
DataFrame time_series_rcpp(List trace, 
                           double time_step = 0.5) {

    // Find out how many rows the dataframe should have
    int N = 0;
    for(int i = 0; i < trace.length(); i++) {
        S4 state = trace[i];
        List agents = state.slot("agents");
        N += agents.length();
    }

    // Initialize all of the needed variables
    IntegerVector iteration(N);
    NumericVector time(N);

    CharacterVector id(N);
    NumericVector x(N);
    NumericVector y(N);
    NumericVector speed(N);
    NumericVector orientation(N);
    IntegerVector cell(N);
    NumericVector group(N);
    CharacterVector status(N);
    NumericVector radius(N);

    CharacterVector goal_id(N);
    NumericVector goal_x(N);
    NumericVector goal_y(N);

    // Loop over the different instances in the trace.
    int idx = 0;
    for(int i = 0; i < trace.length(); i++) {
        // Extract the state and its agents from the trace
        S4 state = trace[i];
        List agents = state.slot("agents");

        if(agents.length() == 0) {
            continue;
        }

        // Loop over each of the agents separately
        for(int j = 0; j < agents.length(); j++) {
            // Extract state variables
            int iteration_j = state.slot("iteration");
            double time_j = iteration_j * time_step;

            iteration[idx] = iteration_j;
            time[idx] = time_j;

            // Extract some agent characteristics
            S4 agent = agents[j];

            std::string id_j = agent.slot("id");
            NumericVector position = agent.slot("center");
            double x_j = position[0];
            double y_j = position[1];
            double speed_j = agent.slot("speed");
            double orientation_j = agent.slot("orientation");
            int cell_j = agent.slot("cell");
            double group_j = agent.slot("group");
            std::string status_j = agent.slot("status");
            double radius_j = agent.slot("radius");

            id[idx] = id_j;
            x[idx] = x_j;
            y[idx] = y_j;
            speed[idx] = speed_j;
            orientation[idx] = orientation_j;
            cell[idx] = cell_j;
            group[idx] = group_j;
            status[idx] = status_j;
            radius[idx] = radius_j;

            // Extract agent goal characteristics
            S4 goal = agent.slot("current_goal");

            std::string goal_id_j = goal.slot("id");
            NumericVector goal_position = goal.slot("position");
            double goal_x_j = goal_position[0];
            double goal_y_j = goal_position[1];

            goal_id[idx] = goal_id_j;
            goal_x[idx] = goal_x_j;
            goal_y[idx] = goal_y_j;

            // Update the index
            idx++;
        }
    }

    // Create a List with all of the variables of interest and transform to a 
    // data.frame
    List data = List::create(
        Named("iteration") = iteration,
        Named("time") = time,
        Named("id") = id,
        Named("x") = x,
        Named("y") = y,
        Named("speed") = speed,
        Named("orientation") = orientation,
        Named("cell") = cell,
        Named("group") = group,
        Named("status") = status,
        Named("goal_id") = goal_id,
        Named("goal_x") = goal_x,
        Named("goal_y") = goal_y,
        Named("radius") = radius
    );

    data.attr("class") = "data.frame";
    data.attr("row.names") = seq(1, idx);
    return data;
}



//' Transform trace to comprehensive data.frame
//' 
//' Rcpp alternative for the \code{\link[predped]{unpack_trace}} function.
//'
//' This function will take a trace and return a data.frame containing all 
//' information contained within a typical time-series (cfr. 
//' \code{\link[predped]{time_series}}) and with all the input that should be 
//' provided to the utility functions. This is therefore the primary function to 
//' use if you want to go from a trace to a data.frame that can be used in 
//' M4MA-based estimations.
//' 
//' @param trace List of objects of the \code{\link[predped]{state-class}}
//' @param velocities Numeric matrix containing the change in speed for an agent
//' whenever they move to the respective cell of this matrix. Is used to create 
//' the cell positions that the agent might move to, as performed through 
//' \code{\link[m4ma]{c_vd_rcpp}}. Currently limited to having 11 rows (direction) 
//' and 3 columns (speed). Defaults to a matrix in which the columns contain 
//' \code{1.5} (acceleration), \code{1}, and \code{0.5}.
//' @param orientations Numeric matrix containing the change in direction for an 
//' agent whenever they move to the respective cell of this matrix. Is used to 
//' create the cell positions that the agent might move to, as performed through
//' \code{\link[m4ma]{c_vd_rcpp}}. Currently limited to having 11 rows (direction)
//' and 3 columns (speed). Defaults to a matrix in which the rows contain 
//' \code{72.5}, \code{50}, \code{32.5}, \code{20}, \code{10}, code{0}, \code{350}, 
//' \code{340}, \code{327.5}, \code{310}, \code{287.5} (note that the larger 
//' angles are actually the negative symmetric versions of the smaller angles).
//' @param stay_stopped Logical denoting whether agents will predict others that 
//' are currently not moving to remain immobile in the next iteration. Defaults 
//' to \code{TRUE}.
//' @param time_step Numeric denoting the time between each iteration. Defaults 
//' to \code{0.5} (the same as in \code{\link[predped]{simulate,predped-method}}).
//' 
//' @examples
//' # This is my example
//'
//' @rdname unpack_trace_rcpp
//' 
//' @export
// [[Rcpp::export]]
DataFrame unpack_trace_rcpp(List trace, 
                            NumericMatrix velocities,
                            NumericMatrix orientations,
                            bool stay_stopped = true,
                            double time_step = 0.5) {

    // Find out how many rows the dataframe should have
    int N = 0;
    for(int i = 0; i < trace.length(); i++) {
        S4 state = trace[i];
        List agents = state.slot("agents");
        N += agents.length();
    }

    // Initialize all of the needed variables
    IntegerVector iteration(N);
    NumericVector time(N);

    CharacterVector id(N);
    NumericVector x(N);
    NumericVector y(N);
    NumericVector speed(N);
    NumericVector orientation(N);
    IntegerVector cell(N);
    NumericVector group(N);
    CharacterVector status(N);
    NumericVector radius(N);

    CharacterVector goal_id(N);
    NumericVector goal_x(N);
    NumericVector goal_y(N);

    IntegerVector agent_idx(N);
    List check(N);
    NumericVector ps_speed(N);
    NumericVector ps_distance(N);
    List gd_angle(N);
    List id_distance(N);
    List id_check(N);
    List id_ingroup(N);
    List ba_angle(N);
    List ba_cones(N);
    List fl_leaders(N);
    List wb_buddies(N);
    List gc_distance(N);
    NumericVector gc_radius(N);
    IntegerVector gc_nped(N);
    List vf_angles(N);

    // Create some NA vectors that correspond to the R alternative. Used whenever
    // the utility variables are not defined at a given iteration for a given 
    // agent.
    LogicalVector NA_logical(1);
    NA_logical[0] = NA_LOGICAL;

    // Loop over the different instances in the trace.
    List copy_trace = clone(trace);
    int idx = 0;
    for(int i = 0; i < trace.length(); i++) {
        // Extract the state and its agents from the trace
        S4 state = copy_trace[i];
        List agents = state.slot("agents");

        if(agents.length() == 0) {
            continue;
        }
        S4 setting = state.slot("setting");

        // Create the agent specifications list as used in the lower level 
        // utility functions
        List specifications = create_agent_specifications_rcpp(
            agents,
            stay_stopped, 
            time_step
        );

        // Loop over each of the agents separately
        for(int j = 0; j < agents.length(); j++) {
            // Extract state variables
            int iteration_j = state.slot("iteration");
            double time_j = iteration_j * time_step;

            iteration[idx] = iteration_j;
            time[idx] = time_j;

            // Extract some agent characteristics
            S4 agent = agents[j];

            std::string id_j = agent.slot("id");
            NumericVector position_j = agent.slot("center");
            double x_j = position_j[0];
            double y_j = position_j[1];
            double speed_j = agent.slot("speed");
            double orientation_j = agent.slot("orientation");
            int cell_j = agent.slot("cell");
            double group_j = agent.slot("group");
            std::string status_j = agent.slot("status");
            double radius_j = agent.slot("radius");

            id[idx] = id_j;
            x[idx] = x_j;
            y[idx] = y_j;
            speed[idx] = speed_j;
            orientation[idx] = orientation_j;
            cell[idx] = cell_j;
            group[idx] = group_j;
            status[idx] = status_j;
            radius[idx] = radius_j;

            // Extract agent goal characteristics
            S4 goal = agent.slot("current_goal");

            std::string goal_id_j = goal.slot("id");
            NumericVector goal_position = goal.slot("position");
            double goal_x_j = goal_position[0];
            double goal_y_j = goal_position[1];

            goal_id[idx] = goal_id_j;
            goal_x[idx] = goal_x_j;
            goal_y[idx] = goal_y_j;

            // Access the utility variables slot in the agent class. 
            DataFrame uv_j = as<DataFrame>(agent.slot("utility_variables"));

            // Do a small check of whether the current utility variables are 
            // filled with information. If not, then we cannot preallocate.
            // For this, I use one of the variables that should always be 
            // defined.
            NumericVector tmp = uv_j["ps_speed"];
            LogicalVector tmp_check = Rcpp::is_na(tmp);

            // Assign different values to the resulting DataFrame depending on 
            // the status of the agent. This approach taken to allow for 
            // the correct preallocation in different variables
            if(!any(tmp_check).is_true()) {
                // Save each of the individual columns within their respective
                // vectors or lists to be used later.
                int agent_idx_j = uv_j["agent_idx"];
                List check_list = uv_j["check"];
                LogicalMatrix check_j = check_list[0];
                double ps_speed_j = uv_j["ps_speed"];
                List ps_distance_j = uv_j["ps_distance"];
                List gd_angle_j = uv_j["gd_angle"];
                List id_distance_j = uv_j["id_distance"];
                List id_check_j = uv_j["id_check"];
                List id_ingroup_j = uv_j["id_ingroup"];
                List ba_angle_j = uv_j["ba_angle"];
                List ba_cones_j = uv_j["ba_cones"];
                List fl_leaders_j = uv_j["fl_leaders"];
                List wb_buddies_j = uv_j["wb_buddies"];
                List gc_distance_j = uv_j["gc_distance"];
                double gc_radius_j = uv_j["gc_radius"];
                int gc_nped_j = uv_j["gc_nped"];
                List vf_angles_j = uv_j["vf_angles"];

                agent_idx[idx] = agent_idx_j;
                check[idx] = check_j;
                ps_speed[idx] = ps_speed_j;
                ps_distance[idx] = ps_distance_j[0];
                gd_angle[idx] = gd_angle_j[0];
                id_distance[idx] = id_distance_j[0];
                id_check[idx] = id_check_j[0];
                id_ingroup[idx] = id_ingroup_j[0];
                ba_angle[idx] = ba_angle_j[0];
                ba_cones[idx] = ba_cones_j[0];
                fl_leaders[idx] = fl_leaders_j[0];
                wb_buddies[idx] = wb_buddies_j[0];
                gc_distance[idx] = gc_distance_j[0];
                gc_radius[idx] = gc_radius_j;
                gc_nped[idx] = gc_nped_j;
                vf_angles[idx] = vf_angles_j[0];

            // If the agent is not moving, then you cannot compute the utility
            // variables. We therefore fill the variables with NAs.
            } else {
                agent_idx[idx] = j + 1;
                check[idx] = NA_logical;
                ps_speed[idx] = NA_REAL;
                ps_distance[idx] = NA_REAL;
                gd_angle[idx] = NA_logical;
                id_distance[idx] = NA_logical;
                id_check[idx] = NA_logical;
                id_ingroup[idx] = NA_logical;
                ba_angle[idx] = NA_logical;
                ba_cones[idx] = NA_logical;
                fl_leaders[idx] = NA_logical;
                wb_buddies[idx] = NA_logical;
                gc_distance[idx] = NA_logical;
                gc_radius[idx] = NA_REAL;
                gc_nped[idx] = NA_INTEGER;
                vf_angles[idx] = NA_logical;
            }

            // Update the index
            idx++;
        }
    }

    // Create a List with all of the variables of interest and transform to a 
    // data.frame
    List data = List::create(
        Named("iteration") = iteration,
        Named("time") = time,
        Named("id") = id,
        Named("x") = x,
        Named("y") = y,
        Named("speed") = speed,
        Named("orientation") = orientation,
        Named("cell") = cell,
        Named("group") = group,
        Named("status") = status,
        Named("goal_id") = goal_id,
        Named("goal_x") = goal_x,
        Named("goal_y") = goal_y,
        Named("radius") = radius,
        Named("agent_idx") = agent_idx,
        Named("check") = check,
        Named("ps_speed") = ps_speed,
        Named("ps_distance") = ps_distance,
        Named("gd_angle") = gd_angle,
        Named("id_distance") = id_distance,
        Named("id_check") = id_check,
        Named("id_ingroup") = id_ingroup,
        Named("ba_angle") = ba_angle,
        Named("ba_cones") = ba_cones,
        Named("fl_leaders") = fl_leaders,
        Named("wb_buddies") = wb_buddies,
        Named("gc_distance") = gc_distance,
        Named("gc_radius") = gc_radius,
        Named("gc_nped") = gc_nped,
        Named("vf_angles") = vf_angles
    );

    data.attr("class") = "data.frame";
    data.attr("row.names") = seq(1, idx);
    return data;
}
