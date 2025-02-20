#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <algorithm>
#include "general.h"
#include "update.h"
#include "utility_model.h"

using namespace Rcpp;



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
//' @rdname time_series
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
void unpack_trace_rcpp(List trace, 
                       NumericMatrix velocities,
                       NumericMatrix orientations,
                       bool stay_stopped = true,
                       double time_step = 0.5) {
    
    // # Create a function that will extract all details of the agents from a 
    // # particular state.
    // extract_state <- function(y) {
    //     # Create the agent-specifications for this state
    //     agent_specifications <- create_agent_specifications(y@agents, 
    //                                                         stay_stopped = stay_stopped, 
    //                                                         time_step = time_step)

    //     # Loop over all of the agents and create their own row in the dataframe.
    //     # This will consist of all variables included in the time_series function
    //     # and the utility variables that are used as an input to the utility 
    //     # functions.
    //     y <- lapply(y@agents, 
    //                 function(a) {
    //                     # Simple time-series such as the one defined in the 
    //                     # designated function
    //                     time_series <- data.frame(iteration = y@iteration,
    //                                               time = y@iteration * time_step,
    //                                               id = id(a),
    //                                               x = position(a)[1], 
    //                                               y = position(a)[2], 
    //                                               speed = speed(a), 
    //                                               orientation = orientation(a), 
    //                                               cell = cell(a), 
    //                                               group = group(a), 
    //                                               status = status(a),
    //                                               goal_id = current_goal(a)@id,
    //                                               goal_x = current_goal(a)@position[1], 
    //                                               goal_y = current_goal(a)@position[2],
    //                                               radius = radius(a))

    //                     # If the agent is not moving, then you cannot compute 
    //                     # the utility variables. We should therefore fill it 
    //                     # with values that make sense and otherwise with NULLs
    //                     # (in hopes utility will be okay with this).
    //                     #
    //                     # If the agent is moving, however, we will compute the 
    //                     # utility variables for that move.
    //                     agent_idx <- which(agent_specifications$id == id(a))
    //                     if(status(a) != "move") {
    //                         utility_variables <- data.frame(agent_idx = agent_idx,
    //                                                         check = NA,
    //                                                         ps_speed = NA, 
    //                                                         ps_distance = NA, 
    //                                                         gd_angle = NA, 
    //                                                         id_distance = NA,
    //                                                         id_check = NA,
    //                                                         id_ingroup = NA,
    //                                                         ba_angle = NA,
    //                                                         ba_cones = NA,
    //                                                         fl_leaders = NA,
    //                                                         wb_buddies = NA,
    //                                                         gc_distance = NA,
    //                                                         gc_radius = NA, 
    //                                                         gc_nped = NA,
    //                                                         vf_angles = NA)

    //                     } else {
    //                         # Get the centers for this participant, given their 
    //                         # current position, speed, and orientation
    //                         centers <- m4ma::c_vd_rcpp(cells = 1:33,
    //                                                    p1 = position(a),
    //                                                    v1 = speed(a),
    //                                                    a1 = orientation(a),
    //                                                    vels = velocities,
    //                                                    angles = orientations,
    //                                                    tStep = time_step)

    //                         # Delete the agent from the agent list in the state
    //                         # (otherwise moving options will give wrong results)
    //                         agent_state <- y
    //                         agents(agent_state) <- agents(agent_state)[-agent_idx] 
    
    //                         # Do an initial check of which of these centers can be 
    //                         # reached and which ones can't
    //                         check <- moving_options(a, 
    //                                                 agent_state, 
    //                                                 agent_state@setting, 
    //                                                 centers)
                            
    //                         # Compute the utility variables for this agent under the
    //                         # current state                            
    //                         utility_variables <- compute_utility_variables(a,
    //                                                                        y,
    //                                                                        y@setting,
    //                                                                        agent_specifications,
    //                                                                        centers,                    
    //                                                                        check)
    //                     }
    
    //                     # Bind them all together in one dataframe and return 
    //                     # the result
    //                     return(cbind(time_series, utility_variables))
    //                 })

    //     return(do.call("rbind", y))
    // }

    // # Iterate over each object in the list and extract the state. 
    // x <- lapply(x, extract_state)
    // x <- do.call("rbind", x)
    // rownames(x) <- NULL

    // # Create a continuous time-variable in seconds
    // return(x)
}

