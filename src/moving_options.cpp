#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <algorithm>
#include "objects.h"
#include "m4ma.h"

using namespace Rcpp;

//' Check agent and object overlap
//' 
//' Rcpp alternative to \code{\link[predped]{overlap_with_objects}}. 
//' 
//' This function checks whether there is an overlap between a given agent and 
//' the objects in the environment, provided that the agent would move to the 
//' locations in \code{centers}. Returns a logical matrix as needed in 
//' \code{\link[predped]{moving_options-method}}.
//' 
//' @details
//' In this function, we can only approximately check the intersection of agent 
//' and object. Specifically, we use the following method. First, we sample 
//' nodes on the circumference of each of the objects in the setting that is 
//' provided to this function. For this, we depend on the function 
//' \code{\link[predped]{nodes_on_circumference}} and we currently take these 
//' nodes to be 5cm. 
//' 
//' In the next step, we bind all these coordinates together in a single matrix. 
//' This matrix thus consists of nodes that should not be embedded in the agents:
//' Whenever one of these points is included in the agents, we can conclude that
//' the agents and objects intersect. [Note, however, that if these points are 
//' not included in the agents, that we cannot with certainty conclude that agent 
//' and object do not intersect]
//' 
//' This check is then performed by looping over all the centers, changing the 
//' agents position to the position of this center, and using the 
//' \code{\link[predped]{in_object-method}} to do the test. This is a vectorized 
//' test: For each position in \code{centers} we have a logical \code{TRUE} or 
//' \code{FALSE} for each of the nodes in the coordinate matrix, resulting in a 
//' logical matrix with an equal number of rows as \code{centers} and an equal 
//' number of columns as nodes in the coordinate matrix. In a last step, 
//' we aggregate over the columns in this matrix so that we have a single logical
//' for each center.
//' 
//' The reason why we use this approximate method is because of time efficiency. 
//' Using the \code{\link[predped]{intersects-method}} takes a longer time than 
//' using the \code{\link[predped]{in_object-method}}, especially as the number 
//' of objects in the environment increases.
//' 
//' @param agent Object of the \code{\link[predped]{agent-class}}.
//' @param background Object of the \code{\link[predped]{background-class}}.
//' @param centers Numerical matrix containing the coordinates at each position
//' the object can be moved to. Should have one row for each cell.
//' @param check Logical matrix of dimensions 11 x 3 denoting whether an agent 
//' can move to a given cell (\code{TRUE}) or not (\code{FALSE}).
//' @param space_between Numeric denoting the space to leave between the nodes 
//' put on the circumference of the objects in the space (used for checking the
//' overlap with an agent). Defaults to \code{0.05} or 5cm.
//' 
//' @return Logical matrix containing availabilities of the centers (\code{TRUE}
//' if available, \code{FALSE} if not).
//' 
//' @examples 
//' # Initialize all objects that you need
//' my_background <- background(shape = rectangle(center = c(0, 0), 
//'                                               size = c(6, 6)), 
//'                             objects = list(circle(center = c(0, 0), 
//'                                                   radius = 2)))
//' my_agent <- agent(center = c(-2.75, 0), 
//'                   radius = 0.25, 
//'                   speed = 1, 
//'                   orientation = 0,
//'                   current_goal = goal(position = c(-2.01, 0)))
//' 
//' # Generate several locations the agent can move to
//' centers <- m4ma::c_vd_r(1:33, 
//'                         position(my_agent), 
//'                         speed(my_agent), 
//'                         orientation(my_agent))
//' check <- matrix(TRUE, nrow = 11, ncol = 3)
//' 
//' # Use moving_options to see which of these possibilities is sound
//' overlap_with_objects(my_agent, 
//'                      my_background,
//'                      centers,
//'                      check,
//'                      cpp = TRUE)
//' 
//' @seealso 
//' \code{\link[predped]{agent-class}},
//' \code{\link[predped]{background-class}},
//' \code{\link[predped]{in_object}},
//' \code{\link[predped]{intersects}},
//' \code{\link[predped]{moving_options}},
//' \code{\link[predped]{nodes_on_circumference}}
//' 
//' @rdname overlap_with_objects_rcpp
//' 
//' @export
// [[Rcpp::export]]
LogicalMatrix overlap_with_objects_rcpp(S4 agent, 
                                        S4 background,
                                        NumericMatrix centers, 
                                        LogicalMatrix check,
                                        double space_between = 5e-2) {

    // Extract objects and shape from the background
    List objects = background.slot("objects");
    S4 shape = background.slot("shape");
    objects.push_back(shape);

    // Square the values of the centers. This is done to make the computation 
    // of of the distance between center and nodes faster. Steered clear from 
    // pow for efficiency purposes.
    int N = centers.nrow();

    // Reinitalize the centers
    std::vector<double> position = agent.slot("center");
    NumericVector dist(N);

    std::vector<double> cx(N);
    std::vector<double> cy(N);
    for(int i = 0; i < N; i++) {
        cx[i] = centers(i, 0);
        cy[i] = centers(i, 1);

        dist[i] = (cx[i] - position[0]) * (cx[i] - position[0]) + (cy[i] - position[1]) * (cy[i] - position[1]);
    }

    // Extract agent characteristics. Will help in determining whether any of 
    // the nodes of an object might intersect with the agent determined through
    // an (arbitrary) cutoff.
    double radius = agent.slot("radius");

    double distance = std::sqrt(max(dist)); 
    double cutoff = distance + radius; // Use of the same cutoff as in R

    // Loop over all of the objects in the environment
    double minimum = 0;

    for(List::iterator i = objects.begin(); i < objects.end(); ++i) {
        // Place nodes on the circumference of the object
        NumericMatrix xy = nodes_on_circumference_rcpp(
            *i, 
            space_between
        );
        NumericVector x = xy(_, 0);
        NumericVector y = xy(_, 1);

        // Compute the distance of the coordinates to the center of the agent. 
        // Retain only those that fall within a given distance
        NumericVector dist = ((x - position[0]) * (x - position[0]) + (y - position[1]) * (y - position[1]));
        LogicalVector idx = sqrt(dist) <= cutoff;

        if(any(idx).is_false()) {
            continue;
        } else {
            x = x[idx];
            y = y[idx];
        }

        for(int j = 0; j < N; j++) {
            // Only adjust the check when it is not yet blocked
            if(check[j]) {
                // Compute the distance from the center to any of the coordinates
                NumericVector dist = (x - cx[j]) * (x - cx[j]) + (y - cy[j]) * (y - cy[j]);
                minimum = min(sqrt(dist));

                // Check whether any of these distances is smaller than the radius of 
                // the agent. If so, then we know that the agent intersects with the 
                // object. Is a bit faster written this way than if it were written 
                // as `check[j] = minimum > radius`
                if(minimum <= radius) {
                    check[j] = false;
                }
            }
        }
    }

    return check;
}


//' Check where an object can be moved to
//'
//' Rcpp alternative to \code{\link[predped]{moving_options}}. This method checks 
//' where an object can be moved to. It returns a logical matrix that codes 
//' \code{TRUE} for the cells that are available and \code{FALSE} for those that 
//' aren't.
//' 
//' @details
//' In general, this method works as follows. First, it checks whether any of the 
//' provided cell centers are freely available, in the sense that they are not 
//' contained inside any objects or fall outside of the setting. This is a crude
//' measure of whether a particular spot is available and is handled by the 
//' \code{\link[m4ma]{free_cells_rcpp}} function of the \code{m4ma} package.
//' 
//' Second, we check whether the object itself can be moved to this space, or 
//' whether it would intersect with any of the objects and/or the outline of the 
//' setting. This is a more direct measure of availability, as it doesn't only 
//' account for whether a specific spot can be reached theoretically, but also 
//' accounts for the size of the object that is being moved there. This is 
//' handled by the \code{\link[predped]{overlap_with_objects}} function in 
//' \code{predped}.
//' 
//' Finally, if the object is an instance of the \code{\link[predped]{agent-class}}, 
//' we also check whether the agent can still see there current goal or path-point
//' when they move to the open spots. They will not move to the spots from which
//' they cannot see their goal/path-point. This is handled by the 
//' \code{\link[m4ma]{seesGoalOK_rcpp}} function in the \code{m4ma} package.
//' 
//' WARNING: Due to its reliance on the \code{m4ma} package, centers needs to be 
//' of length 33 x 2. This corresponds to the 3 (change in speed) x 11 
//' (change in orientation) options that are inherent to M4MA.
//'
//' @param object Object of the \code{\link[predped]{agent-class}} or the 
//' \code{\link[predped]{object-class}} (latter not yet supported).
//' @param state Object of the \code{\link[predped]{state-class}} containing the 
//' current state.
//' @param background Object of the \code{\link[predped]{background-class}}.
//' @param centers Numerical matrix containing the coordinates at each position
//' the object can be moved to. Should have one row for each cell.
//'
//' @return Logical matrix containing availabilities of the centers.
//' 
//' @examples 
//' # Initialize all objects that you need
//' my_background <- background(shape = rectangle(center = c(0, 0), 
//'                                               size = c(6, 6)), 
//'                             objects = list(circle(center = c(0, 0), 
//'                                                   radius = 2)))
//' my_agent <- agent(center = c(-2.75, 0), 
//'                   radius = 0.25, 
//'                   speed = 1, 
//'                   orientation = 0,
//'                   current_goal = goal(position = c(-2.01, 0)))
//' 
//' my_state <- state(iteration = 1,
//'                   setting = my_background, 
//'                   agents = list())
//' 
//' # Generate several locations the agent can move to
//' centers <- m4ma::c_vd_r(1:33, 
//'                         position(my_agent), 
//'                         speed(my_agent), 
//'                         orientation(my_agent))
//' 
//' # Use moving_options to see which of these possibilities is sound
//' moving_options(my_agent, 
//'                my_state, 
//'                my_background,
//'                centers,
//'                cpp = TRUE)
//' 
//' @seealso 
//' \code{\link[predped]{agent-class}},
//' \code{\link[predped]{background-class}},
//' \code{\link[predped]{object-class}},
//' \code{\link[predped]{state-class}},
//' \code{\link[predped]{overlap_with_objects}} 
//'
//' @docType methods
//' 
//' @rdname moving_options_rcpp
//'
//' @export
// [[Rcpp::export]]
LogicalMatrix moving_options_rcpp(S4 agent, 
                                  S4 state, 
                                  S4 background, 
                                  NumericMatrix centers) {

    // These copies are necessary!
    S4 copy_state = clone(state);
    S4 copy_background = clone(background);

    // Add the other agents to the background objects. This will allow us to 
    // immediately test whether cells are occupied by other agents instead of 
    // doing this check only later.
    //
    // Differentiate between a list that does contain the other agents 
    // (slot in background) and a list that does not (obj). This distinction is 
    // used later when checking the seeing of goals 
    NumericVector agent_center = agent.slot("center");
    CharacterVector id_agent = agent.slot("id");

    List agents = copy_state.slot("agents");

    List objects = background.slot("objects");
    List objects_agents = copy_background.slot("objects");
    for(int i = 0; i < agents.length(); i++) {
        S4 agent_i = agents[i];

        // Skip the addition for the current agent
        CharacterVector id_i = agent_i.slot("id");
        LogicalVector id_check = (id_i == id_agent);
        if(all(id_check).is_true()) {
            continue;
        }

        NumericVector center_i = agent_i.slot("center");

        // if(sum((center_i - agent_center) * (center_i - agent_center)) < 5) {
        objects_agents.push_back(agents[i]);
        // }
    }
    copy_background.slot("objects") = objects_agents;

    // Use the `free_cells` function to get all free cells to which the agent
    // might move. Specifically look at whether a cell lies within the background
    // and whether the agent has a direct line of sight to that cell.
    LogicalMatrix check = free_cells(
        agent, 
        copy_background, 
        centers
    );

    // Additional thing to check: Make sure none of these centers lies 
    // within an object. Apparently, this is not automatically checked in 
    // `free_cells_rcpp`!
    NumericMatrix coord(1, 2);
    for(int i = 0; i < check.length(); i++) {
        // If this check is already FALSE, move on to the next one
        bool for_check = check[i];
        if(!for_check) {
            continue;
        }

        // Extract current coordinates
        coord(0, _) = centers(i, _);
        for(List::iterator j = objects.begin(); j < objects.end(); ++j) {
            for_check = as<bool>(in_object_rcpp(*j, coord));

            if(for_check) {
                check[i] = false;
                break;
            }
        }
    }

    // If something blocks the way in the previous column, then it should also 
    // block the way on the columns
    for(int i = 0; i < check.nrow(); i++) {
        if(!check(i, 2)) {
            check(i, 1) = false;
        }

        if(!check(i, 1)) {
            check(i, 0) = false;
        }
    }

    // If there are still cells free, check whether an agent would intersect with
    // an object if it were to move to a given cell. Given that the function
    // `overlap_with_object` only checks those cells that are free, the output
    // of this function can overwrite the local `check` variable without any
    // issues
    if(any(check).is_true()) {
        // Currently, there is an unknown divergence between the R and Rcpp versions
        // of overlap_with_objects. However, we can use the original m4ma function
        // to achieve the same purpose.
        check = overlap_with_objects_rcpp(
            agent, 
            copy_background, 
            centers, 
            check
        );
        // // Make copy of check
        // LogicalMatrix check_vec = clone(check);
        // check_vec.attr("dim") = R_NilValue;

        // check = bodyObjectOK(
        //     agent.slot("radius"), 
        //     centers, 
        //     copy_background.slot("objects"), 
        //     check_vec
        // );

        // If something blocks the way in the previous column, then it should also 
        // block the way on the columns
        for(int i = 0; i < check.nrow(); i++) {
            if(!check(i, 2)) {
                check(i, 1) = false;
            }

            if(!check(i, 1)) {
                check(i, 0) = false;
            }
        }
    }

    // If there are still cells free, check whether the goal can still be seen
    // or whether an agent should re-plan. Additionally, check whether any of 
    // the centers is actually lying inside of any of the objects
    if(any(check).is_true()) {
        // Make copy of check
        LogicalMatrix check_vec = clone(check);
        check_vec.attr("dim") = R_NilValue;

        // Function to rewrite! New arguments are already provided to this one,
        // but not the original one.
        //
        // Unfortunately kept this way for (a) compatability with moving_options 
        // in predped and(b) compatability with m4ma.
        S4 goal = agent.slot("current_goal");
        NumericMatrix path = goal.slot("path");
        path.attr("i") = 1;        
        List goal_list = List::create(path);
        
        List state_dummy = List::create(Named("P") = goal_list);
        LogicalVector local_check = seesGoalOK(
            1,
            objects,
            state_dummy,
            centers,
            check_vec
        );

        // Here, change `check` based on the results of the function. Importantly,
        // agent should still move even if it cannot see their goal (hence the
        // if-statement), otherwise the agent will get stuck
        if(any(local_check).is_true()) {
            LogicalMatrix converted_check(
                11,
                3,
                local_check.begin()
            );
            check = converted_check;
        } 
    }

    return check;
}

