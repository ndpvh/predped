#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "m4ma.h"
#include <typeinfo>
#include <string>
#include <math.h>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]



////////////////////////////////////////////////////////////////////////////////
// compute_utility_variable

//' Distances to group centroid
//'
//' Rcpp version of \code{\link[predped]{distance_group_centroid}}. 
//' 
//' Compute the distance of a given agent to the group centroid. This group 
//' centroid is computed as a summary statistic of the predicted x- and y-
//' coordinates of all pedestrians belonging to the same group as the agent. The 
//' summary statistic of choice should be one of mean-tendency, but can be 
//' specified by the user through the argument \code{fx}.
//' 
//' Note that this function has been defined to be in line with the \code{m4ma}
//' utility functions.
//'
//' @param p_pred Numeric matrix with shape N x 2 containing predicted positions
//' of all pedestrians that belong to the social group of the agent.
//' @param centers Numerical matrix containing the coordinates at each position
//' the object can be moved to. Should have one row for each cell.
//' @param nped Numeric integer indicating number of people in pedestrian `n`'s social group. 
//' @param fx Function used to find the group centroid. Defaults to \code{mean}
//'
//' @return Numeric vector containing the distance from each cell in the `center`
//' to the group centroid. If not other agents belong to the same group as the 
//' agent, returns \code{NULL}.
//' 
//' @seealso 
//' \code{\link[predped]{gc_utility}},
//' \code{\link[predped]{utility}}
//' 
//' @rdname distance_group_centroid_rcpp
//'
//' @export 
// [[Rcpp::export]]
RObject distance_group_centroid_rcpp(NumericMatrix predictions, 
                                     NumericMatrix centers, 
                                     int number_agents) {
   
    // First need to identify whether a pedestrian belongs to a social group    
    if(number_agents == 0) {
        return R_NilValue;   
    }

    // All of the positions of all in-group pedestrians need to be averaged
    // Which represents the group centroid
    NumericVector centroid = {mean(predictions(_, 0)), mean(predictions(_, 1))};

    // Euclidean distance for pedestrian_i is calculated for each cell
    return dist1(centroid, centers);
}

//' Angle between agent and group members
//' 
//' Finds the angle at which the group members are located compared to the agent.
//' Uses the predicted positions of the group members for this.
//'
//' @param agent_idx Numeric denoting the position of the agent in the prediction 
//' matrix \code{p_pred}.
//' @param agent_group Numeric vector with the group membership of all 
//' pedestrians.
//' @param position Numeric vector denoting the current position of the agent.
//' @param orientation Numeric denoting the current orientation of the agent.
//' @param p_pred Numeric matrix with shape N x 2 containing predicted positions
//' of all pedestrians that belong to the social group of the agent.
//' @param centers Numerical matrix containing the coordinates at each position
//' the object can be moved to. Should have one row for each cell.
//' @param any_member Logical denoting whether to consider the angles of all 
//' group members (\code{TRUE}) -- effectively saying that it doesn't matter 
//' which group member the agent can see, as long as they can see one -- or 
//' whether to only consider the nearest group member (\code{FALSE}). Defaults 
//' to \code{TRUE}.
//'
//' @return Numeric vector containing the relative angle of the group member(s)
//' compared to the orientation of the agent within a given cell in \code{centers}.
//' 
//' @seealso 
//' \code{\link[predped]{utility}}
//' \code{\link[predped]{vf_utility_continuous}}
//' \code{\link[predped]{vf_utility_discrete}}
//' 
//' @rdname get_angles_rcpp
//' 
//' @export 
// [[Rcpp::export]]
RObject get_angles_rcpp(int agent_idx, 
                        NumericVector agent_groups, 
                        NumericVector position, 
                        double orientation,  
                        NumericMatrix predictions, 
                        NumericMatrix centers,
                        bool any_member = true) {
    
    // First need to identify whether a pedestrian belongs to a social group
    int nped = predictions.nrow();
    if(nped == 0) {
        return R_NilValue;
    }

    // `any_member` is false, we need to first identify the closest pedestrian 
    // and change the predictions to him. Other computations are the same. 
    if(!any_member) {
        NumericVector distances = dist1(position, predictions);
        int idx = Rcpp::which_min(distances);
        NumericVector selection = predictions(idx, _);
        NumericMatrix predictions(1, 2, selection.begin());
    } 
    
    // Loop over each of the centers and find out what the relative angle is 
    // to the group member(s) from these different positions
    NumericVector rel_angles(centers.nrow());
    for(int i = 0; i < centers.nrow(); i++) {
        // Get orientation of the cell
        double orientation = atan2(
            centers(i, 1) - position[1], 
            centers(i, 0) - position[0]
        );

        // Loop over the relative angles and compute the angle of the 
        // predicted position to the cell center. We only need to know the 
        // angle for which the cosine is maximal
        double max_cosine = -1;
        double max_angle = 0;
        for(int j = 0; j < predictions.nrow(); j++) {
            double angle = atan2(
                predictions(j, 1) - centers(i, 1),
                predictions(j, 0) - centers(i, 0)
            );

            angle = angle - orientation;

            if(angle < 0) {
                angle += M_PI * 2;
            }

            if(cos(angle) > max_cosine) {
                max_cosine = cos(angle);
                max_angle = angle;
            }
        }

        rel_angles[i] = max_angle;
    }

    return rel_angles;
}

//' Compute utility variables
//' 
//' Rcpp version of the \code{\link[predped]{compute_utility_variables}} function.
//' 
//' @param object Object of the \code{\link[predped]{agent-class}}.
//' @param state Object of the \code{\link[predped]{state-class}}.
//' @param background Object of the \code{\link[predped]{background-class}}.
//' @param agent_specifications List created by the 
//' \code{\link[predped]{create_agent_specifications}} function. Contains all 
//' information of all agents within the current \code{state} and allows for the
//' communication between the \code{predped} simulation functions and the 
//' \code{m4ma} utility functions.
//' @param centers Numerical matrix containing the coordinates at each position
//' the object can be moved to. Should have one row for each cell.
//' @param check Logical matrix of dimensions 11 x 3 denoting whether an agent 
//' can move to a given cell (\code{TRUE}) or not (\code{FALSE}).
//' 
//' @return Data.frame containing all of the needed variables to be able to 
//' compute the values of the utility functions.
//' 
//' @seealso 
//' \code{\link[predped]{simulate,predped-method}},
//' \code{\link[predped]{simulate,state-method}},
//' \code{\link[predped]{update,agent-method}},
//' \code{\link[predped]{update,state-method}},
//' \code{\link[predped]{update_position}},
//' \code{\link[predped]{update}}
//' 
//' @rdname compute_utility_variables_rcpp
//' 
//' @export 
// [[Rcpp::export]]
DataFrame compute_utility_variables_rcpp(S4 agent,
                                         S4 state,
                                         S4 background,
                                         List agent_specifications,
                                         NumericMatrix centers,                    
                                         LogicalMatrix check_original) {
    
    LogicalMatrix check = clone(check_original);

    // Retrieve the index of the agent of interest. Importantly, make sure that 
    // the indices are defined for R, not Rcpp (conversions to Rcpp are handled
    // in its own functions)
    std::string agent_id = agent.slot("id");
    CharacterVector everyone = agent_specifications["id"];

    int agent_idx = 0;
    for(int i = 0; i < everyone.length(); i++) {
        std::string agent_i = Rcpp::as<std::string>(everyone[i]);
        if(agent_id == agent_i) {
            agent_idx = i + 1;
        }
    }

    // Preferred speed utility: Required variables are the current speed and the 
    // goal distance
    S4 goal = agent.slot("current_goal");
    NumericMatrix goal_position = goal.slot("path");
    
    double ps_speed = agent.slot("speed");
    NumericVector ps_distance = dist1(agent.slot("center"), goal_position);

    // Goal direction utility: Required variable is the angle between agent and 
    // the goal
    NumericVector agent_position = agent.slot("center");
    NumericMatrix agent_center(1, 2, agent_position.begin());

    NumericMatrix gd_angle_multiple = destinationAngle(
        agent.slot("orientation"),
        agent_center,
        goal_position
    );
    NumericVector gd_angle_vector = gd_angle_multiple(0, _);
    NumericMatrix gd_angle(1, 11, gd_angle_vector.begin());
    gd_angle = gd_angle / 90;


    // Interpersonal distance utility: Required variable is the distance between 
    // agent and other agents, and whether these agents are part of the ingroup, 
    // and whether the distances are all positive.
    RObject id_distance = predClose(
        agent_idx,
        agent_center,
        agent.slot("orientation"),
        agent_specifications["position"],
        agent_specifications["size"],
        centers,
        agent_specifications["predictions"],
        background.slot("objects")
    );
    
    // Check which cells have only positive distance. Already instantiate the 
    // list that will hold either the NULL or the resulting matrix, allowing 
    // us to differentiate between both cases in an easy way.
    LogicalMatrix id_check = check;
    List list_id_distance = List::create(R_NilValue);

    LogicalVector id_ingroup(0);
    if(!(id_distance == R_NilValue)) {
        // Check whether each column consists of only positive distances. If 
        // not, then we have to set this to FALSE.
        NumericMatrix id_distance_matrix(id_distance);
        list_id_distance = List::create(id_distance_matrix);

        // Adjust the "check" matrix so that you have FALSE in those locations 
        // where at least one negative distance showed up. Unfortunately couldn't
        // use the `all` function because of conversion issues, so a loopo was 
        // used instead.
        for(int i = 0; i < id_check.length(); i++) {
            if(check[i]) {
                id_check[i] = all(id_distance_matrix(_, i) > 0).is_true();
            }
        }

        // Get names of ingroup agents and check whether these agents are part of the 
        // ingroup or not.
        //
        // Importantly, you should only retain those who are also present in
        // id_ingroup. In other words, do not only erase the agent itself, but also
        // all the other agents who do not overlap with the agent's centers.
        IntegerVector agent_groups = agent_specifications["group"];
        int group = agent_groups[agent_idx - 1];

        List dimnames = id_distance_matrix.attr("dimnames");
        CharacterVector blocking_agents(dimnames[0]);
 
        IntegerVector retained_groups(blocking_agents.length());
        int k = 0;
        for(int i = 0; i < everyone.length(); i++) {      
            LogicalVector local_check(blocking_agents.length());
            for(int j = 0; j < blocking_agents.length(); j++) {
                local_check[j] = blocking_agents[j] == everyone[i];
            }

            if(any(local_check).is_true()) {
                retained_groups[k] = agent_groups[i];
                k++;
            }
        }

        id_ingroup = retained_groups == group;
    }

    // Blocked angle utility: Required variable is those angles that might be 
    // blocked in the near future. In other words, we are trying to predict which 
    // directions might lead to collisions in the future
    //
    // The underlying functions depend on the predicted positions of everyone 
    // except the current agent. Therefore delete the current agent from the 
    // prediction matrix.
    NumericMatrix predictions = agent_specifications["predictions"];
    NumericMatrix predictions_minus_agent(everyone.length() - 1, 2);
    int j = 0;
    for(int i = 0; i < predictions.nrow(); i++) {
        if(i != agent_idx - 1) {
            predictions_minus_agent(j, _) = predictions(i, _);
            j++;
        }
    } 

    // Make sure predictions_minus_agent contains the names of all other agents 
    // on its rows. If not done, then a stackoverflow error will happen (names 
    // are called in m4ma::blockedAngle_rcpp)
    CharacterVector everyone_else = everyone;
    everyone_else.erase(agent_idx - 1);
    CharacterVector xy = {"x", "y"};
    predictions_minus_agent.attr("dimnames") = List::create(everyone_else, xy);

    NumericVector everyone_size = agent_specifications["size"];
    everyone_size.erase(agent_idx - 1);

    NumericVector ba_angle = blockedAngle(
        agent_center,
        agent.slot("orientation"),
        agent.slot("speed"),
        predictions_minus_agent,
        everyone_size,
        background.slot("objects")
    );

    // Also save which cones these angles belong to in a separate variable. 
    IntegerVector ba_cones(ba_angle.length());
    if(ba_angle.length() != 0) {
        CharacterVector ba_cones_names = ba_angle.attr("names");

        for(int i = 0; i < ba_cones_names.length(); i++) {
            ba_cones[i] = std::stoi(std::string(ba_cones_names[i]));
        }
    } 

    // Follow the leader utility: Required variable is the potential leaders and 
    // their distances. This is all outputted in a list by getLeaders_rcpp, which
    // is why we just append it to the data.frame directly
    RObject fl_leaders = getLeaders(
        agent_idx,
        agent_specifications["position"],
        agent_specifications["orientation"],
        agent_specifications["speed"],
        goal_position,
        agent_specifications["group"],
        centers,
        background.slot("objects")
    );

    // Walking besides utility: Required variable is the potential buddies that 
    // you can walk besides. A similar reasoning to follow the leader is applied 
    // here.
    RObject wb_buddies = getBuddy(
        agent_idx,
        agent_specifications["position"],
        agent_specifications["speed"],
        agent_specifications["group"],
        agent_specifications["orientation"],
        agent_specifications["predictions"],
        centers,
        background.slot("objects"),
        false
    );

    // Group centroid utility: Required variables are the distance to the predicted
    // group centroid, the number of pedestrians in the group, and the radius of 
    // the agent in question
    IntegerVector agent_groups = agent_specifications["group"];
    int group = agent_groups[agent_idx - 1];

    agent_groups.erase(agent_idx - 1);
    LogicalVector all_ingroup = agent_groups == group;

    NumericMatrix predictions_ingroup(sum(all_ingroup), 2);
    j = 0;
    for(int i = 0; i < predictions_ingroup.nrow(); i++) {
        if(all_ingroup[i]) {
            predictions_ingroup(j, _) = predictions_minus_agent(i, _);
            j++; 
        }
    }

    RObject gc_distance = distance_group_centroid_rcpp(
        predictions_ingroup,
        centers,
        predictions_ingroup.nrow()
    );

    double gc_radius = agent.slot("radius");
    int gc_nped = predictions_ingroup.nrow();

    // Visual field utility: Required variable is the angle of one or more 
    // other pedestrians.
    RObject vf_angles = get_angles_rcpp(
        agent_idx, 
        agent_specifications["group"],
        agent.slot("center"),
        agent.slot("orientation"),
        predictions_ingroup,
        centers,
        true
    );

    // Create the list that will contain all of the needed information. 
    // Afterwards, convert to a DataFrame as expected by the utility
    // functions. This is the preferred way of doing things in Rcpp.
    List uv = List::create(
        Named("agent_idx") = agent_idx,
        Named("check") = List::create(check),
        Named("ps_speed") = ps_speed, 
        Named("ps_distance") = ps_distance[0],
        Named("gd_angle") = List::create(gd_angle),
        Named("id_distance") = list_id_distance,
        Named("id_check") = List::create(id_check),
        Named("id_ingroup") = List::create(id_ingroup),
        Named("ba_angle") = List::create(ba_angle),
        Named("ba_cones") = List::create(ba_cones),
        Named("fl_leaders") = List::create(fl_leaders),
        Named("wb_buddies") = List::create(wb_buddies),
        Named("gc_distance") = List::create(gc_distance),
        Named("gc_radius") = gc_radius,
        Named("gc_nped") = gc_nped,
        Named("vf_angles") = List::create(vf_angles)
    );

    // Change attribute to DataFrame and return
    uv.attr("class") = "data.frame";
    uv.attr("row.names") = 1;
    return uv;
}





////////////////////////////////////////////////////////////////////////////////
// utility

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
    LogicalVector optimal = cell_distances >= optimal_distance;
    double difference = 0;
    
    NumericVector V(cell_distances.length());
    for(int i = 0; i < cell_distances.length(); i++) {
        if(optimal[i]) {
            difference = std::abs(cell_distances[i] - optimal_distance);
            V[i] -= b_group_centroid * std::pow(difference, a_group_centroid);
        }
    }

    // Perform a check: If all utilities are lower than the stop utility, return
    // a vector with only zeros. This makes sure that agents are still able to 
    // walk around even when separated from their friends.
    if(is_true(all(V < stop_utility))) {
        return NumericVector(cell_distances.length());
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

    bool check_1 = false;
    bool check_2 = false;

    NumericVector V(relative_angles.length());
    for(int i = 0; i < relative_angles.length(); i++) {
        check_1 = relative_angles[i] > lower_angle;
        check_2 = relative_angles[i] < upper_angle;

        if(check_1 & check_2) {
            V[i] -= b_visual_field;
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

    double stop_utility = parameters["stop_utility"];
    stop_utility *= -1;

    // Preferred speed utility: Check whether the distance to the goal is not 
    // NULL and, if not, compute the utility of deceleration, acceleration, or 
    // maintenance of speed
    if(!(data["ps_distance"] == R_NilValue)) {
        V += psUtility(
            parameters["a_preferred_speed"], 
            parameters["b_preferred_speed"], 
            parameters["preferred_speed"], 
            parameters["slowing_time"], 
            as<NumericVector>(data["ps_speed"]), 
            as<NumericVector>(data["ps_distance"])
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
            as<NumericVector>(gd_angle[0])
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
    //
    // Note that we divide the utility by the number of pedestrians that we need
    // to account for. This is done to average over pedestrians, rather than 
    // summing.
    NumericVector V_id(rows * cols);
    for(int i = 0; i < V_id.length(); i++) {
        if(!check[i]) {
            V_id[i] -= arma::datum::inf;
        }
    }

    List id_distance = data["id_distance"];
    if(!(id_distance[0] == R_NilValue)) {
        List id_ingroup = data["id_ingroup"];
        List id_check = data["id_check"];

        LogicalVector id_ingroup_vector = id_ingroup[0];
        int N = id_ingroup_vector.length();

        V += idUtility(
            parameters["b_interpersonal"], 
            parameters["d_interpersonal"], 
            parameters["a_interpersonal"], 
            as<LogicalVector>(id_ingroup[0]), 
            as<LogicalMatrix>(id_check[0]),
            as<NumericMatrix>(id_distance[0]), 
            V_id
        ) / N;
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
        NumericVector ba_angle = as<NumericVector>(ba_angle_list[0]);

        List ba_cones_list = data["ba_cones"];
        IntegerVector ba_cones = as<IntegerVector>(ba_cones_list[0]);

        // Adjust the index of the cones so that it corresponds to C++ instead
        // of R. Additionally make sure angles are either 0 or greater than 0.
        for(int i = 0; i < ba_angle.length(); i++) {
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
        List fl_leaders = as<List>(fl_leaders_list[0]);

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
        List wb_buddies = as<List>(wb_buddies_list[0]);

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
        V += gc_utility_rcpp(
            parameters["a_group_centroid"],
            parameters["b_group_centroid"],
            data["gc_radius"],
            as<NumericVector>(gc_distance[0]),
            stop_utility,
            data["gc_nped"]
        );
    }

    // Visual field utility: Check whether people are walking in a group and, if 
    // so, compute the utility
    List vf_angles_list = data["vf_angles"];
    if(!(vf_angles_list[0] == R_NilValue)) {
        V += vf_utility_rcpp(
            parameters["b_visual_field"],
            as<NumericVector>(vf_angles_list[0])
        );
    }





    // Add the stopping utility to the vector and transform them according to the 
    // randomness parameter
    double randomness = parameters["randomness"];
    V.insert(V.begin(), stop_utility);
    V = V / randomness;

    return V;    
}

//' Utility
//'
//' This function is the Rcpp equivalent of \code{\link[predped]{utility}}. 
//' This function uses the operational-level utility functions to compute the 
//' utility of moving to any given potential cell in \code{centers}. Here, we 
//' assume that none of the utility variables (i.e., the variables that serve as 
//' input to the utility functions) is precomputed, so that it will first compute
//' their values. This input is then provided to 
//' \code{\link[predped]{utility,data.frame-method}} for the actual computation 
//' of the utility.
//' 
//' @param object Object of the \code{\link[predped]{agent-class}}.
//' @param state Object of the \code{\link[predped]{state-class}}.
//' @param background Object of the \code{\link[predped]{background-class}}.
//' @param agent_specifications List created by the 
//' \code{\link[predped]{create_agent_specifications}} function. Contains all 
//' information of all agents within the current \code{state} and allows for the
//' communication between the \code{predped} simulation functions and the 
//' \code{m4ma} utility functions.
//' @param centers Numerical matrix containing the coordinates at each position
//' the object can be moved to. Should have one row for each cell.
//' @param check Logical matrix of dimensions 11 x 3 denoting whether an agent 
//' can move to a given cell (\code{TRUE}) or not (\code{FALSE}).
//' @param cpp Logical denoting whether to use the Rcpp version of the function
//' (\code{TRUE}) or the R version (\code{FALSE}). Defaults to \code{TRUE}.
//' 
//' @return Numeric vector denoting the (dis)utility of moving to each of the 
//' cells in \code{centers}.
//' 
//' @seealso 
//' \code{\link[predped]{simulate,predped-method}},
//' \code{\link[predped]{simulate,state-method}},
//' \code{\link[predped]{update,agent-method}},
//' \code{\link[predped]{update,state-method}},
//' \code{\link[predped]{utility,data.frame-method}},
//' \code{\link[predped]{compute_utility_variables}},
//' \code{\link[predped]{update_position}}
//' 
//' @rdname utility_agent_rcpp
//' 
//' @export
// [[Rcpp::export]]
NumericVector utility_agent_rcpp(S4 agent,
                                 S4 state,
                                 S4 background,
                                 List agent_specifications,
                                 NumericMatrix centers,                    
                                 LogicalMatrix check) {

    // Compute the utility variables that are used as input to the utility 
    // functions.
    //
    // Name choice "uv" comes from abbreviating the more informative "utility 
    // variables", which would've otherwise made the code a bit less elegant.
    DataFrame uv = compute_utility_variables_rcpp(
        agent,
        state,
        background,
        agent_specifications,
        centers,                    
        check
    );

    DataFrame params(agent.slot("parameters"));
    
    return utility_rcpp(uv, params);
}



