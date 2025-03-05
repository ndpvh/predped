#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <algorithm>
#include "m4ma.h"

using namespace Rcpp;

//' Predict agents' movement
//' 
//' Rcpp alternative of \code{\link[predped]{predict_movement}}.
//'
//' Uses the agents' current speed and orientation to determine where the agent 
//' might end up in the next step, assuming that they do not change direction or 
//' speed. This information is used by other agents to determine where (not) to 
//' go to avoid collisions.
//' 
//' @param agent Object of the \code{\link[predped]{agent-class}}.
//' @param stay_stopped Logical denoting whether agents will predict others that 
//' are currently not moving to remain immobile in the next iteration. Defaults 
//' to \code{TRUE}.
//' @param time_step Numeric denoting the number of seconds each discrete step in
//' time should mimic. Defaults to \code{0.5}, or half a second.
//' 
//' @return Numeric matrix containing the predicted positions all agents if 
//' they all maintain their speed and direction.
//' 
//' @seealso 
//' \code{\link[predped]{create_agent_specifications}},
//' \code{\link[predped]{simulate,predped-method}},
//' \code{\link[predped]{simulate,state-method}},
//' \code{\link[predped]{update,agent-method}},
//' \code{\link[predped]{update,state-method}}
//' 
//' @rdname predict_movement_rcpp
//' 
//' @export
// [[Rcpp::export]]
NumericVector predict_movement_rcpp(S4 agent, 
                                    bool stay_stopped = true,
                                    double time_step = 0.5) { 

    NumericVector position = agent.slot("center");
    NumericVector coordinate = clone(position);
    
    // Compute the coordinate where the agents will end up when moving at the 
    // same speed in the same direction. Different when an agent is currently 
    // stopped vs when they are actively moving. Also different depending on 
    // whether this is considered in the first place.
    std::string status = agent.slot("status");
    if(!(stay_stopped & status == "stop")) {
        NumericVector speed = agent.slot("speed");
        NumericVector orientation = agent.slot("orientation");

        double velocity = as<double>(scaleVel(speed, time_step));
        NumericVector distance = aTOd(orientation);

        for(int i = 0; i < distance.length(); i++) {
            double crossed_distance = velocity * distance[i];
            coordinate[i] += crossed_distance;
        }
    }

    return coordinate;
}

//' Create agent specifications
//' 
//' Rcpp alternative to the \code{\link[predped]{create_agent_specifications}} 
//' function.
//'
//' This list translates the information available in the \code{agents} slot of
//' the current status of the \code{\link[predped]{state-class}} to a list 
//' with all this information in numeric vectors or matrices instead of inside 
//' objects. Allows for a translation from the object-oriented way of doing things
//' in \code{predped} to the vectorized way of doing things in \code{m4ma}.
//'
//' @param agent Object of the \code{\link[predped]{agent-class}}.
//' @param stay_stopped Logical denoting whether agents will predict others that 
//' are currently not moving to remain immobile in the next iteration. Defaults 
//' to \code{TRUE}.
//' @param time_step Numeric denoting the number of seconds each discrete step in
//' time should mimic. Defaults to \code{0.5}, or half a second.
//' 
//' @return List containing all information of all agents within the current 
//' state.
//' 
//' @seealso 
//' \code{\link[predped]{create_agent_specifications}},
//' \code{\link[predped]{simulate,predped-method}},
//' \code{\link[predped]{simulate,state-method}},
//' \code{\link[predped]{update,agent-method}},
//' \code{\link[predped]{update,state-method}}
//' 
//' @rdname create_agent_specifications_rcpp
//' 
//' @export
// [[Rcpp::export]]
List create_agent_specifications_rcpp(List agent_list,
                                      bool stay_stopped = true, 
                                      double time_step = 0.5) {
    
    // Initialize all of the variables that we want to fill with the information
    // of each agent
    CharacterVector id(agent_list.length());
    NumericVector size(agent_list.length());
    NumericMatrix position(agent_list.length(), 2);
    NumericVector orientation(agent_list.length());
    NumericVector speed(agent_list.length());
    NumericVector group(agent_list.length());
    NumericMatrix predictions(agent_list.length(), 2);
    
    // Loop over all agents and do the required computations.
    for(int i = 0; i < agent_list.length(); i++) {
        S4 agent_i = agent_list[i];

        // Needed to ensure that C++ knows what types these attributes have 
        // before assigning them to the overall variables
        std::string id_agent = agent_i.slot("id");
        double size_agent = agent_i.slot("radius");
        NumericVector pos_agent = agent_i.slot("center");
        double orient_agent = agent_i.slot("orientation");
        double speed_agent = agent_i.slot("speed");
        double group_agent = agent_i.slot("group");

        // Assign them to the overall variables
        id[i] = id_agent;
        size[i] = size_agent;
        position(i, _) = pos_agent;
        orientation[i] = orient_agent;
        speed[i] = speed_agent;
        group[i] = group_agent;
        predictions(i, _) = predict_movement_rcpp(
            agent_list[i],
            stay_stopped,
            time_step
        );
    }

    // Change the names of the vectors and matrices so that you have the 
    // agent ids in there. Required by m4ma
    CharacterVector xy = {"x", "y"};

    size.attr("names") = id;
    position.attr("dimnames") = List::create(id, xy);
    orientation.attr("names") = id;
    speed.attr("names") = id;
    group.attr("names") = id;
    predictions.attr("dimnames") = List::create(id, xy);

    // Combine them all in a list
    List specifications = List::create(
        Named("id") = id,
        Named("size") = size,
        Named("position") = position,
        Named("orientation") = orientation,
        Named("speed") = speed,
        Named("group") = group,
        Named("predictions") = predictions
    );

    return specifications;
}