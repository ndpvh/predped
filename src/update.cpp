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
void create_agent_specifications_rcpp(List agent_list,
                                      bool stay_stopped = true, 
                                      double time_step = 0.5) {
    
    // Predict where the agents will be at their current velocity and angle. Is 
    // used by other agents to change their own directions in order to avoid 
    // collisions.
    //
    // In order for this to work with m4ma, we need to transform it to a matrix 
    // and provide it rownames that are equal to the id's of the agents
    NumericMatrix agent_predictions(agent_list.length(), 2);
    for(int i = 0; i < agent_list.length(); i++) {
        agent_predictions(i, _) = predict_movement_rcpp(
            agent_list[i],
            stay_stopped,
            time_step
        );
    }




    // # Make the object-based arguments of predped compatible with the information
    // # needed by m4ma. 
    // agent_specs <- list(id = as.character(sapply(agent_list, id)),
    //                     size = as.numeric(sapply(agent_list, size)),
    //                     position = t(sapply(agent_list, position)),
    //                     orientation = as.numeric(sapply(agent_list, orientation)), 
    //                     speed = as.numeric(sapply(agent_list, speed)), 
    //                     group = as.numeric(sapply(agent_list, group)),
    //                     predictions = agent_predictions)

    // # Required for utility helper functions: Add names of the agents to their
    // # characteristics
    // rownames(agent_specs$position) <- agent_specs$id
    // names(agent_specs$size) <- agent_specs$id
    // names(agent_specs$orientation) <- agent_specs$id
    // names(agent_specs$speed) <- agent_specs$id
    // names(agent_specs$group) <- agent_specs$id
    // rownames(agent_specs$predictions) <- agent_specs$id

    // return(agent_specs)
}