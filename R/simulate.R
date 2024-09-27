# Create the generic for simulate, but keep the documentation for simulate 
# for predped and state separate: There is some overlap in arguments, but not 
# as much as one would like for the documentation to be the same
setGeneric("simulate", function(object,...) standardGeneric("simulate"))

#' Simulate the M4MA
#' 
#' This function allows users to simulate data from their specified 
#' \code{\link[predped]{predped-class}}.
#' 
#' @details 
#' Heavily depends on \code{\link[predped]{simulate-state}} and
#' \code{\link[predped]{update}}.
#' 
#' The arguments that can be used to influence the simulation behavior might 
#' be overwhelming, which is why we include a small categorization of the 
#' arguments in this sections. Roughly speaking, this function has multiple 
#' arguments that influence a same aspect of the simulation. These are the 
#' following (note that here all arguments are provided; some of these may 
#' only appear in the documentation of \code{\link[predped]{simulate-state}}).
#' 
#' Arguments that directly influence the general characteristics of the 
#' simulation itself.
#' \itemize{
#'     \item{\code{max_agent}:}{How many agents that can be in the room.}
#'     \item{\code{iterations}:}{How long the simulation should last.}
#'     \item{\code{add_agent_after}:}{How many iterations to leave between 
#'                                    each agent entering the room.}
#'     \item{\code{time_step}:}{How much time passes after each iteration.}
#' }
#' 
#' Arguments that influence general characteristics of the agents.
#' \itemize{
#'     \item{\code{individual_differences}:}{Whether agents should have 
#'                                           continuous individual differences.}
#'     \item{\code{standing_start}:}{How fast agents are after standing still.}
#' }
#' 
#' Arguments controlling whether an initial condition should be used.
#' \itemize{
#'     \item{\code{initial_agents}:}{An initial list of agents.}
#'     \item{\code{initial_number_agents}:}{An initial number of agents with 
#'                                          which you want to start the 
#'                                          simulation.}
#'     \item{\code{initial_condition}:}{An initial state.}
#' }
#' Preferably, only one is used at a time. 
#' 
#' Arguments controlling the goals, which can be easily traced back to the 
#' \code{\link[predped]{goal_stack}} function.
#' \itemize{
#'     \item{\code{goal_number}:}{The number of goals the agents should have.}
#'     \item{\code{goal_duration}:}{How long each goal should take.}
#'     \item{\code{precompute_goal_paths}:}{Whether the paths to the goals should
#'                                          be precomputed.}
#'     \item{\code{sort_goals}:}{Whether to sort the goals based on distance. 
#'                               Corresponds to the \code{sort} argument in 
#'                               \code{\link[predped]{goal_stack}}.}
#'     \item{\code{precomputed_goals}:}{List of goal stacks that already exist 
#'                                      and which should be assigned to the 
#'                                      agents.}
#' }
#' 
#' Arguments controlling how path points and edges are handled.
#' \itemize{
#'     \item{\code{precomputed_edges}:}{List containing nodes and edges.}
#'     \item{\code{many_nodes}:}{Whether to have many or few path points.}
#'     \item{\code{space_between}:}{Space to leave between objects and nodes.}
#'     \item{\code{close_enough}:}{How close a pedestrian should be before they
#'                                 can (a) interact with a goal or (b) start 
#'                                 planning to go to another path point.}
#' }
#' 
#' Arguments that handle live plotting of the simulated states.
#' \itemize{
#'     \item{\code{plot_live}:}{Whether to make these plots.}
#'     \item{\code{plot_time}:}{How much time to leave between the plotting of 
#'                              each state.}
#' }
#' 
#' @param object Object of the \code{\link[predped]{predped-class}}.
#' @param iterations Numeric denoting the number of iterations to run the 
#' simulation for. Defaults to \code{1800}, which corresponds to 15 minutes of 
#' simulation.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to \code{0.5}, or half a second.
#' @param max_agents Numeric, vector, or function that defines the maximal number
#' of agents at each iteration in the simulation. If a vector, the maximal number
#' of agents will be different at each iteration, allowing users to specify 
#' peak and off-peak scenarios. It's exact value is handled by 
#' \code{\link[predped]{determine_values}}. Defaults to \code{20}.
#' @param group_size Numeric matrix with two columns where the first column 
#' denotes the number of people in a social group and the second column the 
#' probability with which such a group is added to the simulation. Defaults to 
#' a 100\% probability that individuals are added to the simulation (i.e., no 
#' social groups).
#' @param add_agent_after Numeric, vector, or function that defines the maximal number
#' of agents at each iteration in the simulation. It's exact value is handled by 
#' \code{\link[predped]{determine_values}}. Defaults to 
#' \code{\(n) rnorm(n, 60, 15)} or someone walking in every 30 seconds on 
#' average.
#' @param standing_start Numeric denoting the factor of their preferred speed 
#' that agents move when they just came from standing still. Defaults to 
#' \code{0.1}.
#' @param individual_differences Logical denoting whether to use the standard 
#' deviations in the parameter list to create some variation in the parameters.
#' Defaults to \code{TRUE}.
#' @param goal_number Numeric, vector, or function that defines the number of 
#' goals the agents should accomplish. It's exact value is handled by 
#' \code{\link[predped]{determine_values}}. Defaults to \code{\(n) rnorm(n, 10, 2)}. 
#' @param goal_duration Numeric, vector, or function that defines the duration of 
#' the goals of the agents. Defaults to \code{\(n) rnorm(n, 10, 2)}.
#' @param precompute_goal_paths Logical denoting whether to run the
#' \code{\link[predped]{find_path-method}} for each of the generated goals 
#' beforehand. Assumes that the agent does all of the goals in the order of the 
#' goal stack. Defaults to \code{FALSE}. 
#' @param sort_goals Logical denoting whether to order the goal stack in a logical 
#' way. Currently implemented in the following way. First, we select the first 
#' goal as being the one that is closest by the starting position provided in 
#' the argument \code{starting_position}. Then, we define each of the next goals
#' as being the one that is closest to the position of the previous goal.
#' Defaults to \code{TRUE}.
#' @param precomputed_goals List of goal stacks from which the agent can be 
#' assigned one. Defaults to \code{NULL}, triggering the creation of goal stacks
#' in the simulation. 
#' @param initial_agents List of objects of the \code{\link[predped]{agent-class}}
#' with which to start the simulation. Defaults to \code{NULL}, meaning the 
#' simulation starts with an empty room.
#' @param initial_condition Object of the \code{\link[predped]{state-class}} 
#' containing the initial state at which to start the simulation. Defaults to 
#' \code{NULL}, meaning the simulation starts with an empty room. Ignored when
#' \code{initial_agents} or \code{initial_number_agents} is provided.
#' @param initial_number_agents Numeric denoting the number of agents that 
#' the simulation should start out with. Defaults to \code{NULL}, meaning the
#' simulation should start with no agents in the room. Ignored if 
#' \code{initial_agents} is provided.
#' @param precompute_edges Logical denoting whether to precompute the path points
#' on which agents can move. Defaults to \code{TRUE}, triggering the creation 
#' of edges through the \code{\link[predped]{compute_edges}} function.
#' @param many_nodes Logical denoting whether to use the minimal amount of path
#' points necessary for the edges (\code{FALSE}) or to use many more \code{TRUE}.
#' Defaults to \code{TRUE} if \code{precompute_edges = TRUE}, and otherwise 
#' defaults to \code{FALSE}. See \code{\link[predped]{create_edges}} for full
#' disclosure on the effect of this logical.
#' @param space_between Numeric denoting the multiplier for the space to leave 
#' between the circumference of the object and the nodes created under the hood 
#' (see \code{\link[predped]{add_nodes}}). Is multiplied by the agent's radius 
#' to determine the actual space to leave between object and node. Defaults to 
#' \code{2.5}.
#' @param plot_live Logical denoting whether to plot each iteration while the 
#' simulation is going on. Defaults to \code{FALSE}.
#' @param plot_time Numeric denoting the amount of time (in seconds) to wait 
#' between iterations, i.e., the time between updating the plot. Defaults to 
#' \code{0.2}.
#' @param ... Arguments passed on to the \code{\link[predped]{simulate-state}} 
#' function.
#' 
#' @return List of objects of the \code{\link{state-class}} containing the 
#' result of the simulation.
#' 
#' @examples 
#' # Create a setting in which to simulate. Note that this setting also serves 
#' # as an example of one-directional flow, which can be seen if you let the 
#' # simulation run a bit longer.
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(rectangle(center = c(0, 0), 
#'                                                      size = c(1, 1))), 
#'                             limited_access = list(segment(from = c(-1, 0.5), 
#'                                                           to = c(-0.5, 0.5)), 
#'                                                   segment(from = c(0.5, 1), 
#'                                                           to = c(0.5, 0.5)), 
#'                                                   segment(from = c(1, -0.5), 
#'                                                           to = c(0.5, -0.5)), 
#'                                                   segment(from = c(-0.5, -1), 
#'                                                           to = c(-0.5, -0.5))))
#' 
#' # Create a model from which to simulate
#' my_model <- predped(setting = my_background, 
#'                     archetypes = c("BaselineEuropean"))
#' 
#' # Do the simulation for maximally 2 agents
#' trace <- simulate(my_model, 
#'                   max_agents = 2,
#'                   iterations = 10, 
#'                   add_agent_after = 5)
#' 
#' # Check some interesting statistics, such as the length of the trace and the 
#' # number of agents in the room.
#' length(trace)
#' sapply(trace, \(x) length(x@agents))
#' 
#' # If you wish to plot the trace, you can simply use the plot function.
#' plt <- plot(trace)
#' plt[[1]]
#' 
#' @seealso 
#' \code{\link[predped]{simulate-state}},
#' \code{\link[predped]{update}}
#' 
#' @rdname simulate-predped
#' 
#' @export
#
# TO DO
#   - Clean up this function
#   - Allow for more flexibility for a user, being able to really tune the 
#     simulation function to their use-case (which currently is difficult, as 
#     you always have to create a new simulation function)
#   - At this moment, setting is kept separate from rest in trace. However, at 
#     some point, agents should be able to move things in the environment, meaning
#     we should keep a trace of moveable objects as well (either list in 
#     `setting` or a separate list as `moveable_objects`)
#   - Make it possible to have a mix of ordered agents (ordering goal_stacks 
#     beforehand) and chaotic agents (not ordering goal_stacks beforehand)
#   - At this moment, still assumed that agents are circular. Try to remove this
#     assumption in functions and in its definition
setMethod("simulate", "predped", function(object,
                                          max_agents = 20,
                                          iterations = 1800,
                                          add_agent_after = \(x) rnorm(x, 60, 15),
                                          standing_start = 0.1,
                                          initial_agents = NULL,
                                          initial_condition = NULL,
                                          initial_number_agents = NULL,
                                          goal_number = \(x) rnorm(x, 10, 2), 
                                          goal_duration = \(x) rnorm(x, 10, 2),
                                          precompute_goal_paths = FALSE,
                                          sort_goals = TRUE,
                                          precomputed_goals = NULL,
                                          space_between = 2.5,
                                          time_step = 0.5,
                                          precompute_edges = TRUE,
                                          many_nodes = precompute_edges,
                                          individual_differences = TRUE,
                                          group_size = matrix(1, nrow = 1, ncol = 2),
                                          ...) {

    # Simulate the iterations after which agents should be added to the simulation
    # (`add_agent`) and the number of goals each agent should pursue (`goal_number`).
    # For `add_agent`, we should furthermore create a cumulative sum, as we just 
    # need to iteration numbers themselves, not the gaps that are created by 
    # `number`
    add_agent_index <- determine_values(add_agent_after, iterations)
    add_agent_index <- c(1, cumsum(add_agent_index))
    add_agent_index <- add_agent_index[add_agent_index <= iterations]

    goal_number <- determine_values(goal_number, iterations)

    max_agents <- determine_values(max_agents, iterations)

    # If `goal_duration` is not a function, make it a function anyway (assumed
    # by the `goal` class: To be changed)
    if(typeof(goal_duration) != "closure") {
        number <- goal_duration[1]
        goal_duration <- function(x) number
    }

    # If the edges need to be precomputed, do so already and delete the mock 
    # position of agent and goal: These are the only dynamical components to 
    # this recomputation
    if(precompute_edges) {
        cat("\nPrecomputing edges")
        edges <- compute_edges(object@setting, 
                               space_between = space_between * max(params_from_csv[["params_bounds"]]["radius",]), 
                               many_nodes = many_nodes)
    } else {
        edges <- NULL
    }

    # If you want a number of agents to be there at the start, and you don't 
    # have an initial condition yet, generate several agents that stand on 
    # random positions in the environment.
    if(is.null(initial_agents) & !is.null(initial_number_agents)) {
        initial_agents <- create_initial_condition(initial_number_agents,
                                                   object,
                                                   goal_number[1:initial_number_agents],
                                                   goal_duration = goal_duration,
                                                   standing_start = standing_start,
                                                   space_between = space_between,
                                                   time_step = time_step,
                                                   precomputed_edges = edges,
                                                   precompute_goal_paths = precompute_goal_paths,
                                                   sort_goals = sort_goals,
                                                   precomputed_goals = precomputed_goals,
                                                   individual_differences = individual_differences,
                                                   group_size = group_size)

        # First index deleted here so that agents don't immediately get added 
        # to the environment when the initial condition is to be generated 
        # (ensures only `initial_number_agents` are in the first state).
        add_agent_index <- add_agent_index[-1]
    }

    # Initialize the trace and state lists. The state will already contain the 
    # initial condition. The trace list also contains this state. 
    state <- predped::state(iteration = 0, 
                            setting = object@setting, 
                            agents = list())
    if(!is.null(initial_agents)) {
        agents(state) <- initial_agents
        
    } else if(!is.null(initial_condition)) {
        if(!identical(initial_condition$setting, state@setting)) {
            stop(paste0("Setting in the `predped` model is not the same as the ",
                        "setting in the initial condition. ", 
                        "Please make sure the initial condition is compatible ",
                        "with your model."))
        }

        agents(state) <- initial_condition$agents
    }
    trace <- list(state)    
    
    # Loop over each iteration of the model
    for(i in seq_len(iterations)) {
        trace[[i + 1]] <- simulate(trace[[i]],
                                   object,
                                   add_agent = (i %in% add_agent_index) & (length(agents(trace[[i]])) < max_agents[i]),
                                   group_size = group_size,
                                   goal_number = goal_number[i],
                                   time_step = time_step,
                                   space_between = space_between,
                                   standing_start = standing_start,
                                   precomputed_edges = edges,
                                   many_nodes = many_nodes,
                                   precompute_goal_paths = precompute_goal_paths,
                                   ...)
    }

    cat("\n")
    
    return(trace)
})





#' Simulate a singel state for the M4MA
#' 
#' This function allows users to simulate a single state from the M4MA based on 
#' a single previous state.
#' 
#' @details 
#' Heavily depends on \code{\link[predped]{update}}.
#' 
#' @param object Object of the \code{\link[predped]{state-class}}.
#' @param model Object of the \code{\link[predped]{predped-class}}.
#' @param add_agent Logical denoting whether an agent should be added to the 
#' simulation. Defaults to \code{FALSE} and is typically handled by the 
#' \code{\link[predped]{simulate-predped}} function, where it accounts for the 
#' variables \code{add_agent_after} and \code{max_agents}.
#' @param group_size Numeric matrix with two columns where the first column 
#' denotes the number of people in a social group and the second column the 
#' probability with which such a group is added to the simulation. Defaults to 
#' a 100\% probability that individuals are added to the simulation (i.e., no 
#' social groups).
#' @param goal_number Numeric, vector, or function that defines the number of 
#' goals the agents should accomplish. It's exact value is handled by 
#' \code{\link[predped]{determine_values}}. Defaults to \code{\(n) rnorm(n, 10, 2)}. 
#' @param goal_duration Numeric, vector, or function that defines the duration of 
#' the goals of the agents. Defaults to \code{\(n) rnorm(n, 10, 2)}.
#' @param precompute_goal_paths Logical denoting whether to run the
#' \code{\link[predped]{find_path-method}} for each of the generated goals 
#' beforehand. Assumes that the agent does all of the goals in the order of the 
#' goal stack. Defaults to \code{FALSE}. 
#' @param sort_goals Logical denoting whether to order the goal stack in a logical 
#' way. Currently implemented in the following way. First, we select the first 
#' goal as being the one that is closest by the starting position provided in 
#' the argument \code{starting_position}. Then, we define each of the next goals
#' as being the one that is closest to the position of the previous goal.
#' Defaults to \code{TRUE}.
#' 
#' @param max_agents Numeric, vector, or function that defines the maximal number
#' of agents at each iteration in the simulation. It's exact value is handled by 
#' \code{\link[predped]{determine_values}}. Defaults to \code{20}.
#' @param iterations Numeric denoting the number of iterations to run the 
#' simulation for. Defaults to \code{1800}, which corresponds to 15 minutes of 
#' simulation.
#' @param add_agent_after Numeric, vector, or function that defines the maximal number
#' of agents at each iteration in the simulation. It's exact value is handled by 
#' \code{\link[predped]{determine_values}}. Defaults to 
#' \code{\(n) rnorm(n, 60, 15)} or someone walking in every 30 seconds on 
#' average.
#' @param standing_start Numeric denoting the factor of their preferred speed 
#' that agents move when they just came from standing still. Defaults to 
#' \code{0.1}.
#' @param initial_agents List of objects of the \code{\link[predped]{agent-class}}
#' with which to start the simulation. Defaults to \code{NULL}, meaning the 
#' simulation starts with an empty room.
#' @param initial_condition Object of the \code{\link[predped]{state-class}} 
#' containing the initial state at which to start the simulation. Defaults to 
#' \code{NULL}, meaning the simulation starts with an empty room. Ignored when
#' \code{initial_agents} or \code{initial_number_agents} is provided.
#' @param initial_number_agents Numeric denoting the number of agents that 
#' the simulation should start out with. Defaults to \code{NULL}, meaning the
#' simulation should start with no agents in the room. Ignored if 
#' \code{initial_agents} is provided.
#' 
#' @param precomputed_goals List of goal stacks from which the agent can be 
#' assigned one. Defaults to \code{NULL}, triggering the creation of goal stacks
#' in the simulation. 
#' @param close_enough Numeric denoting how close (in radii) the agent needs to 
#' be to an object in order to interact with it. Defaults to `2`, meaning the 
#' agent can interact with objects at `2 * radius(agent)` distance away.
#' @param space_between Numeric denoting the space that should be left between 
#' an object and the created path points for the agents (in radii). Defaults to 
#' `1`, meaning a space of `1 * radius(agent)` is left between an object and the
#' path points agents use in their strategy.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to `0.5`, or half a second.
#' @param precompute_edges Logical denoting whether to precompute the path points
#' on which agents can move. Defaults to `TRUE`.
#' @param individual_differences Logical denoting whether variety on the parameters
#' should be accounted for (even within archetypes). Defaults to `TRUE`.
#' @param plot_live Logical denoting whether to plot each iteration while the 
#' simulation is going on. Defaults to `FALSE`.
#' @param plot_time Numeric denoting the amount of time (in seconds) to wait 
#' between iterations, i.e., the time between updating the plot. Defaults to 
#' `0.2`.
#' @param ... Arguments passed on to the \code{\link[predped]{update}} function.
#' 
#' @export
setMethod("simulate", "state", function(object, 
                                        model,
                                        add_agent = FALSE,
                                        group_size = matrix(c(1, 1), nrow = 1),
                                        goal_number = 5,
                                        stay_stopped = TRUE, 
                                        time_step = 0.5,
                                        close_enough = 2,
                                        space_between = 2.5,
                                        standing_start = 0.1,
                                        precomputed_edges = NULL,
                                        many_nodes = FALSE,
                                        precompute_goal_paths = FALSE,
                                        report = FALSE,
                                        interactive_report = FALSE,
                                        velocities = c(1.5, 1, 0.5) |>
                                           rep(each = 11) |>
                                           matrix(ncol = 3),
                                        orientations = c(72.5, 50, 32.5, 20, 10, 0, 
                                                         350, 340, 327.5, 310, 287.5) |>
                                            rep(times = 3) |>
                                            matrix(ncol = 3),
                                        plot_live = FALSE,
                                        plot_time = 0.2,
                                        ...) {

    # Retrieve and update the iteration number in the state
    i <- iteration(object) + 1
    iteration(object) <- i

    # Check whether to add a pedestrian and, if so, initiate a new 
    # agent. Things to consider are: whether it is time to add a new 
    # pedestrian, whether we already reached the maximal number of agents,
    # and whether there is any space to add the new pedestrian. If there is 
    # already an agent waiting, don't create a new one.
    # if((i %in% add_agent_index) & (length(state@agents) < max_agents[i] & !agent_in_queue)) {
    agents_in_queue <- length(potential_agents(object)) != 0
    if(add_agent & !agents_in_queue) {
        # Sample how many agents you would like to generate (if group, 
        # generate them all together)
        if(nrow(group_size) == 1) {
            number_agents <- group_size[,1]
        } else {
            number_agents <- sample(group_size[,1], 1, replace = TRUE, prob = group_size[,2])
        }

        # Actually add this group and communicate that these agents are waiting
        # to enter the space.
        potential_agents(object) <- add_group(model, 
                                              number_agents = number_agents,
                                              group_number = i,
                                              goal_number = goal_number,
                                              time_step = time_step,
                                              ...)
        agents_in_queue <- TRUE
    }

    # Check whether agents are waiting to enter the space. If so, then we 
    # first check whether there is any space to add the pedestrians. Otherwise
    # they will have to keep waiting in the queue
    if(agents_in_queue) {
        agent_to_add <- potential_agents(object)[[1]]

        pts <- points(agent_to_add)
        agents_in_the_way <- sapply(agents(object), 
                                    \(x) in_object(x, pts))

        if(!any(agents_in_the_way)) {
            agents(object) <- append(agents(object), agent_to_add)
            potential_agents(object) <- potential_agents(object)[-1]
        }
    }

    # Print iteration number and number of agents in the space
    cat(paste0("\rIteration: ", i, "; Number of agents: ", length(agents(object))))

    # Update the current state
    object <- update(object, 
                     stay_stopped = stay_stopped, 
                     time_step = time_step,
                     close_enough = close_enough,
                     space_between = space_between,
                     standing_start = standing_start,
                     precomputed_edges = precomputed_edges,
                     many_nodes = many_nodes,
                     precompute_goal_paths = precompute_goal_paths,
                     report = report,
                     interactive_report = interactive_report,
                     velocities = velocities,
                     orientations = orientations)

    # Check whether one of the pedestrians is waiting at the exit and delete them
    # from the agent-list
    if(length(agents(object)) != 0) {
        exiting <- sapply(agents(object), 
                          \(x) status(x) == "exit")
        idx <- seq_along(exiting)[exiting]
    
        if(length(idx) != 0) {
            agents(object) <- agents(object)[-idx]
        }
    }

    # If you want to plot the result immediately, do so
    if(plot_live) {
        print(plot(object, print_progress = FALSE))
        Sys.sleep(plot_time)
    }

    return(object)
})





#' Add a Group of Agents to the Simulation
#' 
#' @param object The `predped` model that you want to simulate
#' @param number_agents The number of agents to add. Defaults to `1`.
#' @param standing_start Numeric denoting the speed of agents when they start 
#' moving. Defaults to `0.1`.
#' @param individual_differences Logical denoting whether variety on the parameters
#' should be accounted for (even within archetypes). Defaults to `TRUE`.
#' @param ... Additional arguments passed on to `add_agent`
#' 
#' @export 
#
# TO DO
#   - Allow for optional "position" and "orientation" arguments. Will make it 
#     easier to create initial conditions, as goals are immediately computed 
#     then
add_group <- function(model,
                      goal_number = 5,
                      number_agents = 1,
                      standing_start = 0.1,
                      individual_differences = TRUE,
                      ...) {

    agents <- list()

    # Generate a single agent who will serve as the basis for all other 
    # agents (imposed so that all agents in a group share the same goals and 
    # enter the space at the same location).
    agents[[1]] <- add_agent(model,
                             goal_number,
                             standing_start = standing_start,
                             individual_differences = individual_differences,
                             ...)

    # If only one agent needed, we will return it immediately
    if(number_agents == 1) {
        return(agents)
    }

    # Otherwise, we should loop over all other agents and change their 
    # parameter values (and color) so that each agent is unique. 
    #
    # First, draw the parameters for each of the new agents
    model_parameters <- parameters(model)
    idx <- sample(model@archetypes, 
                  number_agents - 1, 
                  prob = model@weights)
    
    # Loop over these agents
    for(i in 2:number_agents) {
        # Create a temporary agent as a copy of the first simulated agent
        tmp_agent <- agents[[1]]

        # Change this temporary agent's characterstics based on simulated
        # parameters
        params <- draw_parameters(1, 
                                  mean = dplyr::filter(model_parameters[["params_archetypes"]], name == idx[i]),
                                  Sigma = model_parameters[["params_sigma"]][[idx[i]]],
                                  bounds = model_parameters[["params_bounds"]],
                                  archetype = idx[i],
                                  individual_differences = individual_differences) 
        radius(tmp_agent) <- params$radius 
        color(tmp_agent) <- params$color
        speed(tmp_agent) <- standing_start * params[["preferred_speed"]]

        # Add the agent to the list
        agents[[i]] <- tmp_agent
    }
}
                        
#' Add an Agent to the Simulation
#' 
#' @param model The `predped` model that you want to simulate
#' @param goal_number Integer, vector of integers, or function that determines 
#' how many goals each of the agents should receive. Defaults to a function that 
#' draws `x` numbers from a normal distribution with mean 10 and standard 
#' deviation 2. 
#' @param goal_duration Integer or function that determines the duration of each 
#' goal. Defaults to a function that draws `x` numbers from a normal distribution 
#' with mean 10 (5 sec) and standard deviation 2 (1 sec).
#' @param position Vector denoting the position of the agent you wish to create.
#' Defaults to `NULL`, meaning the agent's location will be at the entrance of 
#' the room.
#' @param standing_start Numeric denoting the speed of agents when they start 
#' moving. Defaults to `0.1`.
#' @param space_between Numeric denoting the space that should be left between 
#' an object and the created path points for the agents (in radii). Defaults to 
#' `1`, meaning a space of `1 * radius(agent)` is left between an object and the
#' path points agents use in their strategy.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to `0.5`, or half a second.
#' @param precomputed_edges List of nodes and edges that can be used by the 
#' path-finding algorithms under the hood. Defaults to `NULL`, meaning edges 
#' have not been precomputed.
#' @param precompute_goal_paths Logical denoting whether to precompute all path
#' points beforehand. This means that the agent does not only preplan the way 
#' towards their next goal, but also to all goals that follow. Defaults to 
#' `FALSE`.
#' @param sort_goals Logical denoting whether to order the goal stack based
#' on the distance of the agent to the goal. Defaults to `TRUE`
#' @param precomputed_goals List of goal stacks from which the agent can be 
#' assigned one. Defaults to `NULL`, meaning that goal stacks should be created
#' in the simulation.
#' @param individual_differences Logical denoting whether variety on the parameters
#' should be accounted for (even within archetypes). Defaults to `TRUE`.
#' @param group_number The number of the group to which the agent belongs. 
#' Defaults to `1`.
#' 
#' @export 
#
# TO DO
#   - Allow for optional "position" and "orientation" arguments. Will make it 
#     easier to create initial conditions, as goals are immediately computed 
#     then
add_agent <- function(model,
                      goal_number,
                      goal_duration = \(x) rnorm(x, 10, 2),
                      position = NULL,
                      group_number = 1,
                      standing_start = 0.1,
                      space_between = 2.5,
                      time_step = 0.5,
                      precomputed_edges = NULL,
                      precompute_goal_paths = TRUE,
                      sort_goals = TRUE,
                      precomputed_goals = NULL,
                      individual_differences = TRUE) {

    # Extract the background from the `predped` model and determine where the 
    # agent will enter the space
    background <- model@setting

    idx <- sample(seq_len(nrow(background@entrance)), 1)
    entrance_agent <- entrance(background)[idx, ]

    # Sample a random set of parameters from the `predped` class. From this, 
    # extract the needed information and add some individual differences
    idx <- sample(model@archetypes, 1, prob = model@weights)

    params <- parameters(model)
    color <- dplyr::filter(params[["params_archetypes"]], name == idx)$color

    params <- draw_parameters(1, 
                              mean = dplyr::filter(params[["params_archetypes"]], name == idx),
                              Sigma = params[["params_sigma"]][[idx]], 
                              bounds = params[["params_bounds"]], 
                              archetype = idx,
                              individual_differences = individual_differences) 
    radius <- params$radius

    # Create this agents' goal stack
    if(is.null(precomputed_goals)) {
        goal_stack <- goal_stack(goal_number, 
                                 background, 
                                 counter_generator = goal_duration,
                                 precomputed_edges = precomputed_edges,
                                 agent_position = position,
                                 precompute_goal_paths = precompute_goal_paths,
                                 space_between = space_between * radius,
                                 sort = sort_goals)
    } else {
        i <- sample(1:length(precomputed_goals), 1)
        goal_stack <- precomputed_goals[[i]]
    }

    # Determine the agent's orientation. Either perpendicular to the wall in 
    # the agent enters, or directed towards the current goal of the agent.
    if(is.null(position)) {
        angle <- perpendicular_orientation(shape(background),
                                           entrance_agent)
    } else {
        co_1 <- position
        co_2 <- goal_stack[[1]]@position

        angle <- atan2(co_2[2] - co_1[2], co_2[1] - co_1[1]) * 180 / pi
    }    

    # Determine the position of the agent. Either this is at the entrance, or 
    # this is at the specified location
    if(is.null(position)) {
        position <- entrance_agent + 1.025 * max(params_from_csv[["params_bounds"]]["radius",]) * c(cos(angle * pi / 180), sin(angle * pi / 180))
    }
    
    # Create the agent itself
    tmp_agent <- agent(center = position,
                       radius = radius,
                       speed = standing_start * params[["preferred_speed"]],
                       orientation = angle,
                       parameters = params,
                       goals = goal_stack[-1],
                       current_goal = goal_stack[[1]],
                       color = color,
                       group = group_number)

    # Create the path to walk on for the current goal and return the agent. Given 
    # that nothing new is put in the environment, we can put `reevaluate` to 
    # FALSE
    current_goal(tmp_agent)@path <- find_path(current_goal(tmp_agent), 
                                              tmp_agent, 
                                              background,
                                              precomputed_edges = precomputed_edges,
                                              reevaluate = FALSE)
    # current_goal(tmp_agent)@path <- matrix(current_goal(tmp_agent)@position, 
    #                                        nrow = 1, 
    #                                        ncol = 2)
    
    return(tmp_agent)
}

#' Create an Initial Condition
#' 
#' Create a list that contains agents at random locations within the setting.
#' 
#' @param initial_number_agents Integer denoting the number of agents that 
#' the simulation should start out with.
#' @param object The `predped` model that you want to simulate
#' @param goal_number Integer, vector of integers, or function that determines 
#' how many goals each of the agents should receive. 
#' @param goal_duration Integer or function that determines the duration of each 
#' goal. Defaults to a function that draws `x` numbers from a normal distribution 
#' with mean 10 (5 sec) and standard deviation 2 (1 sec).
#' @param standing_start Numeric denoting the speed of agents when they start 
#' moving. Defaults to `0.1`.
#' @param space_between Numeric denoting the space that should be left between 
#' an object and the created path points for the agents (in radii). Defaults to 
#' `1`, meaning a space of `1 * radius(agent)` is left between an object and the
#' path points agents use in their strategy.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to `0.5`, or half a second.
#' @param precomputed_edges List of nodes and edges that can be used by the 
#' path-finding algorithms under the hood. Defaults to `NULL`, meaning edges 
#' have not been precomputed.
#' @param precompute_goal_paths Logical denoting whether to precompute all path
#' points beforehand. This means that the agent does not only preplan the way 
#' towards their next goal, but also to all goals that follow. Defaults to 
#' `FALSE`.
#' @param sort_goals Logical denoting whether to order the goal stack based
#' on the distance of the agent to the goal. Defaults to `TRUE`
#' @param precomputed_goals List of goal stacks from which the agent can be 
#' assigned one. Defaults to `NULL`, meaning that goal stacks should be created
#' in the simulation.
#' @param individual_differences Logical denoting whether variety on the parameters
#' should be accounted for (even within archetypes). Defaults to `TRUE`.
#' @param group_size Matrix of size n x 2 containing the group sizes you would 
#' like to simulate (first column) and the probability of observing groups with 
#' this size (second column). Defaults to a 100% probability of observing single 
#' agents.
#' 
#' @export 
create_initial_condition <- function(initial_number_agents,
                                     object,
                                     goal_number,
                                     goal_duration = \(x) rnorm(x, 10, 2),
                                     standing_start = 0.1,
                                     space_between = 1,
                                     time_step = 0.5,
                                     precomputed_edges = NULL,
                                     precompute_goal_paths = TRUE,
                                     sort_goals = TRUE,
                                     precomputed_goals = NULL,
                                     individual_differences = TRUE,
                                     group_size = matrix(1, nrow = 1, ncol = 2)) {

    # Copy the setting
    setting <- object@setting

    # Make sure you have enough goal-numbers for each of the agents
    goal_number <- determine_values(goal_number, initial_number_agents)

    # If `goal_duration` is not a function, make it a function anyway (assumed
    # by the `goal` class: To be changed)
    if(typeof(goal_duration) != "closure") {
        number <- goal_duration[1]
        goal_duration <- function(x) number
    }

    # Check whether the weights in `group_size` sum to 1. If not, correct this
    if(sum(group_size[,2]) != 1) {
        group_size[,2] <- group_size[,2] / sum(group_size[,2])
    }

    if(!is.numeric(sum(group_size[,2])) | !is.finite(sum(group_size[,2]))) {
        stop("Weights of the `group_size` are not numeric. Please adjust.")
    }

    # Use `group_size` to create the group memberships of all agents that are 
    # going to be created. Here, we first simulate the group sizes that we 
    # would like. Then, we create the indices at which group assignment should 
    # change (i.e., if you start with a group of two, then the group number 
    # should change after agent 2; see below)
    if(nrow(group_size) == 1) {
        groups <- rep(group_size[,1], initial_number_agents)
    } else {
        groups <- sample(group_size[,1], 
                         initial_number_agents, 
                         replace = TRUE,
                         prob = group_size[,2])
    }
    group_indices <- cumsum(groups)

    # Loop over the agents and use `add_agent` to create an initial agent. Note
    # that we have to change some of the characteristics of these agents, 
    # namely their location and their orientation, as `add_agent` assumes that
    # agents start at the entrance walking into the setting.
    group_number <- 1
    agents <- list() ; stop <- FALSE
    for(i in seq_len(initial_number_agents)) {
        # Initial agent to create
        new_agent <- add_agent(object, 
                               goal_number[i], 
                               goal_duration = goal_duration,
                               standing_start = standing_start,
                               space_between = space_between,
                               time_step = time_step,
                               precomputed_edges = precomputed_edges,
                               precompute_goal_paths = precompute_goal_paths,
                               sort_goals = sort_goals,
                               precomputed_goals = precomputed_goals,
                               individual_differences = individual_differences)
        group(new_agent) <- group_number 

        # Update the group number for the future if `i` is in the indices that 
        # indicate a change in group membership
        if(i %in% group_indices) {
            group_number <- group_number + 1
        }

        # Extract the edges from the background. Will help in determining the locations
        # at which the agents can be gathered. Importantly, dense network created so 
        # that there are many potential positions for the agents, even when there 
        # are not many objects in the environment
        edges <- create_edges(c(0, 0), 
                              c(0, 0), 
                              setting,
                              space_between = space_between * size(new_agent),
                              many_nodes = TRUE)

        edges$edges <- edges$edges[!(edges$edges$from %in% c("agent", "goal")),]
        edges$edges <- edges$edges[!(edges$edges$to %in% c("agent", "goal")),]
        edges$nodes <- edges$nodes[!(edges$nodes$node_ID %in% c("agent", "goal")),]

        # Additional check to see if there are enough edges to place agents on
        if(nrow(edges$edges) == 0) {
            message(paste0("Couldn't add any new agents after ", 
                           length(agents), 
                           " due to crowdiness."))
            break
        }

        # Choose a random edge on which the agent will stand and create the 
        # exact position.
        success <- FALSE ; iter <- 0
        position <- NULL
        while(!success) {
            # Check whether you overflow the number of iterations. If so, then 
            # we stop in our tracks, break out of the loop, and give a message 
            # on this
            if(iter > 10) {
                message(paste0("Couldn't add new agent after 10 attempts. ", 
                               "Instead of creating an initial condition with ", 
                               initial_number_agents, 
                               " agents, only ", 
                               length(agents), 
                               " agents will be used in the initial condition."))
                stop <- TRUE
                break
            }

            # Sample a random edge on which the agent will stand
            idx <- sample(1:nrow(edges$edges), 1)

            # Get the coordinates of the two points that make up this edge
            co_1 <- edges$nodes[edges$nodes$node_ID == edges$edges$from[idx], c("X", "Y")]
            co_2 <- edges$nodes[edges$nodes$node_ID == edges$edges$to[idx], c("X", "Y")]

            # Generate several alternative positions along this edge on which the 
            # agent can stand and bind them into a matrix
            n_agents_fit <- sqrt((co_1$X - co_2$X)^2 + (co_1$Y - co_2$Y)^2) / size(new_agent)

            if(any(is.na(n_agents_fit))) {
                browser()
            }
            
            alternatives <- cbind(seq(co_1$X, co_2$X, length.out = floor(n_agents_fit)),
                                  seq(co_1$Y, co_2$Y, length.out = floor(n_agents_fit)))

            # Check which position are accessible for the agent
            dummy <- agent(center = c(0, 0), radius = size(new_agent))

            check <- rep(TRUE, each = nrow(alternatives))
            check <- overlap_with_objects(dummy, 
                                          setting,
                                          alternatives, 
                                          check)

            if(any(check)) {
                idx <- which(check)
                idx <- sample(idx, 1)
                
                new_position <- alternatives[idx,]

                success <- TRUE
            }

            # Increase the iteration number
            iter <- iter + 1
        }
        position(new_agent) <- new_position

        # Let the agent face the way of its goal
        co_1 <- position(new_agent)
        co_2 <- current_goal(new_agent)@path[1,]

        orientation(new_agent) <- atan2(co_2[2] - co_1[2], co_2[1] - co_1[1]) * 180 / pi

        # If you need to stop, break out of the loop
        if(stop) {
            break
        }

        # Put the agent in the `agents` list and continue
        agents[[i]] <- new_agent
        setting@objects <- append(setting@objects, new_agent)
    }    
    
    return(agents)
}