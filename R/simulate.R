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
#' Heavily depends on \code{\link[predped]{simulate,state-method}} and
#' \code{\link[predped]{update}}.
#'
#' The arguments that can be used to influence the simulation behavior might
#' be overwhelming, which is why we include a small categorization of the
#' arguments in this sections. Roughly speaking, this function has multiple
#' arguments that influence a same aspect of the simulation. These are the
#' following (note that here all arguments are provided; some of these may
#' only appear in the documentation of \code{\link[predped]{simulate,state-method}}).
#'
#' Arguments that directly influence the general characteristics of the
#' simulation and the model itself.
#' \itemize{
#'     \item{\code{max_agent}:}{How many agents that can be in the room.}
#'     \item{\code{iterations}:}{How long the simulation should last.}
#'     \item{\code{add_agent_after}:}{How many iterations to leave between
#'                                    each agent entering the room.}
#'     \item{\code{time_step}:}{How much time passes after each iteration.}
#'     \item{\code{velocities}:}{To which extent an agent can change their speed
#'                               from one iteration to the next.}
#'     \item{\code{orientations}:}{To which extent an agent can change their
#'                                 direction from one iteration to the next.}
#'     \item{\code{stay_stopped}:}{Whether agents predict currently immobile
#'                                 pedestrians to remain immobile.}
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
#' Arguments that handle feedback during the simulation.
#' \itemize{
#'     \item{\code{plot_live}:}{Whether to make these plots.}
#'     \item{\code{plot_time}:}{How much time to leave between the plotting of
#'                              each state.}
#'     \item{\code{report}:}{Whether to report whenever agents are reorienting.}
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
#' Defaults to \code{FALSE}.
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
#' @param middle_edge Logical denoting whether to sample the goals from the
#' middle of the edge of the objects in the \code{link[predped]{background-class}}
#' (\code{TRUE}) or to allow the goal locations to fall on all points on these
#' edges (\code{FALSE}). Defaults to \code{FALSE}.
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
#' @param fx Function that takes in and returns an object of the
#' \code{\link[predped]{state-class}}. This will be executed at the beginning of each
#' iteration and allows users some flexibility in their simulations. For example
#' useful when simulating evacuations (giving everyone "goal exit") or trying
#' to guide behavior in any other way. Defaults to "\(x) x", meaning the state
#' remains unaltered.
#' @param plot_live Logical denoting whether to plot each iteration while the
#' simulation is going on. Defaults to \code{FALSE}.
#' @param plot_time Numeric denoting the amount of time (in seconds) to wait
#' between iterations, i.e., the time between updating the plot. Defaults to
#' \code{0.2}.
#' @param ... Arguments passed on to the \code{\link[predped]{simulate,state-method}}
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
#' \code{\link[predped]{simulate,state-method}},
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
                                          middle_edge = FALSE,
                                          space_between = 1.25,
                                          time_step = 0.5,
                                          precompute_edges = TRUE,
                                          many_nodes = precompute_edges,
                                          individual_differences = FALSE,
                                          group_size = matrix(1, nrow = 1, ncol = 2),
                                          fx = \(x) x,                                          
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

    max_agents <- determine_values(max_agents, iterations, positive_integer = FALSE)

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
    if((is.null(initial_agents) & is.null(initial_condition)) & !is.null(initial_number_agents)) {
        initial_agents <- create_initial_condition(initial_number_agents,
                                                   object,
                                                   goal_number[1:initial_number_agents],
                                                   goal_duration = goal_duration,
                                                   middle_edge = middle_edge,
                                                   standing_start = standing_start,
                                                   space_between = space_between,
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

    # Initialize the trace and state lists. Two cases. Either the initial state 
    # already exists, which case we just use this one, or we create an empty 
    # state and check whether there are any agents to add to this state.
    if(!is.null(initial_condition)) {
        if(!identical(initial_condition@setting, object@setting)) {
            stop(paste0("Setting in the `predped` model is not the same as the ",
                        "setting in the initial condition. ",
                        "Please make sure the initial condition is compatible ",
                        "with your model."))
        }

        state <- initial_condition

        # Check whether iteration_variables is defined for this initial condition.
        # If not, then we will have to create them ourselves
        if(!(nrow(iteration_variables(state)) >= iterations)) {
            iteration_variables(state) <- data.frame(max_agents = max_agents[1:iterations],
                                                     goal_number = goal_number[1:iterations],
                                                     add_agent_index = add_agent_index[1:iterations])
        }
    } else {
        state <- predped::state(iteration = 0,
                                setting = object@setting,
                                agents = list(),
                                iteration_variables = data.frame(max_agents = max_agents[1:iterations],
                                                                 goal_number = goal_number[1:iterations],
                                                                 add_agent_index = add_agent_index[1:iterations]), 
                                variables = list())

        if(!is.null(initial_agents)) {
            agents(state) <- initial_agents
        }
    }

    trace <- list(state)

    # Print that the model is being simulated if no feedback is desired
    cat(paste0("\rYour model: ", crayon::bold(id(object)), " is being simulated"), paste0(rep(" ", 10), collapse = ""))

    # Loop over each iteration of the model
    for(i in seq_len(iterations)) {
        altered_state <- fx(trace[[i]])
        trace[[i + 1]] <- simulate(altered_state,
                                   object,
                                   add_agent = (i %in% iteration_variables(altered_state)$add_agent_index) &
                                               (length(agents(trace[[i]])) < iteration_variables(altered_state)$max_agents[i]),
                                   group_size = group_size,
                                   goal_number = iteration_variables(altered_state)$goal_number[i],
                                   time_step = time_step,
                                   space_between = space_between,
                                   standing_start = standing_start,
                                   precomputed_edges = edges,
                                   many_nodes = many_nodes,
                                   precompute_goal_paths = precompute_goal_paths,
                                   middle_edge = middle_edge,
                                   individual_differences = individual_differences,
                                   ...)
    }

    cat("\n")

    return(trace)
})

#' Simulate a single state for the M4MA
#'
#' This function allows users to simulate a single state from the M4MA based on
#' a single previous state.
#'
#' @details
#' Heavily depends on \code{\link[predped]{update}}.
#'
#' Many of the arguments here are either shared or derived from the upper-level
#' \code{\link[predped]{simulate,predped-method}} function. Please refer to the
#' Details in its documentation to see a breakdown of the most important
#' arguments.
#'
#' @param object Object of the \code{\link[predped]{state-class}}.
#' @param model Object of the \code{\link[predped]{predped-class}}.
#' @param add_agent Logical denoting whether an agent should be added to the
#' simulation. Defaults to \code{FALSE} and is typically handled by the
#' \code{\link[predped]{simulate,predped-method}} function, where it accounts for the
#' variables \code{add_agent_after} and \code{max_agents}.
#' @param group_size Numeric matrix with two columns where the first column
#' denotes the number of people in a social group and the second column the
#' probability with which such a group is added to the simulation. Defaults to
#' a 100\% probability that individuals are added to the simulation (i.e., no
#' social groups).
#' @param velocities Numeric matrix containing the change in speed for an agent
#' whenever they move to the respective cell of this matrix. Is used to create
#' the cell positions that the agent might move to, as performed through
#' \code{\link[m4ma]{c_vd_rcpp}}. Currently limited to having 11 rows (direction)
#' and 3 columns (speed). Defaults to a matrix in which the columns contain
#' \code{1.5} (acceleration), \code{1}, and \code{0.5}.
#' @param orientations Numeric matrix containing the change in direction for an
#' agent whenever they move to the respective cell of this matrix. Is used to
#' create the cell positions that the agent might move to, as performed through
#' \code{\link[m4ma]{c_vd_rcpp}}. Currently limited to having 11 rows (direction)
#' and 3 columns (speed). Defaults to a matrix in which the rows contain
#' \code{72.5}, \code{50}, \code{32.5}, \code{20}, \code{10}, code{0}, \code{350},
#' \code{340}, \code{327.5}, \code{310}, \code{287.5} (note that the larger
#' angles are actually the negative symmetric versions of the smaller angles).
#' @param close_enough Numeric denoting how close (in radii) the agent needs to
#' be to an object in order to interact with it. Defaults to \code{2}, meaning the
#' agent can interact with objects at \code{2 * radius(agent)} distance away.
#' @param space_between Numeric denoting the space that should be left between
#' an object and the created path points for the agents (in radii). Defaults to
#' \code{2.5}, meaning a space of \code{2.5 * radius(agent)} is left between an
#' object and the path points agents use in their strategy.
#' @param stay_stopped Logical denoting whether agents will predict others that
#' are currently not moving to remain immobile in the next iteration. Defaults
#' to \code{TRUE}.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to `0.5`, or half a second.
#' @param precomputed_edges Output of \code{\link[predped]{compute_edges}}
#' containing the nodes and edges the agent can use to plan its path. Defauls
#' to \code{NULL}, triggering the creation of these edges whenever they are
#' needed.
#' @param many_nodes Logical denoting whether to use the minimal number of nodes
#' or to use many more (see \code{\link[predped]{create_edges}}). Ignored if
#' \code{precomputed_edges} is provided. Defaults to \code{FALSE}.
#' @param plot_live Logical denoting whether to plot each iteration while the
#' simulation is going on. Defaults to `FALSE`.
#' @param plot_time Numeric denoting the amount of time (in seconds) to wait
#' between iterations, i.e., the time between updating the plot. Defaults to
#' `0.2`.
#' @param report Logical denoting whether to report whenever an agent is
#' reorienting. Defaults to \code{FALSE}, and is usually not needed as feedback.
#' @param print_iteration Logical denoting whether to report each simulated
#' iteration. Defaults to \code{FALSE}, but can be switched off if desired.
#' @param step_report Numeric denoting at which iteration to report the 
#' current iteration in the simulation & the number of agents present 
#' at the current iteration in the simulation. Defaults to 1, which
#' represents each iteration to be reported.
#' @param cpp Logical denoting whether to use the Rcpp alternatives for several
#' of the lower-level functions (\code{TRUE}) or whether to use the R alternatives
#' instead (\code{FALSE}). Defaults to \code{TRUE}.
#' @param ... Arguments passed on to the \code{\link[predped]{plot}} method (if 
#' \code{plot_live = TRUE}).
#'
#' @return Object of the \code{\link[predped]{state-class}}.
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
#' # Create an initial state with no agents in it
#' my_state <- state(iteration = 0,
#'                   setting = my_background,
#'                   agents = list())
#'
#' # Simulate the next state
#' next_state <- simulate(my_state,
#'                        my_model,
#'                        add_agent = TRUE)
#'
#' # Check the number of agents in the next state
#' length(next_state@agents)
#'
#' # If you wish to plot the new state, you can use the plot function.
#' plot(next_state)
#'
#' @seealso
#' \code{\link[predped]{predped-class}},
#' \code{\link[predped]{state-class}},
#' \code{\link[predped]{simulate,predped-method}},
#' \code{\link[predped]{update}}
#'
#' @rdname simulate-state
#'
#' @export
#
# TO DO
#   - Clean up this function
#   - Allow for more flexibility for a user, being able to really tune the
#     simulation function to their use-case (which currently is difficult, as
#     you always have to create a new simulation function)
setMethod("simulate", "state", function(object,
                                        model,
                                        add_agent = FALSE,
                                        group_size = matrix(c(1, 1), nrow = 1),
                                        velocities = c(1.5, 1, 0.5) |>
                                           rep(each = 11) |>
                                           matrix(ncol = 3),
                                        orientations = c(72.5, 50, 32.5, 20, 10, 0,
                                                         350, 340, 327.5, 310, 287.5) |>
                                            rep(times = 3) |>
                                            matrix(ncol = 3),
                                        standing_start = 0.1,
                                        close_enough = 2,
                                        space_between = 1.25,
                                        stay_stopped = TRUE,
                                        time_step = 0.5,
                                        precomputed_edges = NULL,
                                        many_nodes = !is.null(precomputed_edges),
                                        plot_live = FALSE,
                                        plot_time = 0.2,
                                        report = FALSE,
                                        print_iteration = FALSE,
                                        step_report = 1,
                                        goal_number = 5,
                                        goal_duration = \(x) rnorm(x, 10, 2),
                                        precompute_goal_paths = TRUE,
                                        sort_goals = TRUE,
                                        precomputed_goals = NULL,
                                        middle_edge = FALSE,
                                        position = NULL,
                                        individual_differences = FALSE,
                                        cpp = TRUE,
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
            agent_number <- group_size[,1]
        } else {
            agent_number <- sample(group_size[,1], 1, replace = TRUE, prob = group_size[,2])
        }

        # Actually add this group and communicate that these agents are waiting
        # to enter the space.
        potential_agents(object) <- add_group(model,
                                              agent_number = agent_number,
                                              group_number = i,
                                              standing_start = standing_start,
                                              goal_number = goal_number,
                                              goal_duration = goal_duration,
                                              precompute_goal_paths = precompute_goal_paths,
                                              sort_goals = sort_goals,
                                              precomputed_goals = precomputed_goals,
                                              middle_edge = middle_edge,
                                              precomputed_edges = precomputed_edges,
                                              many_nodes = many_nodes,
                                              space_between = space_between,
                                              position = position,
                                              individual_differences = individual_differences)
        agents_in_queue <- TRUE
    }

    # Check whether agents are waiting to enter the space. If so, then we
    # first check whether there is any space to add the pedestrians. Otherwise
    # they will have to keep waiting in the queue
    if(agents_in_queue) {
        agent_to_add <- potential_agents(object)[[1]]

        pts <- rbind(points(agent_to_add), center(agent_to_add))
        agents_in_the_way <- sapply(agents(object),
                                    \(x) in_object(x, pts))

        if(!any(agents_in_the_way)) {
            agents(object) <- append(agents(object), agent_to_add)
            potential_agents(object) <- potential_agents(object)[-1]
        }
    }

    # Print iteration number and number of agents in the space
    if(print_iteration & i %% step_report == 0) {
        cat(paste0("\rIteration: ", i, "; Number of agents: ", length(agents(object)), paste0(rep(" ", 40), collapse = "")))
    }


    # Update the current state
    object <- update(object,
                     stay_stopped = stay_stopped,
                     time_step = time_step,
                     close_enough = close_enough,
                     space_between = space_between,
                     standing_start = standing_start,
                     precomputed_edges = precomputed_edges,
                     many_nodes = many_nodes,
                     report = report,
                     velocities = velocities,
                     orientations = orientations,
                     print_iteration = print_iteration,
                     cpp = cpp)

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
        print(plot(object, ...))
        Sys.sleep(plot_time)
    }

    return(object)
})





#' Add a group of agents to the simulation
#'
#' When multiple agents are added to the simulation as a group, these agents
#' might all be different (either in archetype or in parameter values), but
#' will share the same goals.
#'
#' @param object Object of the \code{\link[predped]{predped-class}}.
#' @param agent_number Numeric denoting the number of agents to add. Defaults
#' to \code{1}.
#' @param standing_start Numeric denoting the factor of their preferred speed
#' that agents move when they just came from standing still. Defaults to
#' \code{0.1}.
#' @param individual_differences Logical denoting whether to use the standard
#' deviations in the parameter list to create some variation in the parameters.
#' Defaults to \code{FALSE}.
#' @param ... Additional arguments passed on to \code{\link[predped]{add_agent}}.
#'
#' @return List of instances of the \code{\link[predped]{agent-class}}.
#'
#' @examples
#' # Create a setting in which to simulate.
#' my_background <- background(shape = rectangle(center = c(0, 0),
#'                                               size = c(2, 2)),
#'                             objects = list(rectangle(center = c(0, 0),
#'                                                      size = c(1, 1))))
#'
#' # Create a model from which to simulate
#' my_model <- predped(setting = my_background,
#'                     archetypes = c("BaselineEuropean",
#'                                    "DrunkAussie"))
#'
#' # Generate a group of 5 pedestrians who have 5 goals to accomplish
#' agents <- add_group(my_model,
#'                     agent_number = 5,
#'                     goal_number = 5)
#'
#' # Get id's, parameters, and number of goals
#' sapply(agents, id)
#' sapply(agents, parameters)
#' sapply(agents, \(x) length(goals(x)))
#'
#' @seealso
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{add_agent}},
#' \code{\link[predped]{simulate,predped-method}},
#' \code{\link[predped]{simulate,state-method}}
#'
#' @rdname add_group
#'
#' @export
#
# TO DO
#   - Allow for optional "position" and "orientation" arguments. Will make it
#     easier to create initial conditions, as goals are immediately computed
#     then
add_group <- function(model,
                      agent_number = 1,
                      standing_start = 0.1,
                      individual_differences = FALSE,
                      ...) {

    agents <- list()

    # Generate a single agent who will serve as the basis for all other
    # agents (imposed so that all agents in a group share the same goals and
    # enter the space at the same location).
    agents[[1]] <- add_agent(model,
                             standing_start = standing_start,
                             individual_differences = individual_differences,
                             ...)

    # If only one agent needed, we will return it immediately
    if(agent_number == 1) {
        return(agents)
    }

    # Otherwise, we should loop over all other agents and change their
    # parameter values (and color) so that each agent is unique.
    #
    # First, draw the parameters for each of the new agents
    model_parameters <- parameters(model)
    idx <- sample(model@archetypes,
                  agent_number - 1,
                  prob = model@weights,
                  replace = TRUE)

    # Loop over these agents
    for(i in 2:agent_number) {
        # Create a temporary agent as a copy of the first simulated agent
        tmp_agent <- agents[[1]]

        # Change this temporary agent's characterstics based on simulated
        # parameters
        tmp <- model_parameters[["params_archetypes"]]
        mean_params <- tmp[tmp$name == idx[i - 1], ]
        params <- generate_parameters(1,
                                      mean = mean_params,
                                      Sigma = model_parameters[["params_sigma"]][[idx[i - 1]]],
                                      bounds = model_parameters[["params_bounds"]],
                                      archetype = idx[i - 1],
                                      individual_differences = individual_differences)

        id(tmp_agent) <- paste(sample(letters, 5, replace = TRUE), collapse = "")
        radius(tmp_agent) <- params$radius
        color(tmp_agent) <- mean_params$color
        speed(tmp_agent) <- standing_start * params[["preferred_speed"]]

        # Add the agent to the list
        agents[[i]] <- tmp_agent
    }

    return(agents)
}

#' Add a single agent to the simulation
#'
#' @param object Object of the \code{\link[predped]{predped-class}}.
#' @param group_number Numeric denoting the group to which the agent belongs.
#' Defaults to \code{1}.
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
#' @param middle_edge Logical denoting whether to sample the goals from the
#' middle of the edge of the objects in the \code{link[predped]{background-class}}
#' (\code{TRUE}) or to allow the goal locations to fall on all points on these
#' edges (\code{FALSE}). Defaults to \code{FALSE}.
#' @param precomputed_edges Output of \code{\link[predped]{compute_edges}}
#' containing the nodes and edges the agent can use to plan its path. Defauls
#' to \code{NULL}, triggering the creation of these edges whenever they are
#' needed.
#' @param space_between Numeric denoting the space that should be left between
#' an object and the created path points for the agents (in radii). Defaults to
#' \code{2.5}, meaning a space of \code{2.5 * radius(agent)} is left between an
#' object and the path points agents use in their strategy. Ignored if
#' \code{precomputed_edges} is provided.
#' @param position Numeric denoting the position you would like to assign to the
#' agent. Defaults to \code{NULL}, making the agent start at the entrance. Note
#' that this is an experimental feature that has not been tested yet, and
#' therefore might not work for the moment.
#' @param standing_start Numeric denoting the factor of their preferred speed
#' that agents move when they just came from standing still. Defaults to
#' \code{0.1}.
#' @param individual_differences Logical denoting whether to use the standard
#' deviations in the parameter list to create some variation in the parameters.
#' Defaults to \code{FALSE}.
#'
#' @return List of instances of the \code{\link[predped]{agent-class}}.
#'
#' @examples
#' # Create a setting in which to simulate.
#' my_background <- background(shape = rectangle(center = c(0, 0),
#'                                               size = c(2, 2)),
#'                             objects = list(rectangle(center = c(0, 0),
#'                                                      size = c(1, 1))))
#'
#' # Create a model from which to simulate
#' my_model <- predped(setting = my_background,
#'                     archetypes = c("BaselineEuropean",
#'                                    "DrunkAussie"))
#'
#' # Generate an agent
#' my_agent <- add_agent(my_model, goal_number = 5)
#' my_agent
#'
#' @seealso
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{add_group}},
#' \code{\link[predped]{simulate,predped-method}},
#' \code{\link[predped]{simulate,state-method}}
#'
#' @rdname add_agent
#'
#' @export
#
# TO DO
#   - Allow for optional "position" and "orientation" arguments. Will make it
#     easier to create initial conditions, as goals are immediately computed
#     then
add_agent <- function(model,
                      group_number = 1,
                      goal_number = 5,
                      goal_duration = \(x) rnorm(x, 10, 2),
                      precompute_goal_paths = TRUE,
                      sort_goals = TRUE,
                      precomputed_goals = NULL,
                      middle_edge = FALSE,
                      precomputed_edges = NULL,
                      many_nodes = !is.null(precomputed_edges),
                      space_between = 1.25,
                      position = NULL,
                      standing_start = 0.1,
                      individual_differences = FALSE) {

    # Extract the background from the `predped` model and determine where the
    # agent will enter the space
    background <- model@setting

    idx <- sample(seq_len(nrow(background@entrance)), 1)
    entrance_agent <- entrance(background)[idx, ]

    # Sample a random set of parameters from the `predped` class. From this,
    # extract the needed information and add some individual differences
    idx <- sample(model@archetypes, 1, prob = model@weights)

    params <- parameters(model)
    tmp <- params[["params_archetypes"]]
    color <- tmp[tmp$name == idx, ]$color

    params <- generate_parameters(1,
                                  mean = tmp[tmp$name == idx, ],
                                  Sigma = params[["params_sigma"]][[idx]],
                                  bounds = params[["params_bounds"]],
                                #   archetype = idx,
                                  individual_differences = individual_differences)
    radius <- params$radius

    # Determine the agent's position. Either this is at the specified location
    # or this is a bit further down from where the agent enters the room at
    # the specified entrance.
    if(is.null(position)) {
        # Offset is the x- and y-offset that needs to be added to the entrance
        # coordinates in order for the agent to not intersect with the shape of
        # the background. The offset is defined by an angle -- which in itself
        # defines the direction in which the offset should happen, as determined
        # to be the perpendicular orientation to the entrance-wall -- and by
        # a distance -- which defines how far away from the entrance-wall the
        # agent should be placed.
        offset_angle <- perpendicular_orientation(shape(background),
                                                  entrance_agent) * pi / 180
        offset_number <- max(params_from_csv[["params_bounds"]]["radius",])

        offset <- 1.025 * offset_number * c(cos(offset_angle), sin(offset_angle))

        position <- entrance_agent + offset

        # Also take the offset_angle to be the direction in which the agent is
        # looking
        angle <- offset_angle * 180 / pi
    } else {
        angle <- NULL
    }

    # Create this agents' goal stack
    if(is.null(precomputed_goals)) {
        # Try to generate a goal stack. If that's not possible, then the agent
        # will be provided with an exit goal.
        if(length(objects(background)) != 0) {
            goal_stack <- goal_stack(goal_number,
                                     background,
                                     counter = goal_duration,
                                     precomputed_edges = precomputed_edges,
                                     many_nodes = many_nodes,
                                     starting_position = position,
                                     precompute_goal_paths = precompute_goal_paths,
                                     space_between = space_between * radius,
                                     sort = sort_goals,
                                     middle_edge = middle_edge)
        } else {
            exits <- exit(background)
            idx <- sample(1:nrow(exits), 1)
                
            goal_stack <- list(goal(id = "goal exit",
                                    position = as.numeric(exits[idx,])))
        }
    } else {
        i <- sample(1:length(precomputed_goals), 1)
        goal_stack <- precomputed_goals[[i]]
    }

    # Determine the agent's orientation. This is only triggered if angles wasn't
    # prevously defined as being perpendicular to the entry.
    if(is.null(angle)) {
        co_1 <- position
        co_2 <- goal_stack[[1]]@position

        angle <- atan2(co_2[2] - co_1[2], co_2[1] - co_1[1]) * 180 / pi
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

    if(nrow(current_goal(tmp_agent)@path) == 0) {
        current_goal(tmp_agent)@path <- matrix(current_goal(tmp_agent)@position,
                                               nrow = 1,
                                               ncol = 2)
    }

    return(tmp_agent)
}

#' Create initial condition
#'
#' Creates a list of agents that can be used as an initial condition to the
#' simulation. Agents are placed at random locations in the room. Main
#' advantage of this function is that you don't have to wait for a room to fill
#' up with the required number of agents, but rather impose that the room is
#' already filled with this number of agents.
#'
#' @details
#' In this function, we use the following approach to simulate an initial
#' condition. First, we create an agent using the \code{\link[predped]{add_agent}}
#' function. This agent is given a specific group id based on the
#' \code{group_size} argument.
#'
#' Once created, we then compute all possible paths that the agent may be
#' walking on through the \code{\link[predped]{compute_edges}} function. One
#' of these paths is then chosen, and subsequently a random point on that path
#' is chosen. We then check whether an agent would be able to take the chosen
#' position without intersecting any of the objects in the environment. If not,
#' then we choose another point until we can place the agent there. If we do not
#' find a fitting position for the agent after 10 iterations, we break out of
#' the loop and stop trying to fit in \code{agent_number} agents in the space.
#'
#' Finally, group members are identified and given the same set of goals.
#'
#' @param agent_number Numeric denoting the number of agents that should be
#' included in the initial condition.
#' @param model Object of the \code{\link[predped]{predped-class}}.
#' @param goal_number Numeric, vector, or function that defines the number of
#' goals the agents should accomplish. It's exact value is handled by
#' \code{\link[predped]{determine_values}}. Defaults to \code{\(n) rnorm(n, 10, 2)}.
#' @param group_size Matrix of size n x 2 containing the group sizes you would
#' like to simulate (first column) and the probability of observing groups with
#' this size (second column). Defaults to a 100% probability of observing single
#' agents.
#' @param space_between Numeric denoting the space that should be left between
#' an object and the created path points for the agents (in radii). Defaults to
#' \code{2.5}, meaning a space of \code{2.5 * radius(agent)} is left between an
#' object and the path points agents use in their strategy. Ignored if
#' \code{precomputed_edges} is provided.
#' @param ... Additional arguments provided to \code{\link[predped]{add_agent}}.
#'
#' @return List of instances of the \code{\link[predped]{agent-class}}.
#'
#' @seealso
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{predped-class}},
#' \code{\link[predped]{add_agent}},
#' \code{\link[predped]{simulate,predped-method}},
#' \code{\link[predped]{simulate_state}}
#'
#' @rdname create_initial_condition
#'
#' @export
#
# TO DO:
#   - Make this work with groups! Currently not the case
create_initial_condition <- function(agent_number,
                                     model,
                                     goal_number = \(n) rnorm(n, 10, 2),
                                     group_size = matrix(1, nrow = 1, ncol = 2),
                                     space_between = 1.25,
                                     ...) {

    # Copy the setting
    setting <- model@setting

    # Make sure you have enough goal-numbers for each of the agents
    goal_number <- determine_values(goal_number, agent_number)

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
        groups <- rep(group_size[,1], agent_number)
    } else {
        groups <- sample(group_size[,1],
                         agent_number,
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
    for(i in seq_len(agent_number)) {
        # Initial agent to create
        new_agent <- add_agent(model,
                               goal_number[i],
                               ...)
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
        edges <- compute_edges(setting,
                               space_between = space_between * size(new_agent),
                               many_nodes = TRUE)

        # Additional check to see if there are enough edges to place agents on
        if(is.null(edges$edges)) {
            stop <- TRUE
        } else if(nrow(edges$edges) == 0) {
            stop <- TRUE
        }

        if(stop) {
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
                               agent_number,
                               " agents, only ",
                               length(agents),
                               " agents will be used in the initial condition."))
                stop <- TRUE
                break
            }

            # Sample a random edge on which the agent will stand
            idx <- sample(1:nrow(edges$edges_with_coords), 1)

            # Get the coordinates of the two points that make up this edge
            coords <- edges$edges_with_coords[idx,]

            # Generate several alternative positions along this edge on which the
            # agent can stand and bind them into a matrix
            n_agents_fit <- sqrt((coords$from_x - coords$to_x)^2 + (coords$from_y - coords$to_y)^2) / size(new_agent)

            if(any(is.na(n_agents_fit))) {
                browser()
            }

            alternatives <- cbind(seq(coords$from_x, coords$to_x, length.out = floor(n_agents_fit)),
                                  seq(coords$from_y, coords$to_y, length.out = floor(n_agents_fit)))

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
        if(nrow(current_goal(new_agent)@path) == 0) {
            co_2 <- current_goal(new_agent)@position
        } else {
            co_2 <- current_goal(new_agent)@path[1,]
        }

        orientation(new_agent) <- atan2(co_2[2] - co_1[2], co_2[1] - co_1[1]) * 180 / pi

        # If you need to stop, break out of the loop
        if(stop) {
            break
        }

        # Put the agent in the `agents` list and continue
        agents[[i]] <- new_agent
        setting@objects <- append(setting@objects, new_agent)
    }

    # Loop over those individuals who belong to the same group and give them
    # the same set of goals
    group_id <- sapply(agents, group)
    for(i in unique(group_id)) {
        idx <- group_id == i

        if(sum(idx) == 1) {
            next
        } else {
            grouped_agents <- agents[idx]
            for(j in 2:length(grouped_agents)) {
                current_goal(grouped_agents[[j]]) <- current_goal(grouped_agents[[1]])
                goals(grouped_agents[[j]]) <- goals(grouped_agents[[1]])
            }

            agents[idx] <- grouped_agents
        }
    }

    return(agents)
}
