#' An S4 Class to Represent Goals
#' 
#' Defines the \code{goal} class, which contains all characteristics of a goal that 
#' the agents can pursue.
#'
#' @slot id Character that serves as an identifier for the goal.
#' @slot position Numerical vector denoting the position of the goal.
#' @slot path Numerical matrix of size n x 2 denoting the different intermediate
#' path points between an agent and a goal. In effect defines which path the 
#' agent will take to move towards the goal.
#' @slot busy Logical denoting whether the goal is currently being interacted 
#' with.
#' @slot done Logical denoting whether a goal has been completed.
#' @slot counter Numeric denoting the number of time steps the agent should 
#' interact with the goal before the goal has been completed. 
#' 
#' @rdname goal-class
#' 
#' @seealso 
#' \code{\link[predped]{busy}},
#' \code{\link[predped]{counter}},
#' \code{\link[predped]{done}},
#' \code{\link[predped]{id}},
#' \code{\link[predped]{path}},
#' \code{\link[predped]{position}},
#' \code{\link[predped]{initialize,goal-method}}
#'
#' @export
#
# TO DO: Phase out coordinate
goal <- setClass("goal", list(id = "character",
                              position = "coordinate",
                              path = "matrix",
                              busy = "logical",
                              done = "logical",
                              counter = "numeric"))

#' Constructor for the \code{\link[predped]{goal-class}}
#' 
#' @param id Character that serves as an identifier for the goal. Defaults to 
#' an empty character, triggering the random generation of an id.
#' @param position Numerical vector denoting the position of the goal. Defaults 
#' to \code{c(0, 0)}.
#' @param path Numerical matrix of size n x 2 denoting the different intermediate
#' path points between an agent and a goal. In effect defines which path the 
#' agent will take to move towards the goal. Defaults to an empty matrix of size 
#' 2 x 2.
#' @param busy Logical denoting whether the goal is currently being interacted 
#' with. Defaults to \code{FALSE}.
#' @param done Logical denoting whether a goal has been completed. Defaults to 
#' \code{FALSE}.
#' @param counter Numeric denoting the number of time steps the agent should 
#' interact with the goal before the goal has been completed. Defaults to \code{5}.
#' 
#' @return Object of the \code{\link[predped]{goal-class}}
#' 
#' @examples
#' # Initialize agent
#' my_goal <- goal(id = "my goal", position = c(1, 1))
#' 
#' # Access the two slots that were specified
#' my_goal@id
#' my_goal@position
#' 
#' @seealso 
#' \code{\link[predped]{goal-class}},
#' \code{\link[predped]{busy}},
#' \code{\link[predped]{counter}},
#' \code{\link[predped]{done}},
#' \code{\link[predped]{id}},
#' \code{\link[predped]{path}},
#' \code{\link[predped]{position}}
#' 
#' @rdname initialize-goal-method
#' 
#' @export
setMethod("initialize", "goal", function(.Object,
                                         id = character(0),
                                         position = numeric(2),
                                         path = matrix(nrow = 0, ncol = 2),
                                         busy = FALSE,
                                         done = FALSE,
                                         counter = 5) {

    .Object@id <- if(length(id) == 0) paste("goal", paste0(sample(letters, 5, replace = TRUE), collapse = "")) else id
    .Object@position <- coordinate(position)
    .Object@busy <- busy
    .Object@done <- done
    .Object@counter <- counter
    .Object@path <- path

    return(.Object)
})

#' Show method for the \code{\link[predped]{goal-class}}
#' 
#' @export
setMethod("show", "goal", function(object) {
    cat(crayon::bold("Goal Attributes"), "\n")
    cat("busy:", object@busy, "\n")
    cat("counter:", object@counter, "\n")
    cat("done:", object@done, "\n")
    cat("id:", object@id, "\n")
    cat("path:\n")
    print(object@path)
    cat("\nposition:", object@position, "\n")
    cat("\nFor more detailed information, please extract the wanted information from the background directly.\n")
})

#' Add a goal to an object
#' 
#' Uses the characteristics of an object to add a goal to it.
#' 
#' @details
#' Takes in an instance of \code{\link[predped]{object-class}} and uses its 
#' characteristics to generate an instance of \code{\link[predped]{goal-class}}
#' that is attached to it. In practice, uses one of the objects in the 
#' \code{objects} slot of a \code{\link[predped]{background-class}} instance. 
#' 
#' Is defined for all instances of \code{\link[predped]{object-class}} except for 
#' \code{\link[predped]{segment-class}}, which cannot contain goals due to its 
#' primary function.
#'
#' @param object Object or instance of the \code{\link[predped]{object-class}}.
#' @param background Object of \code{\link[predped]{background-class}} containing
#' all objects in the environment that can have a goal attached to them in its 
#' \code{objects} slot.
#' @param id Character that serves as an identifier for the goal. Defaults to 
#' an empty character, triggering the random generation of an id.
#' @param counter Numeric denoting the number of time steps the agent should 
#' interact with the goal before the goal has been completed. Defaults to \code{5}.
#' @param ... Arguments passed on to \code{\link[predped]{rng_point-method}}
#'
#' @return Object of \code{\link[predped]{goal-class}}.
#' 
#' @examples 
#' # Create a background
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)))
#' 
#' # Adjust the objects in the background for each of the different objects to 
#' # showcase how add_goals works for each
#' objects(my_background) <- list(circle(center = c(0, 0), radius = 0.5))
#' add_goal(my_background)
#' 
#' objects(my_background) <- list(rectangle(center = c(0, 0), size = c(1, 1)))
#' add_goal(my_background)
#' 
#' objects(my_background) <- list(polygon(points = cbind(c(0.5, 0.5, -0.5, -0.5), 
#'                                                       c(0.5, -0.5, -0.5, 0.5))))
#' add_goal(my_background)
#' 
#' @seealso 
#' \code{\link[predped]{background-class}}
#' \code{\link[predped]{circle-class}}
#' \code{\link[predped]{goal-class}}
#' \code{\link[predped]{polygon-class}}
#' \code{\link[predped]{rectangle-class}}
#' \code{\link[predped]{segment-class}}
#' \code{\link[predped]{rng_point}}
#' 
#' @rdname add_goal-method
#' 
#' @export
# 
# Thought it would be more logical to place this method to the objects here, 
# even though it is not a method of the `goal` class.
setGeneric("add_goal", function(object, ...) standardGeneric("add_goal"))

setMethod("add_goal", signature(object = "object"), function(object, 
                                                             background,
                                                             id = character(0),
                                                             counter = 5,
                                                             ...){
    # Create an ever so slightly bigger polygon
    #
    # Is in response to a bug that appears when `m4ma::seesGoal` is used to 
    # check whether the location of the goal can be seen from a given node 
    # when making the path points: Given that the goal is part of the object, 
    # it is always unseen. With a slightly bigger object, this is not the case
    # anymore
    object <- enlarge(object, extension = 1e-2)

    # Generate a random goal and check whether or not this goal is not contained
    # within another object within the environment
    obj <- objects(background)
    shp <- shape(background)

    continue <- TRUE ; goal_check <- 0
    while(continue) {
        # Generate a random point on the object
        coord <- rng_point(object, ...)

        # Check whether this point is not contained within any other objects, 
        # but is still contained within the setting
        check <- sapply(obj, \(x) in_object(x, coord))
        continue <- out_object(shp, coord) | any(check)

        # Add 1 to the counter
        goal_check <- goal_check + 1

        if(goal_check > 100) {
            stop(paste0("Could not add a goal to the object with id ", 
                        id(object), 
                        ". Check whether goals can be added to accessible places on this object."))
        }
    }
    
    return(goal(id = id,
                position = coord, 
                counter = counter))    
})

#' Find path to a goal
#' 
#' Creates a numerical matrix of coordinates which contain the path points that 
#' the agent will use to move towards a goal. It thus finds itself on the 
#' strategic level (when performed in an initial planning phase) and the tactical 
#' level (when performed in response to blockages).
#' 
#' @details
#' Creates a numerical matrix containing coordinates of path points which, 
#' together, form a strategic path to the goal specified in this function. This
#' forms one of the functions that operate on the strategic and tactical level 
#' of the model.
#' 
#' To create this path, the function takes the following approach. First, it 
#' will create a network of nodes that are connected through unidirectional 
#' edges, as created by the \code{\link[predped]{create_edges}} function. 
#' Nodes are created based on the objects in the environment, so that the nodes
#' are always strategically placed at a certain distance away from the objects
#' in the environment (at a distance \code{space_between}). If the argument
#' \code{many_nodes = TRUE}, then a grid of other nodes are added to this 
#' list of initial nodes. This grid consists of nodes that are placed at an 
#' equal distance apart in the x- and y-direction, and spans 20 rows and 20 
#' columns. Once the nodes have been generated, those nodes that fall within 
#' any of the objects in the environment or falls outside of the environment 
#' are deleted. Unidirectional edges connect the different nodes and are 
#' similarly pruned based on intersections with objects or, when defined, 
#' intersections with instances of the \code{\link[predped]{segment-class}}.
#' 
#' Once the network of nodes and edges has been created, we translate this 
#' network into a graph that can be used by the \code{ccpRouting} package. 
#' Specifically, we use \code{\link[cppRouting]{makegraph}} for this purpose.
#' We then use the \code{\link[cppRouting]{get_path_pair}} function of the 
#' \code{cppRouting} package to find the shortest route to the goal. The user 
#' can specify which algorithm to use for this optimization through the argument 
#' \code{algorithm}, but the algorithm should be unidirectional to ensure that 
#' one-way walking can be enforced (as controlled through the \code{limited_access}
#' slot in the \code{\link[predped]{background-class}}).
#' 
#' Finally, the optimal path is checked for any redundancy and formatted so that 
#' the path points are contained in an n x 2 matrix. 
#'
#' @param object Object of \code{\link[predped]{goal-class}} for which the path
#' should be computed.
#' @param agent Object of \code{\link[predped]{agent-class}} who wants to move 
#' towards the goal defined in argument \code{object}.
#' @param background Object of \code{\link[predped]{background-class}} within 
#' which the goal and agent are contained.
#' @param space_between Numeric denoting the amount of space to leave between 
#' an object and a path point. Within this function, the default is the radius 
#' of the agent that is provided. However, in the \code{\link[predped]{simulate-method}}
#' function, this default is overwritten to be equal to \code{2.5} times this 
#' radius.
#' @param many_nodes Logical denoting whether to create many nodes or leave 
#' it at the minimum. Defaults to \code{FALSE}.
#' @param algorithm Character denoting the algorithm to be used for optimizing 
#' the path towards the goal. Is provided to the 
#' \code{\link[cppRouting]{get_path_pair}} function in the \code{cppRouting}
#' package. Defaults to \code{"Dijkstra"}. Note that one-way routing as 
#' controlled by the \code{limited_access} slot in the \code{background} may not 
#' work if a bidirectional algorithm is used.
#' @param precomputed_edges List containing the network of nodes and edges 
#' for the environment, as created by \code{\link[predped]{compute_edges}}. 
#' Defaults to \code{NULL}, triggering the creation of these edges again.
#' @param new_objects List of instances of \code{\link[predped]{object-class}} 
#' that are not yet contained in the \code{objects} slot in the argument 
#' \code{background}. Usually consists of other agents that the \code{agent}
#' has to account for when planning their route. Defaults to \code{NULL}.
#' @param reevaluate Logical denoting whether to reevaluate the network that is
#' provided in \code{precomputed_edges}. Is useful whenever \code{new_objects}
#' is not \code{NULL}, allowing us to check whether some nodes and edges are now
#' occluded by the new objects. Defaults to \code{FALSE}.
#' 
#' @return Numerical matrix of coordinates representing the path points the 
#' agent will have to move to to reach the goal.
#' 
#' @examples
#' # Create a setting
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)),
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 0.5)))
#' 
#' # Create an agent that is walking around there and a goal that the agent 
#' # will move to
#' my_goal <- add_goal(objects(my_background)[[1]], my_background)
#' my_agent <- agent(center = c(0.7, 0.7), radius = 0.25)
#' 
#' # Find the path of the agent to his goal
#' find_path(my_goal, 
#'           my_agent,
#'           my_background)
#' 
#' @seealso
#' \code{\link[predped]{agent-class}}
#' \code{\link[predped]{background-class}}
#' \code{\link[predped]{goal-class}}
#' \code{\link[predped]{object-class}}
#' \code{\link[predped]{simulate}}
#' \code{\link[predped]{adjust_edges}}
#' \code{\link[predped]{compute_edges}}
#' \code{\link[predped]{create_edges}}
#'
#' @rdname find_path-method
#' 
#' @export
setGeneric("find_path", function(object, ...) standardGeneric("find_path"))

setMethod("find_path", "goal", function(object, 
                                        agent,
                                        background,
                                        space_between = radius(agent),
                                        many_nodes = FALSE,
                                        algorithm = "Dijkstra",
                                        precomputed_edges = NULL,
                                        new_objects = NULL,
                                        reevaluate = FALSE) {
                                            
    # If there are no objects in the environment, you can just make a direct 
    # path between the agent and their goal
    if(length(objects(background)) == 0) {
        path_points <- matrix(position(object), nrow = 1)
        rownames(path_points) <- NULL
        colnames(path_points) <- c("x", "y")

        return(path_points)
    } 

    # Create the edges that are taken in by `makegraph`
    if(is.null(precomputed_edges)) {
        # If they are not precomputed, we should add the new objects to the 
        # objects in the environment before we can make the path
        if(!is.null(new_objects)) {
            objects(background) <- append(objects(background), 
                                          new_objects)
        }

        edges <- create_edges(position(agent),
                              position(object), 
                              background,
                              space_between = space_between,
                              many_nodes = many_nodes)
    } else {
        # If the edges are precomputed, adjust them to fit our current purpose
        edges <- adjust_edges(position(agent),
                              position(object),
                              background,
                              space_between = space_between,
                              new_objects = new_objects,
                              precomputed_edges = precomputed_edges,
                              reevaluate = reevaluate)
    }

    # If edges is actually NULL, we need to return this 
    if(is.null(edges)) {
        return(matrix(nrow = 0, ncol = 2))
    }

    if(nrow(edges$edges) == 0 | nrow(edges$nodes) == 0) {
        return(matrix(nrow = 0, ncol = 2))
    }
    
    # Create a graph that can be used by `cppRouting`. In constrast to Andrew, 
    # I put the directed argument to TRUE as the order of the nodes in the edges
    # matters
    graph <- cppRouting::makegraph(edges$edges, 
                                   directed = TRUE,
                                   coords = edges$nodes)

    # Make a check of whether "agent" and "goal" are contained within the nodes.
    # Might be deleted because of blocking, or because `make_graph` deletes them.
    if(!all(c("agent", "goal") %in% graph$coords$node_ID)) {
        return(matrix(nrow = 0, ncol = 2))
    }
    
    # Use cppRouting to do the strategic planning in this function
    path_points <- cppRouting::get_path_pair(graph,
                                             "agent", 
                                             "goal", 
                                             algorithm = algorithm,
                                             long = TRUE)

    # Once planned, transform the IDs of the nodes to actual coordinates to 
    # which the agent can walk. When merging, we should turn of `sort` to ensure 
    # that the order of the path_points is not changed.
    nodes <- edges$nodes |>
        setNames(c("node", "x", "y"))
    path_points <- merge(path_points, 
                         nodes, 
                         by = "node",
                         sort = FALSE)

    # Only retain the coordinates of interest (where the agent has to walk to) 
    # and transform to a matrix instead of a dataframe
    path_points <- path_points[-1, c("x", "y")] |>
        as.matrix()

    # Check whether you can delete some of the path points to make it a bit more 
    # concise. This only applies if there is more than one path point
    if(nrow(path_points) > 1) {
        # Set the original reference point to the position of the agent
        ref <- position(agent)

        # Loop over the path points starting at number 2 (number 1 being one 
        # that is guaranteed to be seen)
        for(i in 2:nrow(path_points)) {
            # Check whether agents can see the next path point starting from 
            # the reference point
            obj <- objects(background)
            if(length(obj) == 0) {
                seen <- TRUE
            } else {
                seen <- all(prune_edges(objects(background), 
                                        matrix(c(ref, path_points[i,]),
                                               nrow = 1)))
            }
            
            # If the path point is seen, you can just directly go to this path 
            # point instead of making a stop in the intermediate one
            if(seen) {
                path_points[i - 1,] <- NA

            # If the path point is not seen, that means the previous path point
            # is a critical one, and will therefore serve as the new reference
            } else {
                ref <- path_points[i - 1,]
            }
        }

        # Remove all unnecessary path points from the list
        path_points <- matrix(path_points[!is.na(path_points[,1]),],
                              ncol = 2)
    }

    # Some additional changes for convention
    rownames(path_points) <- NULL
    colnames(path_points) <- c("x", "y")    

    return(path_points)
})

#' Interact with goal
#' 
#' Defines the interaction with an object of \code{\link[predped]{goal-class}}. 
#' Subtracts \code{1} from the counter and indicates whether the goal has been 
#' accomplished (whenever the counter drops below 0).
#'
#' @param object Object of \code{\link[predped]{goal-class}}.
#' 
#' @return Object of \code{\link[predped]{goal-class}} with adjusted \code{counter}
#' slot and, if the goal has been completed, an adjusted \code{done} slot
#' 
#' @examples 
#' # Create a goal
#' my_goal <- goal(position = c(0, 0), 
#'                 counter = 5)
#' 
#' # Interact with the goal: Decreases the counter, but the goal is not done yet
#' updated_goal <- interact(my_goal)
#' updated_goal@counter
#' updated_goal@done
#' 
#' # Adjust the goal so that the counter is only 1 and interact with it again.
#' # Now the goal is done.
#' counter(my_goal) <- 1
#' 
#' updated_goal <- interact(my_goal)
#' updated_goal@counter
#' updated_goal@done
#'
#' @seealso 
#' \code{\link[predped]{goal-class}}
#' 
#' @rdname interact-method
#'
#' @export
setGeneric("interact", function(object) standardGeneric("interact"))

setMethod("interact", "goal", function(object) {
    # Decrease the counter and adjust the done slot
    object@counter <- object@counter - 1
    object@done <- object@counter <= 0

    return(object)
})

#' Replace a goal
#' 
#' Replaces an existing object of \code{\link[predped]{goal-class}} with an 
#' alternative object of the same class. The new goal is randomly generated or 
#' randomly drawn from a list of options (if provided).
#' 
#' @details
#' This function allows for a dynamical assignment of goals to agents.
#' 
#' @param object Object of \code{\link[predped]{goal-class}} which should be 
#' replaced by this function. 
#' @param setting Object of \code{\link[predped]{background-class}} within which
#' the goal should be created. Ignored if a list of potential goals is provided.
#' @param goal_list List containing instances of \code{\link[predped]{goal-class}}
#' from which the new goal should be chosen. Defaults to \code{NULL}, triggering 
#' the generation of a random goal.
#' @param counter_generator Function that takes in no arguments and generates 
#' a single numerical value that will be used as the counter of the goal. 
#' See \code{\link[predped]{generate_goal_stack}} for details on this argument.
#' Defaults to \code{\() rnorm(1, 10, 2)}.
#' 
#' @return Object of \code{\link[predped]{goal-class}}.
#' 
#' @examples 
#' # Create a goal
#' my_goal <- goal(position = c(0, 0))
#' my_goal
#' 
#' # Replace it with a random goal drawn from the environment. For this to work, 
#' # we first need to create a background as well.
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(rectangle(center = c(0, 0), 
#'                                                      size = c(1, 1))))
#' replace(my_goal, setting = my_background)
#' 
#' # Replace with a random goal drawn from a list of different goals. For this
#' # to work, we first define this list.
#' goal_list <- list(goal(position = c(-0.5, 0)), 
#'                   goal(position = c(0.5, 0)))
#' replace(my_goal, goal_list = goal_list)
#' 
#' @seealso 
#' \code{\link[predped]{background-class}}
#' \code{\link[predped]{goal-class}}
#' \code{\link[predped]{generate_goal_stack}}
#' 
#' @rdname replace-method
#' 
#' @export 
setGeneric("replace", function(object,...) standardGeneric("replace"))

setMethod("replace", "goal", function(object, 
                                      setting = NULL,
                                      goal_list = NULL,
                                      counter = \(n) rnorm(n, 10, 2)) {

    # If neither the background, nor a goal list is provided, we should throw 
    # an error
    if(is.null(setting) & is.null(goal_list)) {
        stop("Either a setting or a goal list should be defined to replace a goal.")
    }

    # If a goal-list is defined, we need to draw one of these goals as an 
    # alternative to the one being replaced.
    if(!is.null(goal_list)) {
        return(sample(goal_list, 1)[[1]])

    # Otherwise, generate a new one.
    } else {
        return(goal_stack(1, setting, counter = counter)[[1]])
    }
})

#' Generate a goal stack
#' 
#' Use the defined setting to generate the stack of random goals an agent should 
#' complete. This function outputs a list of different instances of the 
#' \code{\link[predped]{goal-class}}.
#' 
#' @param n Integer denoting the number of goals to generate.
#' @param setting Object of \code{\link[predped]{background-class}}.
#' @param counter Numeric, vector, or function that defines the counter for each 
#' of the goals that are generated. When numeric, each goal will have the same 
#' value for counter equal to the value provided to this argument. When a numeric
#' vector, the values of this vector will be iterated as values for the counter 
#' in the goals. When a function, a random value for the counter will be 
#' generated through the function. For this to work, the function that is 
#' provided should take in the input \code{n} which defines the number of 
#' values to draw from the function. Defaults to \code{\(n) rnorm(n, 10, 2)}.
#' @param sort Logical denoting whether to order the goal stack in a logical 
#' way. Currently implemented in the following way. First, we select the first 
#' goal as being the one that is closest by the starting position provided in 
#' the argument \code{starting_position}. Then, we define each of the next goals
#' as being the one that is closest to the position of the previous goal.
#' Defaults to \code{TRUE}.
#' @param starting_position Numeric vector denoting the position at which the 
#' agent starts in the room. Defaults to the first entrance of the \code{setting}.
#' @param precompute_goal_paths Logical denoting whether to run the
#' \code{\link[predped]{find_path-method}} for each of the generated goals 
#' beforehand. Assumes that the agent does all of the goals in the order of the 
#' goal stack. Defaults to \code{FALSE}. 
#' @param middle_edge Logical denoting whether to sample the goals from the 
#' middle of the edge of the objects in the \code{link[predped]{background-class}}
#' (\code{TRUE}) or to allow the goal locations to fall on all points on these 
#' edges (\code{FALSE}). Defaults to \code{FALSE}.
#' @param ... Arguments provided to \code{\link[predped]{find_path-method}} to 
#' precompute the paths that the agents should take to reach their goals. Only
#' used when \code{precompute_goal_paths = TRUE}.
#' 
#' @return List of instances of the \code{\link[predped]{goal-class}}.
#' 
#' @examples 
#' # Create a setting
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(cirlce(center = c(0, 0), 
#'                                                   radius = 0.5)))
#' 
#' # Create a goal stack containing two goals
#' goal_stack <- goal_stack(2, my_background)
#' 
#' # Two goals
#' length(goal_stack)
#' goal_stack
#' 
#' @seealso 
#' \code{\link[predped]{background-class}}
#' \code{\link[predped]{goal-class}}
#' \code{\link[predped]{compute_edges}}
#' \code{\link[predped]{create_edges}}
#' \code{\link[predped]{determine_values}}
#' \code{\link[predped]{multiple_goal_stacks}}
#' 
#' @rdname goal_stack
#' 
#' @export 
#
# TO DO
#   - Sorting algorithm is not that good yet. Try to come up with a better and 
#     efficient solution
goal_stack <- function(n, 
                       setting,
                       counter = \(n) rnorm(n, 10, 2),
                       sort = TRUE, 
                       starting_position = entrance(setting)[1,],
                       precompute_goal_paths = FALSE,
                       middle_edge = FALSE,
                       ...) {
    
    # Step 1: Generating the goal stack

    # Generate all the counter values for the goals to-be-generated. Depends on 
    # the type that has been provided in counter
    counter <- determine_values(counter, n)

    # Select the objects in the environment that can contain a goal, as defined 
    # by the slot interactable
    idx <- sapply(objects(setting), 
                  \(x) x@interactable)
    obj <- objects(setting)[idx]

    # Throw an error if none of the objects are interactable.
    if(length(obj) == 0) {
        stop("None of the objects in the environment can contain a goal.")
    }

    # Randomly sample `n` objects from the `potential_objects` and assign a goal 
    # on one of its edges, as handled by the `add_goal` method. 
    obj <- sample(obj, n, replace = TRUE)
    goal_stack <- lapply(seq_along(obj), 
                         \(i) add_goal(obj[[i]], 
                                       setting, 
                                       counter = counter[i],
                                       middle_edge = middle_edge))



    # Step 2: Ordering the goal stack

    # First check whether the goal stack should be ordered at all. If not, then 
    # the if-statement is not executed and we immediately go to Step 3.
    if(sort & length(goal_stack) > 1 & !is.null(starting_position)) {
        # Create a local function that outputs the goal with the closest distance
        # to a given coordinate y. This function will be ran until we reached 
        # the last goal in the goal stack, allowing us to easily sort the goal
        # stack based on distances from the starting point and from the other 
        # goals.
        closest_goal <- function(goal_stack, start) {
            # Compute the squared distance of each of the goals to the 
            # starting point.
            dist <- sapply(goal_stack, 
                           \(x) (start[1] - position(x)[1])^2 + (start[2] - position(x)[2])^2)

            # Return an index saying for which goal the distance is smallest
            return(dist == min(dist))
        }

        # Get the first goal done and delete it from the list
        idx <- closest_goal(goal_stack, starting_position)

        ordered_goal_stack <- goal_stack[idx]
        goal_stack <- goal_stack[!idx]

        # Now loop over the others, but only if there are enough others to iterate
        # over.
        if(length(goal_stack) != 0) {
            for(i in seq_along(n)) {
                # Double check whether there are any goals left. If not, then 
                # we have to break out of this for-loop. Can occur if there are 
                # many ties in the distances walked.
                if(length(goal_stack) == 0) {
                    break
                }

                # Repeat the same procedure
                N <- length(ordered_goal_stack)
                idx <- closest_goal(goal_stack, 
                                    position(ordered_goal_stack[[N]]))
    
                ordered_goal_stack <- append(ordered_goal_stack, goal_stack[idx])
                goal_stack <- goal_stack[!idx]
            }
        }

        if(length(goal_stack) != 0) {
            ordered_goal_stack <- append(ordered_goal_stack, goal_stack)
        }

        goal_stack <- ordered_goal_stack
    }



    # Step 3: Precomputing the goal paths

    # If you don't want to or cannot precompute the goal paths, return the 
    # goal stack as is
    if(!precompute_goal_paths | length(goal_stack) == 1) {
        return(goal_stack)
    }

    # If you want to precompute the goal paths, loop over the last n - 1 goals
    # in the goal stack and find the path starting at the location of the 
    # previous goal. The path of the first goal is computed in the `add_agent`
    # function and does not need to be addressed here.
    dummy <- agent(center = c(0, 0), radius = 0.25)
    for(i in seq_along(goal_stack)) {
        # Adjust the position of the dummy based on which goal in the goal stack
        # we are looking at
        if(i == 1) {
            position(dummy) <- starting_position
        } else {
            position(dummy) <- position(goal_stack[[i - 1]])
        }
        
        # Compute the path to the goal
        goal_stack[[i]]@path <- find_path(goal_stack[[i]], 
                                          dummy, 
                                          setting,
                                          ...)
    }

    return(goal_stack)
}

#' Generate multiple goal stacks
#'
#' Use the defined setting to generate multiple stacks of random goals that 
#' each agent might have to complete. This function outputs a list of multiple 
#' lists with different instances of the \code{\link[predped]{goal-class}}.
#' 
#' @param n Integer denoting the number of goals to generate.
#' @param setting Object of \code{\link[predped]{background-class}}.
#' @param goal_number Numeric, vector, or function that defines the number of 
#' goals for each of the goal stacks that will be generated. When numeric, each 
#' goal stack will have the same number of goals equal to the value provided 
#' to this argument. When a numeric vector, the values of this vector will be 
#' iterated as values for the number of goals in the goal stacks. When a 
#' function, a random value for the number of goals for each goal stack counter 
#' will be generated through the function. For this to work, the function that is 
#' provided should take in the input \code{n} which defines the number of 
#' values to draw from the function. Defaults to \code{\(n) rnorm(n, 10, 2)}.
#' @param ... Arguments provided to \code{\link[predped]{goal_stack}}.
#' 
#' @return List of lists containing instances of the 
#' \code{\link[predped]{goal-class}}.
#' 
#' @examples 
#' # Create a setting
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(cirlce(center = c(0, 0), 
#'                                                   radius = 0.5)))
#' 
#' # Create two goal stacks containing two goals each
#' goal_stack <- multiple_goal_stacks(2, my_background, goal_number = 2)
#' 
#' # Two goal stacks of two goals each
#' length(goal_stack)
#' 
#' length(goal_stack[[1]])
#' goal_stack[[1]]
#' 
#' length(goal_stack[[2]])
#' goal_stack[[2]]
#' 
#' @seealso 
#' \code{\link[predped]{background-class}}
#' \code{\link[predped]{goal-class}}
#' \code{\link[predped]{determine_values}}
#' \code{\link[predped]{goal_stack}}
#' 
#' @rdname multiple_goal_stack
#' 
#' @export 
multiple_goal_stacks <- function(n, 
                                 setting,
                                 goal_number = \(x) rnorm(x, 10, 2), 
                                 ...) {

    cat("\rGenerating multiple goal stacks")

    # Define the number of goals per goal-stack
    goal_number <- determine_values(goal_number, n)

    # Loop over the number of goal stacks to create
    return(lapply(goal_number[1:n], 
                  \(x) goal_stack(x, setting, ...)))
}





################################################################################
# GETTERS AND SETTERS

#' @rdname busy-method
setMethod("busy", "goal", function(object) {
    return(object@busy)
})

#' @rdname busy-method
setMethod("busy<-", "goal", function(object, value) {
    object@busy <- value
    return(object)
})



#' @rdname counter-method
setMethod("counter", "goal", function(object) {
    return(object@counter)
})

#' @rdname counter-method
setMethod("counter<-", "goal", function(object, value) {
    object@counter <- value
    return(object)
})



#' @rdname done-method
setMethod("done", "goal", function(object) {
    return(object@done)
})

#' @rdname done-method
setMethod("done<-", "goal", function(object, value) {
    object@done <- value
    return(object)
})



#' @rdname id-method
setMethod("id", "goal", function(object) {
    return(object@id)
})

#' @rdname id-method
setMethod("id<-", "goal", function(object, value) {
    object@id <- value
    return(object)
})



#' @rdname path-method
setMethod("path", "goal", function(object) {
    return(object@path)
})

#' @rdname path-method
setMethod("path<-", "goal", function(object, value) {
    object@path <- value
    return(object)
})



#' @rdname position-method
setMethod("position", "goal", function(object) {
    return(object@position)
})

#' @rdname position-method
setMethod("position<-", "goal", function(object, value) {
    object@position <- as(value, "coordinate")
    return(object)
})