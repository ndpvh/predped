#' An S4 Class to Represent Agents
#'
#' @slot id A numerical index for the agent
#' @slot position A numerical vector of two elements denoting the position of
#' the agent in 2D space
#' @slot size A numeric indicating the size of the radius of the agent
#' @slot orientation A numeric denoting the orientation of the agent in degrees
#' @slot interacting A logical denoting whether the agent is interacting with
#' something in its environment
#' @slot parameters A named list that contains the individual-specific parameters
#' for the agent
#' @slot goals A list containing the id's of the goals that are assigned to the
#' agent
#'
#' @export
agent <- setClass("agent", list(id = "character", 
                                speed = "numeric",
                                orientation = "numeric",
                                group = "numeric",
                                cell = "numeric",
                                parameters = "numeric",
                                goals = "matrix",
                                current_goal = "numeric"), contains = c("circle"))

setMethod("initialize", "agent", function(.Object,
                                          id = paste(sample(letters, 5)),
                                          speed = 1,
                                          orientation = 0,
                                          group = 0,
                                          cell = 0,
                                          # parameters = c(),
                                          # goals = list(),
                                          # current_goal = list(),
                                          moveable = TRUE,
                                          busy = FALSE,
                                          interactable = TRUE,
                                          interacted_with = FALSE, ...
) {
    .Object <- callNextMethod(.Object, moveable = moveable, busy = busy, interactable = interactable, interacted_with = interacted_with, ...)

    .Object@id <- id
    .Object@speed <- speed
    .Object@orientation <- orientation
    .Object@group <- group
    .Object@cell <- cell
    # .Object@parameters <- parameters
    # .Object@goals <- goals
    # .Object@current_goal <- current_goal

    return(.Object)
})

utility <- function(object, state, p_pred, centres, objects, ok) {
    PS <- dist1(object@center, matrix(object@current_goal[1:2], ncol = 2))
    GA <- destinationAngle(object@orientation, matrix(object@center, ncol = 2),
                           object@goals) / 90
    ID <- predClose(1, p1 = matrix(object@center, ncol = 2), a1 = object@orientation,
                    p2 = state$p, r = state$r, centres, p_pred,
                    objects = objects)
    BA <- blockedAngle(1, state, p_pred, objects)

    if (length(BA) == 0) BA <- NULL

    FL <- getLeaders(1, state, centres, objects)
    WB <- getBuddy(1, group = state$group, a = state$a, p_pred, centres,
                   objects, state = state)

    out <-
        psUtility(object@parameters, object@speed, PS) +
        # gaUtility(object@parameters, GA) +
        caUtility(object@parameters) +
        idUtility(object@parameters, 1, ID, ok, group = state$group) +
        baUtility(object@parameters, BA) +
        flUtility(object@parameters, FL) +
        wbUtility(object@parameters, WB)

    # Stop baseline (set by gwUtility) and scaling
    return(c(-object@parameters["bS"], out) / object@parameters["rU"])
}


#' Title
#'
#' @param object
#' @param ...
#'
#' @return
#' @export
#'
setGeneric("step", function(object, ...) standardGeneric("step"))

setMethod("step", "agent", function(object,
                                    state,
                                    p_pred,
                                    nests,
                                    alpha,
                                    objects,
                                    sStop = 0.2, # speed to resume after stop
                                    usebestAngle = FALSE,
                                    angle = c(-72.5, -50, -32.5, -20, -10,
                                              0, 10, 20, 32.5, 50, 72.5)) {

    centres <- c_vd(1:33, p1 = object@center, v1 = object@speed, a1 = object@orientation)

    ok <- matrix(TRUE, nrow = 11, ncol = 3)

    V <- utility(object, state, p_pred, centres, objects, ok)

    Pr <- sapply(0:33, function(i) pcnl_rcpp(get_cell_nest()[i, ], V, rep(1, length(nests)), nests, alpha, mu = 1))
    names(Pr) <- 0:33

    cell <- sample.int(length(V), 1, TRUE, prob = Pr) - 1

    pos <- c_vd(cell, p1 = object@center, v1 = object@speed, a1 = object@orientation)

    return(move(object, as.numeric(pos)))
})

