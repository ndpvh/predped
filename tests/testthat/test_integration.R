# Integration tests for the `move` function
#
# The `move` function performs one step for one agent in the simulation.
# This integration tests compares the result of the `predped::move_agent`
# function against the `move` function in the RCore code base.

# Load functions from RCore
library(m4ma)

paths <- c(
  "pp_objects.R",
  "pp_environment.R",
  "pp_geometry.R",
  "pp_plot.R",
  "pp_predict.R",
  "pp_utility_extra.R",
  "pp_utility.R",
  "pp_parameter.R",
  "pp_dcm.R",
  "pp_route.R",
  "pp_goals.R",
  "pp_block.R",
  "pp_block_r.R",
  "pp_collide.R",
  "pp_simulate.R",
  "pp_flow.R",
  "pp_estimation.R"
)

for (p in paths) {
  source(fs::path_package("extdata", p, package = "predped"))
}

# Load example parameters from file
pars <- unlist(
  read.csv(testthat::test_path("data", "play_settings.csv"))[1, 9:29]
)

# Append missing nest parameters
pars <- c(
  c(Central = -Inf, NonCentral = -Inf, acc = -Inf, const = -Inf, dec = -Inf),
  pars
)

# Define starting agent properties
r <- 0.3
a <- 45
v <- 0.5

# Define goal matrix for agents
P1 <-  matrix(c(2, 2, -1, -1), 2, 2)
rownames(P1) <-  c('g1', 'g2')

# Create agents
a1 <- predped::agent(
    center = c(0, 0),
    radius = r,
    orientation = a,
    speed = v,
    group = 2,
    goals = P1,
    current_goal = P1[1, , drop = FALSE],
    parameters = pars
)

a2 <- predped::agent(
    center = c(0.75, 0.75),
    radius = r,
    orientation = a,
    speed = v,
    group = 1,
    goals = P1,
    current_goal = P1[1, , drop = FALSE],
    parameters = pars
)

a3 <- predped::agent(
    center = c(2, 2),
    radius = r,
    orientation = a,
    speed = v,
    group = 2,
    goals = P1,
    current_goal = P1[1, , drop = FALSE],
    parameters = pars
)

# Create state object as list of list of agents
state <- list(
    agents = list(a1, a2, a3)
)

ids <- sapply(state$agents, function(x) x@id)

# Define predicted pedestrian positions
p_pred <- rbind(c(0, 0), c(0.75, 0.75), c(1.25, 1.25))

rownames(p_pred) <- ids

# Define background object with rectangle objects
b <- predped::background(
  shape = predped::rectangle(center = c(0, 0), size = c(6, 6)),
  objects = list(
    predped::rectangle(center = c(0, -1), size = c(0.5, 0.5)),
    predped::rectangle(center = c(-1, 0), size = c(0.5, 0.5))
  )
)

# Define nests and alpha lists
nests <- list(
  Central = c(0, 6, 17, 28),
  NonCentral = c(0:33)[-c(6, 17, 28)],
  acc = c(1:11),
  const = c(12:22),
  dec = c(0, 23:33)
)

alpha <- list(
  Central = rep(1/3, 4),
  NonCentral = c(1/3, rep(0.5, 4), 1/3, rep(0.5, 9), 1/3,
                 rep(0.5, 9), 1/3, rep(0.5, 5)),
  acc = c(rep(0.5, 4), 1, 1/3, rep(0.5, 5)),
  const = c(rep(0.5, 4), 1, 1/3, rep(0.5, 5)),
  dec = c(1/3, rep(0.5, 4), 1, 1/3, rep(0.5, 5))
)

# Index of agent to be moved
n <- 1

# Helper function to convert agent list into RCore state
convert_state <- function(s) {
    id <- sapply(s, predped::id)
    p <- t(sapply(s, predped::position))
    row.names(p) <- id
    r <- sapply(s, predped::size)
    a <- sapply(s, predped::orientation)
    v <- sapply(s, predped::speed)
    g <- sapply(s, predped::group)
    cell <- sapply(s, predped::cell)
    P <- lapply(s, function(x) {
        pN <- x@goals
        attr(pN, "i") <- 1
        attr(pN, "stop") <- 0
        return(pN)
    })
    names(P) <- id

    pMat <- t(sapply(s, function(x) x@parameters))
    return(list(
        p = p,
        v = v,
        r = r,
        a = a,
        P = P,
        group = g,
        pMat = pMat,
        cell = cell
    ))
}

# Helper functions to convert predped objects into RCore objects
convert_object <- function(o) {
    return(list(x = c(o@points[1, 1], o@points[3, 1]), y = c(o@points[1, 2], o@points[3, 2])))
}

convert_objects <- function(b) {
    bo <- convert_object(b@shape)
    os <- lapply(b@objects, convert_object)
    return(c(list(bo), os))
}

converted_state <- convert_state(state$agents)

converted_objects <- convert_objects(b)

move_rcore <- get("move", envir = .GlobalEnv)

testthat::test_that("Move function result matches reference", {
  n_iter <- 5

  for (i in seq_len(n_iter)) {

    set.seed(123)

    new_a1 <- predped::update_agent(a1, state, p_pred, b, nests, alpha)

    set.seed(123)

    ref <- move_rcore(
      n,
      converted_state,
      getP(converted_state),
      p_pred,
      nests,
      alpha,
      converted_objects,
      NULL
    )

    testthat::expect_equal(predped::position(new_a1), predped::coordinate(ref$p))
    testthat::expect_equal(predped::speed(new_a1), ref$v)
    testthat::expect_equal(predped::orientation(new_a1), ref$a)
    testthat::expect_equal(unname(predped::cell(new_a1)), ref$cell)
  }
})
