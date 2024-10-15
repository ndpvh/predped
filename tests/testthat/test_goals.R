# Initialization
testthat::test_that("Goal initialization works", {
    set.seed(1)
    ref <- c(1,1)
    g <- predped::goal(position = ref)

    testthat::expect_no_error(predped::goal(position = ref))
    testthat::expect_equal(g@position, coordinate(ref))
    testthat::expect_equal(g@id, "goal ydgab")

    # In response to bug in which the given id was not assigned to the object
    todo <- predped::goal(id = "test", position = c(0, 0))
    testthat::expect_equal(todo@id, "test")
})

# Creating goal stacks
testthat::test_that("Generating goal stack for rectangles works", {
    # All interactable objects
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(6, 6)),
                                   objects = list(predped::rectangle(center = c(0, -1), 
                                                                     size = c(2, 1),
                                                                     interactable = TRUE),
                                                  predped::rectangle(center = c(0, 1), 
                                                                     size = c(2, 1),
                                                                     interactable = TRUE)), 
                                   entrance = c(-3, 0))

    # Generate goal stack and delete the created path towards it
    set.seed(1)
    goal_stack <- predped::goal_stack(2, setting, counter = 5)

    # Original order switched due to the goals being ordered according to closeness
    ref <- list(goal(id = "goal auujv", 
                     position = coordinate(c(1.01, 1.33)), 
                     busy = FALSE, 
                     counter = 5),
                goal(id = "goal bwknr", 
                     position = coordinate(c(1.01, -1.41)), 
                     busy = FALSE,
                     counter = 5))

    testthat::expect_equal(goal_stack, ref, tolerance = 1e-2)

    # Single interactable object
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(6, 6)),
                                   objects = list(predped::rectangle(center = c(0, -1), 
                                                                     size = c(2, 1),
                                                                     interactable = FALSE),
                                                  predped::rectangle(center = c(0, 1), 
                                                                     size = c(2, 1),
                                                                     interactable = TRUE)), 
                                   entrance = c(-3, 0))

    set.seed(1)
    goal_stack <- predped::goal_stack(2, setting, counter = 5)

    ref <- list(goal(id = "goal bwknr", 
                     position = coordinate(c(1.01, 0.59)), 
                     busy = FALSE,
                     counter = 5),
                goal(id = "goal auujv", 
                     position = coordinate(c(1.01, 1.33)), 
                     busy = FALSE, 
                     counter = 5))

    testthat::expect_equal(goal_stack, ref, tolerance = 1e-2)

    # No interactable objects
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(6, 6)),
                                   objects = list(predped::rectangle(center = c(0, -1), 
                                                                     size = c(2, 1),
                                                                     interactable = FALSE),
                                                  predped::rectangle(center = c(0, 1), 
                                                                     size = c(2, 1),
                                                                     interactable = FALSE)))

    testthat::expect_error(predped::goal_stack(2, setting, counter = 5))
})

# Creating goal stacks
testthat::test_that("Generating goal stack for circles works", {
    # All interactable objects
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(6, 6)),
                                   objects = list(predped::circle(center = c(0, -1), 
                                                                  radius = 1,
                                                                  interactable = TRUE),
                                                  predped::circle(center = c(0, 1), 
                                                                  radius = 1,
                                                                  interactable = TRUE)), 
                                   entrance = c(-3, 0))

    set.seed(1)
    goal_stack <- predped::goal_stack(2, setting, counter = 5)

    ref <- list(goal(id = "goal abwkn", 
                     position = coordinate(c(-0.91, -1.45)), 
                     busy = FALSE, 
                     counter = 5),
                goal(id = "goal sauuj", 
                     position = coordinate(c(0.93, 1.38)), 
                     busy = FALSE,
                     counter = 5))

    testthat::expect_equal(goal_stack, ref, tolerance = 1e-2)

    # Single interactable object
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(6, 6)),
                                   objects = list(predped::circle(center = c(0, -1), 
                                                                  radius = 1,
                                                                  interactable = FALSE),
                                                  predped::circle(center = c(0, 1), 
                                                                  radius = 1,
                                                                  interactable = TRUE)), 
                                   entrance = c(-3, 0))

    set.seed(1)
    goal_stack <- predped::goal_stack(2, setting, counter = 5)

    ref <- list(goal(id = "goal abwkn", 
                     position = coordinate(c(-0.91, 0.55)), 
                     busy = FALSE, 
                     counter = 5),
                goal(id = "goal sauuj", 
                     position = coordinate(c(0.93, 1.38)), 
                     busy = FALSE,
                     counter = 5))

    testthat::expect_equal(goal_stack, ref, tolerance = 1e-2)

    # No interactable objects
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(6, 6)),
                                   objects = list(predped::circle(center = c(0, -1), 
                                                                  radius = 1,
                                                                  interactable = FALSE),
                                                  predped::circle(center = c(0, 1), 
                                                                  radius = 1,
                                                                  interactable = FALSE)))

    testthat::expect_error(goal_stack(2, setting, counter = 5))
})

# Simulating multiple goal stacks
testthat::test_that("Simulating multiple goal stacks works", {
    # All interactable objects
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(6, 6)),
                                   objects = list(predped::rectangle(center = c(0, -1), 
                                                                     size = c(2, 1),
                                                                     interactable = TRUE),
                                                  predped::circle(center = c(0, 1), 
                                                                  radius = 1,
                                                                  interactable = TRUE)),
                                   entrance = c(-3, 0))

    # Create multiple goal stacks
    set.seed(1)
    goal_stacks <- predped::multiple_goal_stacks(3, 
                                                 setting, 
                                                 goal_number = 1:3,
                                                 counter = 5)
    
    # Extract test conditions and create reference
    tst_stack <- length(goal_stacks)
    tst_number <- lapply(goal_stacks, 
                         \(x) length(x))
    tst_counter <- lapply(goal_stacks, 
                          \(x) lapply(x, predped::counter))

    ref_stack <- 3
    ref_number <- list(1, 2, 3)
    ref_counter <- list(list(5), list(5, 5), list(5, 5, 5))

    # Do the actual tests
    testthat::expect_equal(tst_stack, ref_stack)
    testthat::expect_equal(tst_number, ref_number)
    testthat::expect_equal(tst_counter, ref_counter)
})

# Interacting with a goal
testthat::test_that("Goal interaction works", {
    ref <- c(1,1)
    
    # Individual goals and basic counters
    g5 <- predped::goal(position = ref, counter = 5)
    g1 <- predped::goal(position = ref, counter = 1)
    g0 <- predped::goal(position = ref, counter = 0)

    g5_updated <- predped::interact(g5)
    g1_updated <- predped::interact(g1)
    g0_updated <- predped::interact(g0)

    testthat::expect_equal(g5_updated@counter, 4)
    testthat::expect_equal(g1_updated@counter, 0)
    testthat::expect_equal(g0_updated@counter, -1)

    testthat::expect_false(g5_updated@done)
    testthat::expect_true(g1_updated@done)
    testthat::expect_true(g0_updated@done)

    # Within a goal stack
    goal_stack <- list(predped::goal(position = ref, counter = 5), 
                       predped::goal(position = ref, counter = 0), 
                       predped::goal(position = ref, counter = 1))

    updated_goal_stack <- lapply(goal_stack, 
                                 \(x) predped::interact(x))

    testthat::expect_equal(length(updated_goal_stack), 3)
    testthat::expect_equal(lapply(updated_goal_stack, \(x) x@done), 
                           list(FALSE, TRUE, TRUE))
    testthat::expect_equal(lapply(updated_goal_stack, \(x) x@counter), 
                           list(4, -1, 0))
})

# Replacing a goal with another
testthat::test_that("Goal replacement works", {
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(6, 6)),
                                   objects = list(predped::rectangle(center = c(0, -1), 
                                                                     size = c(2, 1),
                                                                     interactable = TRUE),
                                                  predped::rectangle(center = c(0, 1), 
                                                                     size = c(2, 1),
                                                                     interactable = TRUE)))

    set.seed(1)
    goal_stack <- goal_stack(3, setting, counter = 5)
    updated_goal_stack <- goal_stack

    updated_goal_stack[[1]] <- predped::replace(goal_stack[[1]], setting, counter = 4)

    testthat::expect_equal(updated_goal_stack[[1]]@position@.Data, c(-1.01, 1.37), tolerance = 1e-2)
    testthat::expect_equal(updated_goal_stack[[1]]@counter, 4)
    testthat::expect_equal(goal_stack[[2]], updated_goal_stack[[2]])
    testthat::expect_equal(goal_stack[[3]], updated_goal_stack[[3]])
})

testthat::test_that("Goal getters work", {
    tst <- predped::goal(id = "test",
                         position = c(0, 0),
                         path = matrix(0, nrow = 2, ncol = 2),
                         busy = TRUE, 
                         done = TRUE,
                         counter = 5)

    testthat::expect_equal(predped::id(tst), "test")
    testthat::expect_equal(predped::position(tst), predped::coordinate(c(0, 0)))
    testthat::expect_equal(predped::path(tst), matrix(0, nrow = 2, ncol = 2))
    testthat::expect_true(predped::busy(tst))
    testthat::expect_true(predped::done(tst))
    testthat::expect_equal(predped::counter(tst), 5)
})

testthat::test_that("Goal setters work", {
    tst <- predped::goal(id = "test",
                         position = c(0, 0),
                         path = matrix(0, nrow = 2, ncol = 2),
                         busy = TRUE, 
                         done = TRUE,
                         counter = 5)

    predped::id(tst) <- "other goal"
    predped::position(tst) <- c(1, 1)
    predped::path(tst) <- matrix(1, nrow = 2, ncol = 2)
    predped::busy(tst) <- FALSE
    predped::done(tst) <- FALSE
    predped::counter(tst) <- 4

    testthat::expect_equal(predped::id(tst), "other goal")
    testthat::expect_equal(predped::position(tst), predped::coordinate(c(1, 1)))
    testthat::expect_equal(predped::path(tst), matrix(1, nrow = 2, ncol = 2))
    testthat::expect_false(predped::busy(tst))
    testthat::expect_false(predped::done(tst))
    testthat::expect_equal(predped::counter(tst), 4)
})

testthat::test_that("Find path for empty room works", {
    settings <- list(predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                                    size = c(1, 1)), 
                                         objects = list()),
                     predped::background(shape = predped::polygon(points = rbind(c(1, 1),    
                                                                                 c(1, -1), 
                                                                                 c(-1, -1), 
                                                                                 c(-1, 1))), 
                                         objects = list()),
                     predped::background(shape = predped::circle(center = c(0, 0), 
                                                                 radius = 1), 
                                         objects = list()))

    ref <- matrix(0, nrow = 1, ncol = 2)
    colnames(ref) <- c("x", "y")

    tst <- lapply(settings, 
                  \(x) predped::find_path(predped::goal(position = c(0, 0)), 
                                          predped::agent(center = c(1, 0), radius = 1),
                                          x))
    
    testthat::expect_equal(tst[[1]], ref)
    testthat::expect_equal(tst[[2]], ref)
    testthat::expect_equal(tst[[3]], ref)
})
