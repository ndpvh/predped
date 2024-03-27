# Initialization
testthat::test_that("Goal initialization works", {
    set.seed(1)
    ref <- c(1,1)
    g <- predped::goal(position = ref)

    testthat::expect_no_error(predped::goal(position = ref))
    testthat::expect_equal(g@position, coordinate(ref))
    testthat::expect_equal(g@id, "goal ydgab")
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
                                                                     interactable = TRUE)))

    set.seed(1)
    goal_stack <- predped::generate_goal_stack(2, setting, \(x) 5)

    ref <- list(goal(id = "goal oueiy", 
                     position = coordinate(c(1.01, -1.44)), 
                     busy = FALSE,
                     counter = 5), 
                goal(id = "goal tzlyw", 
                     position = coordinate(c(1.01, 0.95)), 
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
                                                                     interactable = TRUE)))

    set.seed(1)
    goal_stack <- predped::generate_goal_stack(2, setting, \(x) 5)

    ref <- list(goal(id = "goal oueiy", 
                     position = coordinate(c(1.01, 0.56)), 
                     busy = FALSE,
                     counter = 5), 
                goal(id = "goal tzlyw", 
                     position = coordinate(c(1.01, 0.95)), 
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

    testthat::expect_error(goal(2, setting, \(x) 5))
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
                                                                  interactable = TRUE)))

    set.seed(1)
    goal_stack <- predped::generate_goal_stack(2, setting, \(x) 5)

    ref <- list(goal(id = "goal rsauu", 
                     position = coordinate(c(-0.70, -1.73)), 
                     busy = FALSE,
                     counter = 5), 
                goal(id = "goal oueiy", 
                     position = coordinate(c(0.93, 0.60)), 
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
                                                                  interactable = TRUE)))

    set.seed(1)
    goal_stack <- predped::generate_goal_stack(2, setting, \(x) 5)

    ref <- list(goal(id = "goal rsauu", 
                     position = coordinate(c(-0.70, 0.27)), 
                     busy = FALSE,
                     counter = 5), 
                goal(id = "goal oueiy", 
                     position = coordinate(c(0.93, 0.60)), 
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

    testthat::expect_error(goal(2, setting, \(x) 5))
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
    testthat::expect_false(g1_updated@done)
    testthat::expect_true(g0_updated@done)

    # Within a goal stack
    goal_stack <- list(predped::goal(position = ref, counter = 5), 
                       predped::goal(position = ref, counter = 0), 
                       predped::goal(position = ref, counter = 1))

    updated_goal_stack <- lapply(goal_stack, 
                                 \(x) predped::interact(x))

    testthat::expect_equal(length(updated_goal_stack), 3)
    testthat::expect_equal(lapply(updated_goal_stack, \(x) x@done), 
                           list(FALSE, TRUE, FALSE))
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
    goal_stack <- generate_goal_stack(3, setting, \(x) 5)
    updated_goal_stack <- goal_stack

    updated_goal_stack[[1]] <- predped::replace(goal_stack[[1]], setting, \(x) 4)

    testthat::expect_equal(updated_goal_stack[[1]]@position@.Data, c(-1.01, -1.45), tolerance = 1e-2)
    testthat::expect_equal(updated_goal_stack[[1]]@counter, 4)
    testthat::expect_equal(goal_stack[[2]], updated_goal_stack[[2]])
    testthat::expect_equal(goal_stack[[3]], updated_goal_stack[[3]])
})
