testthat::test_that("Number of pedestrians blocking view from goal works", {
    mock_setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                                   size = c(10, 2)),
                                        objects = list(), 
                                        entrance = predped::coordinate(c(-5, 0)))

    me <- predped::agent(center = c(-4, 0), 
                         radius = 0.2, 
                         orientation = 0,
                         current_goal = predped::goal(position = c(4, 0)))

    # Create some blocking and non-blocking pedestrians
    block_1 <- predped::agent(center = c(-2, 0), radius = 0.2)
    block_2 <- predped::agent(center = c(2, 0), radius = 0.2)
    noblock_1 <- predped::agent(center = c(-2, 0.75) , radius = 0.2)
    noblock_2 <- predped::agent(center = c(-2, -0.75) , radius = 0.2)

    # Try several combinations and see whether the correct number of blocking
    # agents are derived. `me` is not included in this list anymore, as the 
    # `state$agents` list that gets passed down to this function only contains
    # the other agents
    state_0a <- list(setting = mock_setting, 
                     agents = list(noblock_1, noblock_2))
    state_0b <- list(setting = mock_setting, 
                     agents = list(noblock_1))
    state_0c <- list(setting = mock_setting, 
                     agents = list(noblock_2))

    state_1a <- list(setting = mock_setting, 
                     agents = list(block_1))
    state_1b <- list(setting = mock_setting, 
                     agents = list(noblock_1, block_1))
    state_1c <- list(setting = mock_setting, 
                     agents = list(noblock_2, block_2))

    state_2a <- list(setting = mock_setting, 
                     agents = list(block_1, block_2))
    state_2b <- list(setting = mock_setting, 
                     agents = list(block_1, noblock_1, block_2))
    state_2c <- list(setting = mock_setting, 
                     agents = list(block_2, noblock_2, block_2))

    # Put them all in a list
    states <- list(state_0a, state_0b, state_0c,
                   state_1a, state_1b, state_1c,
                   state_2a, state_2b, state_2c)

    # Test the function
    tst <- lapply(states, \(x) agents_between_goal(me, x))
    ref <- list(0, 0, 0, 1, 1, 1, 2, 2, 2)

    testthat::expect_equal(tst, ref)
})
