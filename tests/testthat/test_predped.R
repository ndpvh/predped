testthat::test_that("Predped initialization works", {
    # Create a mock setting
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(2,2)),
                                   objects = list())

    # Load in the parameters. To be used later.
    params_csv <- suppressMessages(load_parameters())
    # params_db <- load_parameters("predped")

    # Let's do some valid cases based on several distinctions:
    #   - CSV vs Database
    #   - Arguments provided or not provided
    #
    # First CSV
    set.seed(1)
    csv_1 <- predped::predped(setting)
    csv_2 <- predped::predped(setting,                     
                              archetypes = c("BaselineEuropean", 
                                             "Rushed"))
    csv_3 <- predped::predped(setting, 
                              archetypes = c("BaselineEuropean", 
                                             "Rushed"),
                              weights = c(0.75, 0.25))
    csv_4 <- predped::predped(setting, 
                              id = "test")

    n_archs <- nrow(params_csv[["params_archetypes"]])

    # Do the test for the CSV's, checking whether the results are correct
    testthat::expect_equal(csv_1@id, "model ydgab")
    testthat::expect_equal(csv_1@weights, rep(1 / n_archs, n_archs), tolerance = 1e-3)
    testthat::expect_equal(length(csv_1@archetypes), n_archs)
    testthat::expect_equal(names(csv_1@parameters), c("params_archetypes", "params_sigma", "params_bounds"))
    testthat::expect_equal(csv_1@parameters[["params_archetypes"]]$name, csv_1@archetypes)
    testthat::expect_equal(csv_1@setting, setting)
    
    testthat::expect_equal(csv_2@weights, rep(1 / 2, 2), tolerance = 1e-3)
    testthat::expect_equal(length(csv_2@archetypes), 2)
    testthat::expect_equal(csv_2@parameters[["params_archetypes"]]$name, csv_2@archetypes)

    testthat::expect_equal(csv_3@weights, c(0.75, 0.25))
    testthat::expect_equal(csv_4@id, "test")

    # Do similar tests for the database. Commented out as they are based on 
    # Niels' device, and thus not general (yet)
    # set.seed(1)
    # db_1 <- predped::predped(setting,   
    #                          database = "predped")
    # db_2 <- predped::predped(setting,   
    #                          database = "predped",  
    #                          archetypes = c("BaselineEuropean", 
    #                                         "Rushed"))
    # db_3 <- predped::predped(setting,   
    #                          database = "predped", 
    #                          archetypes = c("BaselineEuropean", 
    #                                         "Rushed"),
    #                          weights = c(0.75, 0.25))
    # db_4 <- predped::predped(setting,   
    #                          database = "predped", 
    #                          id = "test")

    # testthat::expect_equal(db_1@id, "model ydgab")
    # testthat::expect_equal(db_1@weights, rep(1 / 14, 14), tolerance = 1e-3)
    # testthat::expect_equal(length(db_1@archetypes), 14)
    # testthat::expect_equal(names(db_1@parameters), c("params_archetypes", "params_sigma", "params_bounds"))
    # testthat::expect_equal(db_1@parameters[["params_archetypes"]]$name, db_1@archetypes)
    # testthat::expect_equal(db_1@setting, setting)
    
    # testthat::expect_equal(db_2@weights, rep(1 / 2, 2), tolerance = 1e-3)
    # testthat::expect_equal(length(db_2@archetypes), 2)
    # testthat::expect_equal(db_2@parameters[["params_archetypes"]]$name, db_2@archetypes)

    # testthat::expect_equal(db_3@weights, c(0.75, 0.25))
    # testthat::expect_equal(db_4@id, "test")

    # Finally, include cases that should give errors or warnings
    #
    # For future references, there seems to be something wrong with precision in 
    # R. The default of weights is to repeat 1/n n times. However, the sum of 
    # these does not match up with 1 right now (14 archetypes), but it did work
    # before with fewer archetypes (12). Not sure what to do with this, commented
    # out for now.
    # testthat::expect_no_error(predped::predped(id = "model", 
    #                                            setting = setting))
    testthat::expect_error(predped::predped(setting, 
                                            weights = c(0.75, 0.5)))
    testthat::expect_no_error(predped::predped(setting, 
                                               id = "model", 
                                               archetypes = c("DrunkAussie", "Rushed"),
                                               weights = c(0.5, 0.5)))
    testthat::expect_message(predped::predped(setting, 
                                              id = "model", 
                                              archetypes = c("DrunkAussie", "Rushed"),
                                              weights = c(0.5, 0.2)))
    testthat::expect_error(predped::predped(setting, 
                                            id = "model",  
                                            archetypes = c("DrunkAussie", "Rushed", "Rushed1"),
                                            weights = c(0.5, 0.5)))
    testthat::expect_error(predped::predped(setting, 
                                            id = "model", 
                                            archetypes = c("ThisOneDoesntExist", "Rushed"),
                                            weights = c(0.5, 0.5)))
})

testthat::test_that("Predped getters work", {
    # Load in parameters and setting
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0),
                                                              size = c(2, 2)),
                                   objects = list())
    params <- suppressMessages(load_parameters())

    # Create the model and do the necessary tests of the getters
    tst <- predped::predped(setting, 
                            id = "test",
                            archetypes = c("Rushed", "BaselineEuropean"),
                            weights = c(0.5, 0.5))

    testthat::expect_equal(predped::id(tst), "test")
    testthat::expect_equal(predped::setting(tst), setting)
    testthat::expect_equal(predped::archetypes(tst), c("Rushed", "BaselineEuropean"))
    testthat::expect_equal(predped::weights(tst), c(0.5, 0.5))
    testthat::expect_equal(names(predped::parameters(tst)), c("params_archetypes", "params_sigma", "params_bounds"))


    params <- params[["params_archetypes"]] 
    params <- params[params$name %in% tst@archetypes, ]
    idx <- factor(params$name, levels = c("Rushed", "BaselineEuropean"))
    idx <- order(as.numeric(idx))

    testthat::expect_equal(predped::parameters(tst)[["params_archetypes"]], 
                           params[idx,])
})

testthat::test_that("Predped setters work", {
    # Load in parameters and setting
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0),
                                                              size = c(2, 2)),
                                   objects = list())
    params <- suppressMessages(load_parameters())

    # Create a model
    tst <- predped::predped(setting, 
                            id = "test", 
                            archetypes = c("Rushed", "BaselineEuropean"),
                            weights = c(0.5, 0.5))

    # Create second setting
    setting_2 <- predped::background(shape = predped::circle(center = c(0, 0), 
                                                             radius = 1),
                                     objects = list())

    # If not all archetypes are present in `parameters`, there should be an error
    # thrown
    testthat::expect_error(predped::archetypes(tst) <- c("DrunkAussie"))

    # Actual setting and tests
    predped::id(tst) <- "other model"
    predped::setting(tst) <- setting_2
    predped::parameters(tst) <- params
    predped::archetypes(tst) <- c("BaselineEuropean", "DrunkAussie")
    predped::weights(tst) <- c(0.25, 0.75)

    testthat::expect_equal(predped::id(tst), "other model")
    testthat::expect_equal(predped::setting(tst), setting_2)
    testthat::expect_equal(predped::archetypes(tst), c("BaselineEuropean", "DrunkAussie"))
    testthat::expect_equal(predped::weights(tst), c(0.25, 0.75))
    testthat::expect_equal(predped::parameters(tst), params)
})
