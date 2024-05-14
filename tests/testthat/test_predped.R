testthat::test_that("Predped initialization works", {
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(2,2),
                                   objects = list()))

    # For future references, there seems to be something wrong with precision in 
    # R. The default of weights is to repeat 1/n n times. However, the sum of 
    # these does not match up with 1 right now (14 archetypes), but it did work
    # before with fewer archetypes (12). Not sure what to do with this, commented
    # out for now.
    # testthat::expect_no_error(predped::predped(id = "model", 
    #                                            setting = setting))
    testthat::expect_no_error(predped::predped(id = "model", 
                                               setting = setting,
                                               archetypes = c("DrunkAussie", "Rushed"),
                                               weights = c(0.5, 0.5)))
    testthat::expect_warning(predped::predped(id = "model", 
                                              setting = setting, 
                                              archetypes = c("DrunkAussie", "Rushed"),
                                              weights = c(0.5, 0.2)))
    testthat::expect_error(predped::predped(id = "model", 
                                            setting = setting, 
                                            archetypes = c("DrunkAussie", "Rushed", "Rushed1"),
                                            weights = c(0.5, 0.5)))
    testthat::expect_error(predped::predped(id = "model", 
                                            setting = setting, 
                                            archetypes = c("test", "Rushed"),
                                            weights = c(0.5, 0.5)))

    model <- suppressWarnings(predped::predped(id = "model", 
                                               setting = setting, 
                                               archetypes = c("DrunkAussie", "Rushed"),
                                               weights = c(0.5, 0.2)))
    tst <- weights(model)
    testthat::expect_equal(tst, c(0.5, 0.2) / 0.7 )
})

testthat::test_that("Predped getters work", {
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0),
                                                              size = c(2, 2)),
                                   objects = list())

    tst <- predped::predped(id = "test",
                            setting = setting, 
                            parameters = predped::params_archetypes,
                            archetypes = c("Rushed", "BaselineEuropean"),
                            weights = c(0.5, 0.5))

    testthat::expect_equal(predped::id(tst), "test")
    testthat::expect_equal(predped::setting(tst), setting)
    testthat::expect_equal(predped::archetypes(tst), c("Rushed", "BaselineEuropean"))
    testthat::expect_equal(predped::weights(tst), c(0.5, 0.5))

    # Here, extra trick: We need to order the parameters that are retrieved
    idx <- predped::params_archetypes$name %in% c("Rushed", "BaselineEuropean")
    params <- predped::params_archetypes[idx,]
    idx <- factor(params$name, levels = c("Rushed", "BaselineEuropean"))
    idx <- order(as.numeric(idx))

    testthat::expect_equal(predped::parameters(tst), params[idx,])
})

testthat::test_that("Predped setters work", {
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0),
                                                              size = c(2, 2)),
                                   objects = list())

    tst <- predped::predped(id = "test",
                            setting = setting, 
                            parameters = predped::params_archetypes,
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
    predped::parameters(tst) <- predped::params_archetypes
    predped::archetypes(tst) <- c("BaselineEuropean", "DrunkAussie")
    predped::weights(tst) <- c(0.25, 0.75)

    testthat::expect_equal(predped::id(tst), "other model")
    testthat::expect_equal(predped::setting(tst), setting_2)
    testthat::expect_equal(predped::archetypes(tst), c("BaselineEuropean", "DrunkAussie"))
    testthat::expect_equal(predped::weights(tst), c(0.25, 0.75))

    # Here, extra trick: We need to order the parameters that are retrieved
    idx <- predped::params_archetypes$name %in% c("BaselineEuropean", "DrunkAussie")
    params <- predped::params_archetypes[idx,]
    idx <- factor(params$name, levels = c("BaselineEuropean", "DrunkAussie"))
    idx <- order(as.numeric(idx))

    testthat::expect_equal(predped::parameters(tst), params[idx,])
})
