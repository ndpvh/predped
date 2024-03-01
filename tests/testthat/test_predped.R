testthat::test_that("Predped initialization works", {
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(2,2),
                                   objects = list()))
    testthat::expect_no_error(predped::predped(id = "model", 
                                               setting = setting))
    testthat::expect_no_error(predped::predped(id = "model", 
                                               setting = setting,
                                               archetypes = c("DrunkAussie", "Rushed"),
                                               weights = c(0.5, 0.5)))
    testthat::expect_error(predped::predped(id = "model", 
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
                                            weights = c(0.5, 0.2)))
})