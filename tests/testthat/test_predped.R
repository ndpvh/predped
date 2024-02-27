testthat::test_that("Predped initialization works", {
    testthat::expect_no_error(predped::predped(id = "model", 
                                               setting = data.frame(0)))
    testthat::expect_no_error(predped::predped(id = "model", 
                                               setting = data.frame(0),
                                               archetypes = c("DrunkAussie", "Rushed"),
                                               weights = c(0.5, 0.5)))
    testthat::expect_error(predped::predped(id = "model", 
                                            setting = data.frame(0), 
                                            archetypes = c("DrunkAussie", "Rushed"),
                                            weights = c(0.5, 0.2)))
    testthat::expect_error(predped::predped(id = "model", 
                                            setting = data.frame(0), 
                                            archetypes = c("DrunkAussie", "Rushed", "Rushed1"),
                                            weights = c(0.5, 0.5)))
    testthat::expect_error(predped::predped(id = "model", 
                                            setting = data.frame(0), 
                                            archetypes = c("test", "Rushed"),
                                            weights = c(0.5, 0.2)))
})