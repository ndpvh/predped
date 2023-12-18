
testthat::test_that("Parameter exponentiation works", {
    # BaselineEuropean parameters
    parameters <- list("rU" = 1, "bS" = 10000, "bWB" = 0, "aWB" = 0, "bFL" = 2, 
                       "aFL" = 2, "dFL" = 0, "bCA" = 1, "bCAlr" = 10, "aCA" = 2,
                       "bBA" = 4, "aBA" = 2, "bGA" = 10, "aGA" = 2, "bPS" = 2, 
                       "aPS" = 2, "sPref" = 125, "sSlow" = 1, "bID" = 2, 
                       "aID" = 2, "dID" = 0, "Central" = 0, "NonCentral" = 0, 
                       "acc" = 0, "const" = 0, "dec" = 0)
    reference <- list("rU" = exp(1), "bS" = exp(10000), "bWB" = exp(0), "aWB" = exp(0), 
                      "bFL" = exp(2), "aFL" = exp(2), "dFL" = exp(0), "bCA" = exp(1), 
                      "bCAlr" = exp(10), "aCA" = exp(2), "bBA" = exp(4), "aBA" = exp(2), 
                      "bGA" = exp(10), "aGA" = exp(2), "bPS" = exp(2), "aPS" = exp(2), 
                      "sPref" = exp(125), "sSlow" = exp(1), "bID" = exp(2), "aID" = exp(2),
                      "dID" = exp(0), "Central" = pnorm(0), "NonCentral" = pnorm(0), 
                      "acc" = pnorm(0), "const" = pnorm(0), "dec" = pnorm(0))

    transformed_parameters <- predped::transform_exponentiate(parameters)    
    testthat::expect_equal(reference, transformed_parameters)
})

testthat::test_that("Parameter logarithmizing works", {
    # BaselineEuropean parameters
    parameters <- list("rU" = 1, "bS" = 10000, "bWB" = 0, "aWB" = 0, "bFL" = 2, 
                       "aFL" = 2, "dFL" = 0, "bCA" = 1, "bCAlr" = 10, "aCA" = 2,
                       "bBA" = 4, "aBA" = 2, "bGA" = 10, "aGA" = 2, "bPS" = 2, 
                       "aPS" = 2, "sPref" = 125, "sSlow" = 1, "bID" = 2, 
                       "aID" = 2, "dID" = 0, "Central" = 0, "NonCentral" = 0, 
                       "acc" = 0, "const" = 0, "dec" = 0)

    transformed_parameters <- predped::transform_exponentiate(parameters)
    twice_transformed <- predped::transform_logarithmic(transformed_parameters)

    # Given that exp(10000) gives back Inf, but the reverse is not possible, we 
    # need to impose that they are equal in both variables to be compared
    twice_transformed[["bS"]] <- 10000

    testthat::expect_equal(twice_transformed, parameters)
})