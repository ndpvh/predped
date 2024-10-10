
# TO DO:
#   - Create tests for probit and normal transformation

testthat::test_that("Loading parameters works", {
    # local_directory <- file.path(".", "tests", "testthat")
    local_directory <- file.path(".")

    ############################################################################
    # LET'S DO THIS METHODICALLY, I.E. PER TYPE OF EXTENSION




    # Create a universal testing function
    test_function <- function(ref, params, to_check) {
        tst <- lapply(seq_along(ref),
                  function(i) {
                      # Just checking what we have here
                      idx <- to_check[[i]]

                      # Do the actual check
                      return(Reduce("&", 
                                    lapply(idx, 
                                           function(x) {
                                               # Distinguish params_archetypes from lists
                                               if(is.list(ref[[i]]) & !is.data.frame(ref[[i]])) {
                                                   # Distinguish params_sigma from lists
                                                   if(all(names(ref[[i]]) %in% c("BaselineEuropean", "DrunkAussie", "Rushed"))) {
                                                       check <- sapply(names(ref[[i]]), 
                                                                       \(y) all(ref[[i]][[y]] == params[[i]][[x]][[y]]))
                                                   } else {
                                                       # Distinguish params_sigma from others
                                                       if(all(names(ref[[i]][[x]]) %in% c("BaselineEuropean", "DrunkAussie", "Rushed"))) {
                                                           check <- sapply(names(ref[[i]][[x]]), 
                                                                           \(y) all(ref[[i]][[x]][[y]] == params[[i]][[x]][[y]]))
                                                       } else {
                                                           check <- ref[[i]][[x]] == params[[i]][[x]]
                                                       }
                                                   }

                                                   return(all(check))

                                               } else {
                                                   return(all(ref[[i]] == params[[i]][[x]]))
                                               }
                                           })))  
                  })
        return(tst)
    }





    # Rda

    # Define the files to be read in
    files <- c("a_valid", 
               "b_valid", 
               "s_valid", 
               "list_a_valid", 
               "list_b_valid", 
               "list_s_valid", 
               "list_ab_valid", 
               "list_as_valid", 
               "list_sb_valid", 
               "list_all_valid")

    # Define which slots to check for their content per file
    to_check <- list(c("params_archetypes"), 
                     c("params_bounds"), 
                     c("params_sigma"), 
                     c("params_archetypes"), 
                     c("params_bounds"), 
                     c("params_sigma"), 
                     c("params_archetypes", "params_bounds"), 
                     c("params_archetypes", "params_sigma"), 
                     c("params_sigma", "params_bounds"), 
                     c("params_archetypes", "params_sigma", "params_bounds"))

    # Create a reference by reading all of these in
    ref <- lapply(files, 
                  \(x) readRDS(file.path(local_directory, "data", paste0(x, ".Rda"))))
    
    # Test whether you can read everything in
    params <- lapply(files, 
                     \(x) load_parameters(file.path(local_directory, "data", paste0(x, ".Rda"))))

    # First check, are all the components defined
    tst <- lapply(params, 
                  \(x) all(names(x) %in% c("params_archetypes", "params_sigma", "params_bounds")))
    testthat::expect_true(Reduce("&", tst))

    # Now check the content of these components    
    testthat::expect_true(Reduce("&", test_function(ref, params, to_check)))





    # Rds: Same files and checks as Rda

    # Create a reference by reading all of these in
    ref <- lapply(files, 
                  \(x) readRDS(file.path(local_directory, "data", paste0(x, ".Rds"))))
    
    # Test whether you can read everything in
    params <- lapply(files, 
                     \(x) load_parameters(file.path(local_directory, "data", paste0(x, ".Rds"))))

    # First check, are all the components defined
    tst <- lapply(params, 
                  \(x) all(names(x) %in% c("params_archetypes", "params_sigma", "params_bounds")))
    testthat::expect_true(Reduce("&", tst))

    # Now check the content of these components    
    testthat::expect_true(Reduce("&", test_function(ref, params, to_check)))





    # csv

    # Define the files to be read in
    files <- c("a_valid", 
               "b_valid")

    # Define which slots to check for their content per file
    to_check <- list(c("params_archetypes"), 
                     c("params_bounds"))

    # Create a reference by reading all of these in
    ref <- lapply(files, 
                  \(x) read.table(file.path(local_directory, "data", paste0(x, ".csv")), 
                                  sep = ",",
                                  header = TRUE))
    
    # Test whether you can read everything in
    params <- lapply(files, 
                     \(x) load_parameters(file.path(local_directory, "data", paste0(x, ".csv"))))

    # First check, are all the components defined
    tst <- lapply(params, 
                  \(x) all(names(x) %in% c("params_archetypes", "params_sigma", "params_bounds")))
    testthat::expect_true(Reduce("&", tst))

    # Now check the content of these components    
    testthat::expect_true(Reduce("&", test_function(ref, params, to_check)))





    # txt: Uses same files as csv

    # Create a reference by reading all of these in
    ref <- lapply(files, 
                  \(x) read.table(file.path(local_directory, "data", paste0(x, ".txt")), 
                                  sep = ",",
                                  header = TRUE))
    
    # Test whether you can read everything in
    params <- lapply(files, 
                     \(x) load_parameters(file.path(local_directory, "data", paste0(x, ".txt"))))

    # First check, are all the components defined
    tst <- lapply(params, 
                  \(x) all(names(x) %in% c("params_archetypes", "params_sigma", "params_bounds")))
    testthat::expect_true(Reduce("&", tst))

    # Now check the content of these components    
    testthat::expect_true(Reduce("&", test_function(ref, params, to_check)))
})

# testthat::test_that("Parameter exponentiation works", {
#     # BaselineEuropean parameters
#     parameters <- c("rU" = 1, "bS" = 10000, "bWB" = 0, "aWB" = 0, "bFL" = 2,
#                        "aFL" = 2, "dFL" = 0, "bCA" = 1, "bCAlr" = 10, "aCA" = 2,
#                        "bBA" = 4, "aBA" = 2, "bGA" = 10, "aGA" = 2, "bPS" = 2,
#                        "aPS" = 2, "sPref" = 125, "sSlow" = 1, "bID" = 2,
#                        "aID" = 2, "dID" = 0, "Central" = 0, "NonCentral" = 0,
#                        "acc" = 0, "const" = 0, "dec" = 0)
#     reference <- c("rU" = exp(1), "bS" = exp(10000), "bWB" = exp(0), "aWB" = exp(0),
#                       "bFL" = exp(2), "aFL" = exp(2), "dFL" = exp(0), "bCA" = exp(1),
#                       "bCAlr" = exp(10), "aCA" = exp(2), "bBA" = exp(4), "aBA" = exp(2),
#                       "bGA" = exp(10), "aGA" = exp(2), "bPS" = exp(2), "aPS" = exp(2),
#                       "sPref" = exp(125), "sSlow" = exp(1), "bID" = exp(2), "aID" = exp(2),
#                       "dID" = exp(0), "Central" = pnorm(0), "NonCentral" = pnorm(0),
#                       "acc" = pnorm(0), "const" = pnorm(0), "dec" = pnorm(0))

#     transformed_parameters <- predped::transform_exponentiate(parameters)
#     testthat::expect_equal(transformed_parameters, reference)
# })

# testthat::test_that("Parameter logarithmizing works", {
#     # BaselineEuropean parameters
#     parameters <- c("rU" = 1, "bS" = 10000, "bWB" = 0, "aWB" = 0, "bFL" = 2,
#                        "aFL" = 2, "dFL" = 0, "bCA" = 1, "bCAlr" = 10, "aCA" = 2,
#                        "bBA" = 4, "aBA" = 2, "bGA" = 10, "aGA" = 2, "bPS" = 2,
#                        "aPS" = 2, "sPref" = 125, "sSlow" = 1, "bID" = 2,
#                        "aID" = 2, "dID" = 0, "Central" = 0, "NonCentral" = 0,
#                        "acc" = 0, "const" = 0, "dec" = 0)

#     transformed_parameters <- predped::transform_exponentiate(parameters)
#     twice_transformed <- predped::transform_logarithmic(transformed_parameters)

#     # Given that exp(10000) gives back Inf, but the reverse is not possible, we
#     # need to impose that they are equal in both variables to be compared
#     twice_transformed[["bS"]] <- 10000

#     testthat::expect_equal(twice_transformed, parameters)
# })

# testthat::test_that("Nest association parameter transformation works", {
#     # Real parameters inbetween 0 and 1
#     parameters <- c("Central" = 0, "NonCentral" = 0.2, "acc" = 0.4,
#                        "const" = 0.6, "dec" = 0.8)
#     reference <- c("Central" = 1, "NonCentral" = 0.8^(-1), "acc" = 0.6^(-1),
#                       "const" = 0.4^(-1), "dec" = 0.2^(-1))

#     transformed_parameters <- predped::transform_mu(parameters)
#     testthat::expect_equal(transformed_parameters, reference)
# })
