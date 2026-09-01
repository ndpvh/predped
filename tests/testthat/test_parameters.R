
# TO DO:
#   - Create tests for probit and normal transformation

testthat::test_that("Loading parameters works", {
    # List all files in the data-directory and select those of relevance
    files <- list.files(file.path("data"))
    files <- files[grepl("parameters__", files, fixed = TRUE)]

    # Create the reference. Is a reduced version consisting of only a few 
    # archetypes
    ref_full <- params_from_csv
    ref_part <- params_from_csv

    archetypes <- c("BaselineEuropean", "BigRushingDutch", "Friends")
    ref_part$params_archetypes <- ref_part$params_archetypes[ref_part$params_archetypes$name %in% archetypes, ]
    ref_part$params_sigma <- ref_part$params_sigma[archetypes]

    # Create a comparison function building on those targets we are interested 
    # in. We don't use identical here because it's too strict, but just examine
    # the information relevant to make predped work
    compare <- function(target, reference) {
        output <- logical(8)

        # Compare the params_archetypes. Here, column names and values should 
        # match
        i <- "params_archetypes"
        output[1] <- all(colnames(reference[[i]]) == colnames(target[[i]]))
        output[2] <- all(reference[[i]] == target[[i]])

        # Compare the params_sigma. Here, we need to look at the values, 
        # names of the lists, rownames, and column names
        i <- "params_sigma"
        output[3] <- all(names(reference[[i]]) == names(target[[i]]))
        output[4] <- sapply(names(reference[[i]]), 
                            function(x) all(reference[[i]][[x]] == target[[i]][[x]])) |>
            all()
        output[5] <- sapply(names(reference[[i]]), 
                            function(x) all(rownames(reference[[i]][[x]]) == rownames(target[[i]][[x]]))) |>
            all()
        output[6] <- sapply(names(reference[[i]]), 
                            function(x) all(colnames(reference[[i]][[x]]) == colnames(target[[i]][[x]]))) |>
            all()

        # Compare the params_bounds. Here, we look at the values and the 
        # rownames
        i <- "params_bounds"
        output[7] <- all(rownames(reference[[i]]) == rownames(target[[i]]))
        output[8] <- all(reference[[i]] == target[[i]])

        # Return the result
        return(all(output))
    }



    #####################################################################
    # Rda

    # Define the files to be read in
    rda_files <- files[grepl(".Rda", files, fixed = TRUE)]

    # Define the expectations: Whenever params_bounds is the only thing, then 
    # we assume the full reference, otherwise we assume only a part of the 
    # full parameter set
    refs <- lapply(rda_files, 
                   function(x) {
                       if(x %in% c("parameters__bounds.Rda", "parameters__list_bounds.Rda")) {
                           return(ref_full)
                       } else {
                           return(ref_part)
                       }
                   })
    
    # Loop over each of these files and perform the checks
    tst <- lapply(seq_along(rda_files), 
                  function(i) compare(load_parameters(file.path("data", rda_files[i])),
                                      refs[[i]])) |>
        as.logical()
    testthat::expect_true(all(tst))



    #####################################################################
    # Rds

    # Define the files to be read in
    rds_files <- files[grepl(".Rds", files, fixed = TRUE)]

    # Define the expectations: Whenever params_bounds is the only thing, then 
    # we assume the full reference, otherwise we assume only a part of the 
    # full parameter set
    refs <- lapply(rds_files, 
                   function(x) {
                       if(x %in% c("parameters__bounds.Rds", "parameters__list_bounds.Rds")) {
                           return(ref_full)
                       } else {
                           return(ref_part)
                       }
                   })
    
    # Loop over each of these files and perform the checks
    tst <- lapply(seq_along(rds_files), 
                  function(i) compare(load_parameters(file.path("data", rds_files[i])),
                                      refs[[i]])) |>
        as.logical()
    testthat::expect_true(all(tst))



    #####################################################################
    # csv

    # Define the files to be read in
    csv_files <- files[grepl(".csv", files, fixed = TRUE)]
    
    # Define the expectations: Whenever params_bounds is the only thing, then 
    # we assume the full reference, otherwise we assume only a part of the 
    # full parameter set
    refs <- lapply(csv_files, 
                   function(x) {
                       if(x == "parameters__bounds.csv") {
                           return(ref_full)
                       } else {
                           return(ref_part)
                       }
                   })
    
    # Loop over each of these files and perform the checks
    tst <- lapply(seq_along(csv_files), 
                  function(i) compare(load_parameters(file.path("data", csv_files[i])),
                                      refs[[i]])) |>
        as.logical()
    testthat::expect_true(all(tst))



    #####################################################################
    # txt

    # Define the files to be read in
    txt_files <- files[grepl(".txt", files, fixed = TRUE)]
    
    # Define the expectations: Whenever params_bounds is the only thing, then 
    # we assume the full reference, otherwise we assume only a part of the 
    # full parameter set
    refs <- lapply(txt_files, 
                   function(x) {
                       if(x == "parameters__bounds.txt") {
                           return(ref_full)
                       } else {
                           return(ref_part)
                       }
                   })
    
    # Loop over each of these files and perform the checks
    tst <- lapply(seq_along(txt_files), 
                  function(i) compare(load_parameters(file.path("data", txt_files[i])),
                                      refs[[i]])) |>
        as.logical()
    testthat::expect_true(all(tst))
})

testthat::test_that("Get parameters works", {
    ref <- params_from_csv
    be <- ref$params_archetypes[ref$params_archetypes$name == "BaselineEuropean", ]

    m_new <- ref$params_archetypes[ref$params_archetypes$name == "BigRushingDutch", ]
    s_new <- diag(nrow(ref$params_sigma$BaselineEuropean))
    b_new <- ref$params_bounds * 2
    
    #####################################################################
    # CASE 1: Default, no file provided so predped defaults used

    params <- get_parameters()

    testthat::expect_equal(names(params), 
                           c("mean", "Sigma", "bounds"))
    testthat::expect_equal(params$mean, be)
    testthat::expect_equal(params$Sigma, 
                           ref$params_sigma$BaselineEuropean)
    testthat::expect_equal(params$bounds, 
                           ref$params_bounds)

    #####################################################################
    # CASE 2: Mean provided, rest is not

    params <- get_parameters(mean = m_new) |>
        suppressWarnings()

    # Test the presence of warnings: Note that there are multiple warnings, so
    # we need to use suppressWarnings() to avoid the test failing
    testthat::expect_warning(get_parameters(mean = m_new)) |>
        suppressWarnings()

    # Test the output
    testthat::expect_equal(params$mean, m_new)
    testthat::expect_equal(params$Sigma, 
                           ref$params_sigma$BaselineEuropean)
    testthat::expect_equal(params$bounds, 
                           ref$params_bounds)

    #####################################################################
    # CASE 3: Covariance provided, rest is not

    params <- get_parameters(Sigma = s_new) |>
        suppressWarnings()

    # Test the presence of warnings: Note that there are multiple warnings, so
    # we need to use suppressWarnings() to avoid the test failing
    testthat::expect_warning(get_parameters(Sigma = s_new))|>
        suppressWarnings()

    # Test the output
    testthat::expect_equal(params$mean, be)
    testthat::expect_equal(params$Sigma, 
                           s_new)
    testthat::expect_equal(params$bounds, 
                           ref$params_bounds)

    #####################################################################
    # CASE 4: Bounds provided, rest is not

    params <- get_parameters(bounds = b_new) |>
        suppressWarnings()

    # Test the presence of warnings: Note that there are multiple warnings, so
    # we need to use suppressWarnings() to avoid the test failing
    testthat::expect_warning(get_parameters(bounds = b_new))|>
        suppressWarnings()

    # Test the output
    testthat::expect_equal(params$mean, be)
    testthat::expect_equal(params$Sigma, 
                           ref$params_sigma$BaselineEuropean)
    testthat::expect_equal(params$bounds, 
                           b_new)
})

testthat::test_that("Generating parameters works", {
    # Extract bounds. Is going to be useful in these tests.
    bounds <- params_from_csv$params_bounds

    # Use generate_parameters to generate 100 parameter sets with a variance of 
    # 1, making sure all are drawn from about a uniform distribution
    set.seed(1)
    params <- generate_parameters(1000,
                                  Sigma = diag(nrow(bounds)) |>
                                    `rownames<-` (rownames(bounds)) |> 
                                    `colnames<-` (rownames(bounds))) |>
        suppressWarnings()

    # Make sure all utility parameters are in the data.frame
    testthat::expect_equal(colnames(params), 
                           rownames(bounds))

    # Make sure all parameters fall within their bound
    tst <- sapply(rownames(bounds), 
                  function(x) all(params[, x] <= bounds[x, 2] & 
                                  params[, x] >= bounds[x, 1]))
    testthat::expect_true(all(tst))
})

testthat::test_that("Plotting the distributions for the parameters works", {
    # Extract bounds. Is going to be useful in these tests.
    bounds <- params_from_csv$params_bounds

    # Check whether the output is a list when asked for
    plt <- plot_distribution(100, as_list = TRUE) |>
        suppressMessages()

    testthat::expect_true(is.list(plt))
    testthat::expect_equal(names(plt), rownames(bounds))
    testthat::expect_true(all(sapply(plt, function(x) ggplot2::is_ggplot(x))))

    # Check whether the output is a ggplot object
    plt <- plot_distribution(100, as_list = FALSE) |>
        suppressMessages()

    testthat::expect_true(ggplot2::is_ggplot(plt))
})
