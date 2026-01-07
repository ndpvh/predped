#' @description
#' Helper functions to allow simulating pedestrian flow with the Minds for 
#' Mobile Agents (M4MA) pedestrian model. Contains functions to generate 
#' environments, adapt pedestrian characteristics, and allow for a wide range of 
#' different kinds of simulation studies.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib predped
#' @import MASS
#' @import methods
#' @import stats
#' @importFrom utils write.table
#' @importFrom utils capture.output
#' @importFrom cppRouting get_path_pair
#' @importFrom dplyr tbl
#' @importFrom dplyr collect
#' @importFrom Rcpp evalCpp
#' @exportPattern "^[[:alpha:]]+"
## usethis namespace: end
NULL
