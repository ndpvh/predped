##############################################################################-
# This file loads all packages and functions.
##############################################################################-


# Packages ----------------------------------------------------------------

require(parallel)
library(cppRouting)
library(TSP)
library(igraph)
require(pdftools)
require(magick)
# devtools::install_github('m4ma/m4ma') or 
# remotes::install_github('m4ma/m4ma') on SURF research cloud
library(m4ma)


# Functions ---------------------------------------------------------------

source("RCore/pp_objects.R")
source("RCore/pp_environment.R")

source("RCore/pp_geometry.R")       # geometry functions
source("RCore/pp_plot.R")           # plotting functions
# source("RCore/pp_see.R")          # line intersection and "sees" functions
source("RCore/pp_predict.R")        # prediction functions
# source("RCore/pp_utility_extra.R")  # pre-compute functions for utility
source("RCore/pp_utility.R")        # utility functions
source("RCore/pp_parameter.R")      # parameter management functions
source("RCore/pp_dcm.R")            # DCM functions
source("RCore/pp_route.R")          # routing and tour functions
source("RCore/pp_goals.R")          # goal management functions
# source("RCore/pp_block.R")        # object blocking functions
source("RCore/pp_block_r.R")        # object blocking functions
source("RCore/pp_collide.R")        # fix collisions functions
source("RCore/pp_simulate.R")       # iterate simulation functions
source("RCore/pp_flow.R")           # flow metrics functions
source("RCore/pp_estimation.R")     # estimation functions

