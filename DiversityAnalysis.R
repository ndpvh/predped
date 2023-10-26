rm(list = ls())
source("RCore/PredictivePedestrian.R")

# 15 second entry

# 1 reroute
Long30_4 <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long30r001.RDS")
Long50_4 <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long50r002.RDS")
Long70_4 <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long70r003.RDS")
Long90_4_1reroute <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long90r004_1reroute.RDS")

# Unlimited reroutes
Long90_4 <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long90r004.RDS")

Long90_baseline <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long90r006.RDS")
Long90_dutch <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long90r007.RDS")
Long90_old <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long90r008.RDS")
Long90_drunk <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long90r0011.RDS")

# Mix 3
Long90_3_nodrunk <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long90r005_nodrunk.RDS")
Long90_3_nodutch <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long90r005.RDS")

# 120 limit
Long120_baseline <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long120r0014.RDS")
Long120_drunk <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long120r0015.RDS")
Long120_old <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long120r0016.RDS")

# Mix 3
Long120_3_nodrunk <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long120r0013.RDS")
Long120_3_nodutch <- readRDS("~/Documents/Projects/COVID/PredPed/predped_noah/predped/Diversity/Data/3-play/play_Diversity_m01s06Long120r0013_nodutch.RDS")


# Environment demonstration
print(load("Diversity/Data/1-make/make_Diversity_m01.RData"))      # objects etc.
print(load("Diversity/Data/2-stack/stack_Diversity_m01s06.RData")) # stacks

agents <- c(BaselineEuropean=1/4,BigRushingDutch=1/4,DrunkAussie=1/4,CautiousOldEuropen=1/4)
allp <- read.csv("Diversity/Config/agents.csv",stringsAsFactors = FALSE)[1:4,]
agent_names <- names(agents)
types <- setNames(allp[,"Color"],allp[,"Name"])
allp <- allp[allp$Name %in% agent_names,-c(1:2)]
row.names(allp) <- agent_names 
# Transform parameters to real scale, add agent probabilities
p_p <- rbind(p=agents,apply(allp,1,toReal))
# Individual variability
allsd <- read.csv("Diversity/Config/agents_sd.csv",stringsAsFactors = FALSE)[1:4,]
p_pSD <- t(allsd[allsd$Name %in% agent_names,-1])
colnames(p_pSD) <- agent_names 
state <- makeState(gstack=stacks[[1]], p=p_p, pSD=p_pSD, group = 1,types=types)

# The supermarket and a pedestrian
# Facing arrow = black
# Goal arrow = red
# Arrow length tippled for visibility
plotPed(2+state$p, getP(state), state$a, state$r, objects,
        types=attr(state$pMat,"types"),amult=3)
# Body circle is grey, here black for visibility
plotPed(2+state$p, getP(state), state$a, state$r, objects,
        types=attr(state$pMat,"types"),amult=3,body_border="black")

# 34 choice options, 3m/s to make options clear
draw_grid(2+state$p[1,], state$v*3, state$a, plotPoints = TRUE)

# Way points
points(pathPoints, pch = 3, lwd = 2, col = "#252525", cex = 1.4)

# Start, end and 200 shopping goals, each agent has 100 items to 
# collect (chosen from 100 randomly generated options). 
points(state$P[[1]][substr(row.names(state$P[[1]]), 1, 1) == "G", ], 
       pch = 16, col = "#CA0020", cex = 2)
edges <- attributes(stacks[[1]])$replan$edges

drawGoalPaths(edges,delay=5)

# Lots of possible paths between goals
drawGoalPaths(edges,goalPaths=goalPaths)

# Greedy path algorithm with best way points
tour10 <- getGoalStack(10, pSwap = 0, pDetour = 0,oneWay = NULL,
             exit = list(exitLow, exitHigh)[[1]],
             objects = objects, goalLines = goalLines, 
             goalRectangles = goalRectangles, entry = entry, 
             pathPoints = pathPoints,showTour = TRUE,tourDelay=1)

# Detour (sub-optimal way points)
tour10detour <- getGoalStack(10, pSwap = 0, pDetour = .5,oneWay = NULL,
                     exit = list(exitLow, exitHigh)[[1]],
                     objects = objects, goalLines = goalLines, 
                     goalRectangles = goalRectangles, entry = entry, 
                     pathPoints = pathPoints,showTour = TRUE,tourDelay=2)


# Swap (sub-optimal order)
tour10swap <- getGoalStack(10, pSwap = .5, pDetour = 0,oneWay = NULL,
                     exit = list(exitLow, exitHigh)[[1]],
                     objects = objects, goalLines = goalLines, 
                     goalRectangles = goalRectangles, entry = entry, 
                     pathPoints = pathPoints,showTour = TRUE,tourDelay=2)

# 100 goals (note the greedy choice at the start)
tour100 <- getGoalStack(100, pSwap = 0, pDetour = 0,oneWay = NULL,
                     exit = list(exitLow, exitHigh)[[1]],
                     objects = objects, goalLines = goalLines, 
                     goalRectangles = goalRectangles, entry = entry, 
                     pathPoints = pathPoints,showTour = TRUE,tourDelay=.1)


save(objects,agents,state,allp,edges,goalPaths,pathPoints,entry,exitLow,
     tour10,tour10detour,tour10swap,tour100,file="demo.RData")


# First Dutch
# Two baseline
# Drunk Aussie
# Old
plotTrace(Long30_4)

# Fast forward to near the end
plotTrace(Long30_4,7000)

par(mfrow=c(2,2))
plot_stat(Long30_4,stat="n")
