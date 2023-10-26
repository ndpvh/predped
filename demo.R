rm(list = ls())
source("RCore/PredictivePedestrian.R")

load("demo.RData")

allp[,c("Name","Color","bCA","bCAlr","bGA","bBA","bID","sPref","bPS","bFL","rU","bS","pReroute")]

allp[,c("Name","Color","sSlow","bWB","dID","dFL","r")]


# The supermarket and a pedestrian
# Facing arrow = black
# Goal arrow = red
# Arrow length tippled for visibility
plotPed(2+state$p, getP(state), state$a, state$r, objects,types=attr(state$pMat,"types"),amult=3)

# Body circle is grey, here black for visibility
plotPed(2+state$p, getP(state), state$a, state$r, objects,types=attr(state$pMat,"types"),amult=3,body_border="black")

# 34 choice options, 3m/s to make options clear
draw_grid(2+state$p[1,], state$v*3, state$a, plotPoints = TRUE)

# Way points
points(pathPoints, pch = 3, lwd = 2, col = "#252525", cex = 1.4)

# Start, end and 200 shopping goals, each agent has 100 items to 
# collect (chosen from 100 randomly generated options). 
points(state$P[[1]][substr(row.names(state$P[[1]]), 1, 1) == "G", ], pch = 16, col = "#CA0020", cex = 2)

# All possible paths between goals are computed, here are a few 
drawGoalPaths(edges,goalPaths=goalPaths,delay=3)


# Different ways of choosing a route

# Greedy path algorithm with best way points
plot_tour(tour10$tour,tour10$goals,objects,entry,exitLow,pathPoints,tourDelay=1)

# Detour (sub-optimal way points, p=.5)
plot_tour(tour10detour$tour,tour10detour$goals,objects,entry,exitLow,pathPoints,tourDelay=1)

# Swap (sub-optimal order, p=.5)
plot_tour(tour10swap$tour,tour10swap$goals,objects,entry,exitLow,pathPoints,tourDelay=1)

# 100 goals (note the greedy choice at the start)
plot_tour(tour100$tour,tour100$goals,objects,entry,exitLow,pathPoints,tourDelay=.1)

# Following simulation have 10% probability of sub-optimal waypoints and order.







# THE FRIDAY NIGHT ALBERT HEIJN CHALLANGE

# First Dutch
# Two baseline
# Drunk Aussie
# Old
plotTrace(Long30_4)

# Near the end of an hour still flowing freely
plotTrace(Long30_4,7150)

# At capacity of 30 after ~8 minutes
plot_stat(Long30_4,"n")

# Takes until 20 minutes before first start to exit
# Red dots are still in after an hour
plot_stat(Long30_4,"time",only_exit=FALSE)

# With a limit of 50 still no blockages at the end
plotTrace(Long50_4,7180)

# Fill up by 12 minutes, ~20 to start exiting, one in for almost an hour!
plot_stat(Long50_4,"n")
plot_stat(Long50_4,"time",only_exit=FALSE)

# With a limit of 70 (the legal limit, ~10m^2/shopper) still no blockages at 
# the end
plotTrace(Long70_4,7150)

# Fill up by 18 minutes, ~22 to start exiting, a few slow ones.
plot_stat(Long70_4,"n")
plot_stat(Long70_4,"time",only_exit=FALSE)

# Friday night, the security guard is overwhelmed and lets in 90 ...

# With a limit of 90 a blockage begins to develop around half way
plotTrace(Long90_4_1reroute,3000)
# Hopelessly stuck
plotTrace(Long90_4_1reroute,7150)

# Unlimited reroutes help but a blockage begins to develop near the end on 
# the top right.
plotTrace(Long90_4,6575)
# and gets worse
plotTrace(Long90_4,7150)

# So who is to blame?

# 1) Who doesn't block the supermarket?
# 2) Who gets through the quickest and avoids getting 
#    lost in the supermarket?






















# Baseline does well
plotTrace(Long90_baseline,7180)
# Old also ok
plotTrace(Long90_old,7180)
# Drunks even better!
plotTrace(Long90_drunk,7180)
# But not the dutch, blockage develops around half way 
plotTrace(Long90_dutch,3750)
# Hopelessly blocked by the end.
plotTrace(Long90_dutch,7180)

# What about getting lost?

























# Baseline quickest and fewest stuck
plot_stat(Long90_baseline,"time",only_exit=FALSE,main="Baseline",ylim=c(0,60))
# Old slower, more lost
plot_stat(Long90_old,"time",only_exit=FALSE,main="Old",ylim=c(0,60))
# Drunk slowest, most lost
plot_stat(Long90_drunk,"time",only_exit=FALSE,main="Drunk",ylim=c(0,60))


# New scenario
# The guard is out with COVID and a lockdown is happening tomorrow, 
# 120 limit!





















# Old begin to block ~ 33 minutes 
plotTrace(Long120_old,4000)
# Fully blocked by ~ 40 minutes
plotTrace(Long120_old,5000)
# Multiply blocked at the end.
plotTrace(Long120_old,7000)


# Baseline closer to failure
plotTrace(Long120_baseline,6500)
# But at least one clears
plotTrace(Long120_baseline,7180)
# Two filling regimes
plot_stat(Long120_baseline,"n")
# Still clearing well
plot_stat(Long120_baseline,"time",only_exit=FALSE,ylim=c(0,60))

# No blockages for the drunks!
plotTrace(Long120_drunk,7180)
# Smooth filling
plot_stat(Long120_drunk,"n")
# But lots lost
plot_stat(Long120_drunk,"time",only_exit=FALSE,ylim=c(0,60))


# What about a mixture, is it better to get rid of 
# a) the self absorbed?
# b) the drunk fish our of water?






















# Both dont block at 90
plotTrace(Long90_3_nodrunk,7180)
plotTrace(Long90_3_nodutch,7180)

# Drunk a little slower and more lost
plot_stat(Long90_3_nodutch,"time",only_exit=FALSE,ylim=c(0,60))
plot_stat(Long90_3_nodrunk,"time",only_exit=FALSE,ylim=c(0,60))

# What about at 120?

























# Not sharing side norms is a bit better than being self-absorbed.

# With dutch start to block ~ 40 mins
plotTrace(Long120_3_nodrunk,4600)
# Fully blocked by end
plotTrace(Long120_3_nodrunk,7180)

# With Aussies starts to block at ~ 45 mins
plotTrace(Long120_3_nodutch,5700)
# Fully blocked by end
plotTrace(Long120_3_nodutch,7180)

# As usual drunks more lost
plot_stat(Long120_3_nodutch,"time",only_exit=FALSE,ylim=c(0,60))
plot_stat(Long120_3_nodrunk,"time",only_exit=FALSE,ylim=c(0,60))





