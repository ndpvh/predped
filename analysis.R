reroute
replan
close
speed

rm(list=ls())
source("RCore/PredictivePedestrian.R")

# NB: I changed the definition of sSlow to the number of seconds from goal at
#     the current velocity where slowing begins. This automatically adjusts for
#     fast agents, a default of 1sec seems to work ok, but results in the 
#.    occasional circle back for rushed, which might mean you want to make it
#.    a bit bigger.

# Made two new test files with all rushed (LucieRushed3) and mixed (LucieMixed3)
# and note I used a smaller set of agent types, some of my 4 and two of your new 
# ones, see Lucie_agents.csv) in order to demonstrate how the simulation changes 
# over time. First I added (to make it a easier to try variations) sim_name 
# to the play files which allow you to pick out different simulation.csv files,
# and used one called simulation_andrew.csv which does 1000 iterations with an
# entry cycle of 10 iterations (as otherwise my mixed simulations got stuck 
# sometimes), a waiting time of 5 iterations and a maximum of 20 in the simulation.  
rush3 <- readRDS("Diversity/Data/3-play/play_Diversity_m01s01Rushedr003.RDS")
mixed3 <- readRDS("Diversity/Data/3-play/play_Diversity_m01s01Mixedr003.RDS")

# You can replay the simulation. 
plotTrace(rush3)
# From any point
plotTrace(mixed3,start=900)

# This counts the number inside at each iteration. You can see for rushed after 
# ~150 or so it achieves a "steady state" (the number in and number out balances)
# whereas for mixed it takes a 100 more (~250) to get to a steady state which is
# the same as the maximum allowed (20).
plot_stat(rush3,stat="n")
plot_stat(mixed3,stat="n")

# Look again excluding the first 150 from rushed, and you see the steady state  
# at ~8 (the thick black line is the mean and it is also printed to the console).
# This occurs even though the simulation allows 20 in because they are so quick
# they get out before 20 is reached. 
plot_stat(rush3,stat="n",exclude=150)

# For mixed excluding 250 we see the numbers bump up against the upper limit of 20. 
plot_stat(mixed3,stat="n",exclude=250)


# The point here is that when you do your simulations you will probably want to
# focus on the steady state, which will differ for different configurations,
# and you might want to adjust the entry cycles so for different agent make
# up the steady state level is about the same. Otherwise you confound the 
# effects of agent type with the number of agents present.


# Now lets look at time and distance. A further potential confound is whether 
# they had time to exit. If you ignore that as in the next command you see the
# those not exiting (indicated by red circles) are inside for less time (here
# measured in iterations, so halve that value for seconds)
plot_stat(rush3,stat="time",only_exit=FALSE)

# Note that you can return the times by assigning. Often when you do this you 
# dont want a plot just the return
n_rush <- plot_stat(rush3,stat="time",only_exit=FALSE,do_plot=FALSE)
n_rush

# By default plot and return are only for agents who exit
print(plot_stat(rush3,stat="time"))

# Your can also look at results sorted to be increasing to get an idea of range
plot_stat(rush3,stat="time",increasing = TRUE)

# Now lets look at distance traveled for the two cases for only pedestrians
# who exit and who enter only after the steady state is reached (i.e., exclude
# now removes anyone who starts before the value). We can see that the
# average distance traveled is greater in the mixed case.
d_rush <- plot_stat(rush3,stat="distance",exclude=150)
d_mixed <- plot_stat(mixed3,stat="distance",exclude=250)

# There are more included for rushed that mixed as expected
length(d_rush)
length(d_mixed)

# The time difference is much more marked, as expected. 
t_rush <- plot_stat(rush3,stat="time",exclude=150)
t_mixed <- plot_stat(mixed3,stat="time",exclude=250)

# To break this down by types get a vector named for each participant with
# entries giving the type
mtypes <- get_types(mixed3)

# This can then pick out how many of each type are included, not that many each
# as this is a small sample 
table(mtypes[names(t_mixed)])

# With the caveat this is noise we can compare type results
round(type_means(d_rush,rush3),1)
round(type_means(d_mixed,mixed3),1)

# Times are as expected
round(type_means(t_rush,rush3),1)
round(type_means(t_mixed,mixed3),1)

# Results can also be calculated relative to distance measures. For time 
# actual distance traveled is divided by time to get average speed.
s_rush <- plot_stat(rush3,stat="time",exclude=150,relative=TRUE)
s_mixed <- plot_stat(mixed3,stat="time",exclude=250,relative=TRUE)
round(type_means(s_rush,rush3),3)
round(type_means(s_mixed,mixed3),3)

# Note that times include time stopped, so if they have a different number of 
# goals measures involving time are confounded. When stops are removed we see
# the expected reduction in average time by a little more than 5 iterations, 
# reflecting 5 ticks waiting at the single goal and a little extra for occasional
# stops between goals.
nt_rush <- plot_stat(rush3,stat="time",exclude=150,nostop=TRUE)
nt_mixed <- plot_stat(mixed3,stat="time",exclude=250,nostop=TRUE)

# Similarly average speed is a little more
ns_rush <- plot_stat(rush3,stat="time",exclude=150,relative=TRUE,nostop=TRUE)
ns_mixed <- plot_stat(mixed3,stat="time",exclude=250,relative=TRUE,nostop=TRUE)


# We can also look at the planned distance, as expected this is the same
# for all up to variation due to randomness due to small sample size.
pd_rush <- plot_stat(rush3,stat="pdistance",exclude=150)
pd_mixed <- plot_stat(mixed3,stat="pdistance",exclude=250)
round(type_means(pd_rush,rush3),1)
round(type_means(pd_mixed,mixed3),1)

# ... and more interestingly the actual distance traveled divided by planned distance
# NB The ratio can be < 1 due to short cutting way points.
nd_rush <- plot_stat(rush3,stat="distance",exclude=150,relative=TRUE)
nd_mixed <- plot_stat(mixed3,stat="distance",exclude=250,relative=TRUE)
round(type_means(nd_rush,rush3),3)
round(type_means(nd_mixed,mixed3),3)

# Looking at stops (excluding stopped at goal)
stop_rush <- plot_stat(rush3,stat="stop",exclude=150,nostop=TRUE)
stop_mixed <- plot_stat(mixed3,stat="stop",exclude=250,nostop=TRUE)
round(type_means(stop_rush,rush3),3)
round(type_means(stop_mixed,mixed3),3)

# number of close encounters (inter-body distance < stat)
close=.5
close_rush <- plot_stat(rush3,stat=close,exclude=150)
close_mixed <- plot_stat(mixed3,stat=close,exclude=250)
round(type_means(close_rush,rush3),3)
round(type_means(close_mixed,mixed3),3)

# Can also get version of these measures relative to actual distance traveled
# e.g., stops/m
rstop_rush <- plot_stat(rush3,stat="stop",exclude=150,relative=TRUE)
rstop_mixed <- plot_stat(mixed3,stat="stop",exclude=250,relative=TRUE)
round(type_means(rstop_rush,rush3),3)
round(type_means(rstop_mixed,mixed3),3)


close=.5
nclose_rush <- plot_stat(rush3,stat=close,exclude=150,relative=TRUE)
nclose_mixed <- plot_stat(mixed3,stat=close,exclude=250,relative=TRUE)
round(type_means(nclose_rush,rush3),3)
round(type_means(nclose_mixed,mixed3),3)

# For re-routing and re-planning I realized I needed a change in goals.R so
# made that and ran two new simulations. I also took the opportunity to try
# remedy the "stuck at the door" problem. Not entirely sure I succeed so if you
# notice it occurring let me know (and ideally give me your code that made it
# happen).
rush4 <- readRDS("Diversity/Data/3-play/play_Diversity_m01s01Rushedr004.RDS")
mixed4 <- readRDS("Diversity/Data/3-play/play_Diversity_m01s01Mixedr004.RDS")

# Looking at re-routing (cant see next path point/goal) 
reroute_rush <- plot_stat(rush4,stat="reroute",exclude=150)
reroute_mixed <- plot_stat(mixed4,stat="reroute",exclude=250)
round(type_means(reroute_rush,rush4),3)
round(type_means(reroute_mixed,mixed4),3)

# Replan (avoiding a crowd, plan new route)
replan_rush <- plot_stat(rush4,stat="replan",exclude=150)
replan_mixed <- plot_stat(mixed4,stat="replan",exclude=250)
round(type_means(replan_rush,rush4),3)
round(type_means(replan_mixed,mixed4),3)

# The latter result is total replans, this can be broken down into successful replans
replan_rush <- plot_stat(rush4,stat="replanOK",exclude=150)
replan_mixed <- plot_stat(mixed4,stat="replanOK",exclude=250)
round(type_means(replan_rush,rush4),3)
round(type_means(replan_mixed,mixed4),3)

replan_rush <- plot_stat(rush4,stat="replanFAIL",exclude=150)
replan_mixed <- plot_stat(mixed4,stat="replanFAIL",exclude=250)
round(type_means(replan_rush,rush4),3)
round(type_means(replan_mixed,mixed4),3)


# As requested by Ben this function plots the locations where bad things happen
plot_bad(mixed4,"replan")

# Point size can be controlled
plot_bad(mixed4,"reroute",cex=.75)

# Points rather than names can be plotted
plot_bad(mixed4,"stop",plotpoints=TRUE)

# Coordinates can be saved
pstop_mixed4 <- plot_bad(mixed4,"stop",cex=.75)
pturn_mixed4
