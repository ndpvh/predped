rm(list=ls())
devtools::load_all(".")   # reload source so changes to noiser.R take effect
library(ggplot2)

noise_model <- "independent"   # "independent" or "temporal"
time_step   <- 0.25
cols = c(check="check",time = "time",x= "x",y = "y",cell="cell",
         goal_x="goal_x",goal_y="goal_y",goal_id="goal_id")


# ------------------------------------------------------------------------------
# 1. Define a simple background: 6x6 room with one central obstacle
# ------------------------------------------------------------------------------

room <- rectangle(center = c(0, 0), size = c(6, 6))

obstacle <- rectangle(center = c(0, 0), size = c(1, 1))

bg <- background(
    shape   = room,
    objects = list(obstacle),
    entrance = matrix(c(-3, 0), nrow = 1),
    exit     = matrix(c( 3, 0), nrow = 1)
)

print(plot(bg) + ggtitle("Background"))

# ------------------------------------------------------------------------------
# 2. Run a short simulation
# ------------------------------------------------------------------------------

set.seed(42)

model <- predped(
    setting    = bg,
    archetypes = "BaselineEuropean"
)

trace <- simulate(
    model,
    iterations      = 100,
    time_step       = time_step,
    max_agents      = 4,
    add_agent_after = 5,
    goal_number     = 3,
    plot_live       = TRUE,
    report          = FALSE
)

# Unpack to data.frame
data_clean <- unpack_trace(trace, time_step = time_step)

cat(sprintf("Agents: %d   Rows: %d\n",
            length(unique(data_clean$id)), nrow(data_clean)))

# First 9 agent ids — used to cap the colour legend
legend_ids <- unique(data_clean$id)[seq_len(min(9, length(unique(data_clean$id))))]

# ------------------------------------------------------------------------------
# 3. Plot clean trajectories
# ------------------------------------------------------------------------------

bg_plot <- plot(bg)

p_clean <- bg_plot +
    geom_path(data = data_clean,
              aes(x = x, y = y, colour = factor(id), group = factor(id)),
              linewidth = 0.7, alpha = 0.8, inherit.aes = FALSE) +
    geom_point(data = data_clean[!duplicated(data_clean$id), ],
               aes(x = x, y = y, colour = factor(id)),
               size = 2, shape = 17, inherit.aes = FALSE) +
    scale_colour_discrete(name = "Agent", breaks = legend_ids) +
    coord_fixed() +
    ggtitle("Clean trajectories") +
    theme_minimal()

print(p_clean)

# ------------------------------------------------------------------------------
# 4. Add noise via noiser() — uses the trace path (extracts background
#    automatically and does reachability checking)
# ------------------------------------------------------------------------------

data_noisy <- noiser(
    data       = data_clean,
    .by        = "id",
    background = bg,
    model      = noise_model,
    ntry       = 200,
    cols       = cols
)

# ------------------------------------------------------------------------------
# 5. Plot noisy trajectories
# ------------------------------------------------------------------------------

p_noisy <- bg_plot +
    geom_path(data = data_noisy,
              aes(x = x, y = y, colour = factor(id), group = factor(id)),
              linewidth = 0.7, alpha = 0.8, inherit.aes = FALSE) +
    geom_point(data = data_noisy[!duplicated(data_noisy$id), ],
               aes(x = x, y = y, colour = factor(id)),
               size = 2, shape = 17, inherit.aes = FALSE) +
    scale_colour_discrete(name = "Agent", breaks = legend_ids) +
    coord_fixed() +
    ggtitle(paste0("Noisy trajectories (", noise_model, " model)")) +
    theme_minimal()

print(p_noisy)

# ------------------------------------------------------------------------------
# 6. Side-by-side overlay: clean (solid) vs noisy (dashed) for one agent
# ------------------------------------------------------------------------------

id1 <- unique(data_clean$id)[1]

d_clean_1 <- data_clean[data_clean$id == id1, ]
d_noisy_1 <- data_noisy[data_noisy$id == id1, ]

p_compare <- bg_plot +
    geom_path(data = d_clean_1, aes(x = x, y = y),
              colour = "steelblue", linewidth = 0.8, linetype = "solid",
              inherit.aes = FALSE) +
    geom_path(data = d_noisy_1, aes(x = x, y = y),
              colour = "firebrick", linewidth = 0.8, linetype = "dashed",
              inherit.aes = FALSE) +
    coord_fixed() +
    ggtitle(paste0("Agent ", id1, ": clean (blue) vs noisy (red)")) +
    theme_minimal()

print(p_compare)

# ------------------------------------------------------------------------------
# 7. Quick sanity check: noise magnitude
# ------------------------------------------------------------------------------


shared_cols <- c("id", "time")
merged <- merge(
    data_clean[, c(shared_cols, "x", "y")],
    data_noisy[, c(shared_cols, "x", "y")],
    by = shared_cols, suffixes = c("_clean", "_noisy")
)

merged$dx <- merged$x_noisy - merged$x_clean
merged$dy <- merged$y_noisy - merged$y_clean
merged$dist <- sqrt(merged$dx^2 + merged$dy^2)

for (dim in c("x", "y")) {
    err  <- merged[[paste0(dim, "_noisy")]] - merged[[paste0(dim, "_clean")]]
    ac1  <- cor(err[-length(err)], err[-1])
    cat(sprintf(
        "%s noise — mean: %+.4f m   SD: %.4f m   max: %.4f m   lag-1 autocorr: %.3f\n",
        dim, mean(err), sd(err), max(abs(err)), ac1
    ))
}

head(data_noisy[,cols[-1]])
head(data_clean[,cols[-1]])
