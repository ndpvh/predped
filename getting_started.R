devtools::load_all()

setting <- background(shape = circle(center = c(0, 0), 
                                     radius = 2), 
                      objects = list(rectangle(center = c(0, 0), 
                                               size = c(1, 1), 
                                               interactable = TRUE)),
                      entrance = coordinate(c(-2, 0)),
                      same_exit = TRUE)
plot(setting)

model <- predped(id = "my model", 
                 setting = setting, 
                 archetypes = "BaselineEuropean")

trace <- simulate(model,
                  max_agents = 25, 
                  iterations = 50,
                  plot_live = TRUE)

plots <- plot(trace, 
              trace = TRUE)
