library(ggplot2)
load("precise.rdata")

# Plot illustrating entire data set

no_border <- theme(
 plot.margin = grid::unit(rep(0, 4), "cm"),
 axis.text.x = element_blank(),
 axis.text.y = element_blank(),
 axis.title.x = element_blank(),
 axis.title.y = element_blank(),
 axis.ticks.length = grid::unit(0, "cm"),
 axis.ticks.margin = grid::unit(0, "cm"),
 legend.position = "none"
)

all <- qplot(long, lat, data = precise, shape = I("."), alpha = I(1/10)) + no_border + xlim(min(precise$long), -121.0) + ylim(36.5, max(precise$lat))

ggsave("location-all.png", all, width = 6, height = 6, dpi = 128)
