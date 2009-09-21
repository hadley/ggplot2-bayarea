library(ggplot2)
load("precise.rdata")

# Plot illustrating entire data set

no_border <- opts(
 plot.margin = unit(rep(0, 4), "cm"),
 axis.text.x = theme_blank(),
 axis.text.y = theme_blank(),
 axis.title.x = theme_blank(),
 axis.title.y = theme_blank(),
 axis.ticks.length = unit(0, "cm"),
 axis.ticks.margin = unit(0, "cm"),
 legend.position = "none"
)

all <- qplot(long, lat, data = precise, shape = I("."), alpha = I(1/10)) + no_border + xlim(min(precise$long), -121.0) + ylim(36.5, max(precise$lat))

ggsave("location-all.png", all, width = 6, height = 6, dpi = 128)
