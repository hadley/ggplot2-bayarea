library(ggplot2)
load("geo.rdata")

precise_qual <- c(
  "QUALITY_ADDRESS_RANGE_INTERPOLATION", "QUALITY_EXACT_PARCEL_CENTROID",
  "gpsvisualizer")

precise <- subset(geo, quality %in% precise_qual)

labels <- list(
  labs(x = NULL, y = NULL),
  opts(axis.text.x = theme_blank()),
  opts(axis.text.y = theme_blank())
)

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

# Plot illustrating entire data set

all <- qplot(long, lat, data = precise, shape = I("."), alpha = I(1/10)) + no_border + xlim(min(precise$long), -121.0) + ylim(36.5, max(precise$lat))
ggsave("location-all.png", all, width = 6, height = 6, dpi = 128)

sf <- subset(precise, city == "San Francisco")
berkeley <- subset(precise, city == "Berkeley")


# Scatterplots ---------------------------------------------------------------

ggplot(sf, aes(long, lat)) + 
  geom_point(size = 0.5, stat = "unique")
ggsave("sf.png", width = 6, height = 6, dpi = 128)

# In principle:
ggplot(sf, aes(long, lat)) + 
  geom_point(stat = "sum")

# In practice:  (takes 42 seconds)
sfsum <- ddply(sf, c("lat", "long"), summarise,
  n = length(lat), 
  avg_year = mean(year, na.rm = TRUE), 
  .progress = "text"
)
ggplot(sfsum, aes(long, lat, size = n)) + 
  geom_point(alpha = 1/2) + 
  scale_area(to = c(0.3, 6), breaks = c(1, 50, 100, 200, 250, 252))
ggsave("sf-sum.png", width = 6, height = 6, dpi = 128, drop = "legend_box")
ggsave("sf-sum-leg.png", width = 1, height = 2, dpi = 128, keep = "legend_box")

biggest <- subset(sfsum, n == max(n))
biggest_locs <- subset(sf, lat == biggest$lat & long == biggest$long)

# Histograms -----------------------------------------------------------------

qplot(year, data = sf, binwidth = 10)
qplot(year, data = sf, binwidth = 1)

# Missing values
qplot(factor(br), data = sf)

# Explore relationship between age and location ------------------------------

sf$sect <- cut_number(sf$year, n = 6)
qplot(year, data = sf, binwidth = 1, fill = sect)

ggplot(subset(sf, !is.na(year)), aes(long, lat, size = n)) + 
  geom_point(stat = "unique", colour = alpha("black", 0.5)) + 
  scale_area(to = c(0.3, 4), breaks = c(1, 50, 100, 200, 252)) +
  labels + facet_wrap(~ sect)

ggplot(locsum, aes(long, lat, colour = avg_year)) + 
  geom_point(alpha = 1/2, size = 2) + 
  labels
ggplot(locsum, aes(long, lat, colour = avg_year)) + 
  geom_point(alpha = 1 / 4, size = 3) +  
  scale_colour_gradient(low = "yellow", high = "blue") + 
  labels

# interactive


# Time series ----------------------------------------------------------------


sf$decade <- round_any(sf$year, 10, floor)
sf$month <- update(sf$date, mday = 0)

sales <- ddply(sf, c("decade","month"), nrow)

qplot(month, nrow, data = sales)
