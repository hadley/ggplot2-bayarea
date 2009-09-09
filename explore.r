library(ggplot2)
load("geo.rdata")

precise <- c(
  "QUALITY_ADDRESS_RANGE_INTERPOLATION", "QUALITY_EXACT_PARCEL_CENTROID")

sf <- subset(geo, city == "San Francisco" & quality %in% precise)
berkeley <- subset(geo, city == "Berkeley" & quality %in% precise)


labels <- list(
  labs(x = NULL, y = NULL),
  opts(axis.text.x = theme_blank()),
  opts(axis.text.y = theme_blank())
)

# Scatterplots ---------------------------------------------------------------

ggplot(sf, aes(long, lat)) + 
  geom_point(size = 0.5, stat = "unique") + 
  labels

# In principle:
ggplot(sf, aes(long, lat)) + 
  geom_point(stat = "sum") + 
  labels

# In practice:
system.time(sfsum <- ddply(sf, c("lat", "long"), summarise,
  n = length(lat), 
  avg_year = mean(year, na.rm = TRUE), 
  .progress = "text"
))


system.time(sfsum <- ddply(sf[, -10], c("lat", "long"), summarise,
  n = length(lat), 
  avg_year = mean(year, na.rm = TRUE), 
  .progress = "text"
))
save(sfsum, file = "sf.rdata")

ggplot(locsum, aes(long, lat, size = n)) + 
  geom_point(alpha = 1/2) + 
  scale_area(to = c(0.3, 6), breaks = c(1, 50, 100, 200, 252)) +
  labels

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
