library(ggplot2)
load("sf.rdata")

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
biggest <- subset(sfsum, n == max(n))
biggest_locs <- subset(sf, lat == biggest$lat & long == biggest$long)

# Histograms -----------------------------------------------------------------

qplot(year, data = sf, binwidth = 10)
ggsave("year-10.png", width = 8, height = 6, dpi = 128)
qplot(year, data = sf, binwidth = 1)
ggsave("year-1.png", width = 8, height = 6, dpi = 128)
qplot(year %% 10, data = sf, binwidth = 1)
ggsave("year-mod-10.png", width = 8, height = 6, dpi = 128)

# Missing values
qplot(factor(br), data = sf)
ggsave("bedrooms.png", width = 8, height = 6, dpi = 128)

# Explore relationship between age and location ------------------------------

ggplot(sfsum, aes(long, lat, colour = avg_year)) + 
  geom_point(alpha = 1/2, size = 2)
ggsave("sf-year.png", width = 6, height = 6, dpi = 128)

# Didn't work so well.  Let's try facetting.  
# Facetting requires a categorical variable, so we'll make that first.

sf$sect <- cut_number(sf$year, n = 5, dig.lab = 4)
qplot(year, data = sf, binwidth = 1, fill = sect)
ggsave("year-cut-6.png", width = 8, height = 6, dpi = 128)

sfsumsect <- ddply(sf, c("lat", "long" , "sect"), "nrow", .progress = "text")

ggplot(sfsumsect, aes(long, lat, size = nrow)) + 
  geom_point(colour = alpha("black", 0.5)) + 
  scale_area(to = c(0.5, 3)) +
  facet_wrap(~ sect)
ggsave("sf-average-year.png", width = 8, height = 6, dpi = 128)
