library(ggplot2)
source("~/Documents/r/cache.r")

# Summarise sales by day and city - 17,025 rows
cache_block({
  load("geo.rdata")
  cities <- as.data.frame(table(geo$city))
  names(cities) <- c("city", "freq")
  big_cities <- subset(cities, freq > 2910 * 2) # 20 sales per week on avg

  # Only look at big cities, plus a couple of interesting ones
  selected <- c(as.character(big_cities$city), "Mountain View", "Berkeley")
  
  inbig <- subset(geo, city %in% selected & !is.na(date))
  rm(geo)
  
  bigsum <- ddply(inbig, .(city, date), summarise, 
    n = length(price), 
    price = mean(price, na.rm = TRUE),
   .progress = "text")
  rm(inbig)
})


ggplot(bigsum, aes(date, price / 1e6)) + 
  geom_line() + 
  facet_wrap(~ city)
ggsave("cities-price.png", width = 8, height = 6, dpi = 128)

# Smoothing ------------------------------------------------------------------

library(mgcv)
smooth <- function(y, x) {
  as.numeric(predict(gam(y ~ s(x), na.action = na.exclude)))
}
index <- function(y, x) {
  y / y[order(x)[1]]
}

bigsum <- ddply(bigsum, .(city), transform,
  price_s = smooth(price, as.numeric(date)))

ggplot(bigsum, aes(date, price_s / 1e6)) + 
  geom_line() + 
  facet_wrap(~ city)
ggsave(file = "cities-smooth.png", width = 8, height = 6, dpi = 128)

bigsum <- ddply(bigsum, .(city), transform,
  price_si = index(price_s, date))

ggplot(bigsum, aes(date, price_si)) + 
  geom_line() + 
  facet_wrap(~ city) 
ggsave(file = "cities-index.png", width = 8, height = 6, dpi = 128)

# Simplify  ---------------------------------------------------------

covar <- ddply(bigsum, "city", summarise,
  peak = price_si[date == "2006-02-05"],
  plummet = price_si[date == "2008-11-09"]
)

ggplot(covar, aes(peak, plummet)) + 
  geom_point()
ggsave(file = "cities-2d.png", width = 6, height = 6, dpi = 128)

ggplot(covar, aes(peak, plummet)) + 
  geom_point() +
  geom_text(aes(label = city), size = 4, hjust = -0.05)
ggsave(file = "cities-2d-labelled.png", width = 6, height = 6, dpi = 128)


# Try and explain with covariates from the census data ----------------------

covar$delta <- with(covar, plummet - peak)

census <- read.csv("census-city.csv")
covar <- s(covar, census, by = "city")

base <- ggplot(covar, aes(y = delta)) + geom_point()
base + aes(grads)
ggsave(file = "covar-grads.png", width = 6, height = 6, dpi = 128)
base + aes(income)
ggsave(file = "covar-income.png", width = 6, height = 6, dpi = 128)
base + aes(housesold_size)
ggsave(file = "covar-household.png", width = 6, height = 6, dpi = 128)
