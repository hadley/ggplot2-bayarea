library(ggplot2)
library(mgcv)
theme_set(theme_bw())

# Select the biggest cities in terms of numbers of sales ---------------------
cities <- as.data.frame(table(geo$city))
names(cities) <- c("city", "freq")
big_cities <- subset(cities, freq > 2910) # 10 sales per week on avg

# Only look at houses in big cities, reduces records to ~ 420,000
inbig <- subset(geo, city %in% big_cities$city)

# Summarise sales by day and city - 17,025 rows
if (file.exists("bigsum.rdata")) {
  load("bigsum.rdata") 
} else {
  bigsum <- ddply(inbig, .(city, date), function(df) {
    data.frame(
      n = nrow(df), 
      avg = mean(df$price, na.rm = T)
    ) 
  }, .progress = "text")
  save(bigsum, file = "bigsum.rdata")
}

qplot(date, avg / 1e6, data = bigsum, geom = "line", alpha = I(1/3), 
  group = city, ylab="Average sale price (millions)", xlab = NULL)
ggsave("cities-price.png", width = 8, height = 3, dpi = 128)

# Smoothing ------------------------------------------------------------------

smooth <- function(y, x) as.numeric(predict(gam(y ~ s(x), na.action = na.exclude)))
index <- function(y, x) {
  y / y[order(x)[1]]
}

library(mgcv)
bigsum <- ddply(bigsum, .(city), transform,
  price_s = exp(smooth(log(avg), as.numeric(date))))

qplot(date, price_s / 1e6, data = bigsum, geom = "line", alpha = I(1/3), 
  group = city, ylab="Average sale price (millions)", xlab = NULL)
ggsave(file = "cities-smooth.png", width = 8, height = 4, dpi = 128)

bigsum <- ddply(bigsum, .(city), transform,
  price_si = index(price_s, date))
qplot(date, price_si, data = bigsum, geom = "line", alpha = I(1/3), 
  group = city, ylab="Change in value", xlab = NULL)
ggsave(file = "cities-index.png", width = 8, height = 4, dpi = 128)

ggplot(bigsum, aes(date, price_si)) +
  geom_line(alpha = 1/3) + 
  facet_wrap(~ city) +
  
  scale_x_date(major = "2 years", minor = "6 months", format = "%y") + 
  ylab("Change in value") +
  xlab(NULL)


# Data manipulation ---------------------------------------------------------

covar <- ddply(bigsum, "city", summarise,
  peak = price_si[date == "2006-02-05"],
  plummet = price_si[date == "2008-11-09"]
)
covar$delta <- with(covar, plummet - peak)
qplot(delta, reorder(city, delta), data = covar)

# Try and explain with covariates from the census data ----------------------

census <- read.csv("census-city.csv")
explain <- merge(covar, census, by = "city")

# Most affected have:
#   * lower incomes
#   * fewer bachelors degrees
#   * more babies & children
#   * bigger households
#   * longer commutes
#   * fewer firms per capita
#   * more multiracial people
qplot(income, price_drop, data = covar)
ggsave(file = "beautiful-data/graphics/cities-income.pdf", width = 6, height = 6)
qplot(grads, price_drop, data = covar)
ggsave(file = "beautiful-data/graphics/cities-grads.pdf", width = 6, height = 6)
qplot(babies, price_drop, data = covar)
qplot(children, price_drop, data = covar)
qplot(housesold_size, price_drop, data = covar)
qplot(commute, price_drop, data = covar)
ggsave(file = "beautiful-data/graphics/cities-commute.pdf", width = 6, height = 6)
qplot(firms / pop, price_drop, data = covar)
qplot(multir, price_drop, data = covar)
