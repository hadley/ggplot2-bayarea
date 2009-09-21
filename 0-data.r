library(plyr)
# Generate merged addresses and sales data.

# Read and merge
ad <- read.csv(bzfile("addresses.csv.bz2"), stringsAsFactors = FALSE)

sales <- read.csv(bzfile("house-sales.csv.bz2"), stringsAsFactors = FALSE)
sales$date <- as.POSIXct(strptime(sales$date, "%Y-%m-%d"))
sales$price <- as.numeric(sales$price)
sales <- sales[!is.na(sales$price), ]

geo <- join(sales, ad, by = c("city", "zip", "street"))
save(geo, file = "geo.rdata")

precise_qual <- c(
  "QUALITY_ADDRESS_RANGE_INTERPOLATION", "QUALITY_EXACT_PARCEL_CENTROID",
  "gpsvisualizer")
precise <- subset(geo, quality %in% precise_qual)
save(precise, file = "precise.rdata")

sf <- subset(precise, city == "San Francisco")
save(sf, file = "sf.rdata")

# Summary for time series data -----------------------------------------------

cities <- as.data.frame(table(precise$city))
names(cities) <- c("city", "freq")

# Look at big cities and a couple of interesting ones
big_cities <- subset(cities, freq > 2910 * 2) # 20 sales per week on avg
selected <- c(as.character(big_cities$city), "Mountain View", "Berkeley")

inbig <- subset(geo, city %in% selected & !is.na(date))

bigsum <- ddply(inbig, .(city, date), summarise, 
  n = length(price), 
  price = mean(price, na.rm = TRUE),
 .progress = "text")

save(bigsum, file = "city-summary.rdata")
