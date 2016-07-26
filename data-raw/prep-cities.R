library(readr)
cities <- read_csv("data-raw/cities.csv")
coords <- ggmap::geocode(cities$city, output = "more")
write_csv(coords, "data-raw/coords.csv")

# At this point the file is edited by hand to introduce errors
geocoded_cities <- read_csv("data-raw/cities.csv")
devtools::use_data(geocoded_cities)
