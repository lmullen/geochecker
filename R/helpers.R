# Get all columns of a data frame into a Leaflet popup
get_popup <- function(df) {
  text <- vapply(names(df), function(n) {
    paste("<b>", n, ":</b> ", df[[1, n]], collapse = "")
  }, character(1), USE.NAMES = FALSE)
  paste(text, collapse = "<br>")
}

fill_color <- function(df, checked) {
  checked <- df[[1, checked]]
  ifelse(is.na(checked), "red", "green")
}

# Heavily borrowed from leaflet:::guessLatLongCols
guess_lat <- function(n) {
  possibilities <- n[grep("^(lat|latitude|lats|latitudes)$", n, ignore.case = TRUE)]
  if (length(possibilities == 1)) {
    message(paste0("Using ", possibilities, " for latitude"))
    possibilities
  } else {
    stop("Could not guess latitude column")
  }
}

guess_lng <- function(n) {
  possibilities <- n[grep("^(lon|lng|long|longitude|longs|longitudes)$",
                          n, ignore.case = TRUE)]
  if (length(possibilities == 1)) {
    message(paste0("Using ", possibilities, " for longitude"))
    possibilities
  } else {
    stop("Could not guess longitude column")
  }
}