#' Check geocoded coordinates
#'
#' Given a data frame of geocoded coordinates, check their locations on the map
#' one by one. The function will show you each point in the data frame one by
#' one. You can move forward and backwards through the points with the "next"
#' and "previous" buttons, which will not save any changes. If a point is
#' correctly geocoded, then clicking "mark correct" will save that information
#' and move on to the next point. If a point is incorrect, click on the map to
#' identify the correct location. When you have identified the correct location,
#' click "move point" to save the new point. Points which have not been marked
#' correct are shown in red; points which have been marked correct are shown in
#' green; points which are being reassigned are shown in blue.
#'
#' @param data A data frame containing latitude and longitude coordinates, and
#'   possibly other metadata to identify the geocoded place.
#' @param latitude The name of the column containing latitudes. If \code{NULL},
#'   the function will try to guess the correct column.
#' @param longitude The name of the column containing longitudes. If
#'   \code{NULL}, the function will try to guess the correct column.
#' @param checked The name of the column that keeps track of whether a point has
#'   been marked as accurately geocoded or not. If this column already exists,
#'   it must contain logical values. If it does not exist, then it will be
#'   created. A value of \code{NA} means that the row has not been checked, and
#'   a value of \code{TRUE} means that it has been marked as accurate.
#' @param zoom The level of zoom to use when showing each point. A whole number
#'   between \code{0} and \code{18}.
#' @param tile_provider The code for a tile provider. See the leaflet package's
#'   \code{\link[leaflet]{addProviderTiles}} function or the
#'   \href{http://leaflet-extras.github.io/leaflet-providers/preview/}{Leaflet-providers
#'   preview}.
#'
#' @return The original data frame with a new column indicating which values
#'   have been checked with any corrections to latitudes and longitudes.
#'
#' @examples
#' \dontrun{
#' geocheck(geocoded_cities)
#' geocheck(geocoded_cities, zoom = 8,
#'          latitude = "latitude", longitude = "longitude")
#' }
#'
#' @export
geocheck <- function(data, latitude = NULL, longitude = NULL,
                     checked = "checked", zoom = NULL,
                     tile_provider = "Esri.WorldStreetMap") {

  stopifnot(is.data.frame(data))
  if (!is.null(zoom)) stopifnot(0 <= zoom, zoom <= 18)

  corrected_point_coords <- NULL
  if (is.null(latitude)) latitude <- guess_lat(colnames(data))
  if (is.null(longitude)) longitude <- guess_lng(colnames(data))

  # Check that the column doesn't exist or that if it exists it is logical
  stopifnot(is.character(checked),
            length(checked) == 1)
  col_exists <- checked %in% colnames(data)
  if (col_exists && !is.logical(data[[checked]])) {
    stop(paste0("The column for keeping track of corrections already exists\n",
                "but it is not logical."))
  } else if (!col_exists) {
    data[[checked]] <- NA
  }

  ui <- miniUI::miniPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML(
      ".form-group {margin: 6px;}
       .leaflet-popup-content-wrapper,
       .leaflet-popup-tip {background: rgba(255,255,255,0.6); box-shadow: none;}"
    ))),
    shiny::tags$script(type = "text/javascript", "$(document).ready(function() { Shiny.addCustomMessageHandler('showalert', function(message) { alert(message); }); });"),
    miniUI::gadgetTitleBar("Geochecker", left = NULL),
    miniUI::miniContentPanel(padding = 0, scrollable = FALSE,
      leaflet::leafletOutput("map", height = "100%")
    ),
    miniUI::miniButtonBlock(
      shiny::actionButton("previous", "Previous",
                          icon = shiny::icon("arrow-left", lib = "glyphicon")),
      shiny::actionButton("skip", "Next",
                          icon = shiny::icon("arrow-right", lib = "glyphicon")),
      shiny::actionButton("move", "Move point",
                          icon = shiny::icon("edit", lib = "glyphicon")),
      shiny::actionButton("mark_correct", "Mark correct",
                          icon = shiny::icon("ok", lib = "glyphicon")),
      shiny::numericInput("current", NULL, width = 80,
                          value = 1, step = 1, min = 1, max = nrow(data))
    )
  )

  server <- function(input, output, session) {

    current_data <- shiny::reactive({data[input$current, ]})

    output$map <- leaflet::renderLeaflet({
      df <- current_data()
      lat <- df[1, latitude]
      lng <- df[1, longitude]
      map <- leaflet::leaflet(data = df) %>%
        leaflet::addProviderTiles(tile_provider) %>%
        leaflet::addCircleMarkers(color = "black", stroke = TRUE,
                                  weight = 2, opacity = 1,
                                  fillColor = fill_color(df, checked),
                                  fillOpacity = 0.5,
                                  group = "data_to_check",
                                  lat = lat, lng = lng) %>%
        leaflet::addPopups(popup = get_popup(df),
                           lat = lat, lng = lng,
                           group = "data_to_check")
      if (!is.null(zoom)) {
        map <- map %>%
          leaflet::setView(lng, lat, zoom = zoom)
      }
      map
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp(data)
    })

    shiny::observeEvent(input$previous, {
      corrected_point_coords <<- NULL
      if (input$current == 1) {
        shiny::updateNumericInput(session, "current", value = nrow(data))
      } else {
        shiny::updateNumericInput(session, "current", value = input$current - 1)
      }
    })

    shiny::observeEvent(input$skip, {
      corrected_point_coords <<- NULL
      if (input$current == nrow(data)) {
        shiny::updateNumericInput(session, "current", value = 1)
      } else {
        shiny::updateNumericInput(session, "current", value = input$current + 1)
      }
    })

    shiny::observeEvent(input$mark_correct, {
      corrected_point_coords <<- NULL
      data[[input$current, checked]] <<- TRUE
      if (input$current == nrow(data)) {
        shiny::updateNumericInput(session, "current", value = 1)
      } else {
        shiny::updateNumericInput(session, "current", value = input$current + 1)
      }
    })

    shiny::observeEvent(input$map_click, {
      corrected_point_coords <<- input$map_click
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("corrected_point") %>%
        leaflet::addCircleMarkers(color = "black", stroke = TRUE,
                                  weight = 2, opacity = 1,
                                  fillColor = "blue", fillOpacity = 0.5,
                                  lat = input$map_click$lat,
                                  lng = input$map_click$lng,
                                  group = "corrected_point")
    })

    shiny::observeEvent(input$move, {
      if (is.null(corrected_point_coords))
        session$sendCustomMessage(type = "showalert", "Click on the map to set the correct location before moving the point.")
      shiny::req(!is.null(corrected_point_coords))

      data[input$current, latitude] <<- corrected_point_coords$lat
      data[input$current, longitude] <<- corrected_point_coords$lng
      data[input$current, checked] <<- TRUE

      df <- data[input$current, ]
      lat <- df[1, latitude]
      lng <- df[1, longitude]

      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("corrected_point") %>%
        leaflet::clearGroup("data_to_check") %>%
        leaflet::clearPopups() %>%
        leaflet::addCircleMarkers(color = "black", stroke = TRUE,
                                  weight = 2, opacity = 1,
                                  fillColor = fill_color(df, checked),
                                  fillOpacity = 0.5,
                                  group = "data_to_check",
                                  lat = lat, lng = lng) %>%
        leaflet::addPopups(popup = get_popup(df),
                           lat = lat, lng = lng)

    })

  }

  shiny::runGadget(ui, server)

}
