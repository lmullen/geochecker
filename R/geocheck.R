#' Check geocoded coordinates
#'
#' Given a data frame of geocoded coordinates, check their locations on the map
#' one by one.
#'
#' @param data A data frame containing latitude and longitude coordinates, and
#'   possibly other metadata to identify the geocoded place.
#'
#' @export
geocheck <- function(data, latitude = NULL, longitude = NULL,
                     checked = "checked") {

  stopifnot(is.data.frame(data))
  corrected_point_coords <- NULL
  if (is.null(latitude)) latitude <- guess_lat(colnames(data))
  if (is.null(longitude)) longitude <- guess_lng(colnames(data))

  if (!is.null(checked)) {
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
  }

  ui <- miniUI::miniPage(
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
      shiny::numericInput("current", "Current row", width = 100,
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
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(color = "black", stroke = TRUE,
                                  weight = 2, opacity = 1,
                                  fillColor = fill_color(df, checked),
                                  fillOpacity = 1,
                                  group = "data_to_check",
                                  lat = lat, lng = lng) %>%
        leaflet::addPopups(popup = get_popup(df),
                           lat = lat, lng = lng,
                           group = "data_to_check")
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp(data)
    })

    shiny::observeEvent(input$previous, {
      if (input$current == 1) {
        shiny::updateNumericInput(session, "current", value = nrow(data))
      } else {
        shiny::updateNumericInput(session, "current", value = input$current - 1)
      }
    })

    shiny::observeEvent(input$skip, {
      if (input$current == nrow(data)) {
        shiny::updateNumericInput(session, "current", value = 1)
      } else {
        shiny::updateNumericInput(session, "current", value = input$current + 1)
      }
    })

    shiny::observeEvent(input$mark_correct, {
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
                                  fillColor = "blue", fillOpacity = 1,
                                  lat = input$map_click$lat,
                                  lng = input$map_click$lng,
                                  group = "corrected_point")
    })

    shiny::observeEvent(input$move, {
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
                                  fillOpacity = 1,
                                  group = "data_to_check",
                                  lat = lat, lng = lng) %>%
        leaflet::addPopups(popup = get_popup(df),
                           lat = lat, lng = lng)

    })


  }

  shiny::runGadget(ui, server)

}