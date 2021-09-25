#' Create a local health area Geographic Map to visualize data
#'
#' @param lha_data_frame         - data.frame: the data for visualization in the map
#' @param lha_id_column_name     - character : the column name containing the LHA IDs
#' @param colorby_column_name    - character : the column name for initial visualization
#' @param title                  - character : chart title (default = "Data By Local Health Area")
#' @param subtitle               - character : subtitle    (default = colorby_column_name value)
#' @param colors                 - character vector: list of color values to use on the map.
#'                                 can be specified as html names, hex codes, rgb or rgba string values
#'                                 default = c("white", "yellow", "orange", "red", "firebrick")
#' @param color_breaks           - numeric vector: list of locations for the color breaks
#'                                 must be the same length as the colors parameter
#'                                 default = c(0, 25, 50, 75, 100)
#'
#' @return a CanvasXpress plot object
#'
#' @examples
#'
#' \code{#TODO}
#'
#' @import assertthat jsonlite canvasXpress htmlwidgets
#'


create_lha_heatmap <- function(lha_data_frame,
                               lha_id_column_name,
                               colorby_column_name,
                               title        = "Data By Local Health Area",
                               subtitle     = colorby_column_name,
                               colors       = c("white", "yellow", "orange", "red", "firebrick"),
                               color_breaks = c(0, 25, 50, 75, 100)) {
  # check inputs
  assertthat::assert_that(all(!is.null(lha_data_frame),
                              is.data.frame(lha_data_frame),
                              NROW(lha_data_frame) > 0),
                          msg = "The lha_data_frame parameter is invalid (is NULL, NA, not a data frame or has no rows).")

  assertthat::assert_that(all(!is.null(lha_id_column_name),
                              !is.na(lha_id_column_name),
                              is.character(lha_id_column_name),
                              assertthat::has_name(lha_data_frame, lha_id_column_name)),
                          msg = "The lha_id_column_name parameter must be specified and contain a column name present in the data frame.")

  assertthat::assert_that(all(!is.null(colorby_column_name),
                              !is.na(colorby_column_name),
                              is.character(colorby_column_name),
                              assertthat::has_name(lha_data_frame, colorby_column_name)),
                          msg = "The colorby_column_name parameter must be specified and contain a column name present in the data frame.")

  use_colors <- TRUE

  if (is.null(colors) || is.null(color_breaks)) {
    warning('Default coloring will be utilized on the map because colors and/or color_breaks are NULL')
    use_colors <- FALSE
  } else if (length(colors) != length(color_breaks)) {
    warning('Default coloring will be utilized on the map because the length of colors and color_breaks are not the same')
    use_colors <- FALSE
  }

  if (!use_colors) { # use defaults
    colors <- c("white", "yellow", "orange", "red", "firebrick")
    color_breaks <- c(0, 25, 50, 75, 100)
  }

  # lat/long geojson for the shapes of the LHA (see data notes)
  boundaries.lha <- jsonlite::read_json('R/map_LHA_boundaries.json')

  features    <- boundaries.lha$features
  keep        <- c(2, 3, 8, 9) # fields to keep from the original map data
  keep.prefix <- "_"           # pull them to the top of the sorted list on the map

  # check that at least some of the IDs are present in the map
  # test using the first map element, all elements have the same property set)
  test <- boundaries.lha$features[[1]]$properties$LOCAL_HLTH_AREA_CODE
  if (length(lha_data_frame[, lha_id_column_name] %in% test) == 0) {
    warning("None of the specified IDs are present in the LHA IDs.  Map cannot be created")
    return(NULL)
  }

  features.data <- lapply(features, FUN = function(x) {
    lha                 <- x$properties$LOCAL_HLTH_AREA_CODE
    new_data            <- lha_data_frame[which(lha_data_frame[, lha_id_column_name] == lha), ]
    x$properties        <- x$properties[keep]
    names(x$properties) <- paste0(keep.prefix, names(x$properties))
    x$properties        <- append(x$properties, new_data[1, ])
    x
  })
  boundaries.lha$features <- features.data

  # custom tooltip event for the chart
  events <- htmlwidgets::JS("{
        'mousemove' : function(o, e, t) {
                          if (o) {
                              t.showInfoSpan(e, '<b>' + this.colorBy + ': ' + o.z[this.colorBy] +
                                                '</b><hr/>' + o.z._LOCAL_HLTH_AREA_NAME[0] +
                                                '<i>(' + o.z._LOCAL_HLTH_AREA_CODE[0] + ')</i><br/>' +
                                                'Health Authority: ' + o.z._HLTH_AUTHORITY_NAME[0] +
                                                ' <i>(' + o.z._HLTH_AUTHORITY_CODE[0] + ')</i>');
                          };
        }}")

  canvasXpress::canvasXpress(
    data                    = FALSE,
    graphType               = "Map",
    title                   = title,
    subtitle                = subtitle,
    titleScaleFontFactor    = 0.5,
    subtitleScaleFontFactor = 0.3,
    legendPosition          = "bottom",
    colorSpectrum           = colors,
    colorSpectrumBreaks     = color_breaks,
    topoJSON                = boundaries.lha,
    colorBy                 = colorby_column_name,
    citation                = "Note: Hover over chart areas to view detailed information",
    citationScaleFontFactor = 2,
    events                  = events
  )
}
