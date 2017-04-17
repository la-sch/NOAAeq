# module 1 ====

#' Clean location names
#'
#' This function strips out the country name (including the colon)
#' and converts names to title case (as opposed to all caps).
#'
#' @param loc A character string or vector of character strings indicating the location.
#'   The country must be separated from the location name by a colon.
#'
#' @return A character string or vector of character strings wih cleaned location names.
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' eq_location_clean(c("MEXICO:  MICHOACAN",
#'                     "SWITZERLAND:  BASEL",
#'                     "THAILAND:  BANGKOK"))
#'
#' @export
eq_location_clean <- function(loc) {
  stringr::str_sub(loc,
    stringr::str_locate(loc, ":  ")[,"end"] + 1,
    -1) %>%
    stringr::str_to_title()
}

#' Clean data
#'
#' This function unites temporal information in a DATE column,
#' converts the spatial coordinates (LONGITUDE, LATITUDE),
#' the earthquake magnitude (EQ_PRIMARY),
#' and the number of deaths (TOTAL_DEATHS) to numeric,
#' cleans the LOCATION_NAME by using the \code{eq_location_clean} function,
#' and discards several columns not needed for the analysis of earthquakes in this package.
#'
#' @param raw A data frame or tibble with the raw data.
#'   The country must be separated from the location name by a colon.
#'
#' @return A data frame or tibble with the cleaned data.
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' file <- system.file("extdata", "earthquakes.tsv.gz", package = "NOAAeq")
#' readr::read_delim(file = file, delim = "\t") %>%
#'   eq_clean_data()
#'
#' @export
eq_clean_data <- function(raw) {
  raw %>%
    dplyr::mutate(CE = as.factor(ifelse(YEAR >= 0, "CE", "BCE")),
                  YEAR = ifelse(YEAR > 2017, NA, sprintf("%04d", abs(YEAR)))) %>%
    tidyr::unite(DATE, YEAR, MONTH, DAY, sep = "-") %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE, truncated = 2),
                  LONGITUDE = as.numeric(LONGITUDE),
                  LATITUDE = as.numeric(LATITUDE),
                  LOCATION_NAME = eq_location_clean(LOCATION_NAME),
                  EQ_PRIMARY = as.numeric(EQ_PRIMARY),
                  TOTAL_DEATHS = as.numeric(TOTAL_DEATHS)) %>%
    dplyr::select(DATE, CE, LONGITUDE, LATITUDE, COUNTRY, LOCATION_NAME, EQ_PRIMARY, TOTAL_DEATHS)
}

# module 2 ====

#' Geom for timelines
#'
#' \code{GeomTimeline} is a custom Geom for displaying timelines of earthquakes.
#'
#' @param x A vector of dates.
#' @param y (optional) A factor indicating some stratification in which case
#'   multiple time lines will be plotted for each level of the factor (e.g. country).
#' @param colour (optional) A numeric vector used for colouring the timeline marker borders.
#' @param fill (optional) A numeric vector used for colouring the timeline marker bodies.
#' @param size (optional) A numeric vector used for adjusting the size the timeline markers.
#' @param alpha (optional) A numeric value indicating the marker's level of transparency.
#' @param shape (optional) An integer code (0:25) for one of a set of graphics symbols.
#'   See \code{pch} at \code{?points}.
#'
#' @note This is an internal function that is not directly called by the user.
#'
#' @return A Geom object that is used within the \code{geom_timeline} function.
#'
#' @import ggplot2
#' @importFrom magrittr "%>%"
#'
#' @export
GeomTimeline <- ggproto("GeomTimeline", Geom,
                        required_aes = c("x"),
                        default_aes = aes(y = 0.1, colour = "grey20", fill = "grey20",
                                          size = 3, # summary(data$EQ_PRIMARY)
                                          alpha = 0.3, shape = 21, stroke = 0.5),
                        draw_key = draw_key_point, # GeomPoint$draw_key; GeomPoint$default_aes
                        draw_panel = function(data, panel_scales, coord) {

                          # transformation
                          coords <- coord$transform(data, panel_scales)

                          # grob objects
                          # segment
                          gS <- grid::segmentsGrob(
                            x0 = min(coords$x), x1 = max(coords$x),
                            y0 = coords$y, y1 = coords$y,
                            gp = grid::gpar(
                              col = "grey",
                              lwd = 1 * .pt
                            )
                          )

                          # points
                          gP<- grid::pointsGrob(
                            x = coords$x, y = coords$y,
                            pch = coords$shape,
                            size = unit(coords$size / 3, "char"),
                            gp = grid::gpar(
                              col = scales::alpha(coords$colour, coords$alpha),
                              fill = scales::alpha(coords$fill, coords$alpha)
                            )
                          )

                          # tree
                          grid::gTree(children = grid::gList(gS, gP))
                        }
)

#' Display timelines
#'
#' The \code{geom_timeline} function displays timelines of earthquakes based on the \code{GeomTimeline}.
#'
#' @param mapping Set of aesthetic mappings created by \code{aes} or \code{aes_}.
#'   If specified and \code{inherit.aes = TRUE} (the default), it is combined with the default mapping
#'   at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer.
#'   If \code{NULL], the default, the data is inherited from the plot data as specified in the call to \code{ggplot}.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning.
#'   If \code{TRUE}, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them.
#' @param ... other arguments passed on to \code{layer}.
#'   These are often aesthetics, used to set an aesthetic to a fixed value.
#'
#' @return A timeline plot with one point symbol for each date.
#'
#' @note The function only returns reasonable results for CE (Common Era) dates,
#'   because the \code{lubridate} package cannot handle BCE (Before Common Era) dates, yet.
#'
#' @import ggplot2
#' @importFrom magrittr "%>%"
#'
#' @examples
#' # size and colour as figure 1 of module 2
#' file <- system.file("extdata", "earthquakes.tsv.gz", package = "NOAAeq")
#' readr::read_delim(file = file, delim = "\t") %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(lubridate::year(DATE) %in% 2000:2017 & COUNTRY == "USA") %>%
#'   ggplot(aes(x = DATE, size = EQ_PRIMARY, fill = TOTAL_DEATHS)) +
#'   geom_timeline() +
#'   theme_classic() +
#'   theme(legend.position = "bottom") +
#'   scale_size_continuous(name = "Richter scale value") +
#'   scale_fill_continuous(name = "# deaths") +
#'   guides(size = guide_legend(order = 1),
#'          fill = guide_colourbar(order = 2))
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Geom for timeline labels
#'
#' \code{GeomTimelineLabel} is a custom Geom for displaying timeline labels.
#'
#' @param x A vector of dates.
#' @param label A character vector, e.g. the column \code{LOCATION_NAME}
#' @param n_max (optional) An integer value indicating the maximum number of labels per timeline.
#'   The \code{n_max} highest values of \code{col_max} will be selected.
#' @param col_max (optional) The column based on which the \code{n_max} highest values will be selected.
#' @param y (optional) A factor indicating some stratification in which case
#'   multiple time lines will be plotted for each level of the factor (e.g. country).
#' @param colour (optional) A colour used for colouring the vertical lines leading to the label.
#'
#' @note This is an internal function that is not directly called by the user.
#'
#' @return A Geom object that is used within the \code{geom_timeline_label} function.
#'
#' @import ggplot2
#' @importFrom magrittr "%>%"
#'
#' @export
GeomTimelineLabel <- ggproto("GeomTimelineLabel", Geom,
                             required_aes = c("x", "label"),
                             default_aes = aes(n_max = NA, col_max = NA, y = 0.1, colour = "grey"),
                             draw_key = draw_key_label, # GeomPoint$draw_key; GeomPoint$default_aes
                             draw_panel = function(data, panel_scales, coord) {

                               # transformation
                               coords <- coord$transform(data, panel_scales)

                               # subset
                               if (!is.na(coords$n_max[1])) {
                                 coords %<>%
                                   dplyr::group_by(y) %>%
                                   dplyr::top_n(coords$n_max[1], col_max)
                               }

                               # grob objects
                               # vertical lines
                               gL <- grid::polylineGrob(
                                 x = rep(coords$x, 2),
                                 y = c(coords$y, coords$y + 0.05),
                                 id = rep(1:nrow(coords), 2),
                                 gp = grid::gpar(
                                   col = coords$colour
                                 )
                               )

                               # labels
                               gT <- grid::textGrob(
                                 label = coords$label,
                                 x = coords$x, y = coords$y + 0.06,
                                 just = "left", rot = 45
                               )

                               # tree
                               grid::gTree(children = grid::gList(gL, gT))
                             }
)

#' Display timeline labels
#'
#' The \code{geom_timeline_label} function displays timeline labels based on the \code{GeomTimelineLabel}.
#' It is usually used together with the \code{geom_timeline} function.
#'
#' @param mapping Set of aesthetic mappings created by \code{aes} or \code{aes_}.
#'   If specified and \code{inherit.aes = TRUE} (the default), it is combined with the default mapping
#'   at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer.
#'   If \code{NULL], the default, the data is inherited from the plot data as specified in the call to \code{ggplot}.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning.
#'   If \code{TRUE}, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them.
#' @param ... other arguments passed on to \code{layer}.
#'   These are often aesthetics, used to set an aesthetic to a fixed value.
#'
#' @return Timeline labels are added to a timeline plot.
#'
#' @import ggplot2
#' @importFrom magrittr "%>%"
#'
#' @examples
#' # stratification and text annotations as in figure 3 of module 2
#' file <- system.file("extdata", "earthquakes.tsv.gz", package = "NOAAeq")
#' readr::read_delim(file = file, delim = "\t") %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(lubridate::year(DATE) %in% 2000:2017 & COUNTRY %in% c("USA", "CHINA")) %>%
#'   ggplot(aes(x = DATE, y = COUNTRY, fill = TOTAL_DEATHS)) +
#'   geom_timeline() +
#'   geom_timeline_label(aes(label = LOCATION_NAME, col_max = EQ_PRIMARY), n_max = 5) +
#'   theme_classic() +
#'   theme(legend.position = "bottom",
#'         axis.title.y = element_blank(), axis.ticks.y = element_blank(),
#'         axis.line.y = element_blank()) +
#'   scale_fill_continuous(name = "# deaths", breaks = c(1, 87652))
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# module 3 ====

#' Map earthquakes
#'
#' This function creates an interactive leaflet map with the earthquake locations.
#'
#' @param data A data frame or tibble with spatial coordinates (\code{LONGITUDE}, \code{LATITUDE}) and
#'   the earthquake magnitude (\code{EQ_PRIMARY}).
#' @param annot_col A column contained in \code{data}
#'   whose information will be displayed in the pop-up window.
#'
#' @return An interactive leaflet map with the earthquake locations.
#'   The marker size is proportional to the earthquake magnitude (\code{EQ_PRIMARY}).
#'   When clicking on a marker, a pop-up window will open.
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' # date pop-up as in figure 1 of module 3
#' file <- system.file("extdata", "earthquakes.tsv.gz", package = "NOAAeq")
#' readr::read_delim(file = file, delim = "\t") %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   eq_map(annot_col = "DATE")
#'
#' @export
eq_map <- function(data, annot_col = "DATE") {
  leaflet::leaflet(data) %>%
    leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
    leaflet::addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE,
                              popup = stats::as.formula(paste0("~", annot_col)),
                              radius = ~EQ_PRIMARY,
                              weight = 1 # stroke / border width
                              )
}

#' Create HTML labels
#'
#' This function creates HTML labels for pop-up windows in leaflet maps.
#'
#' @param data A data frame or tibble with the following columns:
#'   \code{LOCATION_NAME}, \code{EQ_PRIMARY}, and \code{TOTAL_DEATHS}.
#'
#' @return A character vector in HTML format.
#'
#' @examples
#' # html label pop-up as in figure 2 of module 3
#' file <- system.file("extdata", "earthquakes.tsv.gz", package = "NOAAeq")
#' readr::read_delim(file = file, delim = "\t") %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#'
#' @export
eq_create_label <- function(data) {
  text <- character(nrow(data))
  text <- ifelse(!is.na(data$LOCATION_NAME), paste(text, "<b>Location:</b>", data$LOCATION_NAME), text)
  text <- ifelse(text != "" & !is.na(data$EQ_PRIMARY), paste(text, "<br/>"), text)
  text <- ifelse(!is.na(data$EQ_PRIMARY), paste(text, "<b>Magnitude:</b>", data$EQ_PRIMARY), text)
  text <- ifelse(text != "" & !is.na(data$TOTAL_DEATHS), paste(text, "<br/>"), text)
  text <- ifelse(!is.na(data$TOTAL_DEATHS), paste(text, "<b>Total deaths:</b>", data$TOTAL_DEATHS), text)
  paste("<p>", text, "</p>")
}
