library(testthat)
library(NOAAeq)

# devtools::test()

context("Test NOAAeq package")

data <- readr::read_delim(file = system.file("extdata", "earthquakes.tsv.gz", package = "NOAAeq"), delim = "\t")

test_that("Test location name", expect_that(eq_location_clean("MEXICO:  MICHOACAN"), equals("Michoacan")))

test_that("Test date column", expect_is(data %>% eq_clean_data() %>% dplyr::select(DATE) %>% .[["DATE"]], "Date"))

test_that("Test geom",
          expect_is({x <- data %>%
            eq_clean_data() %>%
            dplyr::filter(lubridate::year(DATE) %in% 2000:2017 & COUNTRY == "USA") %>%
            ggplot(aes(x = DATE, size = EQ_PRIMARY, fill = TOTAL_DEATHS)) +
            geom_timeline()
          x$layers[[1]]$geom}, "GeomTimeline"))

test_that("Test label",
          expect_that({x <- data %>%
            eq_clean_data() %>%
            dplyr::filter(lubridate::year(DATE) %in% 2000:2017 & COUNTRY %in% c("USA", "CHINA")) %>%
            ggplot(aes(x = DATE, y = COUNTRY, fill = TOTAL_DEATHS)) +
            geom_timeline() +
            geom_timeline_label(aes(label = LOCATION_NAME, col_max = EQ_PRIMARY), n_max = 5)
          x$labels$label}, equals("LOCATION_NAME")))

test_that("Test map",
          expect_is(data %>%
                      eq_clean_data() %>%
                      dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
                      eq_map(annot_col = "DATE"), "leaflet"))

test_that("Test html", expect_match(eq_create_label(data[1,]), "<p>  <b>"))


