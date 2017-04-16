
<!-- README.md is generated from README.Rmd. Please edit that file -->
Introduction to NOAAeq
======================

The `NOAAeq` package analyses data from the [Significant Earthquake Database](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1) of the U.S. National Oceanographic and Atmospheric Administration (NOAA). This dataset contains information about 5,933 earthquakes over an approximately 4,000 year time span.

The package has 3 main functionalities:

-   Data cleaning

-   Timeline plots

-   Interactive leaflet maps

Timelines
---------

Here is an example of a timeline plot for the USA, covering the recent period 2000-2017.

``` r
data %>%
 eq_clean_data() %>%
 dplyr::filter(lubridate::year(DATE) %in% 2000:2017 & COUNTRY == "USA") %>%
 ggplot(aes(x = DATE, size = EQ_PRIMARY, fill = TOTAL_DEATHS)) +
 geom_timeline() +
 theme_classic() +
 theme(legend.position = "bottom") +
 scale_size_continuous(name = "Richter scale value") +
 scale_fill_continuous(name = "# deaths") +
 guides(size = guide_legend(order = 1),
        fill = guide_colourbar(order = 2))
```

![](README-timeline1-1.png)
