#' Get data points for graph for a given year
#' 
#' Intersect fire perimeters for a given year with the water use data and 
#' compute the (1) proportion of water use watersheds affected by fire and (2) 
#' the total downstream population that use water from the affected watersheds.
#' 
#' @param perim Fire perimeter dataset.
#' @param huc Water use by HUC dataset.
#' @param year The year given to summarize within.
#' 
data_by_year <- function(perim, huc, year){
  # Set attributes to constant over geometry (to prevent warnings)
  st_agr(perim) <- "constant"
  st_agr(huc) <- "constant"
  
  # Filter fire perim by year
  perim_year <- perim %>%
    filter(Year == year)
  
  # Filter only hucs that supply surface water
  huc_intersection <- huc %>%
    filter(SUM_POP > 0) %>%
    mutate(area_ws = st_area(.)) %>%
    st_intersection(perim_year) %>%
    mutate(area_int = st_area(.)) %>%
    mutate(prop_burned = area_int/area_ws)
  
  out <- data.frame(Year = year,
                    Mean_ws_prop_affected_pc = mean(huc_intersection$prop_burned) %>%
                      as.numeric()*100,
                    Population_affected_mil = sum(huc_intersection$SUM_POP)/1000000)
  
  return(out)
}

#' Get chart data for all years
#' 
#' Get data points for all years specified and prep for graphing.
#' 
#' @param years Numeric vector of years to be included (vector must be a target 
#'   to be dynamically branched)
#' @param perim Fire perimeter dataset.
#' @param huc Water use by HUC dataset.
#' 
build_chart_data <- function(years, perim, huc){
  lapply(years, FUN = function(X) data_by_year(perim, huc, X)) %>%
    bind_rows() %>%
    pivot_longer(-one_of("Year")) %>%
    mutate(name = recode(name, "Mean_ws_prop_affected_pc" = "Proportion of US water supply watersheds affected by wildfire (%)",
                         "Population_affected_mil" = "Consumers of water from affected watersheds (millions of people)")) %>%
    mutate(name_f = factor(name, levels = c("Proportion of US water supply watersheds affected by wildfire (%)",
                                            "Consumers of water from affected watersheds (millions of people)")))
}

#' Linear interpolation
#' 
#' Generic linear interpolation function that returns intermediate values 
#' between given values "factor" parameter is how much to interpolate 
#' (factor = 10 will return 9 intermediate values at 1/10th intervals).
#' 
#' @param from The lower value to be interpolated between.
#' @param to The upper value to be interpolated between.
#' @param factor The step at which to interpolate.
#' 
#' @examples 
#' linear_interpolation(from = 1, to = 2, factor = 4)
#' # 1.25  1.50  1.75
#' 
linear_interpolation <- function(from, to, factor){
  seq(from = from, to = to, length.out = factor+1) %>%
    head(-1) %>%
    tail(-1)
}

#' Expand vector through linear interpolation
#' 
#' Expand vector by interpolating values between input values in given vector, 
#' returning intermediate values.
#' 
#' @param data A numeric vector of values to interpolate between.
#' @param factor The step at which to interpolate.
#' 
#' @seealso 
#' linear_interpolation
#' 
interpolate <- function(data, factor) {
  if(!is.null(ncol(data))) data <- pull(data)
  map2(
    head(data, -1),
    tail(data, -1),
    linear_interpolation,
    factor = factor) %>%
    unlist()
}

#' Add rows to expand data frame
#' 
#' Add rows to expand data frame for graph with interpolated years and values.
#' 
#' @param data The chart data to be expanded by interpolation.
#' @param factor The step at which to interpolate.
#' 
#' @seealso 
#' interpolate 
#' linear_interpolation
add_interpolation <- function(data, factor){
  data %>%
    arrange(name) %>%
    add_row(value = split(., .$name) %>%
              map(~interpolate(.$value, factor = factor)) %>%
              unlist(),
            Year = rep(interpolate(unique(.$Year), factor = factor), times = 2),
            name = rep(unique(.$name), each = (nrow(.)/2-1) * (factor-1)),
            name_f = rep(unique(.$name), each = (nrow(.)/2-1) * (factor-1))) %>%
    arrange(name, Year)
}
