#' @title 
#' @description Using predefined vectors of cells that contain streams
#' of various orders, set attributes for the grid cells. Where streams are
#' present, set the value based on the highest order of stream that
#' is present. Where no streams are present, but the cell overlaps the states 
#' polygon, set the value to the `min_value`. Where the cell falls outside
#' of the states polygon, set the value to the `no_data_value`
#' @param empty_grid - A sf object of grid polygons w/o any attribute data
#' @param stream_mapping - tibble of stream orders and the values that cells
#' that contain those streams should be assigned, according to the highest
#' stream order present within each cell.
#' @param state_cells - vector of grid cells that overlap the polygon of us states
#' @param stream_cells - vector of grid cells that contain a stream of any Strahler order
#' @param stream_cells_so3 - vector of grid cells that contain a 3rd order stream
#' @param stream_cells_so4 - vector of grid cells that contain a 4th order stream
#' @param stream_cells_so5 - vector of grid cells that contain a 5th order stream
#' @param stream_cells_so6 - vector of grid cells that contain a 6th order stream
#' @param stream_cells_so7 - vector of grid cells that contain a 7th order stream
#' @param min_value - value to assign to cells w/i the states but w/o a stream
#' @param no_data_value - value to assign to cells outside of the states
#' @return A sf object of the grid polygons, with attributes indicating which stream
#' orders are presents, and a column for `value` indicating the fixed height value
#' assigned to each cell depending on which order of stream, if any, falls within that
#' cell
get_gridded_data <- function(empty_grid, stream_mapping, state_cells, stream_cells, stream_cells_so3, stream_cells_so4,
                             stream_cells_so5, stream_cells_so6, stream_cells_so7, 
                             min_value, no_data_value) {
  empty_grid %>%
    st_sf() %>%
    mutate(cell_id = row_number(),
           in_states = cell_id %in% state_cells,
           has_river = cell_id %in% stream_cells,
           has_river_so3 = cell_id %in% stream_cells_so3,
           has_river_so4 = cell_id %in% stream_cells_so4,
           has_river_so5 = cell_id %in% stream_cells_so5,
           has_river_so6 = cell_id %in% stream_cells_so6,
           has_river_so7 = cell_id %in% stream_cells_so7,
           value = case_when(
             # set value based on the highest order of stream within each cell
             has_river_so7 ~ pull(filter(stream_mapping, stream_order==7), value),
             has_river_so6 ~ pull(filter(stream_mapping, stream_order==6), value),
             has_river_so5 ~ pull(filter(stream_mapping, stream_order==5), value),
             has_river_so4 ~ pull(filter(stream_mapping, stream_order==4), value),
             has_river_so3 ~ pull(filter(stream_mapping, stream_order==3), value),
             # Where no streams are present, but the cell overlaps the states polygon, set the value to the `min_value`
             in_states ~ min_value,
             # Where the cell falls outside of the states polygon, set the value to the `no_data_value`
             TRUE ~ no_data_value
           ))
}

#' @title Interpolate peaks
#' @description For grid cell centroids in each row (Y), when the value
#' changes, linearly interpolated that change in the value across an 
#' set of additional X values that increment by a specified interval
#' @param line_data - data.frame of X and Y coordinates of grid cell centroids,
#' and values for those coordinates (assigned in `get_gridded_data()`)
#' @param cell_size - the cell_size of the gridded data
#' @param interp_interval - interval by which to interpolate the X values.
#' Must be less than the cell_size
#' @return - a data.frame of X and Y coordinates of grid cell centroids and
#' values for those coordinates (interpolated to a smaller `X` interval than
#' dictated by the grid `cell_size` where values shift)
interpolate_peaks <- function(line_data, cell_size, interp_interval) {
  if (interp_interval >= cell_size) {
    stop(message('The interpolation interval must be smaller than the cell size'))
  }
  
  # Pull the maximum Y (latitude)
  max_Y <- max(line_data$Y)
  result <- line_data %>%
    # For each row (latitude) of the grid
    group_by(Y) %>%
    # Interpolate values to a smaller X interval where values shift
    group_modify(~ {
      # Pull the starting X (longitude)
      start_x <- .x %>% slice(1) %>% pull(X)
      # pull the starting cell value
      start_val <- .x %>% slice(1) %>% pull(value)
      
      # Map over all longitudes
      purrr::pmap_dfr(.x, function(...) {
        # pull the row for the current latitude and longitude
        current <- tibble(...)
        # compute the difference between the value for the current
        # longitude and that for the previous one
        diff_val <- current$value - start_val
        # if the |difference| is > 0, 
        if (abs(diff_val) > 0) {
          # get a new set of interpolated X values
          new_x = c(
            seq(
              from = floor((start_x+interp_interval)/interp_interval)*interp_interval,
              to = floor((current$X-interp_interval)/interp_interval)*interp_interval,
              by=interp_interval
            ),
            current$X
          )
          # Determine the interval by which the value needs to increment
          value_interval <- (current$value - start_val)/(length(new_x)-1)
          # Linearly interpolate the values to match the interpolated X values
          updated_line_data <- tibble(
            X = new_x,
            value =
              seq(
                from = start_val+value_interval, 
                to = current$value, 
                length.out=length(new_x)),
          )
        } else {
          # if the |difference| is <= 1,
          updated_line_data <- current
        }
        
        # store the current value and X
        start_val <<- current$value
        start_x <<- current$X
        
        # Assign the group, such that the highest Y (latitude) is the first group
        updated_line_data <- mutate(updated_line_data, Y_group = max_Y - .y$Y)
        
        # Return the data for the row (latitude)
        return(updated_line_data)
      })
    })
}
