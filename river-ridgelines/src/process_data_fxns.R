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
             has_river_so7 ~ pull(filter(stream_mapping, stream_order==7), value),
             has_river_so6 ~ pull(filter(stream_mapping, stream_order==6), value),
             has_river_so5 ~ pull(filter(stream_mapping, stream_order==5), value),
             has_river_so4 ~ pull(filter(stream_mapping, stream_order==4), value),
             has_river_so3 ~ pull(filter(stream_mapping, stream_order==3), value),
             in_states ~ min_value,
             TRUE ~ no_data_value
           ))
}

interpolate_peaks <- function(grid_data, cell_size, seq_interval) {
  max_Y <- max(grid_data$Y)
  result <- grid_data %>%
    group_by(Y) %>%
    group_modify(~ {
      group_start_lon <- .x %>% slice(1) %>% pull(X)
      group_start_val <- .x %>% slice(1) %>% pull(value)
      purrr::pmap_dfr(.x, function(...){
        current <- tibble(...)
        diff_val <- current$value - group_start_val
        if (abs(diff_val) > 1) {
          seq_interval <- ifelse(diff_val <0, seq_interval*-1, seq_interval)
          new_values <- c(
            seq(from = group_start_val + 1, to=current$value -1, by = seq_interval),
            current$value
          )
          edit <- tibble(
            value = new_values,
            X = seq(from=group_start_lon+((cell_size)/length(new_values)), to=current$X, length.out=length(new_values)),
          ) %>%
            select(X, value)
        } else {
          edit <- current
        }
        group_start_val <<- current$value
        group_start_lon <<- current$X
        edit <- mutate(edit, Y_group = max_Y - .y$Y)
        return(edit)
      })
    })
}