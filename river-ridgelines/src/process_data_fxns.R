get_gridded_data <- function(empty_grid, stream_mapping, stream_cells, stream_cells_so3, stream_cells_so4,
                             stream_cells_so5, stream_cells_so6, stream_cells_so7, 
                             min_value, clip_area) {

  
  grid_with_data <- empty_grid %>%
    st_sf() %>%
    mutate(cell_id = row_number(),
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
             TRUE ~ min_value
           ),
           value_so7 = case_when(
             has_river_so7 ~ 3000,
             TRUE ~ 0
           ),
           value_so6 = case_when(
             has_river_so7 ~ 0,
             has_river_so6 ~ 2200,
             TRUE ~ 0
           ),
           value_so5 = case_when(
             has_river_so6 ~ 0,
             has_river_so5 ~ 1400,
             TRUE ~ 0
           ),
           value_so4 = case_when(
             has_river_so7 ~ 0,
             has_river_so6 ~ 0,
             has_river_so5 ~ 0,
             has_river_so4 ~ 700,
             TRUE ~ 0
           ),
           value_so3 = case_when(
             has_river_so7 ~ 0,
             has_river_so6 ~ 0,
             has_river_so5 ~ 0,
             has_river_so4 ~ 0,
             has_river_so3 ~ 500,
             TRUE ~ 0
           )) %>%
    st_intersection(clip_area) %>%
    select(cell_id, has_river, value, geometry)
}
# 
# get_gridded_data <- function(empty_grid, streams, stream_mapping, cellsize, min_value, clip_area) {
#   current_stream_order <- stream_mapping$stream_order
#   cells_w_streams = st_intersects(filter(streams, Strahler==current_stream_order), empty_grid) %>% unlist() %>% unique()
#   
#   grid_with_data <- empty_grid %>%
#     st_sf() %>%
#     mutate(cell_id = row_number(),
#            has_river = cell_id %in% cells_w_streams,
#            value = case_when(
#              has_river ~ pull(stream_mapping, value),
#              TRUE ~ min_value
#            )) %>%
#     st_intersection(clip_area) %>%
#     mutate(stream_order = current_stream_order) %>%
#     select(stream_order, cell_id, has_river, value, geometry)
# }