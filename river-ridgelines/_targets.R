library(targets)

tar_option_set(packages = c("tidyverse", "sf", "httr","spData", "ggplot2","scico",'sysfonts','cowplot','magick','grid','showtext'))

# source scripts
source("src/process_data_fxns.R")
source("src/plot_fxns.R")

# Fetch data
p1 <- list(
  tar_target(p1_proj, "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 + ellps=GRS80 +datum=NAD83 +units=m +no_defs"),
  tar_target(
    p1_states_sf,
    spData::us_states %>% st_transform(p1_proj)
  ),
  tar_target(p1_states_bbox,
             st_bbox(p1_states_sf)),
  tar_target(p1_hydro_gdb_dir,
             {
               tar_path <- 'in/hydro.tar'
               GET('https://prd-tnm.s3.amazonaws.com/StagedProducts/Small-scale/data/Hydrography/hydrusm010g.gdb_nt00897.tar.gz',
                   write_disk(tar_path))
               gdb_dir <- 'in/hydro_gdb'
               if (!dir.exists(gdb_dir)) dir.create(gdb_dir)
               untar(tar_path, exdir=gdb_dir)
               return(gdb_dir)
             },
             format='file'),
  tar_target(p1_hydro_gdb,
             list.dirs(p1_hydro_gdb_dir, recursive=FALSE),
             format='file'),
  tar_target(
    p1_streams_sf,
    st_read(p1_hydro_gdb, quiet = TRUE, layer='Stream') %>%
      st_zm() %>%
      st_transform(crs = st_crs(p1_proj))
  )
)
# Process the data
p2 <- list(
  tar_target(p2_cell_size, 20000),
  tar_target(p2_bounding_poly, {
    x_min <- p1_states_bbox$xmin-(5*p2_cell_size)
    x_max <- p1_states_bbox$xmax+(5*p2_cell_size)
    y_min <- p1_states_bbox$ymin-(5*p2_cell_size)
    y_max <- p1_states_bbox$ymax+(5*p2_cell_size)
    sf::st_sf(sf::st_sfc(sf::st_polygon(list(
      cbind(lon = c(x_min, x_min, x_max, x_max, x_min),
            lat = c(y_min, y_max, y_max, y_min, y_min))
    ))), crs=p1_proj)
  }),
  # Create a mask for the state polygon
  tar_target(p2_states_inverse, 
             sf::st_difference(p2_bounding_poly, st_buffer(st_union(p1_states_sf), p2_cell_size*1.5))),
  # Build a grid covering the states
  tar_target(p2_grid_sf,
             sf::st_make_grid(cellsize = p2_cell_size, 
                              n=c(floor((p1_states_bbox$xmax-p1_states_bbox$xmin)/p2_cell_size), floor((p1_states_bbox$ymax-p1_states_bbox$ymin)/p2_cell_size)), 
                              offset = c(p1_states_bbox$xmin, p1_states_bbox$ymin), crs = st_crs(p1_proj))),
  # Determine the overlap between the grid cells and the states
  tar_target(p2_grid_cells_in_states, st_intersects(p1_states_sf, p2_grid_sf) %>% unlist() %>% unique()),
  # Determine the overlap between the grid cells and specific stream orders
  tar_target(p2_grid_cells_w_rivers, st_intersects(p1_streams_sf, p2_grid_sf) %>% unlist() %>% unique()),
  tar_target(p2_grid_cells_w_rivers_3, st_intersects(filter(p1_streams_sf, Strahler==3), p2_grid_sf) %>% unlist() %>% unique()),
  tar_target(p2_grid_cells_w_rivers_4, st_intersects(filter(p1_streams_sf, Strahler==4), p2_grid_sf) %>% unlist() %>% unique()),
  tar_target(p2_grid_cells_w_rivers_5, st_intersects(filter(p1_streams_sf, Strahler==5), p2_grid_sf) %>% unlist() %>% unique()),
  tar_target(p2_grid_cells_w_rivers_6, st_intersects(filter(p1_streams_sf, Strahler==6), p2_grid_sf) %>% unlist() %>% unique()),
  tar_target(p2_grid_cells_w_rivers_7, st_intersects(filter(p1_streams_sf, Strahler==7), p2_grid_sf) %>% unlist() %>% unique()),
  # Set the values that will be assigned to cells containing streams, based on the stream order
  tar_target(p2_mapping_levels,
             tibble(
               stream_order = c(3,4,5,6,7),
               value = c(900, 1300, 2000, 3500, 5000) # 500, 700, 1400, 2200, 3000
             )),
  # Add attribute data to the grid, with values set based on the highest stream order in each cell
  tar_target(p2_grid_data_sf,
             get_gridded_data(empty_grid = p2_grid_sf,
                              stream_mapping = p2_mapping_levels,
                              state_cells = p2_grid_cells_in_states,
                              stream_cells = p2_grid_cells_w_rivers,
                              stream_cells_so3 = p2_grid_cells_w_rivers_3,
                              stream_cells_so4 = p2_grid_cells_w_rivers_4,
                              stream_cells_so5 = p2_grid_cells_w_rivers_5,
                              stream_cells_so6 = p2_grid_cells_w_rivers_6,
                              stream_cells_so7 = p2_grid_cells_w_rivers_7,
                              min_value = 100,
                              no_data_value = 0)),
  # Convert the gridded data to a dataframe with X and Y coordinates of the cell centroids
  tar_target(p2_line_data, {
    grid_data_w_coords <- cbind(p2_grid_data_sf, sf::st_coordinates(sf::st_centroid(sf::st_geometry(p2_grid_data_sf))))
    line_data <- as.data.frame(grid_data_w_coords[,c("X","Y","value"), drop=TRUE])
    return(line_data)
  }),
  # For each row (latitude), interpolate the line data where values change between grid cells 
  tar_target(p2_line_data_interpolated,
             interpolate_peaks(p2_line_data, p2_cell_size, interp_interval=100))
)
# Generate the map
p3 <- list(
  tar_target(p3_usgs_logo_png,
             'in/2021_usgs-logo-black.png',
             format='file'),
  tar_target(p3_linemap,
    plot_linemap(line_data = p2_line_data_interpolated,
                 vertical_exag_factor = 5, 
                 line_thickness = 0.3,
                 gradient_low = '#EEEEE0',
                 gradient_high = '#3B352D',
                 states_poly = p1_states_sf,
                 poly_color = "#FFFFF0", 
                 states_mask = p2_states_inverse,
                 bkgd_color = '#EEEEE0',
                 google_font = 'Marcellus SC',
                 text_color = '#8C8B82',
                 usgs_logo_filepath = p3_usgs_logo_png,
                 width = 1600,
                 height = 900,
                 dim_units = 'px',
                 res = 100,
                 outfile = 'out/linemap.png'))
)

# Combined list of target outputs
c(p1, p2, p3)