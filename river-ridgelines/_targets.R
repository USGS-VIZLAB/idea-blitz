library(targets)

tar_option_set(packages = c("tidyverse", "nhdplusTools", "sf", "secret", "sbtools", "retry", "httr", "rmapshaper","linemap"))

# source scripts
source("src/authentication_helpers.R")
source("src/sb_fetch_helpers.R")
source("src/process_data_fxns.R")
source("src/plot_fxns.R")

# Fetch data
p1 <- list(
  tar_target(p1_proj, "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 + ellps=GRS80 +datum=NAD83 +units=m +no_defs"),
  tar_target(p1_sb_credentials_exist, has_sb_credentials()),
  tar_target(p1_mainstem_watersheds_gpkg,
             download_sb_files(sb_id = "60cb5edfd34e86b938a373f4",
                               sb_files_to_download = "nhdplusv2wbd_outlets.gpkg",
                               dest_dir = "in",
                               sb_secret_exists = p1_sb_credentials_exist),
             format = 'file'
  ),
  tar_target(p1_nhdplusv2_huc_info, sf::read_sf(p1_mainstem_watersheds_gpkg)),
  tar_target(p1_huc12, p1_nhdplusv2_huc_info %>% pull(HUC12) %>% unique()),
  tar_target(p1_huc02, unique(substr(p1_huc12, 1, 2))),
  tar_target(p1_huc08, unique(substr(p1_huc12, 1, 8))),
  tar_target(p1_huc_lookup, 
             sapply(p1_huc02, function(x, huc08) {
               huc08[grepl(paste0("^", x), huc08)]
             }, huc08 = p1_huc08, USE.NAMES = TRUE)),
  tar_target(p1_focal_huc02,
             c('05','06','07','08','10','11')), #'05','06','07', ,'10','11'
  tar_target(p1_focal_huc08,
             sort(p1_huc_lookup[[p1_focal_huc02]]),
             pattern = map(p1_focal_huc02)),
  tar_target(p1_focal_huc08_sf,
             nhdplusTools::get_huc8(id = p1_focal_huc08) %>% 
               st_union() %>% 
               st_make_valid() %>%
               st_sf(),
             pattern = map(p1_focal_huc08)),
  tar_target(p1_focal_flowlines_sf,
             purrr::map_df(st_sf(p1_focal_huc08_sf), function(huc_geom) {
               get_nhdplus(huc_geom, streamorder = 5)}),
             pattern = map(p1_focal_huc08_sf)),
  tar_target(p1_focal_flowlines_proj_sf,
            st_transform(p1_focal_flowlines_sf, p1_proj),
            pattern = map(p1_focal_huc08_sf)),
  # tar_target(p1_focal_flowlines_proj_simp_sf,
  #            ms_simplify(p1_focal_flowlines_proj_sf, keep=0.001)),
  tar_target(p1_states_shp_dir,
             {
               tar_path <- 'in/states.tar'
               GET('https://prd-tnm.s3.amazonaws.com/StagedProducts/Small-scale/data/Boundaries/statesp010g.shp_nt00938.tar.gz',
                         write_disk(tar_path))
               shp_dir <- 'in/states_shapefile'
               if (!dir.exists(shp_dir)) dir.create(shp_dir)
               untar(tar_path, exdir=shp_dir)
               return(shp_dir)
             },
             format='file'),
  tar_target(
    p1_states_sf,
    st_read(p1_states_shp_dir, quiet = TRUE) %>%
      filter(!(STATE_ABBR %in% c('HI','AK','PR','VI')) & TYPE=='Land') %>%
      # filter(STATE_ABBR %in% c('AR','MO','MS')) %>%
      st_transform(crs = st_crs(p1_proj))
  ),
  tar_target(
    p1_states_simp_sf,
    ms_simplify(p1_states_sf, keep=0.005)
  ),
  tar_target(p1_states_bbox,
             st_bbox(p1_states_simp_sf)),
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
  ),
  tar_target(p1_cell_size, 20000),
  tar_target(p1_grid_sf,
             sf::st_make_grid(cellsize = p1_cell_size, 
                              n=c(floor((p1_states_bbox$xmax-p1_states_bbox$xmin)/p1_cell_size), floor((p1_states_bbox$ymax-p1_states_bbox$ymin)/p1_cell_size)), 
                              offset = c(p1_states_bbox$xmin, p1_states_bbox$ymin), crs = st_crs(p1_proj))),
  tar_target(p1_grid_cells_w_rivers, st_intersects(p1_streams_sf, p1_grid_sf) %>% unlist() %>% unique()),
  # tar_target(p1_grid_cells_w_rivers_1, st_intersects(filter(p1_streams_sf, Strahler==1), p1_grid_sf) %>% unlist() %>% unique()),
  # tar_target(p1_grid_cells_w_rivers_2, st_intersects(filter(p1_streams_sf, Strahler==2), p1_grid_sf) %>% unlist() %>% unique()),
  tar_target(p1_grid_cells_w_rivers_3, st_intersects(filter(p1_streams_sf, Strahler==3), p1_grid_sf) %>% unlist() %>% unique()),
  tar_target(p1_grid_cells_w_rivers_4, st_intersects(filter(p1_streams_sf, Strahler==4), p1_grid_sf) %>% unlist() %>% unique()),
  tar_target(p1_grid_cells_w_rivers_5, st_intersects(filter(p1_streams_sf, Strahler==5), p1_grid_sf) %>% unlist() %>% unique()),
  tar_target(p1_grid_cells_w_rivers_6, st_intersects(filter(p1_streams_sf, Strahler==6), p1_grid_sf) %>% unlist() %>% unique()),
  tar_target(p1_grid_cells_w_rivers_7, st_intersects(filter(p1_streams_sf, Strahler==7), p1_grid_sf) %>% unlist() %>% unique()),
  tar_target(p1_mapping_levels,
             tibble(
               stream_order = c(3,4,5,6,7),
               value = c(500, 700, 1400, 2200, 3000)
             )),
  tar_target(p1_grid_data,
             get_gridded_data(empty_grid = p1_grid_sf,
                              stream_mapping = p1_mapping_levels,
                              stream_cells = p1_grid_cells_w_rivers,
                              stream_cells_so3 = p1_grid_cells_w_rivers_3,
                              stream_cells_so4 = p1_grid_cells_w_rivers_4,
                              stream_cells_so5 = p1_grid_cells_w_rivers_5,
                              stream_cells_so6 = p1_grid_cells_w_rivers_6,
                              stream_cells_so7 = p1_grid_cells_w_rivers_7,
                              min_value = 1,
                              clip_area = p1_states_simp_sf)),
  # tar_target(p1_grid_data,
  #            get_gridded_data(empty_grid = p1_grid_sf,
  #                             stream_mapping = p1_mapping_levels,
  #                             min_value = 1,
  #                             clip_area = p1_states_simp_sf)),
  # tar_target(p1_min_value, 1),
  # tar_target(p1_grid_data_by_stream_order,
  #            get_gridded_data(empty_grid = p1_grid_sf,
  #                             streams = p1_streams_sf,
  #                             stream_mapping = p1_mapping_levels,
  #                             cellsize = p1_cell_size,
  #                             min_value = p1_min_value,
  #                             clip_area = p1_states_simp_sf),
  #            pattern = map(p1_mapping_levels)),
  # tar_target(p1_grid_data_all_streams,
  #            p1_grid_data_by_stream_order %>%
  #              filter(has_river==TRUE) %>%
  #              group_by(cell_id) %>%
  #              summarize(
  #                value = max(value),
  #                stream_order = max(stream_order, na.rm=TRUE))
  #            ),
  # tar_target(p1_grid_data_no_streams,
  #              p1_grid_data_by_stream_order %>%
  #                group_by(cell_id) %>%
  #                filter(all(has_river)==FALSE) %>%
  #                summarize(
  #                  value = max(value),
  #                  stream_order = NA)
  #            ),
  # tar_target(p1_grid_data_all,
  #            bind_rows(p1_grid_data_all_streams, p1_grid_data_no_streams)),
  tar_target(p1_line_data, getgrid(x = p1_grid_data, cellsize = p1_cell_size, var = "value")),
  # tar_target(p1_line_data_so7, getgrid(x = p1_grid_data, cellsize = p1_cell_size, var = "value_so7")),
  # tar_target(p1_line_data_so6, getgrid(x = p1_grid_data, cellsize = p1_cell_size, var = "value_so6")),
  # tar_target(p1_line_data_so5, getgrid(x = p1_grid_data, cellsize = p1_cell_size, var = "value_so5")),
  # tar_target(p1_line_data_so4, getgrid(x = p1_grid_data, cellsize = p1_cell_size, var = "value_so4")),
  # tar_target(p1_line_data_so3, getgrid(x = p1_grid_data, cellsize = p1_cell_size, var = "value_so3")),
  tar_target(p1_line_data_no_streams, getgrid(x = filter(p1_grid_data, value==1), cellsize = p1_cell_size, var = "value")),
  tar_target(p1_line_data_so7, getgrid(x = filter(p1_grid_data, value==(pull(filter(p1_mapping_levels, stream_order == 7), value))), cellsize = p1_cell_size, var = "value")),
  tar_target(p1_line_data_so6, getgrid(x = filter(p1_grid_data, value==(pull(filter(p1_mapping_levels, stream_order == 6), value))), cellsize = p1_cell_size, var = "value")),
  tar_target(p1_line_data_so5, getgrid(x = filter(p1_grid_data, value==(pull(filter(p1_mapping_levels, stream_order == 5), value))), cellsize = p1_cell_size, var = "value")),
  tar_target(p1_line_data_so4, getgrid(x = filter(p1_grid_data, value==(pull(filter(p1_mapping_levels, stream_order == 4), value))), cellsize = p1_cell_size, var = "value")),
  tar_target(p1_line_data_so3, getgrid(x = filter(p1_grid_data, value==(pull(filter(p1_mapping_levels, stream_order == 3), value))), cellsize = p1_cell_size, var = "value")),
  # tar_target(p1_line_data_so7, getgrid(x = filter(p1_grid_data, value == pull(filter(p1_mapping_levels, stream_order==7), value)), cellsize = p1_cell_size, var = "value")),
  # tar_target(p1_line_data_so6, getgrid(x = p1_grid_data, cellsize = p1_cell_size, var = "value_so6")),
  # tar_target(p1_line_data_so5, getgrid(x = p1_grid_data, cellsize = p1_cell_size, var = "value_so5")),
  # tar_target(p1_line_data_so4, getgrid(x = p1_grid_data, cellsize = p1_cell_size, var = "value_so4")),
  # tar_target(p1_line_datas_so3, getgrid(x = p1_grid_data, cellsize = p1_cell_size, var = "value_so3")),
  tar_target(p1_linemap,
    plot_linemap(background_poly = p1_states_simp_sf,
                 line_data = p1_line_data,
                 line_data_no_streams = p1_line_data_no_streams,
                 line_data_so3 = p1_line_data_so3,
                 line_data_so4 = p1_line_data_so4,
                 line_data_so5 = p1_line_data_so5,
                 line_data_so6 = p1_line_data_so6,
                 line_data_so7 = p1_line_data_so7,
                 bkgd_color = "ivory2", 
                 poly_color = "ivory1", 
                 line_fill_color = "ivory1", 
                 line_border_color = "ivory4",
                 width = 1600,
                 height = 900,
                 outfile = 'out/linemap.jpg'))
)


# Combined list of target outputs
c(p1)