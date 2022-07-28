# Get fire perimeter data
# Download fire perimeter
get_fire_perim <- function(url, perim_zip_path, perim_tmp_path, crs){
  download.file(url = url, destfile = perim_zip_path)
  unzip(perim_zip_path, exdir = perim_tmp_path)
  
  mtbs_file_path <- unzip(perim_zip_path, list = T) %>%
    select(Name) %>%
    filter(str_sub(Name, -3) == "shp") %>%
    unlist() %>%
    paste(perim_tmp_path, ., sep = "/")
  
  perim <- st_read(mtbs_file_path)
  
  return(perim)
}

# Get HUC water use assessment data
# Can't figure out how to download directly from Box
# Data available from https://usfs-public.app.box.com/v/Forests2Faucets/file/938183618458
get_huc <- function(file_in = NULL, layer = NULL, crs){
  if(is.null(layer)){
    out <- st_read(file_in)
  }else{
    out <- st_read(dsn = file_in, layer = layer)
  }
  out %>%
    st_transform(crs = crs)
}

# Get basemap tiles
get_basemap <- function(file_in_for_extent, file_out){
  get_tiles(file_in_for_extent, provider = "CartoDB.DarkMatterNoLabels", crop = T, verbose = T, zoom = 4,
            cachedir = "1_fetch/tmp/", forceDownload = T) %>%
    writeRaster(file_out, overwrite = T)
  return(file_out)
}
