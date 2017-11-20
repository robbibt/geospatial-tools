# Packages
library(raster)
library(USAboundaries)
library(FedData)
library(rgeos)
library(rgdal)
library(countrycode)
library(gdalUtils)
library(sf)
library(tidyverse)

options(timeout = 240000)

# Download and unzip data from online ---------------------------------------------------------------------------------

download.unzip = function(download.link, download.folder, unzip = TRUE) {
  
  # Downloads and unzips data from online
  #
  # Args:
  #   download.link: URL/location of file to be downloaded
  #   download.folder: Folder directory to export downloaded/unzipped file
  #   unzip: If file is .zip, unzip after download?
  #
  # Returns:
  #   String location of output file (File exported to download.folder)
  
  if (tools::file_ext(download.link) == "zip" & unzip) {
    
    # Download to temp file 
    message(paste0("Downloading ", basename(download.link)))
    temp = tempfile()
    tryCatch( {
      
      utils::download.file(download.link, temp, mode = "wb", quiet = TRUE)
      
      # Unzip to download folder
      message(paste0("  Unzipping ", basename(download.link)))
      unzip(temp, overwrite = TRUE, junkpaths = TRUE, exdir = download.folder)
      output_location = paste0(download.folder, "/", basename(unzip(temp, list = TRUE)[[1]]))
      
      # Remove temporary file
      unlink(temp)
      
    }, 
    
    # Catch any errors
    error = function(err) { 
      message("URL timed out or does not exist. Original error message:")
      message(err)
      
    } )
    
  } else {
    
    # Download file directly
    message(paste0("Downloading ", basename(download.link)))
    output_location = paste0(download.folder, "/", basename(download.link))
    tryCatch( {
      
      # Attempt to download
      utils::download.file(download.link, output_location, mode = "wb", quiet = TRUE) 
      
    }, 
    
    # Catch any errors
    error = function(err) { 
      message("URL timed out or does not exist. Original error message:")
      message(err) 
      
    } )
    
  }
  
  return(output_location)
  
}


# # Example 
# download.link = "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/gtc/downloads/treecover2010_v3_individual/80N_120W_treecover2010_v3.tif.zip"
# download.link = "https://storage.googleapis.com/earthenginepartners-hansen/GFC2015/Hansen_GFC2015_treecover2000_40N_080W.tif"
# download.folder = "D:/Geography/GIS_data/Vegetation/Tree data/hansen2013_treecover"
# output = download.unzip(download.link, download.folder, unzip = FALSE)



# Run for batch -------------------------------------------------------------------------------------------------------

coord.grids = function(shape.file) {
  
  # For a given shapefile, returns vector of corresponding 10 degree tile IDs in "XXN_XXXW" format
  #
  # Args:
  #   shape.file: Input shapefile
  #
  # Returns:
  #   Vector containing corresponding tile IDs in "XXN_XXXW" format

  # Round to nearest 10 degrees
  bbox.rounded = 10 * ceiling(bbox(shape.file) / 10)
  
  # All x and y
  all_x = seq(bbox.rounded[1,1], bbox.rounded[1,2], 10)
  all_y = seq(bbox.rounded[2,1], bbox.rounded[2,2], 10)
  
  # Identify all combinations of x and y
  all_combs = expand.grid(x=all_x, 
                          y=all_y)
  
  # Convert to "XXN_XXXW" format
  all_combs %>%
    
    # Remove duplicates
    distinct(x, y) %>% 
    
    # For each row
    rowwise() %>% 
    
    # Convert values to N/E/S/W and return vector
    mutate(x = x - 10,
           y_string = ifelse(y < 0, paste0(abs(y), "N"), paste0(y, "N")),
           x_string = ifelse(x < 0, paste0(stringr::str_pad(abs(x), 3, pad = 0), "W"), 
                             paste0(stringr::str_pad(abs(x), 3, pad = 0), "E")),
           combined = paste0(y_string, "_", x_string))

}

#Specify target ISO country code and path to downloaded shapefile
country_name = countrycode('Canada', 'country.name', 'iso3c')
country = getData("GADM", 
                  country = country_name, 
                  level=0); plot(country)

# # For USA, remove Hawaii and Alaska
# country = getData("GADM", 
#                   country = "USA", 
#                   level=1)[c(4:12, 14:length(country)),]

# # For USA, select only Alaska
# country = getData("GADM",
#                   country = "USA",
#                   level=1)[3,]
# plot(country)

# # For USA, select only Hawaii
# country = getData("GADM",
#                   country = "USA",
#                   level=1)[13,]
# plot(country)

# Table of coordinates for country
coord.table = coord.grids(shape.file = country)

# Iterate over all corresponding tiles
for (i in 1:nrow(coord.table)) { 
  
  tile.id = coord.table[[i,"combined"]]
  download.folder = "D:/Geography/GIS_data/Vegetation/Tree data/hansen2013_treecover" 
  plot(SpatialPoints(coord.table[i,1:2]), add = TRUE, col = "green", bg = "green", pch = 20, cex = 5)
  
  if (!file.exists(paste0(download.folder, "/", tile.id, "_treecover2010_v3.tif"))) {
  
    # Use try to skip tiles that do not download correctly
    tryCatch({
      
      download.link = paste0("https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/gtc/downloads/treecover2010_v3_individual/", 
                             tile.id, "_treecover2010_v3.tif.zip")
  
      download.unzip(download.link, download.folder, unzip = TRUE) 
      
    }, 
    
    # Catch any errors
    error = function(err) { 
      
      plot(SpatialPoints(coord.table[i,1:2]), add = TRUE, col = "red", bg = "red", pch = 20, cex = 5)
      
    } )
  
  } else { 
    
    # If file exists
    message(paste0(tile.id, "_treecover2010_v3.tif", " already exists; skipping"))
    
  }
  
}


# Test of compressing file after download -----------------------------------------------------------------------------
#   Currently has issues with pyramids in Arc, changing values in QGIS

# Read in raster and export compressed
raster_in = raster(output)
writeRaster(x=raster_in,
            filename = paste0(tools::file_path_sans_ext(output), "_comp.tif"),
            options=c("COMPRESS=DEFLATE", "ZLEVEL=1"))

# Delete original file
unlink(output)


# Download US NED data ------------------------------------------------------------------------------------------------

g.clip = function(shape.file, bounding.box) {
  
  # Uses bounding box to clip shapefile into two parts
  #
  # Args:
  #   shape.file: URL/location of file to be downloaded
  #   bounding.box: Folder directory to export downloaded/unzipped file
  #
  # Returns:
  #   Shapefile clipped to bounding box
  
  if(class(bounding.box) == "matrix") { 
    
    bounding.poly <- as(extent(as.vector(t(bounding.box))), "SpatialPolygons") 
    
  } else { 
    
    b_poly <- as(extent(bounding.box), "SpatialPolygons")
    
  }
  
  gIntersection(shape.file, bounding.poly, byid = TRUE)
  
}

# Extract boundary
state_name = "Michigan"
state_boundary = us_states(resolution = "high", states = state_name)
state_boundary = spTransform(state_boundary, CRS("+proj=utm +datum=NAD83 +zone=12"))
plot(state_boundary)

# # Extract country
# state_boundary = getData("GADM", country = "MEX", level=0)
# state_boundary = spTransform(state_boundary, CRS("+proj=utm +datum=NAD83 +zone=12"))

# Optional: split vertically to process both halves separately (for large areas)
new_bb = bbox(state_boundary)
# new_bb[2] = new_bb[2] + (new_bb[4] - new_bb[2]) / 2  # north side
new_bb[4] = new_bb[2] + (new_bb[4] - new_bb[2]) / 2  # south side
state_boundary = g.clip(state_boundary, new_bb)
plot(state_boundary)

# # Optional: split horizontally to process both halves separately (for large areas)
# new_bb = bbox(state_boundary)
# new_bb[1] = new_bb[1] + (new_bb[3] - new_bb[1]) / 2  # east side
# # new_bb[3] = new_bb[1] + (new_bb[3] - new_bb[1]) / 2  # west side
# state_boundary = g.clip(state_boundary, new_bb)
# plot(state_boundary)

# Get the NED; return a raster
NED = get_ned(template=state_boundary, label=paste0(state_name,"1"), 
              raw.dir = "D:/Geography/GIS_data/Elevation/USA/NED/",  
              extraction.dir = "D:/Geography/GIS_data/Elevation/USA/NED/")



# Import large number of rasters into dataframe plottable in ggplot2---------------------------------------------------

# Function to import raster and fix column names
raster.to.df = function(input.string) {
  
  # Import and convert to points
  input.raster = raster(input.string)
  input.points = rasterToPoints(input.raster)
  input.df = data.frame(input.points)
  colnames(input.df) = c("long", "lat", "value")
  input.df$file = input.string
  
  return(input.df)
  
}

# Read in all files
rasters.df = dir(pattern = "example_regex",      
                 recursive = TRUE)  %>% 
  
  # Read in each individually and merge output
  map(raster.to.df) %>%
  reduce(bind_rows) %>% 
  as.tbl()



# Identify tiles that overlap region ----------------------------------------------------------------------------------

# Setup
grid_shp = "grid.shp"
region_shp = "region.shp"

# Load in SRTM grid and MDB boundary shapefile; transform to same projection
grid = read_sf(grid_shp) %>%
               st_transform("+proj=longlat +datum=WGS84 +no_defs")
region = read_sf(region_shp) %>%
               st_transform("+proj=longlat +datum=WGS84 +no_defs")

# Find tiles intersecting with region
grid_region = grid[st_intersects(grid, region, sparse = FALSE),]



# Use gdalUtils to clip and translate raster --------------------------------------------------------------------------

# Setup
input_raster = "input"
output_tif = "output.tif"
output_translated = "output.hgt"
coordinates = c(150, -30, 151, -29)

# Clip mosaic dataset by coordinates and write to temporary .tif file
gdalwarp(srcfile = input_raster,
         dstfile = output_tif,
         te = coordinates,
         overwrite = TRUE,
         multi = TRUE)
  
# Translate to new format
gdal_translate(src_dataset = output_tif,
               dst_dataset = output_translated,
               of = "SRTMHGT")
  


# Import large number of rasters, using gdalwarp on import -----------------------------------------------------------

# Function to import raster and fix column names
gdalwarp.to.df = function(input.string, factor, temp.file = "temp.tif") {
  
  # Resize images
  input.raster = gdalUtils::gdalwarp(srcfile = input.string,
                                     dstfile = temp.file,
                                     tr=res(input.raster)*factor,
                                     overwrite = TRUE,
                                     multi = TRUE,
                                     output_Raster=TRUE)
  
  # Convert to points
  input.points = rasterToPoints(input.raster)
  input.df = data.frame(input.points)
  colnames(input.df) = c("long", "lat", "value")
  input.df$file = input.string
  
  # Remove file
  unlink("temp.tif")
  
  return(input.df)
  
}

# Read in all files
rasters.df = dir(pattern = "*.tif$",      
                 recursive = TRUE)[1]  %>% 
  
  # Read in each individually and merge output
  map(gdalwarp.to.df, 10, "temp.tif") %>%
  reduce(bind_rows) %>% 
  as.tbl()

