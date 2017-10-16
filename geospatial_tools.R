# Packages
library(raster)


# Download and unzip data from online ----------------------------------------------------------------------------------------------------------

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
      message(paste0("Unzipping ", basename(download.link)))
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


# Example 
download.link = "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/gtc/downloads/treecover2010_v3_individual/80N_120W_treecover2010_v3.tif.zip"
download.link = "https://storage.googleapis.com/earthenginepartners-hansen/GFC2015/Hansen_GFC2015_treecover2000_40N_080W.tif"
download.folder = "D:/Geography/GIS_data/Vegetation/Tree data/hansen2013_treecover"
output = download.unzip(download.link, download.folder, unzip = FALSE)


# Test of compressing file after download ------------------------------------------------------------------------------------------------------
#   Currently has issues with pyramids in Arc, changing values in QGIS

# Read in raster and export compressed
raster_in = raster(output)
writeRaster(x=raster_in, filename = paste0(tools::file_path_sans_ext(output), "_comp.tif"), options=c("COMPRESS=DEFLATE", "ZLEVEL=1"))

# Delete original file
unlink(output)



