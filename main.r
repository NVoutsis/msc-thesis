# Setting working directory
setwd('/Users/nikosvoutsis/Desktop/MSc_Thesis')
# Split R script into 2 files, the "main" which inlcudes the structure and the code files and "utils" which includes the execution of the code files and the processes

# Source: for using the functions defined in the utils.R
source("utils.R")

# Loading required libraries for the script
library(readxl)
library(sf)
library(mapview)
library(raster)
library(rgdal)
install.packages('rgeos', type='source')
install.packages('rgdal', type='source')
library(maptools)
library(rgeos)
library(ggplot2)
library(dplyr)
library(moments)

################ PRE PROCESSING PHASE ################

## Points  ##
##### STEP 1: data correction, delete measurements with RMS>=2 or Elev_ortho<=-5
GNSS_df = read_excel("/Users/nikosvoutsis/Desktop/MSc_Thesis/GNSS_data_correction.xlsx")
# Re-install Rcpp package due to error (sf_library)
# install.packages('Rcpp')
# library(Rcpp)

##### STEP 2: Transformations
# Convert data frame to SF (Simple Features) points 
GNSS_points = st_as_sf(x = GNSS_df, 
                       coords = c("LONGITUDE", "LATITUDE"),
                       crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Show the projection of the SF object
st_crs(GNSS_points)

# Transform and SF object
st_transform(GNSS_points)
st_wrap_dateline(GNSS_points, options = "WRAPDATELINE=YES", quiet = TRUE)

# Simple plot
plot(st_geometry(GNSS_points))

# Interactive map of SF points:
mapview(GNSS_points)

# Convert to 'sp'(Spatial) object 
my.sp.point = as(GNSS_points, "Spatial")

###  SRTM  ###
srtm = raster("/Users/nikosvoutsis/Desktop/MSc_Thesis/Original_Data/srtm_41_04/srtm_41_04.tif")
plot(srtm)
mapview(srtm)

###  Tandem-X  ###
tandemx = raster("/Users/nikosvoutsis/Desktop/MSc_Thesis/Original_Data/TDM1_DEM__04_N40E023_DEM.tif")
plot(tandemx)
mapview(tandemx)

###  Perioxi_Meletis  ###
##### STEP 3: Subset
shp = readOGR("/Users/nikosvoutsis/Desktop/MSc_Thesis/MSc_Thesis", "periohi_meletis")
# Den diavaze to periohi meletis.shp opote: https://community.rstudio.com/t/error-in-readogr-function-cannot-open-data-source/76202
plot(shp)
mapview(shp)

# For validation, plot srtm and tandemx with shp added
plot(srtm)
plot(shp,add=T)
plot(tandemx)
plot(shp,add=T)
#den diavazei to shp otan einai apothikeumeno se egsa87, mono otan wgs84

# Cropping the DEMs into the area of interest
r1 = crop(srtm, shp)
plot(r1)
mapview(r1)
r2 = crop(tandemx,shp)
plot(r2)
mapview(r2)

##### STEP 4: Apply water body mask (GIS--->R?)
wb = readOGR("/Users/nikosvoutsis/Desktop/MSc_Thesis/MSc_Thesis","limnes")
plot(wb)
mapview(wb)

# For validation, plot srtm(r1) and tandemx(r2) with wb added
plot(r1)
plot(wb,add=T)
plot(r2)
plot(wb,add=T)
mapview(wb)
##### STEP 5: Reclassify
# Not needed

##### STEP 6: 
# Extract Values from a Raster in R, https://www.neonscience.org/resources/learning-hub/tutorials/extract-values-rasters-r
options(stringsAsFactors=FALSE)
GNSS_points2 = read.csv( "/Users/nikosvoutsis/Desktop/MSc_Thesis/GNSS_data_correction_test.csv", sep = ";")

GNSS_points2$LATITUDE = GNSS_points2$LATITUDE %>% gsub(pattern = ",", replacement = ".", fixed=T) %>% as.numeric()
GNSS_points2$LONGITUDE = GNSS_points2$LONGITUDE %>% gsub(pattern = ",", replacement = ".", fixed=T) %>% as.numeric()
GNSS_points2$ELEVcorr_a = GNSS_points2$ELEVcorr_a %>% gsub(pattern = ",", replacement = ".", fixed=T) %>% as.numeric()
head(GNSS_points2) 
my.sf.point = st_as_sf(x = GNSS_points2, 
                        coords = c("LONGITUDE", "LATITUDE"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Convert GNSS points to spatial points for data extraction
my.sp.point2 = as(my.sf.point, "Spatial")

# Plotting the extracted data (DEM with GNSS)
myCol = terrain.colors(7)
plot(r1, col=myCol, main="SRTM with GNSS points plotted");par(new=TRUE)

# Crop the GNSS points that are outside of the area of interest
GNSS_data = crop(my.sp.point2,shp)
plot(GNSS_data, add = T)
head(GNSS_data)

# Data extraction (TandemX_DEM), using the location of the GNSS points as overlays -> extraction of the same location pixel values 
extractdata <- raster::extract(r2, GNSS_data, weights=FALSE) # monodiastatos

# Plot data, histogram (TandemX_DEM, df)
df = data.frame(extractdata)
View(df)
df$id = seq.int(nrow(df))
x1 = df$id
y1 = df$extractdata
plot(x1,y1, main="TandemX_DEM", col.main="black", font.main=4, xlab = "GNSS", ylab="Elevation", type="l")

# Plot data, histogram (GNSS, df_gp)
df_gp = data.frame(GNSS_data)
View(df_gp)
df_gp$id = seq.int(nrow(df_gp))
x2 = df_gp$id
y2 = df_gp$ELEVcorr_a
plot(x2, y2, main="Elevation_GNSS", col.main="black", font.main=4, xlab = "GNSS", ylab="Elevation", type="l")

# Plot both and compare: TandemX_GNSS
x1 = df$id
y1 = df$extractdata
plot(x1, y1, main="Elevation Comparison: TandemX/GNSS", col.main="black", font.main=4, xlab = "GNSS points", ylab="Elevation", type="l", xlim=c(0,41000), ylim=c(0,1000))
x2 = df_gp$id
y2 = df_gp$ELEVcorr_a
lines(x2, y2, col="red")
legend(x=31000, y=900, c("TandemX","GNSS"), cex=.8, col=c("black","red"), lty=c(1,1))

# Data extraction (SRTM_DEM), using the location of the GNSS points as overlays -> extraction of the same location pixel values 
extractdata1 = raster::extract(r1, GNSS_data, weights=FALSE)

# Plot data, histogram (SRTM_DEM, df1) 
df1 = data.frame(extractdata1)
View(df1)
df1$id = seq.int(nrow(df1))
x3 = df1$id
y3 = df1$extractdata1
plot(x3, y3, main="SRTM_DEM", col.main="black", font.main=4, xlab = "GNSS", ylab="Elevation", type="l", xlim=c(0,39982), ylim=c(0,1000))

# Plot both and compare: SRTM_GNSS
x2 = df_gp$OBJECTID
y2 = df_gp$ELEVcorr_a
lines(x2, y2, col="red")
legend(x=31000, y=900, c("SRTM_DEM","GNSS"), cex=.8, col=c("black","red"), lty=c(1,1))

# Plot both and compare: SRTM_TandemX
x1 = df$id
y1 = df$extractdata
plot(x1, y1, main="Elevation Comparison: TandemX/SRTM", col.main="black", font.main=4, xlab = "GNSS points", ylab="Elevation", type="l", xlim=c(0,39982), ylim=c(0,1000))
x3 = df1$id
y3 = df1$extractdata1
lines(x3, y3, col="red")
legend(x=31000, y=900, c("TandemX","SRTM"), cex=.8, col=c("black","red"), lty=c(1,1))

##### STEP 7: resample raster (validating raster)
r = resample(r1, r2, method="bilinear")
plot(r)

################ MAIN PROCESSING PHASE ################
tandemx_data = extractdata
srtm_data = extractdata1
point_data = GNSS_data$ELEVcorr_a %>% gsub(pattern = ",", replacement = ".", fixed=T) %>% as.numeric()

##### STEP 1: Absolute and Relative differences
len = length(point_data)

# Absolute differences
absolute_diffs_rasters = get_list_point_difference(extractdata,extractdata1) # difference between Tandem-X vs SRTM 
absolute_diffs_gr_rd = get_list_point_difference(point_data,extractdata) # difference between Tandem-X and GNSS
absolute_diffs_br_rd = get_list_point_difference(point_data,extractdata1) # difference between SRTM and GNSS
# Relative difference
sample = generate_random_list(100,1,len) # get sample at random
random_GNSS_points = get_random_geo_points(sample,point_data)
random_Tandemx_points = get_random_geo_points(sample,extractdata)
random_SRTM_points = get_random_geo_points(sample,extractdata1)

# Get vector differences for each data list
GNSS_vector_diffs = get_point_difference(random_GNSS_points)
Tandemx_vector_diffs = get_point_difference(random_Tandemx_points)
SRTM_vector_diffs = get_point_difference(random_SRTM_points)

# Get relative differences 
relative_diffs_rasters = get_list_point_difference(Tandemx_vector_diffs,SRTM_vector_diffs) # difference between good and bad raster
relative_diffs_gr_rd = get_list_point_difference(Tandemx_vector_diffs,GNSS_vector_diffs) # difference between good raster and GNSS
relative_diffs_br_rd = get_list_point_difference(SRTM_vector_diffs,GNSS_vector_diffs) # difference between bad raster and GNSS

##### STEP 2: Statistics 
## Absolute Statistics
# Tandem-X vs SRTM 
#raster vs raster absolute difference
rvr_abs_df = get_statistics(absolute_diffs_rasters)
print(rvr_abs_df)
View(rvr_abs_df)

# GNSS vs Tandem-X 
#vector (point) vs raster (tandemx)
rdvgr_abs_df = get_statistics(absolute_diffs_gr_rd)
View(rdvgr_abs_df)

# GNSS vs SRTM 
#vector (point) vs raster (srtm)
rdvbr_abs_df = get_statistics(absolute_diffs_br_rd)
View(rdvbr_abs_df)

## Relative Statistics
# Tandem-X vs SRTM  
rvr_rel_df = get_statistics(relative_diffs_rasters)
View(rvr_rel_df)

# GNSS vs Tandem-X 
rdvgr_rel_df = get_statistics(relative_diffs_gr_rd)
View(rdvgr_rel_df)

# GNSS vs SRTM 
rdvbr_rel_df = get_statistics(relative_diffs_br_rd)
View(rdvbr_rel_df)

##### STEP 3: Histograms

# Absolute diffs
create_histogramm(absolute_diffs_rasters,"Absolute Elevation Error Comparison: TandemX/SRTM", x_title= "Elevation Error" , y_title ="Frequency")
create_histogramm(absolute_diffs_gr_rd,"Absolute Elevation Error Comparison: TandemX/GNSS", x_title= "Elevation Error" , y_title ="Frequency")
create_histogramm(absolute_diffs_br_rd,"Absolute Elevation Error Comparison: SRTM/GNSS", x_title= "Elevation Error" , y_title ="Frequency")

# Relative diffs
create_histogramm(relative_diffs_rasters,"Relative Elevation Error Comparison: TandemX/SRTM", x_title= "Elevation Error" , y_title ="Frequency")
create_histogramm(relative_diffs_gr_rd,"Relative Elevation Error Comparison: TandemX/GNSS", x_title= "Elevation Error" , y_title ="Frequency")
create_histogramm(relative_diffs_br_rd,"Relative Elevation Error Comparison: SRTM/GNSS", x_title= "Elevation Error" , y_title ="Frequency")

# !Cumulative Frequencies histograms! #

##### STEP 4: Correlation with slope
# Slope calculated from the most accurate and higher spatial resolution available dataset (TandemX)
x_slope = terrain(r2, v = "slope", unit = 'degrees')
plot(x_slope)

# Data extraction from slope DEM and data
extractdata_slope <- raster::extract(x_slope, GNSS_data, weights=FALSE)
df_slope = data.frame(extractdata_slope)
df_slope$id = seq.int(nrow(df_slope))
View(df_slope)
df_abs_diffs_tandemx = abs(data.frame(absolute_diffs_gr_rd))
df_abs_diffs_tandemx$id = seq.int(nrow(df_abs_diffs_tandemx))
View(df_abs_diffs_tandemx)
df_abs_diffs_srtm = abs(data.frame(absolute_diffs_br_rd))
df_abs_diffs_srtm$id = seq.int(nrow(df_abs_diffs_tandemx))
View(df_abs_diffs_srtm)
# Graph Elevation errors-slope: Absolute differences TandemX-GNSS and SRTM-GNSS
plot(df_slope$extractdata_slope,df_abs_diffs_tandemx$absolute_diffs_gr_rd, type = "l", xlab = "Degrees (°)", ylab = "Elevation Error (m)")
lines(df_slope$extractdata_slope,df_abs_diffs_srtm$absolute_diffs_br_rd, type = "l", col = "red")

legend("topright",
       legend = c("TandemX", "SRTM"),
       col = c("black", "red"),
       lty = 1)

# Graph Elevation errors-slope: Relative difference TandemX-GNSS and SRTM-GNSS
relative_diffs_gr_rd = get_list_point_difference(Tandemx_vector_diffs,GNSS_vector_diffs)
relative_diffs_br_rd = get_list_point_difference(SRTM_vector_diffs,GNSS_vector_diffs)
create_histogramm(relative_diffs_gr_rd,relative_diffs_br_rd,"Absolute Elevation Error Comparison: TandemX/GNSS", x_title= "Slope Class" , y_title ="Elevation Error")

#####  STEP 5: Correlation with corine
# Corine_df = as.data.frame(corine, xy = TRUE)
corine <- raster("/Users/nikosvoutsis/Desktop/MSc_Thesis/Corine/DATA/U2018_CLC2018_V2020_20u1.tif")
corine_transf <- spTRansform(corine, CRS(proj4string(shp)))
plot(corine_transf)
# Reproject clc raster file to the same projection as tandemx/srtm
corine_reproject <- projectRaster(corine,crs = crs(shp))

# Crop corine to subject area
corine_crop = crop(corine, shp)
plot(corine_crop)
plot(shp,add=T)
#for absolute kai relative diffs

##### STEP 6: maps for absolute diffs
elevation_map = overlay(r2,r1,fun=function(r1,r2) {return(r2-r1)})
