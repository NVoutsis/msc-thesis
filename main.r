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
mapview(shp)
crs(shp)

# For validation, plot srtm and tandemx with shp added
plot(srtm)
plot(shp,add=T)
plot(tandemx)
plot(shp,add=T)

# Cropping the DEMs into the area of interest
r1 = crop(srtm, shp)
mapview(r1)
r2 = crop(tandemx,shp)
mapview(r2)

##### STEP 4: Apply water body mask 
wb = readOGR("/Users/nikosvoutsis/Desktop/MSc_Thesis/MSc_Thesis","limnes")
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
GNSS_points2$ELEV_ortho = GNSS_points2$ELEV_ortho %>% gsub(pattern = ",", replacement = ".", fixed=T) %>% as.numeric()
head(GNSS_points2) 
my.sf.point = st_as_sf(x = GNSS_points2, 
                        coords = c("LONGITUDE", "LATITUDE"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Convert GNSS points to spatial points for data extraction
my.sp.point2 = as(my.sf.point, "Spatial")

# Plotting the extracted data (DEM with GNSS)
myCol = terrain.colors(7)
plot(r1, col=myCol, main="SRTM with GNSS points plotted");par(new=TRUE)
plot(r2, col=myCol, main="TANDEM-X with GNSS points plotted");par(new=TRUE)

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
plot(x1,y1, main="TandemX_DEM", col.main="black", font.main=4, xlab = "GNSS", ylab="Elevation (m)", type="l")

# Plot data, histogram (GNSS, df_gp)
df_gp = data.frame(GNSS_data)
View(df_gp)
df_gp$id = seq.int(nrow(df_gp))
x2 = df_gp$id
y2 = df_gp$ELEV_ortho
plot(x2, y2, main="Elevation_GNSS", col.main="black", font.main=4, xlab = "GNSS", ylab="Elevation (m)", type="l")

# Plot both and compare: TandemX_GNSS
x1 = df$id
y1 = df$extractdata
plot(x1, y1, main="Elevation Comparison: TandemX/GNSS", col.main="black", font.main=4, xlab = "GNSS points", ylab="Elevation (m)", type="l", xlim=c(0,41000), ylim=c(0,1000))
x2 = df_gp$id
y2 = df_gp$ELEV_ortho
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
plot(x2, y2, main="Elevation Comparison: SRTM/GNSS", col.main="black", font.main=4, xlab = "GNSS points", ylab="Elevation (m)", type="l", xlim=c(0,41000), ylim=c(0,1000))
x2 = df_gp$id
y2 = df_gp$ELEV_ortho
lines(x2, y2, col="red")
legend(x=31000, y=900, c("SRTM_DEM","GNSS"), cex=.6, col=c("black","red"), lty=c(1,1))

# Plot both and compare: SRTM_TandemX
x1 = df$id
y1 = df$extractdata
plot(x1, y1, main="Elevation Comparison: TandemX/SRTM", col.main="black", font.main=4, xlab = "GNSS points", ylab="Elevation (m)", type="l", xlim=c(0,39982), ylim=c(0,1000))
x3 = df1$id
y3 = df1$extractdata1
lines(x3, y3, col="red")
legend(x=31000, y=900, c("TandemX","SRTM"), cex=.7, col=c("black","red"), lty=c(1,1))

##### STEP 7: resample raster (validating raster)
r = resample(r1, r2, method="bilinear")
plot(r)

################ MAIN PROCESSING PHASE ################
tandemx_data = extractdata
srtm_data = extractdata1
point_data = GNSS_data$ELEV_ortho %>% gsub(pattern = ",", replacement = ".", fixed=T) %>% as.numeric()

##### STEP 1: Absolute and Relative differences
len = length(point_data)

# Absolute differences
absolute_diffs_rasters = get_list_point_difference(extractdata,extractdata1) # difference between Tandem-X vs SRTM 
absolute_diffs_tandx_gnss = get_list_point_difference(point_data,extractdata) # difference between Tandem-X and GNSS
absolute_diffs_srtm_gnss = get_list_point_difference(point_data,extractdata1) # difference between SRTM and GNSS

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
relative_diffs_rasters = get_list_point_difference(Tandemx_vector_diffs,SRTM_vector_diffs) # difference between Tandem-X vs SRTM 
relative_diffs_tandx_gnss = get_list_point_difference(Tandemx_vector_diffs,GNSS_vector_diffs) # difference between Tandem-X and GNSS
relative_diffs_srtm_gnss = get_list_point_difference(SRTM_vector_diffs,GNSS_vector_diffs) # difference between SRTM and GNSS

##### STEP 2: Statistics 
## Absolute Statistics
# Tandem-X vs SRTM 
#raster vs raster absolute difference
rvr_abs_df = get_statistics(absolute_diffs_rasters)
print(rvr_abs_df)
View(rvr_abs_df)

# GNSS vs Tandem-X absolute difference
#vector (point) vs raster (tandemx)
gnssvtandx_abs_df = get_statistics(absolute_diffs_tandx_gnss)
View(gnssvtandx_abs_df)

# GNSS vs SRTM absolute difference
#vector (point) vs raster (srtm)
gnssvsrtm_abs_df = get_statistics(absolute_diffs_srtm_gnss)
View(gnssvsrtm_abs_df)

## Relative Statistics
# Tandem-X vs SRTM relative difference
rvr_rel_df = get_statistics(relative_diffs_rasters)
View(rvr_rel_df)

# GNSS vs Tandem-X relative difference
tandxvgnss_rel_df = get_statistics(relative_diffs_tandx_gnss)
View(tandxvgnss_rel_df)

# GNSS vs SRTM relative difference
gnssvsrtm_rel_df = get_statistics(relative_diffs_srtm_gnss)
View(gnssvsrtm_rel_df)

##### STEP 3: Histograms
# Absolute diffs
create_histogramm(absolute_diffs_rasters,"Absolute Elevation Error Comparison: TandemX/SRTM", x_title= "Elevation Error (m)" , y_title ="Frequency")
create_histogramm(absolute_diffs_tandx_gnss,"Absolute Elevation Error Comparison: TandemX/GNSS", x_title= "Elevation Error (m)" , y_title ="Frequency")
create_histogramm(absolute_diffs_srtm_gnss,"Absolute Elevation Error Comparison: SRTM/GNSS", x_title= "Elevation Error (m)" , y_title ="Frequency")

# Relative diffs
create_histogramm(relative_diffs_rasters,"Relative Elevation Error Comparison: TandemX/SRTM", x_title= "Elevation Error (m)" , y_title ="Frequency")
create_histogramm(relative_diffs_tandx_gnss,"Relative Elevation Error Comparison: TandemX/GNSS", x_title= "Elevation Error (m)" , y_title ="Frequency")
create_histogramm(relative_diffs_srtm_gnss,"Relative Elevation Error Comparison: SRTM/GNSS", x_title= "Elevation Error (m)" , y_title ="Frequency")

##### STEP 4: Correlation with slope
# Slope calculated from the most accurate and higher spatial resolution available dataset (TandemX)
x_slope = terrain(r2, v = "slope", unit = 'degrees')
plot(x_slope, main = "Slope calculated from TANDEM-X DEM")

# Data extraction from slope DEM and sorted data for better plot
extractdata_slope <- raster::extract(x_slope, GNSS_data, weights=FALSE)
# Slope sorted data
gnss_abs_sorted = getSortAbsoluteList(extractdata_slope)
df_slope = data.frame(gnss_abs_sorted)
df_slope$id = seq.int(nrow(df_slope))
View(df_slope)
# Tandemx Absolute sorted data
tandemx_abs_sorted = getSortAbsoluteList(absolute_diffs_tandx_gnss)
df_abs_diffs_tandemx = data.frame(tandemx_abs_sorted)
df_abs_diffs_tandemx$id = seq.int(nrow(df_abs_diffs_tandemx))
View(df_abs_diffs_tandemx)
# SRTM Absolute sorted data
srtm_abs_sorted = getSortAbsoluteList(absolute_diffs_srtm_gnss)
df_abs_diffs_srtm = data.frame(srtm_abs_sorted)
df_abs_diffs_srtm$id = seq.int(nrow(df_abs_diffs_srtm))
View(df_abs_diffs_srtm)

# Slope Relative (Random 4950) data
sample = generate_random_list(4950,1,len) # get sample at random
random_slope = get_random_geo_points(sample,gnss_abs_sorted)
gnss_rel_sorted = getSortAbsoluteList(random_slope)
df_random_slope = data.frame(gnss_rel_sorted)
df_random_slope$id = seq.int(nrow(df_random_slope))
View(df_random_slope)
# Tandemx Relative sorted data
tandemx_rel_sorted = getSortAbsoluteList(relative_diffs_tandx_gnss)
df_rel_diffs_tandemx = data.frame(tandemx_rel_sorted)
df_rel_diffs_tandemx$id = seq.int(nrow(df_rel_diffs_tandemx))
View(df_rel_diffs_tandemx)
# SRTM Relative sorted data
srtm_rel_sorted = getSortAbsoluteList(relative_diffs_srtm_gnss)
df_rel_diffs_srtm = data.frame(srtm_rel_sorted)
df_rel_diffs_srtm$id = seq.int(nrow(df_rel_diffs_srtm))
View(df_rel_diffs_srtm)

# Graph Elevation errors-slope: Absolute differences TandemX-GNSS and SRTM-GNSS
plot(df_slope$gnss_abs_sorted,df_abs_diffs_tandemx$tandemx_abs_sorted, type = "l", xlab = "Degrees (°)", ylab = "Elevation Error (m)",  
     main = "Absolute elevation errors of DEMS vs Slope variations")
lines(df_slope$gnss_abs_sorted,df_abs_diffs_srtm$srtm_abs_sorted, type = "l", col = "red")

legend("topright",
       legend = c("TandemX", "SRTM"),
       col = c("black", "red"),
       lty = 1)

# Graph Elevation errors-slope: Relative differences TandemX-GNSS and SRTM-GNSS
plot(df_random_slope$gnss_rel_sorted,df_rel_diffs_tandemx$tandemx_rel_sorted, type = "l", xlab = "Degrees (°)", ylab = "Elevation Error (m)",  
     main = "Relative elevation errors of DEMS vs Slope variations")
lines(df_random_slope$gnss_rel_sorted,df_rel_diffs_srtm$srtm_rel_sorted, type = "l", col = "red")

legend("topright",
       legend = c("TandemX", "SRTM"),
       col = c("black", "red"),
       lty = 1)

#####  STEP 5: Correlation with corine
# Import corine and show crs
corine <- raster("/Users/nikosvoutsis/Downloads/u2006_clc2000_v2020_20u1_raster100m/DATA/U2006_CLC2000_V2020_20u1.tif")
plot(corine)
crs(corine)

# Reproject shp in order to be in the same crs with corine
shp_reproject <- spTransform(shp,crs(corine))
crs(shp_reproject)
plot(shp_reproject,add=T)

# Crop corine in subject area
corine_crop <- crop(corine,shp_reproject)
plot(corine_crop)

# Reproject GNSS data and plot as overlays
GNSS_data_reproject <-spTransform(GNSS_data,crs(corine))
plot(GNSS_data_reproject,add=T)

# Read clc classes and rename the levels of the raster file with this classes
clc_classes <- foreign::read.dbf("/Users/nikosvoutsis/Downloads/u2006_clc2000_v2020_20u1_raster100m/DATA/U2006_CLC2000_V2020_20u1.tif.vat.dbf",
                                 as.is = TRUE) %>%dplyr::select(value = Value,landcov = LABEL3)
print(clc_classes$landcov)

# Extract data from corine_crop using GNSS data and create dataframe
extractdata_corine <- extract(corine_crop$LABEL3, GNSS_data_reproject, weights=FALSE) # monodiastatos
View(extractdata_corine)
df_extractdata_corine = data.frame(extractdata_corine)
df_extractdata_corine$id = seq.int(nrow(df_extractdata_corine))
View(df_extractdata_corine)

# Sort all corine values 
extractdata_corine_sorted <- df_extractdata_corine[order(df_extractdata_corine$extractdata_corine),]
View(extractdata_corine_sorted)

# Unique values and count per land cover type
unique_values <- sort(unique(df_extractdata_corine$extractdata_corine))
sum(df_extractdata_corine$extractdata_corine == "") 

# Sort relative corine values
sample_corine = generate_random_list(4950,1,len) # get sample at random
random_corine = get_random_geo_points(sample_corine,extractdata_corine_sorted$extractdata_corine)
View(random_corine)
corine_rel_sorted = getSortAbsoluteList(random_corine)
df_random_corine = data.frame(corine_rel_sorted)
df_random_corine$id = seq.int(nrow(df_random_corine))
View(df_random_corine)

# Graph Elevation errors-land covers: Absolute differences TandemX-GNSS and SRTM-GNSS
plot(extractdata_corine_sorted$extractdata_corine,df_abs_diffs_tandemx$tandemx_abs_sorted, type = "p", xlab = "Land covers", ylab = "Elevation Error (m)",  
     main = "Absolute elevation errors of DEMs vs Corine land cover")
lines(extractdata_corine_sorted$extractdata_corine,df_abs_diffs_srtm$srtm_abs_sorted, type = "p", col = "red")

legend("topleft",
       legend = c("TandemX", "SRTM"),
       col = c("black", "red"),
       pch = 1)

# Graph Elevation errors-land covers: Relative differences TandemX-GNSS and SRTM-GNSS
plot(df_random_corine$corine_rel_sorted,df_rel_diffs_tandemx$tandemx_rel_sorted, type = "p", xlab = "Land covers", ylab = "Elevation Error (m)",  
     main = "Relative elevation errors of DEMs vs Corine land cover")
lines(df_random_corine$corine_rel_sorted,df_rel_diffs_srtm$srtm_rel_sorted, type = "p", col = "red")

legend("topleft",
       legend = c("TandemX", "SRTM"),
       col = c("black", "red"),
       pch = 1)

##### STEP 6: maps for absolute diffs between DEMs
# Error for different extends of r1 and r2, so we need to fix this. Setting the same extent
r2_ext <- crop(r1,r2)
plot(r2_ext)
plot(r1)

# DEM Substraction 
elev_diff_map <- overlay(r2_ext, r1, fun=function(r1,r2) {return(r1-r2)})
# Bad plot
plot(elev_diff_map, main = "Absolute elevation difference map - SRTM vs TANDEM-X")
