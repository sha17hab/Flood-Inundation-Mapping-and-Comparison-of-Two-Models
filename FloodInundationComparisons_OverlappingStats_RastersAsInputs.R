#
# Author: Shahab Afshari
# Email: safshar00@citymail.cuny.edu
# Civil Engineering Department/Water Resources Program
# City College of New York/City University of New York
# Name: FloodInundationComparisons_OverlappingStats_MatriciesAsInputs.R 
#
# Description: Requires three inputs (two inundation depth rasters, and one terrain model)
# all as *.tif images; performing inundation depth and extent comparison of two models by
# visual comparison and computed inundation statistics (e.g. F-overlapping statistics, 
# Kappa-statistics, etc.)
#
# Requirements: "raster", "rgdal", "sp", "rgeos", "maptool", "SDMTools", and "tcltk" packages.
install.packages("raster")
library("raster") # Geographic Data Analysis and Modeling
install.packages("rgdal")
library("rgdal") # Binding for the Geospatial Data Abstraction Library
install.packages("sp")
library("sp") # Sparse Linear Algebra
install.packages("rgeos")
library("rgeos") # Interface to Geometry Engine - Open Source (GEOS)
install.packages("maptools")
library("maptools") # Tools for Reading and Handling Spatial Objects
install.packages("SDMTools")
library("SDMTools") # Species Distribution Modelling Tools
library("tcltk") # Tcl/Tk Interface

# The script Introduction:
##
## The following script is made up of input assignment followed by
## the computation over the inputs.
##
## The user should import three ".tif" raster images as inputs,
## indicating: 1) flood inundation depth produced by model-1, 2) flood inundation
## depth produced by model-2, and 3) the terrain model 
##
## Assessment over the inputs will be made through two following approaches:
## 1) producing inundation maps and inter-comparison of inundation extent
## of model-1 with model-2 applying inundation statistics; The original raster 
## image while cell values are representing different ranges of depth will 
## be converted to a solid raster image where all cells will be modified to 
## have a uniform single value. (i.e. concerning model-1, -2 and the terrain raster, all 
## the raster cell values will be converted to 1, 2, and 0 respectively) 
## "Overall Accuracy Statistics", "F-overlapping Statistics", "Kappa Statistics", 
## and "Conditional Kappa Statistics" along with test of significance are being
## applied for evaluating the difference between the modified versions of model-1 and model-2 
## over the terrain. The model comparison results are being provided as: 1. a PDF file, containing
## schematic view of models intersecting inundated region along with ones being inundated by one model
## while being reported as dry by the other; 2. three Shapefile polygons, one representing models 
## intersecting inundated region and two additional shapefile each representing regions being 
## inundated by one model while being reported as dry by the other model.
##
## 2) comparison of the original inundation depth maps; taking the arithmetic 
## difference between the two models (1 and 2) where the intersecting region will 
## result in 0 (as a perfect match) to a large value (for a poor match). The region(s)
## being inundated only by either model will be reported as a different layer (having
## a different color) than intersecting region. RMSE (or MSE) will be computed considering
## intersecting and non-intersecting regions. 

#############################################################
# The Script:

## 1)
## set the working directory via setwd()

#### 1-1) Importing Inundation Depth Raster Images based on 
#### the percentage of wetted, dry, and NA cells. 

#load raster in an R object called 'DEM'
## Choose a TIFF (*.tif) image as the first inundation depth/extent , "Model-1.tif"
DEM_Flood1_0 = raster(tk_choose.files(caption = "Choose a TIFF (*.tif) image as the first inundation depth/extent"))
## Choose a TIFF (*.tif) Image as the second inundation depth/extent, "Model-2.tif"
DEM_Flood2_0 = raster(tk_choose.files(caption = "Choose a TIFF (*.tif) image as the second inundation depth/extent")) 
## Choose a TIFF (*.tif) Image as terrain model, "TerrainModel.tif"
DEM_Terrain = raster(tk_choose.files(caption = "Choose a TIFF (*.tif) Image as terrain model")) 
mainDir = setwd(tk_choose.dir(caption="Choose working directory"))#setwd("/Volumes/RAID1-2TB/virtualbox_shahab/Paper - MultiModelComaprison/RStudioCodes/") # Choose a working directory
# select a shapefile (*.shp) containing the user's required coordinate reference (or the projection
# ), e.g. GCS 1983 coordinate system, to be applied for exporting inundation maps as shapefiles possessing
# the just assigned coordinate system
Shapefile_as_Coordinate_Ref = shapefile(tk_choose.files(caption = "Select a shapefile (*.shp) containing the user's required coordinate reference ")) 
model1="Name_of_Model_1" # Assign a label name representing "Model-1.tif"
model2="Name_of_Model_2" # Assign a label name representing "Model-2.tif"

subDir_counter_Shp = 0
subDir_counter_PDF = 0
subDir_counter_TIFF = 0

subDir_Shp = "Shapefiles"
if (!file.exists(subDir_Shp)){
  dir.create(file.path(mainDir, subDir_Shp))
} else {
  subDir_counter_Shp = subDir_counter_Shp+1
  dir.create(file.path(mainDir, paste0(subDir_Shp,subDir_counter_Shp)))
}

subDir_PDF = "PDFs"
if (!file.exists(subDir_PDF)){
  dir.create(file.path(mainDir, subDir_PDF))
} else {
  subDir_counter_PDF = subDir_counter_PDF+1
  dir.create(file.path(mainDir, paste0(subDir_PDF,subDir_counter_PDF)))
}

subDir_TIFF = "TIFFs"
if (!file.exists(subDir_TIFF)){
  dir.create(file.path(mainDir, subDir_TIFF))
} else {
  subDir_counter_TIFF = subDir_counter_TIFF+1
  dir.create(file.path(mainDir, paste0(subDir_TIFF,subDir_counter_TIFF)))
}

### NOTE: the extent of "TerrainModel" raster is the baseline, that is, 
###       the cells of "Model-1" and "Model-2" if being mapped
###       beyond the baseline extent will be omitted/clipped

#### 1-2) cell resolution and projection comparison and adjustment; 
#### The coordinate refrence system of "Model-1" and "Model-2" 
#### will be compared with that of the "TerrainModel". 
#### "Model-1" and "Model-2" rasters (or their projected forms) will be resampled
#### to the spatial resolution similar to the resolution "TerrainModel" raster. 
DEM_Flood1 = DEM_Flood1_0
if (crs(DEM_Flood1,asText=T)!=crs(DEM_Terrain,asText=T)) {
  DEM_Flood1 = projectRaster(from = DEM_Flood1,to=DEM_Terrain)
} else if (res(DEM_Flood1)[1]!=res(DEM_Terrain)[1]) {
  DEM_Flood1 = resample(DEM_Flood1,DEM_Terrain,method="bilinear")
} 

DEM_Flood2 = DEM_Flood2_0
if (crs(DEM_Flood2,asText=T)!=crs(DEM_Terrain,asText=T)) {
  DEM_Flood2 = projectRaster(from = DEM_Flood2,to=DEM_Terrain);print("1")
} else if (res(DEM_Flood2)[1]!=res(DEM_Terrain)[1]) {
  DEM_Flood2 = resample(DEM_Flood2,DEM_Terrain,method="bilinear");print("2")
} 

#### 1-3) setting the coordinates of the origin of the "TerrainModel" raster as 
#### the coordinates of the point of origin of "Model-1" and "Model-2" rasters 
#### (or their projected forms)
if ((origin(DEM_Flood1)[1] != origin(DEM_Terrain)[1]) | (origin(DEM_Flood1)[2] != origin(DEM_Terrain)[2])) 
  origin(DEM_Flood1) = origin(DEM_Terrain)
if ((origin(DEM_Flood2)[1] != origin(DEM_Terrain)[1]) | (origin(DEM_Flood2)[2] != origin(DEM_Terrain)[2])) 
  origin(DEM_Flood2) = origin(DEM_Terrain)
rm(DEM_Flood2_0);rm(DEM_Flood1_0)

### UNCOMMENT THE LINES BELOW IF THE CELL RESOLUTIONS ARE INCONSISTENT
# cell resolution comparison; raster image having lower resolution will be resampled
# to the spatial resolution similar to finer resolution raster. 
# if (res(DEM_Flood1_0)[1]!=res(DEM_Flood2_0)[1]) {
#   if (res(DEM_Flood1_0)[1]<res(DEM_Flood2_0)[1]) {
#     DEM_Flood2 = resample(DEM_Flood2_0,DEM_Flood1_0,method="bilinear")
#   } else DEM_Flood1 = resample(DEM_Flood1_0,DEM_Flood2_0,method="bilinear")
# } else DEM_Flood2 = DEM_Flood2_0; DEM_Flood1 = DEM_Flood1_0


#############################################################
## 2)
####### Functions to be used in image processing ###########
# fun0: given the raster image (representing flood depth), it will convert
# cells having zero z-values (i.e. zero depth) to NA
# fun1: given the raster image, it will convert z-values of non-NA cells
# to 1 and NA cells to 0
# fun2: given the raster image, it will convert z-values of non-NA cells 
# to 0
# fun3_1, fun3_2, and fun3_3: given a raster image where cell values
# are classified into 0, 1, 2, and 3 categories, these functions are being
# applied to sort out the raster cells having a particular value, e.g. 1 that is 
# being captured by fun3_1, and exporting it a as new raster layer. 
# fun4: given the raster image (representing flood depth): 1. it will convert
# cells having zero z-values (i.e. zero depth) to NA, 2. converting non-NA cells 
# to 1, and 3. NA values (where also have been added from step 1) to 0
fun0 = function(x){ x[x<=0] = NA; return(x) }
fun1 = function(x) { x[!is.na(x)] = 1; x[is.na(x)] = 0;return(x) }
fun2 = function(x){ x[!is.na(x)] = 0; return(x) }
fun3_union = function(x){ x[x==1|x==2|x==3] = 1; return(x) }
fun3_1 = function(x){ x[x==0|x==2|x==3] = NA; x[x==1] = 0; return(x) }
fun3_2 = function(x){ x[x==0|x==1|x==3] = NA; x[x==2] = 0; return(x) }
fun3_3 = function(x){ x[x==0|x==1|x==2] = NA; x[x==3] = 0; return(x) }
fun4 = function(x){ x[x<=0] = NA; x[!is.na(x)] = 1; x[is.na(x)] = 0; return(x) }

#############################################################
## 3)
# considering the "TerrainModel.tif" or "DEM_Terrain" as the extent for clipping 
# "Model-1.tif" and "Model-2.tif" ("DEM_Flood1" and "DEM_Flood2")
DEM_Flood1 = intersect(calc(DEM_Flood1, fun0),DEM_Terrain*0)
DEM_Flood2 = intersect(calc(DEM_Flood2, fun0),DEM_Terrain*0)
# calculate and save the min and max values of the raster to the raster object
DEM_Flood1 = setMinMax(DEM_Flood1)
DEM_Flood2 = setMinMax(DEM_Flood2)
DEM_Terrain = setMinMax(DEM_Terrain)

### UNCOMMENT THE LINES BELOW IF THE CELL RESOLUTIONS ARE INCONSISTENT
# DEM_Flood1 = resample(DEM_Terrain, DEM_Flood1, method="ngb")
# DEM_Flood2 = resample(DEM_Terrain, DEM_Flood2, method="ngb")
# DEM_Flood2 = calc(DEM_Flood2,fun4)

#### Generating Binary and Discrete Inundation Raster Layers From Depth Rasters ###########
oldw <- getOption("warn")
options(warn = -1)
### 3-1) Flood raster where z-values are classified into 0, 1, 2, and 3 category
DEM_FloodClassified = calc(DEM_Flood1, fun1)+
  calc(DEM_Flood2, fun1)*2+
  calc(DEM_Terrain,fun2) 

### 3-2) Union of flood raster layers, 0 being the union and NA as non-flooded zones 
DEM_FloodUnion = calc(DEM_FloodClassified,fun3_union)

### 3-3) Intersect of flood raster layers, 0 being the intersect and NA as non-flooded or non-intersected zones
DEM_FloodIntersect = calc(DEM_FloodClassified,fun3_3)

### 3-4) Unique flooded zones by a flood raster 1 while it is not flooded by flood raster 2, 
### 0 being the floode zones and NA as non-flooded or non-intersected zones
DEM_FloodClassified_1 = calc(DEM_FloodClassified,fun3_1)

### 3-5) Unique flooded zones by a flood raster 2 while it is not flooded by flood raster 1,
### 0 being the floode zones and NA as non-flooded or non-intersected zones
DEM_FloodClassified_2 = calc(DEM_FloodClassified,fun3_2)
options(warn = oldw)

## 4) 
### Numerical and Visual Comparison of Models

### 4-1) Converting Raster into Matrix
#### Initialization for calculating inundation statistics
S1 = as.matrix(DEM_FloodClassified)
#quick view of the matrix (raster), where cell values are
# 0: Not flooded by both "Model-1.tif" and "Model-2.tif" ("DEM_Flood1" and "DEM_Flood2")
# 1: Only flooded by "Model-1.tif" ("DEM_Flood1")
# 2: Only flooded by "Model-2.tif" ("DEM_Flood2")
# 3: Flooded both by "Model-1.tif" and "Model-2.tif" ("DEM_Flood1" and "DEM_Flood2")
#plot(DEM_FloodClassified, main="Digital Elevation Model")
#we can look at the distribution of values in the raster too
S1_dum = S1[which(!is.na(S1)==T)]
library(lattice)
histogram(S1[which(!is.na(S1)==T)], 
     main="Distribution of elevation values", 
     xlab="Modified Raster, Cell Values",
     ylab="Relative Frequency (%)",
     col= "purple")
# length(S1[which(!is.na(S1)==T)])

Cell_size = 10 # A cell is regarded as a square land of 100 sq.m area (10mx10m)
NumRow = nrow(S1) # Number of cells in a row 
NumCol = ncol(S1) # Number of cells in a column 
S1_Number_of_NoDATA_cells = length(which(is.na(S1)==T))  
S1_Number_of_dry_cells = length(which(S1==0)) #S1_Percent_of_total_area_to_be_dry*NumRow*NumCol
S1_Number_of_inundated_cells = length(which(S1==1|S1==2|S1==3)) #S1_Percent_of_total_area_to_be_inundated*NumRow*NumCol
Total_cell_num = S1_Number_of_NoDATA_cells+S1_Number_of_dry_cells+S1_Number_of_inundated_cells
S1_Percent_of_total_area_to_be_NoDATA = S1_Number_of_NoDATA_cells/Total_cell_num # % of NA's
S1_Percent_of_total_area_to_be_dry = S1_Number_of_dry_cells/Total_cell_num
S1_Percent_of_total_area_to_be_inundated = S1_Number_of_inundated_cells/Total_cell_num
S1_Dry_Area  = S1_Number_of_dry_cells*Cell_size*Cell_size # or the S1-floodmap dry area
S1_Inundation_Area  = S1_Number_of_inundated_cells*Cell_size*Cell_size # or the S1-floodmap inundated area

vector_of_ones = rep(1,S1_Number_of_inundated_cells) #replicating 1s "S1_Number_of_inundated_cells" times, representing wet cells
vector_of_zeros = rep(0,S1_Number_of_dry_cells) #replicating 0s "S1_Number_of_inundated_cells" times, representing dry cells
vector_of_NoDATA = rep(NA,S1_Number_of_NoDATA_cells) # vector of NA's

## 4-2) Calculation of key variables for computing Overall Accuracy,
## Kappa-statisitcs, Conditional Kappa-statisitcs, and
## F-Overlapping statistics
S = S1
n0_S1dry_AND_S2dry = as.numeric(length(which(S==0))) # S1 and S2 are both dry
n1_S1wet_AND_S2dry = as.numeric(length(which(S==1))) # S1 is wet and S2 is dry
n2_S1dry_AND_S2wet = as.numeric(length(which(S==2))) # S1 is dry and S2 is wet
n3_S1wet_AND_S2wet = as.numeric(length(which(S==3))) # S1 and S2 are both wet
n = as.numeric(NumRow*NumCol-S1_Number_of_NoDATA_cells)
## NOTE: "length(which(S==0))" will produce a value "integer" where, based on the allocated 
## 32bit memory for the integers, the maximum is limited to 2147483647. Therefore, any operation
## on integers where the product is larger than this limit will return NA. 
## "as.numeric()" will resolve this issue by turning "integers" into "doubles"
## which is 1.797693e+308 and gives more room for the extremly large/small numbers. 

### Kappa Statistics
Kappa_hat_statistics = (n*(n0_S1dry_AND_S2dry+n3_S1wet_AND_S2wet)-
                          (n2_S1dry_AND_S2wet+n3_S1wet_AND_S2wet)*
                          (n1_S1wet_AND_S2dry+n3_S1wet_AND_S2wet))/
  (n*n-(n2_S1dry_AND_S2wet+n3_S1wet_AND_S2wet)*(n1_S1wet_AND_S2dry+n3_S1wet_AND_S2wet))
### Overall Accuracy Statistics
Overall_Accuracy = (n0_S1dry_AND_S2dry+n3_S1wet_AND_S2wet)/n
### F-Overlapping Statistics
F_Overallping_Stat = n3_S1wet_AND_S2wet/(  # intersected wet area
  (n3_S1wet_AND_S2wet+n1_S1wet_AND_S2dry)+ # S2 being either dry/wet given S1 is wet
    (n3_S1wet_AND_S2wet+n2_S1dry_AND_S2wet)- # S1 being either dry/wet given S2 is wet
    n3_S1wet_AND_S2wet # intersected wet area
)
### Conditional Kappa Statistics 
CondiKappa_hat_statistics_wet = (n*n3_S1wet_AND_S2wet-
                                   (n3_S1wet_AND_S2wet+n1_S1wet_AND_S2dry)*
                                   (n3_S1wet_AND_S2wet+n2_S1dry_AND_S2wet))/
  (n*(n3_S1wet_AND_S2wet+n2_S1dry_AND_S2wet)-
     (n3_S1wet_AND_S2wet+n1_S1wet_AND_S2dry)*
     (n3_S1wet_AND_S2wet+n2_S1dry_AND_S2wet))
CondiKappa_hat_statistics_dry = (n*n0_S1dry_AND_S2dry-
                                   (n0_S1dry_AND_S2dry+n2_S1dry_AND_S2wet)*
                                   (n0_S1dry_AND_S2dry+n0_S1dry_AND_S2dry))/
  (n*(n1_S1wet_AND_S2dry+n0_S1dry_AND_S2dry)-
     (n0_S1dry_AND_S2dry+n2_S1dry_AND_S2wet)*
     (n0_S1dry_AND_S2dry+n0_S1dry_AND_S2dry))

n1_S1wet_AND_S2dry = length(which(S==1)) # S1 is wet and S2 is dry
n2_S1dry_AND_S2wet = length(which(S==2)) # S1 is dry and S2 is wet

### Z-statistics computed for Conditional Kappa
m = matrix(0,ncol=NumCol,nrow=NumRow)
partition_frame_size = NumRow-10 # number of cell in a row or column ()
Kappa_hat_statistics_i = matrix(NA,
                                nrow=(ncol(m)-partition_frame_size+1)*
                                  (nrow(m)-partition_frame_size+1),
                                ncol=1)
i_counter = 1
for (i_col in 1:(ncol(m)-partition_frame_size+1))
{
  for (i_row in 1:(nrow(m)-partition_frame_size+1))
  {
    m[i_row:(i_row+partition_frame_size-1),
      i_col:(i_col+partition_frame_size-1)]=1
    N = prod(dim(m))/partition_frame_size/partition_frame_size
    M = S[which(m==1)]
    dim(M) = c(partition_frame_size,partition_frame_size)
    n0_S1dry_AND_S2dry = as.numeric(length(which(M==0))) 
    n1_S1wet_AND_S2dry = as.numeric(length(which(M==1)))
    n2_S1dry_AND_S2wet = as.numeric(length(which(M==2)))
    n3_S1wet_AND_S2wet = as.numeric(length(which(M==3)))
    n = as.numeric(nrow(M)*ncol(M))
    Kappa_hat_statistics_i[i_counter,1] = 
      (n*(n0_S1dry_AND_S2dry+n3_S1wet_AND_S2wet)-
         (n2_S1dry_AND_S2wet+n3_S1wet_AND_S2wet)*
         (n1_S1wet_AND_S2dry+n3_S1wet_AND_S2wet))/
      (n*n-(n2_S1dry_AND_S2wet+n3_S1wet_AND_S2wet)*(n1_S1wet_AND_S2dry+n3_S1wet_AND_S2wet))
    i_counter = i_counter+1
    m = matrix(0,ncol=NumCol,nrow=NumRow)
  }
}
rm(m);rm(M1);rm(M2);rm(n0_S1dry_AND_S2dry);rm(n1_S1wet_AND_S2dry)
rm(n2_S1dry_AND_S2wet);rm(n3_S1wet_AND_S2wet)

hist(Kappa_hat_statistics_i)
abline(v=Kappa_hat_statistics,col="red")

### Hypothesis Test on "Kappa_hat_statistics"
#### H0: Kappa = 1
#### Ha: Kappa < 1
#### alpha = probability of Kappa < 1 given Kappa = 1 (Probability of making wrong decision when H0 is true)
alpha = 0.05 # 95% confidence level
#### Determining to us Z-test of t-test; Computing p-value, Type II error, and power of the hypothesis test
if (i_counter>30) {
  Z = (Kappa_hat_statistics-1)/sd(Kappa_hat_statistics_i)
  #Z = (mean(Kappa_hat_statistics_i)-1)/sd(Kappa_hat_statistics_i)
  pvalue = 2*pnorm(-abs(Z))
} else {
  t = (Kappa_hat_statistics-1)/sd(Kappa_hat_statistics_i)  
  #t = (mean(Kappa_hat_statistics_i)-1)/sd(Kappa_hat_statistics_i)  
  pvalue = 2*pt(-abs(t),df=i_counter-1)
}
print(pvalue) # if pvalue > 0.05, then Kappa is statistically equal to 1 (i.e. Null Hypothesis is accepted), 
# otherwise there is not enough evidence to accept it to be 1

### 4-3) Visual comparison of "Flood Model 1" vs "Flood Model 2"
pdf(paste0(file.path(mainDir, subDir_PDF),"/",model1,"VS",model2,".pdf"),paper = "USr",width = 10,height = 10)
#add a color map with 5 colors
col=c("#EEE9BF","#00EEEE","#F08080","#1874CD","#1874CD")
#add breaks to the colormap (6 breaks = 5 segments)
brk = c(-1,0, 1, 2, 3) 
#expand right side of clipping rect to make room for the legend
par(xpd = FALSE,mar=c(5.1, 4.1, 4.1, 4.5))
#DEM with a custom legend 
plot(DEM_FloodClassified, # Raster (having classified symbology)
     col=col, # Color of classes (domains)
     breaks=brk, # Number of breaks for legend's color bar
     main="Flood over the Assigned NED", # Plot title
     legend = FALSE) # Disabling the custom legend form to be added
#turn xpd back on to force the legend to fit next to the plot.
par(xpd = TRUE)
#add a legend - but make it appear outside of the plot 
legend( par()$usr[1]+500, #x-coordinate of upper-left corner of legend box
        par()$usr[4]-500, #y-coordinate of upper-left corner of legend box
        legend = c("Dry Zone",
                            eval(bquote(expression(paste(.(model1)," ", intersect(" "),"(",.(model1)," ",union(" "), .(model2),")")))),
                            eval(bquote(expression(paste(.(model2)," ",intersect(" "),"(",.(model1)," ",union(" "),.(model2),")")))),
                            eval(bquote(expression(paste(.(model1)," ",intersect(" "),.(model2)))))),
        bty = "n",fill = col,cex=1)
legend( par()$usr[2]-4500, #x-coordinate of upper-left corner of legend box
        par()$usr[3]+2500, #y-coordinate of upper-left corner of legend box
        legend = c(sprintf("Overal Accu. = %0.3f",Overall_Accuracy), 
                   sprintf("Kappa = %0.3f",Kappa_hat_statistics), 
                   sprintf("Condi. Kappa Wet = %0.3f",CondiKappa_hat_statistics_wet),
                   sprintf("F-Overlapping = %0.3f",F_Overallping_Stat)),
        bty = "n",fill = "black",cex=1,pch=NA,title="Inundation Statistics")
Scalebar(x=par()$usr[1]+500,par()$usr[3]+400,distance=4000,unit='km',scale=0.001)

dev.off()

### 4-4) Exporting outline boundaries of rasters as .shp (shapefile)
#### NOTE: The coordinate reference system (crs) of the exported shapefile will be set to that 
#### of an existing shapefile based on the users' choice. Therefore, the reference shapefile must be
#### imporated and assigned to "Shapefile_as_Coordinate_Ref" variable. 

crs(Shapefile_as_Coordinate_Ref)
# FloodIntersect
pol_FloodIntersect = rasterToPolygons(DEM_FloodIntersect,dissolve = T)
pol_FloodIntersect_trnsfm = spTransform(pol_FloodIntersect, crs(Shapefile_as_Coordinate_Ref))
writeOGR(pol_FloodIntersect_trnsfm,dsn=".",layer=paste0(file.path(mainDir, subDir_Shp),"/",model1,"VS",model2,"_intersect"), driver="ESRI Shapefile")
# FloodClassified 1
pol_FloodClassified_1 = rasterToPolygons(DEM_FloodClassified_1,dissolve = T)
pol_FloodClassified_1_trnsfm = spTransform(pol_FloodClassified_1, crs(Shapefile_as_Coordinate_Ref))
writeOGR(pol_FloodClassified_1_trnsfm,dsn=".",layer=paste0(file.path(mainDir, subDir_Shp),"/",model1,"VS",model2,"_",model1), driver="ESRI Shapefile")
# FloodClassified 2
pol_FloodClassified_2 = rasterToPolygons(DEM_FloodClassified_2,dissolve = T)
pol_FloodClassified_2_trnsfm = spTransform(pol_FloodClassified_2, crs(Shapefile_as_Coordinate_Ref))
writeOGR(pol_FloodClassified_2_trnsfm,dsn=".",layer=paste0(file.path(mainDir, subDir_Shp),"/",model1,"VS",model2,"_",model2), driver="ESRI Shapefile")

#############################################################
## 5) 
#### Assessment over depth rasters
options(warn = -1)
### 5-1) Intersect of flood raster layers 
DEM_FloodDepthIntersect = DEM_Flood1-DEM_Flood2
### 5-2) Unique flooded zones by a flood raster 1 while it is not flooded by flood raster 2,
DEM_FloodDepthClassified_1 = DEM_Flood1+calc(DEM_FloodClassified,fun3_1)
### 5-3) Unique flooded zones by a flood raster 2 while it is not flooded by flood raster 1,
DEM_FloodDepthClassified_2 = DEM_Flood2+calc(DEM_FloodClassified,fun3_2)
### 5-4) Union of flood raster layers
DEM_FloodDepthUnion = 
  merge(DEM_FloodDepthIntersect, 
        DEM_FloodDepthClassified_1, 
        DEM_FloodDepthClassified_2)
options(warn = oldw)

### 5-5) write to a new geotiff file (depends on rgdal)

writeRaster(DEM_FloodDepthIntersect, 
            filename=paste0(file.path(mainDir, subDir_TIFF),
                            "/",model1,"VS",model2,"_intersect",".tif"),
            format="GTiff", overwrite=TRUE)
writeRaster(DEM_FloodDepthClassified_1, 
            filename=paste0(file.path(mainDir, subDir_TIFF),
                            "/",model1,"VS",model2,"_",model1,".tif"), 
            format="GTiff", overwrite=TRUE)
writeRaster(DEM_FloodDepthClassified_2, 
            filename=paste0(file.path(mainDir, subDir_TIFF),
                            "/",model1,"VS",model2,"_",model2,".tif"), 
            format="GTiff", overwrite=TRUE)

####################### THIS IS THE ENDING LINE OF THE SCRIPT ######################


