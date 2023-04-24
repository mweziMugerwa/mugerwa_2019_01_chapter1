# clean memory
rm(list=ls())
gc()

# install libraries
# install.packages("classInt")
# install.packages("maps")
# install.packages("sp")
# install.packages("reshape")
# install.packages("ggmosaic")
# install.packages("raster")
# install.packages("cowplot")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("magrittr")
# install.packages("tidyr")
install.packages("fasterize")

# load libraries
library(classInt)
library(maps)
library(sp)
library(reshape)
library(ggmosaic)
library(raster)
library(cowplot)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(sf)
library(fasterize)

# The functions
# The function that produces the colour matrix
colmat <- function(nquantiles = 3, upperleft = "#0096EB", upperright = "#820050", 
                   bottomleft = "#BEBEBE", bottomright = "#FFE60F",
                   xlab = "x label", ylab = "y label", plotLeg = TRUE,
                   saveLeg = TRUE) {
  require(dplyr)
  require(tidyr)
  require(ggplot2)
  require(magrittr)
  require(classInt)
  
  my.data <- seq(0, 1, .01)
  
  # divide range from 0-1 into nquantiles
  # Default uses terciles (Lucchesi and Wikle [2017] doi: 10.1002/sta4.150)
  my.class <- classInt::classIntervals(my.data,
                                       n = nquantiles,
                                       style = "quantile" )
  # interpolate colors between the corners along both axes (returns 101 values irrespective of the number of intervals)
  my.pal.1 <- findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- findColours(my.class, c(upperright, bottomright))
  
  # create empty color matrix
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  
  # loop over all rows and assign interpolated colors to each cell
  for (i in 1:101) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[102 - i, ] <- findColours(my.class, my.col)
  }
  
  col.matrix.plot <- col.matrix %>%
    as.data.frame(.) %>% 
    mutate("Y" = row_number()) %>%
    mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>% 
    pivot_longer(data = ., cols = -Y, names_to = "X", values_to = "HEXCode") %>% 
    mutate("X" = as.integer(sub("V", "", .$X))) %>%
    distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
    mutate(Y = rev(.$Y)) %>% 
    dplyr::select(-c(4)) %>%
    mutate("Y" = rep(seq(from = 1, to = nquantiles, by = 1), each = nquantiles),
           "X" = rep(seq(from = 1, to = nquantiles, by = 1), times = nquantiles)) %>%
    mutate("UID" = row_number())
  
  # Use plotLeg if you want a preview of the legend
  if (plotLeg) {
    p <- ggplot(col.matrix.plot, aes(X, Y, fill = HEXCode)) +
      geom_raster() +
      scale_fill_identity() +
      coord_equal(expand = FALSE) +
      theme_void() +
      theme(aspect.ratio = 1,
            axis.title = element_text(size = 12, colour = "black",hjust = 0.5, 
                                      vjust = 1),
            axis.title.y = element_text(angle = 90, hjust = 0.5)) +
      xlab(bquote(.(xlab) ~  symbol("\256"))) +
      ylab(bquote(.(ylab) ~  symbol("\256")))
    print(p)
    assign(
      x = "BivLegend",
      value = p,
      pos = .GlobalEnv
    )
  }
  # Use saveLeg if you want to save a copy of the legend
  if (saveLeg) {
    ggsave(filename = "bivLegend.pdf", plot = p, device = "pdf",
           path = "./", width = 4, height = 4, units = "in",
           dpi = 300)
  }
  
  # create sequence from 0 to 100, equally spaced
  seqs <- seq(0, 100, (100 / nquantiles))
  
  # replace 1st value (0) with 1
  seqs[1] <- 1
  
  # subset the color matrix to the colors at the breakpoints
  col.matrix <- col.matrix[c(seqs), c(seqs)]
}

# Function to assign colour-codes to raster data
# As before, by default assign tercile breaks
bivariate.map <- function(rasterx, rastery, colormatrix = col.matrix,
                          nquantiles = 3, export.colour.matrix = TRUE,
                          outname = paste0("colMatrix_rasValues", names(rasterx))) {
  # export.colour.matrix will export a data.frame of rastervalues and RGB codes 
  # to the global environment outname defines the name of the data.frame
  
  # extract raster values of first raster
  quanmean <- getValues(rasterx)
  
  # make data frame containing raster values and a placeholder for the quantile group
  temp <- data.frame(quanmean, quantile = rep(NA, length(quanmean)))
  
  # calculate quantile breaks of the raster values (1 more than nquantiles)
  brks <- with(temp, quantile(temp,
                              na.rm = TRUE,
                              probs = c(seq(0, 1, 1 / nquantiles))
  ))
  ## Add (very) small amount of noise to all but the first break
  ## https://stackoverflow.com/a/19846365/1710632
  #JN: Didn't check why this would be necessary
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  
  # assign quantile group to each raster value (group 1 is omitted, so for 3 quantiles the values are 2,3,4)
  r1 <- within(temp, quantile <- cut(quanmean,
                                     breaks = brks,
                                     labels = 2:length(brks),
                                     include.lowest = TRUE
  ))
  quantr <- data.frame(r1[, 2])
  
  # same for raster 2
  quanvar <- getValues(rastery)
  temp <- data.frame(quanvar, quantile = rep(NA, length(quanvar)))
  brks <- with(temp, quantile(temp,
                              na.rm = TRUE,
                              probs = c(seq(0, 1, 1 / nquantiles))
  ))
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  r2 <- within(temp, quantile <- cut(quanvar,
                                     breaks = brks,
                                     labels = 2:length(brks),
                                     include.lowest = TRUE
  ))
  quantr2 <- data.frame(r2[, 2])
  
  # define function that converts factor labels to numeric
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  
  
  col.matrix2 <- colormatrix
  
  # remove 1st row of color matrix (it is identical to 2nd row)
  # nevertheless 1st column = 2nd column
  cn <- unique(colormatrix)
  
  # loop over all cells of the original color matrix
  # if the color is NA, replace cell with 1, otherwise the the first match of the color i in cn
  # I don't understand why. In my tests with 5 breaks it's always no
  for (i in 1:length(col.matrix2)) {
    ifelse(is.na(col.matrix2[i]), 
           col.matrix2[i] <- 1,
           col.matrix2[i] <- which(col.matrix2[i] == cn)[1]
    )
  }
  # Export the colour.matrix to data.frame()
  if (export.colour.matrix) {
    # create a dataframe of colours corresponding to raster values
    exportCols <- as.data.frame(cbind(
      as.vector(col.matrix2), as.vector(colormatrix),
      t(col2rgb(as.vector(colormatrix)))
    ))
    # rename columns of data.frame()
    colnames(exportCols)[1:2] <- c("rasValue", "HEX")
    # Export to the global environment
    assign(
      x = outname,
      value = exportCols,
      pos = .GlobalEnv
    )
  }
  
  # create vector of 0s, same length as rows in quantr (= number of cells in rasterx)
  cols <- numeric(length(quantr[, 1]))
  
  # loop over all raster cells, and assign color value to each cell
  for (i in 1:length(quantr[, 1])) {
    a <- as.numeric.factor(quantr[i, 1])
    b <- as.numeric.factor(quantr2[i, 1])
    cols[i] <- as.numeric(col.matrix2[b, a])
  }
  # template raster
  r <- rasterx
  # assign color values to each raster cell
  r[1:length(r)] <- cols
  
  return(r)
}

# Function to Create the colour matrix
# Define the number of breaks
nBreaks <- 5
col.matrix <- colmat(nquantiles = nBreaks, xlab = "Maxent", ylab = "Aichi", 
                     ## non default colours
                     # upperleft = "#F7900A", upperright = "#993A65", 
                     # bottomleft = "#44B360", bottomright = "#3A88B5",
                     
                     # example colors like the one example I shared
                     # not ideal also, looks like scorched earth
                     bottomleft = "grey90",
                     upperleft = "#ff4704", 
                     upperright =  "grey10", 
                     bottomright = "#04eaff",
                     
                     saveLeg = FALSE, plotLeg = TRUE)

# function to normalize rasters

# not necessary I believe. bivariate map function calculates quantiles
normalizeRaster <- function(x){
  out <- (x - cellStats(x, min)) / (cellStats(x, max) - cellStats(x, min))  
  return(out)
}

# funtion to throw/overlay points on the matrix
nquantiles <- 5
bottomleft = "grey90"
upperleft = "#ff4704"
upperright =  "grey10"
bottomright = "#04eaff"

my.data <- seq(0, 1, .01)
# Default uses terciles (Lucchesi and Wikle [2017] doi: 10.1002/sta4.150)
my.class <- classInt::classIntervals(my.data,
                                     n = nquantiles,
                                     style = "quantile" )
my.pal.1 <- findColours(my.class, c(upperleft, bottomleft))
my.pal.2 <- findColours(my.class, c(upperright, bottomright))
col.matrix <- matrix(nrow = 101, ncol = 101, NA)
for (i in 1:101) {
  my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
  col.matrix[102 - i, ] <- findColours(my.class, my.col)
}
col.matrix.plot <- col.matrix %>%
  as.data.frame(.) %>%
  mutate("Y" = row_number()) %>%
  mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>%
  pivot_longer(data = ., cols = -Y, names_to = "X", values_to = "HEXCode") %>%
  mutate("X" = as.integer(sub("V", "", .$X))) %>%
  distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
  mutate(Y = rev(.$Y)) %>%
  dplyr::select(-c(4)) %>%
  mutate("Y" = rep(seq(from = 1, to = nquantiles, by = 1), each = nquantiles),
         "X" = rep(seq(from = 1, to = nquantiles, by = 1), times = nquantiles)) %>%
  mutate("UID" = row_number())

p <- ggplot(col.matrix.plot, aes(X, Y, fill = HEXCode)) +
  geom_raster() +
  scale_fill_identity() +
  coord_equal(expand = FALSE) +
  theme_void() +
  theme(aspect.ratio = 1,
        axis.title = element_text(size = 12, colour = "black",hjust = 0.5,
                                  vjust = 1),
        axis.title.y = element_text(angle = 90, hjust = 0.5)) +
  xlab(bquote(.(xlab) ~  symbol("\256"))) +
  ylab(bquote(.(ylab) ~  symbol("\256")))
print(p)
assign(
  x = "BivLegend",
  value = p,
  pos = .GlobalEnv
)

# dir
# dir = "~/BioDivCloud/Mwezi_B_Mugerwa/IMac/IZW/PhD/Research/Chapters/Chapter 1/Analysis/"
# dir = "~/BioDivCloud/Mwezi_B_Mugerwa/IMac/IZW/my_PhD/mugerwa_2019_01_chapter1/"
dir <- "D:/BioDivCloud/Mwezi_B_Mugerwa/IMac/IZW/my_PhD/mugerwa_2019_01_chapter1/"

# load in the biodcrisis map
biodcrisis_map_tif_orig <- raster(paste0(dir,"/output/geo-proc/biodcrisis-maps/Biome1biodcrisis_map_FinalEEExtMatch3.tif"));plot(biodcrisis_map_tif_orig)

# load in the camera trapping allocation map
camtrap_effort_tif_orig <- raster(paste0(dir,"/output/geo-proc/predictions/Biome1_predictionFinal.tif"));plot(biodcrisis_map_tif_orig)

# normalize rasters to 0 and 1
camtrap_effort <- normalizeRaster(camtrap_effort_tif_orig);extent(camtrap_effort) 
biodcrisis_map <- normalizeRaster(biodcrisis_map_tif_orig );extent(biodcrisis_map)

# create a stack of both covariates
s <- stack(list(maxent = camtrap_effort,biodcrisis = biodcrisis_map))

# define area of interest
# aoi <- extent(52, 180, -25, 39)# Asia and Australia
aoi <- extent(-20, 60, -35, 40)# Africa
# aoi <- extent(-180, -20, -55, 90) # Americas
# aoi <- extent(-120, -20, -60, 40)# S. America
# aoi_polygon <- as(aoi, "SpatialPolygons")

# clip to area of interest
s_crop <- crop(s, aoi)
s_agg_norm <- aggregate(s_crop, fact = 10)

# create the bivariate raster
r <- s_agg_norm

bivmap <- bivariate.map(rasterx = r[["maxent"]], rastery = r[["biodcrisis"]],
                        export.colour.matrix = TRUE, outname = "bivMapCols",
                        colormatrix = col.matrix, nquantiles = nBreaks)

# Function bivariate.map is not creating the bivariate map #

# check values in bivmap
tmp <- table(values(bivmap))
length(tmp)   # 25 entries are expected, but 1 is returned
names(tmp)    # 6-10 missing (that's by design, the color.matrix function is weird that way)

# exporting raster for use in QGIS
writeRaster(bivmap,filename = paste0(dir,"/results/After_2000/Final/Models/Final/Predictions/aichi_maps/Biome4_disparity_mapFinal.tif"))

# giving bivariate raster desired color ramp
bivrast <- raster( paste0(dir,"/results/After_2000/Final/Models/Final/Predictions/aichi_maps/Biome4_disparity_mapFinal.tif"))

#ctab <- sample(c(as.vector(col.matrix), rep("purple", times = 256 - length(col.matrix))))   # sample doesn't make sense, randomizes everything. 

# colortable expects values 0...255. bivrast starts at 1. 
bivrast <- bivrast - 1
ctab <- c(as.vector(col.matrix[-1,]), rep("purple", times = 256 - length(col.matrix[-1,])))

colortable(bivrast) <- ctab
plot(bivrast)
head(colortable(bivrast))
writeRaster(bivrast,filename = paste0(dir,"/results/After_2000/Final/Models/Final/Predictions/bivplot_disparity/Biome4_disparity_map_coltabFinal.tif"))

# Convert to dataframe for plotting with ggplot
bivMapDF <- as.data.frame(bivmap, xy = TRUE) %>%
  tbl_df() %>%
  dplyr::rename("BivValue" = 3) %>%
  pivot_longer(., names_to = "Variable", values_to = "bivVal", cols = BivValue)

# Make the map using ggplot
map <- ggplot(bivMapDF, aes(x = x, y = y)) +
  geom_raster(aes(fill = bivVal)) +
  scale_y_continuous(breaks = seq(-60, 90, by = 10), 
                     labels = paste0(seq(-60, 90, 10), "°")) +
  scale_x_continuous(breaks = seq(-180,180,40), 
                     labels = paste0(seq(-180,180,40), "°")) +
  scale_fill_gradientn(colours = col.matrix, na.value = "transparent") + 
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "grey20", size = 0.5) +
  # coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Longitude", y = "Latitude");map

# Overlay the legend on the map
## use draw_plot(..., x, y) to control position.
fig <- ggdraw(map) + 
  draw_plot(BivLegend +
              theme(plot.background = element_rect(fill = "white", colour = NA)),
            width = 0.25, height = 0.25, x = 0.05, y = 0.25)
fig

# Save
#ggsave(filename = paste0(dir,"/results/After_2000/Final/Models/Final/Predictions/Biome1disparity.png"), plot = map, dpi = 300)
ggsave(filename = paste0(file.path(dir,"Biome1disparity.png"), plot = map, dpi = 300))

# plotting the points over the matrix
# extract raster value 
values_maxent4 <- values(s_agg_norm$maxent)
values_aichi4  <- values(s_agg_norm$aichi)
  
# ecdf of all values
ecdf_maxent4 <- ecdf(values_maxent4)
ecdf_aichi4  <- ecdf(values_aichi4)
  
# put ECDF values back in raster
tmp4 <- raster(s_agg_norm)
values(tmp4) <- ecdf_maxent4(values_maxent4)
s_agg_norm$maxent_ecdf4 <- tmp4
  
tmp4 <- raster(s_agg_norm)
values(tmp4) <- ecdf_aichi4(values_aichi4)
s_agg_norm$aichi_ecdf4 <- tmp4
  
# Load biome1 study site locations
biome4_sites <- read.csv(paste0(dir,"/covariates/tables/biome4.csv"));head(biome4_sites)

# My data (study site locations)
df_study_sites4 <- data.frame(lon = biome4_sites$lon,
                               lat = biome4_sites$lat)
  
# extract raster values at study sites
values_at_study_sites4 <- data.frame(raster::extract(s_agg_norm, df_study_sites4))
   
# use only the ecdf values
values_at_study_sites4 <- data.frame(X = values_at_study_sites4[, "maxent_ecdf4"],
                                       Y = values_at_study_sites4[, "aichi_ecdf4"],
                                       HEXCode = "#FFFFFF")
     
# adjust to range of color matrix
values_at_study_sites4$X <- values_at_study_sites4$X * 5 + 0.5
values_at_study_sites4$Y <- values_at_study_sites4$Y * 5 + 0.5

# Finally, overlay the study sites over the matrix
p4 <- p + geom_point(data = values_at_study_sites4,fill = "white", col = "black", size = 2, pch = 21); p4

# world landmass
world_landmass <- raster(paste0(dir,"/covariates/predictors/world_landmass.tif"));plot(world_landmass)

# load in the Biome4 aichi map
global_biome_disparity <- raster( paste0(dir,"/results/After_2000/Final/Models/Final/Predictions/bivplot_disparity/Global_Biomess_disparity.tif"))

# Reprojecting rasters
projectRaster(global_biome_disparity, world_landmass ,0.008333333, prj, method="bilinear", alignOnly=FALSE, over=FALSE, paste0(dir,"/results/After_2000/Final/Models/Final/Predictions/bivplot_disparity/global_biome_disparity_Proj.tif"))

# load projected Biome4 aichi map
global_biome_disparity_prj <- raster( paste0(dir,"/results/After_2000/Final/Models/Final/Predictions/bivplot_disparity/global_biome_disparity_Proj.tif"))


global_biome_disparity_Mask <- raster::mask(x = global_biome_disparity_prj ,mask = world_landmass)

# save the new covariate rasters (masked to biome) as .tif
writeRaster(global_biome_disparity_Mask,paste0(dir,"/results/After_2000/Final/Models/Final/Predictions/bivplot_disparity/global_biome_disparity_Proj_Mask.tif"))


four_biomes <- st_read(paste0(dir,"/covariates/shp/Biomes/All_four_Biomes.shp"));names(four_biomes );head(four_biomes)

# assigning a projecton to the data
prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs";four_biomes_prj <- st_transform(four_biomes, crs(prj))

# define RasterLayer object
 r.raster <- raster()

# define raster extent, prj and pixel size
extent <- extent(-180, 180, -90, 90)
prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

extent(r.raster) <- extent(extent)
crs(r.raster) <-crs(prj)
res(r.raster) <- 0.008333

four_biomes.r <- fasterize(sf = four_biomes, raster = r.raster, field = 'layer', fun = "sum")


