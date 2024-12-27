
library(raster)
library(sp)
library(sf)
library(dplyr)
library(rgdal)
library(caret)
library( nnet)
library(randomForest)
library (e1071)
library (kernlab)
library(mapview)
library(rasterVis)




image <- stack("C:/Namibia Data/2023_Landsat/mosaic/namibia_mosaic1__masked_clean.tif")


names(image) <- c("band2", "band3", "band4","band5", "band6", "band7")


#Plotting false colour composite
par(col.axis="white",col.lab="white",tck=0)
plotRGB(image, r = 4, g = 3, b = 2, axes = TRUE, 
        stretch = "lin", main = "False Color Composite")
box(col="white")


lulc_class <-training_points$class

#Computing NDVI
ndvi <- (image[[4]]-image[[3]])/(image[[4]]+image[[3]])


writeRaster(ndvi, filename = "ndvi.tif", format = "GTiff", overwrite = TRUE)


#Read in vector training points and convert to spatial data
training_points <- st_read("C:/Namibia Data/Training points/FinalTrainingData/training_pts.shp", quiet = T)

training_sp <-as(training_points, 'Spatial')

#Extract value from  the image using the training points
training_df <- raster::extract(image, training_sp, method="simple")

training_df %>% 
  head(n=10)

training_df <- data.frame (lulc_class, training_df)

set.seed(1234)

# Split the data frame into 70-30 by class
trainx = list(0)
evalx = list(0)
for (i in 1:10){ # loop through all ten classes
  cls = training_df[training_df$lulc_class == i,]
  smpl <- floor(0.70 * nrow(cls))
  tt <- sample(seq_len(nrow(cls)), size = smpl)
  trainx[[i]] <- cls[tt,]
  evalx[[i]] <- cls[-tt,]
}

# combine them all into training and evaluation data frames
trn <- do.call(rbind, trainx) 
eva <- do.call(rbind, evalx)

head(trn)
str(trn)

eva %>% 
  head(n=5)

library(tidyverse)

trn %>% 
  group_by(lulc_class) %>% 
  summarize(number_per_class = n())


# Set up a resampling method in the model training process
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number of folds
                   repeats = 5, # number of repeats
                   allowParallel = TRUE, # allow use of multiple cores if specified in training
                   verboseIter = TRUE) # view the training iterations

rf.grid <- expand.grid(mtry=1:20)


# Train the random forest model
rf_model <- caret::train(x = trn[,2:7], y = as.factor(as.integer(as.factor(trn$lulc_class))),
                         method = "rf", metric="Accuracy", trainControl = tc, tuneGrid = rf.grid)

print(rf_model)

plot (rf_model)

#predict for evaluation data
eval <- predict(rf_model, newdata=eva[, 2:7])

head(eval)

#combine prediction for evaluation data with the evaluation data
eval_2 <- data.frame(eva, eval)

#compute accuracy assessment
confusionMatrix((as.factor(eval_2$lulc_class)), (as.factor(eval_2$eval)))

# Apply the random forest model to the image 
rf_lulc = raster::predict(image, model=rf_model)

writeRaster(rf_lulc, filename = "rf_lulc.tif", format = "GTiff", overwrite = TRUE)



