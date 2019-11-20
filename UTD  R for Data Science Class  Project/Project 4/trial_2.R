library(keras)

library(EBImage)
library(stringr)
library(pbapply)

# Image Preprocessing


secondCat <- readImage("C:\\Users\\sxw17\\Desktop\\2019 Spring\\R\\HW\\4\\Data\\train\\cat_1.jpg")
display(secondCat)

# Set image size
width <- 150
height <- 150

extract_feature <- function(dir_path, width, height, labelsExist = T) {
  img_size <- width * height
  
  ## List images in path
  images_names <- list.files(dir_path)
  
  if(labelsExist){
    ## Select only cats or dogs images
    catdog <- str_extract(images_names, "^(cat|dog)")
    # Set cat == 0 and dog == 1
    key <- c("cat" = 0, "dog" = 1)
    y <- key[catdog]
  }
  
  print(paste("Start processing", length(images_names), "images"))
  ## This function will resize an image, turn it into greyscale
  feature_list <- pblapply(images_names, function(imgname) {
    ## Read image
    img <- readImage(file.path(dir_path, imgname))
    ## Resize image
    img_resized <- resize(img, w = width, h = height)
    ## Set to grayscale (normalized to max)
    grayimg <- channel(img_resized, "gray")
    ## Get the image as a matrix
    img_matrix <- grayimg@.Data
    ## Coerce to a vector (row-wise)
    img_vector <- as.vector(t(img_matrix))
    return(img_vector)
  })
  ## bind the list of vector into matrix
  feature_matrix <- do.call(rbind, feature_list)
  feature_matrix <- as.data.frame(feature_matrix)
  ## Set names
  names(feature_matrix) <- paste0("pixel", c(1:img_size))
  
  if(labelsExist){
    return(list(X = feature_matrix, y = y))
  }else{
    return(feature_matrix)
  }
}


# Takes approx. 15min
trainData <- extract_feature("train/", width, height)
# Takes slightly less
testData <- extract_feature("test1/", width, height, labelsExist = F)

# Check processing on second cat
par(mar = rep(0, 4))
testCat <- t(matrix(as.numeric(trainData$X[2,]),
                    nrow = width, ncol = height, T))
image(t(apply(testCat, 2, rev)), col = gray.colors(12),
      axes = F)

# Save
save(trainData, testData, file = "catdogData.RData")


































