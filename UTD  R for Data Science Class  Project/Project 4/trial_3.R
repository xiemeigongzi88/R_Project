library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(keras)
library("imager")
library(imagerExtra)
library(tensorflow)

#devtools::install_github("rstudio/keras")

#system("ls ../input")
# https://github.com/rstudio/keras/issues/216  
# solve the pillow problem 

cats_train<- list.files(path = "C:\\Users\\sxw17\\Desktop\\2019 Spring\\R\\HW\\4\\Data\\train", pattern = "cat_+")
dogs_train<- list.files(path = "C:\\Users\\sxw17\\Desktop\\2019 Spring\\R\\HW\\4\\Data\\train", pattern = "dog_+")

cats_test<-list.files(path = "C:\\Users\\sxw17\\Desktop\\2019 Spring\\R\\HW\\4\\Data\\test", pattern = "cat_+")
dogs_test<-list.files(path = "C:\\Users\\sxw17\\Desktop\\2019 Spring\\R\\HW\\4\\Data\\test", pattern = "dog_+")

size = 150
channels = 3

train <- c(cats_train[1:26],dogs_train[1:14])
train <- sample(train,replace = FALSE)
test <- c(cats_test[1:5],dogs_test[1:5])
test <- sample(test,replace = FALSE)

# reticulate::py_config()

train_prep <- function(images, size, channels, path){
  
  count<- length(images)
  master_array <- array(NA, dim=c(count,size, size, channels))
  
  for (i in seq(length(images))) {
    img <- image_load(path = paste(path, images[i], sep=""), target_size = c(size,size))
    img_arr <- image_to_array(img)
    img_arr <- array_reshape(img_arr, c(1, size, size, channels))
    master_array[i,,,] <- img_arr
  }
  return(master_array)
}


x_train <- train_prep(train, size, channels, "C:\\Users\\sxw17\\Desktop\\2019 Spring\\R\\HW\\4\\Data\\train")
x_test <- train_prep(test, size, channels, "C:\\Users\\sxw17\\Desktop\\2019 Spring\\R\\HW\\4\\Data\\test")
y_train <- as.numeric(grepl("dog_", train, ignore.case = TRUE))
y_test <- as.numeric(grepl("dog_", test, ignore.case = TRUE))




















