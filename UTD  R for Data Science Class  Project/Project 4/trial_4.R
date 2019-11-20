# preprocess data 
# https://rpubs.com/kanedglsk/236125
# https://www.r-bloggers.com/convolutional-neural-networks-in-r/


install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")

library(EBImage)
example_cat_image <- readImage(file.path(image_dir, "cat.0.jpg"))
display(example_cat_image)
