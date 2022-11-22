# install_tf.R

# devtools::install_github("rstudio/tensorflow")
# devtools::install_github("rstudio/keras")
library(tensorflow)
install_tensorflow()
# library(keras)
# install_keras()
# use_condaenv("r-tensorflow")

library(reticulate)
library(keras)
virtualenv_create("myenv")
use_virtualenv("myenv")
install_keras(method="virtualenv", envname="myenv")

use_virtualenv("r-tensorflow")
mnist <- dataset_mnist()

# keras::install_keras(
#   method = "virtualenv",
#   conda = Sys.getenv("RETICULATE_CONDA"),
#   envname = "r-tensorflow"
#   )
