setwd(normalizePath(dirname(R.utils::commandArgs(asValues=TRUE)$"f")))
source("../../scripts/h2o-r-test-setup.R")



test.pubdev.1725 <- function() {
  Log.info("Importing cancar.csv data...")
  cancar.hex <- h2o.importFile(locate("smalldata/glrm_test/cancar.csv"))
  print(summary(cancar.hex))
  
  Log.info("Building GLRM model with init = 'PlusPlus'")
  fitH2O_pp <- h2o.glrm(cancar.hex, k = 4, transform = "NONE", init = "PlusPlus", loss = "Quadratic", regularization_x = "None", regularization_y = "None", max_iterations = 1000)
  print(fitH2O_pp)
  
  Log.info("Building GLRM model with init = 'SVD'")
  fitH2O_svd <- h2o.glrm(cancar.hex, k = 4, transform = "NONE", init = "SVD", loss = "Quadratic", regularization_x = "None", regularization_y = "None", max_iterations = 1000)
  print(fitH2O_svd)
}

doTest("PUBDEV-1725: GLRM poor fit with k-means++ initialization", test.pubdev.1725)
