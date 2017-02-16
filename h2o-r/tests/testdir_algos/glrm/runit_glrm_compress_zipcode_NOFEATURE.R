setwd(normalizePath(dirname(R.utils::commandArgs(asValues=TRUE)$"f")))
source("../../../scripts/h2o-r-test-setup.R")

test.glrm.compresszipcodes <- function() {
  Log.info("Importing zipcode data...")
  acs_orig <- h2o.uploadFile(locate("bigdata/laptop/census/ACS_13_5YR_DP02_cleaned.zip"), col.types = c("enum", rep("numeric", 149)))

  acs_zcta_col <- acs_orig$ZCTA5
  acs_full <- acs_orig[,-which(colnames(acs_orig) == "ZCTA5")]
  
  browser()

  dim(acs_full)
  summary(acs_full)

  acs_model <- h2o.glrm(training_frame = acs_full, k = 10, transform = "STANDARDIZE", loss = "Quadratic", regularization_x = "Quadratic", regularization_y = "L1", max_iterations = 100, gamma_x = 0.25, gamma_y = 0.5)
  plot(acs_model)

  zcta_arch_x <- h2o.getFrame(acs_model@model$representation_name)
  head(zcta_arch_x)

  idx <- ((acs_zcta_col == "10065") |   # Manhattan, NY (Upper East Side)
          (acs_zcta_col == "11219") |   # Manhattan, NY (East Harlem)
          (acs_zcta_col == "66753") |   # McCune, KS
          (acs_zcta_col == "84104") |   # Salt Lake City, UT
          (acs_zcta_col == "94086") |   # Sunnyvale, CA
          (acs_zcta_col == "95014"))    # Cupertino, CA

  city_arch <- as.data.frame(zcta_arch_x[idx,1:2])
  xeps <- (max(city_arch[,1]) - min(city_arch[,1])) / 10
  yeps <- (max(city_arch[,2]) - min(city_arch[,2])) / 10
  xlims <- c(min(city_arch[,1]) - xeps, max(city_arch[,1]) + xeps)
  ylims <- c(min(city_arch[,2]) - yeps, max(city_arch[,2]) + yeps)
  plot(city_arch[,1], city_arch[,2], xlim = xlims, ylim = ylims, xlab = "First Archetype", ylab = "Second Archetype", main = "Archetype Representation of Zip Code Tabulation Areas")
    text(city_arch[,1], city_arch[,2], labels = c("Upper East Side", "East Harlem", "McCune", "Salt Lake City", "Sunnyvale", "Cupertino"), pos = 1)



}

doTest("GLRM: Zip code compression", test.glrm.compresszipcodes)
