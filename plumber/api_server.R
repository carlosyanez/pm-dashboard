library(plumber)
r <- plumb("/opt/plumber/plumber.R")
r$run(port=8000, host='0.0.0.0')