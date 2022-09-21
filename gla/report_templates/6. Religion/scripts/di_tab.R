
## reading in the data
data <- data.frame(st_read("geo_data/ent_change.shp", quiet = TRUE))

## selecting and renaming columns, final formatting
keep <- c(
  "l_dstrc",
  "ward_nm",
  "ent_21",
  "ent_11",
  "change"
)

data <- data[,keep]

colnames(data) <- c("Borough","Ward","2021 diversity","2011 diversity","Change")

tab_dat <- data

