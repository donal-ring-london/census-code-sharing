## regions table

## inputs 

filename <- "regions_21_10"

## reading in data and formatting table

to_read <- paste0("report_data/",filename,".csv")

data <- fread(to_read)

keep <- c("geography","religion","value","percs")

data <- data[,..keep]


colnames(data) <- c("Region","Religion",
                    "Count","Percentage")

tab_dat <- data

## ordering - religion not stated at end for each region, otherwise ordered by percentage 

tab_end_cats <- c(
  "Religion not stated"
)

tab_dat$tag <- 0

tab_dat$tag[tab_dat$Religion %in% tab_end_cats] <- 1

tab_dat$tag[!(tab_dat$Religion) %in% tab_end_cats] <- 2

tab_dat <- tab_dat[order(tab_dat$Region,tab_dat$tag,tab_dat$Percentage, decreasing = TRUE),]

tab_dat <- tab_dat[,-"tag"]


