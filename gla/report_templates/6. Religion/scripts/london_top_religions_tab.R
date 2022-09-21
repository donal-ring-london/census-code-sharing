
## inputs

file_name <- "lond_21_58"

## reading in data

to_read <- paste0("report_data/",file_name, ".csv")
tab_dat <- fread(to_read)

## selecting and renaming columns, 

keep <- c("religion_det","value","percs")

tab_dat <- tab_dat[,..keep]

colnames(tab_dat) <- c("Detailed religion",
                       "Count","Percentage")

## excluding those with a percentage of 0, final table formatting
tab_dat <- tab_dat[tab_dat$Percentage != 0,]

tab_dat <- tab_dat[order(tab_dat$Percentage,decreasing = TRUE),]

tab_dat <- tab_dat[c(1:3,5:12,4),]

tab_dat[2,1] <- "No religion"

font.size <- "9.5pt"

