
## general inputs

file_name <- "lond_21" 

## reading in data

to_read <- paste0("report_data/",file_name, ".csv")
tab_dat <- fread(to_read)

## selecting and renaming columns

keep <- c("passports_region","passports","value","percs")

tab_dat <- tab_dat[,..keep]

colnames(tab_dat) <- c(
  "Passport region",
  "Passport",
  "Count",
  "Percentage"
)

font.size <- "9.5pt"
