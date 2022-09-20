
## general inputs 

filename <- "boroughs_21"


## reading in data, selecting columns

to_read <- paste0("report_data/",filename,".csv")

data <- fread(to_read)

keep <- c("geography","passports_region","passports","value","percs")

data <- data[,..keep]


## final formatting - renaming columns and ordering

colnames(data) <- c("Borough","Passport region","Passport",
                    "Count","Percentage")

tab_dat <- data


  ## ordering - all "other" categories at end, then order by Borough and by percentage

tab_end_cats <- c(
  "Other Central and Western Africa",
  "Other South and Eastern Africa",
  "Other Oceania",
  "EU: Other EU countries",
  "Rest of Europe: Other Europe",
  "Other Eastern Asia",
  "Other Middle East",
  "Other South-East Asia",
  "Other Southern Asia",
  "Other Caribbean"
)

tab_dat$tag <- 0

tab_dat$tag[tab_dat$`Passport` %in% tab_end_cats] <- 1

tab_dat$tag[!(tab_dat$`Passport`) %in% tab_end_cats] <- 2

tab_dat <- tab_dat[order(tab_dat$Borough,tab_dat$tag,tab_dat$Percentage, decreasing = TRUE),]

tab_dat <- tab_dat[,-"tag"]


