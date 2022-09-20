

## general inputs

filename_11 <- "lond"

filename_21 <- "lond_21"


## reading in the data

to_read_11 <- paste0("report_data/",filename_11,".csv")

to_read_21 <- paste0("report_data/",filename_21,".csv")

data_11 <- fread(to_read_11)

data_21 <- fread(to_read_21)

## selecting variables

keep <- c("passports_region","passports","percs","value")

data_11 <- data_11[,..keep]

data_21 <- data_21[,..keep]

## combining 2011 and 2021 into the same dataframe
data_21$passports_region == data_11$passports_region

data_21$passports == data_11$passports

data_both <- data_21

colnames(data_both)[3:4] <- c("percs_21","value_21")

data_both$percs_11 <- data_11$percs

data_both$value_11 <- data_11$value


## deriving the change variables

data_both$percs_change <- round((data_both$percs_21 - data_both$percs_11),2)

data_both$value_change <- data_both$value_21 - data_both$value_11

## renaming and reordering columns
colnames(data_both) <- c(
  "Passport region",
  "Passport",
  "2021 percentage",
  "2021 count",
  "2011 percentage",
  "2011 count",
  "Pp change",
  "Count change"
)

data_both <- data_both[,c(1,2,3,5,7,4,6,8)]

tab_dat <- data_both

## ordering rows

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

tab_end_dat <- tab_dat[tab_dat$Passport %in% tab_end_cats,]

tab_end_dat <- tab_end_dat[order(tab_end_dat$`2021 percentage`, decreasing = TRUE),]

tab_top_dat <- tab_dat[!(tab_dat$Passport %in% tab_end_cats),]

tab_top_dat <- tab_top_dat[order(tab_top_dat$`2021 percentage`, decreasing = TRUE),]

tab_dat <- rbind(tab_top_dat, tab_end_dat)
