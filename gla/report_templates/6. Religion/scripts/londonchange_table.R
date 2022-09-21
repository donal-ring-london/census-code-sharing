

## inputs

filename_11 <- "lond_58"

filename_21 <- "lond_21_58"


## reading in data

to_read_11 <- paste0("report_data/",filename_11,".csv")

to_read_21 <- paste0("report_data/",filename_21,".csv")

data_11 <- fread(to_read_11)

data_21 <- fread(to_read_21)


## formatting tables, combining 2011 and 2021 data

keep <- c("religion","religion_det","percs","value")

data_11 <- data_11[,..keep]

data_21 <- data_21[,..keep]

data_21$religion == data_11$religion # checking order of categories is aligned

data_21$religion_det == data_11$religion_det # checking order of categories is aligned

data_both <- data_21

colnames(data_both)[3:4] <- c("percs_21","value_21")

data_both$percs_11 <- data_11$percs

data_both$value_11 <- data_11$value


## creating the change variables

data_both$percs_change <- round((data_both$percs_21 - data_both$percs_11),2)

data_both$value_change <- data_both$value_21 - data_both$value_11


## renaming columns, final formatting
colnames(data_both) <- c(
  "Religion",
  "Detailed religion",
  "2021 percentage",
  "2021 count",
  "2011 percentage",
  "2011 count",
  "Pp change",
  "Count change"
)

data_both <- data_both[,c(1,2,3,5,7,4,6,8)]

tab_dat <- data_both

rem <- "Religion"
tab_dat <- tab_dat[,-..rem]

tab_dat <- tab_dat[tab_dat$`2021 percentage` != 0,]

colnames(tab_dat)[1] <- "Religion"

## ordering

tab_dat <- tab_dat[(order(tab_dat$`2021 percentage`, decreasing = TRUE)),]

tab_dat <- tab_dat[c(1:3,5:12,4),]




