

## regions table

## inputs 

filename <- "boroughs_21_58"

## reading in data, selecting

to_read <- paste0("report_data/",filename,".csv")

data <- fread(to_read)

keep <- c("geography","religion_det","value","percs")

data <- data[,..keep]


## aggregating "No Religion" detailed categories, removing small categories

no_religion <- c("No religion: Agnostic","No religion: Atheist",
                 "No religion: Free Thinker","No religion: No religion",
                 "No religion: Humanist","No religion: Realist")

data[data$religion_det %in% no_religion,]$religion_det <- "No religion"

data <- data[,.(value = sum(value), percs = sum(percs)),
             by = list(geography,
                       religion_det)]

keep <- c(
  "Christian",
  "No religion",
  "Muslim",
  "Hindu",
  "Jewish",
  "Sikh",
  "Buddhist",
  "Other religion: Jain",
  "Other religion: Pagan",
  "Religion not stated"
)

data <- data[data$religion_det %in% keep,]

colnames(data) <- c("Borough","Religion","Count","Percentage")

tab_dat <- data


## ordering

tab_end_cats <- c(
  "Religion not stated"
)

tab_dat$tag <- 0

tab_dat$tag[tab_dat$`Religion` %in% tab_end_cats] <- 1

tab_dat$tag[!(tab_dat$`Religion`) %in% tab_end_cats] <- 2

tab_dat <- tab_dat[order(tab_dat$Borough,tab_dat$tag,tab_dat$Percentage, decreasing = TRUE),]

tab_dat <- tab_dat[,-"tag"]




