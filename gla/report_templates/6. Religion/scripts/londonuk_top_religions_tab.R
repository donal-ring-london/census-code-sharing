
## inputs

file_name <- "lond_vs_rest_21_10"

var_name <- "religion"

var_name_out <- "Religion"

high_var_name <- ""

high_var_name_out <- ""


## reading in, setting up data

to_read <- paste0("report_data/",file_name,".csv")

data <- fread(to_read)

if(high_var_name != ""){
  keep <- c(high_var_name, var_name, "geography","value","percs")
}else{
  keep <- c(var_name,"geography","value","percs")
}

tab_dat <- data[,..keep]

if(high_var_name_out != ""){
  colnames(tab_dat) <- c(high_var_name_out,var_name_out,"Geography",
                         "Count","Percentage")
}else{
  colnames(tab_dat) <- c(var_name_out,"Geography",
                         "Count","Percentage")
}

font.size = "9.5pt"


## changing geography to wide

tab_dat <- data.table(pivot_wider(
  data = tab_dat,
  names_from = Geography,
  values_from = c(Count, Percentage)
))


## final formatting and renaming
colnames(tab_dat)[2:5] <- c("London Count","England Count",
                            "London Percentage","England Percentage")

tab_dat

tab_dat <- tab_dat[order(tab_dat$`London Percentage`,decreasing = TRUE),]

tab_dat <- tab_dat[c(1,3:9,2),]


