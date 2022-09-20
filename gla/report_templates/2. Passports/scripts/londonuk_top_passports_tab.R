
## general inputs

file_name <- "lond_vs_rest_21"

var_name <- "passports"

var_name_out <- "Passport"

high_var_name <- "passports_region"

high_var_name_out <- "Passport region"


## reading in, setting up data

to_read <- paste0("report_data/",file_name,".csv")

data <- fread(to_read)
colnames(data)
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

colnames(tab_dat)[3:6] <- c("London Count","England Count",
                            "London Percentage","England Percentage")


