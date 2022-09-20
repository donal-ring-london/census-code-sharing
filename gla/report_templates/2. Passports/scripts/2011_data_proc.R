## Initial data processing

getwd()

setwd("C:/Users/DRing/Desktop/final_templates/report_templates/2. Passports")

## inputs

la_file_name <- "la_11"

reg_file_name <- "reg_11"

var_name <- "Passports Held: "

two_level <- "yes" # whether there is a hierarchical structure to the variable. "yes" or "no"

newvar_name <- "passports" # name to be assigned to variable

high_var_name <- "passports_region" # only if hierarchical - name to be assigned to higher variable

lookup_name <- "passports_held_52A" # file name of lookup

## reading in the lookup & cleaning up

to_read_look <- paste0("data_resources/",lookup_name,".csv")

lookup_comp <- fread(to_read_look)

lookup_comp <- lookup_comp[lookup_comp$dat_11 != "", c(3:4)]

lookup_comp$dat_11 <- gsub(" ","_",lookup_comp$dat_11)
lookup_comp$common_cats <- gsub(" ","_",lookup_comp$common_cats)

## REGION DATA ## 

library(gglaplot)
library(dplyr)
library(tidyverse)
library(plotly)
library(data.table)
library(scales)

to_read <- paste0("input_data/",reg_file_name,".csv")

regions <- fread(to_read)

## fixing up names, removing sub-totals, pivoting to wide dataset

colnames(regions) <- gsub(var_name,"",colnames(regions))
colnames(regions) <- gsub("; measures: Value","",colnames(regions))
colnames(regions) <- gsub(" ","_",colnames(regions))

rem <- grep("total",colnames(regions),ignore.case = TRUE)

regions <- regions[,-..rem]

colnames(regions)[5] <- "total_pop" # worth checking manually. But it's always column 5. 

names_vec <- colnames(regions)[6:ncol(regions)] # again worth checking, but it'll always be 6.

reg_long <- pivot_longer(data = regions,
                         cols = names_vec)

reg_long <- data.table(reg_long)

## aggregation to common categories

setkey(reg_long,"name")
setkey(lookup_comp,"dat_11")

reg_long <- lookup_comp[reg_long]

reg_long <- reg_long[,.(value = sum(value)),
                     by = list(common_cats,date,
                               geography,geography_code,
                               Rural_Urban,total_pop)]

colnames(reg_long)[1] <- "name"

## splitting variables into categories
if(two_level == "yes"){
  
  split_names <- tstrsplit(x = reg_long$name,
                           split = ":_")
  
  reg_long[,high_var_name := split_names[[1]]]
  
  colnames(reg_long)[ncol(reg_long)] <- high_var_name
  
  reg_long[,newvar_name := reg_long$name]
  
  colnames(reg_long)[ncol(reg_long)] <- newvar_name
  
}else if (two_level == "no"){
  reg_long[,newvar_name := reg_long$name]
  
  colnames(reg_long)[ncol(reg_long)] <- newvar_name
}


## replacing with the plot names

to_read_ln <- paste0("data_resources/",lookup_name,"_names",".csv")

look_names <- fread(to_read_ln)

look_names$orig_names <- gsub(" ","_",look_names$orig_names)
look_names$plot_name <- gsub(" ","_",look_names$plot_name)

setkey(look_names,"orig_names")

setkeyv(reg_long,newvar_name)

reg_long <- look_names[reg_long]

reg_long[,newvar_name := reg_long$plot_name]
colnames(reg_long)[ncol(reg_long)] <- newvar_name

rem <- c("orig_names","plot_name")
reg_long <- reg_long[,-..rem]

## creating percentages

reg_long$percs <- 100*(reg_long$value/reg_long$total_pop)
reg_long$percs <- round(reg_long$percs,2)

  ## terrible workaround below. Try to find a way in data.table that works

reg_long <- data.frame(reg_long)

reg_long[,newvar_name] <- gsub("_"," ",reg_long[,newvar_name])

if(two_level == "yes"){
  reg_long[,high_var_name] <- gsub("_"," ",reg_long[,high_var_name])
}

reg_long <- data.table(reg_long)

reg_long <- reg_long[,-"name"]

## getting rid of wales

reg_long <- reg_long[reg_long$geography != "Wales",]

## writing the dataset

fwrite(x = reg_long,
       file = "report_data/regions_11.csv")


## LONDON DATA vs England and Wales ##

lond_long <- reg_long[reg_long$geography == "London",]

fwrite(x = lond_long,
       file = "report_data/lond.csv")


rest_long <- reg_long[reg_long$geography != "London"]

if(two_level == "yes"){
  
  rest_agg <- data.table(aggregate(x = rest_long$value,
                                   by = list(rest_long$date,
                                             rest_long$Rural_Urban,
                                             unlist(rest_long[,..newvar_name]),
                                             unlist(rest_long[,..high_var_name])),
                                   FUN = sum))
  
  colnames(rest_agg) <- c("date","Rural_Urban",newvar_name,high_var_name,"value")
  
}else if(two_level == "no"){
  
  rest_agg <- data.table(aggregate(x = rest_long$value,
                                   by = list(rest_long$date,
                                             rest_long$Rural_Urban,
                                             unlist(rest_long[,..newvar_name])),
                                   FUN = sum))
  
  colnames(rest_agg) <- c("date","Rural_Urban",newvar_name,"value")
  
}

rest_agg$geography <- "England (excluding London)"
rest_agg$geography_code <- NA

rest_agg$total_pop <- sum(unique(reg_long$total_pop[reg_long$geography != "London"]))

rest_agg$percs <- 100*(rest_agg$value/rest_agg$total_pop)

col_ords <- colnames(lond_long)

rest_agg <- rest_agg[,..col_ords]

colnames(lond_long) == colnames(rest_agg) # checking that they're matched and aligned correctly

lond_vs_rest <- rbind(lond_long,rest_agg)

lond_vs_rest$percs <- round(lond_vs_rest$percs,2)

lond_vs_rest$geography[lond_vs_rest$geography == "England (excluding London)"] <- "England"

fwrite(x = lond_vs_rest,
       file = "report_data/lond_vs_rest_11.csv")

## BOROUGH DATA ## 

## Reading in data, narrowing to London
lookup <- read.csv("geo_data/oa_ward_address_lookup_2021.csv")

las_london <- unique(lookup$gss_code_la)

to_read <- paste0("input_data/", la_file_name, ".csv")

las <- fread(to_read)

las_london <- las[las$`geography code` %in% las_london,]


## fixing up names, pivoting to wider dataset

colnames(las_london) <- gsub(var_name,"",colnames(las_london))
colnames(las_london) <- gsub("; measures: Value","",colnames(las_london))
colnames(las_london) <- gsub(" ","_",colnames(las_london))

rem <- grep("total",colnames(las_london),ignore.case = TRUE)

las_london <- las_london[,-..rem]

colnames(las_london)[5] <- "total_pop" # worth checking manually. But it's always column 5. 

names_vec <- colnames(las_london)[6:ncol(las_london)] # again worth checking, but it'll always be 6.

las_long <- pivot_longer(data = las_london,
                         cols = names_vec)

las_long <- data.table(las_long)


## aggregation to common categories

setkey(las_long,"name")
setkey(lookup_comp,"dat_11")

las_long <- lookup_comp[las_long]

las_long <- las_long[,.(value = sum(value)),
                     by = list(common_cats,date,
                               geography,geography_code,
                               Rural_Urban,total_pop)]

colnames(las_long)[1] <- "name"

## splitting variables into categories
if(two_level == "yes"){
  
  split_names <- tstrsplit(x = las_long$name,
                           split = ":_")
  
  las_long[,high_var_name := split_names[[1]]]
  
  colnames(las_long)[ncol(las_long)] <- high_var_name
  
  las_long[,newvar_name := las_long$name]
  
  colnames(las_long)[ncol(las_long)] <- newvar_name
  
}else if (two_level == "no"){
  las_long[,newvar_name := las_long$name]
  
  colnames(las_long)[ncol(las_long)] <- newvar_name
}


## replacing with the plot names

to_read_ln <- paste0("data_resources/",lookup_name,"_names",".csv")

look_names <- fread(to_read_ln)

look_names$orig_names <- gsub(" ","_",look_names$orig_names)
look_names$plot_name <- gsub(" ","_",look_names$plot_name)

setkey(look_names,"orig_names")

setkeyv(las_long,newvar_name)

las_long <- look_names[las_long]

las_long[,newvar_name := las_long$plot_name]
colnames(las_long)[ncol(las_long)] <- newvar_name

rem <- c("orig_names","plot_name")
las_long <- las_long[,-..rem]


## creating percentages

las_long$percs <- 100*(las_long$value/las_long$total_pop)
las_long$percs <- round(las_long$percs,2)

## terrible workaround below. Try to find a way in data.table that works

las_long <- data.frame(las_long)

las_long[,newvar_name] <- gsub("_"," ",las_long[,newvar_name])

if(two_level == "yes"){
  las_long[,high_var_name] <- gsub("_"," ",las_long[,high_var_name])
}

las_long <- data.table(las_long)

las_long <- las_long[,-"name"]

## writing the dataset
fwrite(x = las_long,
       file = "report_data/boroughs_11.csv")

