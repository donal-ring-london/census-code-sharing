
## general inputs

filename_11 <- "lond"
filename_21 <- "lond_21"

var_name <- "passports_new" # name of variable to be plotted

tooltip_var <- "Passport: " # name of the variable as it will appear in the tooltip

positions <- c(
  "EU: Poland",
  "EU: France",
  "EU: Italy",
  "EU: Portugal",
  "EU: Germany",
  "EU: Spain",
  "EU: Other EU countries",
  "Other Europe",
  "India",
  "Pakistan",
  "Other Middle East and Asia",
  "Nigeria",
  "Ghana",
  "Other Africa",
  "United States",
  "Other Americas and the Caribbean",
  "Australia",
  "Other Antarctica and Oceania"
)

suf <- "%"


## Reading in data and getting default plot theme

to_read_11 <- paste0("report_data/",filename_11,".csv")
to_read_21 <- paste0("report_data/",filename_21,".csv")

plot_dat_11 <- fread(to_read_11)
plot_dat_21 <- fread(to_read_21)

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))


## 2021 - selecting countries with a percentage higher than 0.6 for inclusion in the plot, aggregating the rest into the regions in which they belong

  ## to avoid duplicating data processing, file is read in after data processing carried out in "london_top_passports" file 

plot_dat_21 <- fread("report_data/spec_dat.csv")

plot_dat_21$passports

  ## final aggregation
plot_dat_21 <- plot_dat_21[,.(percs = sum(percs),value = sum(value)),
                     by = list(passports_new,
                               passports_region)]

plot_dat_21 <- plot_dat_21[!(plot_dat_21$passports_new %in% c("British Overseas Territories",
                                                        "Ireland",
                                                        "No passport held",
                                                        "United Kingdom")),]

## 2011 - narrowing to the same countries selected from 2021, aggregating the rest into the regions in which they belong

  ## for 2011, manually keeping the same countries that were selected based on condition in 2021
countries_keep <- c("Ghana","Nigeria","Australia","EU: France","EU: Germany",
                    "EU: Italy","EU: Poland","EU: Portugal","EU: Spain","EU: Other EU countries",
                    "India", "Pakistan", "United States")

  ## all "other" regions to be aggregated
subreg_agg <- c("Other Central and Western Africa","Other South and Eastern Africa",
                "Other Oceania","Rest of Europe: Other Europe","Other Eastern Asia",
                "Other Middle East","Other South-East Asia","Other Southern Asia",
                "Other Caribbean")

  ## defining new passports variable, filling in with passport country if percentage is over 0.6 (in 2021), filling in with region otherwise
plot_dat_11$passports_new <- "blank"

plot_dat_11[plot_dat_11$passports %in% countries_keep,]$passports_new <- plot_dat_11[plot_dat_11$passports %in% countries_keep,]$passports

plot_dat_11[!(plot_dat_11$passports %in% countries_keep),]$passports_new <- paste0("Other"," ",plot_dat_11[!(plot_dat_11$passports %in% countries_keep),]$passports_region)

plot_dat_11[plot_dat_11$passports %in% subreg_agg,]$passports_new <- paste0("Other"," ",plot_dat_11[plot_dat_11$passports %in% subreg_agg,]$passports_region)

  ## some manual fixing/additions
plot_dat_11$passports_new[plot_dat_11$passports_new == "Other British Overseas Territories"] <- 
  "British Overseas Territories"

plot_dat_11$passports_new[plot_dat_11$passports_new == "Other The Americas and the Caribbean"] <- 
  "Other Americas and the Caribbean"

plot_dat_11$passports_new[plot_dat_11$passports == "Australia"] <- "Australia"
plot_dat_11$passports_new[plot_dat_11$passports == "Ghana"] <- "Ghana"

  ## final aggregation
plot_dat_11 <- plot_dat_11[!(plot_dat_11$passports_new %in% c("British Overseas Territories",
                                                              "No passport held",
                                                              "United Kingdom",
                                                              "Ireland")),]

plot_dat_11 <- plot_dat_11[!(plot_dat_11$passports %in% c("British Overseas Territories",
                                                              "No passport held",
                                                              "United Kingdom",
                                                              "Ireland")),]

plot_dat_11 <- plot_dat_11[,.(percs = sum(percs),value = sum(value)),
                     by = list(passports_new,
                               passports_region)]

## renaming variables
plot_dat_21$Percentage <- round(plot_dat_21$percs,2)
plot_dat_21$`Passport` <- plot_dat_21[,..var_name]

plot_dat_11$Percentage <- round(plot_dat_11$percs,2)
plot_dat_11$`Passport` <- plot_dat_11[,..var_name]

var_name <- "Passport"
to_plot <- "Percentage"


## making the plot

out_plot <- ggplot() + 
  geom_point(data = plot_dat_21,
             aes(x = .data[[var_name]],
                 y = .data[[to_plot]],
                 fill = .data[[var_name]],
                 text = paste0("2021",
                               "<br>",
                               .data[[var_name]],
                               "<br>",
                               .data[[to_plot]],"%",
                               "<br>",
                               value)),
             shape = 1,
             stroke = 0.5,
             size = 2) + 
  geom_point(data = plot_dat_11,
             aes(x = .data[[var_name]],
                 y = .data[[to_plot]],
                 fill = .data[[var_name]],
                 text = paste0("2011",
                               "<br>",
                               .data[[var_name]],
                               "<br>",
                               .data[[to_plot]],"%",
                               "<br>",
                               value)),
             shape = 21,
             size = 2) + 
  coord_flip() + 
  theme(axis.text.y = element_text(angle = 0,size = 10,hjust = 1,vjust = 0.5),
        legend.position = "none") + 
  scale_y_continuous(labels = dollar_format(prefix = "",suffix = suf)) + 
  scale_x_discrete(limits = rev(positions)) + 
  scale_fill_manual(values = c(rep(pal[5],8),
                               rep(pal[1],3),
                               rep(pal[10],3),
                               rep(pal[3],2),
                               rep(pal[2],2),
                               rep(pal[7],1)),
                    limits = positions) + 
  geom_line(aes(
    x = c(2.1,3.9,3.9,2.1,2.1),
    y = c(2.1,2.1,2.5,2.5,2.1)
  )) + 
  geom_line(aes(
    x = c(3.9,2.1),
    y = c(2.5,2.5)
  )) + 
  geom_point(aes(x = 3.5,
                 y = 2.2),
             shape = 21,
             size = 3,
             fill = "gray") + 
  geom_text(aes(x = 3.5,
                y = 2.33,
                label = "2011")) + 
  geom_point(aes(x = 2.5,
                 y = 2.2),
             shape = 1,
             stroke = 0.5,
             size = 3,
             colour =  "gray") + 
  geom_text(aes(x = 2.5,
                y = 2.33,
                label = "2021"))

t <- list(family = "Arial",
          size = 20)
tf <- list(family = "Arial",
           size = 12)

fin_plot <- ggplotly(out_plot, height = 500, width = 700, tooltip = c("text")) %>%
  layout(
    xaxis = list(tickfont = tf,
                 showgrid = TRUE),
    yaxis = list(tickfont = tf,
                 showgrid = FALSE),
    font = tf
  )

fin_plot
