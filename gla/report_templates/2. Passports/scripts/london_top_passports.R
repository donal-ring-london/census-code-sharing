
## general inputs

file_name <- "lond_21" # name of file to be used

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

## setting up plotting theme default

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

## reading in data

to_read <- paste0("report_data/",file_name, ".csv")
plot_dat <- fread(to_read)

## selecting countries with a percentage higher than 0.6 for inclusion in the plot, aggregating the rest into the regions in which they belong
## [this data manipulation is to be moved out of plot-making file - process data separately, then read in that data file]

  ## all "other" to be aggregated no matter what percentage is
subreg_agg <- c("Other Central and Western Africa","Other South and Eastern Africa",
                "Other Oceania","Rest of Europe: Other Europe","Other Eastern Asia",
                "Other Middle East","Other South-East Asia","Other Southern Asia",
                "Other Caribbean")

  ## defining new passports variable, filling in with passport country if percentage is over 0.6, filling in with region otherwise
plot_dat$passports_new <- "blank"

plot_dat[plot_dat$percs >= 0.6,]$passports_new <- plot_dat[plot_dat$percs >= 0.6,]$passports

plot_dat[plot_dat$percs < 0.6,]$passports_new <- paste0("Other"," ",plot_dat[plot_dat$percs < 0.6,]$passports_region)

plot_dat[plot_dat$passports %in% subreg_agg,]$passports_new <- paste0("Other"," ",plot_dat[plot_dat$passports %in% subreg_agg,]$passports_region)

  ## some manual fixing/additions
plot_dat$passports_new[plot_dat$passports_new == "Other British Overseas Territories"] <- "British Overseas Territories"

plot_dat$passports_new[plot_dat$passports_new == "Other The Americas and the Caribbean"] <- 
  "Other Americas and the Caribbean"

plot_dat$passports_new[plot_dat$passports == "Australia"] <- "Australia"
plot_dat$passports_new[plot_dat$passports == "Ghana"] <- "Ghana"
plot_dat$passports_new[plot_dat$passports == "United States"] <- "United States"

  ## to save duplicating data processing in other plot-making files, the dataset is saved at this point
  ## [as noted above, data processing will be removed from file in any case]

fwrite(plot_dat,
       "report_data/spec_dat.csv")


  ## final aggregation

plot_dat <- plot_dat[!(plot_dat$passports_new %in% c("British Overseas Territories",
                                                     "No passport held",
                                                     "United Kingdom",
                                                     "Ireland")),]

plot_dat <- plot_dat[,.(percs = sum(percs),value = sum(value)),
         by = list(passports_new,
                   passports_region)]

### Making the plot

out_plot <- ggplot(data = plot_dat,
                   aes(x = .data[[var_name]],
                       fill = .data[[var_name]],
                       y = value,
                       text = paste0(tooltip_var, .data[[var_name]],
                                     "<br>",
                                     "Percentage: ", percs,"%",
                                     "<br>",
                                     "Count: ", value))) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 0,size = 10,hjust = 1,vjust = 2),
        legend.position = "none") + 
  scale_y_continuous(labels = dollar_format(prefix = "",suffix = "")) + 
  scale_x_discrete(limits = rev(positions)) + 
  scale_fill_manual(values = c(rep(pal[5],8),
                               rep(pal[1],3),
                               rep(pal[10],3),
                               rep(pal[3],2),
                               rep(pal[2],2),
                               rep(pal[7],1)),
                     limits = positions)

tf <- list(family = "Arial",
           size = 12)

fin_plot <- ggplotly(out_plot,height = 500, width = 700, tooltip = "text") %>%
  layout(
    xaxis = list(tickfont = tf,
                 showgrid = TRUE),
    yaxis = list(tickfont = tf,
                 showgrid = FALSE))


fin_plot