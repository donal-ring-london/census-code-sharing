
## general inputs

file_name <- "lond_vs_rest_21" # name of files to be used

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

## reading in data and setting up

to_read <- paste0("report_data/",file_name, ".csv")
plot_dat <- fread(to_read)

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

## selecting categories for plotting and aggregating the rest

  ## splitting the dataset into London and the Rest of England
plot_dat$passports_region <- gsub("The ","",plot_dat$passports_region,ignore.case = FALSE)

lond <- plot_dat$geography == "London"
rest <- plot_dat$geography == "England"

lond_dat <- plot_dat[lond,]
rest_dat <- plot_dat[rest,]

  ## London - selecting countries with a percentage higher than 0.6 for inclusion in the plot, aggregating the rest into the regions in which they belong

  ## to avoid duplicating data processing, the file is read in after data processing carried out in "london_top_passports" file 

lond_dat <- fread("report_data/spec_dat.csv")
    
  ## Matching with the rest of England - adding the new passports variable derived using London data onto the dataset for the rest of England

lond_dat <- lond_dat[order(lond_dat$passports),]
rest_dat <- rest_dat[order(rest_dat$passports),]

lond_dat$passports == rest_dat$passports # checking the order aligns so that one can be added to another as a new column

rest_dat$passports_new <- lond_dat$passports_new

  ## aggregating both

lond_dat <- lond_dat[,.(percs = sum(percs),value = sum(value)),
                     by = list(passports_new,
                               passports_region,
                               geography)]

lond_dat <- lond_dat[!(lond_dat$passports_new %in% c("British Overseas Territories",
                                                     "Ireland",
                                                     "No passport held",
                                                     "United Kingdom")),]

rest_dat <- rest_dat[,.(percs = sum(percs),value = sum(value)),
                     by = list(passports_new,
                               passports_region,
                               geography)]

rest_dat <- rest_dat[!(rest_dat$passports_new %in% c("British Overseas Territories",
                                                     "Ireland",
                                                     "No passport held",
                                                     "United Kingdom")),]

  ## putting the two datasets together, saving conditions for both London and the Rest of England

plot_dat <- rbind(lond_dat,
                  rest_dat)

lond <- plot_dat$geography == "London"
rest <- plot_dat$geography == "England"


## Renaming the variables to be plotted

plot_dat$Percentage <- plot_dat$percs
plot_dat$`Passports` <- plot_dat[,..var_name]

var_name <- "Passports"
to_plot <- "percs"

## Making the final plot

out_plot <- ggplot() + 
  geom_point(data = plot_dat[lond,],
             aes(x = .data[[var_name]],
                 y = .data[[to_plot]],
                 fill = .data[[var_name]],
                 text = paste0("London",
                               "<br>",
                               .data[[var_name]],
                               "<br>",
                               .data[[to_plot]],"%",
                               "<br>",
                               value)),
             shape = 1,
             stroke = 0.5,
             size = 2) + 
  geom_point(data = plot_dat[rest,],
             aes(x = .data[[var_name]],
                 y = .data[[to_plot]],
                 fill = .data[[var_name]],
                 text = paste0("Rest of England",
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
    y = c(1.5,1.5,2.6,2.6,1.5)
  )) + 
  geom_line(aes(
    x = c(3.9,2.1),
    y = c(2.6,2.6)
  )) + 
  geom_point(aes(x = 3.5,
                 y = 1.6),
             shape = 21,
             size = 3,
             fill = "gray") + 
  geom_text(aes(x = 3.5,
                y = 2.0,
                label = "Rest of England")) + 
  geom_point(aes(x = 2.5,
                 y = 1.6),
             shape = 1,
             stroke = 0.5,
             size = 3,
             colour =  "gray") + 
  geom_text(aes(x = 2.5,
                y = 1.825,
                label = "London"))

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
