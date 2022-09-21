
## inputs

file_name <- "lond_21_10" # name of files to be used

var_name <- "religion" # name of variable to be plotted

tooltip_var <- "Religion: " # name of the variable as it will appear in the tooltip

positions <- c(
  "Christian",
  "No religion",
  "Muslim",
  "Hindu",
  "Jewish",
  "Sikh",
  "Buddhist",
  "Other religion",
  "Religion not stated"
)

## setting up

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

## reading in data

to_read <- paste0("report_data/",file_name, ".csv")
plot_dat <- fread(to_read)

## making the plot

out_plot <- ggplot(data = plot_dat,
                   aes(x = .data[[var_name]],
                       fill = .data[[var_name]],
                       y = value,
                       text = paste0(tooltip_var, .data[[var_name]],
                                     "<br>",
                                     "Percentage: ", percs,"%",
                                     "<br>",
                                     "Count: ", value))) + 
  geom_bar(stat = "identity", fill = pal[1]) + 
  coord_flip() + 
  #scale_colour_manual(values = pal) + 
  theme(axis.text.x = element_text(angle = 0,size = 10,hjust = 1,vjust = 2),
        legend.position = "none") + 
  scale_y_continuous(labels = dollar_format(prefix = "",suffix = "")) + 
  scale_x_discrete(limits = rev(positions))

tf <- list(family = "Arial",
           size = 12)

fin_plot <- ggplotly(out_plot,height = 500, width = 700, tooltip = "text") %>%
  layout(
    xaxis = list(tickfont = tf,
                 showgrid = TRUE),
    yaxis = list(tickfont = tf,
                 showgrid = FALSE))

fin_plot
