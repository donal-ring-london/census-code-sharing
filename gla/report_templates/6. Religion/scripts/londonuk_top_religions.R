


## inputs
file_name <- "lond_vs_rest_21_10" # name of file to be used

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

suf <- "%"

## reading in data and setting up

to_read <- paste0("report_data/",file_name, ".csv")
plot_dat <- fread(to_read)

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))


lond <- plot_dat$geography == "London"
rest <- plot_dat$geography == "England"

## renaming variables
plot_dat$Percentage <- plot_dat$percs
plot_dat$`Religion` <- plot_dat[,..var_name]

var_name <- "Religion"
to_plot <- "percs"

## making the plot

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
  scale_fill_manual(values = c(rep(pal[1],9)),
                    limits = positions) + 
  geom_line(aes(
    x = c(2.1,3.9,3.9,2.1,2.1),
    y = c(15,15,35,35,15)
  )) + 
  geom_line(aes(
    x = c(3.9,2.1),
    y = c(35,35)
  )) + 
  geom_point(aes(x = 3.5,
                 y = 16.5),
             shape = 21,
             size = 3,
             fill = "gray") + 
  geom_text(aes(x = 3.5,
                y = 24.5,
                label = "Rest of England")) + 
  geom_point(aes(x = 2.5,
                 y = 16.5),
             shape = 1,
             stroke = 0.5,
             size = 3,
             colour =  "gray") + 
  geom_text(aes(x = 2.5,
                y = 21,
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
