
## inputs

filename_11 <- "lond_10"
filename_21 <- "lond_21_10"

var_name <- "religion"

high_var_name <- ""

to_plot <- "percs"

suf <- "%"

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


## reading in data, setting up

to_read_11 <- paste0("report_data/",filename_11,".csv")
to_read_21 <- paste0("report_data/",filename_21,".csv")

plot_dat_11 <- fread(to_read_11)
plot_dat_21 <- fread(to_read_21)

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))


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
  scale_fill_manual(values = c(rep(pal[1],9)),
                    limits = positions) + 
  geom_line(aes(
    x = c(2.1,3.3,3.3,2.1,2.1),
    y = c(7,7,12.5,12.5,7)
  )) + 
  geom_line(aes(
    x = c(3.3,2.1),
    y = c(12.5,12.5)
  )) + 
  geom_point(aes(x = 3,
                 y = 7.75),
             shape = 21,
             size = 3,
             fill = pal[1]) + 
  geom_text(aes(x = 3,
                y = 10,
                label = "2011")) + 
  geom_point(aes(x = 2.5,
                 y = 7.75),
             shape = 1,
             stroke = 0.5,
             size = 3,
             colour =  pal[1]) + 
  geom_text(aes(x = 2.5,
                y = 10,
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
