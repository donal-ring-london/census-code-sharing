
## general inputs

filename_11 <- "lond"
filename_21 <- "lond_21"

default_category <- "Regions"

var_name <- "passports"

tooltip_var <- "Passport: "

suf <- "%"


## setting up plot themes, fonts, and reading in the data

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

t <- list(family = "Arial",
          size = 20)
tf <- list(family = "Arial",
           size = 12)

wd <- getwd()
setwd("../..")

to_read_dat_11 <- paste0("report_data/",filename_11,".csv")
to_read_dat_21 <- paste0("report_data/",filename_21,".csv")

data_11 <- fread(to_read_dat_11)
data_21 <- fread(to_read_dat_21)

setwd(wd)

## putting together the data for the front chart of the app, which includes aggregated categories

  # 2011
data_11$passports_new <- data_11$passports_region

data_11$passports_new[data_11$passports == "United Kingdom"] <- "United Kingdom"
data_11$passports_new[data_11$passports == "Ireland"] <- "Ireland"

data_11$passports_new[grep("EU:",data_11$passports)] <- "EU"

data_11$passports_new[data_11$passports_new == "Europe"] <- "Rest of Europe"

regs_dat_11 <- data_11[,.(value = sum(value),percs = sum(percs)),
                 by = list(
                   passports_region,
                   passports_new,
                   geography
                 )]

regs_dat_11$passports <- regs_dat_11$passports_new

data_11 <- data_11[!(data_11$passports %in% c("United Kingdom","Ireland")),]


  # 2021
data_21$passports_new <- data_21$passports_region

data_21$passports_new[data_21$passports == "United Kingdom"] <- "United Kingdom"
data_21$passports_new[data_21$passports == "Ireland"] <- "Ireland"

data_21$passports_new[grep("EU:",data_21$passports)] <- "EU"

data_21$passports_new[data_21$passports_new == "Europe"] <- "Rest of Europe"

regs_dat_21 <- data_21[,.(value = sum(value),percs = sum(percs)),
                 by = list(
                   passports_region,
                   passports_new,
                   geography
                 )]

regs_dat_21$passports <- regs_dat_21$passports_new

data_21 <- data_21[!(data_21$passports %in% c("United Kingdom","Ireland")),]

## putting together the app

  ## defining the choices in the app
choices <- c(
  "Main countries and groups",
  "Africa",
  "Antarctica and Oceania",
  "Europe",
  "Middle East and Asia",
  "The Americas and the Caribbean"
)

  ## putting together the user interface

ui <- fluidPage(
  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),
  
  selectInput("cat", "Choose Category", choices, selected = default_category),
  
  plotly::plotlyOutput("plot")
  
)

  ##  putting together the server
server <- function(input,output,session){

output$plot <- plotly::renderPlotly({

  input <- input$cat
  
  # select
  if(input == "Main countries and groups"){
    plot_dat_11 <- regs_dat_11
  }else{
    plot_dat_11 <- data_11[data_11$passports_region == input]
  }
  
  if(input == "Main countries and groups"){
    plot_dat_21 <- regs_dat_21
  }else{
    plot_dat_21 <- data_21[data_21$passports_region == input]
  }
  
  # order + setup
  
  plot_dat_21$Percentage <- plot_dat_21$percs
  plot_dat_21$Passport <- plot_dat_21[,..var_name]
  
  plot_dat_11$Percentage <- plot_dat_11$percs
  plot_dat_11$Passport <- plot_dat_11[,..var_name]
  
  var_name <- "Passport"
  to_plot <- "Percentage"
  
  ords <- order(plot_dat_21$percs, decreasing = TRUE)
  
  plot_dat_21 <- plot_dat_21[ords,]
  
  positions <- rev(unlist(plot_dat_21[,..var_name]))
  

  # plot
  
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
    scale_x_discrete(limits = positions) + 
    scale_fill_manual(values = rep(pal[1], length(unique(unlist(plot_dat_21[,..var_name])))),
                      limits = rev(positions))

  fin_plot <- ggplotly(out_plot, height = 500, width = 700, tooltip = c("text")) %>%
    layout(
      xaxis = list(tickfont = tf,
                   showgrid = TRUE),
      yaxis = list(tickfont = tf,
                   showgrid = FALSE),
      font = tf
    )
  
  fin_plot
  
})
  

}

  ## making the final app
shinyApp(ui = ui,
         server = server,
         options = list(width = 750, 
                        height = 550))

