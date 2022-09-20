
## general inputs

filename <- "lond_vs_rest_21"

default_category <- "Regions"

var_name <- "passports"

tooltip_var <- "Country/Region of Birth: "

suf <- "%"

## setting up plot theme and reading in data

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

wd <- getwd()
setwd("../..")

to_read_dat <- paste0("report_data/",filename,".csv")

data <- fread(to_read_dat)

setwd(wd)

## putting together the data for the front chart of the app, which includes aggregated categories

data$passports_new <- data$passports_region

data$passports_new[data$passports == "United Kingdom"] <- "United Kingdom"
data$passports_new[data$passports == "Ireland"] <- "Ireland"

data$passports_new[grep("EU:",data$passports)] <- "EU"

data$passports_new[data$passports_new == "Europe"] <- "Rest of Europe"

regs_dat <- data[,.(value = sum(value),percs = sum(percs)),
                 by = list(
                   passports_region,
                   passports_new,
                   geography
                 )]

regs_dat$passports <- regs_dat$passports_new

  ## removing United Kingdom and Ireland from the list of countries in the datasets, as they are included in the front chart
data <- data[!(data$passports %in% c("United Kingdom","Ireland")),]


## saving the font to be used in the app
tf <- list(family = "Arial",
           size = 12)

## making the app

  ## defining the choices for the app
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

  # putting together the server
server <- function(input,output,session){

output$plot <- plotly::renderPlotly({
  
  input <- input$cat
  
  # select
  if(input == "Main countries and groups"){
    plot_dat <- regs_dat
  }else{
    plot_dat <- data[data$passports_region == input]
  }
  
  # order + setup
  
  lond <- plot_dat$geography == "London"
  rest <- plot_dat$geography == "England"
  
  plot_dat$Percentage <- plot_dat$percs
  plot_dat$`Country of Birth` <- plot_dat[,..var_name]
  
  var_name <- "Country of Birth"
  to_plot <- "Percentage"

  ords <- order(plot_dat$percs[lond], decreasing = TRUE)
  
  positions <- rev(unlist(plot_dat[lond,][ords,][,..var_name]))
  
  # plot
  
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
    scale_x_discrete(limits = positions) + 
    scale_fill_manual(values = rep(pal[1], length(unique(unlist(plot_dat[,..var_name])))),
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

  ## generating the final app
shinyApp(ui = ui,
         server = server,
         options = list(width = 750, 
                        height = 550))

