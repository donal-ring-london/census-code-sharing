
## general inputs

filename <- "boroughs_21"

default_category <- "United Kingdom"

var_name <- "passports"

var_name_out <- "Passports"

geog <- " Borough" # either Borough or Region



## setting up

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

wd <- getwd()
setwd("../..")

to_read_dat <- paste0("report_data/",filename,".csv")

data <- fread(to_read_dat)

setwd(wd)

## removing the "other" categories for the app

rem <- c(
  "Other Central and Western Africa",
  "Other South and Eastern Africa",
  "Other Oceania",
  "EU: Other EU countries",
  "Rest of Europe: Other Europe",
  "Other Eastern Asia",
  "Other Middle East",
  "Other South-East Asia",
  "Other Southern Asia",
  "Other Caribbean"
)

data <- data[!(data$passports %in% rem),]

data <- data[data$passports != "United Kingdom",]


## making the app

ui <- fluidPage(
  
  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),
  
  selectInput("cat","Choose Category",unique(data[,"geography",])),

  plotly::plotlyOutput("plot")
  
)


server <- function(input,output,session) {
  
  output$plot <- plotly::renderPlotly({
    
    plot_cat <- input$cat
    
    plot_dat <- data[unlist(data[,"geography"]) == plot_cat,]
    
    ords <- order(plot_dat$percs, decreasing = TRUE)
    
    positions <- plot_dat[,..var_name][ords][1:20]
    
    positions <- unlist(positions)
    
    ### Making the plot
    
    out_plot <- ggplot(data = plot_dat,
                       aes(x = .data[[var_name]],y = percs, 
                           text = paste0("Passports: ", .data[[var_name]],
                                         "<br>",
                                         "Count: ", value,
                                         "<br>",
                                         "Percentage: ", percs, "%"))) + 
      geom_bar(stat = "identity", fill = pal[1]) + 
      coord_flip() + 
      theme(axis.text.x = element_text(angle = 0,size = 10,hjust = 1,vjust = 2)) + 
      scale_y_continuous(labels = dollar_format(prefix = "",suffix = "%")) + 
      scale_x_discrete(limits = rev(positions))
    
    tf <- list(family = "Arial",
               size = 12)
    
    ggplotly(out_plot,height = 500, width = 700, tooltip = "text") %>%
      layout(
        xaxis = list(tickfont = tf,
                     showgrid = TRUE),
        yaxis = list(tickfont = tf,
                     showgrid = FALSE))
    
  })
  
}


## making the final app

shinyApp(
  ui = ui,
  server = server
)

