
## Regions plots

filename <- "regions_21_10"

geog_file <- "gb_regions_simpr"

default_category <- "Christian"

var_name <- "religion"

var_name_out <- "Religion"

geog <- " Region"


### setting plot theme, reading in data

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

wd <- getwd()
setwd("../..")

to_read_dat <- paste0("report_data/",filename,".csv")

data <- fread(to_read_dat)

to_read_geog <- paste0("geo_data/",geog_file,".shp")

borders <- st_read(to_read_geog, quiet = TRUE)

setwd(wd)

borders <- data.table(borders[,c("GSS_CODE","NAME")])
setkey(borders,"GSS_CODE")

## making the app

ui <- fluidPage(
  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),
  
  selectInput("cat","Choose Category",unique(data[,..var_name]),selected = default_category),
  
  plotly::plotlyOutput("plot")
  
)

server <- function(input,output,session) {
  
  output$plot <- plotly::renderPlotly({
    plot_cat <- input$cat
    
    plot_dat <- data[unlist(data[,..var_name]) == plot_cat,]
    
    ords <- order(plot_dat$percs, decreasing = TRUE)
    
    positions <- rev(plot_dat$geography[ords])
    
    ### Making the plot
    
    out_plot <- ggplot(data = plot_dat,
                       aes(x = geography,y = percs, 
                           text = paste0("Region: ", geography,
                                         "<br>",
                                         "Count: ", value,
                                         "<br>",
                                         "Percentage: ", percs, "%"))) + 
      geom_bar(stat = "identity", fill = pal[1]) + 
      coord_flip() + 
      #scale_colour_manual(values = pal) + 
      theme(axis.text.x = element_text(angle = 0,size = 10,hjust = 1,vjust = 2)) + 
      scale_y_continuous(labels = dollar_format(prefix = "",suffix = "%")) + 
      scale_x_discrete(limits = positions)
    
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


shinyApp(
  ui = ui,
  server = server
)
