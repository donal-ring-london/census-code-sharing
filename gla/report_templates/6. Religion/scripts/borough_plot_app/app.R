
## inputs

filename <- "boroughs_21_58"

default_category <- "City of London"

var_name <- "religion_det"

var_name_out <- "Religion"

geog <- " Borough" #


## setting plot theme, reading in data

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

wd <- getwd()
setwd("../..")

to_read_dat <- paste0("report_data/",filename,".csv")

data <- fread(to_read_dat)

setwd(wd)

## aggregating no religion, selecting religions to plot

no_religion <- c("No religion: Agnostic","No religion: Atheist",
                 "No religion: Free Thinker","No religion: No religion",
                 "No religion: Humanist","No religion: Realist")

data[data$religion_det %in% no_religion,]$religion_det <- "No religion"
colnames(data)

data <- data[,.(value = sum(value), percs = sum(percs)),
     by = list(geography,
               religion_det)]

keep <- c(
  "Christian",
  "No religion",
  "Muslim",
  "Hindu",
  "Jewish",
  "Sikh",
  "Buddhist",
  "Other religion: Jain",
  "Other religion: Pagan",
  "Religion not stated"
)

data <- data[data$religion_det %in% keep,]


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
    
    positions <- plot_dat[,..var_name][ords]
    
    positions <- unlist(positions)
    
    ### Making the plot
    
    out_plot <- ggplot(data = plot_dat,
                       aes(x = .data[[var_name]],y = percs, 
                           text = paste0("Religion: ", .data[[var_name]],
                                         "<br>",
                                         "Count: ", value,
                                         "<br>",
                                         "Percentage: ", percs, "%"))) + 
      geom_bar(stat = "identity", fill = pal[1]) + 
      coord_flip() + 
      #scale_colour_manual(values = pal) + 
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


shinyApp(
  ui = ui,
  server = server
)

