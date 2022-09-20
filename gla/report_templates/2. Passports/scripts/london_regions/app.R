## inputs

filename <- "lond_21"

default_category <- "Regions"

selvar_name <- "passports_region"

var_name <- "passports"

tooltip_var <- "Country/Region of Birth: "


## setting up

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
       passports_new
     )]

regs_dat$passports <- regs_dat$passports_new

data <- data[!(data$passports %in% c("United Kingdom","Ireland")),]


## making the app

choices <- c(
  "Main countries and groups",
  "Africa",
  "Antarctica and Oceania",
  "Europe",
  "Middle East and Asia",
  "The Americas and the Caribbean"
)

ui <- fluidPage(
  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),
  
  selectInput("cat", "Choose Category", choices, selected = default_category),
  
  plotly::plotlyOutput("plot")
  
)


server <- function(input,output,session){

output$plot <- plotly::renderPlotly({
  
  input <- input$cat
  
  # select
  if(input == "Main countries and groups"){
    plot_dat <- regs_dat
  }else{
    plot_dat <- data[data$passports_region == input]
  }
  
  # order
  ords <- order(plot_dat$percs, decreasing = TRUE)
  
  plot_dat <- plot_dat[ords,]
  
  positions <- unlist(plot_dat[,..var_name])
  
  # plot
  out_plot <- ggplot(data = plot_dat,
                     aes(x = .data[[var_name]],y = percs,
                         text = paste0(tooltip_var, .data[[var_name]],
                                       "<br>",
                                       "Percentage: ", percs,"%",
                                       "<br>",
                                       "Count: ", value))) + 
    geom_bar(stat = "identity", fill = pal[1]) + 
    coord_flip() + 
    #scale_colour_manual(values = pal) + 
    theme(axis.text.x = element_text(angle = 0,size = 10,hjust = 1,vjust = 2)) + 
    scale_y_continuous(labels = dollar_format(prefix = "",suffix = "%")) + 
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
  
})
  

}

shinyApp(ui = ui,
         server = server,
         options = list(width = 750, 
                        height = 550))






