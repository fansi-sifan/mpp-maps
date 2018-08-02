
# README ------------------------------------------------------------------

# KEY FUNCTIONS to be developed

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/

# 1. (/) File upload: https://shiny.rstudio.com/gallery/file-upload.html
# 2. (/) Choose the variable to map for state color: https://github.com/daattali/colourpicker
# 3. (/) Choose from the color template: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
# 4. (/) Save the plot to local
# 5. ( ) MSA bubble maps
# 6. ( ) County maps
# 7. (/) Select donwload option (pdf/png)

# Author: Sifan Liu
# Date: Thu Jul 26 09:13:31 2018
# --------------
# pkgs <- c('dplyr','maps','mapproj','ggplot2','scales','ggthemes','RColorBrewer','plotly','fiftystater',
          # 'shiny','colourpicker','plyr')

# check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
# if(any(!check)){
#     pkgs.missing <- pkgs[!check]
#     install.packages(pkgs.missing)
#     check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
#   }

# sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)

# Read Data ---------------------------------------------------------------


library('maps')
library('mapproj')
library('ggplot2')
library('scales')
library('ggthemes')
library('RColorBrewer')
library('plotly')
library('fiftystater')
library('shiny')
library('colourpicker')
library('plyr')


states <- map_data("state")



# Shiny R -----------------------------------------------------------------


ui <- fluidPage(
  titlePanel("US State Mapping Tool"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Contact: sliu@brookings.edu"),
      
      fileInput('file1',"Choose CSV File",
                accept = c(".csv")),
      
      actionButton("choice", "show Data"),
      
      tags$hr(),
      
      selectInput("var", "Choose a variable to plot",
                  choices = NULL),
    
      colourInput("low", "Choose a color for low value","#deebf7"),
      colourInput("high", "Choose a color for high value", "#08519c"),
      
      radioButtons("filetype", "File type:", choices = c("png", "pdf")),
      downloadButton("plot", label = "Download the plot")
    ),
   
     mainPanel(
      tableOutput("contents"),
      plotOutput("map"))
      
    )
  )

# Server logic ----
server <- function(input, output,session) {
  
  info <- eventReactive(input$choice,{
    req(input$file1)
    
    df <- read.csv(input$file1$datapath)
    df$State <- tolower(df$State)
    vars <- names(df[-1])
    updateSelectInput(session,'var','Choose a variable to plot', choices = vars)
    
    df
  })
  
  output$contents <- renderTable({

    input_data <- info()
    head(input_data)

  })

  output$map <- renderPlot({
    print(plotInput())
  })
    
    
  plotInput <- function(){
    
    input_data <- info()
    
    map_wrapper <- ggplot() + 
      geom_map(data = states, map = fifty_states, size = 1,
               aes(x = long, y = lat, map_id = region), 
               fill = "light grey", color = "white") +
      geom_map(data = input_data, map = fifty_states, color = "white",
               aes_string(fill = input$var, map_id = 'State')) +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      theme_map() %+% 
      theme(legend.key = element_rect(colour = NA, fill = NA))
    
    if (is.discrete(input_data[[input$var]])){
      map_wrapper + 
        scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(length(unique(input_data[[input$var]]))),
                          labels = comma, 
                          name = gsub("\\."," ",input$var))
    } 
    else {
      map_wrapper + 
        scale_fill_continuous(labels = comma, name = gsub("\\."," ",input$var),
                              low = input$low, high = input$high)
    }
  } 
    
  
  output$plot <- downloadHandler(
    filename = function(){
      paste("plot", input$filetype, sep = ".")
    },
    content = function(file){
      ggsave(file, plotInput(), device = input$filetype, width = 16, height = 10.4, bg = "transparent")
    }
  )
  
}

# Run app ----
shinyApp(ui, server)



