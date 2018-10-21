library(shiny)
library(CANSIM2R)
library(dplyr)
library(ggplot2)
library(plotly)
# install.packages("CANSIM2R")

# We will download Table 36-10-0434-01:
# Gross domestic product (GDP) at basic prices, by industry, monthly (x 1,000,000)
# From Statistics Canada

gdp_raw <- getCANSIM(36100434, raw=TRUE)

# Print the first rows of the dataset
head(gdp_raw)

# Verify how many unique variables exist in a given colum name 
unique(gdp_raw$Seasonal.adjustment)
unique(gdp_raw$Prices)
unique(gdp_raw$GEO)

# Cleaning the data:
gdp <- gdp_raw %>% 
  filter(Seasonal.adjustment %in% "Seasonally adjusted at annual rates")%>%
  select("time"="REF_DATE", 
         "naics"="North.American.Industry.Classification.System..NAICS.", 
         "value"="VALUE", 
         "prices"="Prices") 

# Creating a new column time1 which will be a "date" object.
gdp$time1<-as.Date(paste((gdp$time), "-01", sep=""))

gdp$time1<-as.Date(gdp$time1)

head(gdp)


# Define UI for app that draws a line chart  ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("GDP Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Dropdown menu that allows users to select a specific industry and
      #        a specific price measure ----
      selectInput(inputId = "industry",
                  label = "Select an industry:",
                  choices = unique(gdp$naics), multiple= TRUE), #
      radioButtons(inputId = "prices",
                  label = "Select price measure:",
                  choices = unique(gdp$prices))
      
    ),
    
    # Output: Line chart ----
    mainPanel(
      
      # Output: Histogram ----
      plotlyOutput("plot1", height = "400px", width = "400px")
      # plotOutput(outputId = "plot2", height = "800px", width = "700px")
      
      
    )
  )
)

# Define server logic required to draw a line chart ---- ----
server <- function(input, output) {
  
  # Plot of the monthly GDP ----
  # After users choose a:
  # - specific industry in the dropdown menu
  # - specific price measurement
  # 
  # The server will generate a line chart which is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$industry and input$price) change
  # 2. Its output type is a plot
  
  
  output$plot1 <- renderPlotly({
    
    dat <- gdp %>%
      filter(naics %in% input$industry ) %>%
      filter(prices %in% input$prices)
    
    ggplot(dat)+
      # geom_point(aes(x= time1 , y= value, color=dat$naics))+
      geom_line(aes(x= time1 , y= value, color=dat$naics))+
      theme(axis.text.x = element_text(color="#993333",
                                       size=11, angle=90))+
      scale_x_date(date_labels = "%b %y", date_breaks = "4 year")
    
  })
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
