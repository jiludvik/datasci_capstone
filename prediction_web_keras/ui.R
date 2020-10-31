# Load libraries
library(shiny)
library(shinythemes)

# Define the UI

shinyUI(fluidPage(
    
    # Theme
    theme = shinytheme("flatly"),
    
    # Application title
    titlePanel("Word Prediction With Deep Learning"),
    
    # Sidebar ####    
    sidebarLayout(
        
        sidebarPanel(
            # Text input
            textInput("text", label = ('Your Text'), value = ''),
            
            # Link to report
            helpText(a('More info on the app',
                       href='http://rpubs.com/', 
                       target = '_blank')
            ),
            
            # Link to repo
            helpText(a('Source code',
                       href='https://github.com/jiludvik/datasci_capstone',
                       target = '_blank')
            ),
            # Wordcloud output
            #plotOutput('wordcloud')
        ),
        
        # Main Panel
        
        mainPanel(
            
            wellPanel(
                strong("Next Word"),
                textOutput("predicted_word")
                #dataTableOutput('table'),
                #plotOutput('wordcloud', width="50%")
            )
        ) 
    )
)
)