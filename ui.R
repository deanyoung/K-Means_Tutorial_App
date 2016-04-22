shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    
                    h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color: green;
                    }
                    
                    "))
    ),
  
  h1(strong("K-Means Tutorial App"), align="center"),
  helpText("Developed by Dean Young (deanyoung168@gmail.com)"),
  helpText("Reference material: ", 
           tags$a(href="http://www-bcf.usc.edu/~gareth/ISL/", "http://www-bcf.usc.edu/~gareth/ISL/")),
  
  sidebarLayout(
    sidebarPanel(
      
      list(tags$head(tags$style(".well {background-color: green; 
                                }"))),
      

        
      h3(strong("Options"), align="center"),
      
      selectizeInput('df', 'Select Dataset', choices = c("mtcars","iris")),
      uiOutput('var'),
      sliderInput("k", "Select K", min = 1, 
                  max = 10, value = 3),
      
      h3(strong("Step Through"), align="center"),
      actionButton("backward","Step Backward"),
      actionButton("forward","Step Forward")
      
      
    ),
    
    mainPanel(
      list(tags$head(tags$style("body {background-color: #ADD8E6; }"))),
      plotOutput("graph"),
      textOutput("status"),
      textOutput("max"),
      textOutput("desc")
      
    )
  
  )
  
  
  
  
        )
)