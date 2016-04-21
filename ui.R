shinyUI(fluidPage(
  
  h1(strong("K-Means Tutorial App"), align="center"),
  
  sidebarLayout(
    sidebarPanel(
      h3(strong("Options")),
      selectizeInput('df', 'Select Dataset', choices = c("mtcars","iris")),
      uiOutput('var'),
      sliderInput("k", "Select K", min = 1, 
                  max = 10, value = 3),
      actionButton("run", "Run"),
      h3(strong("Step Through")),
      actionButton("backward","Step Backward"),
      actionButton("forward","Step Forward")
      
      
    ),
    
    mainPanel(
      plotOutput("graph"),
      textOutput("status"),
      textOutput("max"),
      textOutput("desc")
    )
  
  )
  
  
  
  
        )
)