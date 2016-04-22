shinyUI(fluidPage(
  
  h1(strong("K-Means Tutorial App"), align="center"),
  p("Developed by Dean Young (deanyoung168@gmail.com)"),
  "Reference material: ",
  tags$a(href="http://www-bcf.usc.edu/~gareth/ISL/", "http://www-bcf.usc.edu/~gareth/ISL/"),
  
  sidebarLayout(
    sidebarPanel(
      h3(strong("Options")),
      selectizeInput('df', 'Select Dataset', choices = c("mtcars","iris")),
      uiOutput('var'),
      sliderInput("k", "Select K", min = 1, 
                  max = 10, value = 3),
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