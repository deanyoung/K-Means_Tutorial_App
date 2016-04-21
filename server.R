shinyServer(function(input, output) {
  
  values <- reactiveValues()
  
  values$current.iter <- 0
  values$current.step <- "a"
  values$counter <- 1
  
  idf <- reactive({
    
    switch(input$df,
           "mtcars"={idf <- mtcars},
           "iris"={idf <- iris}
           
           
           )
    
  })
  
  output$var <- renderUI({
    selectizeInput(
      'var', 'Select Variables', choices = colnames(idf()),
      multiple = TRUE, options = list(maxItems = 2)
    )
    
  })
  
  df <- reactive({
    
    idf()[,input$var]
    
  })
  
  stop.cond <- reactive({
    
    if(length(input$var) != 2){
      
      TRUE
      
    }else{
      
      FALSE
      
    }
    
  })
  
  glist <- reactive({
    
    if(stop.cond() == FALSE){

      values$current.iter <- 0
      values$current.step <- "a"
      values$counter <- 1
      
      km.iterate(df(),input$k)
      
    }else{
      
      stop("Waiting for 2 variables to be selected")
      
    }
    
  })
  
  output$max <- renderText({
    
    if(stop.cond() == FALSE){
      
      paste("Total iterations:", as.character((length(glist()) - 1)/2))
      
    }else{
      
      ""
      
    }
  })
  
  output$graph <- renderPlot({
    
    glist()[values$counter]
    
  })
  
  output$status <- renderText({
      
    if(stop.cond() == FALSE){  
      "Current iteration: Initial random assignment"
    }else{
      
      ""
      
    }
  })
    
  output$desc <- renderText({

    if(stop.cond() == FALSE){
      "Randomly assign observations to clusters with equal probability"
    }else{
     
      "" 
      
    }
    
  })
  
  observeEvent(input$forward,{
    
    if(stop.cond()==FALSE){
    
      if(values$counter < length(glist())){
        values$counter <- values$counter + 1
        
        if(values$current.iter==0){
          values$current.iter <- 1
          values$current.step <- "a"
          
          
        }else{
          
          if(values$current.step=="a"){
            values$current.step <- "b"
          }else{
            values$current.step <- "a"
            values$current.iter <- values$current.iter + 1
            
          }
          
          
        }
        
        
      }
      
      output$graph <- renderPlot({
        
        glist()[values$counter]
        
      })
      
      output$status <- renderText({
        
        if(values$current.iter==0){
          
          "Current iteration: Initial random assignment"
          
        }else{
          
          paste("Current iteration:", values$current.iter, "Step:", values$current.step)
          
        }
        
      })
      
      output$desc <- renderText({
        
        if(values$current.iter==0){
          
          "Randomly assign observations to clusters with equal probability"
          
        }else{
          
          if(values$current.step=="a"){
            
            "(Re)compute the centroids"
            
          }else{
            
            if(values$current.iter==(length(glist())-1)/2){
              
              "(Re)assign the observations to cluster based on nearest centroid. No change in cluster assignments.
              Stopping condition met. K-means algorithm is now finished."
              
            }else{
            
              "(Re)assign the observations to cluster based on nearest centroid"
              
            }
            
          }
          
          
        }
        
      })
      
    }
    
  })
    
    
  
  observeEvent(input$backward,{
    
    if(stop.cond()==FALSE){  
    
      if(values$counter > 1){
        values$counter <- values$counter - 1
        
        if(values$current.iter <= 1 & values$current.step=="a"){
          
          values$current.iter <- 0
    
        }else{
          
          if(values$current.step=="a"){
            
            values$current.step <- "b"
            values$current.iter <- values$current.iter - 1
            
          }else{
            values$current.step <- "a"
            
  
          }
          
          
        }
        
        
      }
      
      output$graph <- renderPlot({
        
        glist()[values$counter]
        
      })
      
      output$status <- renderText({
        
        if(values$current.iter==0){
          
          "Current iteration: Initial random assignment"
          
        }else{
          
          paste("Current iteration:", values$current.iter, "Step:", values$current.step)
          
        }
        
      })
      
      output$desc <- renderText({
        
        if(values$current.iter==0){
          
          "Randomly assign observations to clusters with equal probability"
          
        }else{
          
          if(values$current.step=="a"){
            
            "(Re)compute the centroids"
            
          }else{
            
            if(values$current.iter==(length(glist())-1)/2){
              
              "(Re)assign the observations to cluster based on nearest centroid. No change in cluster assignments.
              Stopping condition met. K-means algorithm is now finished."
              
            }else{
              
              "(Re)assign the observations to cluster based on nearest centroid"
              
            }
            
          }
          
          
        }
        
      })
  
    }
  
  
})
  
})