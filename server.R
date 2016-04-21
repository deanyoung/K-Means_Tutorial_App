shinyServer(function(input, output) {
  
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
    
    glist()[counter]
    
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
    
      if(counter < length(glist())){
        counter <<- counter + 1
        
        if(current.iter==0){
          current.iter <<- 1
          current.step <<- "a"
          
          
        }else{
          
          if(current.step=="a"){
            current.step <<- "b"
          }else{
            current.step <<- "a"
            current.iter <<- current.iter + 1
            
          }
          
          
        }
        
        
      }
      
      output$graph <- renderPlot({
        
        glist()[counter]
        
      })
      
      output$status <- renderText({
        
        if(current.iter==0){
          
          "Current iteration: Initial random assignment"
          
        }else{
          
          paste("Current iteration:", current.iter, "Step:", current.step)
          
        }
        
      })
      
      output$desc <- renderText({
        
        if(current.iter==0){
          
          "Randomly assign observations to clusters with equal probability"
          
        }else{
          
          if(current.step=="a"){
            
            "(Re)compute the centroids"
            
          }else{
            
            if(current.iter==(length(glist())-1)/2){
              
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
    
      if(counter > 1){
        counter <<- counter - 1
        
        if(current.iter <= 1 & current.step=="a"){
          
          current.iter <<- 0
    
        }else{
          
          if(current.step=="a"){
            
            current.step <<- "b"
            current.iter <<- current.iter - 1
            
          }else{
            current.step <<- "a"
            
  
          }
          
          
        }
        
        
      }
      
      output$graph <- renderPlot({
        
        glist()[counter]
        
      })
      
      output$status <- renderText({
        
        if(current.iter==0){
          
          "Current iteration: Initial random assignment"
          
        }else{
          
          paste("Current iteration:", current.iter, "Step:", current.step)
          
        }
        
      })
      
      output$desc <- renderText({
        
        if(current.iter==0){
          
          "Randomly assign observations to clusters with equal probability"
          
        }else{
          
          if(current.step=="a"){
            
            "(Re)compute the centroids"
            
          }else{
            
            if(current.iter==(length(glist())-1)/2){
              
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