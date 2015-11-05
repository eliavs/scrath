 
 shinyServer(function(input, output,session) {
   
   output$SlicingReason <- renderUI({
     l = dir("data")
     filelist = as.integer(substr(l, 1,4))
     selectInput('chosenfile','', choices=c(filelist),selected =max(filelist),multiple = T)
   
 })
 
 
 })