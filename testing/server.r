library(leaflet)
server <- (function(input, output) {
	 
		map <- reactive({
		your.map <- leaflet() %>% addTiles()
		 map <- your.map
		 return(map)
		 })
		 output$myMap <- renderLeaflet({
		 map<-map()
		 if(interactive()) print(map)
		 })
		 })