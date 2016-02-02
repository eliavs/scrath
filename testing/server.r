library(htmlwidgets)
library(leaflet)
Sys.setlocale(category = "LC_ALL", locale = "hebrew")
shinyServer(function(input, output,session) {
map<-reactive({ 
  map_data<- read.csv("../map-data.csv",quote = "", header=T,encoding ="utf-8",row.names = NULL,  stringsAsFactors =F, sep =",")

map_data[,221] <-as.numeric(map_data[,221])
map_data[,220] <-as.numeric(map_data[,220])

 name1 <-paste(names(map_data[10]),collapse=' ' )
 
  a<-aggregate(map_data[c(10,220,221)], by = list(map_data$name), FUN = mean)
 
  names(a)[2]<-"dat"
   print(str(a))
   a<-na.omit(a)
   print(str(a))
  pal1 <- colorNumeric(palette = heat.colors(10),  domain = a$dat,10)
  map <- leaflet() %>% addTiles()
  map1 <- map %>% addCircleMarkers(data = a, lat = ~y, lng = ~x, color = ~pal1(dat) ,radius = 3, popup = a[[1]])%>% addLegend(position = "bottomleft",pal = pal1, values= a$dat	,title = name1,opacity = 1)
  
  return(map1)
})


output$myMap<-renderLeaflet({
  map()
})
})