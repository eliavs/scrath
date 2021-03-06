library(htmlwidgets)
library(lubridate) #to play with dates
library(ggplot2)
library(plyr) #combining the data
library(ggmap)
library(leaflet)
library(reshape2)
Sys.setlocale(category = "LC_ALL", locale = "hebrew")
shinyServer(function(input, output, session) {

 ##-------------
##upload the data
##---------------
userdata <- reactive({
	Sys.setlocale(category = "LC_ALL", locale = "hebrew")
    rawdata <- read.csv("data.txt", stringsAsFactors =F, header=T,fileEncoding = "UTF-8")
	mapdata <-read.csv("places.csv", stringsAsFactors =F, header=T,fileEncoding = "UTF-8")
	names(rawdata)[24]<-"setid"
	new_data<-join(rawdata,mapdata,"setid")
	raw_date<-new_data[12]
	foo <- unlist(strsplit(unlist(raw_date,use.names = F),"/"))
	df<-data.frame(x=c(foo[seq(1,length(foo),3)]),y=c(foo[seq(2,length(foo),3)]),z=c(foo[seq(3,length(foo),3)]))
	names(new_data)[12]<-"dec_date"
	new_data$new_date<-strptime(paste(as.character(df[,1]),as.character(df[,2]), as.character(df[,3])),format="%e %m %Y")
	raw_harshama<-new_data[13]
	foo <- unlist(strsplit(unlist(raw_harshama,use.names = F),"/"))
	df<-data.frame(x=c(foo[seq(1,length(foo),3)]),y=c(foo[seq(2,length(foo),3)]),z=c(foo[seq(3,length(foo),3)]))
	names(new_data)[13]<-"har_date"
	new_data$harshama_date<-strptime(paste(as.character(df[,1]),as.character(df[,2]), as.character(df[,3])),format="%e %m %Y")
	
	return(new_data)
  })
  
   output$downloadbutton <- renderUI({
    downloadButton('downloadPlot', 'לשמירה')
   
   })
	  ########----------
	  #situation dropdown
	  #בחירת
	  ##----------
   output$SlicingData <- renderUI({
		data1<-userdata()
        situation <- data1[10]
		Mode<-max(names(table(situation)))
        selectInput('chosenSituation','', choices=unique(situation),selected = Mode,multiple = T)
    })
	########----------
	  #reason dropdown
	  #סיבה
	  ##----------
   output$SlicingReason <- renderUI({
		data1<-userdata()
        Reason <- data1[11]
        selectInput('chosenReason','', choices=c(unique(Reason),"all"),selected ="all",multiple = T)
    })
	  ########----------
	  #major dropdown
	  #בחירת חוג
	  ##----------
   output$SlicingMajor <- renderUI({
		
		data1<-userdata()
        Major <- data1[20]
		choices1<-c(unique(Major),'all')
        selectInput('chosenMajor','', choices=choices1,selected = "all",multiple = T)
		
    })
	########----------
	  #places dropdown
	  #בחירת מיקום
	  ##----------
	output$SlicingLocation <- renderUI({
		data1<-userdata()
        location <- data1$name
        selectInput('chosenLocation','', choices=c(unique(location),"all"),multiple = T,selected="all")  
		})
	########----------
	  #date range
	  #בחירת זמן
	  ##----------
	
	output$TimeSlicer <-renderUI({
		data1 <- userdata()
		
		date_dec<-data1$new_date
		min = min(range(date_dec,na.rm=TRUE))
		max = max(range(date_dec,na.rm=TRUE))
		dateRangeInput("Time",'',start = min, end = max, min = min, max = max, format = "dd-mm-yy", startview = "month", weekstart = 0, language = "he", separator = " עד ")
	}) 
	  
	
	##----------------
  ##build a graph first tab
  #------------------------
    plotdata<-reactive({
		data1<-userdata() 
		Situation <- c(input$chosenSituation)
		Reason <- c(input$chosenReason)
		Major <- c(input$chosenMajor)
		Location <- c(input$chosenLocation)
		data1<-data1[data1[[10]]==c(Situation),]
		if (Reason!="all"){
			data1<-data1[data1[[11]]==c(Reason),]
		}
		if (Major!="all"){
			data1<-data1[data1[[20]]==c(Major),]
		}
		if (Location!="all"){
			data1<-data1[data1[['name']]==c(Location),]
		}
		mini <- min(c(input$Time))
		maxi <- max(c(input$Time))
		int1 <- new_interval(ymd(mini),ymd(maxi))
		
		data1 <- data1[ymd(data1$new_date) %within% int1,]
		
		return(data1)
	})
	
	##----------------
  ##build a time plot
  #------------------------
    plotdate<-reactive({
		foo<-plotdata()
		df <- data.frame(x = c(foo$new_date,foo$harshama_date),g = gl(2, length(foo[,1]),labels =c("תאריך החלטה", "תאריך הרשמה")))
		a <- ggplot(data = df, aes(x , color = g)) + stat_ecdf() +labs(title = "תאריכי הרשמה", x= " תאריך", y= "כמות", color = "מקרא")
		leafmap <-ggplot(data = foo) + stat_ecdf(data = foo , aes(new_date),color = "green") + stat_ecdf(data = foo, aes(harshama_date),color = "red")+ labs(title = "תאריכי הרשמה", x= " תאריך", y= "כמות")
    
		
		return(a)
	})

	##----------------
  ##build a leaflet map
  #-------------------
	map<-reactive({
		 data1<-plotdata()
		 map_data<- data1[!is.na(data1$x),]
		 a<-aggregate(map_data[c(21,26,27)], by = list(map_data$name), FUN = mean)
		 pal1 <- colorNumeric(palette = heat.colors(10),  domain = a$ציון.עברית,10)
		 #print(pal1)
		 map <- leaflet() %>% addTiles()
		 map1 <- map %>% addCircleMarkers(data = a, lat = ~y, lng = ~x, color = ~pal1(ציון.עברית),radius = 3, popup = a[[1]]) %>% addLegend(position = "bottomleft",pal = pal1, values = a$ציון.עברית	,title = "מקרא",opacity = 1)
		 return(map1)
	 })
	
	##plot_leaflet map
	output$myMap<-renderLeaflet({
		 print("here")
		 map()
		 })
		 
	##plot the data
    output$viewData<-renderPlot({
		 #input$submit
		 plotdate()
		 },height = 400, width = 700)

	output$downloadPlot <- downloadHandler(
		filename = function() {
		paste('saved-', Sys.time(), '.png', sep='')
		},
		content = function(file) {
		png(file)
		print(plotdate())
		dev.off()
		}
		
	)
		 
	
	
	
})