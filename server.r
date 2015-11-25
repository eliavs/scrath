library(htmlwidgets)
library(lubridate) #to play with dates
library(ggplot2)
library(plyr) #combining the data
library(ggmap)
library(leaflet)
library(reshape2)
Sys.setlocale(category = "LC_ALL", locale = "hebrew")
shinyServer(function(input, output,session) {

output$FileUpload <- renderUI({
fileInput('fileChoice_progress', 'העלאת קובץ', accept=c("text/csv","text/comma-separated-values","text/plain,.csv"))
})

filedata <- observe({
    infile <- input$fileChoice_progress
	print(infile$datapath)
	file.copy(from = infile$datapath, to = paste("data/",infile$name))
	})



####-------------------######
##check the data folder for the dat files ##
##and output the years###
##---------------###########

output$Slicingfile <- renderUI({
  l = dir("data")
  
  filelist = as.integer(substr(l, 1,4))
  selectInput('chosenfile','', choices=c(filelist),selected =max(filelist),multiple = T)
})
####-------------------######
##upload the data###########
##---------------###########
userdata <- reactive({
	list_of_data <-list()
	
	if (!is.null(input$chosenfile)){
	files <- c(input$chosenfile)
	}
	else{
	l = max(dir("data"))
	filelist = as.integer(substr(l, 1,4))
	files = c(max(filelist))
	}
	
	Sys.setlocale(category = "LC_ALL", locale = "hebrew")
	for (file in 1:length(files)){
	foon <- paste("data/",files[file],".txt",sep="")
	
	if(.Platform$OS.type == "unix") {
    system(paste("sed 's/\t/,/g' ", foon," | tail -n +2 > file",file, ".txt"))  
	}
   else {
    shell(paste("sed 's/\t/,/g' ", foon," | tail -n +2 > file",file,".txt", sep=""))  
  }
  
  file_1 <- read.csv(paste("file",file,".txt",sep = ""), header=T, stringsAsFactors =F, fileEncoding = "UTF-8" )
  
  
  b<-data.frame(matrix(NA, ncol = 38))  
  names(b)<-names(file_1[1:38])
  for (i in 25:95){
    foon <- data.frame(file_1[1:24], file_1[i:(i+13)])
    foon <- setNames(foon,names(b))
    file2<-rbind(b,foon)}
  #rawdata <- read.csv("data.txt", stringsAsFactors =F, header=T,fileEncoding = "UTF-8")
  mapdata <-read.csv("places.csv", stringsAsFactors =F, header=T,fileEncoding = "UTF-8")
  names(file_1)[214]<-"setid"
  new_data<-join(file_1,mapdata,"setid")
  raw_date<-new_data[36]
  foo <- unlist(strsplit(unlist((raw_date),use.names = F),"/"))
  foo <-as.integer(foo)
  df<-data.frame(x=c(foo[seq(1,length(foo),3)]),y=c(foo[seq(2,length(foo),3)]),z=c(foo[seq(3,length(foo),3)]))
  names(new_data)[36]<-"dec_date"
  new_data$new_date<-strptime(paste(as.character(df[,1]),as.character(df[,2])),format="%e %m")
  raw_harshama<-new_data[37]
  foo <- unlist(strsplit(unlist(raw_harshama,use.names = F),"/"))
  df<-data.frame(x=c(foo[seq(1,length(foo),3)]),y=c(foo[seq(2,length(foo),3)]),z=c(foo[seq(3,length(foo),3)]))
  names(new_data)[37]<-"har_date"
  new_data$harshama_date<-strptime(paste(as.character(df[,1]),as.character(df[,2])),format="%e %m")
  assign(paste("new_data_",file,sep =""), new_data)
  
  list_of_data[[file]]<-get(paste("new_data_",file,sep =""))
  
  }
  return(list_of_data) 
})
####download image button
output$downloadbutton <- renderUI({
  downloadButton('downloadPlot', 'לשמירה')
  
})
#######download data
output$downloaddatabutton <- renderUI({
  downloadButton('downloadDat', 'לשמירת נתונים')
  
})
########----------
#situation dropdown
#
##----------
output$SlicingData <- renderUI({
  data1<-userdata()[[1]]
  situation <- data1[[32]]
  Mode<-max(names(table(situation)))
  selectInput('chosenSituation','', choices=c(unique(situation),"all"),selected = "all",multiple = T)
})
########----------
#reason dropdown
#׳¡׳™׳‘׳”
##----------
output$SlicingReason <- renderUI({
  data1<-userdata()[[1]]
  Reason <- data1[33]
  selectInput('chosenReason','', choices=c(unique(Reason),"all"),selected ="all",multiple = T)
})
########----------
#major dropdown
#׳‘׳—׳™׳¨׳× ׳—׳•׳’
##----------
output$SlicingMajor <- renderUI({
  
  data1<-userdata()[[1]]
  Major <- data1[26]
  choices1<-c(unique(Major),'all')
  selectInput('chosenMajor','', choices=choices1,selected = "all",multiple = T)
  
})
########----------
#places dropdown
#׳‘׳—׳™׳¨׳× ׳׳™׳§׳•׳
##----------
output$SlicingLocation <- renderUI({
  data1<-userdata()[[1]]
  location <- data1$name
  selectInput('chosenLocation','', choices=c(unique(location),"all"),multiple = T,selected="all")  
})

########----------
#date range
#׳‘׳—׳™׳¨׳× ׳–׳׳
##----------

output$TimeSlicer <-renderUI({
  data1 <- userdata()[[1]]
  
  date_dec<-data1$new_date
  min = min(range(date_dec,na.rm=TRUE))
  max = max(range(date_dec,na.rm=TRUE))
  dateRangeInput("Time",'',start = min, end = max, min = min, max = max, format = "dd-mm", startview = "month", weekstart = 0, language = "he", separator = " עד ")
}) 

output$dataradio<-renderUI({radioButtons("madad", "מדדים",
             c( "פסיכומטרי" = "psychometric",
               "בגרות" = "bagrut",
               "אנגלית" = "english",
			   "עברית" = "hebrew"))
			   })


output$downloadhistbutton <- renderUI({
  downloadButton('downloadhist', 'לשמירת נתונים')
})  
##----------------
##build a graph first tab
#------------------------
plotdata<-reactive({
  datas<-list()
  data1<-userdata() 
  for (datafile in data1){
  Situation <- c(input$chosenSituation)
  Reason <- c(input$chosenReason)
  Major <- c(input$chosenMajor)
  Location <- c(input$chosenLocation)
  if (Situation!="all"){
  datafile<-datafile[datafile[[32]]==c(Situation),]
  }
  if (Reason!="all"){
    datafile<-datafile[datafile[[33]]==c(Reason),]
  }
  if (Major!="all"){
    datafile<-datafile[datafile[[26]]==c(Major),]
  }
  if (Location!="all"){
    datafile<-datafile[datafile[['name']]==c(Location),]
  }
  mini <- min(c(input$Time))
  maxi <- max(c(input$Time))
  int1 <- new_interval(ymd(mini),ymd(maxi))
  datafile <- datafile[ymd(datafile$new_date) %within% int1,]
  datas[[length(datas)+1]] <- datafile
  }
  return(datas)
})

##----------------
##build a time plot
#------------------------
plotdate<-reactive({
  data1<-plotdata()
  #foo = data1[[1]]
  print(str(input$chosenfile))
  i = 1
  for (foo in data1){
  if (i<=1){
  df <- data.frame(x = c(foo$new_date,foo$harshama_date),g = gl(2, length(foo[,1]),labels =c(paste("תאריך החלטה " , input$chosenfile[1]), paste("תאריך הרשמה ",input$chosenfile[1]))))
  a <- ggplot(data = df, aes(x , color = g)) + stat_ecdf() +labs(title = "תאריכי הרשמה", x= " תאריך", y= "כמות", color = "מקרא")
  }
  else{
  df <- data.frame(x = c(foo$new_date,foo$harshama_date),g = gl(2, length(foo[,1]),labels =c(paste("תאריך 1 החלטה",input$chosenfile[i]),paste("תאריך 1 הרשמה", input$chosenfile[i]))))
  a<- a + stat_ecdf(data = df , aes(x , color = g))
  }
  #leafmap <-ggplot(data = foo) + stat_ecdf(data = foo , aes(new_date),color = "green") + stat_ecdf(data = foo, aes(harshama_date),color = "red")+ labs(title = "׳×׳׳¨׳™׳›׳™ ׳”׳¨׳©׳׳”", x= " ׳×׳׳¨׳™׳", y= "׳›׳׳•׳×")
  i= i+1
  }
  return(a)
})
##-----------------
### build a histogram
#####----------
his<-reactive({
 data1<-plotdata()
 print(input$madad)
 if (input$madad %in% "psychometric"){
 j= 3
 title = "פסיכומטרי"
 }
 else if (input$madad == "bagrut"){
 j = 10
 title = "בגרות"
 }
 else if (input$madad == "english"){
 j= 17
 title ="אנגלית"}
 else if (input$madad == "hebrew"){
 j= 18
 title ="עברית"}
 i = 1
  for (foo in data1){
  if (i<=1){
  a<- qplot(as.numeric(foo[,j]), geom="histogram", xlab= "ציון" ,main = title)
  }
  }
  return(a)
})
##----------------
##build a leaflet map
#-------------------
map<-reactive({
  data1<-plotdata()[[1]]  
  map_data<- data1[!is.na(data1$x),]
 if (input$madad %in% "psychometric"){
 j= 3
 title = "פסיכומטרי"
 }
 else if (input$madad == "bagrut"){
 j = 10
 title = "בגרות"
 }
 else if (input$madad == "english"){
 j= 17
 title ="אנגלית"}
 else if (input$madad == "hebrew"){
 j= 18
 title ="עברית"}
 name1 <-paste(names(map_data[j]),collapse=' ' )
  a<-aggregate(map_data[c(j,220,221)], by = list(map_data$name), FUN = mean)
  names(a)[2]<-"dat"
  pal1 <- colorNumeric(palette = heat.colors(10),  domain = a$dat,10)
  print(names(a))
  map <- leaflet() %>% addTiles()
  map1 <- map %>% addCircleMarkers(data = a, lat = ~y, lng = ~x, color = ~pal1(dat) ,radius = 3, popup = a[[1]])%>% addLegend(position = "bottomleft",pal = pal1, values= a$dat	,title = name1,opacity = 1)
  
  return(map1)
})
###plot hist
output$histo<-renderPlot({
his()
})
##plot_leaflet map
output$myMap<-renderLeaflet({
  map()
})

##plot the data
output$viewData<-renderPlot({
  #input$submit
  plotdate()
},height = 400, width = 700)

output$downloadPlot <- downloadHandler(
  filename = function() {
    paste(input$chosenfile, input$chosenSituation,Sys.time(), '.png', sep='')
  },
  content = function(file) {
    png(file,width     = 700,
  height    = 400,
  units     = "px",)
    print(plotdate())
    dev.off()
  }
  
)

output$downloadData <- downloadHandler(
    filename = function() { paste(input$chosenfile, input$chosenSituation,Sys.time(), '.csv', sep='') },
    content = function(file) {
      write.csv(plotdata(), file)
    }
  )

output$downloadhist <- downloadHandler(
    filename = function() { paste(input$chosenfile, input$chosenSituation,Sys.time(), '.png', sep='') },
    content = function(file) {
        png(file,width     = 700,
  height    = 400,
  units     = "px",)
    print(his())
    dev.off()
  }
  )  


})