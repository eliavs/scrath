
library(scales)
library(htmlwidgets)
library(lubridate) #to play with dates
library(ggplot2)
library(plyr) #combining the data
library(ggmap)
library(leaflet)
library(reshape2)
library(openxlsx)
library(zoo)
#library(xlsx)
#library(WriteXLS)
Sys.setlocale(category = "LC_ALL", locale = "hebrew")
options(shiny.maxRequestSize=30*1024^2) 
options(java.parameters = "- Xmx1024m")
shinyServer(function(input, output,session) {
## upload file
	
#
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
	print(input$fileChoice$name)
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
    system(paste("sed 's/\t/,/g' ", foon," | tail -n +2 > file",file, ".txt",sep=""))
	}
   else {
    shell(paste("sed 's/\t/,/g' ", foon," | tail -n +2 > file",file,".txt", sep=""))  
  }
  
  file_1 <- read.csv(paste("file",file,".txt",sep = ""), header=T, stringsAsFactors =F)
  
  
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
  downloadButton('downloadData', 'לשמירת נתונים')
  
})
########----------
#situation dropdown
#
##----------
output$SlicingData <- renderUI({
  data1<-userdata()[[1]]
  situation <- data1[[32]]
  Mode<-max(names(table(situation)))
  selectInput('chosenSituation','', choices=c(unique(situation),"הכל"),selected = "הכל",multiple = T)
})
########----------
#reason dropdown
#
##----------
output$SlicingReason <- renderUI({
  data1<-userdata()[[1]]
  Reason <- data1[33]
  selectInput('chosenReason','', choices=c(unique(Reason),"הכל"),selected ="הכל",multiple = T)
})
########----------
#major dropdown
#
##----------
output$SlicingMajor <- renderUI({
  
  data1<-userdata()[[1]]
  Major <- data1[26]
  choices1<-c(unique(Major),'הכל')
  selectInput('chosenMajor','', choices=choices1,selected = "הכל",multiple = T)
  
})

######--------
#Chug  dropdown
####
#------
output$SlicingHug <- renderUI({
  if (is.null(input$chosenMajor)| is.null(input$chosenSituation))
  {
  choices1<-""
  }
  else{
  data1<-plotdata()[[1]]
  Hug <- data1[30]
  choices1<-c(unique(Hug),'הכל')}
  validate(
  need(!is.null(input$chosenMajor),"אנא בחר פקולטה"),
  need(!is.null(input$chosenSituation),"אנא בחר מצב החלטה")
  )
  selectInput('chosenHug','', choices=choices1,selected = "הכל",multiple = T)
})

########----------
#places dropdown
##
##----------
output$SlicingLocation <- renderUI({
  data1<-userdata()[[1]]
  location <- data1$name
  selectInput('chosenLocation','', choices=c(unique(location),"הכל"),multiple = T,selected="הכל")  
})

########----------
#date range
#
##----------

output$TimeSlicer <-renderUI({
  data1 <- userdata()[[1]]
  date_dec<-data1$new_date
  min = min(range(date_dec,na.rm=TRUE))
  max = max(range(date_dec,na.rm=TRUE))
  dateRangeInput("Time",'',start = min, end = max, min = min, max = max, format = "dd-mm", startview = "week", weekstart = 0, language = "he", separator = " עד ", width = '500px')
}) 
output$TimeSlicer1 <-renderUI({
	selectInput('MonthF','', choices=c(1,2,3,4,5,6,7,8,9,10,11,12),multiple = F,selected=1, width ='50px')  
	})
output$day <-renderUI({
	selectInput('DayF','', choices=c(seq(from=1, to =31, by=1)),multiple = F,selected=1,width ='50px')  
	})
output$TimeSlicer2 <-renderUI({
	selectInput('MonthT','', choices=c(1,2,3,4,5,6,7,8,9,10,11,12),multiple = F,selected=12, width ='50px')  
	})
output$day1 <-renderUI({
	selectInput('DayT','', choices=c(seq(from=1, to =31, by=1)),multiple = F,selected=31,width ='50px')  
	})		
##########------
###build output for dataradio madadim
############--------
output$dataradio<-renderUI({radioButtons("madad", "מדדים",
             c( "פסיכומטרי" = "psychometric",
               "בגרות" = "bagrut",
               "אנגלית" = "english",
			   "עברית" = "hebrew"), selected = "psychometric" )
			   })


output$downloadhistbutton <- renderUI({
  downloadButton('downloadhist', 'לשמירת היסטוגרמה')
})  

output$downloadrunbutton <- renderUI({
  downloadButton('downloadrun', ' לשמירת ממוצע רץ')
})  
##----------------
##cut data
#------------------------
plotdata<-reactive({
	if (input$andor=="and")
	print("it is and")
  datas<-list()
  data1<-userdata() 
  for (datafile in data1){
  Situation <- c(input$chosenSituation)
  Reason <- c(input$chosenReason)
  Major <- c(input$chosenMajor)
  Location <- c(input$chosenLocation)
  if (Situation!="הכל"){
  datafile<-datafile[datafile[[32]]==c(Situation),]
  }
  if (Reason!="הכל"){
    datafile<-datafile[datafile[[33]]==c(Reason),]
  }
  if (Major!="הכל"){
    datafile<-datafile[datafile[[26]]==c(Major),]
  }
  if (Location!="הכל"){
    datafile<-datafile[datafile[['name']]==c(Location),]
  }
  
  monthfrom <-input$MonthF
  dayfrom <-input$DayF
  monthto <-input$MonthT
  dayto <-input$DayT
  
  #mini <- min(c(input$Time))
  mini2 <-c(paste(monthfrom,dayfrom))
  maxi2 <-c(paste(monthto,dayto))
  mini2<- strptime(mini2, format ="%m %d")
  maxi <- max(c(input$Time))
  maxi2 <-strptime(maxi2, format ="%m %d")
  print(ymd(mini2))
  int1 <- new_interval(ymd(mini2),ymd(maxi2))
  datafile <- datafile[ymd(datafile$new_date) %within% int1,]
  datas[[length(datas)+1]] <- datafile
  }
  return(datas)
})

##----------------
##build a time plot
#------------------------
plotdate<-reactive({
  if (is.null(input$chosenSituation))
	return(" ")
  data1<-plotdata()
  i = 1
  a<-ggplot()
  for (foo in data1){
  foo$mikdama[foo[[21]]=="שילם מקדמה"]<-as.character(foo[[223]])
  if (length(foo$mikdama)<=1)
  return("")
  foo$mikdama<-as.POSIXct(foo$mikdama, format="%Y-%m-%d")
  df <- data.frame(x = c(foo$new_date,foo$harshama_date),y =foo$mikdama,L =foo[[32]],g = gl(2, length(foo[,1]),labels =c(paste("תאריך החלטה " , input$chosenfile[i]), paste("תאריך הרשמה ",input$chosenfile[i]),paste("שילמו מקדמה",input$chosenfile[i])))) 
  if (input$andor=="or"){
	for (j in unique(foo[[32]])) {
		data <- subset(foo,foo[[32]] == j)
		df <- data.frame(x = c(data$new_date,data$harshama_date),g = gl(2, length(data[,1]),labels =c(paste("תאריך החלטה ",j,input$chosenfile[i] ), paste("תאריך הרשמה ",j,input$chosenfile[i]))))
		df$x <- as.Date(df$x)
		if(length(df$x) <=1)
		return("")
		a <- a +stat_ecdf(data = df , aes(x,color = g)) +labs(title = "תאריכי הרשמה", x= " תאריך", y= "כמות", color = "מקרא")  +  scale_x_date(labels = date_format("%d/%m"),breaks = date_breaks("5 week") )
		
}
}
 else{
	df$y<-as.Date(df$y)
	df$x<-as.Date(df$x)
	if(length(df$x)<=1)
	return("")
	a <-a + stat_ecdf(data = df, aes(x, color = g))+ stat_ecdf(data = df, aes(y , color = "שילמו מקדמה")) +labs(title = "תאריכי הרשמה", x= " תאריך", y= "כמות", color = "מקרא")  +  scale_x_date(labels = date_format("%d/%m"),breaks = date_breaks("5 week") )

  }
  i= i+1
  }
  return(a)
})
###set range for histogram
getnums<-reactive({
data1<-plotdata()
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
 for (foo in data1){
 nums = c(min(foo[,j]), max(foo[,j]))
 }
 return(nums)
})
###slider
output$slider1 <- renderUI({

a<-getnums()[1]
b<-getnums()[2]
print(a)

sliderInput("slider1", "slider:",
                min = a, max = b, value = c(a,b), step = 1)
				})

##-----------------
### build a histogram
#####----------
his<-reactive({
print (sessionInfo())
if (is.null(input$chosenSituation))
	return(" ")
 data1<-plotdata()
 #print(input$madad)
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
	if (length(foo[,j])<1){
	print(foo[,j])
	print("short")
	return("")
	}
  a<- qplot(as.numeric(foo[,j]),fill="#43140190", geom="histogram", xlab= "ציון" ,main = title) + scale_x_continuous(breaks=pretty_breaks(n=10), limits=c(input$slider1[1],input$slider1[2])) + theme(legend.position="none")
  }
  }
  return(a)
})

##-----------------
##build a running average
##---------------
runing<-reactive({
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
if (is.null(input$chosenSituation))
	return(" ")
 data1<-plotdata()
 #print(input$madad)
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
  if (input$analtype == "mean")
  {
  #bar<- tapply(foo[,j][foo[,j]!=0], foo$dec_date[foo[,j]!=0], mean)
  bar<- tapply(foo[,j][foo[,j]!=0], as.character(foo$new_date[foo[,j]!=0]), mean)
  validate(
  need(!is.null(bar), "אין מספיק נתונים")
  )
  erech = "ממוצע"
  }
  else if (input$analtype=="max"){
 erech = "מקסימום"
  bar<- tapply(foo[,j][foo[,j]!=0], as.character(foo$new_date[foo[,j]!=0]), max)
  validate(
  need(!is.null(bar), "אין מספיק נתונים")
  )
  }
   else if (input$analtype=="min"){
  bar<- tapply(foo[,j][foo[,j]!=0], as.character(foo$new_date[foo[,j]!=0]), min)
  validate(
  need(!is.null(bar), "אין מספיק נתונים")
  )
  erech= "מינימום"
  }
    else if (input$analtype=="deviation"){
  #bar <-rollapply(foo[,j][foo[,j]!=0], width = 3, FUN = sd, na.pad = TRUE)
  bar<- tapply(foo[,j][foo[,j]!=0], as.character(foo$new_date[foo[,j]!=0]), sd)
  erech = "סטיית תקן"
  }
  else if (input$analtype=="median"){
  bar<- tapply(foo[,j][foo[,j]!=0], as.character(foo$new_date[foo[,j]!=0]), median)
  erech = "חציון"
  }
    else if (input$analtype=="mode"){
  bar<- tapply(foo[,j][foo[,j]!=0], as.character(foo$new_date[foo[,j]!=0]), Mode)
  erech = "שכיח"
  }
  bar<-bar[!is.na(bar)]
  validate(need(!is.null(bar), "we got a problam")
  )

 b<-data.frame(names(bar), bar)
 if (length(b)<=1){
 return("")}
 validate(need(b!=0, "we got a problam")
  )
 names(b)<-c("nam","bar")

 b$nam<-as.Date(b$nam)
 a<-ggplot(b,aes(x = nam, y =cumsum(bar)/seq_along(bar), group = 1)) + geom_line(col='#4061F0') +labs(title = title) + labs(x="תאריך", y= erech)+  scale_x_date(labels = date_format("%d/%m"),breaks = date_breaks("5 week"))#+ scale_x_discrete(breaks = 20)#scale_x_continuous(foo$new_date)#[c(T, rep(F, 9))])#+ theme(text = element_text(size=5),axis.text.x = element_text(angle=90, vjust=1)) 
 print(paste(bar," ," ,cumsum(bar)))
 
 #a<-ggplot(b,aes(x = nam, y =bar, group = 1)) + geom_line(col='#4061F0') +labs(title = title) + labs(x="תאריך", y= erech)+  scale_x_date(labels = date_format("%d/%m"),breaks = date_breaks("5 week"))#+ scale_x_discrete(breaks = 20)#scale_x_continuous(foo$new_date)#[c(T, rep(F, 9))])#+ theme(text = element_text(size=5),axis.text.x = element_text(angle=90, vjust=1)) 
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
  #print(names(a))
  map <- leaflet() %>% addTiles()
  map1 <- map %>% addCircleMarkers(data = a, lat = ~y, lng = ~x, color = ~pal1(dat) ,radius = 3, popup = a[[1]])%>% addLegend(position = "bottomleft",pal = pal1, values= a$dat	,title = name1,opacity = 1)
  
  return(map1)
})
###plot hist
output$histo<-renderPlot({
validate(
need(his()!="", "אין מספיק נתונים")
)
if(is.null(his()))
print("is null")

his()

})
###plot running
output$runo<-renderPlot({
validate(
need(runing()!="", "אין מספיק נתונים")
)
runing()
})
##plot_leaflet map
output$myMap<-renderLeaflet({
  map()
})

##plot the data
output$viewData<-renderPlot({
  validate(
  need(plotdate()!="", "אין מספיק נתונים")
  )
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
    filename = function() { paste(Sys.time(), '.xlsx', sep='') },
    content = function(file){
	datfram<-plotdata()[[1]]
	print(names(datfram))
	datfram <-datfram[,c(1:220), drop=F]
	#print(datfram)
	#datfram <- replace(plotdata()[[1]] , is.na(plotdata()[[1]] ),"")
    wb <- createWorkbook()
	addWorksheet(wb = wb, sheetName = "Sheet 1", gridLines = FALSE)
	is_character_col <- which(sapply(datfram, class) %in% "character")
	for(i in is_character_col){
	datfram[[i]] <- gsub("\b", "", datfram[[i]], fixed = TRUE)
	}
  
	writeDataTable(wb = wb, sheet = 1, x = datfram )
	#write.xlsx(datfram,"anothertry.xlsx")
	#WriteXLS(datfram, ExcelFileName = "andanother.xls", SheetNames = NULL, perl = "perl", verbose = T, Encoding = "UTF-8", col.names = TRUE)
	saveWorkbook(wb, file, overwrite = TRUE)
	write.csv(datfram, "compare.csv")
    }
  )
name<-reactive({
a<-iconv(paste(input$chosenfile,input$chosenSituation, '.png', sep="_"),to ="windows-1255")
print(Encoding(a))
print(a)
return(paste(input$chosenfile,input$chosenSituation, '.png', sep="_"))
})
output$downloadhist <-
downloadHandler(	
    filename = function() { paste(input$chosenfile,input$chosenSituation, '.png', sep="_")},
    content = function(file) {
        png(file,width     = 700,
  height    = 400,
  units     = "px",)
    print(his())
    dev.off()
  }
  ) 
output$downloadrun <- downloadHandler(
    filename = function() { paste(input$chosenfile, input$chosenSituation,Sys.time(), '.png', sep='') },
    content = function(file) {
        png(file,width     = 700,
  height    = 400,
  units     = "px",)
    print(runing())
    dev.off()
  }
  )
  
    output$uploadfile<-renderUI({
	fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv'))
							})	 
 output$contents <- renderDataTable({
    inFile <- input$file1
    print(dir("data"))
	if (is.null(inFile))
      return(NULL)
	print(inFile$datapath)
	#file.copy(from = inFile$datapath, to = paste("data/",inFile$name,sep=""), overwrite = TRUE)
	if(.Platform$OS.type == "unix") {
    system(paste("iconv -f WINDOWS-1255 -t UTF-8 ", inFile$datapath,"> data/",inFile$name,sep="" ))
	}
	 else {
	 file.copy(from = inFile$datapath, to = paste("data/",inFile$name,sep=""))
	 #shell(paste("iconv -f ISO-8859-9 -t UTF-8 ", inFile$datapath,"> data/",inFile$name,sep="" ))
	 }
	all_content = readLines(inFile$datapath)
	skip_second = all_content[-1]
    read.delim(textConnection(skip_second), header = TRUE,sep="\t", stringsAsFactors = FALSE,fileEncoding = "UTF-8")
  })
  
output$files2del <- renderUI({
  lst<-dir("data")
  selectInput('chosen4del',"", choices=c(lst),multiple = T)
}) 
output$debuton<-renderUI({
actionButton("delit", "מחק")
})

a<-reactive({

return(input$chosen4del)
})

output$nText<-renderText({
if (is.null(a()))
 return(" ") 
 else{
 file.remove(paste("data/",input$chosen4del,sep=""))
 paste(a(), "נמחק")
 
 }
})
##AND - OR
output$allpart<-renderUI({radioButtons("andor", "",
             c( "AND" = "and",
               "OR" = "bagrut"
               ), selected = "and" )
			   })
  

output$analasystype<-renderUI({radioButtons("analtype", "",
             c( "ממוצע" = "mean",
               "מקסימום" = "max",
			   "מינימום" = "min",
			   "שכיח" = "mode",
			   "סטיית תקן" = "deviation",
			   "חציון" = "median"
               ), selected = "mean" )
			   
			   })
			   })
  ###############################################################################################################################
