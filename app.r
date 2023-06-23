library(RCurl)
library(jpeg)
library(tidyverse)
library(shiny)
library(rvest)
library(httr)
library(mailR)
library(paws)

Sys.setenv(TZ = "America/Denver")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Snowpack Comparison Tool"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("camId", "Webcam Select", scan("keys.txt", what="", sep="\n")),
      selectInput("plotId", "Select NOAA Plot", c("SBDU1 (Snowbird)","BRIU1 (Brighton)","CAMU1 (Cascade Mountain)","TABN5 (Taos)", "WSCS2 (Wolf Creek)", "VLMC2 (Vail)", "CPMC2 (Copper Mtn)")),
      checkboxInput("snowpackCheckbox", "Show snowpack averages?", value = FALSE),
      checkboxInput("showLineChart", "Show snowpack chart?", value = FALSE),
      actionButton("timelapseAction", "Show timelapse"),
      sliderInput("lapseRate", "Lapse Rate (secs)", 0.1, 2, step = 0.1, value = 1),      
      htmlOutput(outputId = "AST"),
      htmlOutput(outputId = "formHeader"),
      textInput("reqLocation","Resort/Area Name:"),
      textInput("reqCamUrl","Webcam URL:"),
      actionButton("submitForm", "Submit Request"),
      htmlOutput(outputId = "formSuccess")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      htmlOutput(outputId = "open_resorts"),
      htmlOutput(outputId = "timelapseStamp"),
      htmlOutput(outputId = "timelapse"),
      htmlOutput(outputId = "timeStampPast"),
      htmlOutput(outputId = "renderCamPast"),
      htmlOutput(outputId = "timeStampNow"),
      htmlOutput(outputId = "renderCamNow"),
      htmlOutput(outputId = "renderSnowpack"),
      htmlOutput(outputId = "renderLineChart")
    )
  )
)

server <- function(input, output) {
  
 iterate <- reactiveValues(iteration = 1)
 s3 <- s3()
 
 ##statics
 baseUrl <- "https://resort-cams.s3.us-west-2.amazonaws.com/"
 dateString <- substr(as.character(Sys.Date()),6,nchar(as.character(Sys.Date())))

 currFolderS3 <- reactive({
   s3$list_objects(Bucket = 'resort-cams',Prefix = paste('bin/',input$camId,sep=""))$Contents
   
   })
 
 snowpackS3 <- s3$list_objects(Bucket = 'resort-cams',Prefix = 'bin/Snowpack',)$Contents
 
 observeEvent(input$timelapseAction,{
   
   timer <- reactiveTimer(input$lapseRate * 1000)
   
   observeEvent(timer(),{
     
     if(iterate$iteration <= length(currFolderS3())) {
     
     iterate$iteration <- iterate$iteration + 1
    
     }
     
     else { 
       
      iterate$iteration <- 1 
       
       }
     
   })
   
 })
 
  
 fileName <-  reactive({
   
  currFolderS3()[[iterate$iteration]][1][[1]][1]#file.path(paste("./",input$camId, "//", list.files(input$camId)[iterate$iteration], sep = ""))

  })
 
 open_list = list()
 
 html <- read_html("https://www.onthesnow.com/united-states/open-resorts")
 
 items <- html %>% 
   html_nodes("tbody") %>% 
   html_nodes("tr")
 
 for (i in 1:length(items)){
   temp <- items[i] %>% 
     html_nodes("a") %>% 
     html_nodes("span") %>% 
     html_text()
   print(temp)
   open_list[i] <- temp %>% unlist()
   
 }
 
 
output$open_resorts <- renderText({
  paste("<h3> Open Resorts:</h3>\n","<p style=margin-bottom:100px;>",paste(open_list, collapse=", "),"</p>")
  })
 
output$timelapse <- reactive({

  if (iterate$iteration > 1){
    paste("<img style=' margin-bottom: 50px; height:250px' src=",baseUrl,gsub(" ","+",fileName()),">",sep="")

  } 
  else { "" 
    }
}
)
  
output$timelapseStamp <- reactive({
  if (iterate$iteration > 1){
  paste("<h3><strong>TIMELAPSE</strong></h3></br>",str_split(fileName(), " ", simplify = TRUE)[2],sep="")
  }
  else {
    ""
  }
  })
  
output$renderCamPast <- renderText({
    
   paste("<img style=' margin-bottom: 50px; height:250px' src=",baseUrl,gsub(" ","+",currFolderS3()[[1]][1][[1]][1]),">",sep=""
)})


output$timeStampPast <- reactive(paste("<h4><strong>ONE YEAR AGO*</strong></h4></br>",str_split(currFolderS3()[[1]][1][[1]][1], " ", simplify = TRUE)[2],sep=""))

output$debug <- renderText({class(currFolderS3)})

output$renderCamNow <- renderText({
  
  foo <-paste("<img style=' margin-bottom: 50px; height:250px' src=",baseUrl,gsub(" ","+",currFolderS3()[[length(currFolderS3())]][1][[1]][1]),">",sep=""
  )
})

output$timeStampNow <- reactive(paste("<h4><strong>TODAY</strong></h4></br>",str_split(currFolderS3()[[length(currFolderS3())]][1][[1]][1], " ", simplify = TRUE)[2],sep=""))

output$renderSnowpack <- renderText({
  
  if(input$snowpackCheckbox == TRUE) {
  
    paste("<img style=' margin-bottom: 50px; height:500px' src=",baseUrl,gsub(" ","+",snowpackS3[[length(snowpackS3)]][1][[1]][1]),">",sep="")
  
  } else {
    ""
}
  
  })
    


output$AST <- renderText({
  
"<h1 style = 'margin-top: 50px;  font-size:24px;' >Sponsored by:</h1> <img style=' margin-bottom: 50px; height:200px' src= https://images.squarespace-cdn.com/content/v1/61b543560957ef4511ea017e/2f1c13d6-8aef-460a-9367-8213cc46d20b/Sonoran.png?format=1500w>"
} )


output$renderLineChart <- renderText({
  if (input$showLineChart == TRUE){
    plot <- strsplit(input$plotId,split=" ")[[1]][1]
    year <- "2023"
    hourAdjusted <- (strtoi(format(Sys.time(), "%H"), base=10L)+6)%%24 #hour of day +6 for GMT
    plusOneDay = 0
    
    if (strtoi(format(Sys.time(), "%M")) < 10){
      
      hourAdjusted = hourAdjusted + 1
      
    }
    
    if (0 <= hourAdjusted & hourAdjusted <= 6){#add a day to curr date if GMT has passed midnight
      plusOneDay = 1
    }
    
    if (hourAdjusted < 10){ #add 0 at beginning of str if needed
      hourAdjusted <- paste("0", hourAdjusted, sep = "")
    }


    foo <- GET(paste("https://www.cbrfc.noaa.gov/station/sweplot/sweplot2.cgi?yrList=median&yrList=", year, "&stationList=",plot,"&tavg=s&hsim=&index=&dbsvr=&indextitle=Untitled",sep=""))
    print(foo)
    url <- paste("https://www.cbrfc.noaa.gov/station/sweplot/png/",tolower(plot),".",Sys.Date()+plusOneDay,"-",hourAdjusted,"--median.",year,".0.s.0.0.0.0.0.0.0.0.png",sep="")#6
    print(url)
    c('<img src="',url,'">')

  } else {
    ""
  }
    

  
})

output$formHeader <- renderText({"<h4> Submit Webcam Request </h4>"})

observeEvent(input$submitForm, {
  
  sender <- "tannersundwall@gmail.com"
  recipients <- c("tannersundwall@gmail.com")
  send.mail(from = sender,
            to = recipients,
            subject = "Shiny Apps Response",
            body = paste("Location: ", input$reqLocation,"\nURL: ", input$reqCamUrl,sep=""),
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "tannersundwall@gmail.com",            
                        passwd = "jagfykwizfthhggj", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  
  output$formSuccess <- renderText({"Thanks!"})
  
  })

}

shinyApp(ui = ui, server = server)