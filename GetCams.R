library(RCurl)
library(jpeg)


setwd("bin") 
#LOCALLY - USE 'curl' instead of 'libcurl' AND remove previous line
download_cam <- function(name, url, format = ".jpg") {
  
  download.file(url, destfile= paste(name, "//", name, " ", Sys.Date(), " ", format(Sys.time(),'%H'), format,  sep = ""),method='libcurl')
  
  print(paste(name, "downloaded successfully"))
  
}

download_cam_dynamic_brute <- function(folderName, camId,name = "00") {
  
  bsFound = FALSE
  currMin = format(Sys.time(), "%M")
  currHour = format(Sys.time(), "%H")
  bsDestFile = paste(folderName, "//", folderName, " ", Sys.Date(), " ", format(Sys.time(),'%H'), ".jpg",  sep = "")
  counter = 0
  while (bsFound == FALSE & counter < 20){
    str = paste("https://storage.googleapis.com/prism-cam-000",camId,"/",format(Sys.time(), "%Y"),"/",format(Sys.Date(), "%m"),"/",format(Sys.Date(), "%d"),"/",currHour,"-",currMin,"/720.jpg",sep='')
    download.file(str,bsDestFile,method='curl')
    

    if (file.info(paste(folderName,"//",list.files(folderName)[length(list.files(folderName))],sep=''))$size > 1000){
      
      bsFound = TRUE
    }
    
    if (currMin == "00"){
      currMin = "59"
      currHour = as.character(as.numeric(currHour) - 1)
    }
    
    else {
      currMin = as.character(as.numeric(currMin) - 1)
    }
    
    if (as.numeric(currMin) < 10){
      currMin = paste("0", currMin, sep ="")
    }
    
    if (as.numeric(currHour) < 10){
      currHour = paste("0", currHour, sep ="")
    }
    counter = counter + 1
  }
  file.rename(paste(folderName,"//",list.files(folderName)[length(list.files(folderName))],sep=''),paste(folderName,"//",name,".jpeg",sep=""))

  if (file.info(paste(folderName,"//",name,".jpeg",sep=""))$size < 1000){
    file.remove(paste(folderName,"//",name,".jpeg",sep=""))
  }
}

download_cam_dynamic <- function(folderName, camId) {
  
  bsFound = FALSE
  currMin = format(Sys.time(), "%M")
  currHour = format(Sys.time(), "%H")
  bsDestFile = paste(folderName, "//", folderName, " ", Sys.Date(), " ", format(Sys.time(),'%H'), ".jpg",  sep = "")
  counter = 0
  while (bsFound == FALSE){
    
    str = paste("https://storage.googleapis.com/prism-cam-000",camId,"/",format(Sys.time(), "%Y"),"/",format(Sys.Date(), "%m"),"/",format(Sys.Date(), "%d"),"/",currHour,"-",currMin,"/720.jpg",sep='')
    download.file(str,bsDestFile,method='curl')
    print(currMin)
    print(str)
    
    if (file.info(paste(folderName,"//",list.files(folderName)[length(list.files(folderName))],sep=''))$size > 1000){
      bsFound = TRUE
    }
    
    if (as.numeric(currMin) == 0){
      currMin = "59"
      currHour = as.character(as.numeric(currHour) - 1)
    }
    
    else {
      currMin = as.character(as.numeric(currMin) - 1)
    }
    
    if (as.numeric(currMin) < 10){
      currMin = paste("0", as.numeric(currMin), sep ="")
    }
    
    if (as.numeric(currHour) < 10){
      currHour = paste("0", as.numeric(currHour), sep ="")
    }

  }
  
}

download_cam("SnowbirdLC","https://backend.roundshot.com/cams/44cfff4ff2a218a1178dbb105d95846a/medium")
download_cam("SnowbirdMineral","https://backend.roundshot.com/cams/48fc223c0ed88474ecc2f884bf39de63/medium")
download_cam("BachelorPM","https://www.mtbachelor.com/api/v1/cams/mtn/1")
download_cam("BachelorSummit","https://www.mtbachelor.com/api/v1/cams/mtn/8")
download_cam("TimberlinePalmer","https://www.timberlinelodge.com/snowcameras//palmerbottom.jpg")
download_cam("Solitude","https://webcams.solitudemountain.com/ph.jpg") #url updated 6/6/23
download_cam("MammothSummit", "https://media.mammothresorts.com/mmsa/mammoth/cams/Top_Of_Sierra_1_1280x720.jpg")
download_cam("TetonPassGloryBowl", "https://www.wyoroad.info/Highway/webcameras/WYO22TetonPass/WYO22TetonPassGlory2.jpg")
download_cam("TargheeDC", "https://copyrighted.seejh.com/gtrplaza/gtrplaza.jpg") #url updated 6/6/23
download_cam("TargheeValley", "https://thm.seejh.com/thumbs/66.jpg")
download_cam("JacksonBase", "https://thm.seejh.com/thumbs/23.jpg")
download_cam("AltaBaldy", "http://app.prismcam.com/public/helpers/realtime_preview.php?c=88&s=720")
download_cam("Snowpack", "https://www.wcc.nrcs.usda.gov/ftpref/gis/images/west_swepctnormal_update.png", format = ".png")
download_cam("WinterPark", "http://cams.winterparkresort.com/snoasis-cam.jpg")
download_cam("CrystalBase", "https://cams.mtnfeed.com/fullsize/CRYM4_l_1280x720.jpg")
download_cam("TargheeSummit", "https://copyrighted.seejh.com/gtrsummit/gtrsummit.jpg") #added 6/6/23
download_cam("AltaBase", "https://images-webcams.windy.com/57/1353256057/current/full/1353256057.jpg")#added 6/6/23
download_cam("TimpWest", "https://marvin.byu.edu/Weather/webcam800.jpg")#added 6/6/23
download_cam("JacksonCody", "https://cams.jacksonhole.com/webcam/thumbnails/codybowl.jpg")#added 6/6/23
#download_cam("WhistlerBlackcomb", "https://cache.snow.com/Mtncams/3glaciercreek.jpg")#added 6/6/23


#big sky
print('\nBIG SKY\n')
download_cam_dynamic('BigSkyLonePeak',"92")
print('\nWHISTLER\n')
download_cam_dynamic('Whistler7th',"43")
print('\nSUNDANCE\n')
download_cam_dynamic('TimpSundance',"75")
print('\nBEAV\n')
download_cam_dynamic('BeaverCreek',"93")
print('\nSTOWE\n')
download_cam_dynamic('Stowe',"76")
print('\nALBION\n')
download_cam_dynamic('AltaAlbion',"65")
print('\nCRESTED\n')
download_cam_dynamic('CrestedButte',"38")

#telluride

#baker

