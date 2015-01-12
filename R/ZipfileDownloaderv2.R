# Team: ZeaPol   
# Team Members: Roeland de Koning / Barbara Sienkiewicz    
# Date: 08/01/2015       
# Exercise 5

#function to download and extract specified zip files to the data folder of R projects
#may need adjustment

ZipFileDownloader<- function(url,filedestination,filename) {
  
  deposit<- paste(filedestination,filename)
  file<- strsplit(filename,".zip")
  filelocation<-paste("data/",file)
  download.file(url,deposit)
  
  unzip(deposit,files = NULL, list = FALSE, overwrite = TRUE,junkpaths = FALSE, exdir = filelocation, unzip = "internal",
        setTimes = FALSE)

}
