#File handling



#exists and download
#checks if a files exists
#downloads if it does not
#also unzips a temp file

if(!file.exists("household_power_consumption.txt")){
    temp<-tempfile()
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",                
                  destfile=temp)
    unzip(temp)
    unlink(temp)
    
    dateDownloaded<-date()
    dateDownloaded
}



#write as csv
write.csv(data,"activity.csv", row.names=FALSE)
