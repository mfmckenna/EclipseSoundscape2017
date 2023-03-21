#Extract audio clips from directory of audio files
# written for eclipse dataset 20 Oct 2017 mfm

#EXTRACTS the following audio clips of interest (for each site) for days: 18Apr2018 - 25Apr2018, if available
#1) TotalEclipse_Site_YYMMSS_HHMMSS.wav.... WORKING, but blocked because too big of files!
#2) Eclipse_SITE_YYMMSS_HHMMSS.wav.... WORKING 
#3) Dawn_SITE_YYMMSS_HHMMSS.wav...WORKING
#4) Dusk_SITE_YYMMSS_HHMMSS.wav... NOT WORKING BECAUSE NEXT DAY ISSUES (ugh!)
#5) BeforeEclipse_20180821_HHMMSS.wav
#6) AfterEclipse_20180821_HHMMSS.wav

rm(list=ls())

#libraries
library(chron)
library(lubridate)
library(data.table) 
library(tuneR)

#PARAMS
FS = 44100 #sampling rate in Hz

#----------------------------------------------------------
#READ in ECLIPSE TIMES
#----------------------------------------------------------
eTimes = read.csv("F:\\RESEARCH\\NSNSD_EclipseSoundScape\\ANALYSIS\\EclipseTimes.csv")
eTimes$Sites = paste(eTimes$Park, eTimes$SiteName, sep= "")

#----------------------------------------------------------
#READ IN SUN TIMES
#----------------------------------------------------------
setwd ("F:\\RESEARCH\\NSNSD_EclipseSoundScape\\ANALYSIS")
SunTimesFiles <- list.files(path = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\ANALYSIS", pattern="sunTimes.csv$")
Sites = sapply(strsplit( as.character(SunTimesFiles) , "_"), head,1)
sTimes = NULL
for(ff in 1:length(Sites)) {
  tst = cbind(Sites[ff], read.csv(SunTimesFiles[ff]))
  sTimes = rbind(sTimes,tst)
}

#----------------------------------------------------------
#READ in AUDIO FILES
#----------------------------------------------------------
setwd ("F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO")
outDir = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO_eclipse"
audioFiles <- dir(pattern=".wav$",recursive = T)
audioFiles <- list.files(pattern=".wav$",recursive = T)

#----------------------------------------------------------
#FIND audio files to a given site
#----------------------------------------------------------
for ( tt in 20 : dim(eTimes)[1]) { # tt = 7 #given site
  
  fname = as.character(eTimes$Sites[tt]) #site name 
 
  #Audio files for site...
  mfilesTmp = list.files(pattern = paste(fname,".*wav$",sep=''),recursive = T)
  udays = unique( sapply(strsplit( as.character(basename(mfilesTmp)) , "_"), "[" ,2) )
  
  #Start times of audio files for a given site...
  dtemp1= ( sapply(strsplit( as.character(basename(mfilesTmp)) , "[.]"), "[" ,1) )
  dtemp2= ( sapply(strsplit( as.character((dtemp1)) , "_"), "[" ,2)  ) #date
  dtemp3= as.character( sapply(strsplit( as.character(dtemp1) , "_"), "[" ,3) ) #times
  dtemp4 = paste(dtemp2, as.character(dtemp3), sep=" ") #combine
  StartTimesFilesALL = ymd_hms(dtemp4)
  rm (dtemp1, dtemp2, dtemp3, dtemp4)
  
  #Convert sun times for site...
  sunTmp = as.data.frame( sTimes[sTimes$`Sites[ff]` == fname, ] )
  sunTmp$date = as.Date(sunTmp$date )
  sunTmp$solarNoon = ymd_hms(sunTmp$solarNoon)
  sunTmp$nadir= ymd_hms(sunTmp$nadir)
  sunTmp$sunrise= ymd_hms(sunTmp$sunrise)
  sunTmp$sunset= ymd_hms(sunTmp$sunset)
  sunTmp$sunriseEnd= ymd_hms(sunTmp$sunriseEnd)
  sunTmp$sunsetStart= ymd_hms(sunTmp$sunsetStart)
  sunTmp$dawn= ymd_hms(sunTmp$dawn)
  sunTmp$dusk= ymd_hms(sunTmp$dusk) 
  
  #Convert eclipse times for site... all on 2017-08-21
  Ebegin =  ymd_hms( paste("2017-08-21 ", eTimes$PartialEclipseBegins[tt]) )
  Eend =    ymd_hms( paste("2017-08-21 ", eTimes$PartialEclipseEnds[tt]) )
  
  #How much time (in samples) to grab in the recordings- duration of eclipse?
  durE =  as.numeric(difftime(Eend,Ebegin,units='secs')) * FS # Eend - Ebegin

  #How much time (in samples) to grab in the recordings- duration of totality?
  if (eTimes$TotalEclipseBegins[tt] != "-") {
    TEbegin = ymd_hms( paste("2017-08-21 ", eTimes$TotalEclipseBegins[tt]) )
    TEend =   ymd_hms( paste("2017-08-21 ", eTimes$TotalEclipseEnd[tt]) )
    durT =  as.numeric(difftime(TEend,TEbegin,units='secs')) * FS 
  }
  
  #LOOP through days to grab the data of interest...
  for (uu in 1:length(udays)) # uu = 2 # given day of recording
  {
    #get files to get data from...
    dayAud = mfilesTmp[grep(udays[uu],mfilesTmp)] 
    
    #get files from the next day...
    if (uu < length(udays)) { dayAud2 = mfilesTmp[grep(udays[uu+1],mfilesTmp)] } 
    
    #day for this loop
    dt = as.Date(udays[uu], format="%Y%m%d") 
    cat("--------------------",sep="\n")
    cat("Processing site ", fname, "(", tt, " of ", length(Sites), ") on ", as.character(dt), "(", uu, " of ", length(udays), ")")
    cat("--------------------",sep="\n")
    
    #get eclipse times
    Ebegin1 = ymd_hms( paste(dt, eTimes$PartialEclipseBegins[tt]))
    Eend1   = ymd_hms( paste(dt, eTimes$PartialEclipseEnds[tt]))
    if (eTimes$TotalEclipseBegins[tt] != "-") {
      TEbegin1 = ymd_hms( paste(dt, eTimes$TotalEclipseBegins[tt]) )
      TEend1 =   ymd_hms( paste(dt, eTimes$TotalEclipseEnd[tt]) )
    }
    
    #Start times of audio files
    dtemp1= ( sapply(strsplit( as.character(basename(dayAud)) , "[.]"), "[" ,1) )
    dtemp2= ( sapply(strsplit( as.character((dtemp1)) , "_"), "[" ,2)  ) #date
    dtemp3= as.character( sapply(strsplit( as.character(dtemp1) , "_"), "[" ,3) ) #times
    dtemp4 = paste(dtemp2, as.character(dtemp3), sep=" ") #combine
    StartTimesFiles = ymd_hms(dtemp4)
    rm (dtemp1, dtemp2, dtemp3, dtemp4)
    
    if (exists("dayAud2")) {
      dtemp1= ( sapply(strsplit( as.character(basename(dayAud2)) , "[.]"), "[" ,1) )
      dtemp2= ( sapply(strsplit( as.character((dtemp1)) , "_"), "[" ,2)  ) #date
      dtemp3= as.character( sapply(strsplit( as.character(dtemp1) , "_"), "[" ,3) ) #times
      dtemp4 = paste(dtemp2, as.character(dtemp3), sep=" ") #combine
      StartTimesFiles2 = ymd_hms(dtemp4)
      rm (dtemp1, dtemp2, dtemp3, dtemp4)
    }
    
    # !!!!!!!!!!!! FIND THE CLIPS   !!!!!!!!!!!!!!!!
    
    #FIND DATA DURING: total eclipse, only if in path of totatality!
    #-----------------------------------------
    if (eTimes$TotalEclipseBegins[tt] != "-") {
      #which audio file do I open?
      indx = which( (TEbegin1-StartTimesFiles) >=0 ) # only keep the positive values
      
      if ( length(indx) != 0 ) {
      idx =  which(abs(TEbegin1-StartTimesFiles[indx]) == min(abs(TEbegin1 - StartTimesFiles[indx]))) #find the closest value  
 
      #What sample do I start on?
      srd = as.numeric( difftime(TEbegin1,StartTimesFiles[idx],units='secs')) * FS
      testSound = readWave(dayAud[idx], header=T) #   srd+durT > testSound$samples
      
      if (srd+durT > testSound$samples )  {
        
        #open next files...
        if (is.na(dayAud[idx+1]) ) {
          cat("Not enough data in this file: skip to next day",sep="\n")
          next  #skip to next day..
        }else {
          #open next file
          soundfile1 =  readWave(dayAud[idx] ,from = srd, to = testSound$samples, units="samples")
          mtograb = durT - (testSound$samples-srd ) 
          soundfile2 = readWave(dayAud[idx+1] ,from = 1, to = mtograb, units="samples")
          # check: length(soundfile) + length(soundfile2)
          soundfile = bind(soundfile1,soundfile2)
        }
      } else {
        soundfile = readWave(dayAud[idx] ,from = srd, to = srd+durT, units="samples")
      }
      
      #name and save out the file...
      fname1 = substr( as.character(basename(dayAud[idx])), 1, 20) 
      stname = sapply(strsplit(fname1,"_"), `[`, 1)
      tmstmp = sapply(strsplit(fname1,"_"), `[`, 2)
      etime = gsub(":","",as.character(eTimes$PartialEclipseBegins[tt]))
      #actualday = gsub("-","", substr(as.character(sclip), 1,10) )
      filename = paste( outDir, "\\",stname, "_TotalEclipse_", tmstmp, "_", etime, ".wav", sep="")
      
      writeWave(soundfile, filename, extensible=FALSE)
      
      rm(soundfile, srd, idx, filename, etime, fname1, TEbegin1, TEend1)
      }
    }
    
    #FIND DATA DAWN: same duration as total eclipse
    #-----------------------------------------
    if (eTimes$TotalEclipseBegins[tt] != "-") {
      
      #find the correct sunrise time... based on date we are processing...
      indx2 =  which( (dt - sunTmp$date) == 0 )
      #time to start on... grab the time before sunrise...
      sclip1 = sunTmp$sunrise[indx2] - ((durT/FS))
      #days are off in the sun calculation- so need to adjust by one day
      #sclip = sclip + (24*60*60) 
      #better method is to use the current day...
      sclip = ymd_hms( paste(dt, paste(hour(sclip1),minute(sclip1),second(sclip1),sep=":")) )
   
      #which audio file do I open to get the data?
      indx = which( (sclip-StartTimesFilesALL) >=0 ) # look at all files for the site... and only keep the positive values
      if ( length(indx) != 0 ) {
        
        idx2 = which(abs(sclip-StartTimesFilesALL[indx]) == min(abs(sclip - StartTimesFilesALL[indx])))
        #sample start on in audio file...
        srd = as.numeric( difftime(sclip, StartTimesFilesALL[idx2],units='secs')) * FS  
        testSound = readWave(mfilesTmp[idx2], header=T)
        
        if ( srd + durT > testSound$samples ) #not all samples in first file
        {
          #open next files if it exists...
          if (is.na(StartTimesFilesALL[idx2+1]) ) {
            cat("Not enough data in this file: skip to next day",sep="\n")
            next  #skip to next day
          }else {
            #open first file
            soundfile1 =  readWave(mfilesTmp[idx2] ,from = srd, to = testSound$samples, units="samples")
            #how much time to grab in next file- samples
            mtograb = durT - (testSound$samples - srd ) 
            #open next file
            soundfile2 = readWave(mfilesTmp[idx2+1] ,from = 1, to = mtograb, units="samples")
            # check: length(soundfile) + length(soundfile2)
            soundfile = bind(soundfile1,soundfile2)
          }
        } else {
          soundfile = readWave(mfilesTmp[idx2] ,from = srd, to = srd+durT, units="samples")
          # length(soundfile)/FS
        }
        #name and save out the file
        fname1 = substr( as.character(basename(mfilesTmp[idx2])), 1, 20)
        stname = sapply(strsplit(fname1,"_"), `[`, 1)
        tmstmp = sapply(strsplit(fname1,"_"), `[`, 2)
        etime = gsub(":","", substr(as.character(sunTmp$sunrise[indx2]), 12,21) )
        actualday = gsub("-","", substr(as.character(sclip), 1,10) )
        filename = paste( outDir, "\\",stname, "_SunRise_", tmstmp, "_", actualday, "_", etime, ".wav", sep="")
        
        writeWave(soundfile, filename, extensible=FALSE)
      }else { cat("No data for sunrise ",sep="\n")
        
      }
      rm( soundfile, srd, sclip, sclip1, indx2, indx,idx2,filename,fname1)
    }
    
    #FIND DATA DUSK: same duration as eclipse-
    #-----------------------------------------
    if (eTimes$TotalEclipseBegins[tt] != "-") {
      
      #find the correct sunrise time... based on date we are processing...
      indx2 =  which( (dt - sunTmp$date) == 0 )
      #time to start on... grab the time before sunrise...
      sclip1 = sunTmp$sunset[indx2] + ((durT/FS))
      #days are off in the sun calculation- so need to adjust
      if ( hour(sclip1) < 8 ) { 
        sclip = ymd_hms( paste(dt+1, paste(hour(sclip1),minute(sclip1),second(sclip1),sep=":")) )
      }else{ sclip = sclip1 }
      
      #which audio file do I open to get the data?
      indx = which( (sclip-StartTimesFilesALL) >=0 ) # look at all files for the site... and only keep the positive values
      if ( length(indx) != 0 ) {
        
        idx2 = which(abs(sclip-StartTimesFilesALL[indx]) == min(abs(sclip - StartTimesFilesALL[indx])))
        #sample start on in audio file...
        srd = as.numeric( difftime(sclip, StartTimesFilesALL[idx2],units='secs')) * FS  
        testSound = readWave(mfilesTmp[idx2], header=T)
        
        if ( srd + durT > testSound$samples ) #not all samples in first file
        {
          #open next files if it exists...
          if (is.na(StartTimesFilesALL[idx2+1]) ) {
            cat("Not enough data in this file: skip to next day",sep="\n")
            next  #skip to next day
          }else {
            #open first file
            soundfile1 =  readWave(mfilesTmp[idx2] ,from = srd, to = testSound$samples, units="samples")
            #how much time to grab in next file- samples
            mtograb = durT - (testSound$samples - srd ) 
            #open next file
            soundfile2 = readWave(mfilesTmp[idx2+1] ,from = 1, to = mtograb, units="samples")
            # check: length(soundfile) + length(soundfile2)
            soundfile = bind(soundfile1,soundfile2)
          }
        } else {
          soundfile = readWave(mfilesTmp[idx2] ,from = srd, to = srd+durT, units="samples")
          # length(soundfile)/FS
        }
        #name and save out the file
        fname1 = substr( as.character(basename(mfilesTmp[idx2])), 1, 20)
        stname = sapply(strsplit(fname1,"_"), `[`, 1)
        tmstmp = sapply(strsplit(fname1,"_"), `[`, 2)
        etime = gsub(":","", substr(as.character(sunTmp$sunset[indx2]), 12,21) )
        actualday = gsub("-","", substr(as.character(sclip), 1,10) )
        filename = paste( outDir, "\\",stname, "_SunSet_", tmstmp, "_", actualday, "_", etime, ".wav", sep="")
        
        writeWave(soundfile, filename, extensible=FALSE)
      }else { cat("No data for sunrise ",sep="\n")
        
      }
      rm( soundfile, srd, sclip, sclip1, indx2, indx,idx2,filename,fname1)
    }
   
    #FIND DATA BEFORE Totality: day of only, same duration as total eclipse
    #-----------------------------------------
    # if (eTimes$TotalEclipseBegins[tt] != "-") {
    #   #find time to start
    #   idx = which(abs(Ebegin1 - sunTmp$sunset) == min(abs(Ebegin1- sunTmp$sunset))) #day for sunset
    #   sclip = sunTmp$sunset[idx] + ((durT/FS)) # grab the time after sunset... but this is the next day
    #   
    #   #is there data for next day?
    #   if (exists("dayAud2")){
    #     idx2 = which(abs(sclip-StartTimesFiles2) == min(abs(sclip-StartTimesFiles2))) #day for sunrise
    #     StartTimesFiles
    #   }else { cat("no data for next day!")}
    # }
    
    #FIND DATA AFTER Totality: day of only, same duration as total eclipse
    #-----------------------------------------
    # if (eTimes$TotalEclipseBegins[tt] != "-") {
    #   #find time to start- 
    #   sclip = Ebegin1 + (durT/FS) 
    #   idx2 = which(abs(sclip-StartTimesFiles) == min(abs(sclip - StartTimesFiles)))
    #   #What sample do I start on?
    #   srd = as.numeric( difftime(sclip, StartTimesFiles[idx2],units='secs')) * FS #StartTimesFiles[idx2]
    #   testSound = readWave(dayAud[idx2], header=T)
    #   
    #   if (srd+durT > testSound$samples )  {
    #     #open next files...
    #     if (is.na(dayAud[idx+1]) ) {
    #       cat("Not enough data in this file: skip to next day",sep="\n")
    #       next  #skip to next day..
    #     }else {
    #       #open next file
    #       soundfile1 =  readWave(dayAud[idx] ,from = srd, to = testSound$samples, units="samples")
    #       mtograb = durE - (testSound$samples-srd ) 
    #       soundfile2 = readWave(dayAud[idx+1] ,from = 1, to = mtograb, units="samples")
    #       # check: length(soundfile) + length(soundfile2)
    #       soundfile = bind(soundfile1,soundfile2)
    #     }
    #   } else {
    #     soundfile = readWave(dayAud[idx2] ,from = srd, to = srd+durT, units="samples")
    #     #length(soundfile)/FS
    #   }
    #   #name and save out the file
    #   fname1 = substr( as.character(basename(dayAud[idx2])), 1, 20) 
    #   etime = gsub(":","", substr(as.character(sunTmp$sunrise[idx]), 12,21) )
    #   filename = paste( outDir, "\\After_", fname1, etime, ".wav", sep="")
    #   writeWave(soundfile, filename, extensible=FALSE)
    #   
    #   rm(soundfile, srd, idx2, filename, fname1,sclip)
    # }
    
    #FIND DATA DURING: eclipse
    #-----------------------------------------
    #which audio file do I open?
    # idx = which(abs(Ebegin1-StartTimesFiles) == min(abs(Ebegin1 - StartTimesFiles)))
    #What sample do I start on?
    # srd = as.numeric( difftime(Ebegin1,StartTimesFiles[idx],units='secs')) * FS
    # testSound = readWave(dayAud[idx], header=T)
    # if (srd+durE > testSound$samples )  {
    #   #open next files...
    #   if (is.na(dayAud[idx+1]) ) {
    #     cat("Not enough data in this file: skip to next day",sep="\n")
    #     next  #skip to next day..
    #   }else {
    #     #open next file
    #     soundfile1 =  readWave(dayAud[idx] ,from = srd, to = testSound$samples, units="samples")
    #     mtograb = durE - (testSound$samples-srd ) 
    #     soundfile2 = readWave(dayAud[idx+1] ,from = 1, to = mtograb, units="samples")
    #     # check: length(soundfile) + length(soundfile2)
    #     soundfile = bind(soundfile1,soundfile2)
    #   }
    #   } else {
    #     soundfile = readWave(dayAud[idx] ,from = srd, to = srd+durE, units="samples")
    #   }
    #name and save out the file
    # fname1 = substr( as.character(basename(dayAud[idx])), 1, 20) 
    # etime = gsub(":","",as.character(eTimes$PartialEclipseBegins[tt]))
    # filename = paste( outDir, "\\Eclipse_", fname1, etime, ".wav", sep="")
    # writeWave(soundfile, filename, extensible=FALSE)
    # rm(soundfile, srd, idx, filename, etime, fname1)
  }
}
