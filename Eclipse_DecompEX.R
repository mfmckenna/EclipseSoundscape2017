# playing around with nsprcomp
rm(list=ls(all=TRUE)) 

## LOAD PACKAGES
library(reshape2)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(chron)
library(MuMIn)
library(lme4)
library(zoo) 
library(nsprcomp)
library(audio)
library(heatmaply)

## SET directory of Audio Files
WAVDirsDIR = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO_eclipse"
WAVDirs <- list.dirs(WAVDirsDIR)
filext = "%Y%m%d_%H%M%S.wav" # 
filpat = ".wav"
ff = 1
WAVFiles1 = list.files(WAVDirs[ff], pattern = filpat) 
WAVFiles1
sitnms = unique( sapply(strsplit(WAVFiles1,"_"), `[`, 1) ) #unique sites

## SET calibration params
mhset = -36
Gset = 24
vADCset = 1.414
enviset = "Air"
envir = 2  #1 = water, 2 = air measurements
test  = 0  #set to 1 if you want to test a file

## LOOP through sites... FINAL PLOTS WITH Eclipse_SPLplot.R
for (ss in 4:length(sitnms) ) {
  
  # LIST OF FILES FOR A GIVEN SITE- to be processed
  cat("processing files for: ", sitnms[ss])
  WAVFiles =  WAVFiles1[grepl(sitnms[ss],basename(WAVFiles1))]
  # WAVFiles
  
  ## CALCULATE SPL
  for (ww in 1:length(WAVFiles)) # ww = 12 # loop through each wav file
  {
    cat('Calculating SPL: file ', ww, ' of ',length(WAVFiles), sep="")
    
    s1 =   unlist (strsplit( WAVFiles[ww], '_') ) [1] 
    site = unlist (strsplit( s1, '_') )[1]
    timePeriod = unlist (strsplit( WAVFiles[ww], '_') ) [2] 
    dy = unlist (strsplit( WAVFiles[ww], '_') ) [3]
    if (timePeriod == "TotalEclipse")
    {
      filename = paste(site, timePeriod, filext, sep="_")
    } else {
      filename = paste(site, timePeriod, dy, filext, sep="_")
    }
    
    filenms =  paste(WAVDirsDIR, "\\", WAVFiles[ww], sep="") # gets the full path of the file
    
    ## run code to process
    PAMdir = "E:\\CODE\\Rwork\\PAMGuideSEKI_NVSPL_24Oct2016"
    setwd(PAMdir)
    source('Meta2.R') #modified to not write a conk array
    
    ## 1 Hz... is this too big??
    Meta2(chunksize = 500, atype = 'PSD', timestring = filename,
          r=0, outwrite=1, plottype = "None", calib=1, 
          envi=enviset, ctype="TS", Mh=mhset, G=Gset, vADC=vADCset)
    
    ## 1/3 octave
    #Meta2(chunksize = 500, atype = 'TOL', timestring = filename,
          #r=0, outwrite=1, plottype = "None", calib=1, 
          #envi=enviset, ctype="TS", Mh=mhset, G=Gset, vADC=vADCset)
    
    ## save files in useable in same directory.... sweet!
    
    rm(s1, site, timePeriod, dy, filename, filenms)
  }
  
  ## READ files back in and and combine... 
  #wrkdir = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO_eclipse\\Meta_AUDIO_eclipse_TOL_Abs_44100ptHannWindow_0pcOlap"
  #TOLfiles <- list.files(wrkdir, pattern = paste(sitnms[ss], ".*.TOL.*.csv", sep=""), recursive=T, full.names=T)
  wrkdir = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO_eclipse\\Meta_AUDIO_eclipse_PSD_Abs_44100ptHannWindow_0pcOlap"
  TOLfiles <- list.files(wrkdir, pattern = paste(sitnms[ss], ".*.PSD.*.csv", sep=""), recursive=T, full.names=T)
  
  
  basename(TOLfiles)
  aa = NULL
  site = NULL
  timePeriod = NULL
  dy = NULL
  
  for (ff in 1: length (TOLfiles) ) 
  {
    
    # TOLfiles[ff]
    conk <- as.matrix( read.csv(TOLfiles[ff], colClasses="numeric",header=FALSE) )
    
    # GET file idenity
    fname = basename(TOLfiles[ff])
    site = c(site, unlist (strsplit( fname, '_') )[1])
    timePeriod = c(timePeriod, unlist (strsplit( fname, '_') ) [2] )
    dy = c(dy, (unlist (strsplit( fname, '_') ) [3]) )
    dyTime = paste(dy,timePeriod,sep="_")
    
    # ASSIGN PAMGuide variables envi, calib, atype from metadata
    aid <- conk[1,1]  
    tstampid <- substr(aid,1,1)		#extract time stamp identifier
    enviid <- substr(aid,2,2)			#extract in-air/underwater identifier
    calibid <- substr(aid,3,3)		#extract calibrated/uncalibrated identifier
    atypeid <- substr(aid,4,4)
    if (tstampid == 1){tstamp = 1} else {tstamp = ""}
    if (enviid == 1){
      envi = 'Air'  ; pref <- 20			
    } else {envi = 'Wat' ; pref <- 1}
    if (calibid == 1){calib = 1
    } else {calib = 0}
    if (atypeid == 1){atype = 'PSD'
    } else if (atypeid == 2) {atype = 'PowerSpec'
    } else if (atypeid == 3) {atype = 'TOLf'
    } else if (atypeid == 4) {atype = 'Broadband'
    } else if (atypeid == 5) {atype = 'Waveform'}
    
    # EXTRACT  DATA SPL DATA and TIMESTAMP
    dimc <- dim(conk)  	
    t <- conk[2:dimc[1],1]
    t <- as.POSIXct(t,origin="1970-01-01")
    tString <- as.character(t)
    a <- conk[2:dimc[1],2:dimc[2]] 
    f <- conk[1,2:dimc[2]]
    
    rm(conk)
    
    ## PROCESS the SPL data- only if 1/3 octave
    if (atype == "TOL" ) {
      
      if(dim(a)[2] > 30) a <- a[,1:30]# check to see of more 1/3 OCB than 33... if so truncate data
      endA = ((33-4)-dim(a)[2])+1 # check to see if less than 33 octave
      aweight <- c(-63.4,-56.7,-50.5,-44.7, -39.4, -34.6, -30.2, -26.2, -22.5, - 19.1, -16.1,
                   -13.4, -10.9, -8.6, -6.6, -4.2, -3.2, -1.9, -0.8, 0, 0.6, 1, 1.2,
                   1.3, 1.2, 1.0, 0.5, -0.1, -1.1, -2.5, -4.3, -6.6, -9.3)
      aA = t( t(a) + aweight[4:(33-endA)] )
      press <- rowMeans(10^(aA/10)) # convert to pressures, take the mean for each time step
      dBA = 10*log10(press) # convert to dB
      # hist(dBA)
      
      ## LEQ for the each frequency band- so one row of data
      aa = rbind(aa, 10*log10 (colMeans(10^(a/10)) ))
      # hist(aa)
    }
   
    ## Non-Negative and Sparce PCA 
    #plot of data
    image(t, log(f), a,
          col = colorRampPalette(c("blue","orange","white"))(500),
          xlab="Time (s)",ylab = "Freq Idx" , zlim=c(-40,100), main ="Origional Data")
    dim(a)[1] #number of seconds
    dim(a)[2] #frequency bins 1/3 octave bands
    
    #process data
    a2 = nsprcomp(a, ncomp = 5,nneg= TRUE)
    a2$sdev
    a2$rotation  # matrix of non-negative loadings with principle axes as colmns
    a2$xp       # the scores of matrix with principal components at columns
    
    #image(t, log(f), a2$xp,
          #col = colorRampPalette(c("blue","orange","white"))(500),
          #xlab="Time (s)",ylab = "Freq Idx" , main ="deflated data matrix") # zlim=c(10,80),
     
    image(t, log(f), a2$xp-a,
          col = colorRampPalette(c("blue","orange","white"))(500),
          xlab="Time (s)",ylab = "Freq Idx" , main ="deflated-orig") # zlim=c(10,80),
    # looks like it removed the insects and low tone

    a3 = a2$xp-a
    min(a3)
    max(a3)
    
    min(a)
    max(a)
    # play(WAVFiles[1], "wmplayer.exe")
    
  }
  
  ## PLOT SPL for a given site
  aa1 = aa
  colnames(aa1) = round(f)
  rownames(aa1) = dyTime
  aa2 <- melt(aa1)  # convert to long format
  aa2$Var3 = sapply(strsplit(as.character(aa2$Var1),"_"), `[`, 2) 
  aa2$Day =  sapply(strsplit(as.character(aa2$Var1),"_"), `[`, 1) 
  ALL2 = aa2[aa2$Var3 == "TotalEclipse",] 
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  temp_plot = ggplot(data =  ALL2, aes(x  = log (Var2), y = value, color=Var1)) +
    scale_colour_manual(values=cbPalette) + 
    geom_line() +
    geom_point() +
    labs(x = "Frequency [Hz]", y = "Leq in 1/3 octave bands", title = site[1])
  temp_plot
  
  
 }
