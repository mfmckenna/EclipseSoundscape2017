# run PAMGUIDE for single files... 

rm(list=ls(all=TRUE)) 

library(reshape2)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(chron)
library(MuMIn)
library(lme4)
library(zoo) 
library(heatmaply)

## SET eclipse data set
WAVDirsDIR = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO_eclipse"
WAVDirs <- list.dirs(WAVDirsDIR)
filext = "%Y%m%d_%H%M%S.wav" # 
filpat = ".wav"


## SET calibration params
mhset = -36
Gset = 24
vADCset = 1.414
enviset = "Air"
envir = 2  #1= water, 2 = air measurements
test  = 0  #set to 1 if you want to test a file

## get file for a given site
ff = 1
WAVFiles1 = list.files(WAVDirs[ff], pattern = filpat) 
WAVFiles1

## eventyally, want to loop through each site.. process and plot (to save some memory!)
#!!!! FINAL PLOTS WITH Eclipse_SPLplot.R

sitnms = unique( sapply(strsplit(WAVFiles1,"_"), `[`, 1) ) #unique sites
for (ss in 4:length(sitnms) ) {
  
  # ss = 1
  #sitnms[3]
  WAVFiles =  WAVFiles1[grepl(sitnms[ss],basename(WAVFiles1))]
  WAVFiles
  
  ## sets the filename
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
    #Meta2(chunksize = 500, atype = 'PSD', timestring = filename,
    # r=0, outwrite=1, plottype = "None", calib=1, 
    # envi=enviset, ctype="TS", Mh=mhset, G=Gset, vADC=vADCset)
    
    ## 1/3 octave
    Meta2(chunksize = 500, atype = 'TOL', timestring = filename,
          r=0, outwrite=1, plottype = "None", calib=1, 
          envi=enviset, ctype="TS", Mh=mhset, G=Gset, vADC=vADCset)
    
    ## save files in useable in same directory.... sweet!
    
    rm(s1, site, timePeriod, dy, filename, filenms)
  }
  
  #_____________________________________________
  ## READ files back in and and combine... 
  #may need to separate these parts... after the above are run
  #_____________________________________________
  
  sitnms[ss]
  
  wrkdir = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO_eclipse\\Meta_AUDIO_eclipse_TOL_Abs_44100ptHannWindow_0pcOlap"
  TOLfiles <- list.files(wrkdir, pattern = paste(sitnms[ss], ".*.TOL.*.csv", sep=""), recursive=T, full.names=T)
  basename(TOLfiles)
  aa = NULL
  site = NULL
  timePeriod = NULL
  dy = NULL
  
  for (ff in 1: length (TOLfiles) ) # ff = 1
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
    
    # PROCESS the SPL data...
    # 1) dBA values
    # check to see of more 1/3 OCB than 33... if so truncate data
    if(dim(a)[2] > 30) a <- a[,1:30]
    # check to see if less than 33 octave
    endA = ((33-4)-dim(a)[2])+1
    # calculate a dBA
    aweight <- c(-63.4,-56.7,-50.5,-44.7, -39.4, -34.6, -30.2, -26.2, -22.5, - 19.1, -16.1,
                 -13.4, -10.9, -8.6, -6.6, -4.2, -3.2, -1.9, -0.8, 0, 0.6, 1, 1.2,
                 1.3, 1.2, 1.0, 0.5, -0.1, -1.1, -2.5, -4.3, -6.6, -9.3)
    aA = t( t(a) + aweight[4:(33-endA)] )
    press <- rowMeans(10^(aA/10)) # convert to pressures, take the mean for each time step
    dBA = 10*log10(press) # convert to dB
    #hist(dBA)
    
    #2) LEQ for the each frequency band- so one row of data
    aa = rbind(aa, 10*log10 (colMeans(10^(a/10)) ))
    
  }
  
  # plot by site
  aa1 = aa
  
  colnames(aa1) = round(f)
  rownames(aa1) = dyTime
  aa2 <- melt(aa1)  # convert to long format
  aa2$Var3 = sapply(strsplit(as.character(aa2$Var1),"_"), `[`, 2) 
  aa2$Day = sapply(strsplit(as.character(aa2$Var1),"_"), `[`, 1) 
  
  ALL2 = aa2[aa2$Var3 == "TotalEclipse",] 
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  temp_plot = ggplot(data =  ALL2, aes(x  = log (Var2), y = value, color=Var1)) +
    scale_colour_manual(values=cbPalette) + 
    geom_line() +
    geom_point() +
    labs(x = "Frequency [Hz]", y = "Leq in 1/3 octave bands", title = site[1])
  temp_plot
  
  #plot the mean of all the days... and eclipse day over laid
  #add column for eclipse vs not
  for (ii in 1:dim(ALL2)[1])
    if (ALL2$Day[ii] == "20170821") {
      ALL2$TimePeriod[ii] = "Eclipse"
    }else {  ALL2$TimePeriod[ii] = "Before/After"}
  
  by_siteDate <- group_by(ALL2, Var2, TimePeriod)
  delay <- summarise(by_siteDate,
                     len = n(),
                     se = sd(value, na.rm = TRUE),
                     SPLm = mean(value, na.rm = TRUE))
  
  temp_plot1 = ggplot(data =  delay, aes(x  = log(Var2), y = SPLm, color=TimePeriod)) +
    scale_colour_manual(values=cbPalette) + 
    geom_errorbar(aes(ymin=SPLm-se, ymax=SPLm+se), width=.1) +
    geom_line() +
    geom_point() +
    labs(x = "Frequency [Hz]", y = "Leq in 1/3 octave bands", title = site[1])
  temp_plot1
  
  ## save out plots
  setwd(WAVDirsDIR)
  ggsave(temp_plot1, file=paste0("plot1_", sitnms[ss],".png"), width = 14, height = 10, units = "cm")
  ggsave(temp_plot, file=paste0("plot0_", sitnms[ss],".png"), width = 14, height = 10, units = "cm")
}
