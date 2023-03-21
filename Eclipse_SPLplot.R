# PLOT SPL files for eclipse

rm(list=ls(all=TRUE)) 

library(reshape2)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## get files
wrkdir = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO_eclipse\\Meta_AUDIO_eclipse_TOL_Abs_44100ptHannWindow_0pcOlap"
filpat = ".csv"
WAVFiles1 = list.files(wrkdir, recursive= T, pattern = filpat) 
WAVFiles1
sitnms = unique( sapply(strsplit(WAVFiles1,"_"), `[`, 1) ) #unique sites
wrkdirOut = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\ANALYSIS\\SPL"
#_____________________________________________
## READ files back in and and combine... 
#may need to separate these parts... after the above are run
#_____________________________________________
OutDiffBA = NULL
OutSigBA = NULL
#OutDiffSS = NULL
#OutSigSS = NULL
#OutDiffSR = NULL
#OutSigSR = NULL

for (ss in 1:length(sitnms) ) {
  
  sitnms[ss]
  
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
  
  # PLOTS by site
  aa1 = aa
  colnames(aa1) = round(f)
  rownames(aa1) = dyTime
  aa2 <- melt(aa1)  # convert to long format
  aa2$Var3 = sapply(strsplit(as.character(aa2$Var1),"_"), `[`, 2) 
  aa2$Day = sapply(strsplit(as.character(aa2$Var1),"_"), `[`, 1) 
  
  ## ECLIPSE TIMES all: PLOT just eclipse times for all days
  ALL2 = aa2[aa2$Var3 == "TotalEclipse",] 
  temp_plot = ggplot(data =  ALL2, aes(x  = (Var2), y = value, color=Var1)) +
    scale_colour_manual(values=cbPalette) + 
    geom_line() +
    geom_point() +
    scale_x_log10() +
    labs(x = "Frequency [Hz]", y = "Leq in 1/3 octave bands", title = site[1])
  
  ## ECLIPSE TIMES, mean: PLOT the mean of all the days... and eclipse day over laid
  for (ii in 1:dim(ALL2)[1])
    if (ALL2$Day[ii] == "20170821") {
      ALL2$TimePeriod[ii] = "Eclipse"
    }else {  ALL2$TimePeriod[ii] = "Before/After"}
  
  by_siteDate <- group_by(ALL2, Var2, TimePeriod)
  delay <- summarise(by_siteDate,
                     len = n(),
                     se = sd(value, na.rm = TRUE),
                     SPLm = mean(value, na.rm = TRUE))
  
  temp_plot1 = ggplot(data =  delay, aes(x  = (Var2), y = SPLm, color=TimePeriod)) +
    scale_colour_manual(values=cbPalette) + 
    #geom_errorbar(aes(ymin=SPLm-se, ymax=SPLm+se), width=.1) +
    geom_line() +
    geom_point() +
    scale_x_log10() +
    labs(x = "Frequency [Hz]", y = "Leq in 1/3 octave bands", title = site[1])
  temp_plot1 = temp_plot1 + geom_ribbon(aes(ymin=SPLm-se, ymax=SPLm+se), linetype=1, alpha=0.1)
  
  
  ## MEAN OF TIME PERIODS: Psunset, sunrise, eclipse times, eclipse
  for (ii in 1:dim(aa2)[1])
    if (aa2$Day[ii] == "20170821") {
        aa2$TimePeriod[ii] = "Eclipse"
    }else {   
        aa2$TimePeriod[ii] = "Before/After"}
  #add column with eclipse, sunrise, sunset, before after
  for (ii in 1:dim(aa2)[1])
    if (aa2$Var3[ii] == "TotalEclipse" && aa2$Day[ii] == "20170821") {
        aa2$TimePeriod2[ii] =   "Eclipse" #becomes eclipse
    } else if ( aa2$Var3[ii] == "TotalEclipse" && aa2$Day[ii] != "20170821")  {
        aa2$TimePeriod2[ii] =   aa2$TimePeriod[ii] #becomes before/after
    } else {
      aa2$TimePeriod2[ii] =  aa2$Var3[ii] #becomes sunrise or set
    }
  by_siteTy <- group_by(aa2,Var2,Var3,TimePeriod2)
  alltimes <- summarise(by_siteTy,
                     len = n(),
                     se = sd(value, na.rm = TRUE),
                     SPLm = mean(value, na.rm = TRUE))
  
  p <- ggplot(data =  alltimes, aes(x  = (Var2), y = SPLm, color=TimePeriod2)) +
    scale_colour_manual(values=cbPalette) + 
    #geom_errorbar(aes(ymin=SPLm-se, ymax=SPLm+se), width=.1) +
    geom_line() +
    geom_point() +
    scale_x_log10() +
    labs(x = "Frequency [Hz]", y = "Leq in 1/3 octave bands", title = site[1])
  p <- p + geom_ribbon(aes(ymin=SPLm-se, ymax=SPLm+se), linetype=1, alpha=0.1)
  
  
  ## PLOT mean of all days- sunset, sunrise, eclipse
  ALL2 = alltimes[alltimes$TimePeriod2 != "Before/After",] 
  p1 <- ggplot(data =  ALL2, aes(x  = (Var2), y = SPLm, color=TimePeriod2)) +
    scale_colour_manual(values=cbPalette) + 
    #geom_errorbar(aes(ymin=SPLm-se, ymax=SPLm+se), width=.1) +
    geom_line() +
    geom_point() +
    scale_x_log10() +
    labs(x = "Frequency [Hz]", y = "Leq in 1/3 octave bands", title = site[1])
  p1 <- p1 + geom_ribbon(aes(ymin=SPLm-se, ymax=SPLm+se), linetype=1, alpha=0.1)
  
  
  ## SUNSET: PLOT mean of all days- with just SUNSET, eclipse
  ALL3 = ALL2[ALL2$TimePeriod2 != "SunRise",] 
  p3 <- ggplot(data =  ALL3, aes(x  = (Var2), y = SPLm, color=TimePeriod2)) +
    scale_colour_manual(values=cbPalette) + 
    #geom_errorbar(aes(ymin=SPLm-se, ymax=SPLm+se), width=.1) +
    geom_line() +
    geom_point() +
    scale_x_log10() +
    labs(x = "Frequency [Hz]", y = "Leq in 1/3 octave bands", title = site[1])
  p3 <- p3 + geom_ribbon(aes(ymin=SPLm-se, ymax=SPLm+se), linetype=1, alpha=0.1)
  
  
  ## SUNRISE: PLOT mean of all days- just SUNRISE, eclipse
  ALL4 = ALL2[ALL2$TimePeriod2 != "SunSet",] 
  p4 <- ggplot(data =  ALL4, aes(x  = (Var2), y = SPLm, color=TimePeriod2)) +
    scale_colour_manual(values=cbPalette) + 
    #geom_errorbar(aes(ymin=SPLm-se, ymax=SPLm+se), width=.1) +
    geom_line() +
    geom_point() +
    scale_x_log10() +
    labs(x = "Frequency [Hz]", y = "Leq in 1/3 octave bands", title = site[1])
  
  p4 <- p4 + geom_ribbon(aes(ymin=SPLm-se, ymax=SPLm+se), linetype=1, alpha=0.1)
  
  ## SIGNIFICANCE: BEFORE/AFTER
  # delay: for before/after
  ec = delay[delay$TimePeriod == "Eclipse",]
  tp = delay[delay$TimePeriod != "Eclipse",]
  mdif = (ec$SPLm - tp$SPLm) #difference from mean
  sig = NULL  #is differnce greater that SD?
  for (dd in 1:length(mdif)) {
    if( is.nan(tp$se[dd])  ) {
      sig[dd] = 0 #no variation in days
    } else if ( abs(mdif[dd]) <= abs(tp$se[dd]) ) { 
      sig[dd] = 0 #not significant
    } else if (abs(mdif[dd]) > abs(tp$se[dd]) ) {
      sig[dd] = 1 }  # significant 
  }
  test = as.data.frame( cbind(ec$Var2, ec$SPLm,sig ) )
  label.df = test[test$sig==1,]
  delay2 = delay[delay$len>1,]
  p4C =  ggplot() +
      scale_colour_manual(values=cbPalette) + 
      geom_line(  data = delay,  aes(x  = (Var2), y = SPLm, color=TimePeriod)) +
      geom_point( data = delay,  aes(x  = (Var2), y = SPLm, color=TimePeriod)) +
      geom_ribbon(data = delay2, aes(x  = (Var2), ymin = SPLm-se, ymax = SPLm+se), linetype=1, alpha=0.1) +
      geom_point(data = label.df, aes(x=V1, y=V2),shape="*",size=8) +
      scale_x_log10() +
      labs(x = "Frequency [Hz]", y = "Leq in 1/3 octave bands", title = site[1])
  OutDiffBA = rbind(OutDiffBA, mdif ) 
  OutSigBA = rbind(OutSigBA,sig)
  rm(ec,tp,mdif,sig,test,label.df)
  
  ## SIGNIFICANCE: SUNSET
  # ALL3: for sunset
  
  ## SIGNIFICANCE: SUNRISE
  # ALL4: for sunrise
  
  ## save out plots
  setwd(wrkdirOut)
  #ggsave(temp_plot1, file=paste0("plotEclipseTimes_", sitnms[ss],".png"), width = 14, height = 10, units = "cm")
  #ggsave(temp_plot, file=paste0("plotAvgEclipse_", sitnms[ss],".png"), width = 14, height = 10, units = "cm")
  #ggsave(p, file=paste0("plotALL_", sitnms[ss],".png"), width = 14, height = 10, units = "cm")
  #ggsave(p1, file=paste0("plotRiseSet_", sitnms[ss],".png"), width = 14, height = 10, units = "cm")
  #ggsave(p3, file=paste0("plotRise_", sitnms[ss],".png"), width = 14, height = 10, units = "cm")
  #ggsave(p4, file=paste0("plotSet_", sitnms[ss],".png"), width = 14, height = 10, units = "cm")
 
  rm(sig,mdif) 
}


rownames(OutDiffBA) = sitnms
colnames(OutDiffBA) = unique(delay$Var2)
rownames(OutSigBA) = sitnms
colnames(OutSigBA) = unique(delay$Var2)

OutDiffBA1 <- melt(OutDiffBA) 
OutSigBA1 <- melt(OutSigBA)
data = cbind(OutDiffBA1,OutSigBA1$value)
colnames(data) = c("site","frequency","SPL","sigf")

data1 = data[data$sigf ==1, ]
datac = data[data$site !="BRSEE03929",]

gg1 <- ggplot( ) +
  geom_line(data =  datac, aes(x  = frequency, y = SPL, shape = site)) +
  #geom_point(data = datac, aes(x  = frequency, y = SPL, shape = site) ) +
  scale_x_log10() +
  geom_hline(yintercept = 0) + 
  geom_point(data = data1, aes(x=frequency, y=SPL), color = "orange", size=1) +
  labs(x = "Frequency [Hz]", y = "delta dB Leq", title = "")
  #theme_ipsum(grid=F, plot_title_family = 'Slabo 27px', plot_title_face = 'bold', subtitle_size = 10, base_family = 'Roboto Condensed') +
  #theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), panel.background=element_blank()) +
  #theme(plot.background=element_rect(fill="grey"), panel.background=element_rect(fill='gray'), axis.ticks.y=element_blank())+
  #theme(text=element_text(size=16,  family="TT Times New Roman"))

gg1 
# + theme_dark()


setwd(wrkdirOut)
ggsave(gg1, file="CompareSPL2.png", width = 14, height = 10, units = "cm",dpi = 300, bg = "transparent")
#ggsave(gg1, file="CompareSPL.png", width = 14, height = 10, units = "cm",dpi = 300, bg = "transparent")

