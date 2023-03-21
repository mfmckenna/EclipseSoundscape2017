rm(list=ls(all=TRUE)) 
library(nsprcomp)

wrkdir = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO_eclipse\\Meta_AUDIO_eclipse_TOL_Abs_44100ptHannWindow_0pcOlap"
TOLfiles <- list.files(wrkdir, pattern = ".*.TOL.*.csv", recursive=T, full.names=T)


basename(TOLfiles)
aa = NULL
site = NULL
timePeriod = NULL
dy = NULL

for (ff in 1: length (TOLfiles) ) # ff = 73
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
  if (atype == "TOLf" ) {
    
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
  
  #plot of data
  stname = substr(as.character(fname), 1,27)
  image(t, log(f), a,
        col = colorRampPalette(c("blue","orange","white"))(500),
        xlab="Time (s)",ylab = "Freq Idx" , zlim=c(-40,100), main = paste( "Origional Data:", stname, sep=" " ) ) 
  dim(a)[1] #number of seconds
  dim(a)[2] #frequency bins 1/3 octave bands
  
  
  ## NCOMP: Non-Negative and Sparce PCA- not what we wanted because it does not do robust part
  a2 = nsprcomp(a, ncomp = 5, nneg= TRUE)
  a2$rotation  # matrix of non-negative loadings with principle axes as colmns
  a2$x        #the scores matrix XW containing the principal components as columns (after
               #centering and scaling if requested). For the formula method, n
  a2$xp       # the deflated data matrix corresponding to x
  image(t, log(f), a2$xp-a,
        col = colorRampPalette(c("blue","orange","white"))(500),
        xlab="Time (s)",ylab = "Freq Idx" , main =paste( "deflated-orig:", stname, sep=" " )) # zlim=c(10,80),
  plot(a2$x[,1]) #looking at the time sequence of the Pcomponents
  plot(a2$q[,1])
 
  
  ## rNMF: Robust Nonnegative Matrix Factorization
  # https://cran.r-project.org/web/packages/rNMF/rNMF.pdf
  
  #1) 
  a3 = rnmf(a, k = 5)
  str(a3)
  image(t, log(f), a3$fit,
        col = colorRampPalette(c("blue","orange","white"))(500),
        xlab="Time (s)",ylab = "Freq Idx" , main =paste( "rnmf:", stname, sep=" " ))
  
  #2)
  a3 = rnmf(a, k = 5, gamma=0.03) #addes in trimming precentage- default is 0.05
  str(a3)
  image(t, log(f), a3$fit,
        col = colorRampPalette(c("blue","orange","white"))(500),
        xlab="Time (s)",ylab = "Freq Idx" , main =paste( "rnmf:", stname, sep=" " ))
  a3$H[1,]
  a3$W[,1]
  a3$W[,1]%*%t(a3$H[1,])
  image(t, log(f), a3$W[,1]%*%t(a3$H[1,]),
        col = colorRampPalette(c("blue","orange","white"))(500),
        xlab="Time (s)",ylab = "Freq Idx" , main =paste( "deflated-orig:", stname, sep=" " ))
 
  #3) in pressure- works pretty good
   a3 = rnmf(10^(a/10), k = 5)
   str(a3)
   image(t, log(f), a3$fit,
         col = colorRampPalette(c("blue","orange","white"))(500),
         xlab="Time (s)",ylab = "Freq Idx" , main =paste( "rnmf:", stname, sep=" " ))
   #look at each component
   image(t, log(f), log10(a3$W[,2]%*%t(a3$H[2,])),
        col = colorRampPalette(c("blue","orange","white"))(500),
        xlab="Time (s)",ylab = "Freq Idx" , main =paste( "deflated-orig:", stname, sep=" " ))
   #look at outliers
   image(t, log(f), log10(10^(a/10)-a3$fit),
           col = colorRampPalette(c("blue","orange","white"))(500),
           xlab="Time (s)",ylab = "Freq Idx" , main =paste( "deflated-orig:", stname, sep=" " ))
   cor(t(a3$H))
   cor(a3$W)
   plot(a3$H[1,],a3$H[3,])
   
}