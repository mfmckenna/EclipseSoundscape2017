# process Eclipse Survey results

rm(list=ls())
library(readxl)

#---------------------------------------------------------------
# READ in survey clip details
#---------------------------------------------------------------
clipDet = read_xlsx("F:\\RESEARCH\\NSNSD_EclipseSoundScape\\EclipseSoundscape_clips.xlsx", 
                    col_types = c("date","text","numeric","numeric","numeric","text","text","text","text","text",
                                  "text","text","text","text"))
clipDet = clipDet[is.na(clipDet$Sort)==F,] #only clips that were processed
# unique(clipDet$TimePeriod)


#---------------------------------------------------------------
# READ in spreadsheets
#---------------------------------------------------------------
inDir = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\Listening"
setwd(inDir)
inFiles = list.files(inDir, pattern = "*.xlsx")

dataC = NULL
for (ff in 1:length(inFiles))
{
  #read in survey result file
  tmp = read_excel(inFiles[ff]) 
  #get the survey number from the file name
  ntmp = unlist(strsplit(inFiles[ff],"V",fixed=TRUE))[2]
  survNum = as.numeric( unlist(strsplit(ntmp,")",fixed=TRUE))[1] )
  
  #get details on the clips in the survey- match with clipDets
  tempDet = clipDet[clipDet$SurveyNumber == survNum,]
  tempDet2 = as.data.frame(cbind(tempDet$ClipID, tempDet$TimePeriod, tempDet$Date,tempDet$FileNames))
  colnames(tempDet2) = c("clipID","TimePeriod","Date","Clip")
  tempDet2 =  tempDet2[rowSums(is.na(tempDet2))!=ncol(tempDet2),]
  
  
  # loop through responses to get the details...
  survResult = NULL
  for (rr in 1:dim(tmp)[1])
  {
    
    tmp2 = tmp[rr,]
    
    # person
    tempDet2$Surv =        c(survNum,survNum,survNum,survNum,survNum)
    tempDet2$Email =       c(tmp2[2],tmp2[2],tmp2[2],tmp2[2],tmp2[2])
    tempDet2$HeadPhones =  c(tmp2[3],tmp2[3],tmp2[3],tmp2[3],tmp2[3])
    tempDet2$Locations =   c(tmp2[4],tmp2[4],tmp2[4],tmp2[4],tmp2[4])
    tempDet2$Hear =        c(tmp2[5],tmp2[5],tmp2[5],tmp2[5],tmp2[5])
    tempDet2$Feedback =    c(tmp2[66],tmp2[66],tmp2[66],tmp2[66],tmp2[66])
    
    # clip 1 dim(tempDet2)
    tempDet2$NumBirds[1] = tmp2[6]
    tempDet2$Decrip[1] =   tmp2[7]
    tempDet2$Bird[1]=      tmp2[8]
    tempDet2$Mammal[1] =   tmp2[9]
    tempDet2$Insect[1] =   tmp2[10]
    tempDet2$Voices[1] =   tmp2[11]
    tempDet2$Activity[1] = tmp2[12]
    tempDet2$Weather[1]=   tmp2[13]
    tempDet2$Unk[1] =      tmp2[14]
    tempDet2$Static[1] =   tmp2[15]
    tempDet2$Species[1]=   tmp2[16]
    tempDet2$Guess[1] =    tmp2[17]
    
    # clip 2 dim(tempDet2)
    tempDet2$NumBirds[2] = tmp2[18]
    tempDet2$Decrip[2] =   tmp2[19]
    tempDet2$Bird[2]=      tmp2[20]
    tempDet2$Mammal[2] =   tmp2[21]
    tempDet2$Insect[2] =   tmp2[22]
    tempDet2$Voices[2] =   tmp2[23]
    tempDet2$Activity[2] = tmp2[24]
    tempDet2$Weather[2]=   tmp2[25]
    tempDet2$Unk[2] =      tmp2[26]
    tempDet2$Static[2] =   tmp2[26]
    tempDet2$Species[2]=   tmp2[28]
    tempDet2$Guess[2] =    tmp2[29]
    
    # clip 3 dim(tempDet2)
    tempDet2$NumBirds[3] = tmp2[30]
    tempDet2$Decrip[3] =   tmp2[31]
    tempDet2$Bird[3]=      tmp2[32]
    tempDet2$Mammal[3] =   tmp2[33]
    tempDet2$Insect[3] =   tmp2[34]
    tempDet2$Voices[3] =   tmp2[35]
    tempDet2$Activity[3] = tmp2[36]
    tempDet2$Weather[3]=   tmp2[37]
    tempDet2$Unk[3] =      tmp2[38]
    tempDet2$Static[3] =   tmp2[39]
    tempDet2$Species[3]=   tmp2[40]
    tempDet2$Guess[3] =    tmp2[41]
    
    
    # clip 4 dim(tempDet2)
    tempDet2$NumBirds[4] = tmp2[42]
    tempDet2$Decrip[4] =   tmp2[43]
    tempDet2$Bird[4]=      tmp2[44]
    tempDet2$Mammal[4] =   tmp2[45]
    tempDet2$Insect[4] =   tmp2[46]
    tempDet2$Voices[4] =   tmp2[47]
    tempDet2$Activity[4] = tmp2[48]
    tempDet2$Weather[4]=   tmp2[49]
    tempDet2$Unk[4] =      tmp2[50]
    tempDet2$Static[4] =   tmp2[51]
    tempDet2$Species[4]=   tmp2[52]
    tempDet2$Guess[4] =    tmp2[53]
    
    
    # clip 5 dim(tempDet2)
    tempDet2$NumBirds[5] = tmp2[54]
    tempDet2$Decrip[5] =   tmp2[55]
    tempDet2$Bird[5]=      tmp2[56]
    tempDet2$Mammal[5] =   tmp2[57]
    tempDet2$Insect[5] =   tmp2[58]
    tempDet2$Voices[5] =   tmp2[59]
    tempDet2$Activity[5] = tmp2[60]
    tempDet2$Weather[5]=   tmp2[61]
    tempDet2$Unk[5] =      tmp2[62]
    tempDet2$Static[5] =   tmp2[63]
    tempDet2$Species[5]=   tmp2[64]
    tempDet2$Guess[5] =    tmp2[65]
    
    survResult = rbind(survResult,tempDet2)
  }
  
  dataC = rbind(dataC,survResult)
  
  rm(tmp,tmp2, ntmp, survNum, tempDet, tempDet2, survResult)
  
}

#---------------------------------------------------------------
#CLEAN UP
#---------------------------------------------------------------
# EXTRACT SITE NAMES
#strsplit(as.character(dataC$Clip[1]),"_")
dataC$Site = sapply(strsplit(as.character(dataC$Clip),"_"), `[`, 1)
#---------------------------------------------------------------
#FIX DOUBLE RESPONSES- just take first answer
respons1 = as.character(unique(dataC$Bird))
dataC[dataC$Bird == (respons1[5]),]
idx = which(grepl(as.character((respons1[5])), dataC$Bird))
dataC$Bird[idx] = unlist( strsplit(as.character(dataC$Bird[idx]),",") )[1]
responsBird = as.character(unique(dataC$Bird))
dataC$Insect = sapply(strsplit(as.character(dataC$Insect),","), `[`, 1)
#as.character(unique(dataC$Insect))
dataC$Activity = sapply(strsplit(as.character(dataC$Activity),","), `[`, 1)
#as.character(unique(dataC$Activity))
dataC$Mammal = sapply(strsplit(as.character(dataC$Mammal),","), `[`, 1)
#as.character(unique(dataC$Mammal))
dataC$Voices = sapply(strsplit(as.character(dataC$Voices),","), `[`, 1)
#as.character(unique(dataC$Voices))
dataC$Weather = sapply(strsplit(as.character(dataC$Weather),","), `[`, 1)
#as.character(unique(dataC$Weather))
dataC$Unk = sapply(strsplit(as.character(dataC$Unk),","), `[`, 1)
#as.character(unique(dataC$Unk))
dataC$Static = sapply(strsplit(as.character(dataC$Static ),","), `[`, 1)
#as.character(unique(dataC$Static ))
#---------------------------------------------------------------
# TURN RESPONSES INTO NUMERIC 0-4 
dataCNum = as.data.frame(dataC) # create new matrix
respons1 = as.character(unique(dataC$Bird))
dataCNum[dataCNum==respons1[1]] = 2 # "Half the clip"
dataCNum[dataCNum==respons1[2]] = 4 # "Always present"
dataCNum[dataCNum==respons1[3]] = 0 # "Not present"
dataCNum[dataCNum==respons1[4]] = 3 # "Majority of clip"
dataCNum[dataCNum==respons1[5]] = 1 # "Some present"

for (ii in 1:dim(dataCNum)[1]){ # ii = 2
  if (as.character(dataCNum$Date[ii]) == "20170821") { dataCNum$Period[ii] = 1
  } else{ dataCNum$Period[ii] = 2}
  
}


#summary details
dim(dataCNum) 
length( unique(dataCNum$Surv) )
length( unique(dataCNum$Email) )
length( unique(dataCNum$Site) )
length( unique(dataCNum$clipID) )

dim( dataCNum[dataCNum$HeadPhones == "Yes-",] ) 
dim( dataCNum[dataCNum$HeadPhones == "No",] ) 

now = Sys.Date()
dataCNum$HeadPhones = gsub(","," ", dataCNum$HeadPhones)
dataCNum$Hear = gsub(","," ", dataCNum$Hear)
dataCNum$Feedback = gsub(",","-", dataCNum$Feedback)
dataCNum$Decrip = gsub(",",";", dataCNum$Decrip)
dataCNum$Species = gsub(",",";", dataCNum$Species)
dataCNum$Guess = gsub(",",";", dataCNum$Guess)
write.csv(as.matrix(dataCNum), file = paste("EclipseListenResults_", now,".csv", sep="") )

#---------------------------------------------------------------
#ANALYZE
#---------------------------------------------------------------
inData = (dataCNum)
colnames(inData)
sites =  unique(inData$Site)

for (ss in 1:length(sites)){ # ss = 7 #site
  
  #combine data for analysis/graphic
  tmp =  inData[inData$Site == sites[ss], ]
  tmp2 = tmp[,c('Period','Bird','Mammal','Voices','Activity',"Weather",'Unk','Static','Insect')]
  tmp2 = ( sapply(tmp2, as.numeric) )
  tmp2 = as.data.frame(tmp2)
  
  ndys = length(unique(tmp$Date))-1
  #combine to get the mean
  #eclipse data
  e =  tmp2[tmp2[, 'Period'] == 1,] 
  eobs = dim(e)[1]
  e = e[rowSums(is.na(e))!= ncol(e)-1, ]
  if (dim(e)[1] > 1 ){
    e = (colMeans(e))        # mean of observations
    e = t(as.data.frame(e))  # convert back to same format as e!!!
  }
    
  if (dim(e)[1] > 0 ) # only proceeded if there is data from the eclipse analyzed!
  { 
    
    #non-eclipse data
    n = tmp2[tmp2[, 'Period'] == 2,] 
    #remove days before/after with different weather than eclipse day??
    wIdx =   round(as.numeric(e['Weather']))
    n2 = as.matrix(n[n[, 'Weather'] == wIdx,]) 
    
    #replace all and plot "weather change" b/c too many not shown!
    n2 = n 
    n2 = n2[rowSums(is.na(n2))!= ncol(n2)-1, ] #removes NA values
    nobs = dim(n2)[1] #how many observations for this sites
    
    if (dim(n2)[1] > 0 ) 
    { 
      
      nMean = colMeans(n2)
      
      ## DIFFERENCE & STD AND PLOT ##
      
      #difference from eclipse time period for each observation: mean & stdev
      n3 = data.matrix((n2))
      #ugh- just do each row...
      nSweep = NULL
      for (rr in 1:nobs){ # rr = 1
        nSweep = rbind(nSweep, e - n3[rr,])
      }
        
      # if positive, eclipse had more; if negative, eclipse had lower
      n3Mean = round( colMeans(nSweep),2 )
      nSD = apply(nSweep,2, sd, na.rm = TRUE)  # check: sd(nSweep[,1])
      nSE = nSD/sqrt(length(nSD))
      x = (colnames(n2)) #add column with type
      
      # create matrix for plotting
      tmp = data.frame(as.numeric((n3Mean)), as.numeric(nSE) ,as.numeric(nSD),x)   
      colnames(tmp) = c('value',"SE","SD","type")
      tmpW =  tmp[-c(1,4,5,6,7,8), ] #remove some rows that are not of interst
      tmpH =  tmp[c(4,5), ] #remove some rows that are not of interst
      #positions <- c("Bird", "Insect", "Mammal","Voices","Activity")
      
      # widlife response by site
      p1 = ggplot(data=tmpW, aes(y=value, x=type, fill=type) ) +
        geom_bar(stat="identity") +  geom_hline(yintercept = 0) +
        geom_errorbar(data = tmpW, aes(ymin=value-SE, ymax=value+SE),
                      width=.2, # Width of the error bars
                      position = position_dodge(.9)) +
        #ylim(-4, 4) +
        labs(title = substr(sites[ss],1,4),
             caption = paste( "DATA: N=",eobs, " eclipse, ", "N=", nobs, " non on ",ndys, " day[s]",sep = ""),
             x = "", y = "") +
        theme_classic(base_size = 12)       +
        theme(legend.position="none") +
        scale_y_continuous(breaks=c(-4, -2,0,2, 4),
                           labels=c("","less", "no change","more", ""),limits=c(-4, 4))
      p1
      #ggsave(p1, file=paste(sites[ss], "_", ss,"_ListeningResultsWildlife.eps",sep = ""), width = 14, height = 10, units = "cm",dpi = 300, bg = "transparent")
      ggsave(p1, file=paste(sites[ss], "_", ss,"_ListeningResultsWildlife.png",sep = ""), width = 14, height = 10, units = "cm",dpi = 300, bg = "transparent")
      
      # human response by site
      p2 = ggplot(data=tmpH, aes(y=value, x=type, fill=type) ) +
        geom_bar(stat="identity") + geom_hline(yintercept = 0) +
        geom_errorbar(data = tmpH, aes(ymin=value-SE, ymax=value+SE),
                      width=.2, # Width of the error bars
                      position = position_dodge(.9)) +
        ylim(-4, 4) +
        labs(title = substr(sites[ss],1,4),
             caption = paste( "DATA: N=",eobs, " eclipse, ", "N=", nobs, " non on ",ndys, " day[s]",sep = ""),
             x = "", y = "") +
        theme_classic(base_size = 12)       +
        theme(legend.position="none") +
        scale_y_continuous(breaks=c(-4, -2,0,2, 4),
                         labels=c("","less", "no change","more", ""),limits=c(-4, 4))
        #scale_y_discrete(labels = c("-2" = "less", "0" = "No change", "2" = "More likely", "4" = "all"))
      p2
      #ggsave(p2, file=paste(sites[ss], "_", ss,"_ListeningResultsHuman.eps",sep = ""), width = 14, height = 10, units = "cm",dpi = 300, bg = "transparent")
      ggsave(p2, file=paste(sites[ss], "_", ss,"_ListeningResultsHuman.png",sep = ""), width = 14, height = 10, units = "cm",dpi = 300, bg = "transparent")
      
      
      # plot the difference between from eclipse as bar for each category! 
      change = t(as.data.frame(e-nMean)) # if positive, eclipse had more; if negative, eclipse had lower.
      colnames(change) = "value"
      change = as.data.frame(change)
      x = (row.names(change)) #add column with type
      change$type = x
      change =  change[-c(1,7,8), ] #remove some rows that are not of interst
      
      p = ggplot(data=change, aes(y=value, x=type) ) +
        geom_bar(stat="identity") + 
        ylim(-4, 4) +
        labs(title = sites[ss])
      p
      
      #ggsave(p, file=paste(sites[ss], "_", ss,"_ListeningResults.png",sep = ""), width = 14, height = 10, units = "cm",dpi = 300, bg = "transparent")
      
    } else { cat("No comparison for: ", sites[ss],"\n") }
    
  } else {   cat("No eclipse for: ", sites[ss],"\n") }
  
  
  rm(tmp,tmp2,e,wIdx,n,n2,change)  
} #end sites










