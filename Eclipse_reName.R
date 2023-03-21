#rename Eclipse SOundscape files- with park code

rm(list=ls())

library(tuneR)
setwd("F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO\\BUIS")
mp3Files <- dir(pattern=".wav$")
site =  "BUIS"

for (ff in 1:length(mp3Files)) # ff  = 1
{
  tmp = unlist (strsplit( as.character(mp3Files[ff])[1], "[.]") )
  FileName2 = paste(site, tmp[1], sep="")
  FileName =  paste(FileName2, tmp[2], sep=".")
  
  #rename the file...
  file.rename ( mp3Files[ff], FileName )
  
  rm(tmp,FileName2,FileName)
}


#-------------------------------------------------
# only run if above was not run first....
#-------------------------------------------------
setwd("G:\\AUDIO\\FODO\\NVSPL")
NVSPLFiles <- dir(pattern=".txt$")
for (ff in 1:length(NVSPLFiles)) # ff  =1
{
  tmp = unlist (strsplit( as.character(NVSPLFiles[ff])[1], "_") )
  tmp2 = paste( site, tmp[2], sep="")
  FileName = paste(tmp[1], tmp2,tmp[3],tmp[4],tmp[5], tmp[6], sep="_")
  
  #rename the file...
  file.rename ( NVSPLFiles[ff], FileName )
  
  rm(tmp,FileName)
}

#FIX site name-NVSPL
setwd("G:\\AUDIO\\FODO\\NVSPL")
NVSPLFiles <- dir(pattern=".txt$")
for (ff in 1:length(NVSPLFiles)) # ff  =1
{
  tmp = unlist (strsplit( as.character(NVSPLFiles[ff])[1], "_") )
  tmp2 = paste( tmp[2], tmp[3],sep="")
  FileName = paste(tmp[1], tmp2,tmp[4],tmp[5], tmp[6],  tmp[7], sep="_")
  
  #rename the file...
  file.rename ( NVSPLFiles[ff], FileName )
  
  rm(tmp,FileName)
}


#FIX site name- audio
setwd("G:\\AUDIO\\JEWH")
NVSPLFiles <- dir(pattern=".wav$")
for (ff in 1:length(NVSPLFiles)) # ff  =1
{
  tmp = unlist (strsplit( as.character(NVSPLFiles[ff])[1], "_") )
  tmp2 = paste( tmp[1], tmp[2],sep="")
  FileName = paste(tmp2,tmp[3], tmp[4], sep="_")
  
  #rename the file...
  file.rename ( NVSPLFiles[ff], FileName )
  
  rm(tmp,FileName)
}

# oh shit....
setwd("G:\\AUDIO\\FODO\\NVSPL")
NVSPLFiles <- dir(pattern=".txt$")
NVSPLFilesBAD <- dir(pattern="_NA")
for (ff in 1:length(NVSPLFiles)) # ff  =1
{
  
  file.rename ( NVSPLFilesBAD[ff], NVSPLFiles[ff] )
  
}

#rename the out oput of extractClips.R ----------------------
rm(list=ls())

library(tuneR)
setwd("F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO_eclipse\\")
mp3Files <- dir(pattern=".wav$")

for (ff in 1:length(mp3Files)) # ff=1
{
  tmp = unlist (strsplit( as.character(mp3Files[ff])[1], "_") )
  FileName =  paste(tmp[2], tmp[1], tmp[3], tmp[4], sep="_")
  
  #rename the file...
  file.rename ( mp3Files[ff], FileName )
  
  rm(tmp,FileName2,FileName)
}