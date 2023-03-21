#NVSPL-----------------------------------------------------------------------------------------------------
rm(list=ls())
workingDir = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO"
nvsplFiles <- list.files(workingDir, pattern="NVSPL", recursive=T, full.names=T)
targetdir = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\NVSPL"
filestocopy = nvsplFiles
file.copy(from=filestocopy, to=targetdir, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
#DONE!

#SPECTROGRAMS-----------------------------------------------------------------------------------------------------
rm(list=ls())
workingDir = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\AUDIO"
pngFiles <- list.files(workingDir, pattern=".png", recursive=T, full.names=T)
targetdir = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\SPECTROGRAMS"
filestocopy = pngFiles
file.copy(from=filestocopy, to=targetdir, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
#DONE!