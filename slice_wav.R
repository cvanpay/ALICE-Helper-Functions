# wav = specify full path for wav file
# output = will create folder and place split wavs in there,
#          or split wavs will be placed in working directory
# prefix - if you want a prefix in the name of your split wavs,
#          otherwise they will be 1.wav through n.wav
# duration - how long you want the split wav files to be in seconds

slice_wav <- function(wav, output = getwd(), prefix, duration = 300) {
  require(tuneR)
  # Load audio wave into object
  wavR <- tuneR::readWave(wav)
  #Create output folder, if specified
  dir.create(file.path(output), showWarnings = FALSE)
  setwd(file.path(output))
  #Set frequency
  freq <- wavR@samp.rate
  # Set the length
  totlen <- length(wavR) # 1 channel default is left
  #Set the duration
  totsec <- totlen / freq
  # How long each sample is (in seconds)
  seglen <- duration
  #Defining the break points
  breaks <- unique(c(seq(0, totsec, seglen), totsec))
  index <- 1:(length(breaks) - 1)
  #Splitting the file
  items <-
    lapply(index, function(i)
      wavR[(breaks[i] * freq):(breaks[i + 1] * freq)])
  # Creating a folder of wavs
  for (i in 1:length(items)) {
    wavName <- paste(prefix, i,'.wav',sep='')  # file name
    writeWave(items[[i]], wavName) # write the file
  }
}
