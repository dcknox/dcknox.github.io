library(Rcpp)
library(jsonlite)
library(Cairo)

sam.dir <- '~/projects/feelR'
source(file.path(sam.dir, 'R/extractAudioFeatures.R'))
## source(file.path(sam.dir, 'R/standardizeFeatures.R'))

`%.%` <- paste0

CairoFonts('Raleway:style=Bold')

red <- '#A51C30'
blue <- '#4E84C4'

## shadowtext <- function(x, y=NULL, labels, col='white', bg='black',
##                        theta = seq(pi/4, 2*pi, length.out=8), r=0.1, cex=1, ... ) {
## 	xy <- xy.coords(x,y)
## 	xo <- r*strwidth('A', cex = cex)
## 	yo <- r*strheight('A', cex = cex)
## 	for (i in theta) {
## 		text( xy$x + cos(i)*xo, xy$y + sin(i)*yo,
##              labels, col=bg, cex=cex, ... )
## 	}
## 	text(xy$x, xy$y, labels, col=col, cex=cex, ... )
## }



################
## timestamps ##
################

if (!file.exists('obama_whoweare.csv')){
  fname <- 'obama_whoweare.wav'
  json <- system(
    sprintf('gcloud ml speech recognize %s  --language-code="en-US" --include-word-time-offsets',
            fname
            ),
    intern = TRUE
  )
  transcript <- do.call(
    rbind,
    lapply(fromJSON(json, simplifyVector = FALSE)$results,
           function(x){
             do.call(rbind, x$alternatives[[1]]$words)
           })
  )
  transcript <- as.data.frame(transcript)
  transcript$endTime <- as.numeric(gsub('s', '', transcript$endTime))
  transcript$startTime <- as.numeric(gsub('s', '', transcript$startTime))
  transcript$word <- as.character(transcript$word)
  write.csv(transcript[, c('word', 'startTime', 'endTime')],
            'obama_whoweare.csv',
            row.names = FALSE
            )
} else {
  transcript <- read.csv('obama_whoweare.csv',
                         stringsAsFactors = FALSE
                         )
}



####################
## audio features ##
####################

audio <- extractAudioFeatures(
  wav.fnames = 'obama_whoweare.wav',
  derivatives = 0
)$data[[1]]

timestamps <- attr(audio, 'timestamps')



#####################
## plot utterances ##
#####################

## ## subset to male vocal range
## audio[audio[,'f0_mhs'] < 85, 'f0_mhs'] <- NA
## audio[audio[,'f0_mhs'] > 180, 'f0_mhs'] <- NA

## average two f0 estimators
audio <- cbind(audio,
               f0 = (audio[,'f0_mhs'] + audio[,'f0_ksv']) / 2
               )
## if either deviate by >10%, discard
audio[audio[, 'f0_mhs'] > 1.1 * audio[, 'f0'], 'f0'] <- NA
audio[audio[, 'f0_mhs'] < 0.9 * audio[, 'f0'], 'f0'] <- NA
audio[audio[, 'f0_ksv'] > 1.1 * audio[, 'f0'], 'f0'] <- NA
audio[audio[, 'f0_ksv'] < 0.9 * audio[, 'f0'], 'f0'] <- NA
## subset to male vocal range
audio[audio[,'f0'] < 85, 'f0'] <- NA
## audio[audio[,'f0'] > 180, 'f0'] <- NA

transcript$energy <- sapply(
  1:nrow(transcript),
  function(i){
    max(
      audio[,'energy_dB'][
        timestamps / 1000 >= transcript$startTime[i] &
          timestamps / 1000 < transcript$endTime[i]
      ],
      na.rm = TRUE
    )
  })

transcript$pitch <- sapply(
  1:nrow(transcript),
  function(i){
    median(
      audio[,'f0'][
        timestamps / 1000 >= transcript$startTime[i] &
          timestamps / 1000 < transcript$endTime[i]
      ],
      na.rm = TRUE
    )
  })



################
## plot words ##
################

CairoPDF('../images/utterance.pdf', 30, 6)
plot(timestamps / 1000,
     audio[,'f0'],
     axes = FALSE,
     type = 'l',
     ylim = c(0, 350),
     xlab = NA,
     ylab = NA,
     col = NA
     )
text((transcript$startTime + transcript$endTime) / 2,
     y = transcript$pitch,
     labels = transcript$word,
     cex = exp((transcript$energy - 50) / 10) / 3,
     adj = c(.5, .5),
     ## col = 'gray30',
     col = paste0(
       '#',
       apply(
         colorRamp(c(blue, 'black', red))(
           scales::rescale(rank(transcript$pitch))
         ),
         1,
         function(x){
           paste(format(as.hexmode(round(x)), width = 2), collapse = '')
         })
     ),
     srt = 0
     )
dev.off()

CairoPNG('../images/utterance_pitch_0.png', 2000, 1000, bg = 'transparent')
plot(timestamps / 1000,
     audio[,'f0'],
     axes = FALSE,
     type = 'l',
     ylim = c(-100, 350),
     xlab = NA,
     ylab = NA,
     lwd = 2
     )
dev.off()

CairoPNG('../images/utterance_pitch_1.png', 2000, 1000, bg = 'transparent')
plot(timestamps / 1000,
     audio[,'f0'],
     axes = FALSE,
     type = 'l',
     ylim = c(-100, 350),
     xlab = NA,
     ylab = NA,
     lwd = 2
     )
for (i in 1:nrow(transcript)){
  lines(rep(transcript$startTime[i], 2),
        y = c(100, 350),
        col = 'gray50'
        )
  lines(rep(transcript$endTime[i], 2),
        y = c(100, 350),
        col = 'gray50'
        )
    lines(c(transcript$startTime[i], transcript$endTime[i]),
        y = rep(100, 2),
        col = 'gray50'
        )
}
labels.y <- 50 - c(0:2, 0:4, 0:4, 0, 0, 0:3, 0:2, 0:2, rep(0, 4)) * 15
for (i in 1:nrow(transcript)){
  lines(rep((transcript$startTime[i] + transcript$endTime[i]) / 2, 2),
        y = c(labels.y[i], 100),
        col = 'gray50'
        )
}
text((transcript$startTime + transcript$endTime) / 2,
     y = labels.y,
     labels = transcript$word,
     adj = c(1, .5),
     col = 'gray30',
     srt = 45,
     cex = 3
     )
dev.off()

CairoPNG('../images/utterance_energy_0.png', 2000, 1000, bg = 'transparent')
plot(timestamps / 1000,
     10^((audio[,'energy_dB'] - 46)/10) + 100,
     axes = FALSE,
     type = 'l',
     ylim = c(-100, 350),
     xlab = NA,
     ylab = NA,
     lwd = 2
     )
dev.off()

CairoPNG('../images/utterance_energy_1.png', 2000, 1000, bg = 'transparent')
plot(timestamps / 1000,
     10^((audio[,'energy_dB'] - 46)/10) + 100,
     axes = FALSE,
     type = 'l',
     ylim = c(-100, 350),
     xlab = NA,
     ylab = NA,
     lwd = 2
     )
for (i in 1:nrow(transcript)){
  lines(rep(transcript$startTime[i], 2),
        y = c(75, 350),
        col = 'gray50'
        )
  lines(rep(transcript$endTime[i], 2),
        y = c(75, 350),
        col = 'gray50'
        )
    lines(c(transcript$startTime[i], transcript$endTime[i]),
        y = rep(75, 2),
        col = 'gray50'
        )
}
labels.y <- 50 - c(0:2, 0:4, 0:4, 0, 0, 0:3, 0:2, 0:2, rep(0, 4)) * 15
for (i in 1:nrow(transcript)){
  lines(rep((transcript$startTime[i] + transcript$endTime[i]) / 2, 2),
        y = c(labels.y[i], 75),
        col = 'gray50'
        )
}
text((transcript$startTime + transcript$endTime) / 2,
     y = labels.y,
     labels = transcript$word,
     adj = c(1, .5),
     col = 'gray30',
     srt = 45,
     cex = 3
     )
dev.off()
