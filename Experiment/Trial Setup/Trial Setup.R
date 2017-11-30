library(tidyr) # for unite()
library(magrittr) # for pipe (%>%)
library(plyr)


Tokens <- list(Pitch = c("Hum", "Piano", "Pulse", "Tone"),
  Amp = c("Hum", "Piano", "Pulse", "Tone"),
  Noise = c("Hum", "Piano", "Pulse", "Tone"),
  Shape = c("BK1", "BK2", "BK3", "BK4"),
  Size = c("Circles", "Squares", "Triangles", "Diamonds"),
  Brightness = c("Circles", "Squares", "Triangles", "Diamonds"),
  Color = c("RG", "RY", "YB", "RB"),
  Speed = c("SP1", "SP2", "SP3", "SP4"),
  Affect = c("HS", "SC", "EB", "PD"))

Domains <- names(Tokens)

# example call: createstimuli(c("Pitch", "Shape", "Affect"))
# for repeated calls per condition and writing to file see at the bottom
createstimuli <- function(dimensions) {
  Inducers <- expand.grid(Inducer = dimensions, Concurrent = Domains)
  Inducers <- subset(Inducers,
    as.character(Inducer) != as.character(Concurrent), na.rm=TRUE)
  Concurrents <- expand.grid(Inducer = Domains, Concurrent = dimensions)
  Concurrents <- subset(Concurrents,
    as.character(Inducer) != as.character(Concurrent), na.rm=TRUE)

  ############################################################################


  # For each condition, the Focal Features will have *all* of their tokens used, counterbalanced across trials.
  # So, when Pitch is focal for example, participants will hear an equal number of Sine, Sung, Piano, and Whistled trials
  # The non-focal features will be chosen randomly such that each participant uses a random 1 kind of token for all trials 
  # e.g. all amplitude trials use sine

  # require(plyr) #this bit of code puts in the TYPE for inducers and concurrents
  # TrialTypes$IndType <- mapvalues(TrialTypes$Inducer, 
  #                              from=c("Pitch", "Amplitude", "Noise", "Size", "Shape", "Brightness", "Affect", "PitchAmplitude"), 
  #                                to=c("Sound", "Sound", "Sound", "Object", "Object", "Object", "Face", "Sound"))
  LeftRight <- c("L", "H")
  LeftRight2 <- c("L", "H","L", "H", "L", "H", "L", "H", "L", "H", "L", "H", "L", "H","L", "H","L", "H","L", "H","L", "H","L", "H")

  Inducers2 <- Inducers
  Inducers$InducerToken <- sapply(Inducers$Inducer, paste, "1", sep = "")  # these bits of code just create another column that appends
  Inducers2$InducerToken <- sapply(Inducers2$Inducer, paste, "2", sep = "") # a number to the inducers for choosing tokens
  Inducers$ConcurrentToken <- Inducers$Concurrent
  Inducers2$ConcurrentToken <- Inducers2$Concurrent

  Inducers <- Inducers[order(Inducers$Inducer),]
  Inducers$InducerLoc <- sample(LeftRight)
  Inducers2 <- Inducers2[order(Inducers2$Inducer),]
  Inducers2$InducerLoc <- sample(LeftRight)

  Inducers$ConcurrentLoc <- sample(LeftRight2)
  Inducers2$ConcurrentLoc <- sample(LeftRight2)

  #############

  Concurrents2 <- Concurrents
  Concurrents$InducerToken <- Concurrents$Inducer
  Concurrents2$InducerToken <- Concurrents2$Inducer
  Concurrents$ConcurrentToken <- sapply(Concurrents$Concurrent, paste, "3", sep = "")
  Concurrents2$ConcurrentToken <- sapply(Concurrents2$Concurrent, paste, "4", sep = "")

  Concurrents <- Concurrents[order(Concurrents$Concurrent),]
  Concurrents$ConcurrentLoc <- sample(LeftRight)
  Concurrents2 <- Concurrents2[order(Concurrents2$Concurrent),]
  Concurrents2$ConcurrentLoc <- sample(LeftRight)

  Concurrents$InducerLoc <- sample(LeftRight2)
  Concurrents2$InducerLoc <- sample(LeftRight2)

  Trials <- rbind(Inducers, Inducers2, Concurrents, Concurrents2)

  # c() around it to turn matrix into a flat list
  FocalTokens <- c(sapply(dimensions, function(d) sample(Tokens[[d]])))
  NonFocalTokens <-c(sapply(Domains, function(d) sample(Tokens[[d]], 1)))
  FocalColumns <- do.call(function(Var1, Var2) paste(Var2, Var1, sep=""),
    expand.grid(1:4, dimensions))
  AllColumns <- c(FocalColumns, Domains)
  print(AllColumns)
  print(c(FocalTokens, NonFocalTokens))

  Trials$InducerToken <- mapvalues(Trials$InducerToken,
                        from = AllColumns, to = c(FocalTokens, NonFocalTokens))

  Trials$ConcurrentToken <- mapvalues(Trials$ConcurrentToken,
                        from = AllColumns, to = c(FocalTokens, NonFocalTokens))

  Trials$InducerLeft <- Trials$InducerLoc
  Trials$InducerRight <- mapvalues(Trials$InducerLeft, 
                                      from=c("L", "H"), 
                                      to= c("H", "L") ) 

  Trials$ConcurrentLeft <- Trials$ConcurrentLoc
  Trials$ConcurrentRight <- mapvalues(Trials$ConcurrentLeft, 
                                   from=c("L", "H"), 
                                   to= c("H", "L") ) 

  Trials <- Trials %>% 
    unite(InducerL, Inducer, InducerToken, InducerLeft, sep = "-", remove = FALSE) # Joins the columns into a single column
  Trials <- Trials %>%   
    unite(InducerR, Inducer, InducerToken, InducerRight, sep = "-", remove = FALSE)
  Trials <- Trials %>%   
    unite(ConcurrentL, Concurrent, ConcurrentToken, ConcurrentLeft, sep = "-", remove = FALSE)
  Trials <- Trials %>% 
    unite(ConcurrentR, Concurrent, ConcurrentToken, ConcurrentRight, sep = "-", remove = FALSE)

  # shuffle order
  Trials <- Trials[sample(nrow(Trials)), ]
  Trials$TrialNum <- seq(nrow(Trials))
  Trials$Focal1 <- dimensions[1]
  Trials$Focal2 <- dimensions[2]
  Trials$Focal3 <- dimensions[3]

  ########################
  # Finally just a bit of cleanup

  return(subset(Trials, select=c("TrialNum", "Focal1", "Focal2", "Focal3", "Inducer", "Concurrent", "InducerL", "InducerR", "ConcurrentL", "ConcurrentR")))
}

# three variables are passed to this function:
# the first one is a vector of 3 strings specifying the inducing dimensions
# the second is the number of participants to create stimuli for
# the third is an optional string that gives the 'name' for the condition.
# if not supplied explicitly, it is set to a concatenation of the three dimension names that are passed as the first argument
writestimuli <- function(dimensions, n, conditionname=paste(dimensions, collapse="-")) {
  # do n times, storing the current iteration number in 'i'
  for (i in 1:n) {
    # create stimuli data frame. 'dimensions' is already in the right format so
    # just pass it on directly
    stimuli <- createstimuli(dimensions)
    # add participant 'Id' column (concatenation of the conditionname and 'i')
    stimuli <- cbind(Id=paste(conditionname, i, sep="-"), stimuli)
    # write resulting data frame to a file
    write.table(stimuli,
      paste("Stimuli-", conditionname, "-", i, ".csv", sep=""),
      sep = "\t", row.names=FALSE)
  }
}

Conditions <- list(c("Pitch", "Shape", "Affect"),
                   c("Amp", "Size", "Speed"),
                   c("Noise", "Brightness", "Color"),
                   c("Pitch", "Size", "Color"),
                   c("Brightness", "Amp", "Affect"),
                   c("Noise", "Shape", "Speed"))

# Conditions is a list of 3-element vectors, so looping over it like this will
# cause writestimuli() to be called 6 times, once each for every set of
# dimension-triplets above
setwd("F:/Google Drive/GitHub Repos/Crossmodality-Toolkit/Stimuli/Stimlists/")

for (condition in Conditions) {
  writestimuli(condition, 25) # number of participants
}
