

loadTimeCourseData <- function(rotations=c(20,30,40,50,60)) {
  
  df <- NA
  
  for (rotation in rotations) {
    
    rdf <- getRotationTimeCourseData(rotation=rotation)
    rdf$condition <- rotation
    
    if (is.data.frame(df)) {
      df <- rbind(df, rdf)
    } else {
      df <- rdf
    }

  }
  
  return(df)
  
}

getRotationParticipants <- function(rotation) {
  
  path <- sprintf('exp/data/aiming%d/',rotation)
  
  # allcsvfiles <- list.files(path=folder, pattern='*.csv')
  
  allfolders <- list.dirs(path=path,
                          full.names = FALSE)
  
  allfolders <- allfolders[which(nchar(allfolders) > 0)]

  participants <- c()
  goodcsvfiles <- c()
  # for (file in allcsvfiles) {
  for (folder in allfolders) {
    csvfile <- sprintf('%s%s/SUMMARY_aiming%d_%s.csv', path, folder, rotation, folder)
    
    # if (substr(file, 1, 17) == sprintf('SUMMARY_aiming%d_',rotation)) {
    if (file.exists(csvfile)) {
      goodcsvfiles <- c(goodcsvfiles, csvfile)
      participants <- c(participants, folder)
    }
  }
  
  # print(goodcsvfiles)
  # check learners:
  learners <- checkLearners(rotation, goodcsvfiles)
  goodcsvfiles <- goodcsvfiles[which(learners)]
  participants <- participants[which(learners)]
  
  # check informed consent? probably shouldn't be in the data set to begin with
  
  # print(participants)
  return(participants)
  
}

checkLearners <- function(rotation, goodcsvfiles) {
  
  folder <- sprintf('exp/data/aiming%d/',rotation)
  learners <- c() # concatenate TRUE or FALSE
  
  for (file in goodcsvfiles) {
    # summary <- read.csv(sprintf('%s%s',folder,file), stringsAsFactors = FALSE)
    summary <- read.csv(file, stringsAsFactors = FALSE)
    # print(dim(summary))
    
    # calculate baseline:
    aligned <- summary[which((summary$task_idx == 2 & summary$trial_idx > 16) | (summary$task_idx == 6 & summary$trial_idx > 8)),]
    baseline <- median(aligned$reachdeviation_deg, na.rm=TRUE)
    
    # take the last 16 trials of the rotated phase, and apply baseline:
    rotated <- summary[which(summary$task_idx == 8 & summary$trial_idx > 104),] 
    meandev <- median(rotated$reachdeviation_deg, na.rm=TRUE) - baseline

    # need to know the rotation to decide cutoff and direction of test:
    rotation = rotated$rotation_deg[1]
    # normalize mean reach deviation to (ideally) go positive regardless of direction of rotation:
    meandev <- -1 * sign(rotation) * meandev
    
    # print(meandev)
    
    # compared reach deviation to criterion:
    if (meandev > (abs(rotation)/2)) {
      learners <- c(learners, TRUE)
    } else {
      learners <- c(learners, FALSE)
    }
  }
  
  return(learners)
  
}

getRotationTimeCourseData <- function(rotation) {
  
  participants <- getRotationParticipants(rotation)
  
  folder <- sprintf('exp/data/aiming%d/',rotation)
  
  df <- NA
  
  for (participant in participants) {
    
    pdf <- read.csv(sprintf('%s/%s/SUMMARY_aiming%d_%s.csv',folder,participant,rotation,participant), stringsAsFactors = FALSE)
    pdf$participant <- participant
    
    if (is.data.frame(df)) {
      df <- rbind(df, pdf)
    } else {
      df <- pdf
    }
  }
  
  return(df)

}