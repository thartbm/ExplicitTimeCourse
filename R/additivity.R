
# ADDITIVITY DATA SET -----

# Processing Raw Data -----

getParticipantData <- function(group, participant) {
  
  # cat('getParticipantData()\n')
  
  # home-target distance in pixels: 433.548
  
  #cat(sprintf('data/%s/%s_p%03d.csv\n', group, group, participant))
  
  rawparticipantdf <- read.csv(sprintf('data/additivity/%s/%s_p%03d.csv', group, group, participant), stringsAsFactors = FALSE)  
  
  # booleans need to be logical variables:
  rawparticipantdf$showcursor_bool <- as.logical(rawparticipantdf$showcursor_bool)
  rawparticipantdf$doaiming_bool <- as.logical(rawparticipantdf$doaiming_bool)
  
  # replace position columns names: all position columns must end in "_pix"
  posvarnames <- c('cursorx','cursory','targetx','targety','mousex','mousey')
  for (pvn in posvarnames) {
    names(rawparticipantdf)[which(names(rawparticipantdf) == pvn)] <- sprintf('%s_pix',pvn)
  }
  
  # data frame for trimmed reach data:
  ppdf <- NA
  
  # cumulative trials to loop through
  unique.trials <- unique(rawparticipantdf$cutrial_no)
  
  for (trialno in unique.trials) {
    
    trialdf <- rawparticipantdf[which(rawparticipantdf$cutrial_no == trialno),]
    
    if (trialdf$showcursor_bool[1]) {
      targetReached <- 433.548*0.05
      untilHold <- NA
    } else {
      targetReached <- NA
      untilHold <- list('kind'='epoch-distance',
                        'mindist'=433.548*0.5,
                        'epoch'=249,
                        'threshold'=0.01*433.5484)
    }
    
    homeHoldEpoch <- 1999
    if (trialdf$doaiming_bool[1]) {
      homeHoldEpoch <- 499
    }
    
    trimdf <- trimReach(trialdf, 
                        homeStart = 'pr0.05',
                        holdHome=  list( 'epoch' = homeHoldEpoch,
                                         'distance' = 0.03125*433.5484),
                        targetReached = targetReached,
                        untilHold = untilHold,
                        device = 'cursor',
                        homepos = c(0,0),
                        velocity = NA )
    
    
    
    # homeHoldEpoch <- 2000
    # if (trialdf$doaiming_bool[1]) {
    #   homeHoldEpoch <- 500
    # }
    
    if (trialdf$showcursor_bool[1]) {
      
      reachdev <- Reach::getReachAngleAt( trimdf, 
                                          location = 'pr0.33333', 
                                          posunit = 'pix', 
                                          timeunit = 'ms', 
                                          device = 'mouse', 
                                          holdvelocity = NA, 
                                          holdduration = NA)
      
    } else {
      
      reachdev <- Reach::getReachAngleAt( trimdf, 
                                          location = 'endpoint', 
                                          posunit = 'pix', 
                                          timeunit = 'ms', 
                                          device = 'mouse', 
                                          holdvelocity = NA, 
                                          holdduration = NA)
      
    }
    
    # rt <- getReactionTime(trimdf,
    #                       distance=433.548*0.10)
    
    # if (rt <= 0) {rt <- NA}
    
    # reachdev <- cbind(reachdev, matrix(rt - homeHoldEpoch - 1, ncol=1, nrow=1, dimnames=list(c(),c('RT_ms'))) )
    
    infodf <- data.frame('group' = group,
                         'participant' = participant,
                         'trial' = trialno,
                         'rotation' = abs(trimdf$rotation_deg[1]),
                         'doaim' = trimdf$doaiming_bool[1],
                         'aimdev' = trimdf$aimdeviation_deg[1],
                         'cursor' = trimdf$showcursor_bool[1],
                         'strategy' = trimdf$usestrategy_cat[1])
    reachdf <- data.frame(reachdev)
    
    if (is.data.frame(ppdf)) {
      ppdf <- rbind( ppdf, cbind(infodf, reachdf) ) 
    } else {
      ppdf <- cbind(infodf, reachdf)
    }
    
    # get aiming deviation (if any...)
    
    
    # if (is.data.frame(ppdf)) {
    #   # when at least one trial is already added
    #   ppdf <- rbind(ppdf, trialdf)
    # } else {
    #   # when it is still NA
    #   ppdf <- trialdf
    # }
  }
  
  
  return(ppdf)
  
}

processData <- function() {
  
  # cat('processData()\n')
  # for (group in c('control', 'instructed', 'aiming')) {
  for (group in c('aiming')) {
    
    cat(sprintf('%s\n',toupper(group)))
    
    groupdf <- NA
    
    participants <- c(1:24)
    # if (group == 'aiming') {
    #   participants <- c(1:23)
    # }
    
    for (participant in participants) {
      
      cat(sprintf('%d... ',participant))
      
      ppdf <- getParticipantData(group=group, participant=participant)
      
      if (is.data.frame(groupdf)) {
        groupdf <- rbind(groupdf, ppdf)
      } else {
        groupdf <- ppdf
      }
      
    }
    cat('\n')
    write.csv(groupdf, sprintf('data/additivity/%s.csv',group), quote=F, row.names=F)
    
  }
  
}

#' @title Trim a data frame describing a reach to the interesting part. 
#' @param trialdf Data frame representing the reach with variables in columns
#' (named consistent with settings below) and samples in rows.
#' @param homeStart If the participant has to get to the home position before
#' starting the out-and-back reach, this part could be trimmed. Set `homeStart`
#' to a numeric value expressing how close the `device` has to be to the home
#' position, specified in the same unit as `posunit`. The start of the trial
#' will be trimmed (not returned) up to when the device is that close to the 
#' start/home position. By default this is NA, so that this part is not 
#' trimmed. If this is a character variable, starting with "pr" and ending with
#' numbers, those numbers should indicate a proportion of the home-to-target
#' distance to use as cutoff value for trimming the first part of the reach.
#' @param targetReached If the return movement is represented in the data, this
#' may have to be trimmed as well. This parameter sets the *distance* at which 
#' the target is considered reached, and data after this point is trimmed.
#' The target position should be in columns named "targetx_[posunit]" and 
#' "targety_[posunit]". This argument is a numeric variable given in the 
#' position unit specified later on.
#' @param velocity Very slow movement is usually not diagnostic. _After_ the
#' other parts of the data are trimmed, the instantaneous velocity is used as
#' a cut-off criterion: the first part of the reach that is under the velocity
#' criterion as a fraction of the maximum velocity in the whole reach, is 
#' trimmed. And either the final part that is below the velocity criterion is
#' trimmed, or everything after the first dip below the velocity criterion, 
#' depending on the `firstMove` parameter. Set to `NA` for no velocity 
#' criterion. By default this is set conservatively to 0.05.
#' 
#' May give unexpected results if used with `untilHold`.
#' @param firstMove Only used if the `velocity` parameter is not `NA`. If set 
#' to TRUE, the first part of the trajectory up to where it dips below the 
#' velocity criterion is kept (the rest is trimmed). If FALSE, only the final
#' part of the trajectory that goes below the velocity criterion is trimmed.
#' 
#' May give unexpected results if used in combination with `untilHold`.
#' @param holdHome Not used if set to `NA` (default). Otherwise, this should
#' be a list with two named entries:
#' 
#' "distance": numeric, the maximum distance the device (see below) can be from
#' the home position to count as a hold, set in position units
#' 
#' "epoch": numeric, the amount of time the device has to be closer than the
#' distance criterion from the home position, for the hold to be completed, set
#' in time units as described below
#' 
#' May give unexpected results if used with any velocity criterion or with
#' homeStart.
#' @param untilHold Not used if set to `NA` (default). Otherwise, this should
#' be a list with four named entries:
#' 
#' "kind": character setting one of (currently) two ways to determine a hold,
#' can be one of "sample-velocity" or "epoch-distance". When it is
#' "sample-velocity", a sequence of samples spanning the hold epoch all should
#' have velocity below the threshold value. When it is "epoch-distance" the 
#' total distance moved during the epoch should be below the threshold value.
#' 
#' "mindist": numeric: minimum distance from home that the hold has to occur 
#' at, given in position units as set below
#' 
#' "threshold": numeric setting maximum velocity or distance in position and
#' time units as set below
#'   
#' "epoch": numeric duration of the hold in time units specified below
#' 
#' All data _after_ the hold is trimmed, but the hold itself is not.
#' 
#' May give unexpected results if used in combination with `firstMove` or any
#' `velocity` criterion.
#' @param device The position columns to use are given by "[device]x_[posunit]"
#' in the `trialdf`, and similar for y. Can be something like 'hand', 'cursor',
#' 'mouse', 'stylus' or 'robot'.
#' @param posunit The unit used for the x and y position data. Could be "pix"
#' or "cm", or whatever is used in the data. Default: "pix".
#' @param timeunit The unit used for the time stamps of each sample. The column
#' names is "time_[timeunit]". Default: "ms"
#' @param homepos The coordinates of the home position. Default is (0,0).
#' @return Data frame describing the reach, minus the trimmed parts.
#' @description
#' ?
#' @details
#' ?
#' @examples
#' ?
#' @export
trimReach <- function(trialdf, homeStart=NA, targetReached=NA, velocity=0.05, firstMove=FALSE, holdHome=NA, untilHold=NA, device='hand', posunit='pix', timeunit='ms', homepos=c(0,0)) {
  
  # cat('trimReach()\n')
  targetposition <- as.numeric( trialdf[ 1, c( sprintf('targetx_%s', posunit ), sprintf( 'targety_%s', posunit ) ) ] )
  targetposition <- targetposition - homepos
  targetdistance <- sqrt( sum( targetposition^2 ) )
  
  nsamples <- dim(trialdf)[1]
  #cat(sprintf('** start with %d samples\n',nsamples))
  # cat('-----\n')
  
  if (!is.na(homeStart)) {
    
    # we need the device position, relative to the home position
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    
    if (is.numeric(homeStart)) {
      cutoff <- homeStart
    }
    
    if (is.character((homeStart))) {
      
      # cutoff at a percentage from home to target in whatever unit is used
      if (substring(homeStart,1,2) == 'pr') {
        
        cutoff <- as.numeric(substring(homeStart, 3))
        cutoff <- cutoff * targetdistance
        
      }
      
    }
    
    # get the distance from home:
    devicedist <- sqrt(x^2 + y^2)
    
    if (devicedist[1] > cutoff) {
      
      # cat('first sample too far from home\n')
      
      # find the first sample, where device is closer to home than the cutoff:
      if (any(devicedist < cutoff)) {
        rown <- max(1, min(which(devicedist < cutoff))-1) # why the minus one?
        trialdf <- trialdf[c(rown:dim(trialdf)[1]),]
      }
      
    }
    
    # if (dim(trialdf)[1]<nsamples) {
    #   newsamples <- dim(trialdf)[1]
    #   cat(sprintf('homeStart cuts %d samples\n', nsamples-newsamples))
    # }
    
  }
  
  if (!is.na(targetReached) && is.numeric(targetReached)) {
    
    # we need the trajectroy and device position, relative to the home position
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    targetx <- trialdf[1,sprintf('targetx_%s',posunit)] - homepos[1]
    targety <- trialdf[1,sprintf('targety_%s',posunit)] - homepos[1]
    
    # distance to target for every sample:
    dist <- sqrt((x - targetx)^2 + (y - targety)^2)
    
    crit <- which(dist < targetReached)
    # we only trim if the target is actually reached:
    if (length(crit) > 0) {
      trialdf <- trialdf[c(1:crit[1]),]
    }
    
  }
  
  # print(holdHome)
  # print(!is.na(holdHome))
  # print(is.list(holdHome))
  # print(length(holdHome) == 2)
  
  if (all(c( !is.na(holdHome), is.list(holdHome), length(holdHome) == 2))) {
    epoch <- holdHome$epoch
    distance <- holdHome$distance
    
    # first we get the necessary variables from the data frame:
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    sample_time <- trialdf[,sprintf('time_%s',timeunit)]
    
    # only look within distance:
    didx <- which(sqrt(x^2 + y^2) < distance & sample_time >= epoch)
    
    for (sample.idx in didx) {
      tidx <- which((sample_time[c(1:sample.idx)]-sample_time[sample.idx]) > (-epoch))
      
      if (all(sqrt(x[tidx]^2 + y[tidx]^2) < distance)) {
        # the trial now includes the hold period:
        trialdf <- trialdf[c(min(tidx):dim(trialdf)[1]),]
        break() # break out of the for-loop
      }
    }
    
  }
  
  if (all(c(!is.na(untilHold), is.list(untilHold), length(untilHold) == 4))) {
    
    # here we use sample-to-sample velocity as used during the experiment
    # (so no smoothed / splined trajectory)
    
    # first we get the necessary variables from the data frame:
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    sample_time <- trialdf[,sprintf('time_%s',timeunit)]
    
    if (untilHold$kind == 'sample-velocity') {
      # calculate instantaneous velocity:
      velocity <- c(0,sqrt(diff(x)^2 + diff(y)^2) / diff(sample_time))
      print(velocity)
      # which samples are below the velocity criterion:
      belowcriterion <- which(velocity < untilHold$threshold)
      
      # this might be helpful for non-averaged hold criterion algorithms:
      bc_runs <- rle(belowcriterion) # no idea how to continue...
      
      # STUFF: NOT COMPLETE!
      cat('sample-velocity method of determining a hold is not complete:\nnot trimming\n')
      
    }
    
    if (untilHold$kind == 'epoch-distance') {
      
      # only look beyond mindist:
      didx <- which(sqrt(x^2 + y^2) > untilHold$mindist)
      
      for (sample.idx in didx) {
        tidx <- which((sample_time[c(1:sample.idx)]-sample_time[sample.idx]) > (-untilHold$epoch))
        # print(sum(sqrt(diff(x[tidx])^2 + diff(y[tidx])^2)))
        if (sum(sqrt(diff(x[tidx])^2 + diff(y[tidx])^2)) < untilHold$threshold) {
          # the trial now includes the hold period:
          trialdf <- trialdf[c(1:max(tidx)),]
          break() # break out of the for-loop
        }
      }
      
    }
    
  }
  
  if (!is.na(velocity)) {
    
    # here we use a spline smoothed velocity signal:
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    sample_time <- trialdf[,sprintf('time_%s',timeunit)]
    SplVel <- getSplinedVelocity(x=x, y=y, t=sample_time)
    velocity <- SplVel$velocity
    spline_time <- SplVel$time
    
    # determine numeric velocity threshold
    
    # see which samples are below this threshold
    if (!is.na(velocity)) {
      if (is.logical(firstMove) && firstMove) {
        
        # trim after first dip below threshold (after first going above it)
        
      } else {
        
        # trim after last dip below threshold (after first going above it) 
        
      }
      cat('velocity trimming is not completed\nnot trimming\n')
    }
    
  }
  
  return(trialdf)
  
}

# getReactionTime <- function(trialdf, distance) {
#   cat('getReactionTime()\n')
#   X <- trialdf$cursorx_pix
#   Y <- trialdf$cursory_pix
#   t <- trialdf$time_ms
#   t <- t - t[1]
#   
#   idx <- which(sqrt(X^2 + Y^2) > distance)[1]
#   return(t[idx])
#   
# }

# Secondary Processing -----

# 
# baseline <- function(reachvector,blidx) {
#   cat('baseline()\n')
#   return(reachvector - mean(reachvector[blidx], na.rm=TRUE))
#   
# }
# 
# removeOutliers <- function(reachvector,sds=3) {
#   cat('removeOutliers()\n')
#   mu <- mean(reachvector, na.rm=TRUE)
#   sigma <- sd(reachvector, na.rm=TRUE)
#   
#   reachvector[ which(abs(reachvector-mu) > (sigma*sds)) ] <- NA
#   
#   return(reachvector)
#   
# }
# 
# getTrainingReaches <- function(df, depvar='reachdeviation_deg') {
#   cat('getTrainingReaches()\n')
#   # retain only the lines with training reaches:
#   df <- df[which(df$cursor == TRUE),]
#   
#   # get the number of participants
#   participants <- unique(df$participant)
#   N <- length(participants)
#   
#   # -- trial-by-trial data --
#   
#   # put reaches in matrix:
#   reachmat <- matrix(data=df[,depvar], ncol=N, nrow=(dim(df)[1]/N), byrow=F)
#   
#   preNAs <- length(which(is.na(reachmat)))
#   
#   # remove exessively large reach deviations
#   reachmat[abs(reachmat) > 60] <- NA
#   
#   post60NAs <- length(which(is.na(reachmat)))
#   
#   # remove outliers:
#   reachmat <- t(apply(reachmat, FUN=removeOutliers, MARGIN=c(1), sds=3))
#   
#   postSDSNAs <- length(which(is.na(reachmat)))
#   
#   cat(sprintf('pre NAs: %d - 60 NAs: %d - sd NAs: %d - (length: %d)\n', preNAs, post60NAs, postSDSNAs, length(reachmat)))
#   
#   NAs <- matrix(is.na(reachmat), nrow=dim(reachmat)[1], ncol=dim(reachmat)[2])
#   pNAs <- colSums(NAs)
#   cat(sprintf('max trials removed: %d - participants with no removals: %d\n', max(pNAs), length(which(pNAs == 0))))
#   
#   # baseline correction:
#   reachmat <- apply(reachmat, FUN=baseline, MARGIN=c(2), blidx=c(17:32))
#   
#   # get descriptive statistics:
#   average <- apply(reachmat, FUN=mean, MARGIN=c(1), na.rm=TRUE)
#   
#   # get 95% confidence intervals:
#   CI <- apply(reachmat, FUN=Reach::getConfidenceInterval, MARGIN=c(1))
#   
#   # make data frame:
#   reachdevs <- data.frame(reachmat)
#   names(reachdevs) <- sprintf('p%03d',participants)
#   
#   # add descriptives:
#   reachdevs$average <- average
#   reachdevs$CI.lo <- CI[1,]
#   reachdevs$CI.hi <- CI[2,]
#   
#   # get columns with other info
#   infocols <- df[which(df$participant == participants[1]),c('trial','rotation')]
#   
#   # -- blocked data --
#   
#   # get a block column (this will end up in the info-columns):
#   df$block <- floor((df$trial-1) / 8) + 1
#   Nblocks <- length(unique(df$block))
#   
#   # use reachmat matrix to get reachmat_b?
#   reacharr <- array(data=c(reachmat), dim=c(8,Nblocks,N))
#   reachmat_b <- apply(reacharr, FUN=mean, MARGIN=c(2,3), na.rm=TRUE)
#   
#   # get descriptive statistics:
#   average_b <- apply(reachmat_b, FUN=mean, MARGIN=c(1), na.rm=TRUE)
#   
#   # get 95% confidence intervals:
#   CI_b <- apply(reachmat_b, FUN=Reach::getConfidenceInterval, MARGIN=c(1))
#   
#   # make data frame:
#   reachdevs_b <- data.frame(reachmat_b)
#   names(reachdevs_b) <- sprintf('p%03d',participants)
#   
#   # add descriptives:
#   reachdevs_b$average <- average_b
#   reachdevs_b$CI.lo <- CI_b[1,]
#   reachdevs_b$CI.hi <- CI_b[2,]
#   
#   # get columns with other info
#   infocols_b <- df[which(df$participant == participants[1] & df$trial %in% seq(1,264,8)),c('block','rotation')]
#   
#   # return everything in PyVMEC-style data frame
#   return(list('trials' = cbind(infocols,   reachdevs),
#               'blocks' = cbind(infocols_b, reachdevs_b)))
#   
# }
# 
# 
# getReactionTimes <- function(df, depvar='reachdeviation_deg') {
#   cat('getReactionTimes()\n')
#   # retain only the lines with training reaches:
#   df <- df[which(df$cursor == TRUE),]
#   
#   # get the number of participants
#   participants <- unique(df$participant)
#   N <- length(participants)
#   
#   # -- trial-by-trial data --
#   
#   # put reaches in matrix:
#   reachmat <- matrix(data=df[,depvar], ncol=N, nrow=(dim(df)[1]/N), byrow=F)
#   
#   # remove exessivelylarge reach deviations
#   reachmat[abs(reachmat) > 60] <- NA
#   
#   # remove outliers:
#   reachmat <- t(apply(reachmat, FUN=removeOutliers, MARGIN=c(1), sds=3))
#   
#   # baseline correction:
#   reachmat <- apply(reachmat, FUN=baseline, MARGIN=c(2), blidx=c(17:32))
#   
#   # get descriptive statistics:
#   average <- apply(reachmat, FUN=mean, MARGIN=c(1), na.rm=TRUE)
#   
#   # get 95% confidence intervals:
#   CI <- apply(reachmat, FUN=Reach::getConfidenceInterval, MARGIN=c(1))
#   
#   # make data frame:
#   reachdevs <- data.frame(reachmat)
#   names(reachdevs) <- sprintf('p%03d',participants)
#   
#   # add descriptives:
#   reachdevs$average <- average
#   reachdevs$CI.lo <- CI[1,]
#   reachdevs$CI.hi <- CI[2,]
#   
#   # get columns with other info
#   infocols <- df[which(df$participant == participants[1]),c('trial','rotation')]
#   
#   # -- blocked data --
#   
#   # # get a block column (this will end up in the info-columns):
#   # df$block <- floor((df$trial-1) / 8) + 1
#   # Nblocks <- length(unique(df$block))
#   # 
#   # # use reachmat matrix to get reachmat_b?
#   # reacharr <- array(data=c(reachmat), dim=c(8,Nblocks,N))
#   # reachmat_b <- apply(reacharr, FUN=mean, MARGIN=c(2,3), na.rm=TRUE)
#   # 
#   # # get descriptive statistics:
#   # average_b <- apply(reachmat_b, FUN=mean, MARGIN=c(1), na.rm=TRUE)
#   # 
#   # # get 95% confidence intervals:
#   # CI_b <- apply(reachmat_b, FUN=Reach::getConfidenceInterval, MARGIN=c(1))
#   # 
#   # # make data frame:
#   # reachdevs_b <- data.frame(reachmat_b)
#   # names(reachdevs_b) <- sprintf('p%03d',participants)
#   # 
#   # # add descriptives:
#   # reachdevs_b$average <- average_b
#   # reachdevs_b$CI.lo <- CI_b[1,]
#   # reachdevs_b$CI.hi <- CI_b[2,]
#   # 
#   # # get columns with other info
#   # infocols_b <- df[which(df$participant == participants[1] & df$trial %in% seq(1,264,8)),c('block','rotation')]
#   
#   # return everything in PyVMEC-style data frame
#   # return(list('trials' = cbind(infocols,   reachdevs),
#   #             'blocks' = cbind(infocols_b, reachdevs_b)))
#   
#   return(list('trials' = cbind(infocols,   reachdevs)))
#   
# }
# 
# saveTrainingdata <- function() {
#   cat('saveTrainingdata()\n')
#   # PyVMEC style output:
#   # for (group in c('control','instructed','aiming')) {
#   for (group in c('aiming')) {
#     
#     df <- read.csv(sprintf('data/additivity/%s.csv',group), stringsAsFactors = FALSE)
#     
#     rdf <- getTrainingReaches(df)
#     
#     write.csv(rdf[['trials']], sprintf('data/additivity/%s-training-trials.csv',group), quote=FALSE, row.names=FALSE)
#     write.csv(rdf[['blocks']], sprintf('data/additivity/%s-training-blocks.csv',group), quote=FALSE, row.names=FALSE)
#     
#   }
#   
#   df <- read.csv('data/aiming.csv', stringsAsFactors = FALSE)
#   
#   adf <- getTrainingReaches(df, depvar='aimdev')
#   
#   write.csv(adf[['trials']], 'data/additivity/aiming-aim-trials.csv', quote=FALSE, row.names=FALSE)
#   write.csv(adf[['blocks']], 'data/additivity/aiming-aim-blocks.csv', quote=FALSE, row.names=FALSE)
#   
#   
# }
# 
# saveNoCursorData <- function() {
#   cat('saveNoCursorData()\n')
#   # for (group in c('control','instructed','aiming')) {
#   for (group in c('aiming')) {
#     
#     # print(group)
#     
#     df <- read.csv(sprintf('data/additivity/%s.csv',group), stringsAsFactors = FALSE)
#     
#     ndf <- getNoCursors(df)
#     
#     #print(ndf)
#     
#     write.csv(ndf[['all']], sprintf('data/additivity/%s-nocursors-all.csv',group), quote=FALSE, row.names=FALSE)
#     write.csv(ndf[['blocks']], sprintf('data/additivity/%s-nocursors-blocks.csv',group), quote=FALSE, row.names=FALSE)
#     write.csv(ndf[['trials']], sprintf('data/additivity/%s_nocursors-trials.csv',group), quote=FALSE, row.names=FALSE)
#     
#   }
#   
# }
# 
# getNoCursors <- function(df,FUN=median) {
#   cat('getNoCursors()\n')
#   df <- df[which(df$cursor == FALSE),]
#   
#   # get a block column... for blocked output
#   df$block <- floor((df$trial-1) / 8) + 1
#   # to counter counterbalancing, we line stuff up by "superblock" though:
#   df$superblock <- floor((df$block) / 2) + 1
#   
#   # first the overall no-cursor stuff:
#   
#   # get the number of participants
#   participants <- unique(df$participant)
#   N <- length(participants)
#   
#   # overall data:
#   avg_long <- aggregate(reachdeviation_deg ~ strategy + participant, data=df, FUN=FUN, na.rm=TRUE)
#   
#   # matrix
#   nc_mat <- matrix(data=avg_long$reachdeviation_deg, ncol=N, nrow=3, byrow=FALSE)
#   
#   # data frame
#   avg_wide <- data.frame(nc_mat)
#   colnames(avg_wide) <- sprintf('p%03d',participants)
#   
#   # get descriptive statistics:
#   average <- apply(nc_mat, FUN=mean, MARGIN=c(1), na.rm=TRUE)
#   
#   # get 95% confidence intervals:
#   CI <- apply(nc_mat, FUN=Reach::getConfidenceInterval, MARGIN=c(1))
#   
#   # add descriptives:
#   avg_wide$average <- average
#   avg_wide$CI.lo <- CI[1,]
#   avg_wide$CI.hi <- CI[2,]
#   
#   # add first column with condition names:
#   avg_wide <- cbind( data.frame( 'condition'=avg_long$strategy[which(avg_long$participant == participants[1])]), avg_wide )
#   
#   
#   
#   # now split by block...
#   
#   # blocked data:
#   avg_long_b <- aggregate(reachdeviation_deg ~ strategy + superblock + participant, data=df, FUN=FUN, na.rm=TRUE)
#   
#   # convert super-block number to iteration:
#   lookup <- data.frame('superblock'=c(3,5,6,13,15,17), 'iteration'=c(1,2,3,1,2,3))
#   avg_long_b$iteration <- lookup$iteration[match(avg_long_b$superblock, lookup$superblock)]
#   
#   # get medians per block:
#   avg_long_b <- aggregate(reachdeviation_deg ~ participant + iteration + strategy, data=avg_long_b, FUN=FUN, na.rm=TRUE)
#   
#   # matrix (keep for descriptives):
#   nc_mat_b <- matrix(data=avg_long_b$reachdeviation_deg, ncol=N, nrow=3*3, byrow=TRUE)
#   
#   # start making data frame:
#   avg_wide_b <- data.frame(nc_mat_b)
#   
#   colnames(avg_wide_b) <- sprintf('p%03d', participants)
#   
#   # descriptives:
#   average_b <- apply(nc_mat_b, FUN=mean, MARGIN=c(1), na.rm=TRUE)
#   
#   # get 95% confidence intervals:
#   CI_b <- apply(nc_mat_b, FUN=Reach::getConfidenceInterval, MARGIN=c(1))
#   
#   # add descriptives:
#   avg_wide_b$average <- average_b
#   avg_wide_b$CI.lo <- CI_b[1,]
#   avg_wide_b$CI.hi <- CI_b[2,]
#   
#   # info columns
#   infocols <- data.frame( 'condition' = rep(unique(avg_long_b$strategy[which(avg_long_b$participant == participants[1])]), each=3),
#                           'iteration' = rep(c(1,2,3), 3) )
#   
#   avg_wide_b <- cbind(infocols, avg_wide_b)
#   
#   return(list('all'    = avg_wide,
#               'blocks' = avg_wide_b))
#   
# }

