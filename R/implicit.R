
library('Reach')


`%notin%` <- Negate(`%in%`)

# basic pre-processing -----


extractAngularDeviations <- function() {
  
  # this is a fraction of the home-target distance
  # 20% of ~8.83 cm is ~1.76 cm (or 1.8 cm)
  distance <- 0.2
  
  cat('-- extracting training reach deviations:\n')
  extractReachDeviations(distance=distance)
  cat('-- extracting no-cursor reach deviations:\n')
  extractNoCursorDeviations(distance=distance)
  cat('-- extracting aiming deviations:\n')
  extractAimingDeviations()
  
  cat('-- removing non-learners:\n')
  removeNonLearners()
  
  # cat('-- combining control groups:\n')
  # combineControls()
  # 
  # cat('-- calculating subtractive implicit estimate:\n')
  # subtractionImplicit()
  
}

extractReachDeviations <- function(distance=0.2) {
  
  info <- read.csv('data/implicit/demographics.csv', stringsAsFactors = FALSE)
  
  for (exp in unique(info$exp)) {
    
    for (condition in unique(info$condition_label[which(info$exp == exp)])) {
      
      cond_df <- NA
      
      cat(sprintf('reaches: exp %d, %s\n',exp,condition))
      
      for (participant in info$participant[which(info$exp == exp & info$condition_label == condition)]) {
        
        infilename <- sprintf('data/implicit/exp%d/%s/%s.csv', exp, condition, participant)
        ppdf <- read.csv(infilename, stringsAsFactors = FALSE)
        
        # get rid of practice phase:
        ppdf <- ppdf[which(ppdf$phase %notin% c('practice','counter','errorclamp')),]
        
        # only keep feedback reaches:
        ppdf <- ppdf[which(ppdf$trialtype != 0),]
        
        reachdeviation_deg <- c()
        for (trialno in c(1:dim(ppdf)[1])) {
          reachdeviation_deg <- c(reachdeviation_deg, getReachDev(target=ppdf$target[trialno],
                                                                  X=convertCellToNumVector(ppdf$mousex_rel[trialno]),
                                                                  Y=convertCellToNumVector(ppdf$mousey_rel[trialno]),
                                                                  distance=distance)) 
        }
        
        
        pdf <- data.frame(exp                = rep(exp,dim(ppdf)[1]),
                          condition          = rep(condition,dim(ppdf)[1]),
                          participant        = rep(participant,dim(ppdf)[1]),
                          trialno            = c(1:dim(ppdf)[1]),
                          target             = ppdf$target,
                          rotation_deg       = ppdf$rotation_deg,
                          reachdeviation_deg = reachdeviation_deg,
                          points             = ppdf$points,
                          phase              = ppdf$phase)
        
        if (is.data.frame(cond_df)) {
          cond_df <- rbind(cond_df, pdf)
        } else {
          cond_df <- pdf
        }
        
      }
      
      outfilename <- sprintf('data/implicit/exp%d/%s_reaches.csv', exp, condition)
      write.csv(cond_df, outfilename, row.names=FALSE, quote=TRUE)
      
    }
  }
  
}

extractNoCursorDeviations <- function(distance=0.2) {
  
  info <- read.csv('data/implicit/demographics.csv', stringsAsFactors = FALSE)
  
  for (exp in unique(info$exp)) {
    
    for (condition in unique(info$condition_label[which(info$exp == exp)])) {
      
      cat(sprintf('no-cursors: exp %d, %s\n',exp,condition))
      
      cond_df <- NA
      
      for (participant in info$participant[which(info$exp == exp & info$condition_label == condition)]) {
        
        infilename <- sprintf('data/implicit/exp%d/%s/%s.csv', exp, condition, participant)
        ppdf <- read.csv(infilename, stringsAsFactors = FALSE)
        
        # get rid of practice phase:
        ppdf <- ppdf[which(ppdf$phase %notin% c('practice','counter','errorclamp')),]
        
        # only keep feedback reaches:
        ppdf <- ppdf[which(ppdf$trialtype == 0),]
        
        reachdeviation_deg <- c()
        for (trialno in c(1:dim(ppdf)[1])) {
          reachdeviation_deg <- c(reachdeviation_deg, getReachDev(target=ppdf$target[trialno],
                                                                  X=convertCellToNumVector(ppdf$mousex_rel[trialno]),
                                                                  Y=convertCellToNumVector(ppdf$mousey_rel[trialno]),
                                                                  distance=distance)) 
        }
        
        
        pdf <- data.frame(exp                = rep(exp,dim(ppdf)[1]),
                          condition          = rep(condition,dim(ppdf)[1]),
                          participant        = rep(participant,dim(ppdf)[1]),
                          trialno            = c(1:dim(ppdf)[1]),
                          target             = ppdf$target,
                          rotation_deg       = ppdf$rotation_deg,
                          reachdeviation_deg = reachdeviation_deg,
                          phase              = ppdf$phase)
        
        if (is.data.frame(cond_df)) {
          cond_df <- rbind(cond_df, pdf)
        } else {
          cond_df <- pdf
        }
        
      }
      
      outfilename <- sprintf('data/implicit/exp%d/%s_nocursors.csv', exp, condition)
      write.csv(cond_df, outfilename, row.names=FALSE, quote=TRUE)
      
    }
  }
  
}

extractAimingDeviations <- function() {
  
  info <- read.csv('data/implicit/demographics.csv', stringsAsFactors = FALSE)
  
  for (exp in unique(info$exp)) {
    
    for (condition in unique(info$condition_label[which(info$exp == exp)])) {
      
      cat(sprintf('aiming: exp %d, %s\n',exp,condition))
      
      cond_df <- NA
      
      for (participant in info$participant[which(info$exp == exp & info$condition_label == condition)]) {
        
        infilename <- sprintf('data/implicit/exp%d/%s/%s.csv', exp, condition, participant)
        ppdf <- read.csv(infilename, stringsAsFactors = FALSE)
        
        # get rid of practice phase:
        ppdf <- ppdf[which(ppdf$phase %notin% c('practice','counter','errorclamp')),]
        
        # only keep feedback reaches:
        ppdf <- ppdf[which(ppdf$trialtype == 1),]
        
        ppdf$trialno <- c(1:dim(ppdf)[1])
        
        ppdf <- ppdf[which(ppdf$aiming_type == 1),]
        
        ppdf$aimingdeviation_deg <- ppdf$aiming_direction - ppdf$aiming_target
        
        # correct wrapped-around responses:
        while (any(ppdf$aimingdeviation_deg > 180)) {
          ppdf$aimingdeviation_deg[which(ppdf$aimingdeviation_deg > 180)] <- ppdf$aimingdeviation_deg[which(ppdf$aimingdeviation_deg > 180)] - 360
        }
        while (any(ppdf$aimingdeviation_deg < -180)) {
          ppdf$aimingdeviation_deg[which(ppdf$aimingdeviation_deg < -180)] <- ppdf$aimingdeviation_deg[which(ppdf$aimingdeviation_deg < -180)] + 360
        }
        
        # remove responses in the wrong direction (arrow starts at 15, but it should be a negative number)
        ppdf$aimingdeviation_deg[which(ppdf$aimingdeviation_deg > 10)] <- NA
        # remove extreme outliers (twice the maximum reasonable strategy):
        ppdf$aimingdeviation_deg[which(ppdf$aimingdeviation_deg < -120)] <- NA
        # maybe the previous check should be done on absolute aiming deviations?
        
        pdf <- data.frame(exp                 = rep(exp,dim(ppdf)[1]),
                          condition           = rep(condition,dim(ppdf)[1]),
                          participant         = rep(participant,dim(ppdf)[1]),
                          trialno             = ppdf$trialno,
                          aiming_target       = ppdf$aiming_target,
                          rotation_deg        = ppdf$rotation_deg,
                          aimingdeviation_deg = ppdf$aimingdeviation_deg,
                          arrow_offset        = ppdf$arrow_offset,
                          phase               = ppdf$phase)
        
        if (is.data.frame(cond_df)) {
          cond_df <- rbind(cond_df, pdf)
        } else {
          cond_df <- pdf
        }
        
      }
      
      outfilename <- sprintf('data/implicit/exp%d/%s_aiming.csv', exp, condition)
      write.csv(cond_df, outfilename, row.names=FALSE, quote=TRUE)
      
    }
  }
  
}

# subtractionImplicit <- function() {
#   
#   # only the aiming group!
#   
#   reaches <- read.csv('data/implicit/exp4/aiming_reaches.csv', stringsAsFactors = FALSE)
#   aiming  <- read.csv('data/implicit/exp4/aiming_aiming.csv',  stringsAsFactors = FALSE)
#   
#   participants <- unique(reaches$participant)
#   
#   subtraction <- NA
#   
#   for (pp in participants) {
#     
#     pr <- reaches[which(reaches$participant == pp),]
#     pa <- aiming[ which(aiming$participant  == pp),]
#     
#     pd <- merge(pr, pa)
#     
#     pd$subtraction <- (pd$reachdeviation_deg - pd$aimingdeviation_deg)
#     
#     pd <- pd[,c('exp', 'condition', 'participant', 'trialno', 'rotation_deg', 'phase', 'target', 'subtraction')]
#     pd <- pd[order(pd$trialno),]
#     
#     if (is.data.frame(subtraction)) {
#       subtraction <- rbind(subtraction, pd)
#     } else {
#       subtraction <- pd
#     }
#     
#   }
#   
#   subtraction <- removeOutliers(df=subtraction, depvar='subtraction')
#   subtraction <- baseline(df=subtraction, depvar='subtraction')
#   
#   write.csv(subtraction, 'data/implicit/exp4/aiming_subtraction.csv', quote = F, row.names = F)
#   
# }


# removeNonLearners <- function() {
#   
#   info <- read.csv('data/implicit/demographics.csv', stringsAsFactors = FALSE)
#   
#   info$learner = FALSE
#   
#   totalN <- 0
#   nlearners <- 0
#   
#   for (exp in unique(info$exp)) {
#     
#     for (condition in unique(info$condition_label[which(info$exp == exp)])) {
#       
#       learners <- c()
#       
#       # cat(sprintf('exp %d, %s\n',exp,toupper(condition)))
#       
#       cond_df <- read.csv(sprintf('data/implicit/exp%d/%s_reaches.csv', exp, condition), stringsAsFactors = FALSE)
#       
#       cond_df <- removeOutliers(cond_df, depvar='reachdeviation_deg')
#       cond_df <- baseline(cond_df, depvar='reachdeviation_deg')
#       
#       org_participants <- unique(cond_df$participant)
#       totalN <- totalN + length(org_participants)
#       for (participant in unique(cond_df$participant)) {
#         
#         # print(participant)
#         part_df <- cond_df[which(cond_df$participant == participant),]
#         part_df <- part_df[which(part_df$phase == 'rotation'),]
#         # print(dim(part_df))
#         ntrials <- dim(part_df)[1]
#         stst <- mean(part_df$reachdeviation_deg[c((ntrials-19):ntrials)], na.rm=TRUE)
#         rot <- part_df$rotation_deg[1]
#         if (rot < 0) {rot <- -1 * rot} else {stst <- -1 * stst}
#         # print(stst)
#         # print(rot)
#         if (stst > (rot/2)) {
#           learners <- c( learners, participant)
#           info$learner[which(info$participant == participant)] <- TRUE
#         }
#       }
#       
#       cat(sprintf('removed %d non-learners from %d %s participants\n',(length(org_participants)-length(learners)),length(org_participants),toupper(condition)))
#       nlearners <- nlearners + length(learners)
#       cond_df <- cond_df[which(cond_df$participant %in% learners),]
#       outfilename <- sprintf('data/implicit/exp%d/%s_reaches.csv', exp, condition)
#       write.csv(cond_df, outfilename, row.names=FALSE, quote=TRUE)
#       
#       # remove from no-cursor and aiming data as well:
#       for (datatype in c('nocursors', 'aiming')) {
#         filename <- sprintf('data/implicit/exp%d/%s_%s.csv', exp, condition, datatype)
#         cond_df <- read.csv(filename, stringsAsFactors = FALSE)
#         
#         cond_df <- removeOutliers(cond_df, depvar=list('nocursors'='reachdeviation_deg', 'aiming'='aimingdeviation_deg')[[datatype]])
#         if (datatype == 'nocursors') {
#           cond_df <- baseline(cond_df, depvar='reachdeviation_deg')
#         }
#         
#         cond_df <- cond_df[which(cond_df$participant %in% learners),]
#         write.csv(cond_df, filename, row.names=FALSE, quote=TRUE)
#       }
#       
#     }
#     
#   }
#   
#   info <- info[order(info$condition_label),]
#   info <- info[order(info$exp),]
#   
#   write.csv(info, 'data/implicit/demographics.csv', quote=TRUE, row.names = FALSE)
#   cat(sprintf('\nkept %d learners out of %d participants\n',nlearners, totalN))
#   
# }


# custom version, using selected learners / non-learners from previous paper
removeNonLearners <- function() {
  
  cat(sprintf('exp %d, %s\n',4,'aiming'))
  
  info <- read.csv('data/implicit/demographics.csv', stringsAsFactors = FALSE)
  
  learners <- info$participant[which(info$learner == TRUE)]
  
  for (file in c('aiming_aiming.csv', 'aiming_reaches.csv', 'aiming_nocursors.csv')) {
    
    filename <- sprintf('data/implicit/exp4/%s',file)
    df <- read.csv(filename, stringsAsFactors = FALSE)
    df <- df[which(df$participant %in% learners),]
    write.csv(df, filename, quote=F, row.names=F)
    
  }
  
}


# combineControls <- function() {
#   
#   for (type in c('reaches', 'nocursors', 'aiming')) {
#     
#     spring <- read.csv(sprintf('data/implicit/exp1/45deg_distance_%s.csv', 
#                                type), 
#                        stringsAsFactors = FALSE)
#     spring <- spring[which(spring$phase %in% c('baseline','rotation')),] # don't need the washout
#     spring$semester <- 'spring'
#     
#     fall   <- read.csv(sprintf('data/implicit/exp2/control_%s.csv', 
#                                type), 
#                        stringsAsFactors = FALSE)
#     fall$semester <- 'fall'
#     
#     control <- rbind(spring, fall)
#     
#     write.csv(control, 
#               sprintf('data/implicit/exp2/control_%s.csv', type), 
#               row.names = FALSE, 
#               quote = TRUE)
#     
#   }
#   
# }

# utility functions -----

convertCellToNumVector <- function(v) {
  
  # remove opening square bracket:
  v <- gsub('\\[', replacement='', x=v)
  # remove closing square bracket:
  v <- gsub(']', replacement='', x=v)
  # split by commas:
  v <- strsplit(v, ',')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  
  return(v)
  
}


getReachDev <- function(target, X, Y, distance=0.2) {
  
  theta <- -1*(target/180)*pi
  R <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2)
  relx <- convertCellToNumVector(X)
  rely <- convertCellToNumVector(Y)
  d <- sqrt(relx^2 + rely^2)
  idx <- which(d > distance)[1]
  xy <- matrix(c(relx[idx],rely[idx]), nrow=2, ncol=1)
  npos <- R %*% xy
  
  reach_dev <- (atan2(npos[2],npos[1])/pi)*180
  
  return(reach_dev)
  
}

