
# old data plots -----

oldTimeCoursePlots <- function() {
  
  # showing the same thing for two data sets:
  # - from Urooj's aiming group in the aiming effects / additivity project
  # - from Sebastian's continuous aiming condition
  
  # we want to show individual timecourses
  
  layout(mat=matrix(c(1:6),nrow=2,ncol=3,byrow=TRUE),width=c(3,0.5,0.5))
  
  
  # ADDITIVITY
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(0,250),ylim=c(-30,30),
       bty='n',ax=F)
  
  title(main="30° rotation, radial targets ('t Hart et al., 2024)")
  title(xlab='trial')
  title(ylab='aiming response [°]')
  
  aiming <- read.csv('data/additivity/aiming.csv', stringsAsFactors = F)
  
  points(x=aiming$trial, y=aiming$aimdev, col='#FF000009', pch=16)

  lines(x=c(1,89,89,248),
        y=c(0,0,30,30),
        col='#000000',
        lty=2)
  
  axis(side=1,at=c(1,89,248))
  axis(side=2,at=c(-30,-15,0,15,30))
  
  # angles <- (seq(0,315,45) / 180) * pi
  # points(x=(cos(angles)*8)+20,
  #        y=(sin(angles)*8)+15,
  #        col='blue')
  
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(0,1),ylim=c(-30,30),
       bty='n',ax=F)
  
  aim_distr <- aiming[which(aiming$trial %in% c(177:184,209:216,241:248)),]
  # aim_distr <- aiming[which(aiming$trial %in% c(177:184)),]
  
  strategy <- aggregate(aimdev ~ participant, data=aim_distr, FUN=median, na.rm=TRUE)
  
  distribution <- density(x=strategy$aimdev, from=-10, to=30, bw=2.5)
  
  lines(x=distribution$y/(max(distribution$y)),
        y=distribution$x)
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(0,1),ylim=c(-30,30),
       bty='n',ax=F)
  
  # IMPLICIT group
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(0,121),ylim=c(-15,60),
       bty='n',ax=F)
  
  title(main="45° rotation, forward targets (D'Amario et al., preprint)")
  title(xlab='trial')
  title(ylab='aiming response [°]')
  
  implicit <- read.csv('data/implicit/exp4/aiming_aiming.csv', stringsAsFactors = F)
  implicit$aimingdeviation_deg <- -1*implicit$aimingdeviation_deg
  
  points(x=implicit$trialno, y=implicit$aimingdeviation_deg, col='#FF000009', pch=16)
  
  lines(x=c(1,21,21,120),
        y=c(0,0,45,45),
        col='#000000',
        lty=2)
  
  axis(side=1,at=c(1,21,120))
  axis(side=2,at=c(-15,0,15,30,45,60))
  
  # angles <- (seq(45,135,30) / 180) * pi
  # points(x=(cos(angles)*8)+10,
  #        y=(sin(angles)*8)+25,
  #        col='blue')
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(0,1),ylim=c(-15,60),
       bty='n',ax=F)
  
  aim_distr <- implicit[which(implicit$trialno %in% c(97:120)),]

  strategy <- aggregate(aimingdeviation_deg ~ participant, data=aim_distr, FUN=median, na.rm=TRUE)
  
  distribution <- density(x=strategy$aimingdeviation_deg, from=-15, to=55, bw=2.5)
  
  lines(x=distribution$y/(max(distribution$y)),
        y=distribution$x)
  
  print(range(distribution$x))
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(0,1),ylim=c(-15,60),
       bty='n',ax=F)
   
}

implicitAiming2Dhist <- function() {
  
  layout(mat=matrix(c(1:3),nrow=3,ncol=1,byrow=TRUE))
  
  explicit <- read.csv('data/implicit/exp4/aiming_aiming.csv', stringsAsFactors = F)
  explicit$aimingdeviation_deg <- -1*explicit$aimingdeviation_deg
  implicit <- read.csv('data/implicit/exp4/aiming_nocursors.csv', stringsAsFactors = F)
  implicit$reachdeviation_deg <- -1*implicit$reachdeviation_deg
  adaptation <- read.csv('data/implicit/exp4/aiming_reaches.csv', stringsAsFactors = F)
  adaptation$reachdeviation_deg <- -1*adaptation$reachdeviation_deg
  
  # trials: pre = 13-20, post = 21-52
  
  expl <- explicit[   which(explicit$trialno   %in% c(13:52)), ]
  impl <- implicit[   which(implicit$trialno   %in% c(13:52)), ]
  adpt <- adaptation[ which(adaptation$trialno %in% c(13:52)), ]
  
  expl <- expl[,c('trialno','aimingdeviation_deg')]
  impl <- impl[,c('trialno','reachdeviation_deg')]
  adpt <- adpt[,c('trialno','reachdeviation_deg')]
  
  names(expl) <- c('x','y')
  names(impl) <- c('x','y')
  names(adpt) <- c('x','y')
  
  plot(x=1000,y=-1000,
       main='explicit',xlab='',ylab='',
       xlim=c(13,52),ylim=c(-15,60), 
       ax=F,bty='n')
  
  img_info <- hist2d(x=expl, nbins=NA, edges=list(seq(12.5,52.5,1),seq(-15,60,2.5)))
  img <- img_info$freq2D
  
  img <- log(img + 1)
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        z=img,
        add=TRUE)
  
  lines(x=c(12,20,20,52),
        y=c(0,0,45,45),
        col='#66F',lty=1,lw=2)
  
  axis(side=1, at=c(12, 20, 28, 36, 44, 52), labels=c(-8,0,8,16,24,32))
  axis(side=2, at=seq(-15,60,15))
  
  
  plot(x=1000,y=-1000,
       main='implicit',xlab='',ylab='',
       xlim=c(13,52),ylim=c(-15,60), 
       ax=F,bty='n')
  
  img_info <- hist2d(x=impl, nbins=NA, edges=list(seq(12.5,52.5,1),seq(-15,60,2.5)))
  img <- img_info$freq2D
  
  img <- log(img + 1)
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        z=img,
        add=TRUE)
  
  lines(x=c(12,20,20,52),
        y=c(0,0,45,45),
        col='#66F',lty=1,lw=2)
  
  axis(side=1, at=c(12, 20, 28, 36, 44, 52), labels=c(-8,0,8,16,24,32))
  axis(side=2, at=seq(-15,60,15))
  
  plot(x=1000,y=-1000,
       main='adaptation',xlab='',ylab='',
       xlim=c(13,52),ylim=c(-15,60), 
       ax=F,bty='n')
  
  img_info <- hist2d(x=adpt, nbins=NA, edges=list(seq(12.5,52.5,1),seq(-15,60,2.5)))
  img <- img_info$freq2D
  
  img <- log(img + 1)
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        z=img,
        add=TRUE)
  
  lines(x=c(12,20,20,52),
        y=c(0,0,45,45),
        col='#66F',lty=1,lw=2)
  
  axis(side=1, at=c(12, 20, 28, 36, 44, 52), labels=c(-8,0,8,16,24,32))
  axis(side=2, at=seq(-15,60,15))
  
}



plotBondTaylorExplicit <- function() {
  
  BT <- getBTsizeData()
  
  explicit   <- BT$explicit
  adaptation <- BT$adaptation
  
  # 5.625
  
  layout(mat=matrix(c(1:8),byrow=TRUE,nrow=4,ncol=2))
  par(mar=c(3.0,3.0,2.5,0.1))
  
  # 6 baseline blocks of 8 trials
  # there are about 8 blocks with large spread across participants
  # we'll look at the first 5?
  
  for (rots in c(1:4)) {
    
    rdf <- adaptation[rots,,]
    
    plot(x=1000,y=-1000,
         main=sprintf('%d° adaptation',c(15,30,60,90)[rots]),xlab='',ylab='',
         xlim=c(-8,41),ylim=c(-15,105), # should ylim depend on rotation?
         ax=F,bty='n')
    
    df <- NA
    for (ppno in c(1:10)) {
      aimlist <- rdf[ppno,c(41:89)]
      
      pdf <- data.frame(x=c(-8:40),
                        y=aimlist,
                        ppno=ppno)
      
      if (is.data.frame(df)){
        df <- rbind(df,pdf)
      } else {
        df <- pdf
      }
    }
    
    img_info <- hist2d(x=df, nbins=NA, edges=list(seq(-8.5,40.5,1),seq(-2.5*5.625,18.5*5.625,5.625)))
    img <- img_info$freq2D
    
    img <- log(img + 1)
    
    image(x=img_info$x.edges,
          y=img_info$y.edges,
          z=img,
          add=TRUE)
    
    # ideal aiming response:
    lines(x=c(-8,0,0,41),
          y=c(rep(0,2),rep(c(15,30,60,90)[rots],2)),
          col='#000',lty=3)
    
    axis(side=1,at=seq(-8,40,8))
    axis(side=2,at=seq(0,90,30))
    
    # now the explicit counter part
    rdf <- explicit[rots,,]
    
    #lines
    plot(x=1000,y=-1000,
         main=sprintf('%d° explicit',c(15,30,60,90)[rots]),xlab='',ylab='',
         xlim=c(-8,41),ylim=c(-12,102),
         ax=F,bty='n')
    # 2d histogram / density
    
    df <- NA
    for (ppno in c(1:10)) {
      aimlist <- rdf[ppno,c(41:89)]
      
      pdf <- data.frame(x=c(-8:40),
                        y=aimlist,
                        ppno=ppno)
      
      if (is.data.frame(df)){
        df <- rbind(df,pdf)
      } else {
        df <- pdf
      }
    }
    
    img_info <- hist2d(x=df, nbins=NA, edges=list(seq(-8.5,40.5,1),seq(-2.5*5.625,18.5*5.625,5.625)))
    img <- img_info$freq2D
    
    img <- log(img + 1)
    
    image(x=img_info$x.edges,
          y=img_info$y.edges,
          z=img,
          add=TRUE)
    
    # ideal aiming response:
    lines(x=c(-8,0,0,41),
          y=c(rep(0,2),rep(c(15,30,60,90)[rots],2)),
          col='#00F',lty=3)
    
    axis(side=1,at=seq(-8,40,8))
    axis(side=2,at=seq(0,90,30))
    
  }
  
}

hist2d <- function(x, y=NA, nbins=c(25,25), edges=NA) {
  
  if (is.data.frame(x)) {
    # check shape of x?
    df <- x
  } else if (is.matrix(x)) {
    # check shape of x?
    df <- as.data.frame(x)
  } else {
    df <- data.frame('x'=x, 'y'=y)
  }
  
  # code below, somewhat based on:
  # http://stackoverflow.com/questions/18089752/r-generate-2d-histogram-from-raw-data
  
  if (is.numeric(nbins)) {
    x.edges <- seq(floor(min(df[,1])), ceiling(max(df[,1])), length=nbins[1])
    y.edges <- seq(floor(min(df[,2])), ceiling(max(df[,2])), length=nbins[2])
  }
  
  if (is.list(edges)) {
    x.edges <- edges[[1]]
    y.edges <- edges[[2]]
  }
  
  xbincount <- findInterval(df[,1], x.edges, rightmost.closed = T, left.open = F, all.inside = F)
  ybincount <- findInterval(df[,2], y.edges, rightmost.closed = T, left.open = F, all.inside = F)
  xbincount <- factor(xbincount, levels=c(1:(length(x.edges)-1)))
  ybincount <- factor(ybincount, levels=c(1:(length(y.edges)-1)))
  
  freq2D <- as.matrix(table(xbincount,ybincount))
  dimnames( freq2D ) <- c()
  rownames( freq2D ) <- c()
  colnames( freq2D ) <- c()
  
  return(list('freq2D'=freq2D, 'x.edges'=x.edges, 'y.edges'=y.edges))
  
}

# old data fits ------


stepFunction <- function(par, trials) {
  
  if (length(trials) == 1) {
    trials <- c(0:(trials-1))
  }
  
  predictions <- rep(0, length(trials))
  
  predictions[which(trials >= par['t'])] <- par['s']
  
  # print(predictions)
  
  return(predictions)
  
}

stepMSE <- function(par, data) {
  
  # trials <- unique(data$trial)
  # 
  # predictions <- stepFunction(par    = par,
  #                             trials = trials)
  # 
  # errors <- data$deviation - predictions
  # 
  # MSE <- mean(errors^2, na.rm=TRUE)
  # 
  # return(MSE)
  
  return(mean((stepFunction(par = par, trials = unique(data$trial)) - data$deviation)^2, na.rm=TRUE))
  
}

require('Reach')

stepFit <- function(data, gridpoints=11, gridfits=10) {
  
  # set the search grid:
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  stepsizes <- (parvals * diff(range(data$deviation, na.rm=TRUE))) - min(data$deviation, na.rm=TRUE)
  
  steptimes <- parvals * max(data$trial, na.rm=TRUE)
  
  searchgrid <- expand.grid('t' = steptimes,
                            's' = stepsizes)
  
  MSE <- apply(searchgrid, FUN=stepMSE, MARGIN=c(1), data=data)
  # print(MSE)
  
  # print(data$deviation)
  
  lo <- c(0,min(data$deviation, na.rm=TRUE))
  hi <- c(max(data$trial, na.rm=TRUE), max(data$deviation, na.rm=TRUE))
  # print(lo)
  # print(hi)
  
  # print(data.frame(searchgrid[order(MSE)[1:gridfits],]))
  
  # run optimx on the best starting positions:
  allfits <- do.call("rbind",
                     apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=stepMSE,
                            # method     = 'L-BFGS-B',
                            # lower      = lo,
                            # upper      = hi,
                            data       = data) )
  
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  winpar <- unlist(win[1:2])
  
  # return the best parameters:
  return(winpar)
  
}


allStepExpoFits <- function() {
  
  explicit <- read.csv('data/implicit/exp4/aiming_aiming.csv', stringsAsFactors = F)
  explicit$aimingdeviation_deg <- -1*explicit$aimingdeviation_deg
  implicit <- read.csv('data/implicit/exp4/aiming_nocursors.csv', stringsAsFactors = F)
  implicit$reachdeviation_deg <- -1*implicit$reachdeviation_deg
  adaptation <- read.csv('data/implicit/exp4/aiming_reaches.csv', stringsAsFactors = F)
  adaptation$reachdeviation_deg <- -1*adaptation$reachdeviation_deg
  
  # trials: pre = 13-20, post = 21-52
  
  expl <- explicit[   which(explicit$trialno   %in% c(21:120)), ]
  impl <- implicit[   which(implicit$trialno   %in% c(20:120)), ]
  adpt <- adaptation[ which(adaptation$trialno %in% c(21:120)), ]
  
  expl$trialno <- expl$trialno - 20
  impl$trialno <- impl$trialno - 20
  adpt$trialno <- adpt$trialno - 20
  
  expl <- expl[,c('participant','trialno','aimingdeviation_deg')]
  impl <- impl[,c('participant','trialno','reachdeviation_deg')]
  adpt <- adpt[,c('participant','trialno','reachdeviation_deg')]
  
  names(expl) <- c('participant','trial','deviation')
  names(impl) <- c('participant','trial','deviation')
  names(adpt) <- c('participant','trial','deviation')
  
  participants <- unique(expl$participant)
  
  # expl.exp.MSE <- c()
  # expl.exp.l <- c()
  # expl.exp.a <- c()
  # expl.step.MSE <- c()
  # expl.step.t <- c()
  # expl.step.s <- c()
  # 
  # impl.exp.MSE <- c()
  # impl.exp.l <- c()
  # impl.exp.a <- c()
  # impl.step.MSE <- c()
  # impl.step.t <- c()
  # impl.step.s <- c()
  # 
  # adpt.exp.MSE <- c()
  # adpt.exp.l <- c()
  # adpt.exp.a <- c()
  # adpt.step.MSE <- c()
  # adpt.step.t <- c()
  # adpt.step.s <- c()
  
  participant <- c()
  process <- c()
  
  exp.MSE <- c()
  exp.l <- c()
  exp.a <- c()
  step.MSE <- c()
  step.t <- c()
  step.s <- c()
  
  for (pp_no in c(1:length(participants))) {
    
    cat(sprintf('working on participant %d / %d\n',pp_no,length(participants)))
    
    ppid <- participants[pp_no]
    
    pex <- expl[which(expl$participant == ppid),]
    pim <- impl[which(impl$participant == ppid),]
    pad <- adpt[which(adpt$participant == ppid),]
    
    
    for (datatype in c('ex','im','ad')) {
      
      df <- list('ex'=pex, 'im'=pim, 'ad'=pad)[[datatype]]
      
      exppar <- Reach::exponentialFit(signal = df$deviation)
      # print(exppar)
      MSEexp <- Reach::exponentialMSE(par    = exppar,
                                      signal = df$deviation)
      
      steppar <- stepFit(data = df)
      # print(steppar)
      MSEstep <- stepMSE(par=steppar, data=df)
      
      # if (datatype == 'ex') {
      #   expl.exp.MSE  <- c(expl.exp.MSE,  expMSE)
      #   expl.exp.l    <- c(expl.exp.l,    exppar['lambda'])
      #   expl.exp.a    <- c(expl.exp.a,    exppar['N0'])
      #   expl.step.MSE <- c(expl.step.MSE, step.MSE)
      #   expl.step.t   <- c(expl.step.t,   steppar['t'])
      #   expl.step.s   <- c(expl.step.s,   steppar['s'])
      # }
      # 
      # if (datatype == 'im') {
      #   impl.exp.MSE  <- c(impl.exp.MSE,  expMSE)
      #   impl.exp.l    <- c(impl.exp.l,    exppar['lambda'])
      #   impl.exp.a    <- c(impl.exp.a,    exppar['N0'])
      #   impl.step.MSE <- c(impl.step.MSE, step.MSE)
      #   impl.step.t   <- c(impl.step.t,   steppar['t'])
      #   impl.step.s   <- c(impl.step.s,   steppar['s'])
      # }
      # 
      # if (datatype == 'ad') {
      #   adpt.exp.MSE  <- c(adpt.exp.MSE,  expMSE)
      #   adpt.exp.l    <- c(adpt.exp.l,    exppar['lambda'])
      #   adpt.exp.a    <- c(adpt.exp.a,    exppar['N0'])
      #   adpt.step.MSE <- c(adpt.step.MSE, step.MSE)
      #   adpt.step.t   <- c(adpt.step.t,   steppar['t'])
      #   adpt.step.s   <- c(adpt.step.s,   steppar['s'])
      # }
      
      participant <- c(participant, ppid)
      
      process <- c(process, list('ex'='explicit', 'im'='implicit', 'ad'='adaptation')[[datatype]])
      
      exp.MSE <- c(exp.MSE, MSEexp)
      exp.l   <- c(exp.l, exppar[['lambda']])
      exp.a   <- c(exp.a, exppar[['N0']])
      
      step.MSE <- c(step.MSE, MSEstep)
      step.t   <- c(step.t, steppar[['t']])
      step.s   <- c(step.s, steppar[['s']])
      
    }
    
  }
  
  # print(length(participant))
  # print(length(process))
  # print(length(exp.MSE))
  # print(length(exp.l))
  # print(length(exp.a))
  # print(length(step.MSE))
  # print(length(step.t))
  # print(length(step.s))
  
  # df <- data.frame( participant = participants,
  #                   expl.exp.MSE,
  #                   expl.exp.l,
  #                   expl.exp.a,
  #                   expl.step.MSE,
  #                   expl.step.t,
  #                   expl.step.s,
  #                   impl.exp.MSE,
  #                   impl.exp.l,
  #                   impl.exp.a,
  #                   impl.step.MSE,
  #                   impl.step.t,
  #                   impl.step.s,
  #                   adpt.exp.MSE,
  #                   adpt.exp.l,
  #                   adpt.exp.a,
  #                   adpt.step.MSE,
  #                   adpt.step.t,
  #                   adpt.step.s                 )
  # 
  # print(df)
  df <- data.frame( participant,
                    process,
                    exp.MSE,
                    exp.l,
                    exp.a,
                    step.MSE,
                    step.t,
                    step.s  )
  
  print(df)
  
  write.csv( df,
             file = 'data/implicit/expStepFits.csv',
             quote = TRUE,
             row.names = FALSE)
  
}


plotImplicitMSE <- function() {
  
  
  df <- read.csv('data/implicit/expStepFits.csv', stringsAsFactors = FALSE)
  
  layout(mat=matrix(c(1:3),nrow=1,ncol=3,byrow=TRUE))
  
  set.seed(1337)
  
  exp.colors  <- c('#0066FFFF', '#0066FF33')
  step.colors <- c('#FF6600FF', '#FF660033')
  
  for (process in c('adaptation', 'implicit', 'explicit')) {
    
    if (process %in% c('implicit','adaptation')) {
      YL <- c(0,3200)
    } else {
      YL <- c(0,400)
    }
    
    plot(-1000,-1000,main=process,
         xlab='function',ylab='MSE',
         xlim=c(0.5,2.5),ylim=YL,
         bty='n',ax=F)
    
    sdf <- df[which(df$process == process),]
    MSEstep <- sdf$step.MSE
    MSEexp  <- sdf$exp.MSE
    
    # EXPONENTIAL FIT
    points(x=rep(0.75,length(MSEexp)),
           y=MSEexp,
           pch=16, cex=2,
           col=exp.colors[2])
    
    avg <- mean(MSEexp)
    CI  <- Reach::getConfidenceInterval(MSEexp,method='b')

    polygon(x=c(1,1.25,1.25,1),
            y=rep(c(CI[1],CI[2]),each=2),
            border=NA,
            col=exp.colors[2])
    lines(x=c(1,1.25),
          y=rep(avg,2),
          col=exp.colors[1])
    
    # STEP FUNCTION FIT
    
    points(x=rep(1.75,length(MSEstep)),
           y=MSEstep,
           pch=16, cex=2,
           col=step.colors[2])
    
    avg <- mean(MSEstep)
    CI  <- Reach::getConfidenceInterval(MSEstep,method='b')
    
    polygon(x=c(2,2.25,2.25,2),
            y=rep(c(CI[1],CI[2]),each=2),
            border=NA,
            col=step.colors[2])
    lines(x=c(2,2.25),
          y=rep(avg,2),
          col=step.colors[1])
    
    if (process %in% c('implicit','adaptation')) {
      axis(side=2,at=seq(0,3200,800))
    } else {
      axis(side=2,at=seq(0,400,100))
    }
    
    axis(side=1, at=c(1,2), labels=c('exponential','step function'))
    
  }
  
}



BFtestImplicitMSE <- function() {
  
  df <- read.csv('data/implicit/expStepFits.csv', stringsAsFactors = FALSE)
  
  for (process in c('adaptation', 'implicit', 'explicit')) {
    sdf <- df[which(df$process == process),]
    MSEstep <- sdf$step.MSE
    MSEexp  <- sdf$exp.MSE
    
    best = 'exponential'
    if (mean(MSEstep) < mean(MSEexp)) {best = 'step function'}
    
    cat(sprintf('PROCESS : %s (%s best)\n', process, best))
    
    print( BayesFactor::ttestBF(  x = MSEexp,
                                  y = MSEstep,
                                  paired=TRUE)              )
    
  }
    
}

# new data plots -------

newTimeCoursePlots <- function() {
  
  demographics <- read.csv('exp/data/demographics.csv', stringsAsFactors = F)
  ID25 <- demographics$Participant.ID[which(grepl("2024", demographics$Timestamp))]
  
  layout(mat=matrix(1:5,ncol=1))
  par(mar=c(3.5,3.5,1,0.1))
  
  for (rotation in c(20,30,40,50,60)) {
    
    df <- loadTimeCourseData(rotations=c(rotation))
    
    df <- df[which(df$participant %in% ID25),]
    
    plot( -1000,-1000,
          main='', xlab='', ylab='',
          xlim=c(-9,33), ylim=c(-10,70),
          ax=F, bty='n')
    
    lines(x=c(-9,0,0,32),
          y=c(0,0,rotation,rotation),
          col='#CCCCCC')
    # lines(x=c(0,32),
    #       y=c(0,0),
    #       col='#CCCCCC')
    
    participants <- unique(df$participant)
    
    for (participant in participants) {
      pdf <- df[which(df$participant == participant),]
      pdf <- pdf[which(pdf$task_idx %in% c(7,8) & pdf$trial_idx < 33),]
      strategy <- pdf$aimdeviation_deg
      # print(length(strategy))
      lines(x=c(-8:31),
            y=strategy,
            col='#FF000033',
            lw=2)
    }
    
    axis(side=1, at=c(-8,0,8,16,24,32))
    axis(side=2, at=c(0,rotation))
    
  }
  
}