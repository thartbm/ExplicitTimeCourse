
# plotting functions ----

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

getColorPalette <- function(n=100, bg='#FFFFFF', fg=rgb(229, 22,  54,  255, max = 255)) {
  
  # get a color palette with n colors, starting from the background color
  # and ending with the foreground color
  
  # bg <- '#FFFFFF'
  # fg <- rgb(229, 22,  54,  255, max = 255)
  
  pal <- grDevices::colorRampPalette(c(bg, fg))(n)
  
  return(pal)
  
  
}



# D'Amario data -----

plotDAmario_exp4 <- function() {
  
  layout(mat=matrix(c(1:3),nrow=3,ncol=1,byrow=TRUE))
  
  par(mar=c(4.1,4,1.5,0.1))
  
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
       main='explicit',xlab='trial',ylab='deviation [°]',
       xlim=c(13,52),ylim=c(-15,60), 
       ax=F,bty='n')
  
  img_info <- hist2d(x=expl, nbins=NA, edges=list(seq(12.5,52.5,1),seq(-15,60,2.5)))
  img <- img_info$freq2D
  
  img <- log(img + 1)
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        z=img,
        add=TRUE,
        col=getColorPalette())
  
  lines(x=c(12,20,20,52),
        y=c(0,0,45,45),
        col='#999',lty=1,lw=2)
  
  avg <- aggregate(y ~ x, data=expl, FUN=mean)
  lines(avg,
        col='#66F',lty=1,lw=2)
  
  axis(side=1, at=c(12, 20, 28, 36, 44, 52), labels=c(-8,0,8,16,24,32))
  axis(side=2, at=c(0,45))
  
  
  plot(x=1000,y=-1000,
       main='implicit',xlab='trial',ylab='deviation [°]',
       xlim=c(13,52),ylim=c(-15,60), 
       ax=F,bty='n')
  
  img_info <- hist2d(x=impl, nbins=NA, edges=list(seq(12.5,52.5,1),seq(-15,60,2.5)))
  img <- img_info$freq2D
  
  img <- log(img + 1)
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        z=img,
        add=TRUE,
        col=getColorPalette())
  
  lines(x=c(12,20,20,52),
        y=c(0,0,45,45),
        col='#999',lty=1,lw=2)
  
  avg <- aggregate(y ~ x, data=impl, FUN=mean)
  lines(avg,
        col='#66F',lty=1,lw=2)
  
  axis(side=1, at=c(12, 20, 28, 36, 44, 52), labels=c(-8,0,8,16,24,32))
  axis(side=2, at=c(0,45))
  
  plot(x=1000,y=-1000,
       main='adaptation',xlab='trial',ylab='deviation [°]',
       xlim=c(13,52),ylim=c(-15,60), 
       ax=F,bty='n')
  
  img_info <- hist2d(x=adpt, nbins=NA, edges=list(seq(12.5,52.5,1),seq(-15,60,2.5)))
  img <- img_info$freq2D
  
  img <- log(img + 1)
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        z=img,
        add=TRUE,
        col=getColorPalette())
  
  lines(x=c(12,20,20,52),
        y=c(0,0,45,45),
        col='#999',lty=1,lw=2)
  
  avg <- aggregate(y ~ x, data=adpt, FUN=mean)
  lines(avg,
        col='#66F',lty=1,lw=2)
  
  axis(side=1, at=c(12, 20, 28, 36, 44, 52), labels=c(-8,0,8,16,24,32))
  axis(side=2, at=c(0,45))
  
}





# new data sets ----


loadNewData <- function(rotations=c(20,30,40,50,60)) {
  
  df <- NA
  
  for (rotation in rotations) {
    
    rdf <- getRotationData(rotation=rotation)
    rdf$condition <- rotation
    
    if (is.data.frame(df)) {
      df <- rbind(df, rdf)
    } else {
      df <- rdf
    }
    
  }
  
  return(df)
  
}


getRotationData <- function(rotation) {
  
  path <- sprintf('data/phase_2/aiming%d/',rotation)
  
  allcsvfiles <- list.files(path=path, pattern='*.csv')
  
  all_data <- NA
  
  for (file in allcsvfiles) {

    csvfile <- sprintf('%s%s', path, file)

    df <- read.csv(csvfile, stringsAsFactors = FALSE)
    
    # since participants had some differences in paradigms,
    # here we select the relevant part for each of them:
    
    if (length(unique(df$task_idx)) == 9) {
      # old paradigm
      task_idx <- c(7,8)
    }
    if (length(unique(df$task_idx)) == 12) {
      # new paradigm
      task_idx <- c(11,12)
    }
    sdf <- df[which(df$task_idx %in% task_idx & df$trial_idx < 33),]
    
    sdf <- sdf[,c('task_idx','trial_idx','rotation_deg','reachdeviation_deg','aimdeviation_deg')]
    
    sdf$participant <- substr(file, 18, 23)
    
    sdf$trialno <- c(1:dim(sdf)[1])-9
    
    if (is.data.frame(all_data)) {
      all_data <- rbind(all_data, sdf)
    } else {
      all_data <- sdf
    }
    
  }
  
  return(all_data)
  
}





plotNewData <- function() {
  
  df <- loadNewData()
  
  layout(mat=matrix(c(1:5),ncol=1,byrow=TRUE))
  
  par(mar=c(4.1,4,1.5,0.1))
  
  for (rotation in c(20,30,40,50,60)) {
    
    sdf <- df[which(df$condition == rotation),]
    
    N <- length(unique(sdf$participant))
    
    plot(x=1000,y=-1000,
         main=sprintf('rotation %d (N=%d)',rotation,N),xlab='trial',ylab='deviation [°]',
         xlim=c(-8,32),ylim=c(-10,70), 
         ax=F,bty='n')
    
    sdf <- sdf[,c('trialno','aimdeviation_deg')]
    names(sdf) <- c('x','y')
    
    img_info <- hist2d(x=sdf, nbins=NA, edges=list(seq(-8.5,32.5,1),seq(-10,70,2.5)))
    img <- img_info$freq2D
    
    img <- log(img + 1)
    
    image(x=img_info$x.edges,
          y=img_info$y.edges,
          z=img,
          add=TRUE,
          col=getColorPalette())
    
    lines(x=c(0,8,8,40)-9,
          y=c(0,0,rotation,rotation),
          col='#999',lty=1,lw=2)
    
    avg <- aggregate(y ~ x, data=sdf, FUN=mean)
    lines(avg,
          col='#66F',lty=1,lw=2)
    
    axis(side=1, at=c(0, 8, 16, 24, 32, 40)-9, labels=c(-8,0,8,16,24,32))
    axis(side=2, at=c(0,rotation))
    
  }
  
  
}

