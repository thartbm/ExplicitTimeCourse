
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

newTimeCoursePlots <- function() {
  
  
  
  layout(mat=matrix(1:5,ncol=1))
  par(mar=c(3.5,3.5,1,0.1))
  
  for (rotation in c(20,30,40,50,60)) {
    
    df <- loadTimeCourseData(rotations=c(rotation))
    
    plot( -1000,-1000,
          main='', xlab='', ylab='',
          xlim=c(-9,25), ylim=c(-15,35),
          ax=F, bty='n')
    
    lines(x=c(-9,0,0,24),
          y=c(0,0,rotation,rotation),
          col='#CCCCCC')
    lines(x=c(0,24),
          y=c(0,0),
          col='#CCCCCC')
    
    participants <- unique(df$participant)
    
    for (participant in participants) {
      pdf <- df[which(df$participant == participant),]
      pdf <- pdf[which(pdf$task_idx %in% c(7,8) & pdf$trial_idx < 25),]
      strategy <- pdf$aimdeviation_deg
      lines(x=c(-8:23),
            y=strategy,
            col='#FF000033',
            lw=2)
    }
    
    axis(side=1, at=c(-8,0,8,16,24))
    axis(side=2, at=c(-10,10,30))
    
  }
  
}