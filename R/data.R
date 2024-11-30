
# previous data sets -----

# download old data

downloadAddivityAiming <- function() {
  
  dir.create(file.path('data', 'additivity'))
  Reach::downloadOSFdata( repository = 'kr5eh',
                          filelist = c('/'='aiming.zip'),
                          folder = 'data/additivity/',
                          overwrite = TRUE,
                          unzip = TRUE,
                          removezips = TRUE)
  
}

downloadImplicitAiming <- function() {
  
  dir.create(file.path('data', 'implicit'))
  Reach::downloadOSFdata( repository = 'ajwyr',
                          filelist = list('data'=c('exp4.zip','demographics.csv')), # maybe processed.zip is easier?
                          folder = 'data/implicit/',
                          overwrite = TRUE,
                          unzip = TRUE,
                          removezips = TRUE)
  
  demographics <- read.csv('data/implicit/demographics.csv', stringsAsFactors = TRUE)
  demographics <- demographics[which(demographics$exp == 4),]
  write.csv(demographics, 'data/implicit/demographics.csv', quote=F, row.names=F)
  
}

downloadExplicitTimecourse <- function() {
  
  # directory should already exist in the github repo
  Reach::downloadOSFdata( repository = '6g3h7',
                          filelist = list('data'=c('summary_data_files.zip')), 
                          folder = 'exp/data/',
                          overwrite = TRUE,
                          unzip = TRUE,
                          removezips = TRUE)
  
  
  
}

