
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

# Bond & Taylor data -----

getBondSizeData <- function() {
  
  matlabArray <- R.matlab::readMat( 'data/bond_2015/Bond2015_RotationSizeExp.mat')[[1]]
  
  explicit <- matlabArray[,,1]$explicit
  
  adaptation <- matlabArray[,,1]$hand.angle
  
  return(list('explicit'=explicit, 'adaptation'=adaptation))
  
}


convertWiltersonDataHDF5 <- function() {
  
  mtlbData <- hdf5r::H5File$new(filename='data/wilterson_2021/RotationData.mat', mode='r+')
  
  # this lists all components of all the structs in there:
  # mtlbData$ls(recursive=TRUE)
  
  # we want:
  # "data/aimAngleCommon"
  # "data/handHeadAngleCommon"
  
  rotation <- abs(t(mtlbData[['data/rotation']]$read()))[,1]

  aiming <- as.data.frame(t(mtlbData[['data/aimAngleCommon']]$read()))
  names(aiming) <- sprintf('p%02d',c(1:12))
  aiming$rotation <- rotation
  aiming <- aiming[c(1:800),]
  
  adaptation <- as.data.frame(t(mtlbData[['data/handHeadAngleCommon']]$read()))
  names(adaptation) <- sprintf('p%02d',c(1:12))
  adaptation$rotation <- rotation
  adaptation <- adaptation[c(1:800),]
  
  write.csv(aiming, 'data/wilterson_2021/aiming.csv', quote=F, row.names=F)
  write.csv(adaptation, 'data/wilterson_2021/adaptation.csv', quote=F, row.names=F)
  
  return(list('explicit'=aiming, 'adaptation'=adaptation))
  
}
# current data set -----

downloadExplicitTimecourse <- function() {
  
  # directory should already exist in the github repo
  Reach::downloadOSFdata( repository = '6g3h7',
                          filelist = list('data'=c('summary_data_files.zip')), 
                          folder = 'exp/data/',
                          overwrite = TRUE,
                          unzip = TRUE,
                          removezips = TRUE)
  
  
  
}


