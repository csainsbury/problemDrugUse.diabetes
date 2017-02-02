## v2 additions / DM
library(data.table)

## functions
returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%d/%m/%Y", tz="GMT"))
  return(returnVal)
}

extractDOBFuntion <- function(input_CHI) {
  
  char_CHI<-as.character(input_CHI)
  char_CHI<-ifelse(nchar(char_CHI)==9,paste("0",char_CHI,sep=""),char_CHI)
  
  yrvals <- substr(char_CHI,5,6)
  yrvalsDF <- as.data.frame(yrvals);colnames(yrvalsDF) <- c("yr")
  
  yrvalsDF$yr.as.char <- as.character(yrvalsDF$yr)
  yrvalsDF$yr.as.num  <- as.numeric(yrvalsDF$yr.as.char)
  yrvalsDF$century    <- ifelse (((yrvalsDF$yr.as.num>16)&(yrvalsDF$yr.as.num<=99)),19,20)
  yrvalsDF$Y          <- paste(yrvalsDF$century,yrvalsDF$yr.as.char,sep="",collapse = NULL)
  yrvalsDF$Y.as.num   <- suppressWarnings(as.numeric(yrvalsDF$Y))
  
  datevalsdaymonth      <- substr(char_CHI,1,4)
  datevals              <- paste(datevalsdaymonth,yrvalsDF$Y,sep='')
  dob                   <- as.POSIXct(datevals,format="%d%m%Y")
  dob.as.num            <- as.numeric(dob)
  
  return(dob.as.num)
}

# OPfilename <- paste("../GlCoSy/source/addictionData.csv",sep="")
OPfilename <- paste("~/R/GlCoSy/source/all_ORT.csv",sep="")
addictionData<-read.csv(OPfilename, header=TRUE , sep="," , row.names=NULL)
colnames(addictionData) <- c("CHI")
addictionData <- unique(addictionData)

diagnosisDataset<-read.csv("~/R/GlCoSy/SDsource/demogALL.txt", quote = "", 
                           row.names = NULL, 
                           stringsAsFactors = FALSE)

allDM_addiction_merge<-merge(addictionData,diagnosisDataset,by.x="CHI",by.y="PatId")

table(allDM_addiction_merge$DiabetesMellitusType_Mapped)
table(allDM_addiction_merge$DeathDate)


## matching function

diagnosisDatasetDT<-data.table(diagnosisDataset)
    diagnosisDatasetDT$age_at_010111 <- ((as.numeric(as.POSIXct("2011-01-01", format="%Y-%m-%d", tz="GMT"))) - (as.numeric(as.POSIXct(diagnosisDatasetDT$BirthDate, format="%Y-%m-%d", tz="GMT")))) / (60*60*24*365.25)
    
    diagnosisDatasetDT$diagnosisDateUnix<-as.numeric(as.POSIXct(diagnosisDatasetDT$DateOfDiagnosisDiabetes_Date, format="%Y-%m-%d", tz="GMT"))
    # diabetes duration at 010111
    diagnosisDatasetDT$diabetesDuration_at_010111<- ((as.numeric(as.POSIXct("2011-01-01", format="%Y-%m-%d", tz="GMT"))) - diagnosisDatasetDT$diagnosisDateUnix) / (60*60*24*365.25)
    
    diagnosisDatasetDT$dmType_match <- ifelse(diagnosisDatasetDT$DiabetesMellitusType_Mapped == "Type 1 Diabetes Mellitus",1,0)
    diagnosisDatasetDT$sex<-ifelse(diagnosisDatasetDT$CurrentGender_Mapped=="Male",1,0)
    
    

allDM_addiction_merge$sex<-ifelse(allDM_addiction_merge$CurrentGender_Mapped=="Male",1,0)

interest_addiction_set <- data.frame(allDM_addiction_merge$CHI,
                                     allDM_addiction_merge$BMI,
                                     allDM_addiction_merge$DeprivationQuintile,
                                     allDM_addiction_merge$sex,
                                     allDM_addiction_merge$DiabetesMellitusType_Mapped,
                                     allDM_addiction_merge$DateOfDiagnosisDiabetes_Date,
                                     allDM_addiction_merge$CurrentTobaccoNicotineConsumptionStatus_Mapped,
                                     allDM_addiction_merge$DeathDate
                                     )
colnames(interest_addiction_set) <- c("CHI", "BMI", "depQuint", "sex", "DMtype", "diagnosisDate", "smoking", "deathDate")

interest_addiction_set$BMI<-as.numeric(levels(interest_addiction_set$BMI))[interest_addiction_set$BMI]
# impute missing BMI with median value
interest_addiction_set$BMI[is.na(interest_addiction_set$BMI)] <- quantile(interest_addiction_set$BMI, na.rm=T)[3]

# interest_addiction_set$dobUnix<-returnUnixDateTime(interest_addiction_set$DOB)
interest_addiction_set$diagnosisDateUnix<-as.numeric(as.POSIXct(interest_addiction_set$diagnosisDate, format="%Y-%m-%d", tz="GMT"))
interest_addiction_set$DeathDateUnix<-as.numeric(as.POSIXct(interest_addiction_set$deathDate, format="%Y-%m-%d", tz="GMT"))
interest_addiction_set$dmType_match <- ifelse(interest_addiction_set$DMtype == "Type 1 Diabetes Mellitus",1,0)

# age at 010111
interest_addiction_set$dob <- extractDOBFuntion(interest_addiction_set$CHI)
interest_addiction_set$age_at_010111 <- ((as.numeric(as.POSIXct("2011-01-01", format="%Y-%m-%d", tz="GMT"))) - interest_addiction_set$dob) / (60*60*24*365.25)

## impute missing diagnosis date with median value
interest_addiction_set[is.na(interest_addiction_set$diagnosisDateUnix),]$diagnosisDateUnix <- median(interest_addiction_set[is.na(interest_addiction_set$diagnosisDateUnix)==FALSE,]$diagnosisDateUnix)

# diabetes duration at 010111
interest_addiction_set$diabetesDuration_at_010111<- ((as.numeric(as.POSIXct("2011-01-01", format="%Y-%m-%d", tz="GMT"))) - interest_addiction_set$diagnosisDateUnix) / (60*60*24*365.25)

# mark the ID in the drug use pool as not for control use
mm<-match(diagnosisDatasetDT$PatId, interest_addiction_set$CHI, nomatch = 0, incomparables = NULL)
diagnosisDatasetDT$useAsControlPool <- ifelse(mm==0,1,0)

matchingPool <- diagnosisDatasetDT[useAsControlPool==1]

ageWindow <- 10
BMIWindow <- 2
diabetesDurationWindow <- 5

reportingFrame<-diagnosisDatasetDT[1,]
reportingFrame$controlPairNumber <- 0
reportingFrame <- reportingFrame[-1,]

interest_addiction_set$casePairNumber <- seq(1,nrow(interest_addiction_set),1)

set.seed(1234)

for (j in seq(1, nrow(interest_addiction_set),1)) {
  
  # print(j)
  
  match_age <- interest_addiction_set$age_at_010111[j]
  match_BMI <- interest_addiction_set$BMI[j]
  match_DMtype <- interest_addiction_set$dmType_match[j]
 # match_depQuint <- interest_addiction_set$depQuint[j]
  match_diabetesDuration <- interest_addiction_set$diabetesDuration_at_010111[j]
  match_sex <- interest_addiction_set$sex[j]
 # match_smoking_status <- interest_addiction_set$smoking[j]
  
  # matchingPool <- diagnosisDatasetDT[useAsControlPool==1]
  
  matchingSet <- matchingPool[ # DeprivationQuintile==match_depQuint &
                                sex == match_sex &
                                # CurrentTobaccoNicotineConsumptionStatus_Mapped == match_smoking_status &
                                dmType_match == match_DMtype &
                                BMI > match_BMI - (BMIWindow / 2) & BMI < match_BMI + (BMIWindow / 2) &
                                age_at_010111 > (match_age - (ageWindow / 2)) & age_at_010111 < (match_age + (ageWindow / 2)) &
                                diabetesDuration_at_010111 > (match_diabetesDuration - (diabetesDurationWindow / 2)) & diabetesDuration_at_010111 < (match_diabetesDuration + (diabetesDurationWindow / 2))]
  
  matchingSet$controlPairNumber <- 0
  
  if (nrow(matchingSet)>0) {
    matching_individual <- matchingSet[sample(1:nrow(matchingSet),1),]
    matching_individual$controlPairNumber <- j 
  }
  
  print(nrow(matchingSet))
  
if (nrow(matchingSet)>0) {
  reportingFrame<-rbind(reportingFrame,matching_individual[1])
}  
}

# generate rows of combined case/control data
mergeCaseControl <- merge(reportingFrame, interest_addiction_set, by.x="controlPairNumber", by.y="casePairNumber")

# simplify cols
mergeCaseControl$BMI.x<-as.numeric(mergeCaseControl$BMI.x)




