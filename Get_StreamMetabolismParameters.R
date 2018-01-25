setwd('')

library(streamMetabolizer)

#Load table of gages with start and end dates and alternate discharge gage stations 
#if not the same as continuous DO site######
gage<- read.table("gage_dates.txt",header=TRUE,stringsAsFactors=FALSE)
gage<- gage[which(gage$DischargeGage!="NA"),]
gage$Site<-paste("0",gage$Site,sep="")
gage$DischargeGage<-paste("0",gage$DischargeGage,sep="")


######assume one header line and tab delimited structure, with # as a comment out to skip#####
parse_fstat<-function(fstat_lines,skip='#',delim='\t'){
  x<-1;
  while(x<length(fstat_lines) && startsWith(fstat_lines[x],skip)){
    x<-x+1;
  }
  header<-strsplit(fstat_lines[x],delim)[[1]];
  D<-as.data.frame(matrix('',ncol=length(header),nrow=length(fstat_lines)-x),stringsAsFactors=F);
  colnames(D)<-header;
  for(i in x+2:length(fstat_lines)){
    r<-strsplit(fstat_lines[i],delim)[[1]];
    D[i-x-1,1:length(r)]<-r;
  }
  D
}

#########Change Parameters here##########################################################################
#    Parameter Description
#    00010     Temperature, water, degrees Celsius
#    00060     Discharge, cubic feet per second
#    00065     Gage height, feet
#    00095     Specific conductance, water, unfiltered, microsiemens per centimeter at 25 degrees Celsius
#    00300     Dissolved oxygen, water, unfiltered, milligrams per liter
#    00301     Dissolved oxygen, water, unfiltered, percent of saturation

n<- dim(gage)[1]
for (i in 1:n){

base_url    <-'https://waterservices.usgs.gov/nwis';
site        <- gage$Site[i];
flowsite    <- gage$DischargeGage[i];
start_date  <- gage$startDT[i];
end_date    <- gage$endDT[i];
parameterCd <-'00010,00095,00300,00301';
FparameterCd<-'00060,00065';
sampleYR    <- gage$sampleYR[i]

###Combine into url for site#########
parts <- c(base_url,'/iv/?format=rdb',
                '&sites=',       site,
                '&startDT=',     start_date,
                '&endDT=',       end_date,
                '&parameterCd=', parameterCd,
                '&siteType=',    'ST',
                '&siteStatus=',   'all');

flowparts <- c(base_url,'/iv/?format=rdb',
           '&sites=',       flowsite,
           '&startDT=',     start_date,
           '&endDT=',       end_date,
           '&parameterCd=', FparameterCd,
           '&siteType=',    'ST',
           '&siteStatus=',   'all');

DO_url<-paste(parts,sep='',collapse='');
Flow_url<-paste(flowparts,sep='',collapse='');

DO<-parse_fstat(readLines(DO_url))
Flow<-parse_fstat(readLines(Flow_url))
Flow<- Flow[,3:length(colnames(Flow))]
DO<-merge(DO,Flow,by=c("datetime","tz_cd"))

##rename colnames##########
for (i in 5:length(colnames(DO))){
  colnames(DO)[i]<-substr(colnames(DO)[i],7,nchar(colnames(DO)[i]))
  ifelse(colnames(DO)[i]=="00010",colnames(DO)[i]<-"temp.water",
         ifelse(colnames(DO)[i]=="00095",colnames(DO)[i]<-"cond",
                ifelse(colnames(DO)[i]=="00300",colnames(DO)[i]<-"DO.obs",
                       ifelse(colnames(DO)[i]=="00301",colnames(DO)[i]<-"DO.sat",
                              ifelse(colnames(DO)[i]=="00065",colnames(DO)[i]<-"depth",
                                     ifelse(colnames(DO)[i]=="00060",colnames(DO)[i]<-"discharge", 
                                     colnames(DO)[i]<-colnames(DO)[i]))))))
}

DO$datetimePOS<- as.POSIXct(DO$datetime,format="%Y-%m-%d %H:%M", tz='America/New_York')
DO$solar.time<- calc_solar_time(DO$datetimePOS,longitude=-106.3)
DO<- DO[,c("site_no","solar.time","temp.water","discharge","depth","cond","DO.obs")]
DO<- DO[!is.na(DO$DO.obs),]
DO<- DO[(DO$DO.obs!=""),]

##convert data to numerics#######
for (i in 3:length(colnames(DO))){
  DO[,i]<- as.numeric(as.character(DO[,i]))
}

##convert gage height to meters##
DO$depth<- DO$depth*0.3048

write.csv(DO,paste("data/",site,"_",sampleYR,".csv",sep=""),row.names=FALSE)

}