# Load packages -----------------------------------------------------------

packs <- c("lubridate", "stringr", "ggplot2",'dplyr','viridis','plotly','tidyr')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)




#' import gas exhange data
#'
#' @param filename : name of the data file to import
#' @param last_point: if True only keep the last point measured in the curve step
#'
#' @return formated data 
#' @export
#'
#' @examples
import.curves=function(filename,last_point=T){
  # filename='./Data/P5F70323.csv'
  head=str_split(string = readLines(con =filename ,n = 1),pattern = ';')[[1]]
  don_raw = data.table::fread(filename, header = FALSE, skip =2, data.table = FALSE)
  colnames(don_raw)=head
  
  colnames(don_raw)[colnames(don_raw) %in% c('Tmin','Ttop')]='Tmin'
  
  don=don_raw%>%
    mutate(Date=ymd(Date),
           Time=hms(Time),
           VPD_kPa=round(VPD*Pamb/1000,3), # see doc Walz
           gs=round(GH2O/1000,5),
           AVPD=A/(ca*sqrt(VPD_kPa)),
           Comment=ifelse(Comment=='',NA,Comment))%>%
    fill(Comment)%>%
    filter(!is.na(Comment))
  
  ##" problem in date format---->fucking excel
  if(is.na(don$Date)[1]){
    don=don_raw%>%
      mutate(Date=dmy(Date),
             Time=hms(Time),
             VPD_kPa=round(VPD/10,3),
             AVPD=A/(ca*sqrt(VPD_kPa)),
             gs=round(GH2O/1000,5),
             Comment=ifelse(Comment=='',NA,Comment))%>%
      fill(Comment)%>%
      filter(!is.na(Comment))
  }
  
  if(last_point==T){
    don=don%>%
      filter(!is.na(ETR))
    }
  
  return(don)
}

 # don=import.curves(filename='./Data/P1F20129.csv')
 # curves=unique(don$Comment)
 # 
 # 
 # test=don[1:5,]
 #   
 # out=don[3,]
 # 
 # var='ci'
 # 
 # don_out=test%>%
 #   mutate(outlier=ifelse(test[,var] %in% out[,var] & don[,'A'] %in% out[,'A'],'True', 'False'))
# 
# don%>%
#   filter(Comment==curves[1])%>%
#   ggplot(aes(x=ci,y=A))+
#   geom_point()
# 
# don%>%
#   filter(Comment==curves[2])%>%
#   ggplot(aes(x=PARtop,y=A))+
#   geom_point()
# 
# don%>%
#   filter(Comment==curves[3])%>%
#   ggplot(aes(x=VPD,y=A))+
#   geom_point()
