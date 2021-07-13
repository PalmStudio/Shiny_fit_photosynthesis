
packs <- c('dplyr','tidyr','data.table')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)



#' Medlyn stoamtal conductance model (Medlyn et al, 2011)
#' https://onlinelibrary.wiley.com/doi/pdf/10.1111/j.1365-2486.2010.02375.x?casa_token=e2UDOkjn7H8AAAAA:5RoNvjDdbHaJqJZJJh8Dl8gzVW9IF2zd7r9ZftpVzjsQEVUlPHijyKXVvCMI993m-FIzFaWd4R-5Bus
#'
#' @param g0 (mol.m-2.s-1)
#' @param g1 (dimensionless)
#' @param VPD vapor pressur deficit (kPa)
#' @param Photo Accimilation (micromol.m-2.s-1)
#' @param Ca Atmospheric C concetration (ppm)
#'
#' @return gs stomatal conductance (mol.m-2.s-1)
#' @export
#'
#' @examples
f.Medlyn=function(g0,g1,VPD,Photo,Ca=400){
  gs = g0 + 1.6*(1 + g1/sqrt(VPD)) * (Photo/Ca)
  return(gs)
}






#' Fitting Medlyn model on gs-VPD  observations
#'
#' @param data : data fame with gs, VPD, A and Ca values
#' @param g0_start 
#' @param g1_start 
#'
#' @return
#' @export
#'
#' @examples
fit.Medlyn=function(data,g0_start=0.0033,g1_start=12.5){
  
  Fit_Medlyn= nls(gs ~f.Medlyn(g0=g0,g1=g1,VPD = VPD_kPa,Photo = A,Ca = ca), 
                  data= data, start = list(g0= g0_start, g1= g1_start))
  
  Medlyn_param=coef(Fit_Medlyn)
  
  df=data%>%
    mutate(g0=Medlyn_param[1],
           g1=Medlyn_param[2],
           gs_sim=f.Medlyn(g0 =g0,g1 = g1,VPD=VPD_kPa,Photo = A,Ca = ca ))%>%
    select(VPD_kPa,A,ca,gs,g0,g1,gs_sim)
  
  fit_med=list(param=Medlyn_param,
               df=df)
    
}



# debug -------------------------------------------------------------------

# source(file = 'import_curves.R')

# test=import.curves(filename='./0-data/P1F20129.csv')%>%
#   filter(Comment=='Rh Curve')
# 
# out=fit.Medlyn(data = test)
# 
# out$df%>%
# ggplot()+
#   geom_point(aes(x=VPD_kPa,y=gs,col='obs'))+
#   geom_line(aes(x=VPD_kPa,y = gs_sim,col='sim'))


