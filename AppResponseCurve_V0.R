# Load packages -----------------------------------------------------------

packs <- c('data.table','shiny',"lubridate", "stringr", "ggplot2",'dplyr','viridis','plotly','tidyr','plantecophys','DT')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)



# inputs ------------------------------------------------------------------
all_params <- data.frame()
d_save <- data.frame()
color_outputs=c(1,grey(0.7))
names(color_outputs)=c('valid','outlier')

# ui ----------------------------------------------------------------------
ui<-fluidPage(
  titlePanel("Gas exchange response curves"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectizeInput(inputId = 'select_input', label = 'Choose the files (in Data)', choices = '*', multiple = TRUE),
      
      checkboxGroupInput("matos", label = h4("Equipment"), 
                         choices = list("Walz",'Licor 6800'),
                         selected ='Walz'),
      tags$hr(),
      uiOutput('ui.action')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data visualisation",
                 fluidRow(
                   column(12,
                          selectInput(inputId = 'curve_type', label = 'Select response curve', choices = '*', multiple = F),
                          plotlyOutput('graph')
                   )
                 )
                 
        ),
        tabPanel("Clean data & fit model",
                 fluidRow(
                   column(12,
                          selectizeInput(inputId = 'curve_input', label = 'Select a curve (Click on point to remove outlier)', choices = '*', multiple = F), 
                          tags$hr(),
                          checkboxInput(inputId = 'fit',label = "Fit model (only for A-Ci gs-VPD)",value = F),
                          tags$hr(),
                          plotlyOutput('clean_plot'),
                          tags$hr(),
                          actionButton("addFit", ("Add to paramaters table")),
                          tags$hr(),
                          tableOutput("FitTable")
                          
                          
                   )
                 )
        ),
        tabPanel("Parameters table",
                 fluidRow(
                   column(12,
                          # tableOutput("table")
                          DT::dataTableOutput("AllFitTable"),
                          downloadButton("downloadData", "Download")
                   )
                 )
        )
      )
      
    )
  )
)




# server ----------------------------------------------------------------------


server<-function(input, output,session){
  
  
  source(file = 'import_curves.R')
  source(file='Medlyn_fit.R')
  
  observe({
    files <- list.files('./Data/')
    
    updateSelectizeInput(session = session, inputId = 'select_input', choices = files)
  })
  
  observe({
    res <- res()
    if (is.null(res)) return(NULL)
    
    updateSelectInput(session = session, inputId = 'curve_type', choices = unique(res$Comment))
  })
  
  
  
  observe({
    res <- res()
    if (is.null(res)) return(NULL)
    cruvChoice=res%>%
      select(curve)%>%
      distinct()
    
    updateSelectInput(session = session, inputId = 'curve_input', choices = cruvChoice)
  })
  
  
  # inputs-----------------------------------------------------
  
  
  
  matos<- reactive({
    input$matos
  })
  
  type<- reactive({
    input$curve_type
  })
  
  curve_input<- reactive({
    input$curve_input
  })
  
  fit_input<- reactive({
    input$fit
  })
  
  res <- reactive({
    raw_data<- reactive({
      input$select_input
    })
    
    input$action
    
    isolate({
      raw_data <- raw_data()
      if (is.null(raw_data)) return(NULL)
      
      
      res=NULL
      for (i in raw_data){
        sub=import.curves(filename =paste0('./Data/',i))%>%
          mutate(id=str_remove(string = i,'.csv'))
        res=rbind(res,sub)
      }
      res=res%>%
        mutate(curve=paste(Comment,id))
      return(res)
      
    })
  })
  
  curvId <- reactive({
    res<-res()
    if (is.null(res)) return(NULL)
    
    curve_input<- curve_input()
    if (is.null(curve_input)) return(NULL)
    
    curvId=res%>%
      filter(curve==curve_input)%>%
      select(id,Time,Comment,curve,A,gs,ci,PARtop,VPD_kPa,Tleaf,ca)
    
    return(curvId)
  })
  
  
  # tables-----------------------------------------------------
  # output$table <-  renderTable({res()})
  
  
  # visualisation-----------------------------------------------------
  output$graph<-renderPlotly({
    res<-res()
    if (is.null(res)) return(NULL)
    
    type<- type()
    if (is.null(type)) return(NULL)
    
    gr=NULL
    x_var=NULL
    y_var=NULL
    if(type=='CO2 Curve'){
      x_var='ci'
      y_var='A'
      xlab='Ci  (ppm)'
      ylab='Assimilation  (micromol.m-2.s-1)'
    }
    if(type=='ligth Curve'){
      x_var='PARtop'
      y_var='A'
      xlab='PPFD  (micromol.m-2.s-1)'
      ylab='Assimilation  (micromol.m-2.s-1)'
    }
    if(type=='Rh Curve'){
      x_var='VPD_kPa'
      y_var='gs'
      xlab='Vapor pressure deficit (kPa)' 
      ylab='Stomatal conductance  (mol.m-2.s-1)'
    }
    
    if (!is.null(x_var) & !is.null(y_var)){
      gr=res%>%
        filter(Comment==type)%>%
        ggplot(aes(x=get(x_var),y=get(y_var),col=id,group=id))+
        geom_smooth(alpha=0.2,se=F,method='loess')+
        geom_point()+
        ylab(ylab)+
        xlab(xlab)
    }
    
    
    
    if(!is.null(gr)){
      ggplotly(gr)
    }
    
  })
  
  # clean & fit -----------------------------------------------------
  
  output$clean_plot <- renderPlotly({
    cleanFit<-cleanFit()
    if (is.null(cleanFit)) return(NULL)
    return(cleanFit$graph)
  })
  
  output$AllFitTable = renderDataTable(FitParams(),rownames= FALSE)
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste('Parameters.csv', sep = "")
    },
    content = function(file) {
      # write.csv(FitParams(), file, row.names = FALSE)
      data.table::fwrite(FitParams(), file, row.names = FALSE)
    }
  )
  
  output$FitTable = renderTable({
    cleanFit<-cleanFit()
    if (is.null(cleanFit)) return(NULL)
    return(cleanFit$params)
  })
  
  
  FitParams = eventReactive(input$addFit,{
    cleanFit<-cleanFit()
    all_params <<- rbind(all_params,cleanFit$params)
    return(all_params)
  })
  
  # output$clean_plot <- renderPlotly({
  cleanFit <- reactive({
    
    x_var_cl=NULL
    y_var_cl=NULL
    gr_clean=NULL
    out=NULL
    params=NULL
    
    curvId<-curvId()
    if (is.null(curvId)) return(NULL)
    
    fit_input<-fit_input()
    
    
    if(!is.null(event_data("plotly_click"))){
      d <- as.data.frame(event_data("plotly_click"))%>%
        mutate(id=unique(curvId$id),
               Comment=unique(curvId$Comment),
               curve=unique(curvId$curve))%>%
        select(id,Comment,curve,x,y)
      d_save <<- rbind(d_save,d)
      
      # d_save=d_save%>%
      #   group_by(id,Comment,curve,x,y)%>%
      #   mutate(n=n())%>%
      #   mutate(outlier=ifelse(n%%2==0,'valid','outlier'))
    }
    
    if(unique(curvId$Comment)=='CO2 Curve'){
      x_var_cl='ci'
      y_var_cl='A'
      xlab='Ci  (ppm)'
      ylab='Assimilation  (micromol.m-2.s-1)'
      if(nrow(d_save)>0){
        sub=d_save%>%
          filter(curve==unique(curvId$curve))%>%
          mutate(ci=x,
                 A=y)%>%
          select(curve,A,ci)
        out=merge(sub,curvId,all.x=T)
      }
    }
    if(unique(curvId$Comment)=='ligth Curve'){
      x_var_cl='PARtop'
      y_var_cl='A'
      xlab='PPFD  (micromol.m-2.s-1)'
      ylab='Assimilation  (micromol.m-2.s-1)'
      if(nrow(d_save)>0){
        sub=d_save%>%
          filter(curve==unique(curvId$curve))%>%
          mutate(PARtop=x,
                 A=y)%>%
          select(curve,A,PARtop)
        out=merge(sub,curvId,all.x=T)
      }
    }
    if(unique(curvId$Comment)=='Rh Curve'){
      x_var_cl='VPD_kPa'
      y_var_cl='gs'
      xlab='Vapor pressure deficit (kPa)' 
      ylab='Stomatal conductance  (mol.m-2.s-1)'
      if(nrow(d_save)>0){
        sub=d_save%>%
          filter(curve==unique(curvId$curve))%>%
          mutate(VPD_kPa=x,
                 gs=y)%>%
          select(curve,VPD_kPa,gs)
        
        out=merge(sub,curvId,all.x=T)
      }
    }
    
    
    
    if (!is.null(x_var_cl) & !is.null(y_var_cl)){
      
      curve_out=curvId%>%
        mutate(outliers='valid')
      
      if(!is.null(out)){
        curve_out=curvId%>%
          mutate(outliers=ifelse(curvId[,x_var_cl] %in% out[,x_var_cl] & curvId[,y_var_cl] %in% out[,y_var_cl]  ,'outlier', 'valid'))
      }
      
      
      gr_clean=ggplotly(ggplot()+
                          geom_point(data=curve_out,aes(x=get(x_var_cl),y=get(y_var_cl),col=outliers))+
                          scale_color_manual(name='outputs',values = color_outputs)+
                          ylab(ylab)+
                          xlab(xlab))
      
      gr_clean%>%add_trace(x = curvId[,x_var_cl], y=curvId[,y_var_cl], mode = "markers")
      
    }
    
    
    if (!is.null(x_var_cl) & !is.null(y_var_cl) & fit_input==T){
      
      if (unique(curvId$Comment)=='CO2 Curve'){
        
        fitAci=curve_out%>%
          filter(outliers=='valid')%>%
          mutate(CO2S=ca,
                 Ci=get(x_var_cl),
                 Tleaf=Tleaf,
                 Photo=get(y_var_cl),
                 PARi=PARtop)%>%
          select(CO2S,Ci,Tleaf,Photo,PARi)
        
        ftpu <- fitaci(fitAci, fitTPU=TRUE, PPFD=1800, Tcorrect=TRUE)
        
        params=data.frame(id=unique(curve_out$id),
                          curve=unique(curve_out$Comment),
                          Vcmax=ftpu$pars['Vcmax','Estimate'],
                          Jmax=ftpu$pars['Jmax','Estimate'],
                          TPU=ftpu$pars['TPU','Estimate'],
                          Rd=ftpu$pars['Rd','Estimate'],
                          g0=NA,
                          g1=NA)
        
        ci_sim=c(seq(0,1500,10))
        
        
        simACi=Photosyn(Ci = ci_sim,Vcmax=params$Vcmax,Jmax=params$Jmax,TPU=params$TPU,Rd=params$Rd,Tleaf=mean(ftpu$df$Tleaf),PPFD=mean(ftpu$df$PPFD),Tcorrect = T,VPD = mean(ftpu$df$VPD),Patm = mean(ftpu$df$Patm))%>%
          group_by(Ci)%>%
          mutate(Ap=ifelse(Ap>100,NA,Ap),
                 A_sim=min(c(Aj-Rd,Ac-Rd,Ap-Rd),na.rm=T))

        gr_clean=ggplotly(ggplot()+
                            geom_line(data=simACi,aes(x=Ci,y=Ac-Rd,col='Ac'),lwd=1,lty=2)+
                            geom_line(data=simACi,aes(x=Ci,y=Aj-Rd,col='Aj'),lwd=1,lty=2)+
                            geom_line(data=simACi,aes(x=Ci,y=Ap-Rd,col='Ap'),lwd=1,lty=2)+
                            geom_line(data=simACi,aes(x=Ci,y=A_sim,col='A sim'),lwd=1.2)+
                            geom_point(data=ftpu$df,aes(x=Ci,y=Ameas,shape='Obs'))+
                            ylab(ylab)+
                            xlab(xlab)+
                            xlim(range(curve_out[,x_var_cl]))+
                            ylim(range(curve_out[,y_var_cl])))
      }
      
      if (unique(curvId$Comment)=='Rh Curve'){
        
        fitgs=curve_out%>%
          filter(outliers=='valid')%>%
          mutate(CO2S=ca,
                 Ci=get(x_var_cl),
                 Tleaf=Tleaf,
                 Photo=get(y_var_cl),
                 PARi=PARtop)%>%
          select(ca,VPD_kPa,A,gs)
        
        ## plantecophys fonction
        Medlyn <-fitBB(data = fitgs,gsmodel = 'BBOpti',varnames = list(ALEAF = "A", GS = "gs", VPD = "VPD_kPa",Ca = "ca"),fitg0=T)
        fitgs$gs_sim <- predict(Medlyn$fit, fitgs)
        
        
        params=data.frame(id=unique(curve_out$id),
                          curve=unique(curve_out$Comment),
                          Vcmax=NA,
                          Jmax=NA,
                          TPU=NA,
                          Rd=NA,
                          g0=coef(Medlyn)['g0'],
                          g1=coef(Medlyn)['g1'])

        ## perso 
        # Medlyn <- fit.Medlyn(data = fitgs)
        # 
        # params=data.frame(id=unique(curve_out$id),
        #                   curve=unique(curve_out$Comment),
        #                   Vcmax=NA,
        #                   Jmax=NA,
        #                   TPU=NA,
        #                   Rd=NA,
        #                   g0=Medlyn$param['g0'],
        #                   g1=Medlyn$param['g1'])
        
        
        gr_clean=ggplotly(fitgs%>%
                            ggplot()+
                            geom_line(aes(x=VPD_kPa,y=gs_sim,col='gs sim'),lwd=1.2)+
                            geom_point(aes(x=VPD_kPa,y=gs,shape='Obs'))+
                            ylab(ylab)+
                            xlab(xlab)+
                            xlim(range(curve_out[,x_var_cl]))+
                            ylim(range(curve_out[,y_var_cl]))
        )
      }
    }
    
    gr_clean=gr_clean%>%
      layout(legend = list(orientation = "h",y = 1.1))
    
    output_fit=list(graph=gr_clean,params=params)
    return(output_fit)
  })
  
  
  
  output$ui.action <- renderUI({
    actionButton("action", "Load data")
  })
  
  # output$ui.fit <- renderUI({
  #   actionButton("fit", "Fit data")
  # })
  
  
  
  
  
  
}
# Run app -------------------------------
shinyApp(ui = ui, server = server)