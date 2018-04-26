library(shiny)
library(dynlm)
library(zoo)
library(stargazer)
reg<-readRDS("reg.rds")

# Define UI ----
ui <- fluidPage(
  
  headerPanel('Resultados de Regressão'),
  sidebarPanel(
    selectInput('vary', label= 'Selecione a variável dependente',
                choices = c("Despesa"=1, "Casos de dengue"=2,"Interesse"=3),
                selected = 1),
    selectInput('varx', label= 'Selecione a variável explicativa',
                choices = c("Despesa"=1, "Casos de dengue"=2,"Interesse"=3),
                selected = 2),
    sliderInput('range', label= 'Selecione o período desejado',
                   min=as.Date("2015-01-01"),
                   max=as.Date("2017-12-011"),value=c(as.Date("2015-01-01"),as.Date("2017-12-31")),timeFormat="%b %Y"),
    mainPanel(
      br(),
      br(),
      uiOutput("myreg",position="right")
    )
  ))



# Define server logic ----
server = function(input, output) {
  output$myreg <- renderUI({
    if(input$vary==1&input$varx==1){
      ys<-as.numeric(substr(input$range[1],1,4))
      ms<-as.numeric(substr(input$range[1],6,7))
      ye<-as.numeric(substr(input$range[2],1,4))
      me<-as.numeric(substr(input$range[2],6,7))
      score<-dynlm(VLIQ~VLIQ,data=reg,start=c(ys,ms),end=c(ye,me))
    }
    if(input$vary==2&input$varx==2){
      ys<-as.numeric(substr(input$range[1],1,4))
      ms<-as.numeric(substr(input$range[1],6,7))
      ye<-as.numeric(substr(input$range[2],1,4))
      me<-as.numeric(substr(input$range[2],6,7))
      score<-dynlm(Casos~Casos,data=reg,start=c(ys,ms),end=c(ye,me))
    }
    if(input$vary==3&input$varx==3){
      ys<-as.numeric(substr(input$range[1],1,4))
      ms<-as.numeric(substr(input$range[1],6,7))
      ye<-as.numeric(substr(input$range[2],1,4))
      me<-as.numeric(substr(input$range[2],6,7))
      score<-dynlm(Media~Media,data=reg,start=c(ys,ms),end=c(ye,me))
    }
    if(input$vary==1&input$varx==2){
      ys<-as.numeric(substr(input$range[1],1,4))
      ms<-as.numeric(substr(input$range[1],6,7))
      ye<-as.numeric(substr(input$range[2],1,4))
      me<-as.numeric(substr(input$range[2],6,7))
      score<-dynlm(VLIQ~Casos,data=reg,start=c(ys,ms),end=c(ye,me))
    }
    if(input$vary==1&input$varx==3){
      ys<-as.numeric(substr(input$range[1],1,4))
      ms<-as.numeric(substr(input$range[1],6,7))
      ye<-as.numeric(substr(input$range[2],1,4))
      me<-as.numeric(substr(input$range[2],6,7))
      score<-dynlm(VLIQ~Media,data=reg,start=c(ys,ms),end=c(ye,me))
    }
    if(input$vary==2&input$varx==1){
      ys<-as.numeric(substr(input$range[1],1,4))
      ms<-as.numeric(substr(input$range[1],6,7))
      ye<-as.numeric(substr(input$range[2],1,4))
      me<-as.numeric(substr(input$range[2],6,7))
      score<-dynlm(Casos~VLIQ,data=reg,start=c(ys,ms),end=c(ye,me))
    }
    if(input$vary==2&input$varx==3){
      ys<-as.numeric(substr(input$range[1],1,4))
      ms<-as.numeric(substr(input$range[1],6,7))
      ye<-as.numeric(substr(input$range[2],1,4))
      me<-as.numeric(substr(input$range[2],6,7))
      score<-dynlm(Casos~Media,data=reg,start=c(ys,ms),end=c(ye,me))
    }
    if(input$vary==3&input$varx==1){
      ys<-as.numeric(substr(input$range[1],1,4))
      ms<-as.numeric(substr(input$range[1],6,7))
      ye<-as.numeric(substr(input$range[2],1,4))
      me<-as.numeric(substr(input$range[2],6,7))
      score<-dynlm(Media~VLIQ,data=reg,start=c(ys,ms),end=c(ye,me))
    }
    if(input$vary==3&input$varx==2){
      ys<-as.numeric(substr(input$range[1],1,4))
      ms<-as.numeric(substr(input$range[1],6,7))
      ye<-as.numeric(substr(input$range[2],1,4))
      me<-as.numeric(substr(input$range[2],6,7))
      score<-dynlm(Media~Casos,data=reg,start=c(ys,ms),end=c(ye,me))
    }
    return(HTML((stargazer(score,type="html"))))
    
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)