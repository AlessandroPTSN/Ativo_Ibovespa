library(rvest)
library(ggfortify)
library(ggdendro)
library(plotly)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(readxl)
library(tidyr)
library(shinythemes)
library(stringr)
library(kableExtra)
library(highcharter)
library(ggplot2)
library(tidyverse)


##########################
### obtencao dos dados ###
##########################

#https://www.infomoney.com.br/cotacoes/ibovespa/historico/

#https://www.datanovia.com/en/lessons/highchart-interactive-bar-plot-in-r/



url <- "https://br.investing.com/indices/bovespa-historical-data"
zzz= read_html(url) %>%
  html_nodes("tr") %>%
  html_text()%>%str_replace("%\n", "")%>%str_replace("\n", " ")%>%str_replace("M", "")

zzz=gsub("\n", " ",zzz[2:20])

zzz=read.table(text=zzz,col.names= c("Data" ,"Ultimo", "Abertura", "Maxima" ,"Minima" ,"Vol" ,"Var"))
zzz[,6]=zzz[,6]%>%str_replace(",", ".")
zzz[,7]=zzz[,7]%>%str_replace(",", ".")
zzz[,1]= as.Date(zzz[,1], "%d.%m.%Y")
for(i in 2:7){
  zzz[,i]=as.numeric(zzz[,i])
}


yyy= zzz


yyy[,2] = c(yyy[1:19,2])
yyy[,6] = yyy[,5] 
yyy[,5] = yyy[,3]
yyy[,3] = yyy[,4]
yyy=yyy[,-c(4,5)]
yyy= yyy[, c(1, 2, 3, 5,4)]
yyy$Maximo = as.numeric(as.matrix(zzz[4]))
colnames(yyy) <- c("Data","Abertura","Fechamento","Variacao","Minimo","Maximo")
#yyy = yyy[-19,]





url2 <- "https://br.investing.com/equities/imc-holdings-on-historical-data"
zzz2= read_html(url2) %>%
  html_nodes("tr") %>%
  html_text()%>%str_replace("%\n", "")%>%str_replace("\n", " ")#%>%str_replace("M", "")

zzz2=gsub("\n", " ",zzz2[2:20])

zzz2=read.table(text=zzz2,col.names= c("Data" ,"Ultimo", "Abertura", "Maxima" ,"Minima" ,"Vol" ,"Var"))
for( i in 2:7){
  zzz2[,i]=zzz2[,i]%>%str_replace(",", ".")
}

zzz2[,1]= as.Date(zzz2[,1], "%d.%m.%Y")
for(i in c(2,3,4,5,7)){
  zzz2[,i]=as.numeric(zzz2[,i])
}

yyy2= zzz2

#yyy2[,2] = c(yyy2[2:20,2],0)
yyy2[,6] = yyy2[,5] 
yyy2[,5] = yyy2[,3]
yyy2[,3] = yyy2[,4]
yyy2=yyy2[,-c(4,5)]
yyy2= yyy2[, c(1, 2, 3, 5,4)]
yyy2$Maximo = as.numeric(as.matrix(zzz2[4]))
colnames(yyy2) <- c("Data","Abertura","Fechamento","Variacao","Minimo","Maximo")
yyy2 = yyy2[-20,]








dif= yyy2$Variacao-yyy$Variacao
difs <- data.frame(matrix(ncol = 11, nrow = 0))
names = c("< -4.5","-4.5_-3.5","-3.5_-2.5","-2.5_-1.5","-1.5_-0.5","-0.5_0.5","-0.5_1.5","1.5_2.5","2.5_3.5","3.5_4.5","4.5>")
colnames(difs) <- names


for(i in 1:length(dif)){
  if(dif[i]<= (-4.5)){
    difs[i,1] = 1
  }
  if((dif[i]> (-4.5))&(dif[i]<= (-3.5))){
    difs[i,2] = 1
  }
  if((dif[i]> (-3.5))&(dif[i]<= (-2.5))){
    difs[i,3] = 1
  }
  if((dif[i]> (-2.5))&(dif[i]<= (-1.5))){
    difs[i,4] = 1
  }  
  if((dif[i]> (-1.5))&(dif[i]<= (-0.5))){
    difs[i,5] = 1
  }  
  if((dif[i]> (-0.5))&(dif[i]<= (0.5))){
    difs[i,6] = 1
  } 
  if((dif[i]> (0.5))&(dif[i]<= (1.5))){
    difs[i,7] = 1
  }
  if((dif[i]> (1.5))&(dif[i]<= (2.5))){
    difs[i,8] = 1
  }
  if((dif[i]> (2.5))&(dif[i]<= (3.5))){
    difs[i,9] = 1
  }
  if((dif[i]> (3.5))&(dif[i]<= (4.5))){
    difs[i,10] = 1
  }
  if(dif[i]> (4.5)){
    difs[i,11] = 1
  }
  
}

#difs
difs[is.na(difs)]<-0
#difs
difs_s =data.frame(matrix(ncol = 12, nrow = 19))
difs_s[,1] = dif
difs_s[,2:12] = difs
colnames(difs_s) <- c("Diferença",as.character(c(-5:5)))

test =c(sum(difs[,1])/sum(difs),
        sum(difs[,2])/sum(difs),
        sum(difs[,3])/sum(difs),
        sum(difs[,4])/sum(difs),
        sum(difs[,5])/sum(difs),
        sum(difs[,6])/sum(difs),
        sum(difs[,7])/sum(difs),
        sum(difs[,8])/sum(difs),
        sum(difs[,9])/sum(difs),
        sum(difs[,10])/sum(difs),
        sum(difs[,11])/sum(difs) )

testy<- data.frame(matrix(ncol = 11, nrow = 0))
colnames(testy) <- names
testy[1,]=test

testo = data.frame(round(test,3),names)

testa = data.frame(dif,yyy$Data)

test2 =c(sum(difs[,1]),
         sum(difs[,2]),
         sum(difs[,3]),
         sum(difs[,4]),
         sum(difs[,5]),
         sum(difs[,6]),
         sum(difs[,7]),
         sum(difs[,8]),
         sum(difs[,9]),
         sum(difs[,10]),
         sum(difs[,11]) )

tabelaa = data.frame(names,test2,test)
colnames(tabelaa) <- c("Intervalo","Frequencia","Porcentagem")











#39057272 (460×460)




shinyServer(function(input, output, session) {

  output$img <- renderUI({
    tags$img(src = "https://github.com/alessandroptsn.png", height = 208)
  })
  
  
  output$img2 <- renderUI({
    tags$img(src = "https://github.com/felipesergios.png", height = 200)
  })
  
  
  
  
  outVar1 <- reactive({
    reshape2::melt(yyy, id.vars = "Data")%>%
      filter(variable %in% c(input$All))%>%mutate(variable = paste(variable, "Ibovespa")) 
  })
  
  outVar2 <- reactive({
    reshape2::melt(yyy2, id.vars = "Data")%>%
      filter(variable %in% c(input$All2))%>%mutate(variable = paste(variable, "Ativo")) 
  })
  
  output$plot <- renderPlotly({
    plot_ly(data=outVar1(), x=~Data,  y = ~value,
            type = 'scatter', mode = 'lines', legendgroup = "1",color = ~variable
    ) %>%
      add_trace(data=outVar2(), x=~Data,  y = ~value,
                type = 'scatter', mode = 'lines', legendgroup = "2",color = ~variable)  %>%
      layout(legend = list(orientation = 'v'))         
  }) 
  
  output$chart2 <- renderHighchart({
    hchart(rbind(outVar1(), outVar2()), "line", hcaes(Data, value, group = variable), regression = TRUE) %>% 
      #hc_colors(c("#d35400", "#2980b9")) %>% 
      #hc_colors(c("#5856d6","#ff9500","#ffcc00","#ff3b30","#5ac8fa")) %>% 
      #hc_add_dependency("plugins/highcharts-regression.js")%>% 
      hc_title(text = "Análise comparativa entre Ibovespa x Ativo") %>% 
      hc_xAxis(title= list(text= "Dias ")) %>%
      hc_yAxis(title= list(text="Variação ")) %>% 
      hc_add_theme(hc_theme_darkunica()) #%>% 
    #hc_add_dependency("plugins/highcharts-regression.js")
    
  }) 
  
  
  output$table_Regiaooo2<- function() {  
    yyy%>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F)
  }
  
  output$table_Regiaooo22<- function() {  
    yyy2%>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F)
  }
  
  
  output$tabelaaa<- function() {  
    tabelaa%>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F)
  }
  
  output$chart_1 <- renderHighchart({
    hchart(cut(yyy$Variacao, breaks=c(-Inf, -4.5,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,4.5,Inf), include.lowest=TRUE),'column')%>% 
      hc_title(text = "Histograma variação Ibovespa") %>% 
      hc_xAxis(title= list(text= "Intervalos ")) %>%
      hc_yAxis(title= list(text="Contagem ")) %>% 
      hc_colors(c("#d35400")) %>% 
      hc_add_theme(hc_theme_darkunica())
  }) 
  output$chart_2 <- renderHighchart({
    hchart(cut(yyy2$Variacao, breaks=c(-Inf, -4.5,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,4.5,Inf), include.lowest=TRUE),'column')%>% 
      hc_title(text = "Histograma variação Ativo") %>% 
      hc_xAxis(title= list(text= "Intervalos ")) %>%
      hc_yAxis(title= list(text="Contagem ")) %>% 
      hc_colors(c("#2980b9")) %>%
      hc_add_theme(hc_theme_darkunica())
  }) 
  
  output$chart_3 <- renderHighchart({
    #hchart(cut(yyy$Variacao-yyy2$Variacao, breaks=c(-Inf, -4.5,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,4.5,Inf), include.lowest=TRUE),"column")
    #s? essa linha d? exatamente tudo que ele queria
    hchart(testo,"column", hcaes(x = names, y = round.test..3.))%>% 
      hc_title(text = "Histograma diferença de variação entre Ativo - Ibovespa") %>% 
      hc_xAxis(title= list(text= "Intervalos ")) %>%
      hc_yAxis(title= list(text="Porcentagem ")) %>% 
      hc_colors(c("#5856d6")) %>%
      hc_add_theme(hc_theme_darkunica())
  })
  
  output$chart_4 <- renderHighchart({
    hchart(testa, "line", hcaes(yyy.Data, dif), regression = TRUE) %>% 
      hc_title(text = "Diferenca entre vaciações Ativo - Ibovespa") %>% 
      hc_xAxis(title= list(text= "Dias ")) %>%
      hc_yAxis(title= list(text="Diferença de variação ")) %>% 
      hc_add_theme(hc_theme_darkunica())
  })
  
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("Ibovespa ",str_replace_all(as.character(Sys.time()),":","_"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(as.data.frame(yyy), file, row.names = FALSE)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("Ativo ",str_replace_all(as.character(Sys.time()),":","_"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(as.data.frame(yyy2), file, row.names = FALSE)
    }
  )
  
  
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste("Intervalo_Frequencia ",str_replace_all(as.character(Sys.time()),":","_"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(as.data.frame(tabelaa), file, row.names = FALSE)
    }
  )
  
  
  
  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste("Diferencas ",str_replace_all(as.character(Sys.time()),":","_"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(as.data.frame(difs_s), file, row.names = FALSE)
    }
  ) 

})
