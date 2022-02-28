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
library(reshape2)
library(DT)



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


Ibo = read.csv("/app/Ibo.csv")
#yyy = novo
#Ibo = antigo
#Inverte
m = length(Ibo[,1])
Ibo = Ibo %>% map_df(rev)
Ibo = as.data.frame(Ibo)
Ibo$Data=as.Date(Ibo$Data)
j=1
while((Ibo[m,1] != yyy[j,1])&(Ibo[m,4] != yyy[j,4])){
  j=j+1
}
#Adiciona novas celulas
if(j>1){
for(i in 1:(j-1)){
  Ibo[(m+i),] = yyy[j-i,]
}}
if(j==1){
  Ibo[m,] = yyy[1,]
}
#Inverte ao normal
Ibo = Ibo %>% map_df(rev)
Ibo = as.data.frame(Ibo)
Ibo$Data=as.Date(Ibo$Data)
#Apaga demais celulas extras
if(j>1){
  Ibo = Ibo[-c(c(1:j-1)[-1])-m,]
}
write.csv(Ibo, "/app/Ibo.csv", row.names = FALSE, quote = TRUE)




Ati = read.csv("/app/Ati.csv")
#yyy2 = novo
#Ati = antigo
#Inverte
n = length(Ati[,1])

Ati = Ati %>% map_df(rev)
Ati = as.data.frame(Ati)
Ati$Data=as.Date(Ati$Data)
tail(Ati)
head(yyy2)
j=1
while((Ati[n,1] != yyy2[j,1])&(Ati[n,4] != yyy2[j,4])){
  j=j+1
}
#Adiciona novas celulas
if(j>1){
  for(i in 1:(j-1)){
    Ati[(n+i),] = yyy2[j-i,]
  }}

if(j==1){
  Ati[n,] = yyy2[1,]
}

#Inverte ao normal
Ati = Ati %>% map_df(rev)
Ati = as.data.frame(Ati)
Ati$Data=as.Date(Ati$Data)
#Apaga demais celulas extras
if(j>1){
  Ati = Ati[-c(c(1:j-1)[-1])-n,]
}
write.csv(Ati, "/app/Ati.csv", row.names = FALSE, quote = TRUE)



yyy2=Ati
yyy=Ibo



tabela_I = yyy[,-c(5,6)]


tabela_I=
  tabela_I%>% select(Data, Abertura, Fechamento, Variacao)%>%
  mutate(Variacao_ = case_when( 
    as.numeric(Variacao)>4.5   ~ 5,
    ((as.numeric(Variacao)<4.5)&(as.numeric(Variacao)>=3.51))   ~ 4,
    ((as.numeric(Variacao)<3.5)&(as.numeric(Variacao)>=2.51))   ~ 3,
    ((as.numeric(Variacao)<2.5)&(as.numeric(Variacao)>=1.51))   ~ 2,
    ((as.numeric(Variacao)<1.5)&(as.numeric(Variacao)>=0.51))   ~ 1,
    ((as.numeric(Variacao)<0.5)&(as.numeric(Variacao)>=-0.5))   ~ 0,
    ((as.numeric(Variacao)> -1.5)&(as.numeric(Variacao)<=-0.5))   ~ -1,
    ((as.numeric(Variacao)> -2.5)&(as.numeric(Variacao)<=-1.5))   ~ -2,
    ((as.numeric(Variacao)> -3.5)&(as.numeric(Variacao)<=-2.5))   ~ -3,
    ((as.numeric(Variacao)> -4.5)&(as.numeric(Variacao)<=-3.5))   ~ -4,
    (as.numeric(Variacao)< -4.5)  ~ -5
  ))

tabela_I=
  tabela_I%>% select(Data, Abertura, Fechamento, Variacao,Variacao_)%>%
  mutate(count = case_when( 
    as.numeric(Variacao_)==5   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==5])),
    as.numeric(Variacao_)==4   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==4])),
    as.numeric(Variacao_)==3   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==3])),
    as.numeric(Variacao_)==2   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==2])),
    as.numeric(Variacao_)==1   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==1])),
    as.numeric(Variacao_)==0   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==0])),
    as.numeric(Variacao_)==-5   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==-5])),
    as.numeric(Variacao_)==-4   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==-4])),
    as.numeric(Variacao_)==-3   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==-3])),
    as.numeric(Variacao_)==-2   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==-2])),
    as.numeric(Variacao_)==-1   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==-1]))
  ))






tabela_A = yyy2[,-c(5,6)]



tabela_A=
  tabela_A%>% select(Data, Abertura, Fechamento, Variacao)%>%
  mutate(Variacao_ = case_when( 
    as.numeric(Variacao)>4.5   ~ 5,
    ((as.numeric(Variacao)<4.5)&(as.numeric(Variacao)>=3.51))   ~ 4,
    ((as.numeric(Variacao)<3.5)&(as.numeric(Variacao)>=2.51))   ~ 3,
    ((as.numeric(Variacao)<2.5)&(as.numeric(Variacao)>=1.51))   ~ 2,
    ((as.numeric(Variacao)<1.5)&(as.numeric(Variacao)>=0.51))   ~ 1,
    ((as.numeric(Variacao)<0.5)&(as.numeric(Variacao)>=-0.5))   ~ 0,
    ((as.numeric(Variacao)> -1.5)&(as.numeric(Variacao)<=-0.5))   ~ -1,
    ((as.numeric(Variacao)> -2.5)&(as.numeric(Variacao)<=-1.5))   ~ -2,
    ((as.numeric(Variacao)> -3.5)&(as.numeric(Variacao)<=-2.5))   ~ -3,
    ((as.numeric(Variacao)> -4.5)&(as.numeric(Variacao)<=-3.5))   ~ -4,
    (as.numeric(Variacao)< -4.5)  ~ -5
  ))


tabela_A=
  tabela_A%>% select(Data, Abertura, Fechamento, Variacao,Variacao_)%>%
  mutate(count = case_when( 
    as.numeric(Variacao_)==5   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==5])),
    as.numeric(Variacao_)==4   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==4])),
    as.numeric(Variacao_)==3   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==3])),
    as.numeric(Variacao_)==2   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==2])),
    as.numeric(Variacao_)==1   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==1])),
    as.numeric(Variacao_)==0   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==0])),
    as.numeric(Variacao_)==-5   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==-5])),
    as.numeric(Variacao_)==-4   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==-4])),
    as.numeric(Variacao_)==-3   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==-3])),
    as.numeric(Variacao_)==-2   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==-2])),
    as.numeric(Variacao_)==-1   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==-1]))
  ))






ativo_ibovespa = yyy2[,-c(2,3,5,6)]
ativo_ibovespa$Variacao = ativo_ibovespa$Variacao -yyy[,4]



ativo_ibovespa=
  ativo_ibovespa%>% select(Data, Variacao)%>%
  mutate(Variacao_ = case_when( 
    as.numeric(Variacao)>4.5   ~ 5,
    ((as.numeric(Variacao)<4.5)&(as.numeric(Variacao)>=3.51))   ~ 4,
    ((as.numeric(Variacao)<3.5)&(as.numeric(Variacao)>=2.51))   ~ 3,
    ((as.numeric(Variacao)<2.5)&(as.numeric(Variacao)>=1.51))   ~ 2,
    ((as.numeric(Variacao)<1.5)&(as.numeric(Variacao)>=0.51))   ~ 1,
    ((as.numeric(Variacao)<0.5)&(as.numeric(Variacao)>=-0.5))   ~ 0,
    ((as.numeric(Variacao)> -1.5)&(as.numeric(Variacao)<=-0.5))   ~ -1,
    ((as.numeric(Variacao)> -2.5)&(as.numeric(Variacao)<=-1.5))   ~ -2,
    ((as.numeric(Variacao)> -3.5)&(as.numeric(Variacao)<=-2.5))   ~ -3,
    ((as.numeric(Variacao)> -4.5)&(as.numeric(Variacao)<=-3.5))   ~ -4,
    (as.numeric(Variacao)< -4.5)  ~ -5
  ))

ativo_ibovespa=
  ativo_ibovespa%>%select(Data, Variacao,Variacao_)%>%
  mutate(count = case_when( 
    as.numeric(Variacao_)==5   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==5])),
    as.numeric(Variacao_)==4   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==4])),
    as.numeric(Variacao_)==3   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==3])),
    as.numeric(Variacao_)==2   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==2])),
    as.numeric(Variacao_)==1   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==1])),
    as.numeric(Variacao_)==0   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==0])),
    as.numeric(Variacao_)==-5   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==-5])),
    as.numeric(Variacao_)==-4   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==-4])),
    as.numeric(Variacao_)==-3   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==-3])),
    as.numeric(Variacao_)==-2   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==-2])),
    as.numeric(Variacao_)==-1   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==-1]))
  ))








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
  
  
  output$chart22 <- renderHighchart({
  hchart(as.factor(ativo_ibovespa$Variacao_), type = "column",series.showInLegend = F)%>%
    hc_chart(events = list(load = JS("function() {

    this.series.forEach(function(series) {
      if (series.name === 'Series 1') {
        series.update({
          showInLegend: false
        });
      }
    });

  }")))  %>% 
    hc_title(text = "Contagem variação ações Ativo - Ibovespa") %>% 
    hc_xAxis(title= list(text= "Variação")) %>%
    hc_yAxis(title= list(text="Contagem")) %>% 
    hc_add_theme(hc_theme_darkunica())
  }) 
  
  output$chart2 <- renderHighchart({
  hchart(rbind(outVar1(), outVar2()), "line", hcaes(Data, value, group = variable), regression = TRUE) %>% 
    hc_title(text = "Análise comparativa entre Ibovespa x Ativo") %>% 
    hc_xAxis(title= list(text= "Dias ")) %>%
    hc_yAxis(title= list(text="Variação ")) %>% 
    hc_add_theme(hc_theme_darkunica()) #%>% 
  
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
  hchart(as.factor(tabela_A$Variacao_), type = "column",series.showInLegend = F)%>%
    hc_chart(events = list(load = JS("function() {

    this.series.forEach(function(series) {
      if (series.name === 'Series 1') {
        series.update({
          showInLegend: false
        });
      }
    });

  }")))  %>% 
    hc_title(text = "Contagem variação ações Ativo") %>% 
    hc_xAxis(title= list(text= "Variação")) %>% 
    hc_colors(c("#2980b9"))  %>%
    hc_yAxis(title= list(text="Contagem")) %>% 
    hc_add_theme(hc_theme_darkunica())
  }) 
  
  
  
    
    output$chart_2 <- renderHighchart({
    hchart(as.factor(tabela_I$Variacao_), type = "column",series.showInLegend = F)%>%
      hc_chart(events = list(load = JS("function() {

    this.series.forEach(function(series) {
      if (series.name === 'Series 1') {
        series.update({
          showInLegend: false
        });
      }
    });

  }")))  %>% 
      hc_title(text = "Contagem variação ações Ibovespa") %>% 
      hc_xAxis(title= list(text= "Variação")) %>% 
      hc_colors(c("#d35400"))  %>%
      hc_yAxis(title= list(text="Contagem")) %>% 
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
          paste("Ativo_H ",str_replace_all(as.character(Sys.time()),":","_"), ".csv", sep="")
        },
        content = function(file) {
          write.csv(as.data.frame(tabela_A), file, row.names = FALSE)
        }
      )
      
      
      
      output$downloadData4 <- downloadHandler(
        filename = function() {
          paste("Ibovespa_H ",str_replace_all(as.character(Sys.time()),":","_"), ".csv", sep="")
        },
        content = function(file) {
          write.csv(as.data.frame(tabela_I), file, row.names = FALSE)
        }
      ) 
      
      output$downloadData5 <- downloadHandler(
        filename = function() {
          paste("Ibovespa_H ",str_replace_all(as.character(Sys.time()),":","_"), ".csv", sep="")
        },
        content = function(file) {
          write.csv(as.data.frame(ativo_ibovespa), file, row.names = FALSE)
        }
      ) 
      
      
      tabPanel("Dados",
               DT::dataTableOutput("table")
               
      )
      output$table_I <- DT::renderDataTable({
        DT::datatable(tabela_I,style = 'bootstrap',options = list(
          initComplete = JS(
            "function(settings, json) {
        $(this.api().table().header()).css({
        'background-color': '#000',
        'color': '#fff'
        }); 
        }")
        ))
      })
      
      output$table_A <- DT::renderDataTable({
        DT::datatable(tabela_A,style = 'bootstrap',options = list(
          initComplete = JS(
            "function(settings, json) {
        $(this.api().table().header()).css({
        'background-color': '#000',
        'color': '#fff'
        }); 
        }")
        ))
      })
      
      output$table_AI <- DT::renderDataTable({
        DT::datatable(ativo_ibovespa,style = 'bootstrap',options = list(
          initComplete = JS(
            "function(settings, json) {
        $(this.api().table().header()).css({
        'background-color': '#000',
        'color': '#fff'
        }); 
        }")
        ))
      })
  
  

})
