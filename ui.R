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
  html_text2()%>%str_replace("%\n", "")%>%str_replace("\n", " ")%>%str_replace("M", "")%>%str_replace("\t", " ")

zzz=gsub("\n", " ",zzz[3:21])

zzz=read.table(text=zzz,col.names= c("Data" ,"Ultimo", "Abertura", "Maxima" ,"Minima" ,"Vol" ,"Var"))
zzz[,6]=zzz[,6]%>%str_replace(",", ".")
zzz[,7]=zzz[,7]%>%str_replace(",", ".")
zzz[,7]=zzz[,7]%>%str_replace("\\+", "")
zzz[,7]=zzz[,7]%>%str_replace("-", "")
zzz[,7]=zzz[,7]%>%str_replace("%", "")
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
  html_text2()%>%str_replace("%\n", "")%>%str_replace("\n", " ")%>%str_replace("\t", " ")#%>%str_replace("M", "")

zzz2=gsub("\n", " ",zzz2[3:21])

zzz2=read.table(text=zzz2,col.names= c("Data" ,"Ultimo", "Abertura", "Maxima" ,"Minima" ,"Vol" ,"Var"))

zzz2[,7]=zzz2[,7]%>%str_replace("\\+", "")
zzz2[,7]=zzz2[,7]%>%str_replace("-", "")
zzz2[,7]=zzz2[,7]%>%str_replace("%", "")
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
#yyy2 = yyy2[-20,]




Ibo = read.csv("/app/Ibo.csv")
#yyy = novo
#Ibo = antigo
#Inverte
m = length(Ibo[,1])
Ibo = Ibo %>% map_df(rev)
Ibo = as.data.frame(Ibo)
Ibo$Data=as.Date(Ibo$Data)
j=1
while((Ibo[m,1] != yyy[j,1])&(Ibo[m,4] != yyy[j,4])&(j<20)){
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
while((Ati[n,1] != yyy2[j,1])&(Ati[n,4] != yyy2[j,4])&(j<20)){
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
    ((as.numeric(Variacao)<=4.5)&(as.numeric(Variacao)>=3.51))   ~ 4,
    ((as.numeric(Variacao)<=3.5)&(as.numeric(Variacao)>=2.51))   ~ 3,
    ((as.numeric(Variacao)<=2.5)&(as.numeric(Variacao)>=1.51))   ~ 2,
    ((as.numeric(Variacao)<=1.5)&(as.numeric(Variacao)>=0.51))   ~ 1,
    ((as.numeric(Variacao)<=0.5)&(as.numeric(Variacao)>=-0.5))   ~ 0,
    ((as.numeric(Variacao)>= -1.5)&(as.numeric(Variacao)<=-0.5))   ~ -1,
    ((as.numeric(Variacao)>= -2.5)&(as.numeric(Variacao)<=-1.5))   ~ -2,
    ((as.numeric(Variacao)>= -3.5)&(as.numeric(Variacao)<=-2.5))   ~ -3,
    ((as.numeric(Variacao)>= -4.5)&(as.numeric(Variacao)<=-3.5))   ~ -4,
    ((as.numeric(Variacao)>= -5.5)&(as.numeric(Variacao)<=-4.5))   ~ -5,
    ((as.numeric(Variacao)>= -6.5)&(as.numeric(Variacao)<=-5.5))   ~ -6,
    ((as.numeric(Variacao)>= -7.5)&(as.numeric(Variacao)<=-6.5))   ~ -7,
    ((as.numeric(Variacao)>= -8.5)&(as.numeric(Variacao)<=-7.5))   ~ -8,
    ((as.numeric(Variacao)>= -9.5)&(as.numeric(Variacao)<=-8.5))   ~ -9,
    (as.numeric(Variacao)<= -10.5)  ~ -10
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
    as.numeric(Variacao_)==-1   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==-1])),
    as.numeric(Variacao_)==-6   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==-6])),
    as.numeric(Variacao_)==-7   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==-7])),
    as.numeric(Variacao_)==-8   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==-8])),
    as.numeric(Variacao_)==-9   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==-9])),
    as.numeric(Variacao_)==-10   ~ length(as.numeric(tabela_I$Variacao_[tabela_I$Variacao_==-10]))
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
    ((as.numeric(Variacao)>= -1.5)&(as.numeric(Variacao)<=-0.5))   ~ -1,
    ((as.numeric(Variacao)>= -2.5)&(as.numeric(Variacao)<=-1.5))   ~ -2,
    ((as.numeric(Variacao)>= -3.5)&(as.numeric(Variacao)<=-2.5))   ~ -3,
    ((as.numeric(Variacao)>= -4.5)&(as.numeric(Variacao)<=-3.5))   ~ -4,
    ((as.numeric(Variacao)>= -5.5)&(as.numeric(Variacao)<=-4.5))   ~ -5,
    ((as.numeric(Variacao)>= -6.5)&(as.numeric(Variacao)<=-5.5))   ~ -6,
    ((as.numeric(Variacao)>= -7.5)&(as.numeric(Variacao)<=-6.5))   ~ -7,
    ((as.numeric(Variacao)>= -8.5)&(as.numeric(Variacao)<=-7.5))   ~ -8,
    ((as.numeric(Variacao)>= -9.5)&(as.numeric(Variacao)<=-8.5))   ~ -9,
    (as.numeric(Variacao)<= -10.5)  ~ -10
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
    as.numeric(Variacao_)==-1   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==-1])),
    as.numeric(Variacao_)==-6   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==-6])),
    as.numeric(Variacao_)==-7   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==-7])),
    as.numeric(Variacao_)==-8   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==-8])),
    as.numeric(Variacao_)==-9   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==-9])),
    as.numeric(Variacao_)==-10   ~ length(as.numeric(tabela_A$Variacao_[tabela_A$Variacao_==-10]))
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
    ((as.numeric(Variacao)>= -1.5)&(as.numeric(Variacao)<=-0.5))   ~ -1,
    ((as.numeric(Variacao)>= -2.5)&(as.numeric(Variacao)<=-1.5))   ~ -2,
    ((as.numeric(Variacao)>= -3.5)&(as.numeric(Variacao)<=-2.5))   ~ -3,
    ((as.numeric(Variacao)>= -4.5)&(as.numeric(Variacao)<=-3.5))   ~ -4,
    ((as.numeric(Variacao)>= -5.5)&(as.numeric(Variacao)<=-4.5))   ~ -5,
    ((as.numeric(Variacao)>= -6.5)&(as.numeric(Variacao)<=-5.5))   ~ -6,
    ((as.numeric(Variacao)>= -7.5)&(as.numeric(Variacao)<=-6.5))   ~ -7,
    ((as.numeric(Variacao)>= -8.5)&(as.numeric(Variacao)<=-7.5))   ~ -8,
    ((as.numeric(Variacao)>= -9.5)&(as.numeric(Variacao)<=-8.5))   ~ -9,
    (as.numeric(Variacao)<= -10.5)  ~ -10
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
    as.numeric(Variacao_)==-1   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==-1])),
    as.numeric(Variacao_)==-6   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==-6])),
    as.numeric(Variacao_)==-7   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==-7])),
    as.numeric(Variacao_)==-8   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==-8])),
    as.numeric(Variacao_)==-9   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==-9])),
    as.numeric(Variacao_)==-10   ~ length(as.numeric(ativo_ibovespa$Variacao_[ativo_ibovespa$Variacao_==-10]))
  ))




Fact_A=as.factor(tabela_A$Variacao_)
Fact_A=ordered(Fact_A, levels =as.character(c(-10:-1)))



Fact_I=as.factor(tabela_I$Variacao_)
Fact_I=ordered(Fact_I, levels =as.character(c(-10:-1)))


Fact_A_I=as.factor(ativo_ibovespa$Variacao_)
Fact_A_I=ordered(Fact_A_I, levels =as.character(c(-10:-1)))



navbarPage("Dashboard",theme = shinytheme("slate"),
           
           tabPanel("Diferença",theme = shinytheme("slate"),
                    titlePanel("Diferença de ações entre Ativo - Ibovespa"),   
                    mainPanel(p("Atualizado em:",yyy[1,1]),
                              
                    ),
                    column(
                      12,fluidRow(column(12, highchartOutput('chart22')))
                    ),
                    downloadButton("downloadData5", "Download Ativo-Ibovespa"),
                    h2("Ativo - Ibovespa"),
                    DT::dataTableOutput("table_AI"),
                    
           ),
           
           tabPanel("Ativo",  
                    column(12,h2("Ativo"),p("Fonte:",a("https://br.investing.com/equities/imc-holdings-on-historical-data",   href="https://br.investing.com/equities/imc-holdings-on-historical-data")),
                           fluidRow(column(12, highchartOutput('chart_1'))),
                           h2("Download dados Ativo"), 
                           downloadButton("downloadData2", "Download Ativo"),
                           h2("Download dados Ativo Histograma"), 
                           downloadButton("downloadData3", "Download Ativo_H"),
                           h2("Ativo - Tabela"),
                           DT::dataTableOutput("table_A"))
           ),
           
           tabPanel("Ibovespa",          
                    column(12,h2("Ibovespa"),p("Fonte:",a("https://br.investing.com/indices/bovespa-historical-data",   href="https://br.investing.com/indices/bovespa-historical-data")),
                           fluidRow(column(12, highchartOutput('chart_2'))),h2("Download dados Ibovespa"), 
                           downloadButton("downloadData1", "Download Ibovespa"),
                           h2("Download dados Ibovespa Histograma"), 
                           downloadButton("downloadData4", "Download Ibovespa_H"),
                           h2("Ibovespa - Tabela"),
                           DT::dataTableOutput("table_I"),),
           ),
           
           
           tabPanel("Criador",
                    fluidRow(
                      tags$style(HTML("
                    img {
                      border-radius: 50%;
                    }")),
                      column(width = 12,uiOutput("img"),h2("Alessandro Pereira"),p("Graduando em Estatística da Universidade Federal do Rio Grande do Norte. Possui experiência em ciência de dados, principalmente na utilização da linguagem R. Usuário avançado do framework shiny, utilizado para a construção de dashboards.") ,p("GitHub:",a("https://github.com/AlessandroPTSN",   href="https://github.com/AlessandroPTSN"))),
                      
                      
                    )
                    
                    
           )
           
           
           
)
