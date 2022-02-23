###########################
### pacotes necessarios ###
###########################

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

                
                tabPanel("Criadores",
                         fluidRow(
                           tags$style(HTML("
                    img {
                      border-radius: 50%;
                    }")),
                           column(width = 6,uiOutput("img"),h2("Alessandro Pereira"),p("Graduando em Estatística da Universidade Federal do Rio Grande do Norte. Possui experiência em ciência de dados, principalmente na utilização da linguagem R. Usuário avançado do framework shiny, utilizado para a construção de dashboards.") ,p("GitHub:",a("https://github.com/AlessandroPTSN",   href="https://github.com/AlessandroPTSN"))),
                           column(width = 6, uiOutput("img2"),h2("Felipe Sergio"),p(" Analista de TI focado em desenvolvimento de software, linguagens de programação e infraestrutura como código. Profissional com experiência comprovada nas áreas de administração de sistemas Linux,Gerenciamento de datacenter, técnico, consultoria e implantação de sistemas corporativos."),p("GitHub:",a("https://github.com/felipesergios",   href="https://github.com/felipesergios")))
                         
                )
                         
               
                )
           
           
           
)
