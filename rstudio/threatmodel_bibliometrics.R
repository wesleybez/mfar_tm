library("dplyr")
library("Matrix")
library("stringr")
library("igraph")
library("FactoMineR")
library("factoextra")
library("ggplot2")
library("bibliometrix")

setwd("/home/wesley/202101/els_proposal_ADT_ThreatDragon/systematic_review")

#bibliometrix de threat model
dados_scopus <- bibliometrix::convert2df("scopus_threatmodel_and_authentication.bib",
                                         dbsource = "scopus",
                                         format = "bibtex")

dplyr::distinct(dados_scopus, DT) %>% 
  knitr::kable(row.names = FALSE)

dados <- dados_scopus %>%
  filter(DT == "ARTICLE")

dplyr::glimpse(dados)

#faz uma analise dos dados
resumo <- bibliometrix::biblioAnalysis(dados_scopus)
#faz um sumario dos dados
sumario=summary(object = resumo, k = 10, pause = FALSE)
#gera o plot
graficos_bibliometrix <- plot(resumo)


####################################################################
############# Periódicos
####################################################################
dados_scopus %>%
  dplyr::mutate(SO = gsub('(.{1,30})(\\s|$)', 
                          '\\1\n', SO )
  ) %>%
  dplyr::count(SO) %>%
  dplyr::arrange(desc(n)) %>% 
  dplyr::slice(1:10) %>% 
  ggplot() +
  geom_col(aes(x = reorder(SO, n),
               y = n),
           fill = "#4b689c") +
  coord_flip() +
  theme_bw() +
  labs(x = "Sources", 
       y = "Articles")

#sumariza todos os dados
S=summary(object = resumo, k = 10, pause = FALSE)
#plota graficos
plot(x = resumo, k = 10, pause = FALSE)

##########################################################
################ sobre os autores
##########################################################
#analise das referencias citadas (citados mais frequentemente)
CR <- citations(dados_scopus,field="article", sep = ";")
CR$Cited[1:10]

#autores mais citados
CR <- citations(dados_scopus, field = "author", sep = ";")
CR$Cited[1:10]

CR <- localCitations(dados_scopus, resumo, sep = ";")
CR[1:10]

#dominancia de autores (kumar&kumar, 2008)
DF <- dominance(resumo, k = 10)
DF

#dados bibliometricos dos autores
i<-Hindex(dados_scopus, field = "author", elements = NULL, sep = ";", years = 10)
# Bornmann's impact indices:
i$H
i$CitationList[1:10]
#somente os top15
authors=gsub(","," ",names(resumo$Authors)[1:15])
i <- Hindex(dados_scopus, field = "author", sep = ";")
i$H
#lotka's law
L <- lotka(resumo)
# Author Productivity. Empirical Distribution
L$AuthorProd
# Beta coefficient estimate
L$Beta
# Constant
L$C
# Goodness of fit
L$R2
# P‐value of K‐S two sample test
L$p.value

# Observed distribution
Observed=L$AuthorProd[,3]
# Theoretical distribution with Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))
plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of
Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty =
         c(1,1,1),cex=0.6,bty="n")

#################################################
################# keywords
#################################################
# Rede de co-ocorrencias de palavras chave
# Create keyword co‐occurrencies network (deu erro no nro de dimensoes)
NetMatrix <- biblioNetwork(
  dados_scopus, 
  analysis = "co‐occurrences", 
  network = "keywords", 
  sep = ";"
)
# Plot the network
net=networkPlot(
  NetMatrix, 
  n = 20, 
  Title = "Keyword Co‐occurrences", 
  type = "kamada", 
  size=T
)
##########

# Conceptual Structure using keywords
CS <- conceptualStructure(
  dados_scopus,
  field="ID", 
  minDegree=4, 
  k.max=5, 
  stemming=FALSE
)

# Create a historical co-citation network
histResults <- histNetwork(dados_scopus, n = 10, sep = ". ")
