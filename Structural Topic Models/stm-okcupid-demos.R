library(stm)
library(quanteda)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(tm)
library(grid)
library(wordcloud)
library(wordcloud2)
library(tidyverse)

setwd('C:\\Users\\lliu9\\Desktop\\UML_Project')
#okcupid data
data <- read.csv('compressed_okcupid.csv')
#data <- sample_n(essay, 100)
data <- data[c('essay0', 'fit', 'edu', 'height_group', 'race_ethnicity')]


processed <- textProcessor(data$essay0, metadata = data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

#plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(processed$documents, processed$vocab,
                     processed$meta, lower.thresh = 0)

#Fit the STM model
FitExp <- stm(documents = out$documents, vocab = out$vocab,
                       K = 25, prevalence =~ fit + edu + height_group + race_ethnicity,
                       max.em.its = 50, data = out$meta,
                       init.type = "Spectral", verbose=FALSE)

#Find optimal topic numbers
coh <- data.frame('numbers' = 1:25, "Score" = semanticCoherence(FitExp, out$documents))
ggplot(data=coh, aes(x = numbers, y = Score))+
       geom_line(color = "#00AFBB", size = 1)

optimalK <- which.max(semanticCoherence(FitExp, out$documents))
 
#Fit the STM model with optimal K
Fit <- stm(documents = out$documents, vocab = out$vocab,
              K = optimalK, prevalence =~ fit + edu + height_group + race_ethnicity,
              max.em.its = 50, data = out$meta,
              init.type = "Spectral", verbose=FALSE)



#Select <- selectModel(out$documents, out$vocab, K = 5,
#                              prevalence =~ fit + edu + height_group + race_ethnicity, 
#                              max.em.its = 50, verbose=FALSE,
#                              data = out$meta, runs = 10, seed = 8458159)

#plotModels(Select, pch=c(1,2,3,4), legend.position="bottomright")

#A topic model with 20 topics, 92 documents and a 27 word dictionary.
#selectedmodel <- Select$runout[[3]]


#storage <- searchK(out$documents, out$vocab, K = seq(2:10), verbose=FALSE,
#                   prevalence =~ fit + edu + height_group + race_ethnicity, data = out$meta)

labelTopics(Fit, c(3, 7, 20))

#Outputs most representative documents for a particular topic
thoughts3 <- findThoughts(Fit, text = out$meta$essay0, n = 2, topics = 3)$docs[[1]]

thoughts20 <- findThoughts(Fit,text = out$meta$essay0, n = 2, topics = 20)$docs[[1]]

par(mfrow = c(1, 2),mar = c(1, 1, 5, 1))
plotQuote(thoughts3, main = "Topic 3")
plotQuote(thoughts20, main = "Topic 20")


#Estimating metadata/topic relationships
prep <- estimateEffect(1:5 ~ fit + edu + height_group + race_ethnicity, Fit,
                         meta = out$meta, uncertainty = "Global")
summary(prep, topics=1)

#Graphical display of estimated topic proportions
plot(Fit, type = "summary", xlim = c(0, .3))



par(mfrow=c(1,1))
#Summary visualization
plot.STM(Fit,type = "summary")

#Metadata/topic relationship visualization
##the marginal topic proportion for each of the levels
plot.estimateEffect(prep, covariate = 'edu', method='pointestimate')
plot.estimateEffect(prep, covariate = 'race_ethnicity', method='pointestimate')

#Topical content
##A topical content variable allows for the vocabulary used to talk about a particular topic to vary.
Content <- stm(out$documents, out$vocab, K = 5,
                          prevalence =~ fit + edu + height_group , 
                          content =~ race_ethnicity,
                          max.em.its = 50, data = out$meta, init.type = "Spectral", verbose=FALSE)



# which words within a topic are more associated with one covariate value versus another
##Figure: Graphical display of topical perspectives.
plot(Content, type = "perspectives", topics = 2)
##Figure: Graphical display of topical contrast between topics 1 and 2
plot(poliblogPrevFit, type = "perspectives", topics = c(1,2))



# Word cloud display of vice President topic.
cloud(Fit, topic = 3, scale = c(5,0.3) )

# Positive correlations between topics indicate that both topics are likely to be discussed within a document.
mod.out.corr <- topicCorr(Fit)
# Graphical display of topic correlations.
plot(mod.out.corr)



