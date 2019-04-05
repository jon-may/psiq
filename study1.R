## Analysis of Study 1 from Andrade et al (2013)
## 
## Jon May, 5 April 2019 CC BY-NC 4.0
## 

library(tidyverse)
library(psych)
library(GPArotation)
library(plyr)

Study1 <- read_csv("PsiQStudy1N404.csv")

# extract the SUIS items by finding just those columns that have SUIS in their name
SUIS <- Study1[, grepl('SUIS',colnames(Study1))]
# find the mean of these items, ignoring any missing values
SUIS$mean <- rowMeans(SUIS, na.rm=TRUE)
# report the descriptives
describe(SUIS$mean)
# 'The mean score obtained on the SUIS was 3.43 (SD = 0.62), 
# with scores ranging over almost the entire scale from 1.33 to 4.83'
# 

# extract the PSIQ items by finding just those columns that have PSI in their name
PSIQ <- Study1[, grepl('PSI',colnames(Study1))]
# find the mean of these items, ignoring any missing values
# subtracting 1 because scale is 0-10 but data values are 1-11
PSIQ$mean <- rowMeans(PSIQ, na.rm=TRUE)-1
# report the descriptives
describe(PSIQ$mean)
# 'The overall mean of the Psi-Q for the remaining 404 participants was 7.05 (SD = 1.61), 
# with scores ranging from 1.11 to 9.94. '


# How many people had low scores, i.e., <5, on PSIQ?
PSIQ$lo <- 0
PSIQ$lo[PSIQ$mean<5]<-1
table(PSIQ$lo)
# 'Only 35 participants scored below 6, 
# suggesting that most people were able to construct the images described the items.'
#
# Error in the paper! We forgot to subtract 1 from data values to map onto response scales
# so it should read 'below 5'
# 

# internal consistency of PSIQ
psych::alpha(PSIQ)
# 'The scale produced a Cronbach’s alpha of .96, 
# with no improvement indicated by deleting any items.'
# 


# Exploratory Factor Analysis
# using psych package functions, and requiring GPArotation for oblimin

# first drop mean and lo
PSIQ <- PSIQ %>% select(-mean, -lo)

# how many factors?
fa.parallel(PSIQ)
# should suggest seven factors
fa(PSIQ, nfactors=7)







## lets plot a fancy raincloud plot
## using the source and code given to the workd by
## https://micahallen.org/2018/03/15/introducing-raincloud-plots/

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


g <- ggplot(data = PSIQ, aes(y = mean, x= 0)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = mean), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 1) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() +
  raincloud_theme

g

