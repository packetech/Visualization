# 1. Assignment

#Data loading
Happy_world <- read.csv("https://gist.githubusercontent.com/sandravizz/8b7cf476e4d07331eee00f1fa0249e12/raw/65395ac49ea7154a43f47f455ec26e270c0fb4a3/World%2520Happiness%2520Report")

#Data checking
names(Happy_world)
head(Happy_world, n=10)
str(Happy_world)
summary(Happy_world)
View(Happy_world)

#Defining the general colors to avoid hard coding 
fill_color = '#111111'
main2_color = '#1ce3cd'
main1_color = '#f20675'
decoration_color = '#cccccc'

install.packages("ggplot2")                # Install ggplot2 package
library("ggplot2")                         # Load ggplot2


#set my theme
if(!'ggthemes'%in%installed.packages()){
  install.packages('ggthemes')}
library(ggthemes)




#Ndi_theme<-theme_dark()
#theme_set(Ndi_theme)

Ndi_theme<-theme_dark()+ theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  plot.title = element_text(size = 14, hjust = 0, color = decoration_color),
  axis.ticks = element_blank(),
  axis.title = element_text(size = 10, hjust = 0.5, color = decoration_color),
  legend.title = element_blank(),
  legend.text=element_text(color="white"),
  panel.background =element_rect(fill = fill_color),
  strip.background =element_rect(fill = fill_color), 
  plot.background = element_rect(fill = fill_color),
  legend.background = element_rect(fill = fill_color)
)

theme_set(Ndi_theme)






#Simple distirbution
ggplot(Happy_world, aes(Generosity)) + 
  geom_freqpoly(color="white") 

#Simple scatter 
ggplot(Happy_world, aes(Generosity, Family)) +
  geom_point(color="white") 

##############################################
###### Distribution investigations ###########
##############################################
##
## Plot 1 - Density plot of the first numeric variable.
### Sub-saharan africa and southern asia have a whole lot of people at zero
#### happiness.score
ggplot(Happy_world, aes(Happiness.Score, colour = Region)) +
  geom_freqpoly(binwidth = 10) +
  scale_color_manual(values=c("#478adb", "#82FA58", "#f20675", "#bcc048", "#1ce3cd","#d73027","#f46d43","#fdae61","#66bd63","#aaaa9a")) 

## Plot 2 - Density plot of the second numeric variable
### All regions have something in common; alot of people in them have no freedom
ggplot(Happy_world, aes(Freedom, colour = Region)) +
  geom_freqpoly(binwidth = 10) +
  scale_color_manual(values=c("#478adb", "#82FA58", "#f20675", "#bcc048", "#1ce3cd","#d73027","#f46d43","#fdae61","#66bd63","#aaaa9a")) 

## Plot 3 - boxplot plot of first numeric variable
### Though sub saharan africa is the worse hit by happiness.score, they have
#### a Free exceptional case (because of the high outlier, a hand full of
##### persons are highly Free)
ggplot(Happy_world, aes(Happiness.Score, colour = Region)) +
  geom_boxplot() +
  scale_color_manual(values=c("#478adb", "#82FA58", "#f20675", "#bcc048", "#1ce3cd","#d73027","#f46d43","#fdae61","#66bd63","#aaaa9a")) 


## Plot 4 - Histogram for different cut options 
### Western europe has the most people having Freedom
ggplot(Happy_world, aes(Freedom, fill = Region)) +
  geom_histogram(position = "dodge", binwidth = 0.09) + 
  scale_fill_manual(values=c("#478adb", "#82FA58", "#f20675", "#bcc048", "#1ce3cd","#d73027","#f46d43","#fdae61","#66bd63","#aaaa9a"))

## Plot 5 Bar-chat
### Even though sub-saharan africa is the worse hit by freedom they still have
#### more people experiencing freedom than other reasons
ggplot(aggregate(Freedom ~ Region, Happy_world, sum),    # theme_dark barplot
       aes(x = Region,
           y = Freedom,
           fill = Region)) +
  geom_bar(stat = "identity") +
  theme_dark() + theme_set(Ndi_theme)

## Plot 6 Histogram color filled
### Central and eastern europe have so many people having hapiness.score of about 5
ggplot(Happy_world, aes(Happiness.Score, fill = Region)) +
  geom_histogram() + 
  scale_fill_manual(values=c("#478adb", "#82FA58", "#f20675", "#bcc048", "#1ce3cd","#d73027","#f46d43","#fdae61","#66bd63","#aaaa9a"))

## Plot 7 Histogram small multiples for Freedom and Happiness.Score
ggplot(Happy_world, aes(Freedom)) + 
  geom_histogram(binwidth = 0.02, fill = main2_color, colour = "black") + 
  facet_wrap(. ~ Region)

ggplot(Happy_world, aes(Happiness.Score)) + 
  geom_histogram(binwidth = 0.1, fill = main2_color) + 
  facet_wrap(. ~ Region)

## Plot 8 - ridgeline plot
if(!"ggridges" %in% installed.packages()) {
  install.packages("ggridges")}

library(ggridges)

ggplot(Happy_world, aes(x = Freedom, y = Region)) +
  geom_density_ridges(fill=main2_color, color=fill_color) 

#Encoding color 
ggplot(Happy_world, aes(x = Freedom, y = Region, fill=Region)) +
  geom_density_ridges(color=NA) +
  scale_fill_manual(values=c("#478adb", "#82FA58", "#f20675", "#bcc048", "#1ce3cd","#d73027","#f46d43","#fdae61","#66bd63","#aaaa9a"))

#Introducing the scaling factor
ggplot(Happy_world, aes(x = Happiness.Score, y = Region, fill=Region)) +
  geom_density_ridges(color=NA, scale = 3) +
  scale_fill_manual(values=c("#478adb", "#82FA58", "#f20675", "#bcc048", "#1ce3cd","#d73027","#f46d43","#fdae61","#66bd63","#aaaa9a"))






##############################################
###### Relationship plots ####################
##############################################
##
## Plot 1 - Simple scatter 
ggplot(Happy_world, aes(x=Happiness.Score, y=Freedom)) +
  geom_point(size=0.5, color=main2_color) 

#Basic scatter plot changing the Y limits
ggplot(Happy_world, aes(x=Happiness.Score, y=Freedom)) + 
  geom_point(size=0.5, color=main2_color) + 
  ylim(0.5, 0.7)

#Axis labeling depending on the quantiles 
ggplot(Happy_world, aes(x=Happiness.Score, y=Freedom)) + 
  geom_point(size=0.7, alpha=0.7, color=main2_color)  + 
  scale_x_continuous(breaks = round(as.vector(quantile(Happy_world$Happiness.Score)), digits = 1))+
  scale_y_continuous(breaks = round(as.vector(quantile(Happy_world$Freedom)), digits = 1)) 

#Axis labeling depending on the quantiles for logaritmic scaling
ggplot(Happy_world, aes(x=Happiness.Score, y=Freedom)) +
  geom_point(size=0.02, alpha=0.5, color=main2_color)  + 
  xlab("")+ 
  ylab("")+ 
  scale_x_log10(breaks = round(as.vector(quantile(Happy_world$Happiness.Score)), digits = 1))+
  scale_y_log10(breaks = round(as.vector(quantile(Happy_world$Freedom)), digits = 1))


## Plot 2 - scatter with color
### As can be seen australia and new zealand have the highest combination of
#### freedom and happiness.score
ggplot(Happy_world, aes(x=Happiness.Score, y=Freedom, colour=Region)) +
  geom_point(size=0.9)


## Plot 3 - Scatter with a trend line 
### They are about directly proportional to each other
ggplot(Happy_world, aes(x=Happiness.Score, y=Freedom)) +
  geom_point(color=main2_color, size=0.8, alpha=0.5)+
  stat_smooth(color=decoration_color)


## Plot 4 - Line
### Though looking messy, but the trend is upwards
ggplot(Happy_world,                           
       aes(x = Freedom,
           y = Happiness.Score,
           col = Region)) +
  geom_line() +
  theme_dark()+ theme_set(Ndi_theme)


## Plot 5 - Density estimation with contours
### The highest point of freedom(with most people) is between O.4 and 0.5
#### and for happiness score is 5 and 6.5
ggplot(Happy_world, aes(x=Freedom, y=Happiness.Score)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")  +
  scale_fill_continuous(type = "viridis") 


## Plot 6 - Small multiples - one variable
### With this plot others are trending upwards except for australia/new zealand,
#### eastern asia, southeastern asia, North America and southern asia that are oscillationg or 
##### show no relationship (as in the case of austraia/new zealand)
ggplot(Happy_world, aes(x=Freedom, y=Happiness.Score)) +
  geom_point(color=main2_color, size=0.8, alpha=0.2)+
  facet_wrap( ~ Region, ncol=2, scales = "free") +
  stat_smooth(color=decoration_color)


## Plot 7 (Additional scatter plots)
#Adding price or gradient as another visual encoding using a colour code 
ggplot(Happy_world, aes(x=Happiness.Score, y=Freedom, colour = Happiness.Score)) +
  geom_point(size=0.7, alpha=1)+
  scale_colour_gradient(low = main1_color, high = main2_color) 

#Another way to handle big datasets is to create a sample  
Happy_sample <- Happy_world[sample(nrow(Happy_world), 100),]
ggplot(Happy_sample, aes(x=Happiness.Score, y=Freedom)) + 
  geom_point(color=main2_color)

#Change the position scale to logarithmic scaling
ggplot(Happy_world, aes(x=Happiness.Score, y=Freedom)) + 
  geom_point(size=2, alpha=0.5, color=main2_color) +
  scale_y_log10() 

#Density
if(!'ggExtra'%in%installed.packages()){
  install.packages('ggExtra')}
library(ggExtra)

pp <- ggplot(Happy_world, aes(x=Happiness.Score, y=Freedom)) +
  geom_point(color=main2_color) + 
  theme(axis.title=element_blank(), axis.text=element_blank())

ggMarginal(pp, type = "density", fill = main2_color, alpha=1, color='transparent')
## we see over-all a positive relationship(although with few outliers)
## The relationship was progressively positive as they then to converge at the higher end

