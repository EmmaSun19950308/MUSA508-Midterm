# Load Libraries
library(sf)
library(tidyverse)
install.packages('mapview')
library(mapview)
library(spdep)
library(caret)
library(ckanr) # for opening data APIs built on CKAN technology
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(jtools)     # for regression model plots

# Identify functions
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 15,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(color = "darkred", size=15, face="bold"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}


# Load hexadecimal color palette

palette <- c('#feedde', '#fdbe85', '#fd8d3c', '#e6550d', '#a63603')

# for calculating average nearest neighbor distance.

nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    dplyr::summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull() # pull() is similar to $. It's mostly useful because it looks a little nicer in pipes, it also works with remote data frames, and it can optionally name the output.
  
  return(output)  
}


# Data Wrangling
## House Price & internal characteristics
miami.sf <- st_read('/Users/penguin/Box Sync/GitHub/MUSA508-Midterm/studentsData.geojson')
miami.sf$Age <- 2020- miami.sf$YearBuilt
  
## Miami base data
miami.base <-st_read("https://opendata.arcgis.com/datasets/5ece0745e24b4617a49f2e098df8117f_0.geojson") %>%
  filter(NAME %in% c("MIAMI", "MIAMI BEACH"))


## Sexual Predator
miami.sexual <- st_read('https://opendata.arcgis.com/datasets/f8759d722aeb4198bfe7c4ad780604d2_0.geojson')

miami.sexual.sf <- miami.sexual %>%
  select(geometry) %>%
  na.omit() %>%
  distinct()


## Shopping Mall
miami.mall <- st_read('https://opendata.arcgis.com/datasets/cb24d578246647a9a4c57bbd80c1caa8_0.geojson')
miami.mall.sf <- miami.mall %>%
  select(geometry) %>%
  na.omit() %>%
  distinct()

## Landmark
miami.landmark <- st_read('https://opendata.arcgis.com/datasets/70a14825e66f4f0eb28d2a9cceba1761_0.geojson')
miami.landmark.sf <- miami.landmark %>%
  select(geometry) %>%
  na.omit() %>%
  distinct()


# =======================================
# create feature: price per square feet
dat <- dat %>%
  mutate(priceFt = SalePrice/LivingSqFt)


ggplot() + 
  geom_sf(data = st_union(miami.base),fill = 'grey') +
  geom_sf(data = st_centroid(dat),aes(color = q5(priceFt)),size = .5) +
  scale_color_manual(values = palette,
                     labels = qBr(dat,'priceFt'),
                     name = "Price/ft^2") +
  labs(title = 'Sale Price Per Square Foot\n',
       subtitle = '',
       caption = 'Figure 1.1') +
  mapTheme() + 
  plotTheme()
  
 
# ===========================================
# Feature Engineering


## 1. Landmark 
### 1/8 miles
miami.sf$landmark.buffer <-
  st_buffer(st_centroid(miami.sf), 660) %>% 
  aggregate(mutate(miami.landmark.sf, counter = 1),., sum) %>%
  pull(counter)


## 1. Shopping Mall
### 1/8 miles
miami.sf$mall.buffer <-
  st_buffer(st_centroid(miami.sf), 660) %>% 
  aggregate(mutate(miami.mall.sf, counter = 1),., sum) %>%
  pull(counter)

## 2. Sexual Assaults 
### 0.5 miles
miami.sf$sexual.buffer <-
  st_buffer(st_centroid(miami.sf), 2640) %>% 
  aggregate(mutate(miami.sexual.sf, counter = 1),., sum) %>%
  pull(counter)

plot.sexual <- ggplot() + 
  geom_sf(data = miami.base, fill = "grey40") +
  stat_density2d(data = data.frame(st_coordinates(miami.sexual.sf)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 40, geom = 'polygon') +
  scale_fill_gradient(low = "#25CB10", high = "#FA7800", name = "Density") +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  labs(title = "Density of Sexual Assaults, Miami") +
  mapTheme()


miami.sf <-
  miami.sf %>% 
  mutate(
    sexual_nn1 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.sexual.sf)), 1),
    sexual_nn2 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.sexual.sf)), 2), 
    sexual_nn3 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.sexual.sf)), 3), 
    sexual_nn4 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.sexual.sf)), 4), 
    sexual_nn5 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.sexual.sf)), 5)) 

    
## Plot NN count over space - Should increase or decrease?
miami.sf.plot <- miami.sf %>% 
  st_drop_geometry() %>% 
  dplyr::select(Folio, starts_with("sexual_")) %>% 
  tidyr::pivot_longer(cols = -Folio, names_to = "sexual_nn")

ggplot(miami.sf.plot, aes(x = sexual_nn, y = value, group = Folio)) +
  geom_line(alpha = 0.05, color = "firebrick") +
  theme_bw()
  
# House Internal characteristics
price.plot <- 
  st_drop_geometry(miami.sf) %>% 
  dplyr::select(SalePrice, AdjustedSqFt, LotSize, Age, Bed, Bath, LivingSqFt, ActualSqFt) %>%
  filter(SalePrice <= 1000000, Age < 500) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_point(size = .5) + 
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  plotTheme() + 
  labs(title = "Price as a function of continuous variables") 

price.plot

## Crime Correlation 
price.sexual.plot <-
  st_drop_geometry(miami.sf) %>% 
  dplyr::select(SalePrice, starts_with("sexual_")) %>%
  filter(SalePrice <= 1000000) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_point(size = .5) + 
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, nrow = 1, scales = "free") +
  labs(title = "Price as a function of continuous variables") +
  plotTheme()

price.sexual.plot

### Corr matrix
miami.numericVars <- 
  select_if(st_drop_geometry(miami.sf), is.numeric) %>% 
  na.omit()

ggcorrplot(
  round(cor(miami.numericVars), 1), 
  p.mat = cor_pmat(miami.numericVars),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 

### Corr coefficients
cor.test(miami.sf$AdjustedSqFt, miami.sf$SalePrice, method = "pearson")
cor.test(miami.sf$Age, miami.sf$SalePrice, method = "pearson")
cor.test(miami.sf$Bed, miami.sf$SalePrice, method = "pearson")
cor.test(miami.sf$LotSize, miami.sf$SalePrice, method = "pearson")
cor.test(miami.sf$Bath, miami.sf$SalePrice, method = "pearson")
cor.test(miami.sf$LivingSqFt, miami.sf$SalePrice, method = "pearson")
cor.test(miami.sf$ActualSqFt, miami.sf$SalePrice, method = "pearson")


#===========
# Cross Validation

# use caret package cross-validation method
fitControl <- caret:: trainControl(method = "cv", 
                                   number = 10,
                                   # savePredictions differs from book
                                   savePredictions = TRUE)

set.seed(717)
# crimes.buffer feature added
# for k-folds CV
reg.cv <- 
  caret::train(SalePrice ~ ., data = st_drop_geometry(miami.sf) %>% 
                 dplyr::select(SalePrice, AdjustedSqFt, LotSize, Bed,
                               Bath, Age,LivingSqFt, ActualSqFt, sexual.buffer), 
               method = "lm", 
               trControl = fitControl, 
               na.action = na.pass)

reg.cv

reg.cv$resample

reg.cv$resample %>% 
  pivot_longer(-Resample) %>% 
  mutate(name = as.factor(name)) %>% 
  ggplot(., aes(x = name, y = value, color = name)) +
  geom_jitter(width = 0.1) +
  facet_wrap(~name, ncol = 3, scales = "free") +
  theme_bw() +
  theme(
    legend.position = "none"
  )


# extract predictions from CV object
cv_preds <- reg.cv$pred
# compare number of observations between data sets
nrow(miami.sf)
nrow(cv_preds)

## Create dataset with "out of fold" predictions and original data
map_preds <- miami.sf %>% 
  rowid_to_column(var = "rowIndex") %>% 
  left_join(cv_preds, by = "rowIndex") %>% 
  mutate(SalePrice.AbsError = abs(cv_preds$pred - miami.sf$SalePrice)) 

# weird CRS fix to miami.sf
st_crs(map_preds) <- st_crs(miami.base)

# plot errors on a map
ggplot() +
  geom_sf(data = miami.base, fill = "grey40") +
  geom_sf(data = map_preds, aes(colour = q5(SalePrice.AbsError)),
          show.legend = "point", size = 1) +
  scale_colour_manual(values = palette5,
                      labels=qBr(map_preds,"SalePrice.AbsError"),
                      name="Quintile\nBreaks") +
  labs(title="Absolute sale price errors on the OOF set",
       subtitle = "OOF = 'Out Of Fold'") +
  mapTheme()
