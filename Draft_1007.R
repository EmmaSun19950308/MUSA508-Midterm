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


#=====================
# Data Wrangling 
## House Price & internal characteristics
miami.sf <- st_read('/Users/penguin/Box Sync/GitHub/MUSA508-Midterm/studentsData.geojson')
miami.sf$Age <- 2020- miami.sf$YearBuilt

## Miami base data
miami.base <-st_read("https://opendata.arcgis.com/datasets/5ece0745e24b4617a49f2e098df8117f_0.geojson") %>%
  filter(NAME %in% c("MIAMI", "MIAMI BEACH"))


## Miami Sexual Predator
miami.sexual <- st_read('https://opendata.arcgis.com/datasets/f8759d722aeb4198bfe7c4ad780604d2_0.geojson')

miami.sexual.sf <- miami.sexual %>%
  select(geometry) %>%
  na.omit() %>%
  distinct()


## Miami Shopping Mall
miami.mall <- st_read('https://opendata.arcgis.com/datasets/cb24d578246647a9a4c57bbd80c1caa8_0.geojson')
miami.mall.sf <- miami.mall %>%
  select(geometry) %>%
  na.omit() %>%
  distinct()

## Miami andmark
miami.landmark <- st_read('https://opendata.arcgis.com/datasets/70a14825e66f4f0eb28d2a9cceba1761_0.geojson')
miami.landmark.sf <- miami.landmark %>%
  select(geometry) %>%
  na.omit() %>%
  distinct()




# ===========================================
# Feature Engineering


## 1. Landmark 
plot.landmark <- ggplot() + 
  geom_sf(data = miami.base, fill = "grey40") +
  stat_density2d(data = data.frame(st_coordinates(miami.landmark.sf)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 40, geom = 'polygon') +
  scale_fill_gradient(low = "#25CB10", high = "#FA7800", name = "Density") +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  labs(title = "Density of Landmark, Miami") +
  mapTheme()

plot.landmark

miami.sf <-
  miami.sf %>% 
  mutate(
    landmark_nn1 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.landmark.sf)), 1),
    landmark_nn2 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.landmark.sf)), 2), 
    landmark_nn3 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.landmark.sf)), 3), 
    landmark_nn4 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.landmark.sf)), 4), 
    landmark_nn5 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.landmark.sf)), 5)) 


## Plot NN count over space  
miami.sf.plot <- miami.sf %>% 
  st_drop_geometry() %>% 
  dplyr::select(Folio, starts_with("landmark_")) %>% 
  tidyr::pivot_longer(cols = -Folio, names_to = "landmark_nn")

ggplot(miami.sf.plot, aes(x = landmark_nn, y = value, group = Folio)) +
  geom_line(alpha = 0.05, color = "firebrick") +
  theme_bw()

### Landmark Correlation 
price.landmark.plot <-
  st_drop_geometry(miami.sf) %>% 
  dplyr::select(SalePrice, starts_with("landmark_")) %>%
  filter(SalePrice <= 1000000) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_point(size = .5) + 
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, nrow = 1, scales = "free") +
  labs(title = "Price as a function of continuous variables") +
  plotTheme()

price.landmark.plot

## 2. Shopping Mall
plot.mall <- ggplot() + 
  geom_sf(data = miami.base, fill = "grey40") +
  stat_density2d(data = data.frame(st_coordinates(miami.mall.sf)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 40, geom = 'polygon') +
  scale_fill_gradient(low = "#25CB10", high = "#FA7800", name = "Density") +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  labs(title = "Density of Shopping Malls, Miami") +
  mapTheme()

plot.mall

miami.sf <-
  miami.sf %>% 
  mutate(
    mall_nn1 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.mall.sf)), 1),
    mall_nn2 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.mall.sf)), 2), 
    mall_nn3 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.mall.sf)), 3), 
    mall_nn4 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.mall.sf)), 4), 
    mall_nn5 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.mall.sf)), 5)) 


## Plot NN count over space  
miami.sf.plot <- miami.sf %>% 
  st_drop_geometry() %>% 
  dplyr::select(Folio, starts_with("mall_")) %>% 
  tidyr::pivot_longer(cols = -Folio, names_to = "mall_nn")

ggplot(miami.sf.plot, aes(x = mall_nn, y = value, group = Folio)) +
  geom_line(alpha = 0.05, color = "firebrick") +
  theme_bw()

### Shopping Mall Correlation 
price.mall.plot <-
  st_drop_geometry(miami.sf) %>% 
  dplyr::select(SalePrice, starts_with("mall_")) %>%
  filter(SalePrice <= 1000000) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_point(size = .5) + 
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, nrow = 1, scales = "free") +
  labs(title = "Price as a function of continuous variables") +
  plotTheme()

price.mall.plot

## 3. Sexual Assaults 

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

### Sexual Assaults Correlation 
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


#================
# Cross Validation on First Model
M1 <- lm(SalePrice ~ ., data = st_drop_geometry(miami.sf) %>% 
           dplyr::select(SalePrice, AdjustedSqFt, LotSize, Bed,
                         Bath, Age,LivingSqFt, ActualSqFt, 
                         sexual_nn1,sexual_nn2,sexual_nn3,sexual_nn4,sexual_nn5,
                         landmark_nn1,landmark_nn2,landmark_nn3,landmark_nn4,landmark_nn5,
                         mall_nn1, mall_nn2, mall_nn3, mall_nn4, mall_nn5) 
)

summary(M1)
# use caret package cross-validation method
fitControl <- caret:: trainControl(method = "cv", 
                                   number = 10,
                                   # savePredictions differs from book
                                   savePredictions = TRUE)

set.seed(717)

# for k-folds CV
m1.cv <- 
  caret::train(SalePrice ~ ., data = st_drop_geometry(miami.sf) %>% 
                 dplyr::select(SalePrice, AdjustedSqFt, LotSize, Bed,
                               Bath, Age,LivingSqFt, ActualSqFt, 
                               sexual_nn1,sexual_nn2,sexual_nn3,sexual_nn4,sexual_nn5,
                               landmark_nn1,landmark_nn2,landmark_nn3,landmark_nn4,landmark_nn5,
                               mall_nn1, mall_nn2, mall_nn3, mall_nn4, mall_nn5) ,
               method = "lm", 
               trControl = fitControl, 
               na.action = na.pass)

m1.cv

m1.cv$resample

m1.cv$resample %>% 
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
m1.cv_preds <- m1.cv$pred
# compare number of observations between data sets
nrow(miami.sf)
nrow(m1.cv_preds)

## Create dataset with "out of fold" predictions and original data
m1.map_preds <- miami.sf %>% 
  rowid_to_column(var = "rowIndex") %>% 
  left_join(m1.cv_preds, by = "rowIndex") %>% 
  mutate(SalePrice.AbsError = abs(m1.cv_preds$pred - miami.sf$SalePrice)) 

# weird CRS fix to miami.sf
st_crs(m1.map_preds) <- st_crs(miami.base)

# plot errors on a map
ggplot() +
  geom_sf(data = miami.base, fill = "grey40") +
  geom_sf(data = m1.map_preds, aes(colour = q5(SalePrice.AbsError)),
          show.legend = "point", size = 1) +
  scale_colour_manual(values = palette,
                      labels=qBr(m1.map_preds,"SalePrice.AbsError"),
                      name="Quintile\nBreaks") +
  labs(title="Absolute sale price errors on the OOF set",
       subtitle = "OOF = 'Out Of Fold'") +
  mapTheme()


#================
# Cross Validation on Second Model
M2 <- lm(SalePrice ~ ., data = st_drop_geometry(miami.sf) %>% 
           dplyr::select(SalePrice, AdjustedSqFt, LotSize, Bed,
                         Bath, ActualSqFt, sexual_nn5,
                         landmark_nn3,landmark_nn4,landmark_nn5,
                         mall_nn1, mall_nn2, mall_nn3) 
)

summary(M2)

set.seed(717)

# for k-folds CV
m2.cv <- 
  caret::train(SalePrice ~ ., data = st_drop_geometry(miami.sf) %>% 
                 dplyr::select(SalePrice, AdjustedSqFt, LotSize, Bed,
                               Bath, Age,LivingSqFt, ActualSqFt, 
                               sexual_nn1,sexual_nn2,sexual_nn3,sexual_nn4,sexual_nn5,
                               landmark_nn1,landmark_nn2,landmark_nn3,landmark_nn4,landmark_nn5,
                               mall_nn1, mall_nn2, mall_nn3, mall_nn4, mall_nn5) ,
               method = "lm", 
               trControl = fitControl, 
               na.action = na.pass)

m2.cv

m2.cv$resample

m2.cv$resample %>% 
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
m2.cv_preds <- m2.cv$pred
# compare number of observations between data sets
nrow(miami.sf)
nrow(m2.cv_preds)

## Create dataset with "out of fold" predictions and original data
m2.map_preds <- miami.sf %>% 
  rowid_to_column(var = "rowIndex") %>% 
  left_join(m2.cv_preds, by = "rowIndex") %>% 
  mutate(SalePrice.AbsError = abs(m2.cv_preds$pred - miami.sf$SalePrice)) 

# weird CRS fix to miami.sf
st_crs(m2.map_preds) <- st_crs(miami.base)

# plot errors on a map
ggplot() +
  geom_sf(data = miami.base, fill = "grey40") +
  geom_sf(data = m2.map_preds, aes(colour = q5(SalePrice.AbsError)),
          show.legend = "point", size = 1) +
  scale_colour_manual(values = palette,
                      labels=qBr(m2.map_preds,"SalePrice.AbsError"),
                      name="Quintile\nBreaks") +
  labs(title="Absolute sale price errors on the OOF set",
       subtitle = "OOF = 'Out Of Fold'") +
  mapTheme()


# ==============
# Spatial Correlation of Errors
miami.training <- miami.sf %>%
  filter(toPredict == '0')

miami.test <- miami.sf %>%
  filter(toPredict == '1') 

m2.training <- lm(SalePrice ~ ., data = as.data.frame(miami.training) %>% 
           dplyr::select(SalePrice, AdjustedSqFt, LotSize, Bed,
                         Bath, ActualSqFt, sexual_nn5,
                         landmark_nn3,landmark_nn4,landmark_nn5,
                         mall_nn1, mall_nn2, mall_nn3) 
)

miami.test <-
  miami.test %>%
  mutate(Regression = "Baseline Regression",
         SalePrice.Predict = predict(m2.training, miami.test),
         SalePrice.Error = SalePrice.Predict - SalePrice,
         SalePrice.AbsError = abs(SalePrice.Predict - SalePrice),
         SalePrice.APE = (abs(SalePrice.Predict - SalePrice)) / SalePrice.Predict)%>%
  filter(SalePrice < 5000000) 


k_nearest_neighbors = 5
#prices
coords <- st_coordinates(miami.sf) 

coords <- as.data.frame(coords) %>%
  group_by(L3) %>%
  summarize(Ave_X = mean(X),
            Ave_Y = mean(Y)) %>%
  select(Ave_X, Ave_Y) %>%
  as.matrix()

# k nearest neighbors
neighborList <- knn2nb(knearneigh(coords, k_nearest_neighbors))
spatialWeights <- nb2listw(neighborList, style="W")
miami.sf$lagPrice <- lag.listw(spatialWeights, miami.sf$SalePrice)

# average errors in five nearest neighbors
coords.test <-  st_coordinates(miami.test) 
coords.test <- as.data.frame(coords.test) %>%
  group_by(L3) %>%
  summarize(Ave_X = mean(X),
            Ave_Y = mean(Y)) %>%
  select(Ave_X, Ave_Y) %>%
  as.matrix()

neighborList.test <- knn2nb(knearneigh(coords.test, k_nearest_neighbors))
spatialWeights.test <- nb2listw(neighborList.test, style="W")
miami.test$lagPriceError <- lag.listw(spatialWeights.test,miami.test$SalePrice.AbsError)

ggplot(miami.sf, aes(x=lagPrice, y=SalePrice)) +
  geom_point(colour = "#FA7800") +
  geom_smooth(method = "lm", se = FALSE, colour = "#25CB10") +
  labs(title = "Price as a function of the spatial lag of price",
       caption = "Public Policy Analytics, Figure 6.6",
       x = "Spatial lag of price (Mean price of 5 nearest neighbors)",
       y = "Sale Price") +
  plotTheme()

ggplot(miami.test, aes(x=lagPriceError, y=SalePrice)) +
  geom_point(colour = "#FA7800") +
  geom_smooth(method = "lm", se = FALSE, colour = "#25CB10") +
  labs(title = "Error as a function of the spatial lag of price",
       caption = "",
       x = "Spatial lag of errors (Mean error of 5 nearest neighbors)",
       y = "Sale Price") +
  plotTheme()


## Moran's I
moranTest <- moran.mc(miami.test$SalePrice.AbsError, 
                      spatialWeights.test, nsim = 999)

ggplot(as.data.frame(moranTest$res[c(1:999)]), aes(moranTest$res[c(1:999)])) +
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = moranTest$statistic), colour = "#FA7800",size=1) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title="Observed and permuted Moran's I",
       subtitle= "Observed Moran's I in orange",
       x="Moran's I",
       y="Count",
       caption="Public Policy Analytics, Figure 6.8") +
  plotTheme()






        
