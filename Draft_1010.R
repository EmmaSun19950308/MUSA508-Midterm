# Load Libraries
library(sf)
library(tidyverse)
# install.packages('mapview')
library(mapview)
library(spdep)
library(caret)
library(ckanr) # for opening data APIs built on CKAN technology
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(jtools)     # for regression model plots
library(stargazer) # for creating table
library(broom)
library(tufte)
library(rmarkdown)
library(kableExtra)
library(tidycensus)


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

# II. DATA
# =====================
# Data Wrangling 

## House Price & internal characteristics
miami.sf <- st_read('/Users/penguin/Box Sync/GitHub/MUSA508-Midterm/studentsData.geojson')


## Miami base data
miami.base <-st_read("https://opendata.arcgis.com/datasets/5ece0745e24b4617a49f2e098df8117f_0.geojson") %>%
  filter(NAME %in% c("MIAMI", "MIAMI BEACH"))

## Neighborhood data
miami.neigh <- st_read('https://opendata.arcgis.com/datasets/2f54a0cbd67046f2bd100fb735176e6c_0.geojson')
plot(miami.neigh)

miami.sf <-st_join(miami.sf,miami.neigh)

### check neighborhood information
miami.neigh.num <- as.data.frame(table(miami.sf$LABEL)) 

## Census Tract data
## View(load_variables(2017,'acs5',cache = TRUE))
tracts17 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E",
                                             "B19013_001E","B25058_001E",
                                             "B06012_002E", 
                                             # vacant variables
                                             "B25002_003E", 
                                             "B25004_002E","B25004_003E",
                                             "B25004_004E","B25004_005E",
                                             # total housing unit
                                             "B25001_001E",
                                             # renter occupied 
                                             'B08137_003E'), 
          year=2017, state= 12, county= 086, geometry=T, output="wide") %>%
  st_transform('EPSG:2236') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E,
         TotalVacant = B25002_003E,
         ForRent = B25004_002E,
         ForRentVac = B25004_003E,
         ForSale = B25004_004E,
         ForSaleVac = B25004_005E,
         TotalUnit = B25001_001E,
         RenterOccupied = B08137_003E
         ) %>%
  dplyr::select(-NAME, -starts_with("B")) %>% #-starts_with("B") awesome!
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         pctTotalVacant = ifelse(TotalUnit > 0, TotalVacant / TotalUnit, 0),
         TotalOccupied = TotalUnit - TotalVacant,
         pctRenterOccupied = ifelse(TotalOccupied >0, RenterOccupied/TotalOccupied, 0)) %>%
  dplyr::select(-Whites, -TotalPoverty) 


projected.tracts17 <- 
  tracts17 %>% 
  st_transform(st_crs(miami.sf))

miami.sf <-st_join(miami.sf, projected.tracts17)


## 1. Miami landmark
miami.landmark <- st_read('https://opendata.arcgis.com/datasets/70a14825e66f4f0eb28d2a9cceba1761_0.geojson')
miami.landmark.sf <- miami.landmark %>%
  select(geometry) %>%
  na.omit() %>%
  distinct()

## 2. Miami Shopping Mall
miami.mall <- st_read('https://opendata.arcgis.com/datasets/cb24d578246647a9a4c57bbd80c1caa8_0.geojson')
miami.mall.sf <- miami.mall %>%
  select(geometry) %>%
  na.omit() %>%
  distinct()

## 3. Miami Sexual Predator
miami.sexual <- st_read('https://opendata.arcgis.com/datasets/f8759d722aeb4198bfe7c4ad780604d2_0.geojson')

miami.sexual.sf <- miami.sexual %>%
  select(geometry) %>%
  na.omit() %>%
  distinct()

## 4. School data
miami.school <- st_read('https://opendata.arcgis.com/datasets/d3db0fce650d4e40a5949b0acae6fe3a_0.geojson')
miami.school.sf <- miami.school %>%
  select(geometry) %>%
  na.omit() %>%
  distinct()


## 5. Hotel data
miami.hotel <- st_read('https://opendata.arcgis.com/datasets/d37bbc15e7304b4ca4607783283147b7_0.geojson')
miami.hotel.sf <- miami.hotel%>%
  select(geometry) %>%
  na.omit() %>%
  distinct()

## 6. College data
miami.college <- st_read('https://opendata.arcgis.com/datasets/7db056c406b943dc8f3f377b99d77588_0.geojson')
miami.college.sf <- miami.college%>%
  select(geometry) %>%
  na.omit() %>%
  distinct()

## 7. Hospital data
miami.hospital <- st_read('https://opendata.arcgis.com/datasets/0067a0e8b40644f980afa23ad34c32c4_0.geojson')
miami.hospital.sf <- miami.hospital %>%
  select(geometry) %>%
  na.omit() %>%
  distinct()





# ===========================================
# Feature Engineering
## 1. Landmark
miami.sf <-
  miami.sf %>% 
  mutate(
    landmark_nn1 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.landmark.sf)), 1),
    landmark_nn2 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.landmark.sf)), 2), 
    landmark_nn3 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.landmark.sf)), 3), 
    landmark_nn4 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.landmark.sf)), 4), 
    landmark_nn5 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.landmark.sf)), 5)) 


## 2. Shopping mall
miami.sf <-
  miami.sf %>% 
  mutate(
    mall_nn1 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.mall.sf)), 1),
    mall_nn2 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.mall.sf)), 2), 
    mall_nn3 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.mall.sf)), 3), 
    mall_nn4 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.mall.sf)), 4), 
    mall_nn5 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.mall.sf)), 5)) 

## 3. Sexual Assaults
miami.sf <-
  miami.sf %>% 
  mutate(
    sexual_nn1 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.sexual.sf)), 1),
    sexual_nn2 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.sexual.sf)), 2), 
    sexual_nn3 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.sexual.sf)), 3), 
    sexual_nn4 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.sexual.sf)), 4), 
    sexual_nn5 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.sexual.sf)), 5)) 


## 4. Hotel Access
miami.sf <-
  miami.sf %>% 
  mutate(
    hotel_nn1 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.hotel.sf)), 1),
    hotel_nn2 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.hotel.sf)), 2), 
    hotel_nn3 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.hotel.sf)), 3), 
    hotel_nn4 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.hotel.sf)), 4), 
    hotel_nn5 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.hotel.sf)), 5)) 


## 5. School Access

miami.sf <-
  miami.sf %>% 
  mutate(
    school_nn1 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.school.sf)), 1),
    school_nn2 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.school.sf)), 2), 
    school_nn3 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.school.sf)), 3), 
    school_nn4 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.school.sf)), 4), 
    school_nn5 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.school.sf)), 5)) 


## 6. College Access
miami.sf <-
  miami.sf %>% 
  mutate(
    college_nn1 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.college.sf)), 1),
    college_nn2 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.college.sf)), 2), 
    college_nn3 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.college.sf)), 3), 
    college_nn4 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.college.sf)), 4), 
    college_nn5 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.college.sf)), 5)) 

## 7. Hospital Access
miami.sf <-
  miami.sf %>% 
  mutate(
    hospital_nn1 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.hospital.sf)), 1),
    hospital_nn2 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.hospital.sf)), 2), 
    hospital_nn3 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.hospital.sf)), 3), 
    hospital_nn4 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.hospital.sf)), 4), 
    hospital_nn5 = nn_function(st_coordinates(st_centroid(miami.sf)), st_coordinates(st_centroid(miami.hospital.sf)), 5)) 

## 8. Internal characteristics
miami.sf <-
  miami.sf %>% 
  mutate(pool_house = ifelse(str_detect(XF1, "Pool"), 1, 0),
         patio_house = ifelse(str_detect(XF1, "Patio"), 1, 0),
         Age = 2020- YearBuilt,
         Age.effective = 2020- EffectiveYearBuilt)


# ============== 
# Split dataset 
miami.training <- miami.sf %>%
  filter(toPredict == '0')

# summary(miami.training)

miami.test <- miami.sf %>%
  filter(toPredict == '1') 

# summary(miami.test)

# =====================================================================
# DATA - 2
# Table of summary statistics 
all.feature.list <- miami.training %>%
  dplyr:: select(-saleDate,-saleType,-saleQual,-saleYear,-Property.Address,
         -Year,-WVDB,-HEX,-GPAR,-County.2nd.HEX,-County.Senior,-County.LongTermSenior,
         -County.Other.Exempt,-City.2nd.HEX,-City.Senior,-City.LongTermSenior,
         -City.Other.Exempt,-MillCode,-Land.Use,-Owner1,-Owner2,-Mailing.Address,
         -Mailing.City,-Mailing.State,-Mailing.Zip,-Mailing.Country,
         -starts_with("Legal"), -YearBuilt, -EffectiveYearBuilt, -X, -FID, -starts_with("Shape_"), -Folio,
         -XF1, -XF2, -XF3) %>%
  st_drop_geometry()


stargazer(all.feature.list, type = 'text')

## 1. internal characteristics
internal.feature.list <- all.feature.list %>%
  dplyr:: select(SalePrice, Land, Bldg, Total, Assessed,County.Taxable,City.Taxable,AdjustedSqFt,
                 LotSize, Bed, Bath, Stories, Units, LivingSqFt, ActualSqFt, Age,Age.effective,
                 pool_house, patio_house) 

stargazer(internal.feature.list, type = 'text')
## 2. amenities/public services
amenity.feature.list <- all.feature.list %>%
  dplyr:: select(starts_with('landmark_'),starts_with('mall_'),starts_with('hospital_'),
                 starts_with('college_'),starts_with('school_'),starts_with('hotel_')  ) 

stargazer(amenity.feature.list, type = 'text')

## 3. spatial structure
spatial.feature.list <- all.feature.list %>%
  dplyr:: select(Property.City, Zoning,LABEL) 

stargazer(spatial.feature.list, type = 'latex')


# ============
# DATA - 3
miami.numericVars <- 
  select_if(all.feature.list, is.numeric) %>% 
  na.omit() 

ggcorrplot(
  round(cor(miami.numericVars), 1), 
  p.mat = cor_pmat(miami.numericVars),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 

# ============
# DATA - 4 Scatter plot 
## 1. Landmark Correlation 
price.landmark.plot <-
  st_drop_geometry(miami.training) %>% 
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

## 2. Shopping Mall Correlation 
price.mall.plot <-
  st_drop_geometry(miami.training) %>% 
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

## 3. Sexual Assaults Correlation 
price.sexual.plot <-
  st_drop_geometry(miami.training) %>% 
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


## 4. House Internal characteristics
price.house.plot <- 
  st_drop_geometry(miami.training) %>% 
  dplyr::select(SalePrice, Land, Bldg, Total, Assessed,County.Taxable,City.Taxable,AdjustedSqFt,
                LotSize, Bed, Bath, Stories, Units, LivingSqFt, ActualSqFt, Age,
                pool_house, patio_house) %>%
  filter(SalePrice <= 1000000, Age < 500) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_point(size = .5) + 
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  plotTheme() + 
  labs(title = "Price as a function of house internal characteristics variables") 

price.house.plot



# =============
# DATA - 5 Map of Sale Price per Square Feet
# 
miami.training <- miami.training %>%
  mutate(priceFt = SalePrice/LivingSqFt)

map.priceFt <- ggplot() + 
  geom_sf(data = st_union(miami.base),fill = 'grey') +
  geom_sf(data = st_centroid(miami.training),aes(color = q5(priceFt)),size = .5) +
  scale_color_manual(values = palette,
                     labels = qBr(miami.training,'priceFt'),
                     name = "Price/ft^2") +
  labs(title = 'Sale Price Per Square Foot\n',
       subtitle = '',
       caption = 'Figure 1.1') +
  mapTheme() + 
  plotTheme()

map.priceFt


# ================
# DATA - 6 

## 1. Landmark
plot.landmark <- ggplot() + 
  geom_sf(data = miami.base, fill = "grey40") +
  stat_density2d(data = data.frame(st_coordinates(miami.landmark.sf)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 40, geom = 'polygon') +
  scale_fill_gradient(low = "#25CB10", high = "#FA7800", name = "Density") +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  labs(title = "Density of landmark Assaults, Miami") +
  mapTheme()

plot.landmark

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

plot.sexual

# =====================================================================
# MODEL BUILDING

## First model: all internal, amenity features
M1 <- lm(SalePrice ~ ., data = st_drop_geometry(miami.training) %>% 
           dplyr::select(colnames(internal.feature.list), 
                         colnames(amenity.feature.list)) 
)

summary(M1)

## Second model: 
M2 <- lm(SalePrice ~ ., data = st_drop_geometry(miami.training) %>% 
           dplyr::select(SalePrice, Land, Bldg, Assessed, County.Taxable,
                         AdjustedSqFt, LotSize, Bed,
                         Bath, ActualSqFt, Age, pool_house, 
                         college_nn2, college_nn1,
                         school_nn3, school_nn4, school_nn5) 
)

summary(M2)


# ==============================================================
# RESULT
# 1. Split dataset
set.seed(31357)

# get index for training sample
inTrain <- caret::createDataPartition(
  y = miami.training$SalePrice, 
  p = .60, list = FALSE)
# split data into training and test
model.miami.training <- miami.training[inTrain,] 
model.miami.test <- miami.training[-inTrain,] 

# 2. Table of model summary on my training set
m2.training <- lm(SalePrice ~ ., data = st_drop_geometry(model.miami.training) %>% 
           dplyr::select(SalePrice, Land, Bldg, Assessed, County.Taxable,
                         AdjustedSqFt, LotSize, Bed,
                         Bath, ActualSqFt, Age, pool_house, 
                         college_nn2, college_nn1,
                         school_nn3, school_nn4, school_nn5
           ) 
)

summary(m2.training)

stargazer(m2.training , type = 'latex')
broom::glance(m2.training)


# 3. MAE, MAPE on my test set
model.miami.test$SalePrice.Predict <- predict(m2.training, newdata = model.miami.test)

m2.miami.test <-
  model.miami.test %>% 
  mutate(SalePrice.Error = SalePrice - SalePrice.Predict,
         SalePrice.AbsError = abs(SalePrice - SalePrice.Predict),
         SalePrice.APE = (abs(SalePrice - SalePrice.Predict)) / SalePrice) 
  
test_GoodnessFit<-as.data.frame(cbind(mean(m2.miami.test$SalePrice.AbsError),mean(m2.miami.test$SalePrice.APE)))
colnames(test_GoodnessFit)<-c("Mean Absoluate Errors (MAE)","MAPE")
kable(test_GoodnessFit, format = "markdown") 


# 4. Cross-Validation Test on Model 2
## 4.1 use caret package cross-validation method
fitControl <- caret:: trainControl(method = "cv", 
                                   number = 100,
                                   savePredictions = TRUE)

set.seed(717)

m2.cv <- 
  caret::train(SalePrice ~ ., data = st_drop_geometry(miami.training) %>% 
                 dplyr::select(SalePrice, Land, Bldg, Assessed, County.Taxable,
                               AdjustedSqFt, LotSize, Bed,
                               Bath, ActualSqFt, Age, pool_house, 
                               college_nn2, college_nn1,
                               school_nn3, school_nn4, school_nn5) ,
               method = "lm", 
               trControl = fitControl, 
               na.action = na.pass)

## 4.2 show RMSE, MAE, R^2
stargazer(m2.cv$resample)

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

ggplot(data = m2.cv$resample) +
  geom_histogram(aes(x = m2.cv$resample$MAE), fill = 'orange') +
  labs(title="Distribution of Cross-validation MAE",
       subtitle = "K = 100") +
  xlab('MAE of Model 2') +
  ylab('Count') +
  plotTheme()
  
# If the model generalized well, the distribution of errors would cluster tightly together. 
# Instead, this range of errors suggests the model predicts inconsistently, and would likely be unreliable for predicting houses that have not recently sold.

# 5. Plot predicted prices as a function of observed prices
preds.train <- data.frame(pred   = predict(m2.training),
                          actual = model.miami.training$SalePrice,
                          source = "training data")
preds.test  <- data.frame(pred   = predict(m2.training,newdata = model.miami.test),
                          actual = model.miami.test$SalePrice,
                          source = "testing data")
preds <- rbind(preds.train, preds.test)

## Overall Plotting
ggplot(preds, aes(x = actual, y = pred, color = source)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkblue", ) +
  geom_abline(color = "orange") +
  coord_equal() +
  theme_bw() +
  labs(title = "Predicted Prices as a Function of Observed Prices\n",
       x = "Observed Prices",
       y = "Predicted Prices") +
  theme(
    legend.position = "none"
  ) +
  plotTheme()

ggplot(preds, aes(x = actual, y = pred, color = source)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green") +
  geom_abline(color = "orange") +
  coord_equal() +
  theme_bw() +
  facet_wrap(~source, ncol = 2) +
  labs(title = "Predicted Prices as a Function of Observed Prices, by Sets",
       subtitle = "",
       x = "Observed Prices",
       y = "Predicted Prices") +
  theme(
    legend.position = "none"
  ) +
  plotTheme()


# 6. 
## 6.1 A map of residuals for test set
map.res.test <- ggplot() + 
  geom_sf(data = st_union(miami.base),fill = 'grey') +
  geom_sf(data = m2.miami.test,aes(color = q5(SalePrice.AbsError)), show.legend = "point",size = .5) +
  scale_color_manual(values = palette,
                     labels = qBr(m2.miami.test,'SalePrice.AbsError'),
                     name = "Residuals") +
  labs(title = 'Map of residuals for test set\n',
       subtitle = '',
       caption = 'Figure 6.1') +
  mapTheme() + 
  plotTheme()
map.res.test


## 6.2 Moran's I Test
moranTest <- moran.mc(model.miami.test$SalePrice.AbsError, 
                      spatialWeights.test, nsim = 999)

moran.plot <- ggplot(as.data.frame(moranTest$res[c(1:999)]), aes(moranTest$res[c(1:999)])) +
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = moranTest$statistic), colour = "#FA7800",size=1) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title="Observed and permuted Moran's I",
       subtitle= "Observed Moran's I in orange",
       x="Moran's I",
       y="Count",
       caption="Figure Result 6.2") +
  plotTheme()

moran.plot
## 6.3 spatial lag in errors


# 7. Map of predicted values for the whole dataset
miami.sf$Predicted.Price<- predict(m2.training, newdata = miami.sf)

map.predicted.price <- ggplot() + 
  geom_sf(data = st_union(miami.base),fill = 'grey') +
  geom_sf(data = st_centroid(miami.sf),aes(color = q5(Predicted.Price)),size = .5) +
  scale_color_manual(values = palette,
                     labels = qBr(miami.sf,'Predicted.Price'),
                     name = "Price, $") +
  labs(title = 'Map of Predicted Sale Price\n',
       subtitle = '',
       caption = 'Figure RESULT 7') +
  mapTheme() + 
  plotTheme()

map.predicted.price



# 8. Map of MAPE by neighborhood
## 8.1 Statistic summary by neighborhood group
nhood_sum <- m2.miami.test %>% 
  group_by(LABEL) %>%
  summarize(meanPrice = mean(SalePrice, na.rm = T),
            meanPrediction = mean(SalePrice.Predict, na.rm = T),
            meanMAE = mean(SalePrice.AbsError, na.rm = T),
            meanMAPE = mean(SalePrice.APE, na.rm = T)) 

nhood_sum %>%
  st_drop_geometry %>%
  arrange(desc(meanMAPE)) %>% 
  kable() %>% 
  kable_styling()

## 8.2 Map of MAPE by neighborhood

miami.neigh.sum <- miami.neigh %>%
  left_join(st_drop_geometry(nhood_sum), by = "LABEL") %>%
  mutate(meanMAPE.expanded = meanMAPE * 100)
  
            
map.MAPE.nhood <- ggplot() + 
  geom_sf(data = miami.neigh.sum,
          aes(fill = q5(meanMAPE.expanded))) + 
  scale_fill_manual(values = palette,
                     labels = qBr(miami.neigh.sum,'meanMAPE.expanded'),
                     name = "MAPE (timed by 100)") +
  labs(title = 'Map of MAPE on Test Set, by Neighborhood\n',
       subtitle = '',
       caption = 'Figure RESULT 8') +
  mapTheme() + 
  plotTheme()

map.MAPE.nhood




# 9. Scatter plot of MAPE by neighborhood as a function of mean price by neighborhood

MAPE.nhood.plot <-ggplot()+
  geom_point(data = nhood_sum, aes(x = meanPrice, y = meanMAPE)) +
  stat_smooth(data = nhood_sum, aes(x = meanPrice, y = meanMAPE), method = "loess", se = FALSE, size = 1, colour="red") + 
  labs(title="MAPE by Neighborhood as a Function of Mean Price by Neighborhood\n") +
  xlab('Mean Price by Neighborhood') +
  ylab('Mean MAPE by Neighborhood') +
  plotTheme()

MAPE.nhood.plot

# 10. Model generalizability on census data
census <-   miami.training %>% # miami.training contains all features we used including census data and geometry information
   mutate(raceContext = ifelse(pctWhite > .5, "Majority White", "Majority Non-White"),
          incomeContext = ifelse(MedHHInc > mean(MedHHInc,na.rm = T), "High Income", "Low Income"),
          povertyContext = ifelse(pctPoverty > .5, "Majority Poverty", "Majority Non-Poverty"),
          vacantContext = ifelse(pctTotalVacant > .5, "Majority Vacant", "Majority Non-Vacant"),
          pctRenterContext = ifelse(pctPoverty > .5, "Majority Renter Occupied", "Majority Non-Renter Occupied")) %>%
  select(raceContext,incomeContext,povertyContext,vacantContext,pctRenterContext)

census <- census %>%
  st_transform(st_crs(tracts17))

grid.arrange(ncol = 2,
             ggplot() + 
               geom_sf(data = st_union(miami.base), fill = 'grey') + 
               geom_sf(data = na.omit(census), aes(color = raceContext)) +
               scale_fill_manual(values = c("#25CB10", "#FA7800"), name="Race Context") +
               labs(title = "Race Context") +
               mapTheme() + 
               theme(legend.position="bottom"), 
             ggplot() + 
               geom_sf(data = na.omit(census), aes(color = incomeContext)) +
               scale_fill_manual(values = c("#25CB10", "#FA7800"), name="Income Context") +
               labs(title = "Income Context") +
               mapTheme() + theme(legend.position="bottom"),
             ggplot() + 
               geom_sf(data = na.omit(census), aes(color = povertyContext)) +
               scale_fill_manual(values = c("#25CB10", "#FA7800"), name="Poverty Context") +
               labs(title = "Poverty Context") +
               mapTheme() + theme(legend.position="bottom"),
             ggplot() + 
               geom_sf(data = na.omit(census), aes(color = vacantContext)) +
               scale_fill_manual(values = c("#25CB10", "#FA7800"), name="Vacant Context") +
               labs(title = "Vacant Context") +
               mapTheme() + theme(legend.position="bottom"),
             ggplot() + 
               geom_sf(data = na.omit(census), aes(color = pctRenterContext)) +
               scale_fill_manual(values = c("#25CB10", "#FA7800"), name="Renter Context") +
               labs(title = "Renter Context") +
               mapTheme() + theme(legend.position="bottom"))


