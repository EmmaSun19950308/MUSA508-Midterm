
# II. DATA
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

## Miami landmark
miami.landmark <- st_read('https://opendata.arcgis.com/datasets/70a14825e66f4f0eb28d2a9cceba1761_0.geojson')
miami.landmark.sf <- miami.landmark %>%
  select(geometry) %>%
  na.omit() %>%
  distinct()

## School data
miami.school <- st_read('https://opendata.arcgis.com/datasets/d3db0fce650d4e40a5949b0acae6fe3a_0.geojson')
miami.school.sf <- miami.school %>%
  select(geometry) %>%
  na.omit() %>%
  distinct()


## Hotel data
miami.hotel <- st_read('https://opendata.arcgis.com/datasets/7db056c406b943dc8f3f377b99d77588_0.geojson')
miami.hotel.sf <- miami.hotel%>%
  select(geometry) %>%
  na.omit() %>%
  distinct()


## Neighborhood data



# ===========================================
# Feature Engineering



## Plot Landmark_NN count over space  
miami.sf.plot <- miami.sf %>% 
  st_drop_geometry() %>% 
  dplyr::select(Folio, starts_with("landmark_")) %>% 
  tidyr::pivot_longer(cols = -Folio, names_to = "landmark_nn")

ggplot(miami.sf.plot, aes(x = landmark_nn, y = value, group = Folio)) +
  geom_line(alpha = 0.05, color = "firebrick") +
  theme_bw()



## Plot mall_NN count over space  
miami.sf.plot <- miami.sf %>% 
  st_drop_geometry() %>% 
  dplyr::select(Folio, starts_with("mall_")) %>% 
  tidyr::pivot_longer(cols = -Folio, names_to = "mall_nn")

ggplot(miami.sf.plot, aes(x = mall_nn, y = value, group = Folio)) +
  geom_line(alpha = 0.05, color = "firebrick") +
  theme_bw()



## Plot sexual_NN count over space - Should increase or decrease?
miami.sf.plot <- miami.sf %>% 
  st_drop_geometry() %>% 
  dplyr::select(Folio, starts_with("sexual_")) %>% 
  tidyr::pivot_longer(cols = -Folio, names_to = "sexual_nn")

ggplot(miami.sf.plot, aes(x = sexual_nn, y = value, group = Folio)) +
  geom_line(alpha = 0.05, color = "firebrick") +
  theme_bw()







### 3. Correlation matrix 
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
c(cor.test(miami.sf$AdjustedSqFt, miami.sf$SalePrice, method = "pearson")$estimate,
  cor.test(miami.sf$Age, miami.sf$SalePrice, method = "pearson")$estimate,
  cor.test(miami.sf$AdjustedSqFt, miami.sf$SalePrice, method = "pearson")$estimate,
  cor.test(miami.sf$Age, miami.sf$SalePrice, method = "pearson")$estimate,
  cor.test(miami.sf$Bed, miami.sf$SalePrice, method = "pearson")$estimate,
  cor.test(miami.sf$LotSize, miami.sf$SalePrice, method = "pearson")$estimate,
  cor.test(miami.sf$Bath, miami.sf$SalePrice, method = "pearson")$estimate,
  cor.test(miami.sf$LivingSqFt, miami.sf$SalePrice, method = "pearson")$estimate,
  cor.test(miami.sf$ActualSqFt, miami.sf$SalePrice, method = "pearson")$estimate)



## 4. Home price correlation scatterplots
###上面做了，换顺序即可。



# 6. Map of Dependent Variables




#================

# 
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


# results - 6 
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



        
