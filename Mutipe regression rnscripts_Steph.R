install.packages("tidyverse")
library(tidyverse)
install.packages("broom")
library(broom)



clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")

str(clim)
View(clim)

#the code beow faied at first tria, so i had to chec it again and correct it using the codes unique and 
#!is.na before running the as.numeric again

clim$altitude <- as.numeric(clim$altitude)
clim$p_mean <- as.numeric(clim$p_mean)

unique(clim$altitude)
unique(clim$p_mean)

clim <- clim[!is.na(as.numeric(clim$altitude)), ]
clim <- clim[!is.na(as.numeric(clim$p_mean)), ]

View(clim)




climfrar <- clim[-c(35, 36), ]

str(climfrar)
View(climfrar)


model_Mean_Annual_Temperature <- lm(t_mean ~ altitude + lat + lon, data = climfrar)


summary(model_Mean_Annual_Temperature)

tidy_model <- tidy(model_Mean_Annual_Temperature)
# A tibble: 4 × 5
#term        estimate std.error statistic  p.value
#<chr>          <dbl>     <dbl>     <dbl>    <dbl>
# 1 (Intercept) 37.2      2.97        12.5   8.90e-13
#2 altitude    -0.00647  0.000959    -6.74  3.06e- 7
#3 lat         -0.533    0.0626      -8.52  3.90e- 9
#4 lon          0.0370   0.0476       0.778 4.43e- 1

View(tidy_model)

#2 exercise 
# to reduce this and excude the variabe thats non significant, this is the ongitude 
model_Mean_reduced <- lm(t_mean ~ altitude + lat, data = climfrar)

tidy_mode_reduced <- tidy(model_Mean_reduced)
#A tibble: 3 × 5
#term        estimate std.error statistic  p.value
#<chr>          <dbl>     <dbl>     <dbl>    <dbl>
# 1 (Intercept) 38.1      2.73         13.9  4.06e-14
#2 altitude    -0.00639  0.000947     -6.75 2.52e- 7
#3 lat         -0.550    0.0585       -9.40 3.72e-10

#extracted the atitude and atitude for the thw paces from the view function of the datat set before taing out rows 35 ad 36
# Data for prediction
# Data for prediction
new_data <- data.frame(
  altitude = c(1212, 2860),
  latitude = c(44.16, 42.93)
)

new_data

pred_mode_reduced <- predict(model_Mean_reduced, newdata = list("altitude" = c(1212, 2860), "lat" = c(44.16, 42.93)))
pred_mode_reduced
#1         2 
#6.056723 -3.791931

clim$t_mean[clim$station=="Mont-Ventoux"] 
clim$t_mean[clim$station=="Pic-du-Midi"]  

#FOR Mont-Ventoux
# The original value of mean temperature of 3.6 degrees centigrade is not thesame with predicted value of 6.056723 degrees

#FOR Pic-du-Midi
# The original value of mean temperature of -1.2 degrees centigrade is not thesame with predicted value of -3.791931 degrees



#3 exercise
install.packages("scatterplot3d")
library(scatterplot3d)

scatter_3d <- scatterplot3d(
  x = climfrar$altitude,  
  y = climfrar$lat,       
  z = climfrar$t_mean,    
  pch = 16,               
  highlight.3d = TRUE,    
  angle = 45,             
  color = "blue",         
  main = "3D Scatterplot with Regression Plane",
  xlab = "Altitude",
  ylab = "Latitude",
  zlab = "Mean Temperature"
)
summary(model_Mean_reduced)

 

