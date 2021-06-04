abnb_bar <- read.csv('/home/dsninja/DataScience/WorkAreas/RStudio/datasets/AIRBNB/bar_airbnb_listings.csv')
abnb_bar_cleaned <- abnb_bar[c('id', 'name', 'host_id', 'host_name', 'host_since', 'host_location', 
                               'host_response_time', 'host_response_rate', 'host_acceptance_rate', 
                               'host_is_superhost', 'host_total_listings_count', 
                               'host_identity_verified', 'neighbourhood_group_cleansed','latitude', 
                               'longitude', 'property_type', 'room_type', 'accommodates', 
                               'bathrooms_text', 'bedrooms', 'beds', 'price', 'minimum_nights', 
                               'availability_30', 'availability_60',  'number_of_reviews', 
                               'review_scores_rating', 'review_scores_accuracy',  
                               'review_scores_cleanliness',
                               'review_scores_checkin', 'review_scores_communication',
                               'review_scores_location', 'review_scores_value')]

abnb_bar_cleaned$City <- "Barcelona"

abnb_mad <- read.csv('/home/dsninja/DataScience/WorkAreas/RStudio/datasets/AIRBNB/mad_airbnb_listings.csv')
abnb_mad_cleaned <- abnb_mad[c('id', 'name', 'host_id', 'host_name', 'host_since', 'host_location', 
                               'host_response_time', 'host_response_rate', 'host_acceptance_rate', 
                               'host_is_superhost', 'host_total_listings_count', 
                               'host_identity_verified', 'neighbourhood_group_cleansed','latitude', 
                               'longitude', 'property_type', 'room_type', 'accommodates', 
                               'bathrooms_text', 'bedrooms', 'beds', 'price', 'minimum_nights', 
                               'availability_30', 'availability_60',  'number_of_reviews', 
                               'review_scores_rating', 'review_scores_accuracy',  
                               'review_scores_cleanliness',
                               'review_scores_checkin', 'review_scores_communication',
                               'review_scores_location', 'review_scores_value')]

abnb_mad_cleaned$City <- "Madrid"

abnb_eus <- read.csv('/home/dsninja/DataScience/WorkAreas/RStudio/datasets/AIRBNB/eus_airbnb_listings.csv')
abnb_eus_cleaned <- abnb_eus[c('id', 'name', 'host_id', 'host_name', 'host_since', 'host_location', 
                               'host_response_time', 'host_response_rate', 'host_acceptance_rate', 
                               'host_is_superhost', 'host_total_listings_count', 
                               'host_identity_verified', 'neighbourhood_group_cleansed','latitude', 
                               'longitude', 'property_type', 'room_type', 'accommodates', 
                               'bathrooms_text', 'bedrooms', 'beds', 'price', 'minimum_nights', 
                               'availability_30', 'availability_60',  'number_of_reviews', 
                               'review_scores_rating', 'review_scores_accuracy',  
                               'review_scores_cleanliness',
                               'review_scores_checkin', 'review_scores_communication',
                               'review_scores_location', 'review_scores_value')]

abnb_eus_cleaned$City <- "Euskadi"

abnb_may <- read.csv('/home/dsninja/DataScience/WorkAreas/RStudio/datasets/AIRBNB/may_airbnb_listings.csv')
abnb_may_cleaned <- abnb_may[c('id', 'name', 'host_id', 'host_name', 'host_since', 'host_location', 
                               'host_response_time', 'host_response_rate', 'host_acceptance_rate', 
                               'host_is_superhost', 'host_total_listings_count', 
                               'host_identity_verified', 'neighbourhood_group_cleansed','latitude', 
                               'longitude', 'property_type', 'room_type', 'accommodates', 
                               'bathrooms_text', 'bedrooms', 'beds', 'price', 'minimum_nights', 
                               'availability_30', 'availability_60',  'number_of_reviews', 
                               'review_scores_rating', 'review_scores_accuracy',  
                               'review_scores_cleanliness',
                               'review_scores_checkin', 'review_scores_communication',
                               'review_scores_location', 'review_scores_value')]

abnb_may_cleaned$City <- "Mallorca"

abnb_mal <- read.csv('/home/dsninja/DataScience/WorkAreas/RStudio/datasets/AIRBNB/mal_airbnb_listings.csv')
abnb_mal_cleaned <- abnb_mal[c('id', 'name', 'host_id', 'host_name', 'host_since', 'host_location', 
                               'host_response_time', 'host_response_rate', 'host_acceptance_rate', 
                               'host_is_superhost', 'host_total_listings_count', 
                               'host_identity_verified', 'neighbourhood_group_cleansed','latitude', 
                               'longitude', 'property_type', 'room_type', 'accommodates', 
                               'bathrooms_text', 'bedrooms', 'beds', 'price', 'minimum_nights', 
                               'availability_30', 'availability_60',  'number_of_reviews', 
                               'review_scores_rating', 'review_scores_accuracy',  
                               'review_scores_cleanliness',
                               'review_scores_checkin', 'review_scores_communication',
                               'review_scores_location', 'review_scores_value')]

abnb_mal_cleaned$City <- "Malaga"


abnb_raw <- rbind(abnb_bar_cleaned, abnb_mad_cleaned, abnb_eus_cleaned, abnb_may_cleaned, abnb_mal_cleaned)

sapply(abnb,function(x)sum(is.na(x))) 
abnb <- abnb[,-c(4,11)]
sapply(abnb,function(x)sum(is.na(x))) 
abnb <- abnb[complete.cases(abnb), ]
str(abnb)
abnb$SuperHost <- ifelse(abnb$host_is_superhost=='t',"YES",'NO')
abnb$SuperHost <- as.factor(abnb$SuperHost)
abnb <- abnb[,-7]
abnb$CityL <- ifelse(abnb$City == 1, "Barcelona",
                    ifelse(abnb$City == 2, "Madrid",
                           ifelse(abnb$City == 3, "Euskadi",
                                  ifelse(abnb$City == 4, "Mallorca", "Malaga"))))

abnb$CityL <- as.factor(abnb$CityL)
abnb <- abnb[,-29]
str(abnb)
abnb$priceN <- round(as.numeric(as.character(gsub("\\$","",abnb$price))))
sapply(abnb,function(x)sum(is.na(x))) 
abnb <- abnb[complete.cases(abnb), ]
sapply(abnb,function(x)sum(is.na(x))) 
summary(abnb$priceN)
abnb$priceD <- cut(abnb$priceN, breaks = c(0,100,200,300,400,500,600,700,800,900,1000), 
                   labels = c("0-100", "100-200", "200-300", "300-400",
                              "400-500","500-600","600-700","700-800","800-900","900-1000"))

write.csv(abnb,"/home/dsninja/DataScience/WorkAreas/RStudio/datasets/AIRBNB/abnb_raw_good.csv", row.names = FALSE)
