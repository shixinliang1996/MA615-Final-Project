library(psych)
library(GPArotation)
library(tidyverse)
library(readxl)
library(janitor)
library(magrittr)
library(GGally)
library(ggmosaic)

# Data Cleaning
listing <- read.csv("listings.csv", header = TRUE)
review <- read.csv("reviews.csv", header = TRUE)

listing$host_is_superhost <- as.character(listing$host_is_superhost)
listing <- filter(listing, listing$host_is_superhost!="")
listing$host_is_superhost[listing$host_is_superhost=="t"] <- "TRUE"
listing$host_is_superhost[listing$host_is_superhost=="f"] <- "FALSE"
listing$host_is_superhost <- as.factor(listing$host_is_superhost)

listing$host_since_year <- listing$host_since
listing$host_since_year <- as.Date(listing$host_since_year)
listing$host_since_year <- format(listing$host_since_year, "%Y")

listing$host_response_time[listing$host_response_time==""] <- NA
listing$host_response_time[listing$host_response_time=="N/A"] <- NA

listing$host_response_rate <- as.character(listing$host_response_rate)
listing$host_response_rate[listing$host_response_rate==""] <- NA
listing$host_response_rate[listing$host_response_rate=="N/A"] <- NA
listing$host_response_rate <- as.factor(listing$host_response_rate)
host_response_rate_level <- levels(listing$host_response_rate)
host_response_rate_level <- host_response_rate_level[c(1,3:35,2)]
listing$host_response_rate <- factor(listing$host_response_rate, ordered = TRUE, levels = host_response_rate_level)

listing$host_has_profile_pic <- as.character(listing$host_has_profile_pic)
listing <- filter(listing, listing$host_has_profile_pic!="")
listing$host_has_profile_pic[listing$host_has_profile_pic=="t"] <- "TRUE"
listing$host_has_profile_pic[listing$host_has_profile_pic=="f"] <- "FALSE"
listing$host_has_profile_pic <- as.factor(listing$host_has_profile_pic)

listing$host_identity_verified <- as.character(listing$host_identity_verified)
listing <- filter(listing, listing$host_identity_verified!="")
listing$host_identity_verified[listing$host_identity_verified=="t"] <- "TRUE"
listing$host_identity_verified[listing$host_identity_verified=="f"] <- "FALSE"
listing$host_identity_verified <- as.factor(listing$host_identity_verified)

listing <- filter(listing, listing$neighbourhood!="")

listing$price <- gsub('[$]','',listing$price) 
listing$price <- as.numeric(gsub(',','',listing$price)) 

listing$security_deposit <- gsub('[$]','',listing$security_deposit) 
listing$security_deposit <- as.numeric(gsub(',','',listing$security_deposit)) 

listing$cleaning_fee <- gsub('[$]','',listing$cleaning_fee) 
listing$cleaning_fee <- as.numeric(gsub(',','',listing$cleaning_fee)) 

listing$extra_people <- gsub('[$]','',listing$extra_people) 
listing$extra_people <- as.numeric(gsub(',','',listing$extra_people)) 

listing$require_guest_profile_picture <- as.character(listing$require_guest_profile_picture)
listing$require_guest_profile_picture[listing$require_guest_profile_picture=="t"] <- "TRUE"
listing$require_guest_profile_picture[listing$require_guest_profile_picture=="f"] <- "FALSE"
listing$require_guest_profile_picture <- as.factor(listing$require_guest_profile_picture)

# --------------------------- --------------------------- --------------------------- --------------------------- 
# Plots
## Part 1: Host
superhost_plot <- ggplot(listing) +
    aes(x = host_is_superhost, fill = host_is_superhost) + 
    geom_bar() + 
    scale_fill_viridis_d() + 
    theme_minimal() + 
    labs(x = "Is Superhost?", y = "Frequency") + 
    ggtitle("Superhost Frequency") + 
    labs(caption = "Figure 1")

host_since_plot <- ggplot(listing) +
    aes(x = host_since_year) + 
    geom_bar(aes(fill = host_is_superhost)) + 
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(x = "Host Begin Year", y = "Frequency") + 
    ggtitle("Host Begin Year Distribution") +
    labs(caption = "Figure 2")

host_response_time_plot <- ggplot(listing) +
    aes(x = host_response_time) + 
    geom_bar(aes(fill = host_is_superhost)) + 
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(x = "Host Response Time", y = "Frequency") + 
    ggtitle("Host Response Time Distribution") +
    labs(caption = "Figure 3")

host_response_rate_plot <- ggplot(listing) +
    aes(x = host_response_rate) + 
    geom_bar(aes(fill = host_is_superhost)) + 
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(x = "Host Response Rate", y = "Frequency") + 
    ggtitle("Host Response Rate Distribution") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 10)) + 
    labs(caption = "Figure 4")

host_has_profile_pic_plot <- ggplot(listing) +
    aes(x = host_has_profile_pic) + 
    geom_bar(aes(fill = host_is_superhost)) + 
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(x = "Host Has Profile Picture", y = "Frequency") + 
    ggtitle("Host Has Profile Picture Distribution") + 
    labs(caption = "Figure 5")

host_identity_verified_plot <- ggplot(listing) +
    aes(x = host_identity_verified) + 
    geom_bar(aes(fill = host_is_superhost)) + 
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(x = "Host Identity Verified", y = "Frequency") + 
    ggtitle("Host Identity Verified Distribution") + 
    labs(caption = "Figure 6")

## Part 2: Listing
neighbourhood_plot <- ggplot(listing) +
    aes(x = neighbourhood) + 
    geom_bar(aes(fill = host_is_superhost)) + 
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(x = "Neighbourhood", y = "Frequency") + 
    ggtitle("Neighbourhood Distribution") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 10)) + 
    labs(caption = "Figure 7")

room_type_plot <- ggplot(listing) +
    geom_mosaic(aes(x = product(host_is_superhost, room_type), fill=host_is_superhost), na.rm=TRUE) + 
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() +  
    labs(x = "Room Type", y = "Is Superhost?", title='Room Type Distribution')+
    theme(axis.text.x = element_text(hjust = 1, size=7)) + 
    labs(caption = "Figure 8")

accommodates_plot <- ggplot(listing) +
    geom_mosaic(aes(x = product(host_is_superhost, accommodates), fill=host_is_superhost), na.rm=TRUE) + 
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() +  
    labs(x = "Accommodates", y = "Is Superhost?", title='Accommodates Distribution')+
    theme(axis.text.x = element_text(hjust = 1, size=7)) + 
    labs(caption = "Figure 9")

bathrooms_plot <- ggplot(listing) +
    geom_mosaic(aes(x = product(host_is_superhost, bathrooms), fill=host_is_superhost), na.rm=TRUE) + 
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() +  
    labs(x = "Number of Bathrooms", y = "Is Superhost?", title='Number of Bathrooms Distribution')+
    theme(axis.text.x = element_text(hjust = 1, size=7)) + 
    labs(caption = "Figure 10")

bedrooms_plot <- ggplot(listing) +
    geom_mosaic(aes(x = product(host_is_superhost, bedrooms), fill=host_is_superhost), na.rm=TRUE) + 
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() +  
    labs(x = "Number of Bedrooms", y = "Is Superhost?", title='Number of Bedrooms Distribution')+
    theme(axis.text.x = element_text(hjust = 1, size=7)) + 
    labs(caption = "Figure 11")

beds_plot <- ggplot(listing) +
    geom_mosaic(aes(x = product(host_is_superhost, beds), fill=host_is_superhost), na.rm=TRUE) + 
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() +  
    labs(x = "Number of Beds", y = "Is Superhost?", title='Number of Beds Distribution')+
    theme(axis.text.x = element_text(hjust = 1, size=6)) + 
    labs(caption = "Figure 12")

bed_type_plot <- ggplot(listing) +
    aes(x = bed_type) + 
    geom_bar(aes(fill = host_is_superhost)) + 
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(x = "Bed Type", y = "Frequency") + 
    ggtitle("Bed Type Distribution") + 
    theme(axis.text.x = element_text(hjust = 1, size = 10)) + 
    labs(caption = "Figure 13")

## Part 3: Price
price_rating_plot <- ggplot(data = listing, aes(x = review_scores_rating, y = price)) +
    geom_point(aes(colour = host_is_superhost)) + 
    scale_colour_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(y = "Price", x = "Rating") + 
    ggtitle("Price vs. Rating") + 
    theme(axis.text.x = element_text(hjust = 1, size = 8)) +
    labs(caption = "Figure 14")

security_deposit_rating_plot <- ggplot(data = listing, aes(x = review_scores_rating, y = security_deposit)) +
    geom_point(aes(colour = host_is_superhost)) + 
    scale_colour_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(y = "Security Deposit", x = "Rating") + 
    ggtitle("Security Deposit vs. Rating") + 
    theme(axis.text.x = element_text(hjust = 1, size = 8)) +
    labs(caption = "Figure 15")

cleaning_fee_plot <- ggplot(data = listing, aes(x = review_scores_rating, y = cleaning_fee)) +
    geom_point(aes(colour = host_is_superhost)) + 
    scale_colour_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(y = "Cleaning Fee", x = "Rating") + 
    ggtitle("Cleaning Fee vs. Rating") + 
    theme(axis.text.x = element_text(hjust = 1, size = 8)) +
    labs(caption = "Figure 16")

extra_people_plot <- ggplot(data = listing, aes(x = review_scores_rating, y = extra_people)) +
    geom_point(aes(colour = host_is_superhost)) + 
    scale_colour_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(y = "Extra People Fee", x = "Rating") + 
    ggtitle("Extra People Fee vs. Rating") + 
    theme(axis.text.x = element_text(hjust = 1, size = 8)) + 
    theme(axis.text.y = element_text(hjust = 1, size = 6)) +
    labs(caption = "Figure 17")

## Part 4: Rules
minimum_nights_plot <- ggplot(data = filter(listing, listing$minimum_nights<250 & listing$maximum_nights<250), aes(y = maximum_nights, x = minimum_nights)) +
    geom_point(aes(colour = host_is_superhost)) + 
    scale_colour_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(x = "Minimum Nights", y = "Maximum Nights") + 
    ggtitle("Minimum Nights vs. Maximum Nights") + 
    theme(axis.text.x = element_text(hjust = 1, size = 8)) +
    labs(caption = "Figure 18")

cancellation_policy_plot <- ggplot(listing) +
    geom_mosaic(aes(x = product(host_is_superhost, cancellation_policy), fill=host_is_superhost), na.rm=TRUE) + 
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() +  
    labs(x = "Cancellation Policy", y = "Is Superhost?", title='Cancellation Policy Distribution')+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size=7)) + 
    labs(caption = "Figure 19")

require_guest_profile_picture_plot <- ggplot(listing) +
    geom_mosaic(aes(x = product(host_is_superhost, require_guest_profile_picture), fill=host_is_superhost), na.rm=TRUE) +
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() +  
    labs(x = "Require Guest Profile Picture", y = "Is Superhost?", title='Require Guest Profile Picture Distribution')+
    theme(axis.text.x = element_text(hjust = 1, size=10)) + 
    labs(caption = "Figure 20")

## Part 5: Review
avg_number_of_reviews <- aggregate(listing$number_of_reviews, list(listing$host_is_superhost), mean)
number_of_reviews_plot <- ggplot(data = avg_number_of_reviews, aes(x = Group.1, y = x, fill = Group.1)) + 
    geom_bar(stat="identity") +
    scale_fill_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(x = "Is Superhost?", y = "Number of Reviews") + 
    ggtitle("Number of Reviews Distribution") + 
    labs(caption = "Figure 21")

review_scores_rating_plot <- ggplot(data = listing, aes(x = review_scores_rating, y = number_of_reviews)) +
    geom_point(aes(colour = host_is_superhost)) + 
    scale_colour_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(y = "Number of Reviews", x = "Average Rating") + 
    ggtitle("Number of Reviews vs. Average Rating") + 
    theme(axis.text.x = element_text(hjust = 1, size = 8)) +
    labs(caption = "Figure 22")

review_scores_accuracy_plot <- ggplot(data = listing, aes(x = review_scores_rating, y = review_scores_accuracy)) +
    geom_point(aes(colour = host_is_superhost)) + 
    scale_colour_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(y = "Accuracy Rating", x = "Average Rating") + 
    ggtitle("Accuracy Rating vs. Average Rating") + 
    theme(axis.text.x = element_text(hjust = 1, size = 8)) +
    labs(caption = "Figure 23")

review_scores_cleanliness_plot <- ggplot(data = listing, aes(x = review_scores_rating, y = review_scores_cleanliness))+
    geom_point(aes(colour = host_is_superhost)) + 
    scale_colour_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(y = "Cleanliness Rating", x = "Average Rating") + 
    ggtitle("Cleanliness Rating vs. Average Rating") + 
    theme(axis.text.x = element_text(hjust = 1, size = 8)) +
    labs(caption = "Figure 24")

review_scores_checkin_plot <- ggplot(data = listing, aes(x = review_scores_rating, y = review_scores_checkin)) +
    geom_point(aes(colour = host_is_superhost)) + 
    scale_colour_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(y = "Checkin Rating", x = "Average Rating") + 
    ggtitle("Checkin Rating vs. Average Rating") + 
    theme(axis.text.x = element_text(hjust = 1, size = 8)) +
    labs(caption = "Figure 25")

review_scores_communication_plot <- ggplot(data = listing, aes(x = review_scores_rating, y = review_scores_communication)) +
    geom_point(aes(colour = host_is_superhost)) + 
    scale_colour_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(y = "Communication Rating", x = "Average Rating") + 
    ggtitle("Communication Rating vs. Average Rating") + 
    theme(axis.text.x = element_text(hjust = 1, size = 8)) +
    labs(caption = "Figure 26")

review_scores_location_plot <- ggplot(data = listing, aes(x = review_scores_rating, y = review_scores_location)) +
    geom_point(aes(colour = host_is_superhost)) + 
    scale_colour_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(y = "Location Rating", x = "Average Rating") + 
    ggtitle("Location Rating vs. Average Rating") + 
    theme(axis.text.x = element_text(hjust = 1, size = 8)) +
    labs(caption = "Figure 27")

review_scores_value_plot <- ggplot(data = listing, aes(x = review_scores_rating, y = review_scores_value)) +
    geom_point(aes(colour = host_is_superhost)) + 
    scale_colour_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(y = "Value Rating", x = "Average Rating") + 
    ggtitle("Value Rating vs. Average Rating") + 
    theme(axis.text.x = element_text(hjust = 1, size = 8)) +
    labs(caption = "Figure 28")

review_scores_rating_per_plot <- ggplot(data = listing, aes(x = reviews_per_month, y = number_of_reviews)) +
    geom_point(aes(colour = host_is_superhost)) + 
    scale_colour_viridis_d(name="Is Superhost?") + 
    theme_minimal() + 
    labs(y = "Number of Reviews", x = "Number of Reviews Per Month") + 
    ggtitle("Number of Reviews vs. Number of Reviews Per Month") + 
    theme(axis.text.x = element_text(hjust = 1, size = 8)) +
    labs(caption = "Figure 29")

# --------------------------- --------------------------- --------------------------- --------------------------- 
# Maps
library(knitr)
library(leaflet)
library(maps)
library(htmlwidgets)
library(tidyverse)
library(ggmap)
# --------------------------- --------------------------- --------------------------- --------------------------- 
# Text Ming
## --------------------------- name--------------------------- 
library(dplyr)
name_df <- listing[, c(5, 29)]
name_df$name <- as.character(name_df$name)
library(tidytext)

name_df <- name_df %>%  
    group_by(host_is_superhost) %>%
    unnest_tokens(word, name)
name_df <- name_df %>%
    count(word, sort = TRUE) 
data(stop_words)
name_df <- name_df %>%
    anti_join(stop_words)

# a visualization of the most common words
name_df_superhost_frequency <- name_df  %>%
    filter(n > 50) %>%
    filter(host_is_superhost=="TRUE") %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() + 
    ggtitle("Most Common Words of Listings' Name (Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12))

name_df_not_superhost_frequency <- name_df  %>%
    filter(n > 120) %>%
    filter(host_is_superhost=="FALSE") %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()+ 
    ggtitle("Most Common Words of Listings' Name (Not Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12))

# Sentiment analysis
library(tidytext)
# sentiment frequency: joy
nrc_joy <- get_sentiments("nrc") %>% 
    filter(sentiment == "joy")

name_df_joy <- name_df %>%
    inner_join(nrc_joy) 

name_df_joy_superhost <- name_df %>%
    filter(n >= 10) %>%
    filter(host_is_superhost=="TRUE") %>%
    inner_join(nrc_joy) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()+ 
    ggtitle("Most Common Joy Words of Listings' Name (Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12))

name_df_joy_not_superhost <- name_df %>%
    filter(n >= 20) %>%
    filter(host_is_superhost=="FALSE") %>%
    inner_join(nrc_joy) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()+ 
    ggtitle("Most Common Joy Words of Listings' Name (Not Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12)) 


# --------------------------- Description--------------------------- 
description_df <- listing[, c(8, 29)]
description_df$description <- as.character(description_df$description)
description_df <- description_df %>%  
    group_by(host_is_superhost) %>%
    unnest_tokens(word, description)
description_df <- description_df %>%
    count(word, sort = TRUE) 
description_df <- description_df %>%
    anti_join(stop_words)

# a visualization of the most common words
description_df_superhost_frequency <- description_df  %>%
    filter(n > 500) %>%
    filter(host_is_superhost=="TRUE") %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()+ 
    ggtitle("Most Common Words of Listings' Description (Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12))

description_df_not_superhost_frequency <- description_df  %>%
    filter(n > 1000) %>%
    filter(host_is_superhost=="FALSE") %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()+ 
    ggtitle("Most Common Words of Listings' Description (Not Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12))

# Sentiment analysis
# sentiment frequency: joy
description_df_joy <- description_df %>%
    inner_join(nrc_joy) 

description_df_joy_superhost <- description_df %>%
    filter(n >= 50) %>%
    filter(host_is_superhost=="TRUE") %>%
    inner_join(nrc_joy) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()+ 
    ggtitle("Most Common Joy Words of Listings' Description (Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12))

description_df_joy_not_superhost <- description_df %>%
    filter(n >= 100) %>%
    filter(host_is_superhost=="FALSE") %>%
    inner_join(nrc_joy) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()+ 
    ggtitle("Most Common Joy Words of Listings' Description (Not Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12)) 



# --------------------------- Review Text: comments---------------------------  
listing_superhost <- listing[, c(1, 29)]
colnames(listing_superhost)[1] <- "listing_id"
review_df  <- list(review, listing_superhost) %>% reduce(full_join, by = "listing_id")
comments_df <- review_df[, c(6, 7)]
comments_df$comments <- as.character(comments_df$comments)
comments_df <- na.omit(comments_df)
comments_df <- comments_df %>%  
    group_by(host_is_superhost) %>%
    unnest_tokens(word, comments)
comments_df <- comments_df %>%
    count(word, sort = TRUE) 
comments_df <- comments_df %>%
    anti_join(stop_words)

# a visualization of the most common words
comments_df_superhost_frequency <- comments_df  %>%
    filter(n > 9000) %>%
    filter(host_is_superhost=="TRUE") %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()+ 
    ggtitle("Most Common Words of Listings' Comments (Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12))

comments_df_not_superhost_frequency <- comments_df  %>%
    filter(n > 6000) %>%
    filter(host_is_superhost=="FALSE") %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()+ 
    ggtitle("Most Common Words of Listings' Comments (Not Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12))

# Sentiment analysis
# Most common positive and negative words
comments_df_superhost_bing_word_counts <- comments_df %>%
    filter(host_is_superhost=="TRUE") %>%
    inner_join(get_sentiments("bing")) 

comments_df_superhost_bing_word_counts_positive <- comments_df_superhost_bing_word_counts  %>%
    filter(n > 5000) %>%
    filter(sentiment=="positive") %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(fill="#388288") +
    xlab(NULL) +
    labs(y = "Contribution to Positive Sentiment") + 
    coord_flip()+ 
    ggtitle("Most Common Positive Words of Listings' Comments (Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12))

comments_df_superhost_bing_word_counts_negative <- comments_df_superhost_bing_word_counts  %>%
    filter(n > 250) %>%
    filter(sentiment=="negative") %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(fill="#ED6664") +
    xlab(NULL) +
    labs(y = "Contribution to Negative Sentiment") + 
    coord_flip()+ 
    ggtitle("Most Common Negative Words of Listings' Comments (Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12))

comments_df_not_superhost_bing_word_counts <- comments_df %>%
    filter(host_is_superhost=="FALSE") %>%
    inner_join(get_sentiments("bing")) 

comments_df_not_superhost_bing_word_counts_positive <- comments_df_not_superhost_bing_word_counts  %>%
    filter(n > 3000) %>%
    filter(sentiment=="positive") %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(fill="#388288") +
    xlab(NULL) +
    labs(y = "Contribution to Positive Sentiment") + 
    coord_flip()+ 
    ggtitle("Most Common Positive Words of Listings' Comments (Not Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12))

comments_df_not_superhost_bing_word_counts_negative <- comments_df_not_superhost_bing_word_counts  %>%
    filter(n > 300) %>%
    filter(sentiment=="negative") %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(fill="#ED6664") +
    xlab(NULL) +
    labs(y = "Contribution to Positive Sentiment") + 
    coord_flip()+ 
    ggtitle("Most Common Negative Words of Listings' Comments (Not Superhost)") + 
    theme(axis.text.y = element_text(hjust = 1, size = 12))



####################################################################################################################
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("Exploration on Airbnb Superhost in Boston"),
    
    navlistPanel(
        "Plots of Five Main Parts", 
        tabPanel("Part 1: Host Information", 
                 textOutput("plot1"),
                 plotOutput("superhost"),
                 plotOutput("host_since"),
                 plotOutput("host_response_time"),
                 plotOutput("host_response_rate"),
                 plotOutput("host_has_profile_pic"),
                 plotOutput("host_identity_verified")),
        
        tabPanel("Part 2: Listing Information", 
                 textOutput("plot2"),
                 plotOutput("neighbourhood"),
                 plotOutput("room_type"),
                 plotOutput("accommodates"),
                 plotOutput("bathrooms"),
                 plotOutput("bedrooms"),
                 plotOutput("beds"),
                 plotOutput("bed_type")),
        
        tabPanel("Part 3: Price", 
                 textOutput("plot3"),
                 plotOutput("price_rating"),
                 plotOutput("security_deposit_rating"),
                 plotOutput("cleaning_fee"),
                 plotOutput("extra_people")),
        
        tabPanel("Part 4: Rules", 
                 textOutput("plot4"),
                 plotOutput("minimum_nights"),
                 plotOutput("cancellation_policy"),
                 plotOutput("require_guest_profile_picture")),
        
        tabPanel("Part 5: Reviews", 
                 textOutput("plot5"),
                 plotOutput("number_of_reviews"),
                 plotOutput("review_scores_rating"),
                 plotOutput("review_scores_rating_per")),
        
        "Maps",
        
        tabPanel("Location of Listings", 
                 leafletOutput(outputId = "mymap1"),
                 textOutput("information1")
        ),
        
        tabPanel("Location of Superhost",
                 #this will create a space to display our map
                 leafletOutput(outputId = "mymap"), 
                 textOutput("information"),
                 #put the checkmarks ontop of the map
                 absolutePanel(top = 130, right = 0, 
                               checkboxInput("year", "Is Superhost?", FALSE))),
        
        "Text Ming",
        
        tabPanel("Name of Listings",
                 textOutput("text1"),
                 plotOutput("name_df_superhost_frequency"),
                 plotOutput("name_df_not_superhost_frequency"),
                 plotOutput("name_df_joy_superhost"),
                 plotOutput("name_df_joy_not_superhost")),
        
        tabPanel("Description of Listings",
                 textOutput("text2"),
                 plotOutput("description_df_superhost_frequency"),
                 plotOutput("description_df_not_superhost_frequency"),
                 plotOutput("description_df_joy_superhost"),
                 plotOutput("description_df_joy_not_superhost")),
        
        tabPanel("Comments", 
                 textOutput("text3"),
                 plotOutput("comments_df_superhost_frequency"),
                 plotOutput("comments_df_not_superhost_frequency"),
                 plotOutput("comments_df_superhost_bing_word_counts_positive"),
                 plotOutput("comments_df_superhost_bing_word_counts_negative"),
                 plotOutput("comments_df_not_superhost_bing_word_counts_positive"), 
                 plotOutput("comments_df_not_superhost_bing_word_counts_negative"))
        
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$superhost <- renderPlot({superhost_plot})
    output$host_since <- renderPlot({host_since_plot})
    output$host_response_time <- renderPlot({host_response_time_plot})
    output$host_response_rate <- renderPlot({host_response_rate_plot})
    output$host_has_profile_pic <- renderPlot({host_has_profile_pic_plot})
    output$host_identity_verified <- renderPlot({host_identity_verified_plot})
    
    output$neighbourhood <- renderPlot({neighbourhood_plot})
    output$room_type <- renderPlot({room_type_plot})
    output$accommodates <- renderPlot({accommodates_plot})
    output$bathrooms <- renderPlot({bathrooms_plot})
    output$bedrooms <- renderPlot({bedrooms_plot})
    output$beds <- renderPlot({beds_plot})
    output$bed_type <- renderPlot({bed_type_plot})
    
    output$price_rating <- renderPlot({price_rating_plot})
    output$security_deposit_rating <- renderPlot({security_deposit_rating_plot})
    output$cleaning_fee <- renderPlot({cleaning_fee_plot})
    output$extra_people <- renderPlot({extra_people_plot})
    
    output$minimum_nights <- renderPlot({minimum_nights_plot})
    output$cancellation_policy <- renderPlot({cancellation_policy_plot})
    output$require_guest_profile_picture <- renderPlot({require_guest_profile_picture_plot})
    
    output$number_of_reviews <- renderPlot({number_of_reviews_plot})
    output$review_scores_rating <- renderPlot({review_scores_rating_plot})
    output$review_scores_rating_per <- renderPlot({review_scores_rating_per_plot})
    
    output$host_is_superhost_map <- renderPlot({host_is_superhost_map})
    
    output$mymap1 <- renderLeaflet({
        leaflet(data = listing) %>%
            setView(-71.10000, 42.32000, zoom = 11) %>% 
            addProviderTiles("CartoDB.Positron", group = "Map") %>%
            addProviderTiles("Esri.WorldImagery", group = "Satellite")%>% 
            addProviderTiles("Esri.WorldShadedRelief", group = "Relief")%>%
            addMarkers(~longitude, ~latitude, label = ~name, group = "Sites") %>%
            addScaleBar(position = "bottomleft") %>%
            addLayersControl(
                baseGroups = c("Map", "Satellite", "Relief"),
                overlayGroups = c("Sites"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    output$information1 <- renderText("By December 4, 2019, there are over 3000 Airbnb listings in Boston.")
    
    #define the color pallate for superhost
    pal1 <- colorFactor(
        palette = c('#ED6664', '#388288'),
        domain = listing$host_is_superhost)
    
    output$mymap <- renderLeaflet({
        leaflet(data = listing) %>%
            setView(-71.10000, 42.32000, zoom = 11) %>% 
            addProviderTiles("CartoDB.Positron", group = "Map") %>%
            addProviderTiles("Esri.WorldImagery", group = "Satellite")%>% 
            addProviderTiles("Esri.WorldShadedRelief", group = "Relief")%>%
            addMarkers(~longitude, ~latitude, label = ~name, group = "Sites") %>%
            addScaleBar(position = "bottomleft") %>%
            addLayersControl(
                baseGroups = c("Map", "Satellite", "Relief"),
                overlayGroups = c("Sites"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    output$information <- renderText("By clicking the 'Is Superhost?' Bottom, you can see the locations of superhosts’ listings and not superhosts’ listings. It is a big surprise that most superhosts’ listings do not locate in the center of Boston and downtown area.")
    
    observe({
        proxy <- leafletProxy("mymap", data = listing)
        proxy %>% clearMarkers()
        if (input$year) {
            proxy %>% addCircleMarkers(stroke = TRUE, color = ~pal1(host_is_superhost), fillOpacity = 0.2,  label = ~as.character(paste0("Is Superhost?", sep = " ", host_is_superhost))) %>%
                addLegend("bottomright", pal = pal1, values = listing$host_is_superhost,
                          title = "Is Superhost?",
                          opacity = 1)}
        else {
            proxy %>% clearMarkers() %>% clearControls()
        }
    })
    
    output$name_df_superhost_frequency <- renderPlot({name_df_superhost_frequency})
    output$name_df_not_superhost_frequency <- renderPlot({name_df_not_superhost_frequency})
    output$name_df_joy_superhost <- renderPlot({name_df_joy_superhost})
    output$name_df_joy_not_superhost <- renderPlot({name_df_joy_not_superhost})
    
    output$description_df_superhost_frequency <- renderPlot({description_df_superhost_frequency})
    output$description_df_not_superhost_frequency <- renderPlot({description_df_not_superhost_frequency})
    output$description_df_joy_superhost <- renderPlot({description_df_joy_superhost})
    output$description_df_joy_not_superhost <- renderPlot({description_df_joy_not_superhost})
    
    output$comments_df_superhost_frequency <- renderPlot({comments_df_superhost_frequency})
    output$comments_df_not_superhost_frequency <- renderPlot({comments_df_not_superhost_frequency})
    output$comments_df_superhost_bing_word_counts_positive <- renderPlot({comments_df_superhost_bing_word_counts_positive})
    output$comments_df_superhost_bing_word_counts_negative <- renderPlot({comments_df_superhost_bing_word_counts_negative})
    output$comments_df_not_superhost_bing_word_counts_positive <- renderPlot({comments_df_not_superhost_bing_word_counts_positive})
    output$comments_df_not_superhost_bing_word_counts_negative <- renderPlot({comments_df_not_superhost_bing_word_counts_negative})
    
    output$plot1 <- renderText("Figures in part 1 display EDA of host information:   
Figures 1 shows the distribution of superhost. We can see the number of superhost is around one-third of the number of not superhost. Therefore, we can say that being a superhost is not an easy thing.   
Figures 2 shows the distribution of host begin year. We can see there is a peak in 2014. It means in 2014, suddenly a large amount of people started to rent their house and became a new Airbnb host in Boston.  
Figures 3 shows the distribution of host response time. We can see most superhost reply in within an hour. 
Figures 4 shows the distribution of host response rate. We can see most superhost reply 100%.")
    output$plot2 <- renderText("Figures in part 2 display EDA of listing information:   
Figures 7 shows the distribution of neighbourhood. We can see Brighton and Dorchester have relatively higher superhost rate.  
Figures 8-12 are mosaic plots showing the proportion different attributes.  
Figures 13 shows the distribution of bed type. Most beds are real bed.")
    output$plot3 <- renderText("Figures in part 3 display EDA of price information:   
These figures shows the price of different attributes arranged by average rating.
We can see the superhost points rest on the right bottom corner of the figures. It means the superhosts’ listings are cheap and high rating.")
    output$plot4 <- renderText("Figures in part 4 display EDA of rules information:   
These figures shows limitation of booking the listings. For example, from Figure 19, it is clear that higher superhost rate exists in the moderate cancellation policy bar.")
    output$plot5 <- renderText("Figures in part 5 display EDA of reviews information:   
Superhosts have larger number of reviews, higher average rating and larger number of reviews per month.")
    output$text1 <- renderText("Here shows text minging results of name of listings. It seems there is no difference in common words they use in the listings’ names between superhosts and normal hosts. The two figures underneath show sentiment analysis results. We can see for superhosts, the most common joy word they use is sunny. While the most common joy word normal hosts use is beautiful.")
    output$text2 <- renderText("Here shows text minging results of description of listings. Again, it seems there is no difference in common words they use in the listings’ description between superhosts and normal hosts.")
    output$text3 <- renderText("Here shows text minging results of comments. Again, it seems there is no difference in common words they use in comments between superhosts’ listings and normal hosts’ listings. The four figures underneath show sentiment analysis results. We can see for superhosts’ listings, positive words occur more often while negative words seldom appear in the comments comparing to normal hosts’ listings.")
    
}

# Run the application 
shinyApp(ui = ui, server = server)
