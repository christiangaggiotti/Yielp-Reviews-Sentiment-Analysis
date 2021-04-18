#data collection and cleaning===================================================

library(jsonlite)
library(data.table)
library(stringr)
library(igraph) 
library(ggraph) 
library(tidyverse)
library(lubridate)
library(DT)
library(ggplot2)
library(gridExtra)
library(leaflet)


#import the dataset business and convert it into datatable

Business = stream_in(file("yelp_academic_dataset_business.json"))
setDT(Business)

#delete all the unnecessary columns

business_11columns=subset(Business, select=c(business_id, name, address, city, state, postal_code, latitude, longitude, stars, review_count, categories))
head(business_11columns)

#create a list of all the words contained in the column categories

categories = str_split(business_11columns$categories,",")
categories = as.data.frame(unlist(categories))
colnames(categories) = c("Name")

head(categories)

#group and plot the top 20 categories

categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  head(20) %>%
  
  
  ggplot(aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white",fill="steelblue4") +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of Category', y = 'Count', 
       title = 'Top 20 Categories of Business') +
  coord_flip() + 
  theme_bw()

#remove all the categories different from restaurants

business_rest=business_11columns[business_11columns$categories %like% "Restaurants",]

#remove the word restaurants from the categories

categories=data.frame(lapply(categories,function(x){gsub("Restaurants","",x)}))
business_semifinal=data.frame(lapply(business_rest,function(x){gsub("Restaurants, ","",x)}))
business_final=data.frame(lapply(business_semifinal,function(x){gsub(", Restaurants","",x)}))

#remove all the spaces and substitute them with underscores

new_nospaces=lapply(business_final$categories,function(x){gsub(", ",",",x)})
new_nospaces=lapply(new_nospaces,function(x){gsub(" ","_",x)}) 
business_final$categories=new_nospaces

#remove the word food from the categories column

business_superfinal=data.frame(lapply(business_final,function(x){gsub("Food,","",x)}))
business_superfinal=data.frame(lapply(business_superfinal,function(x){gsub(",Food","",x)}))

#create a new dataframe with all the cleaned categories

categories_newversion = str_split(business_superfinal$categories,",")
categories_newversion = as.data.frame(unlist(categories_newversion))
colnames(categories_newversion) = c("Name1")

#list and plot the new cleaned categories

categories_newversion %>%
  group_by(Name1) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name1,Count)) %>%
  head(20)  %>%
  
  ggplot(aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white",fill="steelblue4") +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of Category', y = 'Count', 
       title = 'Top 20 different types of restaurants') +
  coord_flip() + 
  theme_bw()


#plot an istogram with the 10 most common cities

business_superfinal %>%
  group_by(city) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(City = reorder(city,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = City,y = Count)) +
  geom_bar(stat='identity',colour="white",fill="steelblue4") +
  geom_text(aes(x = City, y = 1, label = paste0(round(Count),sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'City', y = 'Count of Reviews', 
       title = 'Top ten Cities with the most restaurants') +
  coord_flip() + 
  theme_bw()


#plot an istogram with the 10 most common states

business_superfinal %>%
  group_by(state) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(State = reorder(state,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = State,y = Count)) +
  geom_bar(stat='identity',colour="white",fill="steelblue4") +
  geom_text(aes(x = State, y = 1, label = paste0(round(Count),sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'State', y = 'Count of Reviews', 
       title = 'Top ten States with the most restaurants') +
  coord_flip() + 
  theme_bw()


#plot of the distribution of the stars

business_superfinal %>% ggplot(aes(x=as.factor(stars))) + 
  geom_bar(fill="steelblue4") +
  labs(x = 'Stars', y = 'Count of Stars', 
       title = 'Distributions of the ratings')


#create a dataset with just vegas restaurants

business_vegas=business_superfinal[business_superfinal$city == "Las Vegas",]


#list the categories of restaurants in vegas

categories_vegas = str_split(business_vegas$categories,",")
categories_vegas = as.data.frame(unlist(categories_vegas))
colnames(categories_vegas) = c("Name2")

#plot the vegas categories

categories_vegas %>%
  group_by(Name2) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name2,Count)) %>%
  head(20)  %>%
  
  ggplot(aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white",fill="steelblue4") +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of Category', y = 'Count', 
       title = 'Different types of restaurants in Las Vegas') +
  coord_flip() + 
  theme_bw()


#pick 3 different categories in vegas: Chinese, Italian and Fast Food

#clean the categories columns and delete the words different from the selected categories

business_chinese=business_vegas[business_vegas$categories %like% "Chinese",]
business_chinese=data.frame(lapply(business_chinese,function(x){gsub("Chinese_","",x)}))
business_chinese=data.frame(lapply(business_chinese,function(x){gsub("_Chinese","",x)}))
business_chinese=business_chinese[business_chinese$categories %like% "Chinese",]


business_italian=business_vegas[business_vegas$categories %like% "Italian",]
business_italian=data.frame(lapply(business_italian,function(x){gsub("Italian_","",x)}))
business_italian=data.frame(lapply(business_italian,function(x){gsub("_Italian","",x)}))
business_italian=business_italian[business_italian$categories %like% "Italian",]

business_ffood=business_vegas[business_vegas$categories %like% "Fast_Food",]
business_ffood=data.frame(lapply(business_ffood,function(x){gsub("Fast_Food_","",x)}))
business_ffood=data.frame(lapply(business_ffood,function(x){gsub("_Fast_Food","",x)}))
business_ffood=business_ffood[business_ffood$categories %like% "Fast_Food",]

#substitute the useless categories with only a single word of the selected ones

business_chinese$categories=strrep("Chinese", 1)
business_italian$categories=strrep("Italian", 1)
business_ffood$categories=strrep("Fast_Food", 1)

#aggregate the datasets to remove duplicates with 2 or more categories 

business_3types=rbind(business_italian,business_chinese,business_ffood)
business_3types_noduplicates=business_3types[!duplicated(business_3types$business_id),]

#split again into the 3 datasets 

Business_chinese_final=business_3types_noduplicates[business_3types_noduplicates$categories == "Chinese",]
Business_italian_final=business_3types_noduplicates[business_3types_noduplicates$categories == "Italian",]
Business_ffood_final=business_3types_noduplicates[business_3types_noduplicates$categories == "Fast_Food",]

#map of the restaurants in vegas 

LasvegasCoords = Business %>% filter(city == "Las Vegas")

center_lon = median(LasvegasCoords$longitude,na.rm = TRUE)
center_lat = median(LasvegasCoords$latitude,na.rm = TRUE)

leaflet(LasvegasCoords) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude,radius = ~sqrt(review_count))  %>%
  setView(lng=center_lon, lat=center_lat,zoom = 13)

#review part and choice of the seingle restaurant for the last analysis
#substitute characters with numerical values in the column review_count

Business_chinese_final$review_count=as.numeric(Business_chinese_final$review_count)
Business_italian_final$review_count=as.numeric(Business_italian_final$review_count)
Business_ffood_final$review_count=as.numeric(Business_ffood_final$review_count)

#order the dataset by most reviewed

Business_ffood_final=Business_ffood_final[order(Business_ffood_final$review_count, decreasing = TRUE),]
Business_italian_final=Business_italian_final[order(Business_italian_final$review_count, decreasing = TRUE),]
Business_chinese_final=Business_chinese_final[order(Business_chinese_final$review_count, decreasing = TRUE),]

#extract the most reviewed restaurant for each category

head(Business_ffood_final[1:3,])
head(Business_italian_final[1:3,])
head(Business_chinese_final[1:3,])

#clean global environment
library(gdata)
gdata::keep(Business_ffood_final,Business_chinese_final,Business_italian_final,Reviews,sure=TRUE)



#Reviews sample preparation=====================================================

#we open the review file by tranches, opening only the columns of interest
Rev_1 <- NULL
stream_in(
  file("yelp_academic_dataset_review.json"),
  pagesize=100000,
  handler=function(x) {
    Rev_1 <<- rbind(Rev_1, x[,c("review_id","text")])})

Rev_2 <- NULL
stream_in(
  file("yelp_academic_dataset_review.json"),
  pagesize=100000,
  handler=function(x) {
    Rev_2 <<- rbind(Rev_2, x[,c("review_id","business_id","stars")])})

#merging the two tranches to get the final dataset

library(dplyr)
Reviews <- left_join(Rev_1, Rev_2, by = "review_id")
rm(Rev_1)
rm(Rev_2)

#chinese
businessid_chinese=Business_chinese_final["business_id"]
Reviews_chinese=left_join(businessid_chinese,Reviews,by="business_id")

#ffood
businessid_ffood=Business_ffood_final["business_id"]
Reviews_ffood=left_join(businessid_ffood,Reviews,by="business_id")

#italian
businessid_italian=Business_italian_final["business_id"]
Reviews_italian=left_join(businessid_italian,Reviews,by="business_id")

rm(Business_chinese_final)
rm(Business_ffood_final)
rm(Business_italian_final)
rm(businessid_chinese)
rm(businessid_ffood)
rm(businessid_italian)

#we add an additional column with the type of restaurant
#we will use it later on in the textual analysis
Reviews_chinese$category=strrep("chinese",1)
Reviews_ffood$category=strrep("fast_food",1)
Reviews_italian$category=strrep("italian",1)

#aggregate the 3 datasets in one single dataset
Reviews_final=rbind(Reviews_chinese,Reviews_ffood,Reviews_italian)

rm(Reviews_chinese)
rm(Reviews_ffood)
rm(Reviews_italian)

#erase not needed column

Reviews_final=subset(Reviews_final,select = -c(review_id))
head(Reviews_final)

#preliminary textual analysis===================================================
library( data.table )
library( readtext )
library( quanteda )
library( topicmodels )
library( ggplot2 )
library( gghighlight )
library( stringr )      
library( textstem )     
library( lexicon )      
library( sentimentr )   
library( ggridges )     
library( lemon )        
library( patchwork )    

#star distribution of each category 
star_restaurants <- Reviews_final %>% ggplot(aes(x=as.factor(stars))) + geom_bar(fill = "steelblue4") + labs( x = "Stars") + ggtitle("Restaurant ratings")
star_restaurants
star_restaurants %+% subset(Reviews_final, category=="chinese") + ggtitle("Chinese ratings")
star_restaurants %+% subset(Reviews_final, category=="italian") + ggtitle("Italian ratings")
star_restaurants %+% subset(Reviews_final, category=="fast_food") + ggtitle("Fast_Food ratings")

#create a corpus
Reviews_corp=corpus(Reviews_final)
summary(Reviews_corp, 5)

#create tokens and remove stopwords
Reviews_tokens = quanteda::tokens( Reviews_corp, 
                                   remove_numbers = TRUE, 
                                   remove_punct = TRUE, 
                                   remove_symbols = TRUE, 
                                   remove_url = TRUE, 
                                   split_hyphens = TRUE,
                                   include_docvars = TRUE,)

Reviews_tokens=tokens_tolower(Reviews_tokens)
Reviews_tokens=tokens_remove(Reviews_tokens,stopwords())

#create dfm 
Reviews_dfm = dfm( Reviews_tokens,
                   tolower = TRUE,
                   stem = TRUE,)

#removing words appearing too many or too little times
Reviews_dfm_trim = dfm_trim( Reviews_dfm,
                             min_docfreq = 0.01,
                             max_docfreq = 0.80,
                             docfreq_type = "prop" ) 

Reviews_dfm_trim=dfm_subset(Reviews_dfm_trim, ntoken(Reviews_dfm_trim)>0)
head( Reviews_dfm_trim, n = 10, nf = 10 )

#we divide the dataset in 2 parts
#Above average rating reviews with 4 or more stars
#below average rating reviews with less than 4 stars
Reviews_above_dfm_subset = dfm_subset( Reviews_dfm_trim, stars >= 4 )
Reviews_below_dfm_subset = dfm_subset( Reviews_dfm_trim, stars < 4 )

ndoc(Reviews_above_dfm_subset)
ndoc(Reviews_below_dfm_subset)

# most frequent words
Reviews_above_dfm_subset_inverse = dfm_tfidf( Reviews_above_dfm_subset, scheme_tf = "prop" )
Reviews_above_dfm_subset_inverse_freq = textstat_frequency( Reviews_above_dfm_subset_inverse, 
                                                            n = 10, 
                                                            groups = "category",
                                                            force = TRUE )

ggplot( data = Reviews_above_dfm_subset_inverse_freq, 
        aes( x = nrow( Reviews_above_dfm_subset_inverse_freq ):1, y = frequency ) ) +
  geom_point() +
  geom_line( color = "steelblue4" ) + 
  facet_wrap(~ group, scales = "free_y" ) +
  coord_flip() +
  scale_x_continuous( breaks = nrow( Reviews_above_dfm_subset_inverse_freq ):1,
                      labels = Reviews_above_dfm_subset_inverse_freq$feature ) +
  labs( x = NULL, y = "Inverse frequency") +
  ggtitle("Word Inverse frequency above average")


Reviews_below_dfm_subset_inverse = dfm_tfidf( Reviews_below_dfm_subset, scheme_tf = "prop" )
Reviews_below_dfm_subset_inverse_freq = textstat_frequency( Reviews_below_dfm_subset_inverse, 
                                                            n = 10, 
                                                            groups = "category",
                                                            force = TRUE )
ggplot( data = Reviews_below_dfm_subset_inverse_freq, 
        aes( x = nrow( Reviews_below_dfm_subset_inverse_freq ):1, y = frequency ) ) +
  geom_point() +
  geom_line( color = "steelblue4" ) + 
  facet_wrap(~ group, scales = "free_y" ) +
  coord_flip() +
  scale_x_continuous( breaks = nrow( Reviews_below_dfm_subset_inverse_freq ):1,
                      labels = Reviews_below_dfm_subset_inverse_freq$feature ) +
  labs( x = NULL, y = "Inverse frequency") +
  ggtitle("Word Inverse frequency below average")


# Keyness analysis 
dfm_categories = dfm( Reviews_dfm_trim, groups = "category")
key_categories_chinese = textstat_keyness( dfm_categories, target = "chinese" )
key_categories_fastfood = textstat_keyness( dfm_categories, target = "fast_food" )
key_categories_italian = textstat_keyness( dfm_categories, target = "italian" )

textplot_keyness( key_categories_chinese, labelsize = 2.5, 
                  color = c( "steelblue4", "darkgray" ) ) +
  ggtitle( "Chinese" )

textplot_keyness( key_categories_fastfood, labelsize = 2.5, 
                  color = c( "steelblue4", "darkgray" ) ) +
  ggtitle( "Fast food" ) + 
  theme_bw()

textplot_keyness( key_categories_italian, labelsize = 2.5, 
                  color = c( "steelblue4", "darkgray" ) ) +
  ggtitle( "Italian" ) + 
  theme_bw()


#Preliminary sentiment analysis=================================================

library(sentimentr)
library( corpus )
library( stringr )
library( ggridges )
library( grid )
library( gridExtra )
library( GGally )
library( quantreg )

#tone evolution-----------------------------------------------------------------

Reviews_final$text=tolower(Reviews_final$text)
Reviews_total_sent=sentiment(get_sentences(Reviews_final))
ggplot(Reviews_total_sent, aes( x = as.factor(stars), y = sentiment ) )+
  geom_boxplot() 

means <- matrix(ncol=5, nrow=100)

for (p in 1:5) {
  
  for(i in 1:100){
    means[i,p] <- mean(Reviews_total_sent[stars==p & sentence_id==i,sentiment])
  }
}

colnames(means)=c("S1","S2","S3","S4","S5")
means=as.data.frame(means)
means[,"sentence_id"]=1:100

ggplot() + 
  geom_line( data = means,
             aes( x = sentence_id, y = S1 , color = "1" ), 
             size = 1 ) +
  geom_line( data = means,
             aes( x = sentence_id, y = S2 , color = "2" ), 
             size = 1 ) +
  geom_line( data = means,
             aes( x = sentence_id, y = S3 , color = "3" ), 
             size = 1 ) +
  geom_line( data = means,
             aes( x = sentence_id, y = S4 , color = "4" ), 
             size = 1 ) +
  geom_line( data = means,
             aes( x = sentence_id, y = S5 , color = "5" ), 
             size = 1 ) +
  labs( x = "sentence_id", y = "sentiment",color="stars")+
  ggtitle( "sentence tone evolution" ) + xlim(0,50) + ylim(-0.20,0.40)+
  scale_fill_discrete(name = "stars")

#regression---------------------------------------------------------------------

Reviews_sent_bydoc = sentiment_by(get_sentences(Reviews_final))
Reviews_sent_bydoc = cbind(Reviews_sent_bydoc,Reviews_final)

words_sent = ggplot(Reviews_sent_bydoc,
                    aes(x=word_count,y=ave_sentiment,color=stars))+
  geom_point() + xlim(0,1000) +
  ggtitle( "average sentiment by review length" ) 
words_sent

words_5 = words_sent %+% subset(Reviews_sent_bydoc, stars %in% 5) +
  theme(legend.position="none")+
  ggtitle( "5 Stars" )
words_1 = words_sent %+% subset(Reviews_sent_bydoc, stars %in% 1) +
  theme(legend.position="none")+
  ggtitle( "1 Star" )
words_sent %+% subset(Reviews_sent_bydoc, stars %in% 2)
words_sent %+% subset(Reviews_sent_bydoc, stars %in% 3)
words_sent %+% subset(Reviews_sent_bydoc, stars %in% 4)

reg_5 = lm(ave_sentiment ~ word_count + I(word_count^0.5), data =  subset(Reviews_sent_bydoc, stars==5))
summary(reg_5)
reg_1 = lm(ave_sentiment ~ word_count + I(word_count^0.5), data =  subset(Reviews_sent_bydoc, stars==1))
summary(reg_1)

fun5 <- function(x) {
  reg_5$coefficients[1] + reg_5$coefficients[2]*x + reg_5$coefficients[3]*x^0.5
}

fun1 <- function(x) {
  reg_1$coefficients[1] + reg_1$coefficients[2]*x + reg_1$coefficients[3]*x^0.5
}

words_5 = words_5 + stat_function(fun = fun5 , color="red" , size=1) 
words_1 = words_1 + stat_function(fun = fun1 , color="red" , size=1)
words_5
words_1

#quantile removal---------------------------------------------------------------

subset1 = subset(Reviews_sent_bydoc, stars==1)
sub = subset1[ave_sentiment > quantile(ave_sentiment, 0.90), ]
sub[1:2,6]

subset1 = subset1[ave_sentiment < quantile(ave_sentiment, 0.95), ]
words_1_cut = ggplot(subset1,aes(x=word_count,y=ave_sentiment))+
  geom_point(color="steelblue4")+xlim(0,1000) +
  ggtitle( "1 Star_cut" )
reg_1_cut = lm(ave_sentiment ~ word_count + I(word_count^0.5) , data =  subset1)
summary(reg_1_cut)

subset5 = subset(Reviews_sent_bydoc, stars==5)
subset5 = subset5[ave_sentiment > quantile(ave_sentiment, 0.05), ]
words_5_cut = ggplot(subset5,aes(x=word_count,y=ave_sentiment))+
  geom_point(color="steelblue4") + ggtitle( "5 Star_cut" ) + xlim(0,1000) 
reg_5_cut = lm(ave_sentiment ~ word_count + I(word_count^0.5) , data =  subset5)
summary(reg_5_cut)

fun5_cut <- function(x) {
  reg_5_cut$coefficients[1] + reg_5_cut$coefficients[2]*x + reg_5_cut$coefficients[3]*x^0.5
}

fun1_cut <- function(x) {
  reg_1_cut$coefficients[1] + reg_1_cut$coefficients[2]*x + reg_1_cut$coefficients[3]*x^0.5
}

words_5_cut = words_5_cut + stat_function(fun = fun5_cut , color="red" , size=1) 
words_1_cut = words_1_cut + stat_function(fun = fun1_cut , color="red" , size=1)
words_5_cut
words_1_cut


subset2 = subset(Reviews_sent_bydoc, stars==2)
subset2 = subset2[ave_sentiment < quantile(ave_sentiment, 0.95), ]
ggplot(subset2,aes(x=word_count,y=ave_sentiment)) +
  geom_point(color="steelblue4") + ggtitle( "2 Star_cut" )+xlim(0,1000)

subset3 = subset(Reviews_sent_bydoc, stars==3)
subset3 = subset3[ave_sentiment > quantile(ave_sentiment, 0.025) & ave_sentiment < quantile(ave_sentiment, 0.975), ]
ggplot(subset3,aes(x=word_count,y=ave_sentiment))+ 
  geom_point(color="steelblue4") + ggtitle( "3 Star_cut" ) +xlim(0,1000)

subset4 = subset(Reviews_sent_bydoc, stars==4)
subset4 = subset4[ave_sentiment > quantile(ave_sentiment, 0.05), ]
ggplot(subset4,aes(x=word_count,y=ave_sentiment)) +
  geom_point(color="steelblue4") + ggtitle( "4 Star_cut" )+xlim(0,1000)


#sectorial analysis=============================================================

Reviews_final2 = rbind(subset1,subset2,subset3,subset4,subset5)
Reviews_final2 = subset(Reviews_final2, select = c(business_id,text,stars,category))

Reviews_sent_bystar=with(Reviews_final2, 
                         sentiment_by(get_sentences(text),list(stars)))

Reviews_sent_bystar

#Chinese------------------------------------------------------------------------

Reviews_chi=Reviews_final2[category=="chinese",]
Reviews_chi_sent = get_sentences(Reviews_chi)
Reviews_chi_sent=corpus(Reviews_chi_sent)

#total
chi_total <- with(
  Reviews_chi, 
  sentiment_by(
    get_sentences(text),
    list(stars)))

#food
containstarget = stringr::str_detect(texts(Reviews_chi_sent), "grill|chees|steak|spicy|curry|eat|food|noodl|dim|sum|soup|tast|sushi|hot|cold|chicken|rice|asian|ramen|pork|sauce|fish|soy|duck|thai|dish|crab|shrimp")
Reviews_corp_chi_keep = corpus_subset(Reviews_chi_sent, containstarget)
Reviews_corp_chi_keep=convert(Reviews_corp_chi_keep,"data.frame")

chi_food <- with(
  Reviews_corp_chi_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#price
containstarget = stringr::str_detect(texts(Reviews_chi_sent), "price|bill|charg|pay|expens|cheap|economic|card|cash")
Reviews_corp_chi_keep = corpus_subset(Reviews_chi_sent, containstarget)
Reviews_corp_chi_keep=convert(Reviews_corp_chi_keep,"data.frame")

chi_price <- with(
  Reviews_corp_chi_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#location
containstarget = stringr::str_detect(texts(Reviews_chi_sent), "plac|locat|area|zone|ambien|environm|music|atmos|neighb|street|dirty|clean|smell|view|hote")
Reviews_corp_chi_keep = corpus_subset(Reviews_chi_sent, containstarget)
Reviews_corp_chi_keep=convert(Reviews_corp_chi_keep,"data.frame")

chi_location <- with(
  Reviews_corp_chi_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#service
containstarget = stringr::str_detect(texts(Reviews_chi_sent), "servic|assist|help|kind|polit|rud|disrespect|unkind|mean|waiter|waitres|staf|chef|welcom")
Reviews_corp_chi_keep = corpus_subset(Reviews_chi_sent, containstarget)
Reviews_corp_chi_keep=convert(Reviews_corp_chi_keep,"data.frame")

chi_service <- with(
  Reviews_corp_chi_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#timing
containstarget = stringr::str_detect(texts(Reviews_chi_sent), "tim|wait|expect|speed|quick|punctual|delay|lat|slow")
Reviews_corp_chi_keep = corpus_subset(Reviews_chi_sent, containstarget)
Reviews_corp_chi_keep=convert(Reviews_corp_chi_keep,"data.frame")

chi_time <- with(
  Reviews_corp_chi_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#vegan
containstarget = stringr::str_detect(texts(Reviews_chi_sent), "veg|bio|fruit|healt")
Reviews_corp_chi_keep = corpus_subset(Reviews_chi_sent, containstarget)
Reviews_corp_chi_keep=convert(Reviews_corp_chi_keep,"data.frame")

chi_veg <- with(
  Reviews_corp_chi_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#delivery
containstarget = stringr::str_detect(texts(Reviews_chi_sent), "deliver|drive-in|driv|transp|dist|pack|home")
Reviews_corp_chi_keep = corpus_subset(Reviews_chi_sent, containstarget)
Reviews_corp_chi_keep=convert(Reviews_corp_chi_keep,"data.frame")

chi_del <- with(
  Reviews_corp_chi_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#drink
containstarget = stringr::str_detect(texts(Reviews_chi_sent), "drin|wine|beer|water|cup|glass|bever|vin|bar|liquor|alco|alch|sak|cok|fant|peps|tea|drew|refill")
Reviews_corp_chi_keep = corpus_subset(Reviews_chi_sent, containstarget)
Reviews_corp_chi_keep=convert(Reviews_corp_chi_keep,"data.frame")

chi_dri <- with(
  Reviews_corp_chi_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))


#Italian------------------------------------------------------------------------

Reviews_ita=Reviews_final2[category=="italian",]
Reviews_ita_sent = get_sentences(Reviews_ita)
Reviews_ita_sent=corpus(Reviews_ita_sent)

#total
ita_total <- with(
  Reviews_ita, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#food
containstarget = stringr::str_detect(texts(Reviews_ita_sent), "food|chees|tast|pizza|pasta|spaghet|polpet|meat|spicy|eat|soup|hot|cold|chicken|rice|pork|sauce|fish|duck|dish|crab|shrimp|risotto|carbonar|bologn|ragù|ragu|lasagn|steak|grill")
Reviews_corp_ita_keep = corpus_subset(Reviews_ita_sent, containstarget)
Reviews_corp_ita_keep=convert(Reviews_corp_ita_keep,"data.frame")

ita_food <- with(
  Reviews_corp_ita_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#price
containstarget = stringr::str_detect(texts(Reviews_ita_sent), "price|bill|charg|pay|expens|cheap|economic|card|cashprice|bill|charg|pay|expens|cheap|economic|card|cash")
Reviews_corp_ita_keep = corpus_subset(Reviews_ita_sent, containstarget)
Reviews_corp_ita_keep=convert(Reviews_corp_ita_keep,"data.frame")

ita_price <- with(
  Reviews_corp_ita_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))


#location
containstarget = stringr::str_detect(texts(Reviews_ita_sent), "plac|locat|area|zone|ambien|environm|music|atmos|neighb|street|dirty|clean|smell|view|hote")
Reviews_corp_ita_keep = corpus_subset(Reviews_ita_sent, containstarget)
Reviews_corp_ita_keep=convert(Reviews_corp_ita_keep,"data.frame")

ita_location <- with(
  Reviews_corp_ita_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#service
containstarget = stringr::str_detect(texts(Reviews_ita_sent), "servic|assist|help|kind|polit|rud|disrespect|unkind|mean|waiter|waitres|staf|chef|welcom")
Reviews_corp_ita_keep = corpus_subset(Reviews_ita_sent, containstarget)
Reviews_corp_ita_keep=convert(Reviews_corp_ita_keep,"data.frame")

ita_service <- with(
  Reviews_corp_ita_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#timing
containstarget = stringr::str_detect(texts(Reviews_ita_sent), "tim|wait|expect|speed|quick|punctual|delay|lat|slow")
Reviews_corp_ita_keep = corpus_subset(Reviews_ita_sent, containstarget)
Reviews_corp_ita_keep=convert(Reviews_corp_ita_keep,"data.frame")

ita_time <- with(
  Reviews_corp_ita_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#vegan
containstarget = stringr::str_detect(texts(Reviews_ita_sent), "veg|bio|fruit|healt")
Reviews_corp_ita_keep = corpus_subset(Reviews_ita_sent, containstarget)
Reviews_corp_ita_keep=convert(Reviews_corp_ita_keep,"data.frame")

ita_veg <- with(
  Reviews_corp_ita_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#delivery
containstarget = stringr::str_detect(texts(Reviews_ita_sent), "deliver|drive-in|driv|transp|dist|pack|hom")
Reviews_corp_ita_keep = corpus_subset(Reviews_ita_sent, containstarget)
Reviews_corp_ita_keep=convert(Reviews_corp_ita_keep,"data.frame")

ita_del <- with(
  Reviews_corp_ita_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#drink
containstarget = stringr::str_detect(texts(Reviews_ita_sent), "drin|wine|beer|water|cup|glass|bever|vin|bar|liquor|alco|alch|sak|cok|fant|peps|tea|drew|refill")
Reviews_corp_ita_keep = corpus_subset(Reviews_ita_sent, containstarget)
Reviews_corp_ita_keep=convert(Reviews_corp_ita_keep,"data.frame")

ita_dri <- with(
  Reviews_corp_ita_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#fast food----------------------------------------------------------------------

Reviews_ff=Reviews_final2[category=="fast_food",]
Reviews_ff_sent = get_sentences(Reviews_ff)
Reviews_ff_sent=corpus(Reviews_ff_sent)

#total
ff_total <- with(
  Reviews_ff, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#food
containstarget = stringr::str_detect(texts(Reviews_ff_sent), "food|chees|tast|meat|burger|frie|cheddar|sanwich|pizza|pasta|polpet|meat|spicy|eat|soup|hot|cold|chicken|rice|pork|sauce|fish|duck|dish|crab|shrimp|lasagn|steak|grill|lasagna|bolognes|onion|mushroom")
Reviews_corp_ff_keep = corpus_subset(Reviews_ff_sent, containstarget)
Reviews_corp_ff_keep=convert(Reviews_corp_ff_keep,"data.frame")

ff_food <- with(
  Reviews_corp_ff_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#price
containstarget = stringr::str_detect(texts(Reviews_ff_sent), "price|bill|charg|pay|expens|cheap|economic|card|cashprice|bill|charg|pay|expens|cheap|economic|card|cash")
Reviews_corp_ff_keep = corpus_subset(Reviews_ff_sent, containstarget)
Reviews_corp_ff_keep=convert(Reviews_corp_ff_keep,"data.frame")

ff_price <- with(
  Reviews_corp_ff_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))


#location
containstarget = stringr::str_detect(texts(Reviews_ff_sent), "plac|locat|area|zone|ambien|environm|music|atmos|neighb|street|dirty|clean|smell|view|hote")
Reviews_corp_ff_keep = corpus_subset(Reviews_ff_sent, containstarget)
Reviews_corp_ff_keep=convert(Reviews_corp_ff_keep,"data.frame")

ff_location <- with(
  Reviews_corp_ff_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#service
containstarget = stringr::str_detect(texts(Reviews_ff_sent), "servic|assist|help|kind|polit|rud|disrespect|unkind|mean|waiter|waitres|staf|chef|welcom")
Reviews_corp_ff_keep = corpus_subset(Reviews_ff_sent, containstarget)
Reviews_corp_ff_keep=convert(Reviews_corp_ff_keep,"data.frame")

ff_service <- with(
  Reviews_corp_ff_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#timing
containstarget = stringr::str_detect(texts(Reviews_ff_sent), "tim|wait|expect|speed|quick|punctual|delay|lat|slow")
Reviews_corp_ff_keep = corpus_subset(Reviews_ff_sent, containstarget)
Reviews_corp_ff_keep=convert(Reviews_corp_ff_keep,"data.frame")

ff_time <- with(
  Reviews_corp_ff_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#vegan
containstarget = stringr::str_detect(texts(Reviews_ff_sent), "veg|bio|fruit|healt")
Reviews_corp_ff_keep = corpus_subset(Reviews_ff_sent, containstarget)
Reviews_corp_ff_keep=convert(Reviews_corp_ff_keep,"data.frame")

ff_veg <- with(
  Reviews_corp_ff_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#delivery
containstarget = stringr::str_detect(texts(Reviews_ff_sent), "deliver|drive-in|driv|transp|dist|pack|hom")
Reviews_corp_ff_keep = corpus_subset(Reviews_ff_sent, containstarget)
Reviews_corp_ff_keep=convert(Reviews_corp_ff_keep,"data.frame")

ff_del <- with(
  Reviews_corp_ff_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#drink
containstarget = stringr::str_detect(texts(Reviews_ff_sent), "drin|wine|beer|water|cup|glass|bever|vin|bar|liquor|alco|alch|sak|cok|fant|peps|tea|drew|refill")
Reviews_corp_ff_keep = corpus_subset(Reviews_ff_sent, containstarget)
Reviews_corp_ff_keep=convert(Reviews_corp_ff_keep,"data.frame")

ff_dri <- with(
  Reviews_corp_ff_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))


#outputs------------------------------------------------------------------------

chi_table = cbind(chi_total$stars,chi_total$ave_sentiment,chi_food$ave_sentiment,
                  chi_location$ave_sentiment,chi_price$ave_sentiment,chi_service$ave_sentiment,
                  chi_time$ave_sentiment,chi_veg$ave_sentiment,chi_del$ave_sentiment,
                  chi_dri$ave_sentiment)
colnames(chi_table) = c("stars","total","food","location","price","service","timing",
                        "vegan","delivery","drink")
chi_table


ita_table = cbind(ita_total$stars,ita_total$ave_sentiment,ita_food$ave_sentiment,
                  ita_location$ave_sentiment,ita_price$ave_sentiment,ita_service$ave_sentiment,
                  ita_time$ave_sentiment,ita_veg$ave_sentiment,ita_del$ave_sentiment,
                  ita_dri$ave_sentiment)
colnames(ita_table) = c("stars","total","food","location","price","service","timing",
                        "vegan","delivery","drink")
ita_table


ff_table = cbind(ff_total$stars,ff_total$ave_sentiment,ff_food$ave_sentiment,
                 ff_location$ave_sentiment,ff_price$ave_sentiment,ff_service$ave_sentiment,
                 ff_time$ave_sentiment,ff_veg$ave_sentiment,ff_del$ave_sentiment,
                 ff_dri$ave_sentiment)
colnames(ff_table) = c("stars","total","food","location","price","service","timing",
                       "vegan","delivery","drink")
ff_table


#Single restaurant analysis=====================================================

X="6Q7-wkCPc1KF75jZLOTcMw"
Reviews_rest = Reviews_final2[Reviews_final2$business_id==X,]
Business2 = Business[Business$business_id==X,]
head(Business2)

Reviews_rest=subset(Reviews_rest, select = -c(business_id,category))

ggplot(data=Reviews_rest , aes(stars))+
  geom_histogram(fill = "steelblue4",binwidth = 0.5)+
  ggtitle("stars distribution")


#Keyness-------------------------------------------------------------------------

Reviews_rest_corp = corpus(Reviews_rest)
Reviews_rest_tokens = quanteda::tokens( Reviews_rest_corp, 
                                        remove_numbers = TRUE, 
                                        remove_punct = TRUE, 
                                        remove_symbols = TRUE, 
                                        remove_url = TRUE, 
                                        split_hyphens = TRUE,
                                        include_docvars = TRUE,)

Reviews_rest_tokens=tokens_remove(Reviews_rest_tokens,stopwords())
Reviews_rest_dfm = dfm( Reviews_rest_tokens,
                        stem = TRUE)

Reviews_rest_dfm = dfm_trim( Reviews_rest_dfm,
                             min_docfreq = 0.01,
                             max_docfreq = 0.80,
                             docfreq_type = "prop" ) 


Reviews_rest_dfm = dfm_subset(Reviews_rest_dfm, ntoken(Reviews_rest_dfm)>0)
Reviews_rest_dfm = dfm_subset(Reviews_rest_dfm, stars==5 | stars==1)

dfm_1_vs_5 = dfm( Reviews_rest_dfm, groups = "stars")
key_1_vs_5 = textstat_keyness( dfm_1_vs_5, target = "1" )

textplot_keyness( key_1_vs_5, labelsize = 2.5, 
                  color = c( "steelblue4", "darkgray" ) ) +
  ggtitle( "Keyness 1 Star vs 5 Stars" ) + 
  theme_gray()

#Bigrams cloud------------------------------------------------------------------

library(tidytext)
library(lubridate)
library(wordcloud)
library(tm) 
library(SnowballC) 
library(textcat)

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()}

bigrams_rest <- Reviews_rest %>%
  count_bigrams()

head(bigrams_rest)

bigrams_rest %>%
  filter(n > 20) %>%
  visualize_bigrams()

#sentiment analysis-------------------------------------------------------------
Reviews_rest_sent = get_sentences(Reviews_rest)
Reviews_rest_sent = corpus(Reviews_rest_sent)

#total
total <- with(
  Reviews_rest, 
  sentiment_by(
    get_sentences(text),
    list(stars)))

#food
containstarget = stringr::str_detect(texts(Reviews_rest_sent), "food|chees|tast|pizza|pasta|spaghet|polpet|meat|spicy|eat|soup|hot|cold|chicken|rice|pork|sauce|fish|duck|dish|crab|
                                     shrimp|risotto|carbonar|bologn|ragù|ragu|lasagn|steak|grill|breakfast|buffe|dinner|lunch|mexican|nacho|taco|burrito|fajita|guacamole|bean|chilli|chili|
                                     empanada|chorizo|texmex|pinchos|paella|polpett|stake|filet|ribeye|sirloin")
Reviews_corp_rest_keep = corpus_subset(Reviews_rest_sent, containstarget)
Reviews_corp_rest_keep=convert(Reviews_corp_rest_keep,"data.frame")

food <- with(
  Reviews_corp_rest_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#price
containstarget = stringr::str_detect(texts(Reviews_rest_sent), "price|bill|charg|pay|expens|cheap|economic|card|cash|fee|coupon|credit")
Reviews_corp_rest_keep = corpus_subset(Reviews_rest_sent, containstarget)
Reviews_corp_rest_keep=convert(Reviews_corp_rest_keep,"data.frame")

price <- with(
  Reviews_corp_rest_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))


#location
containstarget = stringr::str_detect(texts(Reviews_rest_sent), "building|plac|locat|area|zone|ambien|environm|music|atmos|neighb|street|view|hote")
Reviews_corp_rest_keep = corpus_subset(Reviews_rest_sent, containstarget)
Reviews_corp_rest_keep=convert(Reviews_corp_rest_keep,"data.frame")

location <- with(
  Reviews_corp_rest_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#rooms
containstarget = stringr::str_detect(texts(Reviews_rest_sent), "dirty|clean|smell|bed|room|bug|fridge|tv|air|conditioning|shower|door|window|screen|
                                     soiled|filth|mud|dust|mucky|stink|wash|polish|unstained|carpet|spac|balcon|terrace")
Reviews_corp_rest_keep = corpus_subset(Reviews_rest_sent, containstarget)
Reviews_corp_rest_keep=convert(Reviews_corp_rest_keep,"data.frame")

rooms <- with(
  Reviews_corp_rest_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#service
containstarget = stringr::str_detect(texts(Reviews_rest_sent), "bell|servic|assist|help|kind|polit|rud|disrespect|unkind|mean|waiter|waitres|staf|chef|welcom|customer|front|desk")
Reviews_corp_rest_keep = corpus_subset(Reviews_rest_sent, containstarget)
Reviews_corp_rest_keep=convert(Reviews_corp_rest_keep,"data.frame")

service <- with(
  Reviews_corp_rest_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#timing
containstarget = stringr::str_detect(texts(Reviews_rest_sent), "tim|wait|expect|speed|quick|punctual|delay|lat|slow|minut|hour")
Reviews_corp_rest_keep = corpus_subset(Reviews_rest_sent, containstarget)
Reviews_corp_rest_keep=convert(Reviews_corp_rest_keep,"data.frame")

time <- with(
  Reviews_corp_rest_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#drink
containstarget = stringr::str_detect(texts(Reviews_rest_sent), "drin|wine|beer|water|cup|glass|bever|vin|bar|liquor|alco|alch|sak|cok|fant|peps|tea|drew|refill|bar|cocktail")
Reviews_corp_rest_keep = corpus_subset(Reviews_rest_sent, containstarget)
Reviews_corp_rest_keep=convert(Reviews_corp_rest_keep,"data.frame")

dri <- with(
  Reviews_corp_rest_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#parking
containstarget = stringr::str_detect(texts(Reviews_rest_sent), "parking|car|bycicl|garag|motor|lodge")
Reviews_corp_rest_keep = corpus_subset(Reviews_rest_sent, containstarget)
Reviews_corp_rest_keep=convert(Reviews_corp_rest_keep,"data.frame")

parking <- with(
  Reviews_corp_rest_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#entertainment
containstarget = stringr::str_detect(texts(Reviews_rest_sent), "adventur|dome|park|amusement|arcade|game|carnival|island|treasure|coaster
                                     roller|casino|slot|machine|gamble|strip|fun|win|tower|play|skyrise|cards|roulett")
Reviews_corp_rest_keep = corpus_subset(Reviews_rest_sent, containstarget)
Reviews_corp_rest_keep=convert(Reviews_corp_rest_keep,"data.frame")

entertainment <- with(
  Reviews_corp_rest_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

#security
containstarget = stringr::str_detect(texts(Reviews_rest_sent), "security|guard|steal|rob|crim|safe|lock")
Reviews_corp_rest_keep = corpus_subset(Reviews_rest_sent, containstarget)
Reviews_corp_rest_keep=convert(Reviews_corp_rest_keep,"data.frame")

security <- with(
  Reviews_corp_rest_keep, 
  sentiment_by(
    get_sentences(text), 
    list(stars)))

table = cbind(total$stars,total$ave_sentiment,food$ave_sentiment,price$ave_sentiment,
              location$ave_sentiment,rooms$ave_sentiment,service$ave_sentiment,
              time$ave_sentiment, dri$ave_sentiment, parking$ave_sentiment,entertainment$ave_sentiment,
              security$ave_sentiment)

colnames(table) = c("stars","total","food","price","location","rooms","service","timing",
                    "drink","parking","entertainment","security")
table

##end of script

