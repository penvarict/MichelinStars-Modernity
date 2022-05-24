library("gplots")



#obtain data and drop cols. 
michelin_my_maps <- read.csv("michelin_my_maps.csv")
michelin_my_maps$Address <- NULL
michelin_my_maps$WebsiteUrl <- NULL
michelin_my_maps$Url <- NULL
michelin_my_maps$PhoneNumber <- NULL  


# search string for modern cuisines.
modern_search_str = "Modern|Contemporary|Creative"


# extract_from will extract the subset of a cuisine frame, that matches 
# the search string. 
extract_from <- function(cuisine_frame, variation_search_str, exclude = FALSE){
  
  cuisine_frame$Award = gsub('B', '0',cuisine_frame$Award)
  
  cuisine_frame$award_int = substr(cuisine_frame$Award,1,1)
  cuisine_frame$award_int = as.numeric(as.character(cuisine_frame$award_int))
  if (!exclude){
    return(cuisine_frame[grepl(variation_search_str, cuisine_frame$Cuisine), ])
  }
  else {
    return(cuisine_frame[!grepl(variation_search_str, cuisine_frame$Cuisine), ])
  }
}


# Find data for French cuisine block, init as french dataframe.
french=michelin_my_maps[grepl("French",michelin_my_maps$Cuisine),]
#extract high/low
french_modern = extract_from(french, modern_search_str)
french_not_modern = extract_from(french, modern_search_str, exclude = TRUE)

french_modern$National <- "French"
french_modern$Modernity <- "Modern"
french_not_modern$National <- "French"
french_not_modern$Modernity <- "Classic"

# Find data for Japanese Cuisine.
japanese = michelin_my_maps[grepl("Japanese",michelin_my_maps$Cuisine),]
# extract high/low
japanese_modern = extract_from(japanese,modern_search_str)
japanese_not_modern = extract_from(japanese, modern_search_str, exclude =  TRUE)

japanese_modern$National <- "Japanese"
japanese_modern$Modernity <- "Modern"
japanese_not_modern$National <- "Japanese"
japanese_not_modern$Modernity <- "Classic"


# Find data for Italian cuisine
italian = michelin_my_maps[grepl("Italian", michelin_my_maps$Cuisine),]
# extrack high/low 
italian_modern = extract_from(italian, modern_search_str)
italian_not_modern = extract_from(italian, modern_search_str, exclude = TRUE)

italian_modern$National <- "Italian"
italian_modern$Modernity <- "Modern"
italian_not_modern$National <- "Italian"
italian_not_modern$Modernity <- "Classic"



french <- NULL
italian <- NULL
japanese <- NULL 

###

# for each frame, convert all to uniform cuisine names. 
# Example: Modern Japanese can go by "Contemporary Japanese", or "Creative Japanese"
# this does not help us when we need to model by those factors. 

# Modify French modern and not Modern
french_modern$Cuisine <- "Modern French"
french_not_modern$Cuisine <- "Classic French"


# Modify japanese modern and not modern
japanese_modern$Cuisine <- "Modern Japanese"
japanese_not_modern$Cuisine <- "Classic Japanese"

# Modify italian modern and not modern 
italian_modern$Cuisine <- "Modern Italian"
italian_not_modern$Cuisine <- "Classic Italian"


# join tables vertically. 

df_restauraunts <- rbind(french_modern, french_not_modern, japanese_modern, 
                         japanese_not_modern, italian_modern, italian_not_modern)
french_combined <- rbind(french_modern, french_not_modern)
italian_combined <- rbind(italian_modern, italian_not_modern)
japanese_combined <- rbind(japanese_modern, japanese_not_modern)

df_restauraunts$Award <- NULL
# drop unused tables and columns 
michelin_my_maps <- NULL




# Remember to change some columns to factors. 

df_restauraunts$National = as.factor(df_restauraunts$National)
df_restauraunts$Modernity = as.factor(df_restauraunts$Modernity)
df_restauraunts$Currency = as.factor(df_restauraunts$Currency)
# 
# french_combined$National = as.factor(french_combined$National)
# french_combined$Modernity = as.factor(french_combined$Modernity)
# french_combined$Currency = as.factor(french_combined$Currency)
# 
# italian_combined$National = as.factor(italian_combined$National)
# italian_combined$Modernity = as.factor(italian_combined$Modernity)
# italian_combined$Currency = as.factor(italian_combined$Currency)
# 
# japanese_combined$National = as.factor(japanese_combined$National)
# japanese_combined$Modernity = as.factor(japanese_combined$Modernity)
# japanese_combined$Currency = as.factor(japanese_combined$Currency)

 
# Some initial plots 
# hist(italian_combined$award_int, breaks=seq(min(italian_combined$award_int)-0.5, 
#                                   max(italian_combined$award_int)+0.5,
#                                   by=1), freq = FALSE, 
#      xlab = "Michelin Stars (0 for \"Bib Gourmand\" Award) ", 
#      main = "Distribution of Michelin Stars Italian Cuisines")


hist(df_restauraunts$award_int, breaks=seq(min(df_restauraunts$award_int)-0.5, 
                                           max(df_restauraunts$award_int)+0.5,
                                           by=1), freq = FALSE, 
     xlab = "Michelin Stars (0 for \"Bib Gourmand\" Award) ", 
     main = "Distribution of Michelin Stars. (French, Italian, & Japanese Cuisines)")

nat_means = plotmeans(award_int ~ National, data = df_restauraunts, 
                       ylab = "# of Stars", xlab = "Cuisine Nationality",
                       p = 0.0, n.label = TRUE,
                       main = "Mean Number of Michelin Stars By Cuisine Nationality")

modernity_means = plotmeans(award_int ~ Modernity, data = df_restauraunts, 
                            ylab = "# of Stars", xlab = "Cuisine Modernity", 
                            p=0.0, n.label = TRUE,
                            main = "Mean Number of Michelin Stars By Cuisine Nationality")

currency_means = plotmeans(award_int ~ Currency, data = df_restauraunts, 
                            ylab = "# of Stars", xlab = "Currency", p = 0.0,
                           main = "Average Number of Michelin Stars By Currency For French, Japanese, and Italian Cuisines")

two_fac = aov(award_int ~ National + Modernity, data = df_restauraunts )
summary(two_fac)

nk_fac = aov(formula = award_int ~ National*Modernity, data = df_restauraunts )
summary(nk_fac)

block_eff = aov(award_int ~ National, data = df_restauraunts )
summary(block_eff)

hist(resid(two_fac),  freq = FALSE, main = "Histogram of Residuals", 
     xlab= "Awarding Residuals (in Michelin Stars)")

