library(ROAuth)
library(streamR)
library(twitteR)
library(ggplot2)
library(stringr)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "*****" # From dev.twitter.com create account and get consumer Key
consumerSecret <- "******" #Consumer Secret Key can be generated 

#Connection is made
my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

### STOP HERE!!! ###

save(my_oauth, file = "my_oauth.Rdata")
filterStream(file.name = "tweets3.json", # Save tweets in a json file
             track = c("sport","football","movie","NBA","Basketball","Pokemon Go","cartoon","TV" ),
             language = "en",
             location = c(-124, 44, -66, 57), # latitude/longitude pairs 
             timeout = 18, # Keep connection alive for 1800 seconds
             oauth = my_oauth) # Use my_oauth file as the OAuth credentials

tweets1.df <- parseTweets("tweets3.json", simplify = FALSE) #Tweets stored in a Data frame

tweets_tab <- tweets1.df
location <- c(tweets_tab$location)

# Search for states from the location column of the tweets_tab table
locpatNY <- c("NY")
locpatNewY <- c("New York")
locpatCA <- c("CA")
locpatCali <- c("California")
locpatOH <- c("OH")
locpatOhio <- c("Ohio")
locpatTX <-c('TX')
locpatTexas <- c("Texas")

#pattern matching
NY <- grepl(locpatNY,location)
NewYork <- grepl(locpatNewY,location)
CA <- grepl(locpatCA,location)
California <- grepl(locpatCali,location)
OH <- grepl(locpatOH,location)
Ohio <- grepl(locpatOhio,location)
TX <- grepl(locpatTX,location)
Texas <- grepl(locpatTexas,location)

Preference<- c(tweets_tab$text)
TV <- grepl("tv",Preference)
Movie <- grepl("movie",Preference)

#Number of entertainment tweets are generated for each state
entertainmentTX <- length(which(TV=="TRUE" & (TX=="TRUE" | Texas=="TRUE"))) + length(which(Movie=="TRUE" & (TX=="TRUE" | Texas=="TRUE")) )
entertainmentTX
entertainmentCA <- length(which(TV=="TRUE" & (CA=="TRUE" | California=="TRUE"))) + length(which(Movie=="TRUE" & (CA=="TRUE" | California=="TRUE")) )
entertainmentCA
entertainmentNY <- length(which(TV=="TRUE" & (NY=="TRUE" | NewYork=="TRUE"))) + length(which(Movie=="TRUE" & (NY=="TRUE" | NewYork=="TRUE")) )
entertainmentNY
entertainmentOH <- length(which(TV=="TRUE" & (OH=="TRUE" | Ohio=="TRUE"))) + length(which(Movie=="TRUE" & (OH=="TRUE" | Ohio=="TRUE")) )
entertainmentOH

#Filtered entertainment data is stored in Enter.Data Table
State <- c("Texas","California","New York","Ohio")
EnterFactors <- c(entertainmentTX,entertainmentCA,entertainmentNY,entertainmentOH)
Enter.Data <- data.frame(State,EnterFactors)

#Entertainment Plot
State<- Enter.Data$State
ggplot(Enter.Data,aes(Enter.Data$State,Enter.Data$EnterFactors,fill=State))+
    geom_bar(stat = "identity")+labs(x="State",y="Entertainment Tweets")+ ggtitle("No. of Tweets related to Entertainment")

#pattern matching
Sport <- grepl("sport",Preference)
Football <- grepl("football",Preference)
NBA <- grepl("NBA",Preference)
Basketball <- grepl("Basketball",Preference)

#Number of sports tweets are generated for each state
SportTX <- length(which(Sport=="TRUE" & (TX=="TRUE" | Texas=="TRUE"))) + length(which(Football=="TRUE" & (TX=="TRUE" | Texas=="TRUE")) )+length(which(NBA=="TRUE" & (TX=="TRUE" | Texas=="TRUE")) )+length(which(Basketball=="TRUE" & (TX=="TRUE" | Texas=="TRUE")) )
SportTX
SportCA <- length(which(Sport=="TRUE" & (CA=="TRUE" | California=="TRUE"))) + length(which(Football=="TRUE" & (CA=="TRUE" | California=="TRUE")) )+length(which(NBA=="TRUE" & (CA=="TRUE" | California=="TRUE")) )+length(which(Basketball=="TRUE" & (CA=="TRUE" | California=="TRUE")) )
SportCA
SportNY <- length(which(Sport=="TRUE" & (NY=="TRUE" | NewYork=="TRUE"))) + length(which(Football=="TRUE" & (NY=="TRUE" | NewYork=="TRUE")) )+length(which(NBA=="TRUE" & (NY=="TRUE" | NewYork=="TRUE")) )+length(which(Basketball=="TRUE" & (NY=="TRUE" | NewYork=="TRUE")) )
SportNY
SportOH <- length(which(Sport=="TRUE" & (OH=="TRUE" | Ohio=="TRUE"))) + length(which(Football=="TRUE" & (OH=="TRUE" | Ohio=="TRUE")) )+length(which(NBA=="TRUE" & (OH=="TRUE" | Ohio=="TRUE")) )+length(which(Basketball=="TRUE" & (OH=="TRUE" | Ohio=="TRUE")) )
SportOH

#Filtered entertainment data is stored in Sport.Data Table
State <- c("Texas","California","New York","Ohio")
SportFactors <- c(SportTX,SportCA,SportNY,SportOH)
Sport.Data <- data.frame(State,SportFactors)


#Sport Plot
State<- Sport.Data$State
ggplot(Sport.Data,aes(Sport.Data$State,Sport.Data$SportFactors,fill=State))+
    geom_bar(stat = "identity")+labs(x="State",y="Sport Tweets")+ ggtitle("No. of Tweets related to Sports")
