#Getting Libraries
library("tidyverse")
library("rvest")
library("tuber")

#Using this link as reference
# https://cran.r-project.org/web/packages/tuber/vignettes/tuber-ex.html
# Creating Project on API Console
# Enabling the YouTube Data API V3
# Creating OAuth Credentials and adding users to verify

#Authenticating my Details
yt_oauth()

# Video Details
#videoURL

#Getting all details about a video with id as "videoId"
get_stats(video_id="N708P-A45D0")

#Getting Video Details
get_video_details(video_id="videoId")
1
#Searching all videos with a specific topic
res1 <- yt_search("searchText")
head(res1[, 1:3])

#Getting comments from a specific video comment section
#Any number >100 fetches all results of comments
res2 <- get_comment_threads(c(video_id="videoID"), max_results = 101)
head(res2)

