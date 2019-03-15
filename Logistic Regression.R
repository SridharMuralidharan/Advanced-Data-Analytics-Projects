songs <- read.csv("D:\\MSBA\\Spring 2019\\MSBX5415\\songs.csv")

songs.1 <- songs[songs$year == 2010,]

songs.2 <- songs[songs$artistname == "Michael Jackson",]

songs.3 <- songs.2[songs.2$Top10==1,]

unique(songs.3$songtitle)

unique(songs$timesignature)

table(songs$timesignature)

max(songs$tempo)

songs.4 <- songs[songs$tempo == 244.307,]
songs.4$songtitle

SongsTrain<- songs[songs$year <= 2009,]
SongsTest <- songs[songs$year > 2009,]

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

glm.train <- glm(Top10 ~ .,data = SongsTrain, family = binomial)

summary(glm.train)

with(SongsTrain, cor(loudness, energy))
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)

summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

cancel.prob <- predict(SongsLog3, newdata = SongsTest, type = "response")
cancel.pred <- as.numeric(cancel.prob > 0.45)

table(SongsTest$Top10, cancel.pred)
