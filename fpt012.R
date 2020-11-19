library(tidyverse)
fpt_012 = read_csv("fpt for 0-1-2_raw_20_11_04.csv")
fpt_012_n = fpt_012%>%group_by(date,time,subject)%>%summarize(n=n())

####remove tests from RAs########
tests = c("175738328", 
          "148765242",
          "531699500",
          "563028615",
          "722510442")
fpt.012.complete = fpt_012%>%filter(!subject %in% tests)

######remove photos that were excluded######
bad.photos = c("google-fa20-ww13.jpg",
               "google-fa20-ww21.jpg",
               "google-fa20-ww34.jpg",
               "google-fa20-ww35.jpg")
fpt.012.complete = fpt.012.complete%>%filter(!stimulusitem1 %in% bad.photos)

#####how many ratings per photo per trial
photo.trial.n = fpt.012.complete%>%group_by(stimulusitem1, trialcode)%>%summarize(n=n())



######plot all measures#######
fpt.012.complete$response = as.numeric(fpt.012.complete$response)
hist(fpt.012.complete$response[fpt.012.complete$trialcode=="anger"])
hist(fpt.012.complete$response[fpt.012.complete$trialcode=="happy"])


#####race######
fpt.race = fpt.012.complete%>%
  filter(trialcode == "race")%>%
  group_by(stimulusitem1,response)%>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n))
fpt.race = fpt.race[order(fpt.race$stimulusitem1, -fpt.race$freq),]
fpt.race.most.choice = fpt.race[!duplicated(fpt.race$stimulusitem1),]
fpt.race.remove = fpt.race.most.choice$stimulusitem1[fpt.race.most.choice$freq<=0.8]


#####gender######
fpt.gender = fpt.012.complete%>%
  filter(trialcode == "gender")%>%
  group_by(stimulusitem1,response)%>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n))
fpt.gender = fpt.gender[order(fpt.gender$stimulusitem1, -fpt.gender$freq),]
fpt.gender.most.choice = fpt.gender[!duplicated(fpt.gender$stimulusitem1),]
fpt.gender.remove= fpt.gender.most.choice$stimulusitem1[fpt.gender.most.choice$freq<=0.8]


#####emotioins######
fpt.emotions = fpt.012.complete%>%
  filter(trialcode != "race")%>%
  filter(trialcode != "gender")%>%
  group_by(stimulusitem1,trialcode)%>%
  summarise(avg = mean(as.numeric(response)),
            n=n())
unique(fpt.emotions$trialcode)
hist(fpt.emotions$avg[fpt.emotions$trialcode=="anger"])
fpt.anger.remove = fpt.anger$stimulusitem1[fpt.anger$avg>=4]
hist(fpt.emotions$avg[fpt.emotions$trialcode=="disgusted"])
fpt.disgusted.remove = fpt.emotions$stimulusitem1[fpt.emotions$avg>=4 &
                                                    fpt.emotions$trialcode == "disgusted"]
fpt.sad.remove = fpt.emotions$stimulusitem1[fpt.emotions$avg>=4 &
                                                    fpt.emotions$trialcode == "sad"]

hist(fpt.emotions$avg[fpt.emotions$trialcode=="fearful"])
hist(fpt.emotions$avg[fpt.emotions$trialcode=="photo"])



#######combine info for all ######
colnames(fpt.race.most.choice)[2:4] = paste0("race.",colnames(fpt.race.most.choice)[2:4])
colnames(fpt.gender.most.choice)[2:4] = paste0("gender.",colnames(fpt.gender.most.choice)[2:4])

fpt.012.info = merge(fpt.race.most.choice, fpt.gender.most.choice, by = "stimulusitem1")

###combine with emotions######

fpt.012.emotions.wide = spread(fpt.emotions[,-4], trialcode, avg)
fpt.012.all.info = merge(fpt.012.info,fpt.012.emotions.wide , by = "stimulusitem1", all.x = T)
write.csv(fpt.012.all.info, "google face pretest all info.csv",row.names = F
          )
