library(caret); library(tidyr); library(Hmisc); library(dplyr)

REvent_type <- read.csv("event_type.csv")
RLog_feature <- read.csv("log_feature.csv")
RResource_type <- read.csv("resource_type.csv")
RSeverity_type <- read.csv("severity_type.csv")
RTrain <- read.csv("train.csv")

# ----------Frequency of event type
# Join tables to match event id, severity level, and event types
temp <- inner_join(RTrain, REvent_type, by="id")
temp <- select(temp, event_type, fault_severity)
# Count up frequencies of each event type by their fault severity levels for all events
eventFreq <- temp %>% group_by(event_type, fault_severity) %>% summarize(count=n())
rm(temp)
# Spread the table to wide format so that severity levels are variables with frequencies as values
eventFreq <- spread(eventFreq, fault_severity, count)
colnames(eventFreq) <- c("event_type", "fault0", "fault1", "fault2")
# Some event type and severity level combinations are not going to occur and will have NA 
# Replace with 0 frequency count
eventFreq[is.na(eventFreq)] <- 0
# Divide the freqency range into 5 buckets
eventFreq <- eventFreq %>% mutate(buckets0 = cut2(eventFreq$fault0, g=5), buckets1 = cut2(eventFreq$fault1, g=5),
                          buckets2 = cut2(eventFreq$fault2, g=5)) 
## a1 = event_fault0_5_1; b2 = event_fault1_5_2
levels(eventFreq$buckets0) <- c("a1", "a2", "a3", "a4", "a5")
levels(eventFreq$buckets1) <- c("b1", "b2", "b3", "b4", "b5")
levels(eventFreq$buckets2) <- c("c1", "c2", "c3", "c4", "c5")
eventFreq$fault0 <- NULL; eventFreq$fault1 <- NULL; eventFreq$fault2 <- NULL
eventFreq$freq <- 1
eventFreq <- spread(eventFreq, buckets0, freq)
eventFreq$freq <- 1
eventFreq <- spread(eventFreq, buckets1, freq)
eventFreq$freq <- 1
eventFreq <- spread(eventFreq, buckets2, freq)
eventFreq[is.na(eventFreq)] <- 0
event <- left_join(REvent_type, eventFreq, by="event_type")
event[is.na(event)] <- 0
eventFinal <- event %>% group_by(id) %>% summarize("a1"=sum(a1), "a2"=sum(a2), "a3"=sum(a3),
                                                   "a4"=sum(a4), "a5"=sum(a5),
                                                   "b1"=sum(b1), "b2"=sum(b2), "b3"=sum(b3),
                                                   "b4"=sum(b4), "b5"=sum(b5),
                                                   "c1"=sum(c1), "c2"=sum(c2), "c3"=sum(c3),
                                                   "c4"=sum(c4), "c5"=sum(c5))
rm(eventFreq, event)
colnames(eventFinal) <- c("id",
                          "event_fault0_5_1", "event_fault0_5_2", "event_fault0_5_3",
                          "event_fault0_5_4", "event_fault0_5_5",
                          "event_fault1_5_1", "event_fault1_5_2", "event_fault1_5_3",
                          "event_fault1_5_4", "event_fault1_5_5",
                          "event_fault2_5_1", "event_fault2_5_2", "event_fault2_5_3",
                          "event_fault2_5_4", "event_fault2_5_5")


# ----------Frequency of log feature
featureVolume <- RLog_feature
# Bucket volume by v=1, v=2, v=3:4, v=5:9, v>10
featureVolume$volume <- sapply(featureVolume$volume, function(x){
  if(x==1){x <- "V1"
  } else if(x==2){x <- "V2"
  } else if(x==3 | x==4){x <- "V3"
  } else if(x>4 && x<10){x <- "V4"
  } else{x <- "V5"}
})
temp <- inner_join(RTrain, featureVolume, by="id")
temp <- select(temp, log_feature, volume, fault_severity)
featureFreq <- temp %>% group_by(log_feature, volume, fault_severity) %>% summarize(count=n())
rm(temp)
featureFreq <- spread(featureFreq, fault_severity, count)
colnames(featureFreq) <- c("log_feature", "volume", "fault0", "fault1", "fault2")
featureFreq[is.na(featureFreq)] <- 0
featureFreq <- featureFreq %>% mutate(buckets0 = cut(featureFreq$fault0, breaks=c(-0.1,0.1,1,2,3,6,10,18,50,200,567)), 
                                      buckets1 = cut(featureFreq$fault1, breaks=c(-0.1,0.1,1,2,3,6,10,18,50,100,231)),
                                      buckets2 = cut(featureFreq$fault2, breaks=c(-0.1,0.1,1,2,3,6,10,18,30,50,417))) 
## a1 = feature_fault0_5_1; b2 = feature_fault1_5_2
levels(featureFreq$buckets0) <- c("a1", "a2", "a3", "a4", "a5", "a6","a7","a8","a9","a10")
levels(featureFreq$buckets1) <- c("b1", "b2", "b3", "b4", "b5", "b6","b7","b8","b9","b10")
levels(featureFreq$buckets2) <- c("c1", "c2", "c3", "c4", "c5", "c6","c7","c8","c9","c10")
featureFreq$fault0 <- NULL; featureFreq$fault1 <- NULL; featureFreq$fault2 <- NULL
featureFreq$freq <- 1
featureFreq <- spread(featureFreq, buckets0, freq)
featureFreq$freq <- 1
featureFreq <- spread(featureFreq, buckets1, freq)
featureFreq$freq <- 1
featureFreq <- spread(featureFreq, buckets2, freq)
featureFreq[is.na(featureFreq)] <- 0
feature <- left_join(featureVolume, featureFreq, by=c("log_feature","volume"))

feature[is.na(feature)] <- 0
featureFinal <- feature %>% group_by(id) %>% summarize(sum(a1),sum(a2),sum(a3),sum(a4),sum(a5),
                                                       sum(a6),sum(a7),sum(a8),sum(a9),sum(a10),
                                                       sum(b1),sum(b2),sum(b3),sum(b4),sum(b5),
                                                       sum(b6),sum(b7),sum(b8),sum(b9),sum(b10),
                                                       sum(c1),sum(c2),sum(c3),sum(c4),sum(c5),
                                                       sum(c6),sum(c7),sum(c8),sum(c9),sum(c10))
rm(featureFreq, feature)
colnames(featureFinal) <- c("id",
                          "feature_fault0_10_1", "feature_fault0_10_2", "feature_fault0_10_3",
                          "feature_fault0_10_4", "feature_fault0_10_5",
                          "feature_fault0_10_6", "feature_fault0_10_7", "feature_fault0_10_8",
                          "feature_fault0_10_9", "feature_fault0_10_10",
                          "feature_fault1_10_1", "feature_fault1_10_2", "feature_fault1_10_3",
                          "feature_fault1_10_4", "feature_fault1_10_5",
                          "feature_fault1_10_6", "feature_fault1_10_7", "feature_fault1_10_8",
                          "feature_fault1_10_9", "feature_fault1_10_10",
                          "feature_fault2_10_1", "feature_fault2_10_2", "feature_fault2_10_3",
                          "feature_fault2_10_4", "feature_fault2_10_5",
                          "feature_fault2_10_6", "feature_fault2_10_7", "feature_fault2_10_8",
                          "feature_fault2_10_9", "feature_fault2_10_10")


# ----------Resource type only 10 factors --> dummy variables
resourceFinal <- RResource_type
resourceFinal$freq <- 1
resourceFinal <- spread(resourceFinal, resource_type, freq)
resourceFinal[is.na(resourceFinal)] <- 0

# ----------Severity only 5 factors --> dummy variables
severityFinal <- RSeverity_type
severityFinal$freq <- 1
severityFinal <- spread(severityFinal, severity_type, freq)
severityFinal[is.na(severityFinal)] <- 0

# ----------Frequency of locations
locationFreq <- RTrain %>% group_by(location, fault_severity) %>% summarize(count = n())
locationFreq <- spread(locationFreq, fault_severity, count)
colnames(locationFreq) <- c("location", "fault0", "fault1", "fault2")
locationFreq[is.na(locationFreq)] <- 0
locationFreq <- locationFreq %>% mutate(buckets0 = cut(locationFreq$fault0, c(-0.1,0.1,1,2,3,4,6,8,15,30,69)),
                                        buckets1 = cut(locationFreq$fault1, c(-0.1,0.1,1,2,3,4,5,6,8,15,27)),
                                        buckets2 = cut(locationFreq$fault2, c(-0.1,0.1,1,2,3,5,7,11,15,25,33)))
## a1 = location_fault0_5_1; b2 = location_fault1_5_2
levels(locationFreq$buckets0) <- c("a1", "a2", "a3", "a4", "a5", "a6","a7","a8","a9","a10")
levels(locationFreq$buckets1) <- c("b1", "b2", "b3", "b4", "b5", "b6","b7","b8","b9","b10")
levels(locationFreq$buckets2) <- c("c1", "c2", "c3", "c4", "c5", "c6","c7","c8","c9","c10")
locationFreq$fault0 <- NULL; locationFreq$fault1 <- NULL; locationFreq$fault2 <- NULL
locationFreq$freq <- 1
locationFreq <- spread(locationFreq, buckets0, freq)
locationFreq$freq <- 1
locationFreq <- spread(locationFreq, buckets1, freq)
locationFreq$freq <- 1
locationFreq <- spread(locationFreq, buckets2, freq)
locationFreq[is.na(locationFreq)] <- 0
## locationFinal only has location as key, no id
locationFinal <- locationFreq
rm(locationFreq)
colnames(locationFinal) <- c("location",
                            "location_fault0_10_1", "location_fault0_10_2", "location_fault0_10_3",
                            "location_fault0_10_4", "location_fault0_10_5",
                            "location_fault0_10_6", "location_fault0_10_7", "location_fault0_10_8",
                            "location_fault0_10_9", "location_fault0_10_10",
                            "location_fault1_10_1", "location_fault1_10_2", "location_fault1_10_3",
                            "location_fault1_10_4", "location_fault1_10_5",
                            "location_fault1_10_6", "location_fault1_10_7", "location_fault1_10_8",
                            "location_fault1_10_9", "location_fault1_10_10",
                            "location_fault2_10_1", "location_fault2_10_2", "location_fault2_10_3",
                            "location_fault2_10_4", "location_fault2_10_5",
                            "location_fault2_10_6", "location_fault2_10_7", "location_fault2_10_8",
                            "location_fault2_10_9", "location_fault2_10_10")


# Expand RTrain with all features
total <- left_join(RTrain, eventFinal, by="id")
total <- left_join(total, featureFinal, by="id")
total <- left_join(total, locationFinal, by="location")
total$location <- NULL
total <- left_join(total, resourceFinal, by="id")
total <- left_join(total, severityFinal, by="id")

total$id <- NULL
total$fault_severity <- as.factor(total$fault_severity)
for (n in 2:dim(total)[2]){
  total[,n] <- as.integer(total[,n])
}

levels(total$fault_severity) <- c("predict_0", "predict_1", "predict_2")

# Modeling
set.seed(123)
inTrain <- createDataPartition(total$fault_severity, p=0.7, list=FALSE)
training <- total[inTrain,]
testing <- total[-inTrain,]

set.seed(123)
fitCtrl <- trainControl(verboseIter = TRUE)
modelRF <- train(fault_severity ~ ., data=training, method="rf", prox=TRUE, trControl=fitCtrl,
                 tuneGrid = expand.grid(mtry = c(9)))
predictions <- predict(modelRF, testing)
confusionMatrix(predictions, testing$fault_severity)


test <- read.csv("test.csv")
totalTest <- left_join(test, eventFinal, by="id")
totalTest <- left_join(totalTest, featureFinal, by="id")
totalTest <- left_join(totalTest, locationFinal, by="location")
totalTest$location <- NULL
totalTest <- left_join(totalTest, resourceFinal, by="id")
totalTest <- left_join(totalTest, severityFinal, by="id")
totalTest[is.na(totalTest)] <- 0
predictions <- predict(modelRF, totalTest, type="prob")

submission <- cbind(id=totalTest$id,predictions)

write.csv(submission, file="submission.csv", row.names=FALSE)
