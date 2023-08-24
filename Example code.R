
library(rem)
library(survival)

# 1. Longitudinal network data 

events<-data.frame(s=c('s1','s2','s1','s3','s2','s3','s3','s1'), # Sender (i.e.,student) ID
               r=c('r1_a1','r2_a1','r3_a1','r4_a1','r1_a2','r2_a2','r1_a3','r2_a3'), # Receiver (i.e., annotation) ID 
               t=c('T1','T2','T3','T4','T5','T6','T7','T8'), ## Event time stamp (in format "%Y-%m-%d"); it is used for ordering the annotation event
               thread=c('a1','a1','a1','a1','a2','a2','a3','a3'), ## Thread ID
               article=c('d1','d1','d1','d1','d1','d1','d2','d2'), ## Article (i.e., document) ID
               class=c('c1','c1','c1','c1','c1','c1','c1','c1'),  ## Class ID
               annotation.text=c('string1','string2','string3','string4','string5','string6','string7','string8')  ## annotation text strings
               )
  ## Note: We here use a mock relational data set for illustration


## Using events data, we yield the event sequence indicator (i.e., "event.seq.ord")
  nw.dt <- eventSequence(events$t,
                         dateformat = "%Y-%m-%d",
                         byTime = "daily",
                         data = events,
                         type = "ordinal",
                         returnData = TRUE,
                         sortData = TRUE)


## Risk set data

dtrem<-createRemDataset(data = nw.dt,
                            sender = dt$s,
                            target = dt$r,
                            eventSequence = dt$event.seq.ord,
                            atEventTimesOnly = TRUE,
                            untilEventOccurrs = T,
                            returnInputData = F)
  ### In this risk set data, we have variables of "eventID", "sender", "target", "eventTime", "eventDummy", "eventAtRiskFrom", and "eventAtRiskUntil". 
  ### sort the data frame according to the event sequence
  dtrem <- dtrem[order(dtrem$eventTime), ]

  
# 2. Previous relational events

## 2.1 Prior annotation popularity (i.e., indegree of target)

dtrem$inDeg.target <- degreeStat(dtrem,
                                       time = dtrem$eventTime,
                                       degreevar = dtrem$target, 
                                       halflife = halflife, 
                                 ### "halflife" is the decay temporal weight 
                                 ### We calculated the halfliife values for each classroom using the forumla: total annotations / weeks /2 
                                       weight=NULL, 
                                       returnData = FALSE,
                                       eventvar = dtrem$eventDummy)


## 2.2 Prior actor activity (i.e., outdegree of actors)
dtrem$outDeg.actor <- degreeStat(dtrem,
                                 time = dtrem$eventTime,
                                 degreevar = dtrem$sender, 
                                 halflife = halflife, 
                                 weight=NULL,
                                 returnData = FALSE,
                                 eventvar = dtrem$eventDummy)


## 2.3 Inertia: It measures whether events have a tendency to be repeated with the same sender and target in the same thread.
dtrem$inertia <- inertiaStat(dtrem, 
                              time = dtrem$eventTime,
                              sender = dtrem$sender,
                              target = dtrem$target,
                              halflife = halflife, 
                              weight=NULL, 
                              returnData = FALSE,
                              eventvar = dtrem$eventDummy)


# 3. Semantic features of artifacts

semantic.features<-data.frame (
  r=c('r1_a1','r2_a1','r3_a1','r4_a1','r1_a2','r2_a2','r1_a3','r2_a3'), # Receiver (i.e., annotation) ID 
  t=c('T1','T2','T3','T4','T5','T6','T7','T8'), ## Event timestamp (in formate "%Y-%m-%d"); it is used for ordering the annotation event
  thread=c('a1','a1','a1','a1','a2','a2','a3','a3'), ## Thread ID
  thread.total.tag=c(x1,x1,x1,x1,x2,x2,x2,x2), ## Accumulated taq counts within threads.
  thread.total.QMark=c(z1,z1,z1,z1,z2,z2,z2,z2),   ## Accumulated question counts within threads.
  avg.pairwise.cosineSimilarity=c(0, s(r1_a1,r2_a1), (s(r1_a1,r2_a1)+s(r2_a1,r3_a1))/2, (s(r1_a1,r2_a1)+s(r2_a1,r3_a1)+s(r1_a1,r3_a1))/3, 
                                  0, s(r1_a2, r2_a2), 0, s(r1_a3, r2_a3))# Semantic cohesion within threads
  )

  ## Note: To calculate the cosine similarity among words and sentences, we used word2vec and GLOVE pretrained word embedding models. We here used mock data for simplicity.  



# 4. Control variables

attributes.dt<- data.frame(
  thread=c('a1','a1','a1','a1','a2','a2','a3','a3'), ## Thread ID
  thread.location.quintile= c(q1,q1,q1,q1,q2,q2,q2,q2),
      ## The location of a thread relative to the article (ranked by qunitile)
  thread.highlighted.length= c(l1,l1,l1,l1,l2,l2,l2,l2), ## The word count of the highlighted text
  thread.durtaion.week.quintile=c(d1,d1,d1,d1,d2,d2,d2,d2) ## The count of a thread's active week terms (ranked by qunitile) 
)

# 5. Modeling relationship using REM

## Full data 
    dtrem.full<-dtrem %>% 
    left_join(semantic.features, by=c("r")) %>% 
    left_join(attributes.dt, by=c("thread","eventTime" )) 
  

## 5.1 Network-only model 
model1 <-clogit(eventDummy~inDeg.target+outDeg.actor+inertia # Previous relational events
                       +relevel(thread.location.quintile, ref=q5)+thread.highlighted.length+relevel(thread.durtaion.week.quintile, ref=d5) # Control variables
                       cluster(article)+strata(eventTime),
                       data=remdt.full,
                       method="approximate")

## 5.2 Semantic-only model 
model2 <- clogit(eventDummy~thread.total.tag+thread.total.QMark+avg.pairwise.cosineSimilarity # Semantic features
                            +relevel(thread.location.quintile, ref=q5)+thread.highlighted.length+relevel(thread.durtaion.week.quintile, ref=d5) # Control variables
                            cluster(doc.ID)+strata(eventTime),
                            data=remdt.full, 
                            method="approximate")

## 5.3 Full model 
model3 <- clogit(eventDummy~inDeg.target+outDeg.actor+inertia  # Previous relational events
                        +thread.total.tag+thread.total.QMark+avg.pairwise.cosineSimilarity # Semantic features
                        +relevel(thread.location.quintile, ref=q5)+thread.highlighted.length+relevel(thread.durtaion.week.quintile, ref=d5) # Control variables
                        cluster(article)+strata(eventTime),
                        data=remdt.full,
                        method="approximate")


 

