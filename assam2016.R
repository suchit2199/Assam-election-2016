#import data
assam2016 <- read.csv(file.choose(), header = TRUE, sep = ",")
View(assam2016)

#install packages
install.packages("tydyverse")
install.packages("ggplot2")
#load packages
library(tidyverse)
library(ggplot2)

#total no. of votes
party <- assam2016 %>% group_by(Party) %>% summarise(TotalVotes = sum(Total.Votes))
View(party)
ggplot(party,aes(Party,TotalVotes)) + 
  geom_bar(stat = "identity", fill = "pink") +labs(title = "Total no. of votes ") + 
  coord_flip() + geom_text(aes(label=TotalVotes), vjust=-0.3, size=3.5)


#Total no. of constituency
constituency <- assam2016 %>% group_by(Party) %>% summarise(Totalconstituency = sum(AC.No.))
View(constituency)
ggplot(constituency,aes(Party,Totalconstituency)) + 
  geom_bar(stat = "identity", fill = "lightblue") +labs(title = "Total no. of constituency ") + 
  coord_flip() + geom_text(aes(label=Totalconstituency), vjust=-0.3, size=3.5)


#Total no. of Seats
Seats <- aggregate(assam2016$Party,list(assam2016$Party), FUN = length)
View(Seats) 
colnames(Seats) <- c("Party","Totalseats")
ggplot(Seats,aes(Party,Totalseats)) + 
  geom_bar(stat = "identity", fill = "green") +labs(title = "Total no. of Seats") + 
  coord_flip() + geom_text(aes(label=Totalseats), vjust=-0.3, size=3.5)


#top 3 winning candidate
candidate <- assam2016 %>% group_by(Winning.Candidate) %>% summarise(TotalVotes = sum(Total.Votes))
View(candidate)
candidate1 <- arrange(candidate, candidate$TotalVotes)
View(candidate1)
candidate2 <- slice(candidate1,123:125)
View(candidate2)
ggplot(candidate2,aes(Winning.Candidate,TotalVotes)) + 
  geom_bar(stat = "identity", fill = "blue") +labs(title = "top 3 winning candidate ") + 
  geom_text(aes(label=TotalVotes), vjust=-0.3, size=3.5)

#top 10 winning candidate by margin
margin <- assam2016 %>% group_by(Winning.Candidate) %>% summarise(totalmargin = sum(Margin))
View(margin)
margin <- arrange(margin, margin$totalmargin)
margin1 <- slice(margin,115:125)
View(margin1)
ggplot(margin1,aes(Winning.Candidate,totalmargin)) + 
  geom_bar(stat = "identity", fill = "lightgreen") +labs(title = "top 10 winning candidate by margin") + 
  coord_flip() +
  geom_text(aes(label=totalmargin), vjust=-0.3, size=3.5)

#caste
caste <- aggregate(assam2016$Type,list(assam2016$Type), FUN = length)
View(caste) 
colnames(caste) <- c("Caste","Count")
ggplot(caste, aes(x="", y=Count, fill=Caste)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#Total no. of electors as per district
district <- assam2016 %>% group_by(District) %>% summarise(TotalElectors = sum(Total.Electors))
View(district)
ggplot(district,aes(District,TotalElectors)) + 
  geom_bar(stat = "identity", fill = "pink") +labs(title = "Total no. of electors as per district") + 
  coord_flip() + geom_text(aes(label=TotalElectors), vjust=-0.3, size=3.5)


