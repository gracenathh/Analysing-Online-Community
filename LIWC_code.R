library(ggplot2)
library(lattice)
library(reshape2)
library(lubridate)
library(igraph)
library(igraphdata)

rm(list = ls())
set.seed(30241510) # XXXXXXXX = your student ID

#importing data
webforum = read.csv("webforum.csv")
webforum = webforum [sample(nrow(webforum), 20000), ] # 20000 rows

#tidying data by removing anonymous authorID and 0 word count
webforum = subset(webforum, AuthorID != -1)
webforum = subset(webforum, WC != 0)

#Formating Date to Date since it was previously a string
webforum$Date = as.Date(webforum$Date)

#Formating ThreadID as factor
webforum$ThreadID = as.factor(webforum$ThreadID)

#Creating Month-Year by extracting from Date
webforum$month_year = format(as.Date(webforum$Date), "%Y-%m")

#Getting summary of the data
variables_summary = summary(webforum[,5:29])
######################## Part A ########################
#Part A.1: How active are participants, and are there periods where this increases or decreases?
#Calculating Frequency of post in each month_year
freq_count = as.data.frame(as.table(tapply(webforum$ThreadID,list(webforum$month_year),length)))
colnames(freq_count) = c("Month_Year","Freq") #rename the columns' name

#Plotting the freqcount
timeplot = ggplot(data = freq_count, aes(x=Month_Year,y=Freq, group = 1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(color = "#00AFBB") + ggtitle("Activity in Online Community over Month-Year")

ggsave("PartA_Point1.jpg",timeplot,width = 50, height = 15, units = "cm")

#Part A.2.1: Looking at the linguistic variables, do these change over time?
#Calculating the mean of each variable group by year - only some variables are used based on the correlation
variable_variety = as.data.frame(aggregate(webforum[,c(6,7,8,9,10,17,18,19,22)],list(webforum$month_year),mean))
colnames(variable_variety)[1] = 'Month_Year'

#Melting our data to create a graph
variable_variety = melt(variable_variety, id = c("Month_Year"))
variable_variety$value = round(variable_variety$value,digits = 2)
colnames(variable_variety) = c("Month_Year","Linguistic_Variables","Value")

#Plotting a multiple lines graph to know the usage of linguistic variables over time
time_variable = ggplot(data = variable_variety, aes(x = Month_Year, y = Value, group = Linguistic_Variables, color = Linguistic_Variables)) +
  geom_line(size = 1) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Average Proportion of Linguistic Variables over Month-Year (2002 to 2011)")

ggsave("PartA_Point2.1a.jpg",time_variable,width = 50, height = 15, units = "cm")

#Part A.2.2: Is there a relationship between them?
#Calculating each variable's correlation into each other by first filtering the linguistic variables
correlation_data = webforum[,6:29]
correlation_data = round(cor(correlation_data),2)
diag(correlation_data) = -1
correlation_data = melt(correlation_data)
cor_heatmap = ggplot(data = correlation_data, aes(x=Var1, y=Var2, fill=value))
cor_heatmap = cor_heatmap + geom_tile(color = "white") + scale_fill_gradient(high = "#8ce2ff", low = "#fff48c")
cor_heatmap = cor_heatmap + theme(axis.text.x = element_text(angle = 90, hjust = 1))
cor_heatmap = cor_heatmap + ggtitle("Correlation Between Linguistic Variables")
cor_heatmap = cor_heatmap + theme(legend.position = "right")

ggsave("PartA_Point2.2.jpg",cor_heatmap,width = 25, height = 17, units = "cm")

#Proving that linguistic variables do change over times. Variables taken: Analytic, Authentic, and negemo
#Data A would be from 2002, and DataB would be from 2011
webforum$Year = year(webforum$Date)
data_2002 = webforum[webforum$Year == 2002,]
data_2011 = webforum[webforum$Year == 2011,]

#Perform t-test for Analytic
t.test(data_2002$Analytic,data_2011$Analytic, conf.level = 0.95)

#Perform t-test for Authentic
t.test(data_2002$Authentic,data_2011$Authentic, conf.level = 0.95)

#Perform t-test for negemo since it's considered to be stable
t.test(data_2002$negemo,data_2011$negemo, conf.level = 0.95)

######################## Part B ########################
#Part B.1: Describe the threads present in your data.
#Since a Thread happens because people contribute to it and we are now analysing linguistic variables, we have to know which author
#is the most active (this will help us for further analysis such as in part C)
#Take top 6 Threads over month-year

top_thread = as.table(by(webforum$Date,webforum$ThreadID,length))
top_thread = sort(top_thread, decreasing = TRUE)
top_thread6 = as.data.frame(head(top_thread,6))
colnames(top_thread6) = c("ThreadID","Freq")
top_thread6 = webforum[(webforum$ThreadID %in% top_thread6$ThreadID),]

#Creating a histograph for each ThreadID
g = ggplot(data = top_thread6) + 
  geom_histogram(mapping = aes(x = as.Date(Date), fill = ThreadID), color = "black") +
  ggtitle("6 Top Threads (January 2002 to December 2011)") +
  xlab("Date") +
  ylab("Number of Post") +
  facet_wrap(~ ThreadID, nrow = 2)

ggsave("PartB_Point1.jpg",g,width = 20, height = 10, units = "cm")

#Part B.2: By analysing the linguistic variables for all or some of the threads, is it possible to see a difference in the language
#used by these different groups?
#We will be using the top 6 threads from previous point to analyse the language used by each group

language_in_thread = as.data.frame(aggregate(top_thread6[,6:29],list(top_thread6$ThreadID),mean))
colnames(language_in_thread)[1] = "ThreadID"

#Since there exist some variables that are dependent to another variable, we will only select:
#Analytics, Clout, Authentic, Tone, ppron, posemo and negemo, as well as words referring to topics
language_in_thread = language_in_thread[,c(-7,-8,-9,-10,-11,-12,-13,-24,-25)]

language_in_thread = melt(language_in_thread, id = "ThreadID")

language_usage_graph = barchart(value~ThreadID,data = language_in_thread, main=list(label = "Language Used in Top 6 Threads", cex = 2),xlab = "ThreadID",
                                  ylab = "Average Proportion Used by Thread", group = variable, scales=list(x=list(cex=1.5)), auto.key = list(space = "right",title = "Linguistic\nVariables",cex.title = 1)) 


language_usage_ggplot = ggplot(language_in_thread, aes(x = ThreadID, y = value, fill =  variable))+  geom_bar(position = "dodge", stat = "identity") 

#Performing t-test on Analytics and Tone in ThreadID 127115 and 472752
data_127115 = webforum[webforum$ThreadID == 127115,]
data_472752 = webforum[webforum$ThreadID == 472752,]
data_532649 = webforum[webforum$ThreadID == 532649,]

#t-test for Analytics in ThreadID 127115 and 472752
t.test(data_127115$Analytic,data_472752$Analytic, "less", conf.level = 0.95)

#t-test for Tone in ThreadID 127115 and 472752
t.test(data_127115$Tone,data_472752$Tone, "greater", conf.level = 0.95)

#t-test for family in ThreadID 472752 and 532649
t.test(data_532649$family,data_472752$family, "less", conf.level = 0.95)

#Part B.3: Does the language used within threads change over time?
#Calculating the mean of each variable in top 6 threads group by year
language_over_years = as.data.frame(aggregate(top_thread6[,6:29],list(top_thread6$Year),mean))
language_over_years = language_over_years[,c(-7,-8,-9,-10,-11,-12,-13,-24,-25)]
colnames(language_over_years)[1] = "Year"

#Melting our data to create a graph
language_over_years = melt(language_over_years, id = c("Year"))
language_over_years$value = round(language_over_years$value, digits = 2)
colnames(language_over_years) = c("Year","Linguistic_Variables","Value")

thread_time_variable = ggplot(data = language_over_years, aes(x = Year, y = Linguistic_Variables, fill = Value)) + geom_tile(color="white") +
  ggtitle("Average Proportion of Lingustic Variables Used by Top 6 Threads over Years (2002 to 2011)") + scale_fill_gradient(low ="#8cf2ff", high = "#ff8cbc")

ggsave("PartB_Point3.jpg",thread_time_variable,width = 22, height = 12, units = "cm")

#t-test for friend to prove that variables referring to words also change.
#Choose data in 2004 to 2007 (Data A)
friend_data_0407 = webforum[as.Date(webforum$Date, "%Y-%m-%d") >= as.Date("2004-01-01","%Y-%m-%d"),]
friend_data_0407 = friend_data_0407[as.Date(friend_data_0407$Date, "%Y-%m-%d") < as.Date("2008-01-01","%Y-%m-%d"),]

#Choose data in 2008 to 2011 (Data B)
friend_data_0811 = webforum[as.Date(webforum$Date, "%Y-%m-%d") >= as.Date("2008-01-01","%Y-%m-%d"),]
  
#doing t-test for friend from (Data A and B)
t.test(friend_data_0407$friend, friend_data_0811$friend, conf.level = 0.95)

######################## Part C ########################
#Part C.1: Social Networks Online

#Choosing a period of time to get top author from that period of time
#Time: December 2005 when  of post is the highest
SNO = webforum[as.Date(webforum$Date,"%Y-%m-%d") >= as.Date("2005-12-01","%Y-%m-%d"), ]
SNO = SNO[as.Date(SNO$Date,"%Y-%m-%d") < as.Date("2006-01-01","%Y-%m-%d"), ]

top_author = as.table(by(SNO$Date, SNO$AuthorID, length))
top_author = sort(top_author, decreasing = TRUE)
top_author10 = as.data.frame(head(top_author,10))
colnames(top_author10) = c("AuthorID","Freq")
top_author10_data = SNO[(SNO$AuthorID%in%top_author10$AuthorID),]
top_author10_data = top_author10_data[,1:2]

#Finding on which ThreadID(s) top 10 authors posts at. This is to create a matrix for social network
for (i in 1:10){
  author = top_author10[i,1]
  author = as.character(author)
  cat("AuthorID: ",author,"\n")
  author_data = top_author10_data[top_author10_data$AuthorID == author,]
  cat("ThreadID(s): ",as.character(unique(author_data$ThreadID)),"\n","\n")
}

#Creating the graph for the social network and Analysing it
author = read.csv("author10_sno.csv", header = TRUE, row.names = 1)
colnames(author) = c(34292,76174,41237,61230,64019,79878,83857,62481,32925,81525)
author_matrix = as.matrix(author)

g1 = graph_from_adjacency_matrix(author_matrix, mode = "undirected")
plot(g1, vertex.color = "red", main = "Connection Among Top 10 Authors in December 2005")

#Calculating the number of vertex and edges in the network
print(vcount(g1)) #10
print(ecount(g1)) #69

#Checking whether the network is simple or not
print(is.simple(g1)) #False

#Calculating the graph's diameter and avg path length
print(diameter(g1))
print(average.path.length((g1)))
hist(degree(g1), breaks = 5, col = "grey")

#Calculatig graph's clique
table(sapply(cliques(g1), length))

#Calculating the degree, closeness, betweenness and eigenvector of each vertex
#and round it to 2 and put it together as a dataframe
deg = as.table(round(degree(g1), digits = 2))
cl = as.table(round(closeness(g1), digits = 2))
bet = as.table(round(betweenness(g1)), digits = 2)
eig = as.table(round(evcent(g1)$vector, digits = 2))

tab = as.data.frame(cbind(deg,cl,bet,eig))
colnames(tab) = c("Degree","Closeness","Betweenness","Eigenvector")

#Order by Degree
print(tab[order(-tab$Degree),])

#Order by Closeness
print(tab[order(-tab$Closeness),])

#Order by Betweeneess
print(tab[order(-tab$Betweenness),])

#Order by Eigenvector
print(tab[order(-tab$Eigenvector),])

#Looking at the largest clique to determine that author 34292 is the most important author
cliques(g1)[sapply(cliques(g1), length)== 8]

#Checking top 10 most active authors in January 2006
SNO_jan = webforum[as.Date(webforum$Date,"%Y-%m-%d") >= as.Date("2006-01-01","%Y-%m-%d"), ]
SNO_jan = SNO_jan[as.Date(SNO_jan$Date,"%Y-%m-%d") < as.Date("2006-02-01","%Y-%m-%d"), ]

top_author_jan = as.table(by(SNO_jan$Date, SNO_jan$AuthorID, length))
top_author_jan = sort(top_author_jan, decreasing = TRUE)
top_author10_jan = as.data.frame(head(top_author_jan,10))
