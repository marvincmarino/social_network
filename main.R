
#### Data load ####

data = read.csv("./data.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
col_names = c("createdBy","name1","name2","date","place","itemType","path")
colnames(data) = col_names; colnames(data)


#### Data prep ####

# Need to create links and nodes objects, where:
# nodes are people
# links are events

# get list of all names
inputNames1 = data$createdBy; length(inputNames1)
inputNames2 = data$name1; length(inputNames2)
inputNames3 = data$name2; length(inputNames3)

listNames = c(inputNames1, inputNames2, inputNames3); str(listNames)
listNames = unique(listNames); str(listNames)

# declare nodes as list of names
nodes = as.data.frame(listNames)
# nodes = cbind(1:dim(nodes)[1], nodes); colnames(nodes) = c("id","nodesName"); nodes

# links
links = data[, c("name1", "name2", "date", "place")]


#### Network plots & general stats ####
library(igraph)

# init graph object
net = graph.data.frame(links, directed = T, vertices = nodes)

# general network stats
diameter(net)
farthest.nodes(net)

# type 0: plain vanilla
plot(net)

# improve dataviz: some plot for tests

# type 1 - normal improved
plot(net, edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$nodesName, vertex.label.color="black", 
     main="Network",
     vertex.size=0.1, vertex.label.cex=0.8) 

# type 2 - grey style
plot(net, vertex.shape="none", vertex.label=V(net)$nodesName, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")

# type 3 - customised with different layouts
net.bg = net

net.bg = simplify(net.bg, remove.multiple = T, remove.loops = T) # simplify

# edit parameters
V(net.bg)$frame.color = "white"
V(net.bg)$color = "orange"
V(net.bg)$label = "" 
V(net.bg)$size = 2
V(net.bg)$label.cex=0.8
E(net.bg)$arrow.mode = 0

# select one layout l
l = layout.circle(net.bg)
l = layout.kamada.kawai(net.bg)
l = layout.fruchterman.reingold(net.bg)

# plot
plot(net.bg, vertex.label=V(net.bg)$name, 
     rescale=T, layout=l,  main="Media-Saturn Account Lunch Date Network")

# plot(net.bg, vertex.label=V(net.bg)$name)


# plot 4 - copied from forum
l = layout.fruchterman.reingold(net, niter=5000, area=vcount(ig)^4*10)

plot(net, layout=l, 
     edge.arrow.size=0.5, 
     vertex.label.cex=0.75, 
     vertex.label.family="Helvetica",
     vertex.label.font=2,
     vertex.shape="circle", 
     vertex.size=1, 
     vertex.label.color="black", 
     edge.width=0.5)


#### Network analytics: 1.1 Who is the most connected? ####

# Questions:
# who is the most connected?

# compute node degrees
deg = degree(net, mode="all"); deg
degTable = as.data.frame(deg)

degTable$names = rownames(degTable); head(degTable)

# hist
library(ggplot2)
q = ggplot(degTable, aes(names, deg))
qq = q + geom_bar(stat = "identity")
qq + theme(axis.text.x = element_text(angle=90, hjust=1)) + coord_flip() + ggtitle("Who is the most connected?") + theme(plot.title = element_text(lineheight=.8, face="bold"))

# network graph weighted per degrees
net.bg = net

net.bg = simplify(net.bg, remove.multiple = T, remove.loops = T) # remove double connections

V(net.bg)$frame.color = "white"
V(net.bg)$color = "orange"
V(net.bg)$label = "" 
V(net.bg)$size = deg*2 # weighting the size per degree
V(net.bg)$label.cex=0.8
E(net.bg)$arrow.mode = 0
l = layout.fruchterman.reingold(net.bg)

# plot
plot(net.bg, vertex.label=V(net.bg)$name, 
     rescale=T, layout=l,  
     main="Media-Saturn Account Lunch Date Network", sub = "Bubble size is directly proportional to number of links.")


#### Network analytics: 1.2 Are there communities? ####

# define network
net.bg = simplify(net.bg, remove.multiple = T, remove.loops = T) # simplify

# ***************************************************************************

## TRY 1 - NOT WORKING

# detect communities
V(net.bg)$community = optimal.community(net.bg)$membership
colrs = adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)

# plot param
V(net.bg)$label = "" 
V(net.bg)$label.cex=0.8
E(net.bg)$arrow.mode = 0
l = layout.fruchterman.reingold(net.bg)

# plot
plot(net.bg, vertex.color=colrs[V(net.bg)$community])

# ***************************************************************************

## TRY 2 - use walktrap algo / OK

walktrapComm = walktrap.community(net.bg); walktrapComm # lauch algo

colors = rainbow(max(membership(walktrapComm))) # generate colors = n communities

# plot
plot(net.bg,vertex.color=colors[membership(walktrapComm)], 
     layout=layout.fruchterman.reingold, 
     edge.arrow.mode = 0, main = "Communities Identification using WalkTrap Algo")




#### Places analysis: Where people go to eat? ####

# calculate place frequencies
library(plyr)
places = count(links, 'place')

# wordcloud
library(wordcloud)
wordcloud(places$place, places$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"), main="Where Accenture goes mostly to eat?")

# ggplot
library(ggplot2)
q = ggplot(places, aes(place, freq))
qq = q + geom_bar(stat = "identity")
qq + theme(axis.text.x = element_text(angle=90, hjust=1)) + coord_flip()


#### Engagement: Who created more records? ####

# count
createdCount = count(data, 'createdBy')

# plot
q = ggplot(createdCount, aes(createdBy, freq))
qq = q + geom_bar(stat = "identity")
qq + theme(axis.text.x = element_text(angle=90, hjust=1)) + coord_flip()


#### (DEV) Time series analysis: Lunch dates ####

# table lunches per day - method 1
lunchesPerDay = data.frame(table(data$date)); lunchesPerDay

# table lunches per day - method 2
library(plyr)
lunchesPerDay_2 = count(data, 'date')

# time series
library("TTR")
lunches_ts = ts(lunchesPerDay)
plot.ts(lunches_ts,  main = "lunch dates time series")

# (1) ARIMA forecast
library("forecast")

# differentiate ts and auto.arima
lunches_ts.diff1 = diff(lunches_ts, differences=1)
auto.arima(lunches_ts.diff1)

# correlograms
par(mfrow = c(2, 1))
acf(lunches_ts.diff1, lag.max=20) # plot 
pacf(lunches_ts.diff1, lag.max=20) # plot 

# fit model
lunches_model.ar = arima(lunches_ts.diff1, order=c(1,0,0))

# apply model
lunches_prediction = forecast.Arima(lunches_model.ar, h = 5); lunches_prediction
par(mfrow = c(1, 1))
plot.forecast(lunches_prediction)

# (2) Exp smoothing
lunches_model.es = HoltWinters(lunches_ts, beta=F, gamma=F); lunches_model.es
plot(lunches_model.es)






