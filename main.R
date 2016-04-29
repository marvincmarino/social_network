
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

#### Plot the network ####
library(igraph)

# type 0: plain vanilla
net = graph.data.frame(links, directed = T, vertices = nodes)
plot(net)

# improve dataviz: some plot for tests

# type 1 - normal improved
plot(net, edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$nodesName, vertex.label.color="black", 
     main="Network",
     vertex.size=0.1, vertex.label.cex=0.8) 

# type 2
plot(net, vertex.shape="none", vertex.label=V(net)$nodesName, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")

# type 3 - customised with different layouts
net.bg = net

net.bg = simplify(net.bg, remove.multiple = T, remove.loops = T) # simplify

V(net.bg)$frame.color = "white"
V(net.bg)$color = "orange"
V(net.bg)$label = "" 
V(net.bg)$size = 0.1
V(net.bg)$label.cex=0.8
E(net.bg)$arrow.mode = 0

# select one layout "l"
l = layout.circle(net.bg)
l = layout.kamada.kawai(net.bg)
l = layout.fruchterman.reingold(net.bg)

# plot
plot(net.bg, vertex.label=V(net.bg)$name, 
     rescale=T, layout=l)

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


#### Places analysis: Where people go to eat? ####

# need to calculate the place frequencies
library(plyr)
places = count(links, 'place')

# wordcloud
library(wordcloud)
wordcloud(places$place, places$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"), max.words=100)

# ggplot
library(ggplot2)
q = ggplot(places, aes(place, freq))
qq = q + geom_bar(stat = "identity")
qq + theme(axis.text.x = element_text(angle=90, hjust=1)) + coord_flip()


#### Engagement: Who created more records? ####
createdCount = count(data, 'createdBy')
q = ggplot(createdCount, aes(createdBy, freq))
qq = q + geom_bar(stat = "identity")
qq + theme(axis.text.x = element_text(angle=90, hjust=1)) + coord_flip()
