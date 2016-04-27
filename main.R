
# loading data
data = read.csv("./data.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
col_names = c("createdBy","name1","name2","date","place","itemType","path")
colnames(data) = col_names; colnames(data)

#### First tests: Data prepp ####

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
nodes = cbind(1:dim(nodes)[1], nodes); colnames(nodes) = c("id","nodesName"); nodes

# links
links = data[, c("name1", "name2", "date", "place")]

# plot the network
library(igraph)
net = graph.data.frame(links, directed = T, vertices = nodes)
plot(net)
