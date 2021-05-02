# importing libraries
library(igraph)
library(ggplot2)
library(ggraph)
library(readxl)
library(car)

# Getting data
data <- read_xlsx("MerchantRoute.xlsx")

# creating edge_list
start <- vector()
end <- vector()

# Matching surplus to sought_after
for (i in 1:dim(data)[1]){
        
        start[i] <- data$Outpost[i]
        end[i] <- data$Outpost[data$Surplus[i] == data$Sought_after]
        
        edge_list <- cbind.data.frame(start,end)
}

# plotting network from edge_list
merchant_graph <- graph.data.frame(edge_list,directed=TRUE)
plot(merchant_graph)

# coordinates for outposts (https://seaofthieves.fandom.com/wiki/Locations#Land_Locations)
locs <- c("F-7","D-10","J-18","Q-17","M-8","R-8","V-17")

# splitting coordinates
split <- strsplit(locs,"-")
x <- tolower(sapply(split,`[[`,1))
y <- as.numeric(sapply(split,`[[`,2))

# reverse coding y because the axis is inverted in the map
y <- (25 - y) # we need to take (ScaleMaximum+1) - value to reverse code

# translating letters to numbers from 1-26
for (i in 1:length(x)){
        
        x[i] <- which(letters == x[i])
}

# making x numeric
x <- as.numeric(x)

# rescaling coordinates to -1 to 1 range for igraph plotting
x <- scales::rescale(as.numeric(x), to = c(-1, 1))
y <- scales::rescale(as.numeric(y), to = c(-1, 1))

# combining location and names to metadata
meta <- cbind.data.frame(name = data$Outpost,x,y)

# creating layout from metainformation
location_graph <- graph.data.frame(edge_list, directed = TRUE, vertices = meta)
graph_layout <- layout.norm(data.matrix(meta[,2:3]))

# plotting graph (with commodities as edge names)
plot.igraph(location_graph, 
    layout = graph_layout,
    rescale=TRUE,
    edge.label = data$Surplus)

# improving on visuals
