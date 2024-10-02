library(shiny)
library(mwshiny)
library(vcd)
library(igraph)
library(shape)

####################################################################
# data and functions needed

#source("functions.R")

load("./data/box_profiles_20151217.Rds")
# box sequence is bbox; functional layout is tsne.unwt
# LINE layout is line_lout_12 = 100 cols level 2, 100 cols level 2

# the comms graph:
g <- graph_from_edgelist(as.matrix(edges[,1:2]))
V(g)$size <- 0.1 * log(1 + degree(g))
V(g)$label <- ""

nbyte <- nbyte[,2]
deg <- degree(g)
cex_factor <- 2

source("functions.R")

####################################################################

source("ui.R")
source("server.R")

####################################################################
# RUN ----

runApp(mwsApp(ui, server_calc, server_out),
       host = "192.168.1.221",
       launch.browser = FALSE)
