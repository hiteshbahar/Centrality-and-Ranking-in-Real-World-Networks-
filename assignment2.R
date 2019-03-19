# Load Library
library(igraph)

# Read the Graphs 
politcalNetwork <- read_graph("polblogs.gml", format = "gml")
internetNetwork <- read_graph("internet.gml",format = "gml")
neuralNetwork <- read_graph("neuralnetwork.gml", format = "gml")


# Calculation of Degree, Eccentricity, Closeness, Betweeness, katz Index, 
#Page Rank,  Kleinberg's Authority score, and Kleinberg's Hub score

# Degree Maximum
maxDegree <- which.max(degree(politcalNetwork))
maxEccentricity <- which.max(eccentricity(politcalNetwork))
maxPageRank <- which.max(page_rank(politcalNetwork)$vector)

# Library for Katz index
library(centiserve)


# Question 2, 
er20 <- erdos.renyi.game(20,100,type ="gnm")
er40 <- erdos.renyi.game(40,120,type="gnm")


laplacian_matrix(er20)
graph.laplacian(er20)

df <- data.frame(Graph_Name = character(0),  
                 Nodes = numeric(0),
                 Edges = numeric(0),
                 Min_degree = numeric(0),
                 Max_degree = numeric(0),
                 Avg_path_length = numeric(0),
                 Diameter = numeric(0),
                 Global_coeff = numeric(0),
                 second_Eigen = numeric(0),
                 large_Eigen = numeric(0),
                 stringsAsFactors = FALSE)
df[nrow(df)+1,] <- c("Erdos-Renyi-20",
                     vcount(er20),
                     ecount(er20),
                     min(degree(er20)),
                     max(degree(er20)),
                     mean_distance(er20),
                     diameter(er20),
                     transitivity(er20, type= "globalundirected"),
                     sort(eigen(laplacian_matrix(er20))$values,decreasing = FALSE)[2],
                     sort(eigen(laplacian_matrix(er20))$values,decreasing = TRUE)[1])


ggplot(erdos_g, aes(x = x)) + 
  geom_line(aes(y = lambda2), colour="blue", size = 1.2, linetype="dashed") + 
  geom_line(aes(y = lambdaN), colour = "red", size = 1) + 
  ylab(label="Number of new members") + 
  xlab("Week")

plot(x,l, type = 'l')


ggplot(data = plotdfBB, aes( x = x_axis)) +
  geom_line( aes(y = lambda2))


ggplot(plotdfBB, aes(x=x_axis, y=lambda2, col=lambdaN)) + geom_line()


ggplot(barabasi_g, aes(x = x)) + 
  geom_line(aes(y = lambda2, colour = "lambda 2")) + 
  geom_line(aes(y = lambdaN, colour="lambda N")) + 
  scale_colour_manual("", 
                      breaks = c("lambda 2", "lambda N"),
                      values = c("red", "blue")) +
  xlab("Vertex ID") + ylab("Eigen Vector Value") + labs(title = "Barabasi graph: Eigen vector VS Vertex id")



ggplot(plotdfBB, aes(x =x)) +
  geom_line(aes(y = lambda2, colour ="lambda 2")) +
  geom_line(aes(y= lambdaN, colour="lambda N")) + 
  scale_color_manual("",
                     breaks = c("lambda 2", "lambda N"),
                     values = c("red", "blue")) 
