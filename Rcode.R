

## Assignment2: Multidimensional scaling of a high-dimensional dataset
library(tidyr)
library(dplyr)
library(readxl)
library(plotly)
library(MASS)

# Part 1
baseball = read_excel("baseball-2016.xlsx",)


# Part 2
baseball.numeric= scale(baseball[,3:28])
d=dist(baseball.numeric, method = "minkowski", p=2)
res=isoMDS(d,k=2)
coords=res$points
coordsMDS=as.data.frame(coords)
coordsMDS$League=baseball$League
coordsMDS$name=baseball$Team
custom_colors <- c("red", "blue")
plot_ly(coordsMDS, x=~V1, y=~V2, color= ~League,type = "scatter",hovertext=~name,colors = custom_colors, mode = "markers")


sh <- Shepard(d, coords)
delta <-as.numeric(d)
D<- as.numeric(dist(coords))

n=nrow(coords)
index=matrix(1:n, nrow=n, ncol=n)
index1=as.numeric(index[lower.tri(index)])

n=nrow(coords)
index=matrix(1:n, nrow=n, ncol=n, byrow = T)
index2=as.numeric(index[lower.tri(index)])


plot_ly()%>%
  add_markers(x=~delta, y=~D, hoverinfo = 'text',
              text = ~paste('Obj1: ', coordsMDS$name[index1],
                            '<br> Obj 2: ', coordsMDS$name[index2]))%>%
  #if nonmetric MDS inolved
  add_lines(x=~sh$x, y=~sh$yf)



Baseball_new<-as.data.frame(baseball[,3:28])
Baseball_new$V2=coordsMDS$V2

# The strongest positive connection
p1 <- ggplot(Baseball_new)+aes(x=Baseball_new[,10],y=V2)+
  geom_point(color = "darkblue")+
  labs(x = colnames(Baseball_new)[10],y="V2", title ="Scatterplot:Strongest Positive Connection between Variables")
p1

# The strongest negative connection
p2 <- ggplot(Baseball_new)+aes(x=Baseball_new[,23],y=V2)+
  geom_point(color = "darkgreen")+
  labs(x = colnames(Baseball_new)[23],y="V2", title ="Scatterplot:Strongest Negative Connection between Variables")
p2