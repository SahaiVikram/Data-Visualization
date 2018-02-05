library(dplyr)
library(ggplot2)
library(plotly)
kcc<-read.csv("C:/Users/Administrator/Desktop/Data Sets/KCC_July17_Kangra.csv")

#Sector-wise Bar Chart
qplot(kcc$Sector,geom="bar",fill=I("Green"),colour=I("Black"),xlab = "Sector"
      ,ylab ="No. of Queries",main="Sector-wise Call to KCC from Kangra(July-17)")

#Block-Wise Bar Chart
block_query<- kcc%>%group_by(BlockName)%>%summarise(count= length(BlockName))
a<- block_query$BlockName
b<- block_query$count
pct <- round(b/sum(b)*100)
lbls <- paste(a, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="")
pie(b,labels = lbls, col=rainbow(length(lbls)),main="Pie Chart wrt Blocks")

#Season-wise bar chart
par(las=2) #label text perpendicular to axis
counts <- table(kcc$Season)
barplot(counts, ,main="Crop-type call to KCC from Kangra(July-17)", horiz=TRUE,
        names.arg=c("Jayad", "Kharif", "Rabi"))

#Donut Chart for Categories
kcc %>%group_by(Category) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~Category, values = ~count) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Donut charts using Plotly",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Query Type using Line Plot
v<-kcc %>%group_by(QueryType) %>%summarize(count = n())
plot_ly(v, x = ~QueryType, y = ~count, type = 'scatter', mode = 'lines')

#Crop Type using Bar Plot
m<-kcc %>%group_by(Crop) %>%summarize(count = n())
plot_ly(m,x = ~Crop, y = ~count, name = "Crop",type = "bar")