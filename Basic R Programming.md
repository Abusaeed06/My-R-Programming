# My R Programmie
 
#File saving and loading directory####

setwd("F:/R/Excel File")

getwd()

#BA=read.csv("F:/My course/R/6th Batch 25 July R Course/Biol_data.csv")

BA=read.csv("Biol_data.csv")     #My data set


#Simple Equations

#General Equation####

A=2  
B=3  
Y=A+B       
z=(A+B)/Y    
z

A=c(2,3,4)
B=c(1,1,1)
s=A-B
s



#Table Making

#Making Table#### 

A <-         c("Phytoplankton",  "Diatom", "Zooplankton") 
B    <-              c(1,-2,3)
C <-         c(22,12,46)

Table <-       data.frame(A,  B,  C) 

#revealing the data table as chart
View(Table)

#Checking Table####

str(BA)   #viewing the structure of a table

summary(Table)

dim(Table)
#Table Sorting

#sorting column####

attach(Table)

X <- Table[order(C),]
X     
Y <- Table[order(-B),]
Y

#Naming Column and Rows


#Naming Column and Rows####

colnames(Table) <- c("Groups","Summer","Winter")


rownames(Table) <- c("Station 1","Station 2","Station 3")


#Adding New Column

#Adding extra column####
Table$Autumn <- c(1.3,1.5,1.7)

View(Table)

#Adding extra Row####

LoLo <- Table

new_row <- c("Cyanobacteria",33,44,55)

LoLo[nrow(LoLo) + 1       ,     ] <- new_row

#NB: Table[   rowCommand       ,    ColCommand       ]

#Cutting table by Columns####

W <- Table[,c(2,3)]

#Cutting table by rows####
U <- Table[c(2,3),]

#Cutting both columns and rows
OO <- Table[c(1,2),c(2,3)]

#Deleting Columns####
#subset is indicating to the main table

E1= subset(Table, select = -c(Winter,Autumn) )

#separating column in new data table####
E2 = subset(Table, select = c(Winter,Autumn) )

#replace data####
Table[2,3] <- 111

#arranging columns

RR <- Table[,c(2,1,4,3)]

#Adding values in columns

Table$Total  <-  rowSums(Table[,c(2,3)],na.rm = T)
Table

#Average Column Values

Table$Average <-  rowMeans(Table[,c(2,3,4)],na.rm =  T)
Table

#Subtraction of Column Values

Table$Winter_Autumn = Table$Winter-Table$Autumn
Table

#default data loaded in R####
data()

#selecting random data set
View(mtcars) 


#Exporting table from R

#Exporting table from R as CSV format####

#write.csv(Table,"F:/My course/R/6th Batch 25 July R Course/Table.csv",row.names = FALSE)

write.csv(Table,"Table.csv", row.names = TRUE)

write.csv(Table,"Table.csv", row.names = FALSE)


#Exporting table from R as Excel format####

#install.packages("writexl")

library("writexl")

write_xlsx(Table,"Table.xlsx")


#Importing table in R

#Importing new table from computer (csv file) in R####

BA=read.csv("Biol_data.csv")


#Importing data by selecting files

#BA=read.csv(file.choose(T))  

View(BA)

#Data filtering
#install.packages("dplyr")
library(dplyr)

BC <- filter(BA,Stations=='A')
BU <- filter(BA,Depth==3)
BY <- filter(BA,Depth==c(1,4))

BP <- filter(BA,Stations==c('A','B')) #code will not properly work


#Install the package:
#devtools::install_github("rstudio/addinexamples", type = "source")
#install.packages("devtools")

GG <- filter(BA, Stations %in% c('A', 'B')) 

FR <- filter(BA, Latitude == 18.0)

#Average, Mean, Median, max-min (Range)
summary(BA)

#install.packages("vtable")
library(vtable)

st(BA[,6:9])

#install.packages("dplyr")
library(dplyr)

#pipe operator %>% =Alt + Shift + M

ZZ <- BA %>%
  group_by(Zones) %>% 
  summarise( 
    mean = mean(Phytoplankton),
    std = sd(Phytoplankton)
  )
ZZ

YY <- ZZ %>% 
  mutate_if(is.numeric, round,2)

YY$Average <- paste(ZZ$mean,"±",ZZ$std)

YY$Average <- paste(round(ZZ$mean,2),"±",round(ZZ$std,2))
#install.packages("table1")
library(table1)

table1::label(BA$Phytoplankton)
table1::label(BA$Diatoms)
table1::label(BA$Dinoflagilates)
table1::label(BA$Cyanobacteria)
table1::label(BA$Temperature)

table1::table1(~Phytoplankton+Diatoms+
                 Dinoflagilates+Cyanobacteria+
                 Temperature| Depth, data = BA)

table1::table1(~Phytoplankton+Diatoms+
                 Dinoflagilates+Cyanobacteria+
                 +Temperature| Zones, data = BA)

#Just Mean Table####

#Zone wise average
J1 <- aggregate(BA[,c(6:17)], list(BA$Zones), mean)

J2 <- aggregate(BA[,c(6:17)], list(BA$Zones), sd)

J3 <- aggregate(BA[,c(6:17)], list(BA$Stations), max)

J4 <- aggregate(BA[,c(6:17)], list(BA$Stations), min)

J5 <- aggregate(BA[,c(6:17)], list(BA$Zones), length)

J6 <- aggregate(BU[,1], list(BU$Zones), list)
#Bar plot####
#install.packages("ggplot2")
library(ggplot2)

#Barplot of numerics 
ggplot(BA, aes(Phytoplankton, Stations))+
  geom_bar(stat="identity") 


#Barplot of numerics 
BU <- filter(BA,Depth==3)

ggplot(BU, aes(Stations, Phytoplankton))+
  geom_bar(stat="identity")+
  
  geom_abline(intercept = 200)+
  
  
  geom_text(aes(label = Phytoplankton), vjust = 0)


#Barplot of groups
ggplot(BA, aes(Zones))+
  geom_bar()+theme_classic()

#removing background
theme_set(theme_classic())

EE <- ggplot(BA, 
             aes(Stations, Phytoplankton))+
  geom_bar(stat="identity", 
           width = 0.7, 
           fill="tomato2") +
  labs(title="Phytoplankton Abundances", 
       subtitle="Concentration in different stations", 
       caption="Source: Biol_data",
       y="Phytoplankton (cell/L)",
       x="Study Area/Name of Stations") +
  theme(axis.text.x = element_text(angle=65, vjust=0.5))

EE

#Segmented Bar plot with 1 group
EE+facet_wrap(~Depth)

#segmenting bar plot with 2 groups

EE + 
  facet_grid(BA$Zones~BA$Depth)+theme()


#Bar Plot+Error Bars

library(dplyr)
#Data filtering for single station names
BU <- filter(BA,Depth==3)

ggplot(BU,aes(Stations,Phytoplankton))+
  geom_bar(stat = "identity")+
  
  geom_errorbar(aes(x=Stations, 
                    ymin=Phytoplankton-sd(Phytoplankton), 
                    ymax=Phytoplankton+sd(Phytoplankton)),
                width=0.3, 
                colour="orange", 
                alpha=0.9, 
                size=0.3)


#responsive Bar plot####
#install.packages("plotly")
library(plotly)

library(dplyr)
#Data filtering for single station names
BU <- filter(BA,Depth==3)

plot_ly(
  x = BU$Stations,
  y = BU$Phytoplankton,
  name = "SF Zoo",
  type = "bar"
)

#More Bar plots in Plot_ly

#link: https://plotly.com/r/bar-charts/

#Multiple barplot with different variables

library(ggplot2)
#install.packages("reshape")
library(reshape)

WW <- BA[,c(1,6:9)]

PP <- melt(WW, id.vars = "Stations")

ggplot(PP, aes(Stations, value, fill = variable)) +
  
  
  geom_bar(stat="identity", position = "dodge") +
  
  
  labs(title="Multiple Bar plots")



#line plot with points####
library(ggplot2)

BU <- filter(BA,Depth==3)

ggplot(data=BU, aes(x=Stations, y=Phytoplankton, group=1)) +
  geom_line(color="red")+
  geom_point(color="green")

##Line graph+multiple variables
ggplot(BU, aes(x=Stations, group=1)) +
  
  geom_line(aes(y=Phytoplankton,color="Phytoplankton"))+
  geom_point(aes(y=Phytoplankton,color="Phytoplankton"))+
  
  geom_line(aes(y=Diatoms,color="Diatoms"))+
  geom_point(aes(y=Diatoms,color="Diatoms"))+
  
  geom_line(aes(y=Dinoflagilates,color="Dinoflagilates"))+
  geom_point(aes(y=Dinoflagilates,color="Dinoflagilates"))+
  
  labs(y="Density (Cells/L)")

#Multiple line graph in R####

library(dplyr)
#Data filtering for single station names
BU <- filter(BA,Depth==3)

#selecting data for multiple line
GF <- BU[,c(1,6:9)]

# install.packages("reshape")
library(reshape)
#arranging data in one table
BF <- melt(GF, id.vars = "Stations")

library(ggplot2)

#Viewing line graphs
ggplot(data=BF, 
       aes(x=Stations,
           y=value,
           color=variable,
           group=variable)) +
  geom_line()+
  geom_point()+
  
  facet_wrap(~variable)



#Bar plot + Line Graph ####
library(ggplot2)


ggplot(BU)  + 
  
  
  geom_bar(aes(x=Stations, y=Phytoplankton),
           stat="identity", 
           fill="gray",
           colour="black")+
  
  
  geom_line(aes(x=Stations, y=Phytoplankton,group=1),
            stat="identity",
            color="red",
            size=1)+
  
  labs(title= "Combined Graph",
       x="Stations",
       y="Phytoplankton")

#Dot plot as Bar style


ggplot(BA, aes(Stations, Phytoplankton))+ 
  geom_point(size=2,color="red")

#Stacked Bar Plot

# Stacked Bar Ploting####
ggplot(BA, 
       aes(fill=Depth, 
           y=Phytoplankton, 
           x=Stations)) + 
  geom_bar(position="stack", 
           stat="identity")

#Grouped Stacked Bar Plot

# Stacked
P <- ggplot(BA, 
            aes(fill=Zones, 
                y=Phytoplankton, 
                x=Stations)) + 
  geom_bar(position="stack", 
           stat="identity")
P

#Multiple Grouped Bar#### 
ggplot(BA, aes(fill=Zones, y=Phytoplankton, x=Stations)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Depth)

#non scientific Graph
ggplot(BA, aes(fill=Depth, y=Phytoplankton, x=Stations)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Zones)

# (%) Stacked Bar Plot

Q <- ggplot(BA, aes(fill=Depth, y=Phytoplankton, x=Stations)) + 
  geom_bar(position="fill", stat="identity")
Q



#Pie Plot

#Pie Plot####
R <- ggplot(BA, 
            aes(x = "", 
                y = Phytoplankton, 
                fill = Zones)) +
  geom_col() +
  coord_polar(theta = "y")+
  theme_void()
R




#Circular Barplot

ggplot(BA) +
  geom_bar(aes(x=Stations, y=Phytoplankton,fill=Depth),
           stat="identity") +
  ylim(-800,800)+
  coord_polar(start=0)+theme_void()

#Circular Grouped Bar
ggplot(BA) +
  geom_bar(aes(x=Stations, y=Phytoplankton,fill=Zones),
           stat="identity") +
  ylim(-800,800)+
  coord_polar(start=0)+theme_void()

#Multiple Graphs in same sheet

#Multiple Graphs in same sheet####
#install.packages("ggpubr")
library(ggpubr)

ggarrange(P, Q,
          ncol = 1, nrow = 2)


ggarrange(P, Q + font("x.text", size = 10),
          ncol = 2, nrow = 1)



#Horizontal Graph Arrangement


#Horizontal Arrangement####
ggarrange(P,Q,R,
          ncol = 1, nrow = 3,
          labels = c("(A)", "B", "C"))



#Vertical Graph Arrangement


#Vertical Arrangement####
ggarrange(P, R, Q, Q + 
            font("x.text", size = 8),
          ncol = 2, 
          nrow = 2, 
          labels = c("(A)", "(B)", "C","D"))




#Default -scatter plots####

#Cutting table in sub-table###  
BO <- BA[,c(-1:-5)]

BB <- BA[,c(6:17)]
#View(BB)
plot(BB)


#Scatter plots with Biological Parameters


#Scatter plot of Biological Parameters
BX=BB[,c(1:4)]
BX
plot(BX)


#Scatter plot of Environmental Parameters
BY=BB[,c(-1,-2,-3,-4)]

plot(BY)


#Single-scatterplot

#General linear regression
GG <- lm(Phytoplankton~Diatoms,BA)

#coefficient Value
GG  

#r-square Value
summary(GG)

#Single-scatterplot with linear regression####

#linear regression####
library(ggpubr)
library(ggplot2)

ggplot(BA,
       aes(x = Phytoplankton, 
           y = Diatoms)) + 
  
  geom_point(aes(color = Zones),
             size = 5, 
             alpha = 0.7) + 
  
  geom_smooth(aes(color = NULL),
              method = "lm", 
              formula = "y~x",
              se=T) +
  
  
  
  stat_cor(aes(label = paste(..rr.label.., 
                             ..p.label.., 
                             sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 0, 
           label.y = 375, 
           size = 3)



#Slinear Regression####
LR <- ggplot(BA,aes(x = NH4, 
                    y = Diatoms)) + 
  geom_point(aes(color = Zones),
             size = 5, 
             alpha = 0.7) + 
  geom_smooth(aes(color = NULL),
              method = "loess", 
              formula = "y~x",
              se=T) +
  stat_regline_equation(label.x = 2,
                        label.y = 400, 
                        aes(label = ..eq.label..)) +
  stat_regline_equation(label.x = 2,
                        label.y = 350, 
                        aes(label = ..rr.label..))

LR

#Zone-wise segmentation
LR+facet_wrap(~Zones)

#Depth-wise segmentation
LR+facet_wrap(~Depth)+ 
  
  labs(x="NH4 (ug/L)", 
       y="Diatoms (cells/L)", 
       title="Scatterplot", 
       subtitle="Phytoplankton Vs Diatoms",
       caption = "Source: Biol_data")


#Segmented Scatter plot####

#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggExtra")
library(ggExtra)
theme_set(theme_bw())  #pre-set the bw theme.

g <- ggplot(BA,aes(Phytoplankton,
                   Diatoms, 
                   color=Zones,
                   size=Depth))+
  geom_point(alpha=0.5)

#Viewing Graph
plot(g)

ggMarginal(g, type = "histogram", fill="Pink")

ggMarginal(g, type = "boxplot", fill="Pink")

#type = “density”, “histogram”, 
#type = “boxplot”, “violin”, “densigram”

#Multiple scatter plot


#Multiple scatter plot####
library(ggplot2)
#install.packages("tidyverse")

library(tidyverse)
library(modelr)
library(ggpubr)

#segmenting scatter plot with 2 groups

ggplot(BA, aes(Phytoplankton,
               Temperature, 
               colour = Depth,
               size=Phytoplankton)) + 
  geom_point() + 
  facet_grid(BA$Zones~BA$Depth)+
  theme()+
  stat_regline_equation(label.x = 0.3,
                        label.y = 400, 
                        aes(label = ..eq.label..)) +
  stat_regline_equation(label.x = 0.3,
                        label.y = 350, 
                        aes(label = ..rr.label..))


#Sized Scatter-Plot####
#non-scientific

GF <- ggplot(BA,
             aes(Zones,
                 Stations, 
                 color=Depth,
                 size=Phytoplankton))+
  geom_point(alpha=0.5)+
  theme_classic()

GF

GF+
  facet_wrap(~Depth)


#inverse Scatter Plot####

ggplot(BA,
       aes(Depth,
           Stations, 
           color=Zones,
           size=Phytoplankton))+
  geom_point(alpha=0.5)+
  theme_classic()

#NB: These are non-scientific Plot

#3d Scatter Plot####
#install.packages("tidyverse")
library(tidyverse)
#install.packages("rgl")
library(rgl)

plot3d(x=BA$Temperature,
       z=BA$Phytoplankton, 
       y=BA$Salinity,
       xlab="Temperature",
       ylab = "Salinity",
       zlab = "Phytoplankton",
       col=1:5,type="s",
       size =2)


rgl.snapshot("3dTS.png")


#Colored scatter Plot

smoothScatter(y=BA$Temperature,
              x=BA$Salinity,
              
              xlab = "Salinity",
              ylab = "Temperature", 
              main="T-S Diagram")    #main title

library(RColorBrewer)
library(ggplot2)

ggplot(data = BA, 
       aes(y=Temperature,
           x=Salinity)) +   
  stat_density2d(aes(fill = ..density..^0.15), 
                 geom = "tile", 
                 contour = F, 
                 n = 120) +   
  scale_fill_continuous(low = "yellow", 
                        high = "red")+
  geom_point(size=3,alpha=0.7)

#T-S diagram with zones####

library(ggplot2)

ggplot(BA,aes(x=Salinity,y=Temperature))+
  stat_density2d(geom="polygon",
                 aes(fill=Zones,
                     alpha = ..level..))+
  geom_point(aes(shape=Zones),
             color="black",
             size=2)+
  theme_bw()+
  scale_fill_manual(
    values=c("#ff0061","#10E6F0","#ffae00","#ff0100"))

#General BoxPlot

# BoxPlot####
library(ggplot2)

#Horozontal Boxplot



#Horozontal Boxplot####

ggplot(BA, 
       aes(Phytoplankton, 
           Stations))+
  geom_boxplot(varwidth=T, 
               fill="red") + 
  labs(title="Box plot", 
       subtitle="Phytoplankton grouped by Stations",
       caption="Source: Biol_data",
       x="Phytoplankton",
       y="Stations")



#Vertical boxplot
ggplot(BA, aes(Stations, 
               Phytoplankton))+
  geom_boxplot(varwidth=T, 
               fill="green") + 
  labs(title="Box plot", 
       subtitle="Phytoplankton grouped by Stations",
       caption="Source: Biol_data",
       y="Phytoplankton",
       x="Stations")+
  theme_classic()



#vertical boxplot with group####

#vertical boxplot with group 1
ggplot(BA,
       aes(Stations,
           Phytoplankton,
           fill=Zones))+
  geom_boxplot()

#vertical boxplot with group 2
ggplot(BA,
       aes(Stations,
           Phytoplankton))+
  geom_boxplot(aes(fill=Zones))

#Responsive Boxplot
#install.packages("plotly")
library(plotly)

plot_ly(
  x = BA$Phytoplankton,
  y = BA$Zones,
  name = "SF Zoo",
  type = "box"
)


#Dot Plot####

ggplot(BA,
       aes(Stations,
           Phytoplankton))+
  geom_dotplot(binaxis='y', 
               binwidth = 0.7,
               stackdir='center',   #up,down 
               dotsize = 22, 
               fill="red")

#Box Plot + Dot Plot

library(ggplot2)


# Box plot + Dot Plots####
ggplot(BA, 
       aes(Phytoplankton, 
           Stations))+
  
  geom_boxplot() +
  
  geom_dotplot(binaxis='y', binwidth = 0.7,
               stackdir='center', 
               dotsize = .5, 
               fill="red")  + 
  
  labs(title="Box plot + Dot plot", 
       subtitle="Phytoplankton samples Stations: 
       Each dot represents 1 row in source data",
       caption="Source: mpg",
       x="Phytoplankton",
       y="Stations")+
  
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        axis.text.y = element_text(angle=65, vjust=0.6))



#Segmented Boxplot#####
summary(BA)

BA$sT <- cut(BA$Temperature, 
             breaks=c(0,17,25,100),
             Right=F)

BA <- within(BA,{sT=NA
sT[Temperature<17]="Low Tempered"
sT[Temperature>=17 & Temperature<25]="Mid Tempered"
sT[Temperature>=25]="Tempered"
})

library(ggplot2)

ggplot(BA,
       aes(Zones,
           Phytoplankton,
           fill=sT))+
  geom_boxplot(outlier.colour="red") + 
  coord_cartesian(ylim = c(0, 500))+
  theme(axis.title.x = element_text(colour = "red"),
        axis.title.y = element_text(colour = "blue"),
        axis.title = element_text(colour = "#7F3D17"),
        panel.background = element_rect(fill='pink'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())


ggsave("Temp-Phyto.png") 


#=====================================
  
  #R-commander as SPSS####

#install.packages("Rcmdr")

library(Rcmdr)


#Correlations####


str(BA)
#Default Correlation Plot
BL=BA[,-c(1:5,18)]
BL
BD=cor(BL)
BD

#Decimal control in Table

BE <- round(BD,1)
BE

#Correlation Table with p and cor value

#Correlation Matrix Function
CorTable <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
#=============================================

#install.packages("Hmisc")

library(Hmisc)

#Data Cutting
CR <- BA[,c(6:9)]

#running correlation                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
CRR <-rcorr(as.matrix(CR))

# Extract the correlation coefficients
RR <- round(CRR$r,2)

# Extract p-values
PP <- round(CRR$P,2)

#Applying Cor Table function
Tab <- CorTable(RR, PP)

View(Tab)

#Merging Rows
Tab$Pairs_Correlated <- paste(Tab$row,"~",Tab$column)

#Arranging Rows
Lab <- Tab[,c(5,3,4)]

#Final Correlation Table
View(Lab)


#Simple correlation plots####


#install.packages("corrplot")
library(corrplot)

#Simple correlation plot####
corrplot(cor(BE),             #Correlation matrix
         method = "circle",   #Correlation plot method 
         type = "full",       #Correlation plot style type= full,lower,upper
         diag = FALSE,        #If TRUE (default), adds the diagonal
         tl.col = "black",    #Labels color
         bg = "white",        #Background color
         title = "",          #Main title
         col = NULL)          #Color palette 


#NB: method= "circle", "square", "ellipse", 
#"number", "shade", "pie", and "color".


#Awesome correlation plot####
#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(BE, 
           outline.color = "white",
           ggtheme = theme_bw(),
           colors = c("red", "whitesmoke", "steelblue4"),
           legend.title = "Correlation",
           lab = T,
           lab_col = "white",
           lab_size = 3, insig="blank",
           tl.cex = 8,
           tl.srt = 90,
           title = "Correlations among nutrients and 
           environmental Parameters") 
+
  theme(plot.title = element_text(hjust = 0.5, size=10), 
        legend.title = element_text(size = 10))

?colors


#Best correlation Plot#####

BF <- BE[1:4,5:12]
BF
#generating plots
ggcorrplot(BF, 
           outline.color = "white",
           ggtheme = theme_bw(),
           colors = c("Red", "whitesmoke", "steelblue4"),
           legend.title = "Correlation",
           lab = TRUE,
           lab_col = "white",
           lab_size = 3, insig="blank",
           tl.cex = 8,
           tl.srt = 90,
           title = "Nutrients vs Environmental parameters") +
  theme(plot.title = element_text(hjust = 0.5, size=10), 
        legend.title = element_text(size = 10))+ 
  coord_flip()


#Correlation Matrix####

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(BE, histogram=TRUE,
                  method = c("pearson", "kendall", "spearman"))



#pairs plot of Correlation#### 

#install.packages("mlr3") 
library(mlr3)
#install.packages("mlr3viz")
library(mlr3viz)
#install.packages("GGally") 
library(GGally)

#set the data
BT <- BA[,c(1,6,7,8,9)]

# get the task
task <-  as_task_regr(BT, 
                      target = "Phytoplankton", 
                      id = "Stations")

# subset task to only use the 3 first features
task$select(head(task$feature_names, 3))

autoplot(task, type = "pairs")

#Color-segmented Scatter/Correlation plot


#Color-segmented Scatter/Correlation plot####
#install.packages("mclust")
library(mclust)

BT <- BA[,c(6,7,8,9)]

BT <- clPairs(BA[,c(6,7,8,9)],
              class=BA$Zones,
              lower.panel=NULL)

clPairsLegend(0.1, 0.4, 
              class = BT$class, 
              col = BT$col, 
              pch = BT$pch, 
              title = "Zones")
#===================================
  
  # Lecture 7
  
  #PCA Ploting####

#install.packages("Factoextra",  "FactoMiner")

#remove libraries
#remove.packages(rlang)
#remove.packages(usethis)

# install libraries
#install.packages("rlang")
#install.packages("usethis")

# call libraries
library(devtools)
#install.packages("factoextra")

library(factoextra)
#install.packages("FactoMineR")
library(FactoMineR)


#data deriving

library(dplyr)
TT <- filter(BA,Depth==1)
BB <- TT[,c(12:17)]
row.names(BB) <- TT$Stations

#Creating Principle Component 
XX <- prcomp(BB, scale. = TRUE)
XX

#Ploting Sum of All components
plot(XX)


# see eigen values and variances

get_eig(XX)

# visualize eigenvalues and variance

fviz_screeplot(XX, addlabels = TRUE, ylim = c(0, 70))

#PCA Variables
var= get_pca_var(XX)
var

#PCA Graph with Arrows

fviz_pca_var(XX,
             col.var="contrib", 
             gradient.cols=c("darkgreen","red","blue","sienna2"),
             repel=T)



#PCA with Samples

fviz_pca_ind(XX,
             col.ind="contrib", 
             gradient.cols=c("darkgreen","red","sienna2"))





#PCA with all parameters

fviz_pca_biplot(XX,
                col.var="contrib",
                col.ind="contrib", 
                geom=c("point","text"), 
                gradient.cols=c("darkgreen","red","sienna2"))+
  ylim(-3,3)+
  xlim(-3,3)


#PCA with circles

fviz_pca_ind(XX, 
             label="all",           #lebel="none"
             habillage=TT$Zones,
             addEllipses=T, 
             ellipse.level=0.95,
             geom=c("point","text"))


#sample less than 4
#install.packages("ggforce")
library(ggforce)

fviz_pca_ind(XX, 
             label="all",           #lebel="none"
             habillage=TT$Zones,
             repel = TRUE, 
             # Don't use default Ellipses!!!!
             # addEllipses = TRUE,
             invisible='quali')+
  ggforce::geom_mark_ellipse(aes(fill = Groups,
                                 color = Groups)) +
  theme(legend.position = 'right') +
  coord_equal()


#Dendrogram/Cluster Analysis


#Dendrogram/Cluster Analysis####

#install.packages("ggdendro")
library(ggdendro)
BA=read.csv("Biol_data.csv")
View(BA)

library(dplyr)
BT <- filter(BA,Depth=='1')

BX <- BT[,-c(1:5,10:17)]
View(BX)

#giving Row names    
rownames(BX) <- BT[,1] #similar number of row is needed

hT <- as.dendrogram(hclust(dist(BX),"ave"))


plot(hT)

abline(h = 150, lty=2)


#colorful dendrogram####
library(factoextra)
library(ggplot2)

fviz_dend(hT,
          cex = 0.8, 
          lwd = 0.8, 
          k = 5,                      #Group Segmentation
          k_colors = "jco",
          rect = TRUE, 
          rect_border = "jco", 
          rect_fill = TRUE,
          color_labels_by_k = TRUE,
          xlab="Segmented Groups",
          ylab = "Distance", 
          main = "Cluster Dendrogram",
          sub = "",
          ggtheme = theme_classic(),     # Change theme
          horiz = TRUE) #rotate the graph 

#More dendrogram


#More dendrogram is here:(1) http://ow.ly/D4jW30rUOgq
# (2) :https://www.statmethods.net/advstats/cluster.html


#===========================
  
#Heatmap

#Heatmap with New Data#####

#Preparing Data

BA=read.csv("Biol_data.csv")

library(dplyr)
BT <- filter(BA,Depth=='1')
BB <- BT[,6:9]

MX = as.matrix(BB)

View(MX)

rownames(MX)=BT[,1]

#install.packages("pheatmap")
library(pheatmap)

pheatmap(MX) #normal Heatmap


#Gorgeous Heatmap####

library(tidyr)
library(RColorBrewer)
install.packages("SmarterPoland")
library(SmarterPoland)

MX_scale = scale(MX)

pheatmap(MX_scale)

pheatmap(MX_scale,
         border="white",
         color = brewer.pal(5,"Set3"),
         main="Plankton Abundances")

#segmenting Heat map row and columns####

#Preparing Data

BA=read.csv("Biol_data.csv")

library(dplyr)
BT <- filter(BA,Depth=='1')
BB <- BT[,6:9]

MX = as.matrix(BB)

View(MX)

rownames(MX)=BT[,1]

MX_scale = scale(MX)

#For group segmentation of row
BL <- BT[,c(1,4,6:9)]
View(BL)

SR = data.frame("Zones" = BL$Zones)

rownames(SR) = rownames(MX) #Row name matching

#For group segmentation of column
SC = data.frame(
  Plankton = factor(rep(c( "Macro","Micro"), c(2,2))))

rownames(SC) = colnames(MX) #Column name matching

pheatmap(MX_scale,
         color = brewer.pal(3,"Greens"),
         display_numbers =TRUE,    #display_numbers=TRUE/FALSE
         annotation_col = SC,
         annotation_row = SR,
         main = "Multivariate Clustering",
         cutree_cols = 2,
         cutree_rows = 4)



#More Heatmaps


#https://towardsdatascience.com/pheatmap-draws-pretty-heatmaps-483dab9a3cc




#Density Plot#####

#2D density Plot

library(mclust)
#2D density of Single Parameter####

BP <- densityMclust(BA$Phytoplankton)

summary(BP)

plot(BP,
     what = "density",
     data = BA$Phytoplankton,
     breaks = 68,
     xlab = "Phytoplankton",
     ylab = "Density",
     title("Cell count density"))

#Awesome Density Curve
library(ggplot2)

ggplot(BA,
       aes(x=Phytoplankton,
           fill="red"))+
  geom_density()

#Grouped Density Plot####

ggplot(BA, aes(Phytoplankton))+
  geom_density(aes(fill=factor(Depth)),
               alpha=0.7) +
  labs(title="Density plot",
       subtitle="Phytoplankton Grouped by Water Depths",
       caption="Source: Biol_data",
       x="Phytoplankton",
       fill="Depths")


#lyred-Grouped Density Plot####

#installed.packages("ggridges")
library(ggridges)
ggplot(BA,
       aes(Phytoplankton,
           Zones,
           fill=Zones,
           color=Zones))+
  geom_density_ridges(alpha=0.5)+
  theme_classic()



#3D Model Based Density Clustering


#3D Model Based Density Clustering####
library(mclust)
#Density Cluster####

BM <- BA[,c(6,10)]
BR <- densityMclust(BM)
summary(BR)
plot(BR, what = "density", type = "persp")

#3D Density-Plot####
library(plotly)

dens <- with(BA, tapply(Phytoplankton,
                        INDEX = Zones,
                        density))

data <- data.frame(
  x = unlist(lapply(dens, "[[", "x")),
  y = unlist(lapply(dens, "[[", "y")),
  Zones = rep(names(dens),
              each = length(dens[[1]]$x)))


plot_ly(data,
        x = ~Zones,
        y = ~x,
        z = ~y,
        type = 'scatter3d',
        mode='lines',
        color = ~Zones)

plot_ly              

#In RStudio, I went to [Tools/GlobalOptions.../Advanced] and
#choosing 'Desktop OpenGL' in the Rendering engine.
#Then Click apply and OK


#==========================

#Marginal Distribution Plot####


#SLinear Regression with Marginal Distribution Plot

#devtools::install_github("jtlandis/ggside")

library(ggside)
library(tidyverse)
library(tidyquant)
library(ggpubr)

ggplot(BA,
       aes(x = Phytoplankton,
           y = Temperature)) +
  geom_point(aes(color = Zones),
             size = 5,
             alpha = 0.7) +
  geom_smooth(aes(color = NULL),
              method = "loess",
              formula = "y~x",
              se=T) +
  stat_regline_equation(label.y = 40, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 35, aes(label = ..rr.label..)) +
  
  geom_xsidedensity(
    aes(
      y    = after_stat(density),
      fill = Zones
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  geom_ysidedensity(
    aes(
      x    = after_stat(density),
      fill = Zones
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Temperature derived Phytoplankton concentrations" ,
       subtitle = "Cell density",
       x = "Phytoplankton", y = "Temperature") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        
        ggside.panel.scale.x = 0.4,
        ggside.panel.scale.y = 0.4
  )



#Depth-wise regression

#Depth-wise regression####
library(ggside)
library(tidyquant)
library(tidyverse)

BA %>%
  ggplot(aes(x = Phytoplankton,
             y = Temperature,
             color = Zones)) +
  #creating scatter plot
  geom_point() +
  #Creating regression line
  geom_smooth(aes(color = NULL)) +
  #creating box-plot
  geom_xsideboxplot(
    alpha    = 0.5,
    size     = 0.2
  ) +
  #segmenting the scatter plot
  facet_grid(cols = vars(Depth), scales = "free_x") +
  #applying theme
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(
    title = "Depth segmented Regressions"
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        ggside.panel.scale.x = 0.4
  )


#RainCloud Plots


#RAINCLOUD PLOTS (Box+Density)####

#Very powerful for visualizing modality of distributions
#install.packages("ggdist")

library(ggdist)
library(tidyquant)
library(tidyverse)

# Pipe operator %>%  = Ctrl + Shift + m

BA %>%
  filter(Zones %in% c("Hot","Cold",
                      "Dry","Rainy")) %>%
  ggplot(aes(x = factor(Zones),
             y = Diatoms,
             fill = factor(Zones))) +
  
  #add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    # custom bandwidth
    adjust = 0.5,
    # move geom to the right
    justification = -.2,
    # remove slab interval
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = .12,
    # remove outliers
    outlier.color = NA,
    alpha = 0.5
  ) +
  #Add dot plots from {ggdist} package
  ggdist::stat_dots(
    # orientation to the left
    side = "left",
    # move geom to the left
    justification = 1.1,
    # adjust grouping (binning) of observations
    binwidth = .25,
    size= 0.3
  ) +
  coord_flip()+
  
  #Adjust theme
  scale_fill_tq() +
  theme_tq() +
  labs(
    title = "Raincloud Plot Comparision",
    subtitle = "Diatom Distribution in different zones",
    x = "Sampling Zones",
    y = "Diatoms (Cell/L)",
    fill = "Zones"
  )



#Linear regression Models


#Linear regression Models####


#install.packages("performance", dependencies = TRUE)
#Installs all dependencies
#install.packages("qqplotr")

library(qqplotr)
library(tidyverse)
library(performance)



#Performance Plot/model


#Performance Plot####

model_lm <- lm(Phytoplankton ~ Zones+Depth, data = BA)

model_lm

summary(model_lm)

#save the plot in size 1000*900 pixel

RP <- check_model(model_lm)

RP

#more: https://www.scribbr.com/statistics/linear-regression-in-r/



#Flow-chart/Trees


#Flow-chart####

#install.packages("pacman")
library(pacman)
#install.packages("DiagrammeR")
library(DiagrammeR)
#install.packages("tidyverse")
library(tidyverse)
# All instructions are within a large character string

grViz("
digraph surveillance_diagram {    

# 'digraph' means 'directional graph', then the graph name  

# graph statement

  graph [layout = dot,
         rankdir = TB,  #chart direction= BT,LR,RL
         overlap = true,
         fontsize = 10]
 
  #nodes
 
  node [shape = circle,           #shape = circle
       fixedsize = true
       width = 1.3]               #width of circles
 
  Plankton                        #names of nodes
  Phytoplankton
  Zooplankton
  Diatom
  Dinoflagillates
  Cyanobacteria
  Tintinid
  Radiolaria
  Protozoa
  Microplankton
  Macroplankton  

 #relation of circles with one another
 
  Plankton   -> Phytoplankton [label = ' Flora']
  Plankton   -> Zooplankton [label = ' Fauna']
  Phytoplankton -> Diatom
  Phytoplankton -> Dinoflagillates
  Phytoplankton -> Cyanobacteria
  Zooplankton -> Tintinid
  Zooplankton -> Radiolaria
  Zooplankton -> Protozoa  
  Microplankton -> Diatom
  Microplankton -> Dinoflagillates
  Microplankton -> Cyanobacteria
  Macroplankton -> Tintinid
  Macroplankton -> Radiolaria
  Macroplankton -> Protozoa
}
")



#Diverging Charts####

library(ggplot2)
library(dplyr)

BU <- filter(BA,Depth==1)

BG <- BU[,6:9]

#Compute normalized Phytoplankton


BG$Phyto <- round(
  (BG$Phytoplankton -
     mean(BG$Phytoplankton))/sd(BG$Phytoplankton),2)
# above / below avg flag

BG$Segment <- ifelse(BG$Phyto < 0, "below", "above")

# Diverging Bar-charts####

ggplot(BG,
       aes(x=BU$Stations,
           y=Phyto,
           label=Phyto))+
  geom_bar(stat='identity',
           aes(fill=Segment), width=.5)+
  scale_fill_manual(name="Cell Density",
                    labels = c("Above Average", "Below Average"),    
                    values = c("above"="#00ba38", "below"="#f8766d")) +
  labs(subtitle="Phytoplankton cell distribution in different stations",  
       title= "Diverging Bars",
       x="Stations",
       y="Phytoplankton") +
  coord_flip()+
  ylim(-2,2)



#Diverging Lilipop-charts####

#sort

BG <- BG[order(BG$Phyto),]

# convert to factor to retain sorted order in plot

BG$`Stations` <- factor(BU$`Stations`,
                        levels = BU$`Stations`)


ggplot(BG, aes(x=`Stations`, y=Phyto, label=Phyto)) +
  geom_point(stat='identity', fill="black", size=6) +
  geom_segment(aes(y = 0,
                   x = `Stations`,
                   yend = Phyto,
                   xend = `Stations`),
               color = "black") +
  geom_text(color="white", size=2) +
  labs(title="Diverging Lollipop Chart",
       subtitle="Phytoplankton cell distribution in different stations") +
  ylim(-2.5, 2.5) +
  coord_flip()



#Diverging Dotplot


#Diverging Dotplot####
library(ggplot2)

ggplot(BG, aes
       (x=`Stations`, y=Phyto, label=Phyto)) +
  geom_point(stat='identity', aes(col=Segment), size=6) +
  scale_color_manual(name="Cell Density",
                     labels = c("Above Average", "Below Average"),
                     values = c("above"="#00ba38", "below"="#f8766d")) +
  geom_text(color="white", size=2) +
  labs(title="Diverging Dotplot Chart",
       subtitle="Phytoplankton cell distribution in different stations") +
  ylim(-2.5, 2.5) +
  coord_flip()


#======================================
  

#Ven Diagram


#Ven Diagram of different Group####
#(distribution/diversity)                
#install.packages("ggvenn")
library(ggvenn)

ListeVenn=list(Phyto=BA$Phytoplankton,
               Diat=BA$Diatoms,
               Dinof=BA$Dinoflagilates,
               CyB=BA$Cyanobacteria)

#diversity similarity
ggvenn(ListeVenn)



#Sunburst plot


#Sunburst plot####

library(ggplot2)

#Making all numeric data as character for grouping
G <- as.character(BA$Depth)

ggplot(BA,
       aes(y=Phytoplankton)) +
  geom_bar(aes(fill=Zones,
               x=.10),
           width=.20, stat='identity') +
  geom_bar(aes(fill=G,
               x=.15),
           width=.15, stat='identity') +
  coord_polar(theta='y')+
  theme_minimal()


#Polar Sunburst plot

ggplot(BA, aes(x = Zones,
               y = Zones,
               fill = Phytoplankton)) +
  geom_tile() +
  coord_polar()+
  theme_minimal()


#Word Cloud####

Pet=read.csv("Pet.csv")
Pet
library(tidyverse)
library(dplyr)
#install.packages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud2)
library(wordcloud2)
#install.packages("Rcpp)
library(Rcpp)

#gathering all data in a single column

Animal <- aggregate(c(n1,n2,n3,n4) ~
                      c(s1,s2,s3,s4), Pet, mean )

colnames(Animal) <- c("Name", "Number")
Animal

#Running wordcloud

wordcloud(words = Animal$Name,
          freq = Animal$Number,
          min.freq = 1,          
          max.words=40,
          random.order=T,
          rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))

wordcloud2(data=Animal, size=0.6, color='random-dark')

wordcloud2(data=Animal, size = 0.3, shape = 'pentagon')

wordcloud2(Animal, color = "random-light", backgroundColor = "black")

wordcloud2(Animal,
           minRotation = -pi/6,
           maxRotation = -pi/6,
           minSize = 10,
           rotateRatio = 1)


#Mapping in r####

#Normal Contour Map


#Contour Map####

#install.packages("latticeExtra")
library(latticeExtra)

summary(BA)

levelplot(Phytoplankton ~ Latitude * Depth,BA,
          contour=T,
          xlim=c(2,18),
          ylim=c(4,1),
          col.regions=topo.colors,
          at=seq(from=0,to=500,length=70),
          panel=panel.2dsmoother,
          xlab="Latitude",
          ylab="Depth",
          main="Depth wise Phytoplankton Density",
          sub="Bay of Bengal")


#for different colors,
#col.regions=heat.colors,cm.colors,terrain.colors,
#col.regions=topo.colors,rainbow



#Beautiful contour map


#more beautiful contour map

library(plotly)

plot_ly(data = BA,
        x=~Latitude,
        y=~Depth,
        z=~Phytoplankton,
        type = "contour",
        colorscale='Greens')

#normal geographical Map

#install.packages("ggplot2")  only do this once
#install.packages("tidyverse")only do this once
#install.packages("maps")
library(ggplot2)             #needs to be done each r session
library(tidyverse)           #needs to be done each r session
library(maps)


region=c("India","Pakistan","Bangladesh")
People=c(77,44,17)
Map <- data.frame(region, People)
Map


mapdata <- map_data("world") #ggplot2
View(mapdata)

mapdata1 <- left_join(mapdata, Map, by="region")
View(mapdata1)


mapdata2<-mapdata1 %>% filter(!is.na(mapdata1$People))
View(mapdata2)


map1<-ggplot(mapdata2,
             aes( x = long,
                  y = lat,
                  group=group)) +
  geom_polygon(aes(fill = People),
               color = "red")

#Viewing Map
map1

#Color Map

map2 <- map1 + scale_fill_gradient(name = "People",
                                   low = "green",
                                   high =  "red",
                                   na.value = "grey50")
map2
#clearing axis and background
map2+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank())




#Map with Pie chart


#Map with Pie chart####

#install.packages("reshape")
#install.packages("rworldmap")
#install.packages("rworldxtra")
library(reshape)
library(rworldmap)
library(rworldxtra)
library(mapplots)


library(dplyr)
BB <- filter(BA,Depth=='1')

summary(BB)

mapPies(dF = BB,
        nameX="Longitude",
        nameY="Latitude",
        nameZs =c('Phytoplankton', 'Diatoms', 'Dinoflagilates'),
        zColours=c("lightblue", "Red", "lightgrey"),
        symbolSize = 3,
        addCatLegend = T,
        lwd=3,
        addSizeLegend=T,
        oceanCol = "#38F3FC",borderCol = "Black",main = "Diversity",
        landCol = "grey",
        xlim = c(125,135),
        ylim = c(0,20))+
  
  title(main=paste("Plankton Concentrations"),
        cex=3)+
  
  mtext(c("Longitude", "Latitude"), side=c(1,2), line = 2.5,col="blue")

#More Maps


#more map####

#Mapping Bangladesh in r: https://rpubs.com/asrafur_ashiq/map_of_bangladesh

#https://www.molecularecologist.com/2012/09/18/making-maps-with-r/

#color "#" codes = https://htmlcolorcodes.com/


#==================Bonus=========================####

#Cleaning Data


#removing or Cleaning Previous data from environment####  

rm(list=ls())



##Update the moved packages####

#Run the following command in R. Type ‘y’ for every question that popped up.

#---------------update.packages(checkBuilt=TRUE)

#Type the following command in R to check if everything went well

version

packageStatus()

#updating R

#-------------install.packages("installr")

#-------------library(installr)

#-------------updateR()


#More R Graphs

library(Rcmdr)

#Run this command in Main R console

#Helpful website#
#https://github.com/business-science/free_r_tips
#=============================================#




#Colorful Complex Table


#Colorful Complex table####
#Install the relevant libraries - do this one time

#install.packages("data.table")
#install.packages("dplyr")
#install.packages("formattable")
#install.packages("tidyr")

#Load the libraries
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)


#Set a few color variables to make our table more visually appealing

green1 = "#665D1E"
green2 = "#71CA97"
Red1 = "#ff7f7f"
Red2 = "#FF6347"

#For improvement formatter add icons
# Up and down arrow with greater than comparison from the vignette

UpDown_Arrow <-
  formatter("span",
            style = x ~ style
            (font.weight = "bold",
              color = ifelse(x>0,green1,
                             ifelse(x<0,Red1,"black"))),
            x~icontext(ifelse(x>0,"arrow-up","arrow-down"),x))

#Making final table

formattable(Table, align =c("l","c","r","r"),
            list(
              `Indicator Name` = formatter("span",
                                           style = ~ style(color = "grey",
                                                           font.weight = "bold")),
              `Groups`= color_tile(Red1,Red2),
              `Winter` = color_bar(Red1),
              `Summer` = UpDown_Arrow,
              `Autumn`= color_tile(green1,green2)
            ))



#============================

