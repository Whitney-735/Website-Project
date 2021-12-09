GGally::ggpairs(WineData1, aes(color = Class, alpha = 0.5))
ggplot(data=WineData1,mapping = aes(x=Proline,y=`Color intensity`,color=Class))+
  geom_point(alpha=0.5)

ggplot(WineData1, aes(x = Class, y = Proline, color= Class))+
  geom_boxplot()+ 
  ggtitle("Class vs. Proline")

ggplot(WineData1, aes(x = Class, y = `Color intensity`, color= Class ))+
  geom_boxplot()+ 
  ggtitle("Class vs. Color Intensity")

summary(WineData1)
