### Function Set Workspace ###
set_workspace <- function(dir_path){
  
  dir.create(dir_path) # Create Directory
  setwd(dir_path) #Set Working Directory
  print(paste("Working Directory Set to : ",dir_path)) # Print Message
}

### Function To Check/Install/Load Package ###
load_package <- function(package_name) {
  
  # Check If Package Is Installed
  if(is.element(package_name, installed.packages()[,1])){ 
    library (package_name, character.only=TRUE)  # If Yes Load Package                           
  }
  else {                        
    install.packages(package_name) # Install Package
    library (package_name, character.only=TRUE)  # Load Package  
  }
}

### Set Workspace ###
set_workspace("C:/Users/dasani.a/Desktop/RCom");

### Load Requied Libraries ###
load_package('dplyr')
load_package('sqldf')
load_package('ggplot2')
load_package('ggthemes')
load_package('gridExtra')
load_package('grid')
load_package('radarchart')
load_package('ellipse')

### Load CSV Files ###
tbl_projects_user <- read.csv("tbl_projects_user.csv")

### Question Number One ###

from <- 2007
to <- 2012
data <- sqldf(paste("SELECT year AS x, sum(favorites_count) / 1000 AS y1, sum(followed_count) / 1000 AS y2 FROM tbl_projects_user ",
                    "WHERE year >= " , from ," AND year <= ", to ," GROUP BY year"))

# Create the top plot 
g.top <- ggplot(data, aes(x = x, y = y1, fill=y1)) +
  geom_bar(stat = "identity",fill="darkgrey",color="black") +
  theme_bw() +
  theme(plot.margin = unit(c(1,5,-30,6),units="points"),
        axis.title.y = element_text(vjust =0.25)) +
  labs(y = "Favorited By (* 1000)") + 
  ggtitle("Favorited By & Followed By Vs Years")

# Create the bottome plot 
g.bottom <- ggplot(data, aes(x = x, y = y2)) +
  geom_bar(stat = "identity",fill="darkgrey",color="black") +
  theme_bw() +
  theme(plot.margin = unit(c(0,5,1,1),units="points")) + 
  scale_x_continuous(breaks=seq(2007,2012,1)) +
  labs(x = "Years", y = "Followed By (* 1000)")

# Plot graphs and set relative heights
grid.arrange(g.top,g.bottom, heights = c(1/2, 1/2)) 

### Question Number Two ###

names(tbl_projects_user)

# Number of Projects Per Year
sqldf("SELECT country, count(country) AS country_count
      FROM tbl_projects_user GROUP BY country")

from <- 2007
to <- 2012

data <- sqldf(paste("SELECT Country, avg(followed_count) AS Followed, avg(favorites_count) AS Favorites,",
                    "avg(download_count) AS Downloads, avg(viewers_count) AS Viewers FROM tbl_projects_user ",
                    "WHERE country != 'NA' AND COUNTRY !='' AND year >= " , from ," AND year <= " , to ,
                    " GROUP BY country ORDER BY Followed desc, Favorites desc LIMIT 15"))

data$Followed = data$Followed/max(data$Followed)
data$Favorites = data$Favorites/max(data$Favorites)
data$Downloads = data$Downloads/max(data$Downloads)
data$Viewers = data$Viewers/max(data$Viewers)

chartJSRadar(data)

### Question number 3 ###

### Correlation Matrix ###

names(tbl_projects_user)

from <- 2007
to <- 2012

data <- sqldf(paste("SELECT sprites_website AS Sprites, scripts_website AS Scripts, images AS Images, sounds AS Sounds, 
                    favorites_count AS Favorites, followed_count AS Followed, download_count AS Download, 
                    viewers_count  AS Viewers FROM tbl_projects_user ",
                    "WHERE sprites_website != 'NA' AND scripts_website != 'NA' AND images != 'NA' AND ",
                    "sounds != 'NA' AND favorites_count != 'NA' AND followed_count != 'NA' AND ",
                    "download_count != 'NA' AND viewers_count != 'NA' AND " , from ," AND year <= " , to ))

head(data)
dim(data)

#data<-transform(data, Sprites = as.numeric(sprites_website), Scripts = as.numeric(scripts_website),
#                    Images = as.numeric(images), Sounds = as.numeric(sounds), Favorites = as.numeric(favorites_count),
#                    Followed = as.numeric(followed_count),Downloads = as.numeric(download_count), Viewers = as.numeric(viewers_count))

corMatrix<-cor(data)

cols <- ifelse(corMatrix>0, rgb(0,0,abs(corMatrix)), rgb(abs(corMatrix),0,0))
plotcorr(corMatrix,col=cols,mar=c(0,0,0,0))

n <- nrow(corMatrix)
for (i in 1:n)
{
  for (j in 1:n)
  {
    text(j,i,round(corMatrix[n-i+1,j],2),col="white",cex=0.6)     
  }
}
