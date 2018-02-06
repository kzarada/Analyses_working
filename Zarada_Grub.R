#########################################################
# Grub box data
# K. Zarada
# February 2018
#########################################################

### Load packages ###
library(tidyverse)
library(reshape2)
library(ggplot2)
library(Hmisc) 
library(corrplot)

### Load Data ###
weight = read.csv("/Users/katiezarada/Desktop/Oysters/Data/Grub_box_oyster_size_weight_wet_dry.csv", header = TRUE)
bulk = read.csv("/Users/katiezarada/Desktop/Oysters/Data/Grub_box_shell_bulk_weights.csv", header = TRUE)
surface = read.csv("/Users/katiezarada/Desktop/Oysters/Data/Grub_box_surface_oyster_counts.csv", header= TRUE)


############### Tidy up data ################
colnames(weight)[3] <- "Site"  #change Station to Site
colnames(weight)[4] <- "Strata_surface_base" #change above.below to surface/base

weight$Strata_surface_base = ifelse(weight$Strata_surface_base== "a", "strata_surface", "strata_base") #change a and b to strata_surface and strata_base

surface = surface[1:20,] #removing the comma rows

############### Surface densities by Counter ###############
surface$Density = surface$Count_0.25m/surface$Quad_area_m  #calculate density per meter

#plot of counts by counter
ggplot(data = surface, aes(fill = Counter, x = Site.1, y = Density)) + 
  geom_bar(position = "dodge", stat ="identity") + theme_dark()

#split surface counts by Counter
surface_PF = subset(surface, surface$Counter == "PF")
surface_SH = subset(surface, surface$Counter == "SH")

#plots of Live vs Dead by counter
ggplot(data = surface_PF, aes(fill = Status_Live_Dead, x = Site.1, y = Density)) + 
    geom_bar(stat = "identity") + scale_fill_manual(values = c("#ff8f83", "#300670")) + 
    theme_dark() + ggtitle("PF") + theme(plot.title = element_text(hjust = 0))

ggplot(data = surface_SH, aes(fill = Status_Live_Dead, x = Site.1, y = Density)) + 
  geom_bar(stat = "identity") + scale_fill_manual(values = c("#ff8f83", "#300670")) + 
  theme_dark()+ ggtitle("SH") + theme(plot.title = element_text(hjust = 0))

###############Get total count for each site from weight data ###############
sites = unique(weight$Site) #get site list
weight_surface = subset(weight, weight$Strata_surface_base == "strata_surface") #subset by strata
weight_base = subset(weight, weight$Strata_surface_base == "strata_base")   #subset by strata
total.count_base = vector(length =length(sites))    #create empty vector by strata
total.count_surface = vector(length =length(sites))
for(i in 1:length(sites)){  #for loop to loop per site to get the length of the measurements to get the count of oysters per site

  total.count_base[i] = length(which(weight_base$Site == sites[i]))
  total.count_surface[i] = length(which(weight_surface$Site == sites[i]))
  total = data.frame(sites, total.count_base, total.count_surface)
}

# Density per meter (calculated from the volume of 0.25 X 0.1 m ) 
total$density_base = total$total.count_base/(0.25*0.1)
total$density_surface = total$total.count_surface/(0.25)


############### Average Surface Counts ##########
#the levels are different for the sites at the data, so I had to make a new site index
sites.1 = unique(surface$Site.1)
surface_count_L = vector(length = length(sites.1))
surface_count_D = vector(length = length(sites.1))
for(i in 1:length(sites.1)){  #loop to get the average count per site by Live vs Dead
  surface_count_L[i] = mean(subset(surface, surface$Site.1== sites.1[i] & surface$Status_Live_Dead == "L")$Count_0.25m)
  surface_count_D[i] = mean(subset(surface, surface$Site.1== sites.1[i] & surface$Status_Live_Dead == "D")$Count_0.25m)
  }
surface_df = data.frame(sites.1, surface_count_L, surface_count_D)

surface_df$Density_L = surface_df$surface_count_L/(0.1*0.25)  #estimating density from volumue of grub box
surface_df$Density_D = surface_df$surface_count_D/(0.1*0.25)

# right now the sites match up, but using merge for future data sets that might not match up 
oysters = merge(total, surface_df, by.x = "sites", by.y = "sites.1")

############### Getting Weights per Strata and Site ###############
weight = weight[,-c(1,2,5)]  #remove unnecessary columns
weight_melt = melt(weight)  #melt data 
weight_agg = dcast(weight_melt, Site + Strata_surface_base ~ variable, value.var = c("value") , fun.aggregate = mean, na.rm = TRUE) #aggregate

#slit by surface and base
weight_agg_surface = subset(weight_agg, weight_agg$Strata_surface_base== "strata_surface" )
weight_agg_surface$total_count = oysters$total.count_surface
weight_agg_surface$density = oysters$density_surface

weight_agg_base = subset(weight_agg, weight_agg$Strata_surface_base== "strata_base" )
weight_agg_base$total_count = oysters$total.count_base[1:4]
weight_agg_base$density = oysters$density_base[1:4]

############### Looking at Surface and Count data ###############
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442") #color blind friendly colors
#Live density vs Surface Density 
ggplot(data = oysters, aes(x = density_surface, y = Density_L, color = sites)) + geom_point(size = 3) + 
    labs(x = "Surface Density", y = "Live Density") + 
    scale_color_manual(values=cbbPalette) + 
    geom_text(label=sites, color = "black", nudge_x = 300, nudge_y = 50) + 
    scale_x_continuous(limits = c(0, 9000))

#Dead density vs Surface Density
ggplot(data = oysters, aes(x = density_surface, y = Density_D, color = sites)) + geom_point(size = 3) + 
  labs(x = "Surface Density", y = "Dead Density") + 
  scale_color_manual(values=cbbPalette)+ 
  geom_text(label=sites, color = "black", nudge_x = 300, nudge_y = 50) + 
  scale_x_continuous(limits = c(0, 9000))

#Live density vs Base Density 
ggplot(data = oysters, aes(x = density_base, y = Density_L, color = sites)) + geom_point(size = 3) + 
  labs(x = "Base Density", y = "Live Density") + 
  scale_color_manual(values=cbbPalette)+ 
  geom_text(label=sites, color = "black", nudge_x = 100, nudge_y = 50) + 
  scale_x_continuous(limits = c(0, 1000))

#Dead density vs Base Density 
ggplot(data = oysters, aes(x = density_base, y = Density_D, color = sites)) + geom_point(size = 3) + 
  labs(x = "Base Density", y = "Dead Density") + 
  scale_color_manual(values=cbbPalette)+ 
  geom_text(label=sites, color = "black", nudge_x = 50, nudge_y = 50) + 
  scale_x_continuous(limits = c(0, 750))



###############Correlation Plot ##############

oysters_surface = weight_agg_surface[,3:10] #remove cols that aren't numeric

oysters_surface_scaled<-scale(oysters_surface) # normalize the data frame. This will also convert the df to a matrix.  

corr<-rcorr(oysters_surface_scaled) # compute Pearson's (or spearman's corr) with rcorr from Hmisc package. I like rcorr as it allows to separately access the correlations, the # or observations and the p-value. ?rcorr is worth a read.
corr_r<-as.matrix(corr[[1]])# Access the correlation matrix. 
pval<-as.matrix(corr[[3]])# get the p-values

corrplot(corr_r,method="square",type="lower",diag=FALSE,tl.col="black",tl.cex=1,tl.offset=0.1,tl.srt=45)# plot all pairs

### The base data is missing for LCNA --- need at least 4 observations for the corrplot
