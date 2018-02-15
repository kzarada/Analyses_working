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
library(RCurl)

### Load Data ###
#weight = read.csv(text = getURL("https://github.com/LCRoysterproject/Data/blob/master/Oyster_data/Grub/Grub_box_oyster_size_weight_wet_dry.csv"), header = T)

weight = read.csv("/Users/fas/Desktop/Oysters/Data/Grub_box_oyster_size_weight_wet_dry.csv", header = TRUE)
bulk = read.csv("/Users/fas/Desktop/Oysters/Data/Grub_box_shell_bulk_weights.csv", header = TRUE)
surface = read.csv("/Users/fas/Desktop/Oysters/Data/Grub_box_surface_oyster_counts.csv", header= TRUE)


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


############### Getting rid of SH Surface counts##########

surface_PF = subset(surface, surface$Counter == "PF")

surface_df = surface_PF %>% select(Site.1, Status_Live_Dead, Count_0.25m ) %>%
    spread(Status_Live_Dead, Count_0.25m) 

colnames(surface_df) = c("Site", "Count_Dead", "Count_Live") 
surface_df$Density_Dead = surface_df$Count_Dead/0.25 
surface_df$Density_Live = surface_df$Count_Live/0.25

# right now the sites match up, but using merge for future data sets that might not match up 
oysters = merge(total, surface_df, by.x = "sites", by.y = "Site")

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



oysters$Height_surface = weight_agg_surface$Height_mm
oysters$Wet_weight_surface = weight_agg_surface$Wet_weight_g
oysters$Dry_wt_48_surface = weight_agg_surface$Dry_wt_48
############## BULK DATA #################################
#taking bulk data and adding weights onto oyster data frame ####

bulk.sub = bulk %>% select(Site.1, Strata_surface_base, Status_Live_Dead_Hash, Weight_kg)

bulk.sub = bulk.sub[complete.cases(bulk.sub),]

bulk.total.weight = bulk.sub %>% select(Site.1, Strata_surface_base, Weight_kg) %>% 
  group_by(Site.1, Strata_surface_base) %>%
  summarise(Total_Weight = sum(Weight_kg)) %>%
  spread(Strata_surface_base, Total_Weight)

bulk.live = subset(bulk.sub, bulk.sub$Status_Live_Dead_Hash == "L")
bulk.dead = subset(bulk.sub, bulk.sub$Status_Live_Dead_Hash == "D")
bulk.hash = subset(bulk.sub, bulk.sub$Status_Live_Dead_Hash == "H")


oysters$Bottom_total_weight = subset(bulk.total.weight, bulk.total.weight$Strata_surface_base == 'bottom')$Total_Weight
oysters$Sub_total_weight = subset(bulk.total.weight, bulk.total.weight$Strata_surface_base == 'sub')$Total_Weight
oysters$Surface_total_weight = subset(bulk.total.weight, bulk.total.weight$Strata_surface_base == 'surface')$Total_Weight


oysters$Sub_live_weight = subset(bulk.live, bulk.live$Strata_surface_base == 'sub')$Weight_kg
oysters$Surface_live_weight = subset(bulk.live, bulk.live$Strata_surface_base == 'surface')$Weight_kg

oysters$Surface_dead_weight = subset(bulk.dead, bulk.dead$Strata_surface_base == 'surface')$Weight_kg

oysters$Bottom_hash_weight = subset(bulk.hash, bulk.hash$Strata_surface_base == 'bottom')$Weight_kg
oysters$Surface_hash_weight = subset(bulk.hash, bulk.hash$Strata_surface_base == 'surface')$Weight_kg

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

#Total weight vs Surface Density 

ggplot(data = oysters, aes(x = Surface_total_weight, y = density_surface, color = sites)) + geom_point(size = 3) + 
  labs(x = "Surface Biomass", y = "Surface Density") + 
  scale_color_manual(values=cbbPalette)+ 
  geom_text(label=sites, color = "black", nudge_x = 1, nudge_y = 20) 

#Live Biomass vs Surface Density 

ggplot(data = oysters, aes(x = Surface_live_weight, y = density_surface, color = sites)) + geom_point(size = 3) + 
  labs(x = "Live Surface Biomass", y = "Surface Density") + 
  scale_color_manual(values=cbbPalette)+ 
  geom_text(label=sites, color = "black", nudge_x = 0.25, nudge_y = 20) 


#Dead biomass vs surface Density 

ggplot(data = oysters, aes(x = Surface_dead_weight, y = density_surface, color = sites)) + geom_point(size = 3) + 
  labs(x = "Dead Surface Biomass", y = "Surface Density") + 
  scale_color_manual(values=cbbPalette)+ 
  geom_text(label=sites, color = "black", nudge_x = 0.25, nudge_y = 20) 


#Hash Biomass vs Surface Density 

ggplot(data = oysters, aes(x = Surface_hash_weight, y = density_surface, color = sites)) + geom_point(size = 3) + 
  labs(x = "Hash Surface Biomass", y = "Surface Density") + 
  scale_color_manual(values=cbbPalette)+ 
  geom_text(label=sites, color = "black", nudge_x = 0.1, nudge_y = 20) 


#Hash vs Dead Shell Weight 
ggplot(data = oysters, aes(x = Surface_hash_weight, y = Surface_dead_weight, color = sites)) + geom_point(size = 3) + 
  labs(x = "Hash Surface Biomass", y = "Dead Surface Biomass") + 
  scale_color_manual(values=cbbPalette)+ 
  geom_text(label=sites, color = "black", nudge_x = 0.05, nudge_y = 0.1) 

#Hash vs Live Shell Weight
ggplot(data = oysters, aes(x = Surface_hash_weight, y = Surface_live_weight, color = sites)) + geom_point(size = 3) + 
  labs(x = "Hash Surface Biomass", y = "Live Surface Biomass") + 
  scale_color_manual(values=cbbPalette)+ 
  geom_text(label=sites, color = "black", nudge_x = 0.05, nudge_y = 0.1) 




###############Correlation Plot ##############

oysters_surface = weight_agg_surface[,3:10] #remove cols that aren't numeric

oysters_surface_scaled<-scale(oysters_surface) # normalize the data frame. This will also convert the df to a matrix.  

corr<-rcorr(oysters_surface_scaled) # compute Pearson's (or spearman's corr) with rcorr from Hmisc package. I like rcorr as it allows to separately access the correlations, the # or observations and the p-value. ?rcorr is worth a read.
corr_r<-as.matrix(corr[[1]])# Access the correlation matrix. 
pval<-as.matrix(corr[[3]])# get the p-values

corrplot(corr_r,method="square",type="lower",diag=FALSE,tl.col="black",tl.cex=1,tl.offset=0.1,tl.srt=45)# plot all pairs
### The base data is missing for LCNA --- need at least 4 observations for the corrplot


####### Correlation Plots ############


#total surface count, surface density, live denisty, total surface biomass, live surface biomass, dead surface biomass, surface hash biomass, height, wet weight, dry wt 48

corr= oysters[,c(3,5,9,12,14,15,17,18,19, 20)]
corr_scales = scale(corr)

corr = rcorr(corr_scales)
corr_r<-as.matrix(corr[[1]])# Access the correlation matrix. 
pval<-as.matrix(corr[[3]])
corrplot(corr_r,method="square",type="lower",diag=FALSE,tl.col="black",tl.cex=1,tl.offset=0.1,tl.srt=45)# plot all pairs




