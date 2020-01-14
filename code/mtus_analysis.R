library(haven)
library(viridis)
library(compositions)
library(ggtern)

# Definitions
ternaryColors = viridis(n=3, option="viridis", begin=0.25, end=0.75)



####
# Data Cleaning/Preperation
####

mtus = read_dta("Statistik/econproj/data/mtus.dta")
mtus = mtus[mtus$sex > 0,]
mtus = as.data.frame(mtus)

# Calculate geometric mean per country.
mtus_comp = acomp(mtus[,c(19,21,22)])
# ...



####
# Descriptive Statistics. Overview Plots.
####

ggtern(data=mtus, aes(work,homeprod,leisure)) + 
  theme_rgbw(base_size=13) +
  geom_hex_tern(binwidth=0.05,
                aes(fill=..density.., color=..density..),  # Bug graphics dev
                show.legend = FALSE) +
  scale_fill_viridis() +
  labs(x="Work",y="Homeprod",z="Leisure")

ggtern(data=mtus[mtus$work > 0,], aes(work,homeprod,leisure)) + 
  theme_rgbw() +
  geom_hex_tern(binwidth=0.05,
                aes(fill=..density.., color=..density..),  # Bug graphics dev
                show.legend = FALSE) +
  scale_fill_viridis() +
  labs(x="Work",y="Homeprod",z="Leisure")



###
# Gender Differences.

# Contourplot Male/Female
ggtern(data=mtus[mtus$work > 0,], aes(work,homeprod,leisure,color=factor(sex))) + 
  theme_rgbw() + 
  geom_density_tern(alpha=0.7) +
  scale_colour_viridis(name  ="Sex",
                       breaks=c(2, 1),
                       labels=c("Female", "Male"),
                       discrete=TRUE, begin=0.3, end=0.7, option="inferno") +
  labs(x="Work",y="Homeprod",z="Leisure")

# Hexplot Male Working
ggtern(data=mtus[mtus$work > 0 & mtus$sex == 1,], 
       aes(work, homeprod, leisure)) + 
  theme_rgbw() +
  geom_hex_tern(binwidth=0.05,
                aes(fill=..density.., color=..density..),  # Bug graphics dev
                show.legend = FALSE) +
  scale_fill_viridis() +
  labs(x="Work",y="Homeprod",z="Leisure")

# Hexplot Female Working
ggtern(data=mtus[mtus$work > 0 & mtus$sex == 2,], 
       aes(work, homeprod, leisure)) + 
  theme_rgbw() +
  geom_hex_tern(binwidth=0.05,
                aes(fill=..density.., color=..density..),  # Bug graphics dev
                show.legend = FALSE) +
  scale_fill_viridis() +
  labs(x="Work",y="Homeprod",z="Leisure")



###
# Country Differences

# Scatterplot of Geometric Means per Country
ggtern(data=mtus, aes(work,homeprod,leisure,color=factor(country))) + 
  theme_rgbw() + 
  geom_density_tern() +
  #scale_colour_discrete(name  ="Sex",
  #                      breaks=c(2, 1),
  #                      labels=c("Female", "Male")) +
  labs(x="Work",y="Homeprod",z="Leisure")



###
# Age Differences

# Mean Age bin plot.
ggtern(mtus[mtus$work > 0,],aes(work,homeprod,leisure)) + 
  theme_rgbw(base_size=13) +
  geom_hex_tern(binwidth=0.05, aes(value=age),color="#00204DFF",fun=mean) +
  scale_fill_viridis(option="cividis", direction=-1) +
  scale_color_continuous(name = "Mean age") +
  labs(x="Work",y="Homeprod",z="Leisure")


###
# Education

# Mean Age bin plot.
myfun = function(x) sum(x > 1,na.rm=T)/sum(x,na.rm=T)
ggtern(mtus[mtus$work > 0,],aes(work,homeprod,leisure)) + 
  theme_rgbw(base_size=13) +
  geom_hex_tern(binwidth=0.05, aes(value=edcat), color="#00204DFF", fun=myfun) +
  scale_fill_viridis(option="cividis", direction=1) +
  scale_color_continuous(name = "Prop > 2nd edu") +
  labs(x="Work",y="Homeprod",z="Leisure")
