library(tidyverse)
library(writexl)
library(ggplot2)

Batchs <- read.csv("C:/Users/freds/OneDrive/Documents/Capstone Data/Data/Inventory_Batches.csv")
Products <- read.csv("C:/Users/freds/OneDrive/Documents/Capstone Data/Data/Products.csv")


summary(Batchs)

#Get rid of rows that have no data for any all of the compounds
Full_Batchs <- subset(Batchs, !is.na(BETAEUDESMOL_PERCENTAGE)|
                        !is.na(CBD_PERCENTAGE)|
                        !is.na(BETACARYOPHYLLENE_PERCENTAGE)|
                        !is.na(CBDA_PERCENTAGE)|
                        !is.na(CBC_PERCENTAGE)|
                        !is.na(CBG_PERCENTAGE)|
                        !is.na(CBN_PERCENTAGE)|
                        !is.na(CARYOPHYLLENEOXIDE_PERCENTAGE)|
                        !is.na(HUMULENE_PERCENTAGE)|
                        !is.na(LINALOOL_PERCENTAGE)|
                        !is.na(OCIMENE_PERCENTAGE)|
                        !is.na(THCA_PERCENTAGE)|
                        !is.na(THCV_PERCENTAGE)|
                        !is.na(THC_PERCENTAGE)|
                        !is.na(TERPINOLENE_PERCENTAGE))
head(Full_Batchs)
summary(Full_Batchs)

#Save filtered data for batchs
write_excel_csv(Full_Batchs,"C:/Users/freds/OneDrive/Documents/Capstone Data/Data/Cleaned_Batchs.csv")
Clean_Batchs <- read.csv("C:/Users/freds/OneDrive/Documents/Capstone Data/Data/Cleaned_Batchs.csv")

#Select the columns we want to use from batchs
Batchs2 <- Clean_Batchs[,c(2,3,12:14,16:26,34,35)]
#Rename
names(Batchs2)[3:16] = c("BETACARYOPHYLLENE",
                         "BETAEUDESMOL",
                         "CBD",
                         "CBC",
                         "CBG",
                         "CBN",
                         "CARYOPHYLLENEOXIDE",
                         "HUMULENE",
                         "LINALOOL",
                         "OCIMENE",
                         "THCA",
                         "THCV",
                         "THC",
                         "TERPINOLENE")
#summary(!is.na(Batchs2))

# Subset Batchs2 where all values in columns 3 to 16 are either NA, >= 0, or <= 100
#used AI
Batchs2 <- Batchs2[apply(Batchs2[, 3:16], 1, function(row) {
  all(is.na(row) | (row >= 0 & row <= 100))
}), ]

################################################
#Messed up orignally and wanted to keep prices but want them stored
#as the last columns so repeat process above but with prices
Batchs3 <- Clean_Batchs[,c(2,3,12:14,16:26,34,35,7,31,33,36)]
names(Batchs2)[3:16] = c("BETACARYOPHYLLENE",
                         "BETAEUDESMOL",
                         "CBD",
                         "CBC",
                         "CBG",
                         "CBN",
                         "CARYOPHYLLENEOXIDE",
                         "HUMULENE",
                         "LINALOOL",
                         "OCIMENE",
                         "THCA",
                         "THCV",
                         "THC",
                         "TERPINOLENE")
#summary(!is.na(Batchs2))

# Subset Batchs2 where all values in columns 3 to 16 are either NA, >= 0, or <= 100
Batchs3 <- Batchs3[apply(Batchs3[, 3:16], 1, function(row) {
  all(is.na(row) | (row >= 0 & row <= 100))
}), ]

#Keep the variables we want from products
Product3 <- Products[,c(1,5,7,9,10,13)]

#merge the two data sets together by product ID
Prices <-Batchs3 |> inner_join(Product3, by = "PRODUCT_ID")
#Get the price variables we want to add
Add <- Prices[,c(19:22)]






#merge the two data sets together without prices
Product2 <- Products[,c(1,5,7,9,10,13,15)]
Full_Cleaned <-Batchs2 |> inner_join(Product2, by = "PRODUCT_ID")
#add the prices to the back
Full_Cleaned<-cbind(Full_Cleaned,Add)


###################################################
# Data cleaning

#Keep the type of products we would want to use
Types = unique(Full_Cleaned$PRODUCT_TYPE)
Types = Types[c(1:7,9:11,13:18,20:26,30:31)]
#subsets it by products with the types we want including NAs since
#the user might not have entered data here but the rest could still
#be useful to analyze
Full_Cleaned <- subset(Full_Cleaned, is.na(PRODUCT_TYPE)|
                     PRODUCT_TYPE %in% Types)


#Combine the two different strain variables from batchs and product
#into one, we did this because product might have data entered while 
#batchs did not and vise versa.
Full_Cleaned$STRAIN = ifelse(
  Full_Cleaned$STRAIN.x == Full_Cleaned$STRAIN.y, 
       Full_Cleaned$STRAIN.x, 
       str_c(Full_Cleaned$STRAIN.x,Full_Cleaned$STRAIN.y))

#Remove old strain variables since we made a new one that combines them
Full_Cleaned <- subset(Full_Cleaned, select = -c(STRAIN.y,STRAIN.x))


#Alot of the product type had the same value just spelled differently,
#so we wanted to combine them into a common factor.
#Used AI to help with this
Full_Cleaned <- Full_Cleaned |>
  mutate(PRODUCT_TYPE = case_when(
    PRODUCT_TYPE %in% c("Concentrates", "Concentrate", "Concentrate/Extract") ~ "Concentrate",
    PRODUCT_TYPE %in% c("Flower", "Culi - Flower") ~ "Flower",
    PRODUCT_TYPE %in% c("Vaporization", "Vapes", "Vape") ~ "Vape",
    PRODUCT_TYPE %in% c("Vape Cartridge", "Cartridges", "Cartridges\nCartridges") ~ "Vape",
    PRODUCT_TYPE %in% c("Mip - Edibles", "Edible", "Edibles") ~ "Edible",
    PRODUCT_TYPE %in% c("Pre-Roll", "Pre-Rolls", "Pre Roll") ~ "Pre-Roll",
    PRODUCT_TYPE %in% c("Topicals", "Mip - Topicals") ~ "Topicals",
    PRODUCT_TYPE == "Mip - Beverages" ~ "Beverage",
    PRODUCT_TYPE == "Mip - Oral & Nasal" ~ "Oral & Nasal",
    TRUE ~ PRODUCT_TYPE # keep other values as they are
  ))

#Save cleaned data set so we dont have to run the code above again
write_excel_csv(Full_Cleaned,"C:/Users/freds/OneDrive/Documents/Capstone Data/Data/Full_Cleaned.csv")
Full_Cleaned <- read.csv("C:/Users/alexw/Desktop/Capstone/Data/Full_Cleaned.csv")

################################################################
#Frequency of each compound
Freq_na = colSums(is.na(Full_Cleaned[,3:16]))
Freq = Freq_na/nrow(Full_Cleaned)

HIST = data.frame(
  compound = names(Full_Cleaned[,3:16]),
  freq = Freq
)

ggplot(HIST, aes(x = compound, y = freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound 'NA' Frequency",
       x = "Compound",
       y = "Proportion NA") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###########################################
#Percentage of each compound occurring in a product
#Keep
Freq2 = Freq/nrow(Full_Cleaned)

HIST2 = data.frame(
  compound = names(Full_Cleaned[,3:16]),
  precentage = Freq2
)

ggplot(HIST2, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound Precentage",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#############################################
#Percentage of THC and CBD occuring in a product
Freq = colSums(is.na(Full_Cleaned[,c(5,13:15)]))
Freq = nrow(Full_Cleaned)-Freq

Freq3 = Freq/nrow(Full_Cleaned)

HIST3 = data.frame(
  compound = names(Full_Cleaned[,c(5,13:15)]),
  precentage = Freq3
)

ggplot(HIST3, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################################################
#Get a distribution of CBD and THC precentages
#Keep

CBD = Full_Cleaned$CBD[Full_Cleaned$CBD > 0]
THC = Full_Cleaned$THC[Full_Cleaned$THC > 0]
par( mfrow = c(1,2))
hist(CBD, col = "blue", xlim =c(0,2), breaks = 2000)
hist(THC, col = "red",xlim = c(0,100), breaks = 100)

######################################################
#Get a distribution of each compound percentage


BETAEUDESMOL = Full_Cleaned$BETACARYOPHYLLENE
CBC = Full_Cleaned$CBC
CBG = Full_Cleaned$CBG
CBN = Full_Cleaned$CBN
CARYOPHYLLENEOXIDE = Full_Cleaned$CARYOPHYLLENEOXIDE
HUMULENE = Full_Cleaned$HUMULENE
LINALOOL = Full_Cleaned$LINALOOL
OCIMENE = Full_Cleaned$OCIMENE
TERPINOLENE = Full_Cleaned$TERPINOLENE

par( mfrow = c(1,3))

#Changed the x limit to get the majority of the distribution,
#did this by trail and error. Note there are some data points 
#above each of the x limits for each histogram but they are sparse
hist(BETAEUDESMOL, xlim = c(0,1),breaks = 1000)
hist(CBC, xlim = c(0,1), breaks = 1000)
hist(CBG, xlim = c(0,1), breaks = 1000)

hist(CBN, xlim = c(0,1), breaks = 1000)
hist(CARYOPHYLLENEOXIDE, xlim = c(0,.4), breaks = 1000)
hist(HUMULENE, xlim = c(0,1), breaks = 1000)

hist(LINALOOL, xlim = c(0,1), breaks = 1000)
hist(OCIMENE, xlim = c(0,1), breaks = 1000)
hist(TERPINOLENE, xlim = c(0,1), breaks = 1000)

###############################################
#Given that Betacaryophyllene is in the product what percent
#of the time are the other compounds. Left out THC and CBD since
#they are kind of a different compound to analyze separately 
#Keep

BETACAR = subset(Full_Cleaned, BETACARYOPHYLLENE > 0)

Freq = colSums(is.na(BETACAR[,c(4,6:12,16)]))
Freq = nrow(BETACAR)-Freq

Freq4 = Freq/nrow(BETACAR)

HIST4 = data.frame(
  compound = names(BETACAR[,c(4,6:12,16)]),
  precentage = Freq4
)

BetaCar <- ggplot(HIST4, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage given Betacaryophyllene",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
BetaCar

################################################
#Given that BetaEudesmole is in the product what percent
#of the time are the other compounds.
#Keep
BETAEUD = subset(Full_Cleaned, BETAEUDESMOL > 0)

Freq = colSums(is.na(BETAEUD[,c(3,6:12,16)]))
Freq = nrow(BETAEUD)-Freq

Freq5 = Freq/nrow(BETAEUD)

HIST5 = data.frame(
  compound = names(BETAEUD[,c(3,6:12,16)]),
  precentage = Freq5
)

BetaEud <- ggplot(HIST5, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage given Betaeudesmol",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
BetaEud
###########################################################
#Given that CBD is in the product what percent
#of the time are the other compounds.

CBD = subset(Full_Cleaned, CBD > 0)

Freq = colSums(is.na(CBD[,c(3:4,6:17)]))
Freq = nrow(CBD)-Freq

Freq6 = Freq/nrow(CBD)

HIST6 = data.frame(
  compound = names(CBD[,c(3:4,6:17)]),
  precentage = Freq6
)

ggplot(HIST6, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage given CBD",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##########################################################
#Given that CBC is in the product what percent
#of the time are the other compounds.
#Keep
CBC = subset(Full_Cleaned, CBC > 0)

Freq = colSums(is.na(CBC[,c(3:4,6:12,16)]))
Freq = nrow(CBC)-Freq

Freq6 = Freq/nrow(CBC)

HIST6 = data.frame(
  compound = names(CBC[,c(3:4,6:12,16)]),
  precentage = Freq6
)

CBC_H <- ggplot(HIST6, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage given CBC",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
CBC_H

#################################################
#Given that CBG is in the product what percent
#of the time are the other compounds.
#Keep
CBG = subset(Full_Cleaned, CBG > 0)

Freq = colSums(is.na(CBG[,c(3:4,6,8:12,16)]))
Freq = nrow(CBG)-Freq

Freq6 = Freq/nrow(CBG)

HIST6 = data.frame(
  compound = names(CBG[,c(3:4,6,8:12,16)]),
  precentage = Freq6
)

CBG_H<-ggplot(HIST6, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage given CBG",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
CBG_H

##############################################
#Given that CBN is in the product what percent
#of the time are the other compounds.
#Keep
CBN = subset(Full_Cleaned, CBN > 0)

Freq = colSums(is.na(CBN[,c(3:4,6:7,9:12,16)]))
Freq = nrow(CBN)-Freq

Freq6 = Freq/nrow(CBN)

HIST6 = data.frame(
  compound = names(CBN[,c(3:4,6:7,9:12,16)]),
  precentage = Freq6
)

CBN_H<-ggplot(HIST6, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage given CBN",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
CBN_H
###############################################
#Given that Caryophylleneoxide is in the product what percent
#of the time are the other compounds.
#Keep
CARY = subset(Full_Cleaned, CARYOPHYLLENEOXIDE > 0)

Freq = colSums(is.na(CARY[,c(3:4,6:8,10:12,16)]))
Freq = nrow(CARY)-Freq

Freq6 = Freq/nrow(CARY)

HIST6 = data.frame(
  compound = names(CARY[,c(3:4,6:8,10:12,16)]),
  precentage = Freq6
)

Cary<-ggplot(HIST6, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage given Caryophylleneoxide",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
Cary

##################################################
#Given that Humulene is in the product what percent
#of the time are the other compounds.
#Keep
HUM = subset(Full_Cleaned, HUMULENE > 0)

Freq = colSums(is.na(HUM[,c(3:4,6:9,11,12,16)]))
Freq = nrow(HUM)-Freq

Freq6 = Freq/nrow(HUM)

HIST6 = data.frame(
  compound = names(HUM[,c(3:4,6:9,11,12,16)]),
  precentage = Freq6
)

Hum<-ggplot(HIST6, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage given Humulene",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
Hum
###################################################
#Given that Linalool is in the product what percent
#of the time are the other compounds.
#Keep
LINA = subset(Full_Cleaned, LINALOOL > 0)

Freq = colSums(is.na(LINA[,c(3:4,6:10,12,16)]))
Freq = nrow(LINA)-Freq

Freq6 = Freq/nrow(LINA)

HIST6 = data.frame(
  compound = names(LINA[,c(3:4,6:10,12,16)]),
  precentage = Freq6
)

Lina<-ggplot(HIST6, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage given Linalool",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
Lina
################################################
#Given that Ocimene is in the product what percent
#of the time are the other compounds.
#Keep
OCI = subset(Full_Cleaned, OCIMENE > 0)

Freq = colSums(is.na(OCI[,c(3:4,6:11,16)]))
Freq = nrow(OCI)-Freq

Freq6 = Freq/nrow(OCI)

HIST6 = data.frame(
  compound = names(OCI[,c(3:4,6:11,16)]),
  precentage = Freq6
)

OCI_H<-ggplot(HIST6, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage given OCI",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
OCI_H
#################################################

THCA = subset(Full_Cleaned, THCA_PERCENTAGE > 0)

Freq = colSums(is.na(THCA[,c(3:13,15:17)]))
Freq = nrow(THCA)-Freq

Freq6 = Freq/nrow(THCA)

HIST6 = data.frame(
  compound = names(THCA[,c(3:13,15:17)]),
  precentage = Freq6
)

ggplot(HIST6, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################################################

THCV = subset(Full_Cleaned, THCV_PERCENTAGE > 0)

Freq = colSums(is.na(THCV[,c(3:14,16:17)]))
Freq = nrow(THCV)-Freq

Freq6 = Freq/nrow(THCV)

HIST6 = data.frame(
  compound = names(THCV[,c(3:14,16:17)]),
  precentage = Freq6
)

ggplot(HIST6, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################################################

THC = subset(Full_Cleaned, THC_PERCENTAGE > 0)

Freq = colSums(is.na(THC[,c(3:15,17)]))
Freq = nrow(THC)-Freq

Freq6 = Freq/nrow(THC)

HIST6 = data.frame(
  compound = names(THC[,c(3:15,17)]),
  precentage = Freq6
)

ggplot(HIST6, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####################################################
#Given that Terpinolene is in the product what percent
#of the time are the other compounds.
#Keep
TERP = subset(Full_Cleaned, TERPINOLENE > 0)

Freq = colSums(is.na(TERP[,c(3:4,6:12)]))
Freq = nrow(TERP)-Freq

Freq6 = Freq/nrow(TERP)

HIST6 = data.frame(
  compound = names(TERP[,c(3:4,6:12)]),
  precentage = Freq6
)

Terp<-ggplot(HIST6, aes(x = compound, y = precentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Compound precentage given Terpinolene",
       x = "Compound",
       y = "Precentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
Terp
#################################################
#Not working
par( mfrow = c(3,3))
BetaCar
BetaEud
Cary

# Install and load gridExtra (if you haven't already)
# install.packages("gridExtra")
library(gridExtra)

# Arrange plots in a 2x2 grid
grid.arrange(BetaCar, BetaEud, Cary, nrow = 1, ncol = 3, 
             top = "Percentage of Each Compound")

##############################################



ggplot(Full_Cleaned, aes(x = PRODUCT_TYPE)) +
  geom_bar() +
  xlab("Product Type") +
  ylab("Count") +
  ggtitle("Frequency of Each Value in Vector")
#We see for this hist that Vape,Flower,Concentrate, and edibles
#are the most predominant product type

type = unique(Full_Cleaned$PRODUCT_TYPE)
product_type = subset(Full_Cleaned, PRODUCT_TYPE %in% type[c(1,2,3,5)])

ggplot(product_type, aes(x = PRODUCT_TYPE)) +
  geom_bar() +
  xlab("Product Type") +
  ylab("Count") +
  ggtitle("Frequency of Each Product Type")


######################################################




#################################################
#keep both below
type = unique(Full_Cleaned$PRODUCT_TYPE)

Compounds <- Full_Cleaned[,c(3,4,6:12,16,21)]


Compounds <- subset(Compounds, PRODUCT_TYPE %in% type[c(1,2,3,5)])


Compounds_long <- Compounds |>
  pivot_longer(cols = -PRODUCT_TYPE, names_to = "Compound", values_to = "Value")


Compounds_percentage <- Compounds_long |>
  group_by(PRODUCT_TYPE, Compound) |>
  summarise(Total = sum(Value, na.rm = TRUE), .groups = "drop") |>
  group_by(PRODUCT_TYPE) |>
  mutate(Percentage = (Total / sum(Total)) * 100)


#Used AI to help plot
x11()
ggplot(Compounds_percentage, aes(x = PRODUCT_TYPE, y = Percentage, fill = Compound)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Product Type") +
  ylab("Percentage (%)") +
  ggtitle("Percentage Distribution of Compounds by Product Type") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#########################################################
#Same process above but for THC and CBD
THC_CBD <- Full_Cleaned[,c(5,13:15,21)]

THC_CBD <- subset(THC_CBD, PRODUCT_TYPE %in% type[c(1,2,3,5)])


THC_CBD_long <- THC_CBD |>
  pivot_longer(cols = -PRODUCT_TYPE, names_to = "Compound", values_to = "Value")


THC_CBD_percentage <- THC_CBD_long |>
  group_by(PRODUCT_TYPE, Compound) |>
  summarise(Total = sum(Value, na.rm = TRUE), .groups = "drop") |>
  group_by(PRODUCT_TYPE) |>
  mutate(Percentage = (Total / sum(Total)) * 100)


#Used AI to help plot
ggplot(THC_CBD_percentage, aes(x = PRODUCT_TYPE, y = Percentage, fill = Compound)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Product Type") +
  ylab("Percentage (%)") +
  ggtitle("Percentage Distribution of Compounds by Product Type") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#######################################################



Prices <- Full_Cleaned[,c(3:16,22)]

Prices_sample <- Prices |>
  sample_n(1000, replace = TRUE)



Prices_long <- Prices_sample |>
  pivot_longer(cols = -PRICE, names_to = "Compound", values_to = "Value")

x11()
ggplot(Prices_long, aes(x = Value, y = PRICE)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Compound, scales = "free_x") +
  xlab("Compound Precentage") +
  ylab("Price") +
  ggtitle("Scatter Plot of Each Compound vs Price") +
  theme_minimal()

###########################################################
#Vape scatter plot
#Keep all below

Vape_Prices <- Full_Cleaned[,c(3:16,21,22)]
Vape_Prices <- subset(Vape_Prices, PRODUCT_TYPE == "Vape")
Vape_Prices <- subset(Vape_Prices, select = -c(PRODUCT_TYPE))

Vape_Prices_sample <- Vape_Prices |>
  sample_n(1000, replace = TRUE)

Vape_Prices_long <- Vape_Prices_sample |>
  pivot_longer(cols = -PRICE, names_to = "Compound", values_to = "Value")

x11()
ggplot(Vape_Prices_long, aes(x = Value, y = PRICE, color = Compound)) +
  geom_point(alpha = 0.6, size = 2) +  
  facet_wrap(~ Compound, scales = "free_x") +
  xlab("Compound Percentage") +
  ylab("Price") +
  ggtitle("Vape Scatter Plot of Compound vs Price") +
  theme_minimal(base_size = 14) +  
  scale_color_viridis_d(option = "plasma") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )


############################################################
#Flower scatter plot


Flower_Prices <- Full_Cleaned[,c(3:16,21,22)]
Flower_Prices <- subset(Flower_Prices, PRODUCT_TYPE == "Flower")
Flower_Prices <- subset(Flower_Prices, select = -c(PRODUCT_TYPE))

Flower_Prices_sample <- Flower_Prices |>
  sample_n(1000, replace = TRUE)

Flower_Prices_long <- Flower_Prices_sample |>
  pivot_longer(cols = -PRICE, names_to = "Compound", values_to = "Value")


x11()
ggplot(Flower_Prices_long, aes(x = Value, y = PRICE, color = Compound)) +
  geom_point(alpha = 0.6, size = 2) +  
  facet_wrap(~ Compound, scales = "free_x") +
  xlab("Compound Percentage") +
  ylab("Price") +
  ggtitle("Flower Scatter Plot of Compound vs Price") +
  theme_minimal(base_size = 14) +  
  scale_color_viridis_d(option = "plasma") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"  
  )


#########################################
#Edible scatter plot of price

Edible_Prices <- Full_Cleaned[,c(3:16,21,22)]
Edible_Prices <- subset(Edible_Prices, PRODUCT_TYPE == "Edible")
Edible_Prices <- subset(Edible_Prices, select = -c(PRODUCT_TYPE))

Edible_Prices_sample <- Edible_Prices |>
  sample_n(1000, replace = TRUE)

Edible_Prices_long <- Edible_Prices_sample |>
  pivot_longer(cols = -PRICE, names_to = "Compound", values_to = "Value")


x11()
ggplot(Edible_Prices_long, aes(x = Value, y = PRICE, color = Compound)) +
  geom_point(alpha = 0.6, size = 2) +  
  facet_wrap(~ Compound, scales = "free_x") +
  xlab("Compound Percentage") +
  ylab("Price") +
  ggtitle("Edible Scatter Plot of Compound vs Price") +
  theme_minimal(base_size = 14) +  
  scale_color_viridis_d(option = "plasma") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"  
  )


#############################################
#Cart scatter plot vs price

Cart_Prices <- Full_Cleaned[,c(3:16,21,22)]
Cart_Prices <- subset(Cart_Prices, PRODUCT_TYPE == "Flower")
Cart_Prices <- subset(Cart_Prices, select = -c(PRODUCT_TYPE))

Cart_Prices_sample <- Cart_Prices |>
  sample_n(1000, replace = TRUE)

Cart_Prices_long <- Cart_Prices_sample |>
  pivot_longer(cols = -PRICE, names_to = "Compound", values_to = "Value")


x11()
ggplot(Cart_Prices_long, aes(x = Value, y = PRICE, color = Compound)) +
  geom_point(alpha = 0.6, size = 2) +  
  facet_wrap(~ Compound, scales = "free_x") +
  xlab("Compound Percentage") +
  ylab("Price") +
  ggtitle("Cart Scatter Plot of Compound vs Price") +
  theme_minimal(base_size = 14) +  
  scale_color_viridis_d(option = "plasma") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "none" 
  )
