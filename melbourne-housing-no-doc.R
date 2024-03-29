housing_dataset <- read.csv("melbourne_data.csv",
                            stringsAsFactors = FALSE, na.strings = "#N/A")

str(housing_dataset)

housing_dataset$X <- NULL

housing_dataset$Date <- as.Date(housing_dataset$Date, format = "%d/%m/%Y")

str(housing_dataset$Date)
summary(housing_dataset$Date)

housing_dataset$Type[housing_dataset$Type == "h"] <- "House"
housing_dataset$Type[housing_dataset$Type == "u"] <- "Unit/Duplex"
housing_dataset$Type[housing_dataset$Type == "t"] <- "Townhouse"
housing_dataset$Type <- factor(housing_dataset$Type)

levels(factor(housing_dataset$Type))
str(housing_dataset$Type)
summary(housing_dataset$Type)

housing_dataset$Price <- as.integer(housing_dataset$Price)

str(housing_dataset$Price)
summary(housing_dataset$Price)

housing_dataset$Landsize <- as.integer(housing_dataset$Landsize)
housing_dataset$BuildingArea <- as.integer(housing_dataset$BuildingArea)

str(housing_dataset$Landsize)
summary(housing_dataset$Landsize)

str(housing_dataset$BuildingArea)
summary(housing_dataset$BuildingArea)

housing_dataset$Rooms <- as.integer(housing_dataset$Rooms)
housing_dataset$Bathroom <- as.integer(housing_dataset$Bathroom)
housing_dataset$Car <- as.integer(housing_dataset$Car)

str(housing_dataset$Rooms)
summary(housing_dataset$Rooms)

str(housing_dataset$Bathroom)
summary(housing_dataset$Bathroom)

str(housing_dataset$Car)
summary(housing_dataset$Car)

housing_dataset$YearBuilt <- as.integer(housing_dataset$YearBuilt)
str(housing_dataset$YearBuilt)
summary(housing_dataset$YearBuilt)

housing_dataset$Distance <- as.numeric(housing_dataset$Distance)

str(housing_dataset$Distance)
summary(housing_dataset$Distance)

housing_dataset$Regionname <- factor(housing_dataset$Regionname)

levels(factor(housing_dataset$Regionname))
str(housing_dataset$Regionname)
summary(housing_dataset$Regionname)

housing_dataset$Propertycount <- factor(housing_dataset$Propertycount)

levels(factor(housing_dataset$Propertycount))

str(housing_dataset$Propertycount)
summary(housing_dataset$Propertycount)

housing_dataset[is.na(housing_dataset$Distance), ]
housing_dataset[is.na(housing_dataset$Regionname), ]
housing_dataset[is.na(housing_dataset$Propertycount), ]

summary(housing_dataset$Regionname[housing_dataset$Distance == 5.1])

housing_dataset[18524, "Regionname"] <- "Western Metropolitan"

summary(housing_dataset$Regionname[housing_dataset$Distance == 7.7])

housing_dataset[26889, "Regionname"] <- "Southern Metropolitan"

summary(housing_dataset$Propertycount[housing_dataset$Distance == 5.1 &
                                      housing_dataset$Regionname == "Western Metropolitan"])

housing_dataset[18524, "Price"]

summary(housing_dataset$Price[housing_dataset$Distance == 5.1 &
                              housing_dataset$Regionname == "Western Metropolitan" &
                              housing_dataset$Propertycount == 7570])

summary(housing_dataset$Price[housing_dataset$Distance == 5.1 &
                              housing_dataset$Regionname == "Western Metropolitan" &
                              housing_dataset$Propertycount == 2417])

housing_dataset[18524, "Propertycount"] <- 7570

summary(housing_dataset$Propertycount[housing_dataset$Distance == 7.7 &
                                      housing_dataset$Regionname == "Southern Metropolitan"])

housing_dataset[26889, "Price"]

summary(housing_dataset$Price[housing_dataset$Distance == 7.7 &
                              housing_dataset$Regionname == "Southern Metropolitan" &
                              housing_dataset$Propertycount == 8920])

summary(housing_dataset$Price[housing_dataset$Distance == 7.7 &
                              housing_dataset$Regionname == "Southern Metropolitan" &
                              housing_dataset$Propertycount == 8989])

housing_dataset[26889, "Propertycount"] <- 8989

housing_dataset[29484, ]

housing_dataset <- housing_dataset[-c(29484), ]

summary(housing_dataset)

quantile(housing_dataset$Price, 0.99, na.rm = TRUE)
quantile(housing_dataset$Price, 0.01, na.rm = TRUE)

library(ggplot2)
library(scales)

ggplot(data = housing_dataset, aes(x = Price)) +
  geom_histogram(bins = 100) +
  coord_cartesian(xlim = c(0, 5000000)) +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))

housing_dataset <- housing_dataset[(housing_dataset$Price <= quantile(housing_dataset$Price, 0.99, na.rm = TRUE) |
                                    is.na(housing_dataset$Price)), ]
housing_dataset <- housing_dataset[(housing_dataset$Price >= quantile(housing_dataset$Price, 0.01, na.rm = TRUE) |
                                    is.na(housing_dataset$Price)), ]

quantile(housing_dataset$Landsize, 0.99, na.rm = TRUE)
quantile(housing_dataset$Landsize, 0.01, na.rm = TRUE)

ggplot(data = housing_dataset, aes(x = Landsize)) +
  geom_histogram(bins = nrow(housing_dataset)) +
  coord_cartesian(xlim = c(0, 500), ylim = c(0, 5000))

housing_dataset <- housing_dataset[housing_dataset$Landsize > 0 | is.na(housing_dataset$Landsize), ]

quantile(housing_dataset$Landsize, 0.99, na.rm = TRUE)
quantile(housing_dataset$Landsize, 0.01, na.rm = TRUE)

housing_dataset <- housing_dataset[(housing_dataset$Landsize < quantile(housing_dataset$Landsize, 0.99, na.rm = TRUE) |
                                    is.na(housing_dataset$Landsize)), ]
housing_dataset <- housing_dataset[(housing_dataset$Landsize > quantile(housing_dataset$Landsize, 0.01, na.rm = TRUE) |
                                    is.na(housing_dataset$Landsize)), ]

quantile(housing_dataset$BuildingArea, 0.99, na.rm = TRUE)
quantile(housing_dataset$BuildingArea, 0.01, na.rm = TRUE)

ggplot(data = housing_dataset, aes(x = BuildingArea)) +
  geom_histogram(bins = nrow(housing_dataset)) +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 300))

ggplot(data = housing_dataset, aes(x = BuildingArea)) +
  geom_histogram(bins = nrow(housing_dataset)) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 100))

housing_dataset <- housing_dataset[housing_dataset$BuildingArea > 5 | is.na(housing_dataset$BuildingArea), ]

quantile(housing_dataset$BuildingArea, 0.99, na.rm = TRUE)
quantile(housing_dataset$BuildingArea, 0.01, na.rm = TRUE)

housing_dataset <- housing_dataset[(housing_dataset$BuildingArea < quantile(housing_dataset$BuildingArea, 0.99, na.rm = TRUE) |
                                    is.na(housing_dataset$BuildingArea)), ]
housing_dataset <- housing_dataset[(housing_dataset$BuildingArea > quantile(housing_dataset$BuildingArea, 0.01, na.rm = TRUE) |
                                    is.na(housing_dataset$BuildingArea)), ]

housing_dataset <- housing_dataset[housing_dataset$BuildingArea < housing_dataset$Landsize |
                                  is.na(housing_dataset$BuildingArea) |
                                  is.na(housing_dataset$Landsize), ]

quantile(housing_dataset$Rooms, 0.99, na.rm = TRUE)
quantile(housing_dataset$Rooms, 0.01, na.rm = TRUE)

housing_dataset <- housing_dataset[housing_dataset$Rooms <= quantile(housing_dataset$Rooms, 0.99, na.rm = TRUE), ]
housing_dataset <- housing_dataset[housing_dataset$Rooms >= quantile(housing_dataset$Rooms, 0.01, na.rm = TRUE), ]

quantile(housing_dataset$Bathroom, 0.99, na.rm = TRUE)
quantile(housing_dataset$Bathroom, 0.01, na.rm = TRUE)

housing_dataset <- housing_dataset[(housing_dataset$Bathroom <= quantile(housing_dataset$Bathroom, 0.99, na.rm = TRUE) | is.na(housing_dataset$Bathroom)), ]
housing_dataset <- housing_dataset[(housing_dataset$Bathroom >= quantile(housing_dataset$Bathroom, 0.01, na.rm = TRUE) | is.na(housing_dataset$Bathroom)), ]

quantile(housing_dataset$Car, 0.99, na.rm = TRUE)
quantile(housing_dataset$Car, 0.01, na.rm = TRUE)

housing_dataset <- housing_dataset[(housing_dataset$Car <= quantile(housing_dataset$Car, 0.99, na.rm = TRUE) |
                                  is.na(housing_dataset$Car)), ]
housing_dataset <- housing_dataset[(housing_dataset$Car >= quantile(housing_dataset$Car, 0.01, na.rm = TRUE) |
                                  is.na(housing_dataset$Car)), ]

quantile(housing_dataset$YearBuilt, 0.99, na.rm = TRUE)
quantile(housing_dataset$YearBuilt, 0.01, na.rm = TRUE)

housing_dataset <- housing_dataset[(housing_dataset$YearBuilt <= quantile(housing_dataset$YearBuilt, 0.99, na.rm = TRUE) |
                                  is.na(housing_dataset$YearBuilt)), ]
housing_dataset <- housing_dataset[(housing_dataset$YearBuilt >= quantile(housing_dataset$YearBuilt, 0.01, na.rm = TRUE) |
                                  is.na(housing_dataset$YearBuilt)), ]

summary(housing_dataset)

library(mice)
init <- mice(housing_dataset, maxit = 0)

meth <- init$method
meth

meth[c("YearBuilt", "BuildingArea")] <- ""

pred_m <- quickpred(housing_dataset, mincor = 0.2)
pred_m

pred_m[, c("Date", "YearBuilt", "BuildingArea")] <- 0

imp <- mice(data = housing_dataset, method = meth, predictorMatrix = pred_m)
housing_dataset <- complete(imp)

housing_dataset$BuildingArea <- NULL
housing_dataset$YearBuilt <- NULL
summary(housing_dataset)

summary(housing_dataset)

house_percentage <- nrow(housing_dataset[housing_dataset$Type == "House", ]) / nrow(housing_dataset)
ud_percentage <- nrow(housing_dataset[housing_dataset$Type == "Unit/Duplex", ]) / nrow(housing_dataset)
townhouse_percentage <- nrow(housing_dataset[housing_dataset$Type == "Townhouse", ]) / nrow(housing_dataset)

pie_slices <- c(house_percentage, ud_percentage, townhouse_percentage)
pie_labels <- c("House", "Unit/Duplex", "Townhouse")
piechart_data <- data.frame(pie_labels, pie_slices)

ggplot(data = piechart_data, aes(x = "", y = pie_slices, fill = pie_labels)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Property Type Distribution") +
  labs(fill = "Types")

ggplot(data = housing_dataset, aes(y = Regionname, fill = Type)) +
  geom_bar(position = "fill", width = 0.8) +
  theme_bw() +
  labs(title = "Property Distribution per Region") +
  labs(x = "Percentage") + labs(y = "Region Name")

ggplot(data = housing_dataset, aes(x = Landsize)) +
  geom_histogram(color = "black", bins = 25, fill = "orange") +
  theme_classic() +
  labs(title = "Landsize Histogram") + labs(y = "Count")

ggplot(data = housing_dataset, aes(x = Distance, y = Price)) + geom_point(aes(color = Type), size = 2.5, alpha = 0.05) +
  facet_grid(rows = housing_dataset$Type) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = "none") +
  labs(title = "Price Distribution in Distance to City Center")

ggplot(data = housing_dataset, aes(x = Price)) +
  geom_histogram(color = "black", bins = 25, fill = "beige") +
  geom_vline(xintercept = round(mean(housing_dataset$Price)), color = "cyan3", linewidth = 1) +
  geom_vline(xintercept = round(median(housing_dataset$Price)), color = "purple", linewidth = 1) +
  geom_vline(xintercept = round(mean(housing_dataset$Price) + sd(housing_dataset$Price)), color = "darkseagreen4", linewidth = 1) +
  geom_vline(xintercept = round(mean(housing_dataset$Price) - sd(housing_dataset$Price)), color = "darkseagreen4", linewidth = 1) +
  annotate(geom = "label", x = round(mean(housing_dataset$Price)), y = 4800, label = paste("ST-Dev: ±", round(sd(housing_dataset$Price))), fill = "darkseagreen4", size = 3.5) +
  annotate(geom = "label", x = round(mean(housing_dataset$Price)) + 360000, y = 3800, label = paste("Mean:", round(mean(housing_dataset$Price))), fill = "cyan3", size = 3.5) +
  annotate(geom = "label", x = round(median(housing_dataset$Price)) + 370000, y = 4300, label = paste("Median:", round(median(housing_dataset$Price))), fill = "purple", size = 3.5) +
  scale_x_continuous(breaks = round(seq(0, 5000000, by = 250000), 1), labels = unit_format(unit = "", scale = 1e-6)) +
  labs(title = "Price Histogram") + labs(y = "Count") + labs(x = "Price (Million)")

quantile(housing_dataset$Price, 0.25)
quantile(housing_dataset$Price, 0.75)

lowpriced_properties <- housing_dataset[housing_dataset$Price < quantile(housing_dataset$Price, 0.25), ]
midpriced_properties <- housing_dataset[housing_dataset$Price >= quantile(housing_dataset$Price, 0.25) &
                                       housing_dataset$Price <= quantile(housing_dataset$Price, 0.75), ]
highpriced_properties <- housing_dataset[housing_dataset$Price > quantile(housing_dataset$Price, 0.75), ]

summary(lowpriced_properties)

summary(midpriced_properties)

summary(highpriced_properties)

ggplot(data = housing_dataset, aes(x = Price, y = "")) + geom_boxplot() +
  facet_grid(rows = housing_dataset$Type) +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(title = "Prices for Property Types") + labs(y = "")

round(cor(housing_dataset[, c("Price", "Landsize", "Rooms", "Bathroom", "Car", "Distance")]), 2)[, 1]

knitr::purl(input = "melbourne-housing.Rmd", output = "melbourne-housing-no-doc.R", documentation = 0)
file.rename(from = "melbourne-housing.md", to = "README.md")
