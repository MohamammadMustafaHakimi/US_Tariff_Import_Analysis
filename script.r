# 1. installing and loading the necessary libraries
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("plotly")
install.packages("tidyr")
library(dplyr)
library(ggplot2)
library(readxl)
library(plotly)
library(tidyr)

# 2. loading the dataset
tarif_rate_data <- read.csv(file = "WB_WDI_TM_TAX_MRCH_WM_FN_ZS.csv")
View(tarif_rate_data)

imports_data_us_g_customs_value <- read_excel("DataWeb-Query-Export.xlsx", sheet = 2)
imports_data_us_g_cif_value <- read_excel("DataWeb-Query-Export.xlsx", sheet = 3)
imports_data_us_g_customs_value <- imports_data_us_g_customs_value %>% arrange(Country, Year)
imports_data_us_g_cif_value <- imports_data_us_g_cif_value %>% arrange(Country, Year)
imports_data_us_g_cif_value <- imports_data_us_g_cif_value %>% mutate(Description_1.0 = Description)

# temporary fix for the joined table
cif <- imports_data_us_g_cif_value %>% select(
  Country, Year, `HTS Number`, Description, `General CIF Imports Value`
)

joined_data_us_g_customs_value_g__value <- joined_data_us_g_customs_value_g__value %>% mutate(
  General_Customs_Value = imports_data_us_g_customs_value$`General Customs Value`
)


View(imports_data_us_g_customs_value)
View(imports_data_us_g_cif_value)
View(joined_data_us_g_customs_value_g__value)
# 3. cleaning the data
# a) checking for na value
## checking the total number of na values in the dataset
sum(is.na(tarif_rate_data)) # zero


for (i in names(tarif_rate_data)) {
  print(paste(i, "has", sum(is.na(tarif_rate_data[[i]])), "NA values"))
}

for (i in names(joined_data_us_g_customs_value_g__value)) {
  print(paste(i, "has", sum(is.na(joined_data_us_g_customs_value_g__value[[i]])), "NA values"))
}
# the majority of the na values belong to the COMMENT_OBS column which is not relvant for our analysis

# b) finding number of duplicated data
duplicated <- sum(duplicated(tarif_rate_data))
duplicated

# the data set does not contain any duplicated row or column

# 4. selecting the relevant columns
test_tarif_data <- tarif_rate_data %>% select(REF_AREA_ID, REF_AREA_NAME, TIME_PERIOD, OBS_VALUE)

test_imports_data <- joined_data_us_g_customs_value_g__value %>% select(Country, Year, `HTS Number.x`, Description.x, `General CIF Imports Value`, `General Customs Value`)

# 5. sorting by coutries, and descending time
test_tarif_data <-  test_tarif_data %>% arrange(REF_AREA_NAME, desc(TIME_PERIOD))


# 6. the analysis
## 1. checking the change in tariff rate of different countries over time

# Create the ggplot with facets for each country
p <- ggplot(test_tarif_data) +
  geom_line(mapping = aes(x = OBS_VALUE, y = TIME_PERIOD)) +
  facet_wrap(~REF_AREA_NAME, scales = "free", ncol = 1) +  # One column, scrollable facets
  ggtitle("Time Series Data by Country") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Convert the ggplot to an interactive plotly plot
p_interactive <- ggplotly(p)

# Display the interactive plot33
p_interactive


# 2. individual country tariff data
us_afg <- test_tarif_data %>% filter(REF_AREA_NAME == "Afghanistan")
ggplot(data = us_afg) +
  geom_line(mapping = aes(x = TIME_PERIOD, y = OBS_VALUE))

# creating a function that plots the target country's tariff data
us_xyz_tariff <- function(country_name) {
  us_country <- test_tarif_data %>% filter(REF_AREA_NAME == country_name)
  s_plot <- (ggplot(data = us_country) +
    geom_line(mapping = aes(x = TIME_PERIOD, y = OBS_VALUE)) +
      theme_classic() +
      theme(legend.title = element_text(color = "brown", size = 8),
            plot.title = element_text(
              face = 'bold',
              hjust = .5,
              color = 'lightblue',
              size = 12),
            legend.position = "bottom",
            legend.text = element_text(size=5),
            legend.key.height = unit(0.4, 'cm'),
            legend.key.width = unit(0.4, 'cm')

          ) +
      ggtitle(paste('USA tariff on', country_name)) +
      xlab('Years') +
      ylab('OBS Value')
    )
  return(ggplotly(s_plot))
}

# example useage for the function
us_xyz_tariff("Afghanistan")
us_xyz_tariff("Canada")
us_xyz_tariff("Mexico")
us_xyz_tariff("Germany")
us_xyz_tariff("China")
us_xyz_tariff("Russia")

# 3. finding the average of tariffs of countries
avg_us_tariff <- test_tarif_data %>%
  group_by(REF_AREA_NAME) %>%
  summarise(avg_tariff = mean(OBS_VALUE, na.rm = TRUE))
# sorted by descending average tariffs
desc_avg_us_tariff <- avg_us_tariff %>% arrange(desc(avg_tariff))
View(desc_avg_us_tariff)

# sorted by ascending average tariffs
asc_avg_us_tariff <- avg_us_tariff %>% arrange(avg_tariff)
View(asc_avg_us_tariff)

# 20 countries who have the most tariffs imposed on them by the us
twenty_highest_tariffs <- avg_us_tariff %>% arrange(desc(avg_tariff)) %>% slice(1:20)
# plotting the data
plot <- ggplot(data = twenty_highest_tariffs) +
  geom_col(mapping = aes(x = avg_tariff, y = REF_AREA_NAME, fill = REF_AREA_NAME))
ggplotly(plot)
# 20 countries who have the least tariffs imposed on them by the us
twenty_lowest_tariffs <- avg_us_tariff %>% arrange(avg_tariff) %>% slice(1:20)
#plotting the data
plot2 <- ggplot(data = twenty_lowest_tariffs) +
  geom_col(mapping = aes(x = avg_tariff, y = REF_AREA_NAME, fill = REF_ARE_NAME))
