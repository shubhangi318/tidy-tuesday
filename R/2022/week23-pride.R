library(tidytuesdayR)
library(ggplot2)
library(scales)
library(extrafont)
library(png)
library(ggpattern)
font_import()
loadfonts(device = "win")

# Importing dataset
tuesdata <- tidytuesdayR::tt_load('2022-06-07')
tuesdata <- tidytuesdayR::tt_load(2022, week = 23)

# Subsetting required data
allcos <- tuesdata$static_list
allcos <- data.frame(allcos)
allcos

# Renaming columns 
colnames(allcos) <- c("company", "pride", "pledge", "amt", "politician", "state")
head(allcos)

# Sorting by amt and subsetting top 10 cos
allcos <- allcos[order(allcos$amt, decreasing = TRUE), ]
topten <- allcos[c(2:11), ]
total <- allcos[1, ]

# Check % of top ten in total
(sum(topten$amt)/total$amt)*100
# Just 10 firms constitute 48% of total amount donated to anti-LGBTQ+ politicians.

# Convert to thousands, add anti pride flag
topten$amtths <- topten$amt/1000
img <- "C:/Users/Shubhangi Bhatia/Downloads/flag.jpg"
img_paths <-  rep(img, 10)

# Mark firms that have also pledged
topten$company <-  ifelse(topten$pledge == TRUE,  
                          paste0(topten$company, "*"),
                          topten$company)
#######
# Plot
#######
          
ggplot(data = topten, aes(y = reorder(company, amtths),
                          x = amtths,
                          pattern_filename = reorder(company, amtths))) + 
  geom_col_pattern( pattern = 'image',
                    pattern_fill="white",
                    fill="white",
                    pattern_type = 'expand',
                    pattern_aspect_ratio = 1) +
  theme_classic() +
  scale_pattern_filename_discrete(choices = img_paths) +  
  xlab("Amount in thousands") +
  ylab("") + 
  ggtitle("Just 10 firms constitute 48% of the total amount donated to \nanti-LGBTQ+ politicians") +
  scale_x_continuous(expand = c(0,0), limits = c(0, 700)) +
  geom_text(aes(label = paste0("$", round(amtths,1))), hjust = -0.05, size = 6) +
  theme_classic() +
  theme(legend.position="none",
        plot.background = element_rect(fill = "thistle1", colour = "black", size = 4),
        panel.background = element_rect(fill = "thistle1"),
        text = element_text(family = "garamond"),
        plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(t = 0.3, unit = "in")),
        plot.caption = element_text(size = 18, hjust = 0.99),
        axis.title.x = element_text(size= 17),
        axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17, face = "bold")) +
  labs(caption = "Data as of 2021-22. \nCompanies with * have also signed the HRC pledge opposing Anti-LGBTQ legislation.\n Source: Data for Progress | Graphic: Shubhangi Bhatia")

# Saving plot
ggsave("../../plots/week23_pride.png",
       width = 14, height = 10, dpi = 80, units = "in")

