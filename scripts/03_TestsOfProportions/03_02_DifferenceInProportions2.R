# Packages ----------------------------------------------------------------

library(statsr)
library(dplyr)
library(ggplot2)
library(ggpubr)

# Data ----------------------------------------------------------

data(atheism)

us <-
        atheism |> 
        filter(nationality == "United States")

spain <- atheism |> 
        filter(nationality == "Spain")

us_tb <- 
        with(us, table(year, response))

spain_tb <-
        with(spain, table(year, response))

# Test of Proportion ------------------------------------------------------

# We reject the Null Hypothesis, which implies there has been
# a change in the proportion of atheists in the US, from year 2005 to 
# 2012. 
us_test <-
        prop.test(us_tb)


# We fail to reject the Null Hypothesis, which implies there has not been
# a significant change in the proportion of atheists in Spain, 
# from year 2005 to 2012. 
spain_test <-
        prop.test(spain_tb)

# Barplots ----------------------------------------------------------------

us_barplot <- 
ggplot(data = us, mapping = aes(x = year, fill = response)) +
        geom_bar() +
        labs(
                title = "Proportion of Atheists in the US",
                x = "Year",
                y = "Responses",
                fill = "Religion"
        ) +
        scale_x_continuous(breaks = c(2004, 2012)) +
        scale_fill_manual(values = c("red3", "blue4")) +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey"),
                legend.title = element_text(face = "bold", 
                                            color = "dimgrey")
        ) + guides(fill = "none")

spain_barplot <- 
ggplot(data = spain, mapping = aes(x = year, fill = response)) +
        geom_bar() +
        labs(
                title = "Proportion of Atheists in Spain",
                x = "Year",
                y = "Responses",
                fill = "Religion"
        ) +
        scale_x_continuous(breaks = c(2004, 2012)) +
        scale_fill_manual(values = c("red3", "blue4")) +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey"),
                legend.title = element_text(face = "bold", 
                                            color = "dimgrey"),
                legend.text = element_text(color = "dimgrey")
        ) 

ggarrange(us_barplot, spain_barplot, nrow = 1, ncol = 2,
          common.legend = T, legend = "bottom")
