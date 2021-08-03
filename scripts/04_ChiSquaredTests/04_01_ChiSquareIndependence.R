# The samhda (UsingR) data set contains survey data on 590 children. The variables
# gender and amt.smoke contain information about the gender of the participant
# and how often the participant smoked in the last month. Are the two
# variables independent?


# Packages ----------------------------------------------------------------

library(UsingR)
library(ggplot2)


# Data --------------------------------------------------------------------

data(samhda)

df <- samhda |> 
        select(gender, amt.smoke) |> 
        filter(amt.smoke < 98 & gender !=7) |> 
        mutate(gender = ifelse(gender == 1, "Male", "Female"))

tb <- xtabs(data = samhda, formula = ~ gender + amt.smoke, 
            subset = amt.smoke < 98 & gender != 7)


# Barchart ----------------------------------------------------------------

ggplot(data = df, mapping = aes(x = amt.smoke, fill = gender)) +
        geom_bar() +
        labs(
                title = "Smoking Category by Gender",
                x = "Smoking Category",
                y = "Count",
                fill = "Gender"
        ) +
        facet_wrap(~ gender) +
        scale_fill_brewer(type = "qual", palette = 6, 
                          direction = -1) +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey"),
                strip.text = element_text(face = "bold"),
                legend.title = element_text(face = "bold",
                                            color = "dimgrey")
        )


# Chi Square Test: Independence -------------------------------------------

test <-
        chisq.test(tb)

