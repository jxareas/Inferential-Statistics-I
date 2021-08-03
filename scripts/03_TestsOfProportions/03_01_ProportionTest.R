# The samhda (UsingR) data set contains information on marijuana usage
# among children as collected at the Substance Abuse and Mental Health Data
# Archive. The variable marijuana indicates whether the individual has ever
# tried marijuana. A 1 means yes, a 2 no. If it used to be that 50% of the target
# population had tried marijuana, does this data indicate an increase in marijuana
# usage ? Do a significance test of proportions to decide.

# Packages ----------------------------------------------------------------

library(UsingR)
library(ggplot2)
library(dplyr)

# Data --------------------------------------------------------------------

samhda <- samhda |> 
        filter(marijuana %in% 1:2)

proportions <- with(samhda, prop.table(table(marijuana)))

df <- 
        proportions |> 
        as.data.frame() |> 
        mutate(marijuana = case_when(
                marijuana == 1 ~ "Yes",
                marijuana == 2 ~ "No"
        )) |> 
        setNames(c("triedMarijuana", "frequency"))


# Barplot: Proportions ----------------------------------------------------

ggplot(data = df,
       mapping = aes(x = triedMarijuana,
                     y = frequency, fill = triedMarijuana)) +
        geom_bar(stat = "identity") +
        labs(
                title = "Marijuana usage among children",
                x = "Has tried Marijuana?",
                y = "Frequency"
        ) +
        scale_fill_brewer(type = "qual", palette = 2) +
        theme_classic() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.text.x = element_text(
                        face = "bold",
                        color = "black"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.text.y = element_text(
                        face = "bold",
                        color = "black")
        ) + guides(fill = "none") 


# Test of Proportion ------------------------------------------------------

test <-
        with(samhda, 
             prop.test(table(marijuana),
                       alternative = "greater",
                       p = .5,
                       conf.level = .95
                       )
             )

# We fail to reject the Null Hypothesis. 
# The usage of marijuana among children is less or equal to 0.5