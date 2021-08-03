# Packages ----------------------------------------------------------------

library(ggplot2)

# Data --------------------------------------------------------------------

# Data about a Painkiller's effect on a patient's health status
# (Death or No Death).

# H0: Proportion of dead patients that have been dosed
# a certain painkiller is the same as those who haven't been
# dosed.

tb <- matrix(data = c(34, 40, 1350, 217),
             nrow = 2,
             ncol = 2) |>
        as.table() |>
        `dimnames<-`(list(#Rownames
                c("Not Dosed", "Dosed"),
                #Colnames
                c("Dead", "Not Dead")))

df <- as.data.frame(tb) |>
        `colnames<-`(c("Painkiller", "Status", "Count"))


# Bar Chart ---------------------------------------------------------------

ggplot(data = df, mapping = aes(x = Status, y = Count)) +
        geom_bar(stat = "identity", fill = "skyblue",
                 color = "gray") +
        labs(title = "Death Ratio filtered by Painkiller Dosis",
             x = "Health Status",
             y = "Count") +
        facet_wrap(. ~ Painkiller) +
        theme_test() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.text.x = element_text(face = "bold",
                                           color = "red"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.text.y = element_text(color = "black"),
                strip.text = element_text(color = "white",
                                          face = "bold"),
                strip.background = element_rect(fill = "dimgrey")
        ) 
# Difference in Proportions -----------------------------------------------

test <-
        prop.test(tb, correct = T)

# As we would have expected by the barplot, we strongly reject the
# Null Hypothesis, as the two proportions don't seem to be
# relatively close to each other.
