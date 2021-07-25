# Packages ----------------------------------------------------------------

require(ggplot2)
require(ggthemes)
require(dplyr)

# Data Wrangling ----------------------------------------------------------

data(sleep)

df <-
        
        tibble(
                "ID" = sleep |>
                        select(ID) |>
                        unique() |>
                        unlist() |>
                        as.numeric(),
                "Before" = sleep |>
                        filter(group == 1) |>
                        select(extra) |>
                        unlist() |>
                        as.numeric()
                ,
                "After" = sleep |>
                        filter(group == 2) |>
                        select(extra) |>
                        unlist() |>
                        as.numeric()
                ,
        ) |> 
        mutate(
                Difference = After - Before
        )


# Comparison Plot --------------------------------------------------------------------


ggplot(data = sleep, aes(x = group, y = extra, color = ID, group = ID)) + 
        geom_path() +
        geom_point(size = 4) +
        scale_y_continuous(breaks = -1:5) +
        labs(
                title = "Increase in Hours of Sleep",
                x = "Group",
                y = "Hours of Sleep"
        ) +
        theme_gdocs() +
        theme(
                plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold")
        ) +
        guides(col = "none")



# Student's Paired T-Test -------------------------------------------------


t <-
        t.test(
                x = df$After,
                y = df$Before,
                alternative = "two.sided",
                mu = 0,
                paired = T,
                conf.level = 0.95
        )


