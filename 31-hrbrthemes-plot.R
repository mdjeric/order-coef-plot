#######################################
#                                     #
#     THIS IS HRBRTHEMES PLOT         #
#                                     #
#######################################

## USE THE FUNCTION ------------------------------------------------------
# to sort the factors for plotting 
MN <- sort_models(MN)

levels(MN$var)[10] <- "Metrop. area"  # fix only one label

### Whole models significances -------------------------------------------
# create variable for significance of each variable in the model, and 
# manually inspect which do not reach level of significance
# and mark them appropriately (with FALSE) - data from Anova(model)

MN$sig <- TRUE
MN$sig[MN$var == "Immigrant"] <- FALSE
MN$sig[(MN$var == "Veteran") &
         (MN$model == "Large")] <- FALSE
MN$sig[(MN$var == "Parents' education") &
         (MN$model == "Large")] <- FALSE
MN$sig[(MN$var == "Metrop. area") &
         (MN$model == "Large")] <- FALSE
MN$sig[(MN$var == "Region") &
         (MN$model == "Large")] <- FALSE
MN$sig[(MN$var == "Parents' education") &
         (MN$model == "W/ childhood")] <- FALSE
MN$sig[(MN$var == "Metrop. area") &
         (MN$model == "W/ childhood")] <- FALSE

### Variables we are interested in --------------------------------------
# create data frame that will shade boxes behind variables that 
# we are interested looking at
#
# Because of faceting, instead of selecting the ones we are interested in,
# we are just going to make the ones we are not interested in as missing
# while the ones we want to highlighy are going to have inf. values

intrst <- data.frame(var = unique(MN$var),
                     xmn = NA,
                     xmx = NA,
                     ymn = NA,
                     ymx = NA
                     )

intrst[ 9, 2:5] <- c(-Inf, Inf, -Inf, Inf)
intrst[10, 2:5] <- c(-Inf, Inf, -Inf, Inf)



##### COMBINE ALL ELEMENTS ============================================

update_geom_font_defaults() # sometimes it works, sometimes it doesn't

p_n <- ggplot(data = MN, aes(x = reorder(factor, ordr),
                             y = AME,
                             ymin = lower,
                             ymax = upper,
                             colour = model,
                             alpha = sig))

# add marginal effects
p_n <- p_n + geom_pointrange(position = position_dodge(width = 0.5),
                             shape = 21,
                             fill = "white") +
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))

# make vars. that do not reach stat.sig. transparent
p_n <- p_n + scale_alpha_discrete(range = c(0.5, 1.0))


# add intercept line
p_n <- p_n + geom_hline(yintercept = 0,
                        color = "dark gray")

# rotate plot and add some labels (including ref. categories)
p_n <- p_n +
  coord_flip() +
  labs(x = NULL,
       y = "Average Marginal Effect",
       colour = "Models",
       alpha = "Variable sig at p<.05",
       title = "Regression on criteria of national belonging (openess)",
       subtitle = "Focusing on the effect of curent and childhood place of residence",
       caption = "Reference categories are: Moderate or Liberal Protestants; Moderate political views; Midwest; Childhood in medium city;
       Education less than high school; Metrop. area city (top 100); Race white.")

# add facets according to model variables
# order of levels is going to take care of variable order
p_n <- p_n + facet_grid(var ~ .,
                        scales = "free_y",
                        space = "free_y",
                        switch = "y"
)

# fix the appearance of the plot
p_n <- p_n +
  theme_ipsum_tw(plot_margin = margin(0, 0, 0, 0)) +
  theme(strip.text.y = element_text(angle = 180,
                                    vjust = 1,
                                    hjust = 0,
                                    margin = margin(0, 0, 0, 0)
                                    ),
        strip.background = element_blank(),
        strip.placement = "outside",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        legend.position = "bottom",
        axis.text.y = element_text()
        )

# add rectangles to highlight variables we are interested in
p_n <- p_n + geom_rect(data = intrst, aes(xmin = xmn,
                                          xmax = xmx,
                                          ymin = ymn,
                                          ymax = ymx),
                       alpha = 0.09,
                       fill = "black",
                       inherit.aes = FALSE,
                       show.legend = FALSE
                       )


# fix the position of the title
p_n_fix <- ggplotGrob(p_n)
p_n_fix$layout$l[p_n_fix$layout$name == "title"] <- 2
p_n_fix$layout$l[p_n_fix$layout$name == "subtitle"] <- 2

grid::grid.draw(p_n_fix) #note: clean previous plot from pane