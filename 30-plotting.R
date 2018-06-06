#######################################
#                                     #
#     THIS IS REGULAR THEME PLOT      #
#                                     #
#######################################


## USE THE FUNCTION ------------------------------------------------------
# to sort the factors for plotting 
MB <- sort_models(MB)

## Rename ----------------------------------------------------------------
# the variables, so they include reference category in new line

levels(MB$var)
levels(MB$var)[1] <- "Religion\n[ref: mod. or lib. prot.]"
levels(MB$var)[2] <- "Political views\n[ref: moderate]"
levels(MB$var)[3] <- "Region\n[ref: Midwest]"
levels(MB$var)[4] <- "Childhood\n[ref: medium city]"
levels(MB$var)[5] <- "Education\n[ref: LT HS]"
levels(MB$var)[10] <- "Metrop. area\n[ref: city (top 100)]"
levels(MB$var)[13] <- "Race\n[ref: white]"

### Whole models significances -------------------------------------------
# create variable for significance of each variable in the model, and 
# manually inspect which do not reach level of significance
# and mark them appropriately (with FALSE)

MB$sig <- TRUE
Anova(clm_full)
# not sig.
# full - immigrant, veteran, parent BA, srcbelt, region
Anova(clm_small)
# not sig.
# small - immigrant, 
Anova(clm_small_res16)
# not sig.
# small, res16 - immigrant, prt.ba, srcbelt

MB$sig[MB$var == "Immigrant"] <- FALSE
MB$sig[(MB$var == "Veteran") &
       (MB$model == "Large")] <- FALSE
MB$sig[(MB$var == "Parents' education") &
       (MB$model == "Large")] <- FALSE
MB$sig[(MB$var == "Metrop. area\n[ref: city (top 100)]") &
       (MB$model == "Large")] <- FALSE
MB$sig[(MB$var == "Region\n[ref: Midwest]") &
       (MB$model == "Large")] <- FALSE
MB$sig[(MB$var == "Parents' education") &
       (MB$model == "W/ childhood")] <- FALSE
MB$sig[(MB$var == "Metrop. area\n[ref: city (top 100)]") &
       (MB$model == "W/ childhood")] <- FALSE


### Variables we are interested in --------------------------------------
# create data frame that will shade boxes behind variables that 
# we are interested looking at
#
# Because of faceting, instead of selecting the ones we are interested in,
# we are just going to make the ones we are not interested in as missing
# while the ones we want to highlighy are going to have inf. values

intrst <- data.frame(var = unique(MB$var),
                     xmn = NA,
                     xmx = NA,
                     ymn = NA,
                     ymx = NA
                     )

intrst[ 9, 2:5] <- c(-Inf, Inf, -Inf, Inf)
intrst[10, 2:5] <- c(-Inf, Inf, -Inf, Inf)


##### COMBINE ALL ELEMENTS =======================================

# reordering factor created is used to reorder the values
p_m <- ggplot(data = MB, aes(x = reorder(factor, ordr),
                             y = AME,
                             ymin = lower,
                             ymax = upper,
                             colour = model,
                             alpha = sig)
              )

# add marginal effects
p_m <- p_m +
  geom_pointrange(position = position_dodge(width = 0.5),
                  shape = 21,
                  fill = "white"
                  ) +
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))

# make vars. that do not reach stat.sig. transparent
p_m <- p_m + scale_alpha_discrete(range = c(0.5, 1.0))

# add intercept line
p_m <- p_m + geom_hline(yintercept = 0,
                        color = "dark gray")

# rotate plot and add some labels
p_m <- p_m +
  coord_flip() +
  labs(x = NULL,
       y = "Average Marginal Effect",
       colour = "Models",
       alpha = "Variable\nsig at p<.5",
       title = "Regression on criteria of national belonging (openess)")

# add facets according to model variables
# order of levels is going to take care of variable order
p_m <- p_m + facet_grid(var ~ . ,           
                        scales = "free_y",
                        space = "free_y",
                        switch = "y")

# fix the appearance of the plot
p_m <- p_m + 
  theme(strip.text.y = element_text(angle = 180),
        strip.placement = "outside",
        strip.background = element_rect(fill = "#d9d9d9", color = NA),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "#d9d9d9"),
        panel.grid.minor.x = element_line(colour = "#d9d9d9"),
        panel.grid.major.y = element_line(colour = "#d9d9d9"),
        panel.grid.minor.y = element_line(colour = "#d9d9d9"),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.title = element_text(size = 10),
        axis.title =  element_text(size = 10),
        title = element_text(size = 11)
  )

# add rectangles to highlight variables we are interested in
p_m <- p_m + geom_rect(data = intrst,
                       aes(xmin = xmn,
                           xmax = xmx,
                           ymin = ymn,
                           ymax = ymx),
                       alpha = 0.09,
                       fill = "black",
                       inherit.aes = FALSE,
                       show.legend = FALSE
                       )
# finished plot
p_m

# make it even better - move title to the left-most corner

p_m_fix <- ggplotGrob(p_m)
p_m_fix$layout$l[p_m_fix$layout$name == "title"] <- 2
grid::grid.draw(p_m_fix) #note: clean previous plot from pane