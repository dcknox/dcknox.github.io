library(ggplot2)
library(data.table)
library(assertthat)

# functions ====================================================================

`%.%` <- paste0

# style ========================================================================

red <- '#A51C30'
lightred <- '#E16273'
blue <- '#4E84C4'
darkblue <- '#0C2C5C'
lightblue <- '#4C6C9C'



# test =========================================================================

n_minority <- 1000
n_white <- 1000

d_mean_minority <- 1/3
d_mean_white <- 2/3
d_sd <- .1

set.seed(19104)
d_minority <- rnorm(1000, mean = d_mean_minority, sd = d_sd)
d_minority <- pmax(0, d_minority)
d_minority <- pmin(1, d_minority)
d_white <- rnorm(1000, mean = d_mean_white, sd = d_sd)
d_white <- pmax(0, d_white)
d_white <- pmin(1, d_white)

c_minority <- mean(rbinom(1000, size = 1, d_minority))
c_white <- mean(rbinom(1000, size = 1, d_white))

mean(c_minority)
mean(c_white)



# plot =========================================================================

ds <- seq(0, 1, .001)

distrs <- rbind(
  data.table(
    race = 'minority',
    d = ds,
    p_d = dnorm(ds, mean = d_mean_minority, sd = d_sd)
  ),
  data.table(
    race = 'white',
    d = ds,
    p_d = dnorm(ds, mean = d_mean_white, sd = d_sd)
  )
)

png('proportionality_01.png', 1200, 900)
ggplot(distrs,
       aes(x = d,
           color = race,
           fill = race
           )
       ) +
  geom_line(aes(y = p_d * .05),
            size = 3
            ) +
  ## geom_ribbon(aes(ymin = 0,
  ##                 ymax = d * p_d * .05
  ##                 ),
  ##             size = 3,
  ##             color = NA
  ##             ) +
  ## # p(c|d)
  ## annotate('segment',
  ##          x = 0,
  ##          y = 0,
  ##          xend = 1,
  ##          yend = 1,
  ##          linetype = 'dashed'
  ##          ) +
  ## # p(s|d) v1
  ## annotate('segment',
  ##          x = 0,
  ##          y = 0,
  ##          xend = 1,
  ##          yend = .5,
  ##          linetype = 'dotted'
  ##          ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 0,
           yend = 1,
           size = 3,
           lineend = 'square'
           ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 1,
           yend = 0,
           size = 3,
           lineend = 'square'
           ) +
  scale_color_manual(
    values = c(minority = red,
               white = NA
               )
  ) +
  scale_fill_manual(
    values = c(minority = red %.% '80',
               white = NA
               )
  ) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0, 1),
                  ratio = 1
                  ) +
  xlab("Dangerousness of driving, d'") +
  ylab(NULL) +
  ## ylab('Probability of selection, ' * Pr(c[1] * '|' * d))) +
  ## annotate('text',
  ##          x = .5,
  ##          y = .5,
  ##          label = "'Prob. of crash, ' * Pr(c[1] * '|' * d)",
  ##          parse = TRUE,
  ##          family = 'Raleway',
  ##          angle = 45,
  ##          vjust = -0.1,
  ##          size = 12
  ##          ) +
  ## annotate('text',
  ##          x = .75,
  ##          y = .75 / 2,
  ##          label = "'Prob. of stop, ' * Pr(s[1] * '|' * d)",
  ##          parse = TRUE,
  ##          family = 'Raleway',
  ##          angle = atan(.5) / pi * 180,
  ##          vjust = -0.1,
  ##          size = 12
  ##          ) +
  theme_minimal(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none'
        )
dev.off()

png('proportionality_02.png', 1200, 900)
ggplot(distrs,
       aes(x = d,
           color = race,
           fill = race
           )
       ) +
  geom_line(aes(y = p_d * .05),
            size = 3
            ) +
  ## geom_ribbon(aes(ymin = 0,
  ##                 ymax = d * p_d * .05
  ##                 ),
  ##             size = 3,
  ##             color = NA
  ##             ) +
  ## # p(c|d)
  ## annotate('segment',
  ##          x = 0,
  ##          y = 0,
  ##          xend = 1,
  ##          yend = 1,
  ##          linetype = 'dashed'
  ##          ) +
  ## # p(s|d) v1
  ## annotate('segment',
  ##          x = 0,
  ##          y = 0,
  ##          xend = 1,
  ##          yend = .5,
  ##          linetype = 'dotted'
  ##          ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 0,
           yend = 1,
           size = 3,
           lineend = 'square'
           ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 1,
           yend = 0,
           size = 3,
           lineend = 'square'
           ) +
  scale_color_manual(
    values = c(minority = red,
               white = blue
               )
  ) +
  scale_fill_manual(
    values = c(minority = red %.% '80',
               white = blue %.% '80'
               )
  ) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0, 1),
                  ratio = 1
                  ) +
  xlab("Dangerousness of driving, d'") +
  ylab(NULL) +
  ## ylab('Probability of selection, ' * Pr(c[1] * '|' * d))) +
  ## annotate('text',
  ##          x = .5,
  ##          y = .5,
  ##          label = "'Prob. of crash, ' * Pr(c[1] * '|' * d)",
  ##          parse = TRUE,
  ##          family = 'Raleway',
  ##          angle = 45,
  ##          vjust = -0.1,
  ##          size = 12
  ##          ) +
  ## annotate('text',
  ##          x = .75,
  ##          y = .75 / 2,
  ##          label = "'Prob. of stop, ' * Pr(s[1] * '|' * d)",
  ##          parse = TRUE,
  ##          family = 'Raleway',
  ##          angle = atan(.5) / pi * 180,
  ##          vjust = -0.1,
  ##          size = 12
  ##          ) +
  theme_minimal(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none'
        )
dev.off()

png('proportionality_03.png', 1200, 900)
ggplot(distrs,
       aes(x = d,
           color = race,
           fill = race
           )
       ) +
  geom_line(aes(y = p_d * .05),
            size = 3
            ) +
  ## geom_ribbon(aes(ymin = 0,
  ##                 ymax = d * p_d * .05
  ##                 ),
  ##             size = 3,
  ##             color = NA
  ##             ) +
  # p(c|d)
  annotate('segment',
           x = 0,
           y = 0,
           xend = 1,
           yend = 1,
           linetype = 'dashed'
           ) +
  ## # p(s|d) v1
  ## annotate('segment',
  ##          x = 0,
  ##          y = 0,
  ##          xend = 1,
  ##          yend = .5,
  ##          linetype = 'dotted'
  ##          ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 0,
           yend = 1,
           size = 3,
           lineend = 'square'
           ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 1,
           yend = 0,
           size = 3,
           lineend = 'square'
           ) +
  scale_color_manual(
    values = c(minority = red,
               white = blue
               )
  ) +
  scale_fill_manual(
    values = c(minority = red %.% '80',
               white = blue %.% '80'
               )
  ) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0, 1),
                  ratio = 1
                  ) +
  xlab("Dangerousness of driving, d'") +
  ylab(NULL) +
  ## ylab('Probability of selection, ' * Pr(c[1] * '|' * d))) +
  annotate('text',
           x = .5,
           y = .5,
           label = "'Prob. of crash, ' * Pr(c[1] * '|' * d)",
           parse = TRUE,
           family = 'Raleway',
           angle = 45,
           vjust = -0.1,
           size = 12
           ) +
  ## annotate('text',
  ##          x = .75,
  ##          y = .75 / 2,
  ##          label = "'Prob. of stop, ' * Pr(s[1] * '|' * d)",
  ##          parse = TRUE,
  ##          family = 'Raleway',
  ##          angle = atan(.5) / pi * 180,
  ##          vjust = -0.1,
  ##          size = 12
  ##          ) +
  theme_minimal(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none'
        )
dev.off()

png('proportionality_04.png', 1200, 900)
ggplot(distrs,
       aes(x = d,
           color = race,
           fill = race
           )
       ) +
  geom_line(aes(y = p_d * .05),
            size = 3
            ) +
  geom_ribbon(aes(ymin = 0,
                  ymax = d * p_d * .05
                  ),
              size = 3,
              color = NA
              ) +
  # p(c|d)
  annotate('segment',
           x = 0,
           y = 0,
           xend = 1,
           yend = 1,
           linetype = 'dashed'
           ) +
  ## # p(s|d) v1
  ## annotate('segment',
  ##          x = 0,
  ##          y = 0,
  ##          xend = 1,
  ##          yend = .5,
  ##          linetype = 'dotted'
  ##          ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 0,
           yend = 1,
           size = 3,
           lineend = 'square'
           ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 1,
           yend = 0,
           size = 3,
           lineend = 'square'
           ) +
  scale_color_manual(
    values = c(minority = red,
               white = blue
               )
  ) +
  scale_fill_manual(
    values = c(minority = red %.% '80',
               white = blue %.% '80'
               )
  ) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0, 1),
                  ratio = 1
                  ) +
  xlab("Dangerousness of driving, d'") +
  ylab(NULL) +
  annotate('text',
           x = .5,
           y = .5,
           label = "'Prob. of crash, ' * Pr(c[1] * '|' * d)",
           parse = TRUE,
           family = 'Raleway',
           angle = 45,
           vjust = -0.1,
           size = 12
           ) +
  ## annotate('text',
  ##          x = .75,
  ##          y = .75 / 2,
  ##          label = "'Prob. of stop, ' * Pr(s[1] * '|' * d)",
  ##          parse = TRUE,
  ##          family = 'Raleway',
  ##          angle = atan(.5) / pi * 180,
  ##          vjust = -0.1,
  ##          size = 12
  ##          ) +
  theme_minimal(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none'
        )
dev.off()

png('proportionality_05.png', 1200, 900)
ggplot(distrs,
       aes(x = d,
           color = race,
           fill = race
           )
       ) +
  geom_line(aes(y = p_d * .05),
            size = 3
            ) +
  geom_ribbon(aes(ymin = 0,
                  ymax = d * p_d * .025
                  ),
              size = 3,
              color = NA
              ) +
  ## # p(c|d)
  ## annotate('segment',
  ##          x = 0,
  ##          y = 0,
  ##          xend = 1,
  ##          yend = 1,
  ##          linetype = 'dashed'
  ##          ) +
  # p(s|d) v1
  annotate('segment',
           x = 0,
           y = 0,
           xend = 1,
           yend = .5,
           linetype = 'dotted'
           ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 0,
           yend = 1,
           size = 3,
           lineend = 'square'
           ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 1,
           yend = 0,
           size = 3,
           lineend = 'square'
           ) +
  scale_color_manual(
    values = c(minority = red,
               white = blue
               )
  ) +
  scale_fill_manual(
    values = c(minority = red %.% '80',
               white = blue %.% '80'
               )
  ) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0, 1),
                  ratio = 1
                  ) +
  xlab("Dangerousness of driving, d'") +
  ylab(NULL) +
  ## ylab('Probability of selection, ' * Pr(c[1] * '|' * d))) +
  ## annotate('text',
  ##          x = .5,
  ##          y = .5,
  ##          label = "'Prob. of crash, ' * Pr(c[1] * '|' * d)",
  ##          parse = TRUE,
  ##          family = 'Raleway',
  ##          angle = 45,
  ##          vjust = -0.1,
  ##          size = 12
  ##          ) +
  annotate('text',
           x = .75,
           y = .75 / 2,
           label = "'Prob. of stop, ' * Pr(s[1] * '|' * d)",
           parse = TRUE,
           family = 'Raleway',
           angle = atan(.5) / pi * 180,
           vjust = -0.1,
           size = 12
           ) +
  theme_minimal(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none'
        )
dev.off()

png('proportionality_06.png', 1200, 900)
ggplot(distrs,
       aes(x = d,
           color = race,
           fill = race
           )
       ) +
  geom_line(aes(y = p_d * .05),
            size = 3
            ) +
  geom_ribbon(aes(ymin = 0,
                  ymax = d * p_d * .05
                  ),
              data = distrs[race == 'minority'],
              size = 3,
              color = NA
              ) +
  geom_ribbon(aes(ymin = 0,
                  ymax = d * p_d * .025
                  ),
              data = distrs[race == 'white'],
              size = 3,
              color = NA
              ) +
  ## # p(c|d)
  ## annotate('segment',
  ##          x = 0,
  ##          y = 0,
  ##          xend = 1,
  ##          yend = 1,
  ##          linetype = 'dashed'
  ##          ) +
  # p(s|d) v1
  annotate('segment',
           x = 0,
           y = 0,
           xend = 1,
           yend = .375,
           linetype = 'dotted',
           color = blue
           ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 1,
           yend = .75,
           linetype = 'dotted',
           color = red
           ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 0,
           yend = 1,
           size = 3,
           lineend = 'square'
           ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 1,
           yend = 0,
           size = 3,
           lineend = 'square'
           ) +
  scale_color_manual(
    values = c(minority = red,
               white = blue
               )
  ) +
  scale_fill_manual(
    values = c(minority = red %.% '80',
               white = blue %.% '80'
               )
  ) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0, 1),
                  ratio = 1
                  ) +
  xlab("Dangerousness of driving, d'") +
  ylab(NULL) +
  ## ylab('Probability of selection, ' * Pr(c[1] * '|' * d))) +
  ## annotate('text',
  ##          x = .5,
  ##          y = .5,
  ##          label = "'Prob. of crash, ' * Pr(c[1] * '|' * d)",
  ##          parse = TRUE,
  ##          family = 'Raleway',
  ##          angle = 45,
  ##          vjust = -0.1,
  ##          size = 12
  ##          ) +
  annotate('text',
           x = .75,
           y = .75 * .75,
           label = "'Prob. of stop, ' * Pr(s[1] * '|' * d * ',' * r[0])",
           parse = TRUE,
           family = 'Raleway',
           angle = atan(.75) / pi * 180,
           vjust = -0.1,
           size = 12,
           color = red
           ) +
  annotate('text',
           x = .75,
           y = .375 * .75,
           label = "'Prob. of stop, ' * Pr(s[1] * '|' * d * ',' * r[0])",
           parse = TRUE,
           family = 'Raleway',
           angle = atan(.375) / pi * 180,
           vjust = -0.1,
           size = 12,
           color = blue
           ) +
  theme_minimal(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none'
        )
dev.off()

png('proportionality_07.png', 1200, 900)
ggplot(distrs,
       aes(x = d,
           color = race,
           fill = race
           )
       ) +
  geom_line(aes(y = p_d * .05),
            size = 3
            ) +
  geom_ribbon(aes(ymin = 0,
                  ymax = p_d * .05 * .5,
                  ),
              data = distrs[d >= .1,],
              size = 3,
              color = NA
              ) +
  ## # p(c|d)
  ## annotate('segment',
  ##          x = 0,
  ##          y = 0,
  ##          xend = 1,
  ##          yend = 1,
  ##          linetype = 'dashed'
  ##          ) +
  # p(s|d) v1
  annotate('line',
           x = c(0, .1, .1, 1),
           y = c(.005, .005, .5, .5),
           linetype = 'dotted'
           ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 0,
           yend = 1,
           size = 3,
           lineend = 'square'
           ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 1,
           yend = 0,
           size = 3,
           lineend = 'square'
           ) +
  scale_color_manual(
    values = c(minority = red,
               white = blue
               )
  ) +
  scale_fill_manual(
    values = c(minority = red %.% '80',
               white = blue %.% '80'
               )
  ) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0, 1),
                  ratio = 1
                  ) +
  xlab("Dangerousness of driving, d'") +
  ylab(NULL) +
  ## ylab('Probability of selection, ' * Pr(c[1] * '|' * d))) +
  ## annotate('text',
  ##          x = .5,
  ##          y = .5,
  ##          label = "'Prob. of crash, ' * Pr(c[1] * '|' * d)",
  ##          parse = TRUE,
  ##          family = 'Raleway',
  ##          angle = 45,
  ##          vjust = -0.1,
  ##          size = 12
  ##          ) +
  annotate('text',
           x = .75,
           y = .5,
           label = "'Prob. of stop, ' * Pr(s[1] * '|' * d)",
           parse = TRUE,
           family = 'Raleway',
           vjust = -0.1,
           size = 12
           ) +
  theme_minimal(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none'
        )
dev.off()

png('proportionality_08.png', 1200, 900)
ggplot(distrs,
       aes(x = d,
           color = race,
           fill = race
           )
       ) +
  geom_line(aes(y = p_d * .05),
            size = 3
            ) +
  geom_ribbon(aes(ymin = 0,
                  ymax = p_d * .05 * .5,
                  ),
              data = distrs[d >= d_mean_minority,],
              size = 3,
              color = NA
              ) +
  ## # p(c|d)
  ## annotate('segment',
  ##          x = 0,
  ##          y = 0,
  ##          xend = 1,
  ##          yend = 1,
  ##          linetype = 'dashed'
  ##          ) +
  # p(s|d) v1
  annotate('line',
           x = c(0, d_mean_minority, d_mean_minority, 1),
           y = c(.005, .005, .5, .5),
           linetype = 'dotted'
           ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 0,
           yend = 1,
           size = 3,
           lineend = 'square'
           ) +
  annotate('segment',
           x = 0,
           y = 0,
           xend = 1,
           yend = 0,
           size = 3,
           lineend = 'square'
           ) +
  scale_color_manual(
    values = c(minority = red,
               white = blue
               )
  ) +
  scale_fill_manual(
    values = c(minority = red %.% '80',
               white = blue %.% '80'
               )
  ) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0, 1),
                  ratio = 1
                  ) +
  xlab("Dangerousness of driving, d'") +
  ylab(NULL) +
  ## ylab('Probability of selection, ' * Pr(c[1] * '|' * d))) +
  ## annotate('text',
  ##          x = .5,
  ##          y = .5,
  ##          label = "'Prob. of crash, ' * Pr(c[1] * '|' * d)",
  ##          parse = TRUE,
  ##          family = 'Raleway',
  ##          angle = 45,
  ##          vjust = -0.1,
  ##          size = 12
  ##          ) +
  annotate('text',
           x = .75,
           y = .5,
           label = "'Prob. of stop, ' * Pr(s[1] * '|' * d)",
           parse = TRUE,
           family = 'Raleway',
           vjust = -0.1,
           size = 12
           ) +
  theme_minimal(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none'
        )
dev.off()
