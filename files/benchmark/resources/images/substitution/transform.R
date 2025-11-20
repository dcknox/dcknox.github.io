library(ggplot2)
library(data.table)
library(assertthat)

# style ========================================================================

red <- '#A51C30'
lightred <- '#E16273'
blue <- '#4E84C4'
darkblue <- '#0C2C5C'
lightblue <- '#4C6C9C'



# test =========================================================================

n_speed_minority <- 500
n_speed_white <- 500

rr <- 3
p_stop_white <- .2
p_stop_minority <- p_stop_white * rr

n_stop_minority <- p_stop_minority * n_speed_minority
n_stop_white <- p_stop_white * n_speed_white

n_camera_minority <- n_speed_minority - n_stop_minority
n_camera_white <- n_speed_white - n_stop_white

or <-
  (p_stop_white * rr / (1 - p_stop_white * rr)) /
  (p_stop_white / (1 - p_stop_white))

assert_that(
  or ==
    (n_stop_minority / n_camera_minority) /
    (n_stop_white / n_camera_white)
)



# plot =========================================================================

results <- data.table(expand.grid(
  p_stop_white = c(1/2, 1/3, 1/5),
  rr = 2^seq(-2, 2, .01)
))
results[,
        or := (
          (p_stop_white * rr / (1 - p_stop_white * rr)) /
            (p_stop_white / (1 - p_stop_white))
        )]
results <- results[or > 0,]

png('substitution_transform_01.png', 1200, 900)
ggplot(results,
       aes(x = rr,
           y = or,
           group = p_stop_white
           )
       ) +
  geom_line(color = 'white') +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  ## geom_point(x = 2, y = 4, color = lightred, size = 8) +
  coord_fixed(ratio = .375,
              xlim = c(.5, 2),
              ylim = c(.25, 4)
              ) +
  theme_light(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank()
        ) +
  scale_y_continuous(breaks = seq(.5, 4, .5)) +
  xlab('\nActual risk ratio among dangerous drivers') +
  ylab('\nEstimated risk ratio, ignoring substitution\n')
dev.off()

png('substitution_transform_02.png', 1200, 900)
ggplot(results,
       aes(x = rr,
           y = or,
           group = p_stop_white
           )
       ) +
  geom_line(color = 'white') +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  geom_point(x = 1, y = 1, color = 'gray', size = 8) +
  coord_fixed(ratio = .375,
              xlim = c(.5, 2),
              ylim = c(.25, 4)
              ) +
  theme_light(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank()
        ) +
  scale_y_continuous(breaks = seq(.5, 4, .5)) +
  xlab('\nActual risk ratio among dangerous drivers') +
  ylab('\nEstimated risk ratio, ignoring substitution\n')
dev.off()

png('substitution_transform_03.png', 1200, 900)
ggplot(results,
       aes(x = rr,
           y = or,
           group = p_stop_white
           )
       ) +
  geom_line(color = 'white') +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  geom_point(x = 1, y = 1, color = 'gray', size = 8) +
  geom_point(x = 2, y = 4, color = lightred, size = 8) +
  coord_fixed(ratio = .375,
              xlim = c(.5, 2),
              ylim = c(.25, 4)
              ) +
  theme_light(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank()
        ) +
  scale_y_continuous(breaks = seq(.5, 4, .5)) +
  xlab('\nActual risk ratio among dangerous drivers') +
  ylab('\nEstimated risk ratio, ignoring substitution\n')
dev.off()

png('substitution_transform_04.png', 1200, 900)
ggplot(results,
       aes(x = rr,
           y = or,
           group = p_stop_white
           )
       ) +
  geom_line(color = 'white') +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed',) +
  geom_line(color = lightred,
            size = 2,
            data = results[p_stop_white == 1/3 & or > rr]
            ) +
  geom_point(x = 1, y = 1, color = 'darkgray', size = 2) +
  coord_fixed(ratio = .375,
              xlim = c(.5, 2),
              ylim = c(.25, 4)
              ) +
  theme_light(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank()
        ) +
  scale_y_continuous(breaks = seq(.5, 4, .5)) +
  xlab('\nActual risk ratio among dangerous drivers') +
  ylab('\nEstimated risk ratio, ignoring substitution\n')
dev.off()

png('substitution_transform_05.png', 1200, 900)
ggplot(results,
       aes(x = rr,
           y = or,
           group = p_stop_white
           )
       ) +
  geom_line(color = 'white') +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed',) +
  geom_line(color = lightred,
            size = 2,
            data = results[p_stop_white == 1/3 & or > rr]
            ) +
  geom_line(color = lightblue,
            size = 2,
            data = results[p_stop_white == 1/3 & or < rr]
            ) +
  geom_point(x = 1, y = 1, color = 'darkgray', size = 2) +
  coord_fixed(ratio = .375,
              xlim = c(.5, 2),
              ylim = c(.25, 4)
              ) +
  theme_light(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank()
        ) +
  scale_y_continuous(breaks = seq(.5, 4, .5)) +
  xlab('\nActual risk ratio among dangerous drivers') +
  ylab('\nEstimated risk ratio, ignoring substitution\n')
dev.off()

png('substitution_transform_06.png', 1200, 900)
ggplot(results,
       aes(x = rr,
           y = or,
           group = p_stop_white
           )
       ) +
  geom_line(color = 'white') +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed',) +
  geom_line(color = lightred,
            size = 2,
            data = results[p_stop_white == 1/3 & or > rr]
            ) +
  geom_line(color = lightblue,
            size = 2,
            data = results[p_stop_white == 1/3 & or < rr]
            ) +
  geom_line(color = red,
            size = 2,
            data = results[p_stop_white == 1/2 & or > rr]
            ) +
  geom_line(color = darkblue,
            size = 2,
            data = results[p_stop_white == 1/2 & or < rr]
            ) +
  geom_point(x = 1, y = 1, color = 'darkgray', size = 2) +
  coord_fixed(ratio = .375,
              xlim = c(.5, 2),
              ylim = c(.25, 4)
              ) +
  theme_light(base_size = 36) +
  theme(text = element_text(family = 'Raleway'),
        panel.grid = element_blank()
        ) +
  scale_y_continuous(breaks = seq(.5, 4, .5)) +
  xlab('\nActual risk ratio among dangerous drivers') +
  ylab('\nEstimated risk ratio, ignoring substitution\n')
dev.off()
