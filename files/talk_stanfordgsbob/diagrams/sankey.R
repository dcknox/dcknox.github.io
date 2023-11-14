library(Cairo)
library(data.table)
library(ggplot2)
library(ggalluvial)

`%.%` <- paste0

# graphics setup ===============================================================

minority_color <- '#4E84C4'
white_color <- '#A51C30'

CairoFonts(regular = 'Raleway:Regular',
           bold = 'Raleway:Bold',
           italic = 'Raleway:Italic'
           )



# loop over unknown quantities, make plot ======================================

p_shot <- .25
space <- .05

logistic_interpolation <- function(start, end, steps, speed){
  mixture <- 1 / (1 + exp(-1/speed * seq(-5, 5, length.out = steps + 1)))
  mixture <- mixture - min(mixture)
  mixture <- mixture / max(mixture)
  return(mixture * end + (1 - mixture) * start)
}



## 80-20 to 5-95 minority-white ================================================


interpolation_50_80 <- logistic_interpolation(.5, .8, steps = 100, speed = 1.33)

for (i in 1:101){

  p_minority <- interpolation_50_80[i]
  p_white <- 1 - p_minority
  p_minority_cond_shot <- .2
  p_white_cond_shot <- .8
  p_minority_cond_not_shot <-
    (p_minority - p_minority_cond_shot * p_shot) / (1 - p_shot)
  p_white_cond_not_shot <-
    (p_white - p_white_cond_shot * p_shot) / (1 - p_shot)

  d <- data.table(
    Race = c('Minority',
             'Minority',
             'Race_space1',
             'Race_space1',
             'White',
             'White'
             ),
    RaceAction = c('Minority (shot)',
                   'Minority (not shot)',
                   'RaceAction_space1',
                   'RaceAction_space2',
                   'White (shot)',
                   'White (not shot)'
                   ),
    hide = c('no',
             'no',
             'yes',
             'yes',
             'no',
             'no'
             ),
    p = c(p_minority_cond_shot * p_shot * (1 - space),
          p_minority_cond_not_shot * (1 - p_shot) * (1 - space),
          space / 2,
          space / 2,
          p_white_cond_shot * p_shot * (1 - space),
          p_white_cond_not_shot * (1 - p_shot) * (1 - space)
          )
  )
  d[,
    Race := factor(Race,
                   levels = c('Minority',
                              'Race_space1',
                              'White'
                              )
                   )
    ]
  d[,
    RaceAction := factor(RaceAction,
                         levels = c('Minority (not shot)',
                                    'RaceAction_space1',
                                    'Minority (shot)',
                                    'White (shot)',
                                    'RaceAction_space2',
                                    'White (not shot)'
                                    )
                         )
    ]
  d[, p := round(p, 6)]

  p <- ggplot(d,
              aes(y = p, axis1 = Race, axis2 = RaceAction)
              ) +
    geom_alluvium(aes(fill = RaceAction
                      ),
                  width = 1/2
                  ) +
    geom_stratum(aes(color = hide),
                 width = 1/2,
                 size = 2,
                 fill = NA,
                 ) +
    scale_fill_manual(values = c(
      'Minority (shot)' = minority_color,
      'Minority (not shot)' = minority_color %.% '60',
      'RaceAction_space1' = '#FFFFFF00',
      'RaceAction_space2' = '#FFFFFF00',
      'White (shot)' = white_color,
      'White (not shot)' = white_color %.% '60'
    )) +
    scale_color_manual(values = c(
      no = 'black',
      yes = '#FFFFFF00'
    )) +
    geom_text(stat = "stratum",
              aes(label = after_stat(stratum),
                  color = hide
                  ),
              size = 16
              ) +
    theme_void() +
    theme(legend.position = 'none')

  CairoPNG(sprintf('sankey/minority_50_to_80_%03.0f.png', i),
           width = 1600,
           height = 2400
           )
  print(p)
  dev.off()

}
## ffmpeg -y -framerate 40 -pattern_type glob -i 'minority_50_to_80_*.png' -c:v libx264 -pix_fmt yuv420p minority_50_to_80.mp4



## 50-50 to 80-20 minority-white ===============================================

interpolation_80_05 <- logistic_interpolation(.8, .05, steps = 100, speed = 1.33)

for (i in 1:101){

  p_minority <- interpolation_80_05[i]
  p_white <- 1 - p_minority
  p_minority_cond_shot <- .2
  p_white_cond_shot <- .8
  p_minority_cond_not_shot <-
    (p_minority - p_minority_cond_shot * p_shot) / (1 - p_shot)
  p_white_cond_not_shot <-
    (p_white - p_white_cond_shot * p_shot) / (1 - p_shot)

  d <- data.table(
    Race = c('Minority',
             'Minority',
             'Race_space1',
             'Race_space1',
             'White',
             'White'
             ),
    RaceAction = c('Minority (shot)',
                   'Minority (not shot)',
                   'RaceAction_space1',
                   'RaceAction_space2',
                   'White (shot)',
                   'White (not shot)'
                   ),
    hide = c('no',
             'no',
             'yes',
             'yes',
             'no',
             'no'
             ),
    p = c(p_minority_cond_shot * p_shot * (1 - space),
          p_minority_cond_not_shot * (1 - p_shot) * (1 - space),
          space / 2,
          space / 2,
          p_white_cond_shot * p_shot * (1 - space),
          p_white_cond_not_shot * (1 - p_shot) * (1 - space)
          )
  )
  d[,
    Race := factor(Race,
                   levels = c('Minority',
                              'Race_space1',
                              'White'
                              )
                   )
    ]
  d[,
    RaceAction := factor(RaceAction,
                         levels = c('Minority (not shot)',
                                    'RaceAction_space1',
                                    'Minority (shot)',
                                    'White (shot)',
                                    'RaceAction_space2',
                                    'White (not shot)'
                                    )
                         )
    ]
  d[, p := round(p, 6)]

  p <- ggplot(d,
              aes(y = p, axis1 = Race, axis2 = RaceAction)
              ) +
    geom_alluvium(aes(fill = RaceAction
                      ),
                  width = 1/2
                  ) +
    geom_stratum(aes(color = hide),
                 width = 1/2,
                 size = 2,
                 fill = NA,
                 ) +
    scale_fill_manual(values = c(
      'Minority (shot)' = minority_color,
      'Minority (not shot)' = minority_color %.% '60',
      'RaceAction_space1' = '#FFFFFF00',
      'RaceAction_space2' = '#FFFFFF00',
      'White (shot)' = white_color,
      'White (not shot)' = white_color %.% '60'
    )) +
    scale_color_manual(values = c(
      no = 'black',
      yes = '#FFFFFF00'
    )) +
    geom_text(stat = "stratum",
              aes(label = after_stat(stratum),
                  color = hide
                  ),
              size = 16
              ) +
    theme_void() +
    theme(legend.position = 'none')

  CairoPNG(sprintf('sankey/minority_80_to_05_%03.0f.png', i),
           width = 1600,
           height = 2400
           )
  print(p)
  dev.off()

}
## ffmpeg -y -framerate 40 -pattern_type glob -i 'minority_80_to_05_*.png' -c:v libx264 -pix_fmt yuv420p minority_80_to_05.mp4
