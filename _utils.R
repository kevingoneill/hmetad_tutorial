#PALETTE_STIM <- paletteer_d("fishualize::Phractocephalus_hemioliopterus")[c(1, 3)]
#PALETTE_MH <- paletteer_d('MetBrewer::Homer1')[c(1,4,8)]
#PALETTE_CORRECT <- paletteer_d('MetBrewer::Klimt')[c(2,4)]

PALETTE_MH <- paletteer_d('tvthemes::AirNomads')[c(1,4,5)]
PALETTE_STIM <- paletteer_d('tvthemes::WaterTribe')[c(2,5)]
PALETTE_CORRECT <- paletteer_d('tvthemes::FireNation')[c(2,1)]

THEME_SDT <- function(xlab='Evidence',
                      limits=c(-2.5, 2.5), breaks=seq(-4, 4, by=2),
                      base_size=18,
                      expand=expansion(mult=c(.1, .1)),
                      ink='black', paper='white') {
  list(
    scale_x_continuous(xlab, limits=limits, breaks=breaks, expand=expand),
    scale_color_manual('Stimulus', values=PALETTE_STIM),
    scale_fill_manual('Stimulus', values=PALETTE_STIM),
    scale_y_continuous(expand=c(0, 0)),
    theme_classic(base_size=base_size, ink=ink, paper=paper),
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y=element_blank())
  )
}

theme_metad <- function(base_size=18, ...) {
  list(theme_classic(base_size, ...))
}


type1_plot <- function(evidence, c=TRUE, alpha=.85, ...) {
  p <- ggplot(evidence, aes(xdist=dist_normal(mu1, sd))) +
    stat_slab(aes(fill=stimulus), color=NA, alpha=alpha, scale=.8, show.legend=FALSE) +
    THEME_SDT(xlab='Type I Evidence', expand=expansion(), ...)

  if (c) {
    p <- p + geom_vline(aes(xintercept=c))
  }
  
  p
}

type2_plot <- function(evidence, response='both',
                       c=TRUE, c2=TRUE, labels=FALSE,
                       limit=2.5, label_y=0.85, expansion=0.01, alpha=.85, ...) {
  p <- ggplot(evidence)
  if (c) {
    p <- p + geom_vline(xintercept=first(evidence$c))
  }
  
  if (response == 'zero' || response==0 || response=='0') {
    p <- p +
      stat_slab(aes(xdist=dist_truncated(dist_normal(mu2, sd), upper=c), fill=stimulus),
                color=NA, alpha=alpha, scale=.8, show.legend=FALSE) +
      THEME_SDT(xlab=paste0('Type II Evidence\n("', response, '" Response)'),
                limits=c(-limit, first(evidence$c)),
                expand=expansion(add=c(0, expansion)), ...)
    
    if (c2) {
      p <- p + geom_vline(xintercept=first(evidence$c2_0), linetype='dashed')
    }
    if (labels) {
      p <- p +
        geom_text(aes(x=x, y=y, label=label),
                  data=tibble(x=(first(evidence$c) + first(evidence$c2_0))/2,
                              y=label_y, label="C = low"), fontface='bold') +
        geom_text(aes(x=x, y=y, label=label),
                  data=tibble(x=(first(evidence$c2_0) - limit)/2,
                              y=label_y, label="C = high"), fontface='bold')
    }
  } else if (response == 'one' || response==1 || response=='1') {
    p <- p +
      stat_slab(aes(xdist=dist_truncated(dist_normal(mu2, sd), lower=c), fill=stimulus),
                color=NA, alpha=alpha, scale=.8, show.legend=FALSE) +
      THEME_SDT(xlab=paste0('Type II Evidence\n("', response, '" Response)'),
                limits=c(first(evidence$c), limit),
                expand=expansion(add=c(expansion, 0)), ...)
    if (c2) {
      p <- p + geom_vline(xintercept=first(evidence$c2_1), linetype='dashed')
    }
    if (labels) {
      p <- p +
        geom_text(aes(x=x, y=y, label=label),
                  data=tibble(x=(first(evidence$c) + first(evidence$c2_1))/2,
                              y=label_y, label="C = low"), fontface='bold') +
        geom_text(aes(x=x, y=y, label=label),
                  data=tibble(x=(first(evidence$c2_1) + limit)/2,
                              y=label_y, label="C = high"), fontface='bold')
    }
  } else {
    p <- p +
      stat_slab(aes(xdist=dist_normal(mu2, sd), fill=stimulus),
                color=NA, alpha=alpha, scale=.8, show.legend=FALSE) +
      THEME_SDT(xlab='Type II Evidence', expand=expansion(),
                limits=c(-limit, limit), ...)

    if (c2) {
      p <- p +
        geom_vline(xintercept=first(evidence$c2_0), linetype='dashed') +
        geom_vline(xintercept=first(evidence$c2_1), linetype='dashed')
    }

    if (labels) {
      p <- p +
        geom_text(aes(x=x, y=y, label=label),
                  data=tibble(x=c(
                  (first(evidence$c) + first(evidence$c2_0))/2,
                  (first(evidence$c) + first(evidence$c2_1))/2
                  ),
                  y=label_y, label="C = low"), fontface='bold') +
        geom_text(aes(x=x, y=y, label=label),
                  data=tibble(x=c(
                  (first(evidence$c2_0) - limit)/2,
                  (first(evidence$c2_1) + limit)/2
                  ),
                  y=label_y, label="C = high"), fontface='bold')
    }
  }

  p
}


quarto_format <- function() {
  fromJSON(Sys.getenv("QUARTO_EXECUTE_INFO"))$format$identifier$`target-format`
}
