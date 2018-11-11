
## ordering
# reorder an axis in ggplot
p <- ggplot(data = totals, aes(x = reorder(subject, score), y = score)) + geom_point(aes(colour = score))

# or reorder manually as factors
scores$mode <- factor(scores$mode, levels = c("as-is", "less-disfluency", "less-form-error", "less-all-error"))

# reorder by count
multi.addr$group <- factor(multi.addr$group, levels=names(sort(table(multi.addr$group), decreasing=T)))


## geom_point
# point size
geom_point(size = 4)

# point shape
geom_point(shape = 15)  # solid square

# 'outline' points
+ geom_point(fill = "white", size = 3, stroke = 2, shape = 21, alpha = 2/3)

# custom colours for geom_point
+ scale_colour_manual(values = c("#bababa", "#2c7bb6", "#d7191c"), labels = c("England", "Scotland", "Wales"))


## scales
# axis titles
scale_y_continuous("parse")

# limits
scale_y_continuous(limits = c(0, 5))

# fill greyscale
scale_fill_grey(start = 0.4, end = 0.8)

# custom colours
scale_fill_manual(values = brewpal)

# number formatting
library(scales)
scale_x_continuous(labels = comma)
+ scale_y_continuous("count", breaks = pretty_breaks())  # integers

# reverse scale
scale_x_reverse()


# alpha
geom_point(data, aes(), alpha = 0.25)

# nb, to override the alpha for the legends, use 'guides':
p + guides(colour = guide_legend(override.aes = list(alpha = 1)))

# to omit legend title for all used scales: http://ggplot2.tidyverse.org/reference/guides.html
+ guides(colour=guide_legend(""), shape=guide_legend(""), size=guide_legend(""))


## multiple plots
# facets
+ facet_wrap(~ var)

## functions to reorder scales within facets
# https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}
scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}


# new in ggplot
# https://github.com/hadley/ggplot2/wiki/Mixing-ggplot2-graphs-with-other-graphical-output
# https://github.com/baptiste/gridextra/wiki/arranging-ggplot
library(gridExtra)
p1 = qplot(1:10,rnorm(10))
p2 = qplot(1:10,rnorm(10))
grid.arrange(p1, p2, ncol=2, top = "Main title")

# viewport (custom tiling of plots), with title row
library(grid)
vplayout <- function(x,y) {
    viewport(layout.pos.row=x, layout.pos.col=y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 3, heights=unit(c(0.5, 5), 'null'))))  # 2 rows, 3 columns
grid.text("Q1: Is the segment ok?", vp=viewport(layout.pos.row=1, layout.pos.col=1:3))
print(plotB1, vp=vplayout(2,1))  # this layout goes across 2nd row of grid
print(plotB2, vp=vplayout(2,2))
print(plotC1, vp=vplayout(2,3))

# or using multiplots function: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
 if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(p1, p2, p3, p4, cols = 2)



## theme stuff
# black and white
theme_bw()

# legend title: add 'name = ""' to the appropriate 'scale', e.g. --
# color brewer library
library(RColorBrewer)
+ scale_fill_brewer(palette = "Greys", name = "CEFR")
+ scale_fill_brewer(palette = "Reds", direction = -1)  # reverse direction

# fill specific colour: in the geom_x()
geom_bar(fill = "#74c476")

# legend labels: list existing ones as 'breaks', new ones as 'labels'
bp + scale_fill_discrete(name="Experimental\nCondition",
                         breaks=c("ctrl", "trt1", "trt2"),
                         labels=c("Control", "Treatment 1", "Treatment 2"))

# omit legend
theme(legend.position = 'none')
# n.b. to avoid extra legends popping up (size, alpha, etc), place them _outside_ aes(), i.e. --
ggplot(clc, aes(x = wordcount, group = cefr, fill = cefr)) + geom_density(alpha = 1/2, size = 2)


# text size
theme(text = element_text(size = 8))

# font family
theme(text=element_text(family='Times'))
theme(text=element_text(family='Georgia'))

# axis label position (with 0.5 being centre / default, 0 to left horizontally or edge of plot vertically, 1 to right horizontally or edge of labels vertically)
theme(text=element_text(hjust=0.5, vjust=0.1))
theme(text=element_text(angle=90, hjust=1, vjust=0.5))  # turn text 90º

# axis labels x/y specifically
theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.1))

# plot title
+ labs(title = "title")
+ theme(plot.title = element_text(size = 12, vjust = 1))


## factors
# factor levels
levels(df$foo)

# exclude
df$foo <- factor(counshp2$group, exclude = excllevs)


# exclude outliers in plotting
ggplot(subset(annotCounts[annotCounts$rate < 50, ], tagclass == "formal"), aes(x = wordCount, y = rate)) + geom_point(aes(size = 3), colour = "#D94423")


# percent values in histogram (via geom_bar) http://stackoverflow.com/questions/3695497/ggplot-showing-instead-of-counts-in-charts-of-categorical-variables
library(scales)
p <- ggplot(mtcars, aes(x = hp)) +  
        geom_bar(aes(y = (..count..)/sum(..count..)), binwidth = 25) + 
        scale_y_continuous(labels = percent) #version 3.1.0

# proportions in bar chart
library(ggplot2)
library(scales)
ggplot(mtcars, aes(am)) + geom_bar(aes(y=..count../sum(..count..))) + scale_y_continuous(labels=percent_format())


## do not drop unused levels in geom_bar factor plot
+ scale_x_discrete(drop = FALSE)


# geom_smooth without error bars
+ geom_smooth(se = F)


## annotate plot with correlations
library(plyr)
corrs <- ddply(moltenDiv, c("variable"), summarise, cor = round(cor(value, tokens, use = "complete.obs"), 3))
ggplot(moltenDiv, aes(x = tokens, y = value)) + geom_point(aes(colour = topic), alpha = 4/5, size = 3) + geom_text(data = corrs, aes(label = paste0("r=", cor), x = 200, y = 0.4, family = "Georgia")) + facet_wrap(~ variable)

## annotate plot with selected labels
library(ggrepel)
textdf <- merged[grepl("Bradford|Cambridge|Cardiff|Edinburgh|Liverpool|Manchester|Telford|Tower|Westminster", merged$Area.Name, perl = T), ]
+ geom_text_repel(data = textdf, aes(x = Perc.NonUK, y = Perc.Leave, label = Area.Name), colour = "black", nudge_x = 0.01, nudge_y = 0.02)


## mapping
library(ggmap)
cambbox <- c(left = -0.4, bottom = 51.8, right = 0.8, top = 52.5)
cambridge <- get_stamenmap(cambbox, zoom = 10, maptype = 'watercolor')
camblabs <- get_stamenmap(cambbox, zoom = 10, maptype = 'toner-labels')
ggmap(cambridge) + inset_ggmap(camblabs)
