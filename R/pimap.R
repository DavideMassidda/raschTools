pimap <- function(data.pr, data.it, logit.pr, logit.it, item, thr, rel_widths=c(1,2),
                  logit.range=NULL, binwidth=0.25, xlab.pr="", xlab.it="Item", ylab.pr="Count",
                  ylab.it="logit", title.pr="Person\'s abilities", title.it="Item thresholds")
{
    # Checks on data
    if(!is.factor(data.it[,item]))
        data.it[,item] <- as.factor(data.it[,item])
    if(!is.factor(data.it[,thr]))
        data.it[,thr] <- as.factor(data.it[,thr])
    # Range of logits (limits of axes)
    if(is.null(logit.range)) {
        logit.range <- range(c(data.pr[,logit.pr], data.it[,logit.it]))
        logit.range[1] <- testing::decimal.floor(logit.range[1])
        logit.range[2] <- testing::decimal.ceiling(logit.range[2])
    }
    # Person's abilities panel
    pr <- ggplot(data.pr, aes_string(x=logit.pr)) +
        geom_histogram(binwidth=binwidth, colour="black", fill="gray80") +
        scale_y_reverse() +
        coord_flip() +
        scale_x_continuous(limits=logit.range) +
        xlab(xlab.pr) + ylab(ylab.pr) +
        ggtitle(title.pr) +
        theme_bw(base_size=18)
    # Item thresholds panel
    it <- ggplot(data.it, aes_string(x=item, y=logit.it)) +
        geom_line() +
        geom_point(shape=21, bg="gray80", size=5, show.legend=FALSE) +
        geom_text(aes_string(label=thr),hjust=-1.5, vjust=0) +
        scale_y_continuous(limits=logit.range) +
        xlab(xlab.it) + ylab(ylab.it) +
        ggtitle(title.it) +
        theme_bw(base_size=18) +
        theme(legend.direction="horizontal", legend.position="bottom")
    # Merging plots
    it <- ggplot_gtable(ggplot_build(it))
    pr <- ggplot_gtable(ggplot_build(pr))
    # View plots
    plot_grid(pr, it, rel_widths=rel_widths)
}
# Esempio d'uso
#set.seed(22)
#persons <- data.frame(id=factor(1:500),logit=rnorm(500))
#items <- data.frame(item=factor(rep(1:20,each=3)),thr=factor(rep(1:3,20)),logit=rnorm(60))
#pi_map(persons, items, logit.pr="logit", logit.it="logit", item="item", thr="thr")

