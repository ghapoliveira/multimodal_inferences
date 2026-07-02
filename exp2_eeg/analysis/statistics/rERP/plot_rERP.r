# This was very slightly adapted to our data: 30-electrode cap, standard_30_clean.loc, three time
# windows, and 95% CIs (1.96 × SE).
#
# See requirements_R.txt
#
# Credits go to:
#
##
# Christoph Aurnhammer (github.com/caurnhammer/psyp23rerps)
# EEG plotting options for (lme)rERPs
#
# Several functions for topography plotting were
# copied and / or adapted from craddm
# https://craddm.github.io/eegUtils/
##
#
# Differences from the original:
# - generate_topo() reads standard_30_clean.loc (our 30-ch cap), not biosemi70elecs.loc
# - plot_grandavg_ci() CI ribbons use 1.96 × SE (95% CI), not raw SE
# - plot_grandavg_ci() shades all time windows via a loop, not two hardcoded rects
# - plot_full_elec() laid out for our 30-channel head montage, not the original 26/70


library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)

# compute standard error
se <- function(
    x,
    na_rm = FALSE
) {
  if (na_rm == TRUE) {
    sd(x, na.rm = TRUE) / sqrt(length(x[!is.na(x)]))
  } else {
    sd(x) / sqrt(length(x))
  }
}

# Return only the legend of an ggplot object
get_legend <- function(
    a_gplot
) {
  tmp <- ggplot_gtable(ggplot_build(a_gplot +
                                      theme(legend.box = "vertical",
                                            legend.spacing.y = unit(0.005, "inch"))))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# For a single Electrode, plot per-grouping mean.
plot_grandavg_ci <- function(
    df,
    ttl,
    yunit = paste0("Amplitude (", "\u03BC", "Volt\u29"),
    ylims = NULL,
    modus = "Condition",
    tws = list(c(300, 500), c(600, 1000)),
    ci = TRUE,
    leg_labs,
    leg_vals
) {
  ###### DATA PROC
  if (modus %in% c("Quantile", "Condition")) {
    colnames(df)[c(4, 5)] <- c("V", "V_CI")
  } else if (modus == "ConditionQuantile") {
    colnames(df)[c(5, 6)] <- c("V", "V_CI")
  } else if (modus == "Coefficient") {
    colnames(df)[c(3, 4)] <- c("V", "V_CI")
  } else if (modus == "t-value") {
    colnames(df)[c(3, 4, 5)] <- c("V", "V_CI", "V_sig")
    sig_dt <- df[, c("Spec", "Timestamp", "V_sig")]
    sig_dt$posit <- rep(seq(ylims[1], ylims[1] - 2,
                            length = length(unique(sig_dt$Spec))),
                        length = nrow(sig_dt))
    sig_dt$sig <- factor(sig_dt$V_sig, levels = c("0", "1"),
                         labels = c("insign", "sign"))
  }
  ##### PLOTTING
  # Initial plot call
  if (modus %in% c("Coefficient", "t-value")) {
    p <- ggplot(df, aes(x = Timestamp, y = V,
                        color = Spec, fill = Spec)) + geom_line()
  }
  else if (modus %in% c("Condition", "Quantile")) {
    p <- ggplot(df, aes_string(x = "Timestamp", y = "V",
                               color = modus, fill = modus)) + geom_line()
  } else if (modus == "ConditionQuantile") {
    p <- ggplot(df, aes_string(x = "Timestamp", y = "V",
                               color = "Condition", linetype = "Spec")) + geom_line()
  }
  
  # For all plots 
  if (ci == TRUE) {
    if (modus == "Coefficient") {
      p <- p + geom_ribbon(aes(x = Timestamp,
                               ymax = V + (1.96 * V_CI), ymin = V - (1.96 * V_CI)), alpha = 0.25, color = NA)
    } else {
      p <- p + geom_ribbon(aes(x = Timestamp,
                               ymax = V + (1.96 * V_CI), ymin = V - (1.96 * V_CI)), alpha = 0.30, color = NA)
    }
  }
  
  p <- p + geom_hline(yintercept = 0, linetype = "dashed")
  p <- p + geom_vline(xintercept = 0, linetype = "dashed")
  p <- p + theme(panel.background = element_rect(fill = "#FFFFFF",
                                                 color = "#000000", linewidth = 0.1, linetype = "solid"),
                 panel.grid.major = element_line(linewidth  = 0.3,
                                                 linetype = "solid", color = "#A9A9A9"),
                 panel.grid.minor = element_line(linewidth  = 0.15,
                                                 linetype = "solid", color = "#A9A9A9"),
                 legend.position = "top")
  
  # Conditional modifications
  if (is.vector(ylims) == TRUE) {
    p <- p + scale_y_reverse(limits = c(ylims[2], ylims[1]))
  } else {
    p <- p + scale_y_reverse()
  }
  if (modus == "Quantile") {
    p <- p + labs(y = yunit, x = "Time (ms)", title = ttl)
    p <- p + scale_color_manual(name = "N400 Quantile",
                                labels = leg_labs,
                                values = leg_vals)
    p <- p + scale_fill_manual(name = "N400 Quantile",
                               labels = leg_labs,
                               values = leg_vals)
  } else if (modus == "Condition") {
    p <- p + labs(y = yunit, x = "Time (ms)", title = ttl)
    p <- p + scale_color_manual(name = "Condition",
                                labels = leg_labs, values = leg_vals)
    p <- p + scale_fill_manual(name = "Condition",
                               labels = leg_labs, values = leg_vals)
  } else if (modus == "ConditionQuantile") {
    p <- p + labs(y = yunit, x = "Time (ms)", title = ttl)
    p <- p + scale_linetype_manual(name = "Quantile",
                                   labels = leg_labs, values = leg_vals)
    p <- p + scale_color_manual(name = "Condition",
                                labels = c("A: E+A+", "B: E+A-", "C: E-A+", "D: E-A-"),
                                values = c("#000000", "#BB5566", "#004488", "#DDAA33"))
  } else if (modus == "Coefficient") {
    p <- p + labs(y = "Intercept + Coefficient", x = "Time (ms)",
                  title = ttl)
    p <- p + scale_color_manual(name = modus,
                                labels = leg_labs, values = leg_vals)
    p <- p + scale_fill_manual(name = modus,
                               labels = leg_labs, values = leg_vals)
  } else if (modus == "t-value") {
    p <- p + labs(y = "T-value", x = "Time (ms)", title = ttl)
    p <- p + scale_color_manual(name = "Predictor",
                                labels = leg_labs, values = leg_vals)
    p <- p + scale_fill_manual(name = "Predictor",
                               labels = leg_labs, values = leg_vals)
    p <- p + geom_point(data=sig_dt,
                        aes(x = Timestamp, y = posit, shape = sig), size = 2)
    p <- p + scale_shape_manual(values = c(32, 108),
                                name = "Corrected p-values",
                                labels = c("Nonsignificant", "Significant"))
    for (i in 1:length(tws)) {
      p <- p + annotate("rect", 
                        xmin = tws[[i]][1], 
                        xmax = tws[[i]][2],
                        ymin = ylims[1], 
                        ymax = ylims[2], 
                        alpha = 0.15)
    }
  }
  
  p
}

plot_single_elec <- function(
    data,
    e,
    file = FALSE,
    title = "ERPs",
    yunit = paste0("Amplitude (", "\u03BC", "Volt\u29"),
    ylims = NULL,
    modus = "Condition",
    tws = list(c(250, 400), c(600, 1000)),
    ci = TRUE,
    leg_labs,
    leg_vals,
    omit_legend = FALSE,
    save_legend = FALSE
) { 
  if (modus %in% c("Tertile", "Quantile", "Condition")) {
    cols <- c("Spec", "Timestamp", modus)
  } else if (modus == "ConditionQuantile") {
    cols <- c("Spec", "Timestamp", modus, "Condition")
  }
  else if (modus %in% c("Coefficient", "t-value")) {
    data[,"Spec"] <- as.factor(data$Spec)
    cols <- c("Spec", "Timestamp")
  }
  
  # Make individual plots
  plotlist <- vector(mode = "list", length = length(e))
  if (modus == "t-value") {
    for (i in 1:length(e)) {
      varforward <- c(e[i], paste0(e[i], "_CI"), paste0(e[i], "_sig"))
      plotlist[[i]] <- plot_grandavg_ci(cbind(data[, ..cols],
                                              data[, ..varforward]), e[i], yunit,
                                        ylims, modus, tws, ci = FALSE,
                                        leg_labs = leg_labs, leg_vals = leg_vals)
    }
  } else if (modus %in% c("Coefficient", "Tertile")) {
    for (i in 1:length(e)) {
      varforward <- c(e[i], paste0(e[i], "_CI"))
      plotlist[[i]] <- plot_grandavg_ci(cbind(data[, ..cols],
                                              data[, ..varforward]), e[i], yunit = yunit,
                                        ylims = ylims, modus = modus, ci = ci,
                                        leg_labs = leg_labs, leg_vals = leg_vals)
    }
  } else if (modus %in% c("Tertile", "Quantile", "Condition",
                          "ConditionQuantile")) {
    for (i in 1:length(e)) {
      varforward <- c(e[i], paste0(e[i], "_CI"))
      plotlist[[i]] <- plot_grandavg_ci(cbind(data[, ..cols],
                                              data[, ..varforward]), e[i], yunit = yunit,
                                        ylims = ylims, modus = modus, ci = ci,
                                        leg_labs = leg_labs, leg_vals = leg_vals)
    }
  }
  
  # theme formatting
  gg <- plotlist[[1]]
  gg <- gg + theme(legend.key.size = unit(0.5, 'cm'), 
                   legend.key.height = unit(0.5, 'cm'), 
                   legend.key.width = unit(0.5, 'cm'), 
                   legend.title = element_text(size = 7), 
                   legend.text = element_text(size = 5))
  gg <- gg + theme(plot.title = element_text(size = 7.5))
  
  if (omit_legend) {
    if (save_legend) {
      lgnd <- get_legend(gg)
      file_trimmed <- strtrim(file, nchar(file) - 4)
      ggsave(paste0(file_trimmed, "_wavelegend.pdf"),
             lgnd, device = cairo_pdf,
             width = 3.5, height = 0.5)
    }
    gg <- gg + theme(legend.position = "none")
    gg <- arrangeGrob(gg + ggtitle(paste0(e, ": ", title)),
                      heights = c(10, 0.25))
  } else {
    legend <- get_legend(gg)
    nl <- theme(legend.position = "none")
    gg <- arrangeGrob(gg + nl + ggtitle(paste0(e, ": ", title)),
                      legend, heights = c(10, 2))
  }
  
  if (file != FALSE) {
    ggsave(file, gg, device = cairo_pdf, width = 3, height = 3)
  } else {
    plotlist[[1]]
  }
}

plot_nine_elec <- function(
    data,
    e,
    file = FALSE,
    title = "ERPs",
    yunit = paste0("Amplitude (", "\u03BC", "Volt\u29"),
    ylims = NULL,
    modus = "Condition",
    tws = list(c(250, 400), c(600, 1000)),
    ci = TRUE,
    leg_labs,
    leg_vals
) {
  if (modus %in% c("Tertile", "Quantile", "Condition")) {
    cols <- c("Spec", "Timestamp", modus)
  } else if (modus %in% c("Coefficient", "t-value")) {
    data[,"Spec"] <- as.factor(data$Spec)
    cols <- c("Spec", "Timestamp")
  }
  
  plotlist <- vector(mode = "list", length = length(e))
  if (modus == "t-value") {
    for (i in 1:length(e)) {
      varforward <- c(e[i], paste0(e[i], "_CI"), paste0(e[i], "_sig"))
      plotlist[[i]] <- plot_grandavg_ci(cbind(data[, ..cols],
                                              data[, ..varforward]), e[i], yunit,
                                        ylims, modus, tws, ci = FALSE,
                                        leg_labs = leg_labs, leg_vals = leg_vals)
    }
  } else if (modus %in% c("Coefficient", "Tertile")) {
    for (i in 1:length(e)) {
      varforward <- c(e[i], paste0(e[i], "_CI"))
      plotlist[[i]] <- plot_grandavg_ci(cbind(data[, ..cols],
                                              data[, ..varforward]), e[i], yunit = yunit,
                                        ylims = ylims, modus = modus, ci = ci,
                                        leg_labs = leg_labs, leg_vals = leg_vals)
    }
  } else if (modus %in% c("Tertile", "Quantile", "Condition")) {
    for (i in 1:length(e)) {
      varforward <- c(e[i], paste0(e[i], "_CI"))
      plotlist[[i]] <- plot_grandavg_ci(cbind(data[, ..cols],
                                              data[, ..varforward]), e[i], yunit = yunit,
                                        ylims = ylims, modus = modus, ci = ci,
                                        leg_labs = leg_labs, leg_vals = leg_vals)
    }
  }
  legend <- get_legend(plotlist[[1]])
  nl <- theme(legend.position = "none")
  gg <- arrangeGrob(arrangeGrob(
    plotlist[[1]] + nl + labs(x = ""),
    plotlist[[2]] + nl + labs(x = "", y = ""),
    plotlist[[3]] + nl + labs(x = "", y = ""),
    plotlist[[4]] + nl + labs(x = ""),
    plotlist[[5]] + nl + labs(x = "", y = ""),
    plotlist[[6]] + nl + labs(x = "", y = ""),
    plotlist[[7]] + nl,
    plotlist[[8]] + nl + labs(y = ""),
    plotlist[[9]] + nl + labs(y = ""),
    layout_matrix = matrix(1:9, ncol = 3, byrow = TRUE)), legend,
    nullGrob(),
    heights = c(10, 2, 0.4), top = textGrob(title))
  if (file != FALSE) {
    ggsave(file, gg, device = cairo_pdf, width = 7, height = 7)
  } else {
    gg
  }
}

plot_full_elec <- function(
    data,
    e,
    file = FALSE,
    title = "ERPs",
    yunit = paste0("Amplitude (", "\u03BC", "Volt\u29"),
    ylims = NULL,
    modus = "Condition",
    tws = list(c(300, 500), c(600, 1000)),
    ci = FALSE,
    leg_labs,
    leg_vals
) {
  if (modus %in% c("Tertile", "Quantile", "Condition")) {
    cols <- c("Spec", "Timestamp", modus)
  } else if (modus %in% c("Coefficient", "t-value")) {
    data[,"Spec"] <- as.factor(data$Spec)
    cols <- c("Spec", "Timestamp")
  }
  
  plotlist <- vector(mode = "list", length = length(e))
  if (modus == "t-value") {
    for (i in 1:length(e)) {
      varforward <- c(e[i], paste0(e[i], "_CI"), paste0(e[i], "_sig"))
      plotlist[[i]] <- plot_grandavg_ci(cbind(data[, ..cols],
                                              data[, ..varforward]), e[i], yunit,
                                        ylims, modus, tws, ci = FALSE,
                                        leg_labs = leg_labs, leg_vals = leg_vals)
    }
  } else if (modus %in% c("Coefficient", "Tertile")) {
    for (i in 1:length(e)) {
      varforward <- c(e[i], paste0(e[i], "_CI"))
      plotlist[[i]] <- plot_grandavg_ci(cbind(data[, ..cols],
                                              data[, ..varforward]), e[i], yunit = yunit,
                                        ylims = ylims, modus = modus, ci = ci,
                                        leg_labs = leg_labs, leg_vals = leg_vals)
    }
  } else if (modus %in% c("Tertile", "Quantile", "Condition")) {
    for (i in 1:length(e)) {
      varforward <- c(e[i], paste0(e[i], "_CI"))
      plotlist[[i]] <- plot_grandavg_ci(cbind(data[, ..cols],
                                              data[, ..varforward]), e[i], yunit = yunit,
                                        ylims = ylims, modus = modus, ci = ci,
                                        leg_labs = leg_labs, leg_vals = leg_vals)
    }
  }
  
  no <- ggplot(data.frame()) + theme_minimal()
  legend_theme <- theme(legend.key.size = unit(4, 'cm'),
                        legend.key.height = unit(1, 'cm'),
                        legend.key.width = unit(1, 'cm'),
                        legend.title = element_text(size = 20),
                        legend.text = element_text(size = 18))
  legend <- get_legend(plotlist[[1]] + legend_theme)
  
  nl <- theme(legend.position = "none")
  nla <- labs(x = "", y = "")
  ngrid <- theme(panel.background = element_rect(fill=NA, color = NA),
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank())
  axes <- theme(axis.line.x = element_line(color = "black", linewidth = 0.2),
                axis.line.y = element_line(color = "black", linewidth = 0.2))
  for (i in 1:length(e)) {
    plotlist[[i]] <- plotlist[[i]] + nl + nla + ngrid + axes
  }
  
  gg <- arrangeGrob(
    textGrob(title, gp = gpar(fontsize = 22)),
    arrangeGrob(
    # row 1:  .   .    Fp1  .    Fp2  .    .
    no, no,            plotlist[[1]],  no,             plotlist[[2]],  no, no,
    # row 2:  .   F7   F3   Fz   F4   F8   .
    no, plotlist[[3]], plotlist[[4]],  plotlist[[5]],  plotlist[[6]],  plotlist[[7]],  no,
    # row 3:  FT9 FC5  FC1  .    FC2  FC6  FT10
    plotlist[[8]], plotlist[[9]], plotlist[[10]], no, plotlist[[11]], plotlist[[12]], plotlist[[13]],
    # row 4:  .   T7   C3   Cz   C4   T8   .
    no, plotlist[[14]],plotlist[[15]], plotlist[[16]], plotlist[[17]], plotlist[[18]], no,
    # row 5:  .   CP5  CP1  .    CP2  CP6  .
    no, plotlist[[19]],plotlist[[20]], no,             plotlist[[21]], plotlist[[22]], no,
    # row 6:  .   P7   P3   Pz   P4   P8   .
    no, plotlist[[23]],plotlist[[24]], plotlist[[25]], plotlist[[26]], plotlist[[27]], no,
    # row 7:  .   .    O1   Oz   O2   .    .
    no, no,            plotlist[[28]], plotlist[[29]], plotlist[[30]], no, no,
    layout_matrix = matrix(1:49, ncol = 7, byrow = TRUE)),
    legend,
      heights = c(1, 15, 1))
  if (file != FALSE) {
    ggsave(file, gg, device = cairo_pdf, width = 16, height = 14)
  } else {
    gg
  }
}

plot_topo <- function(
    data,
    file,
    tw = c(600, 1000),
    cond_man,
    cond_base,
    label = "Amplitude (μV)",
    add_title = "",
    omit_legend = FALSE,
    save_legend = FALSE,
    subtitle = ""
) {
  first_elec_ind <- grep("Fp1$", colnames(data))
  electrodes <- colnames(data)[c(first_elec_ind:(ncol(data) - 26))]
  
  CI_elecs <- grep("_CI", colnames(data))
  data <- data[, !..CI_elecs]
  data <- data[, !c("Type", "Spec")]
  
  data_m <- melt(data, id.vars = c("Timestamp", "Condition"),
                 variable.name = "Electrode", value.name = "EEG")
  
  data_m_tw <- data_m[(Timestamp >= tw[1] & Timestamp <= tw[2]), ]
  
  clean_man <- gsub("Cond_", "", cond_man)
  clean_base <- gsub("Cond_", "", cond_base)
  new_title <- paste0("Estimate ", clean_man, " - ", clean_base)
  
  generate_topo(data_m_tw, file, tw, cond_man, cond_base,
                amplim = 2.8, elec = electrodes,
                title = new_title,
                label = label, omit_legend, save_legend,
                subtitle = subtitle)
}

generate_topo <- function(
    data, file, tw, cond_man, cond_base, amplim, elec, title, label, omit_legend, save_legend,
    subtitle = ""
) {
  data_agg <- data[, lapply(.SD, mean), by = list(Electrode, Condition), .SDcols = c("EEG")]
  if (!grepl("t-values", file)) {
    data_diff <- compute_difference(data_agg, cond_man, cond_base, title)
  } else {
    data_diff <- data_agg[Condition == cond_man, c("Condition", "Electrode", "EEG")]
  }
  
  elec_locs <- fread("standard_30_clean.loc", sep = "\t",
                     col.names = c("ElecNum", "Theta", "Radius", "Electrode"))
  
  elec_locs <- elec_locs[Electrode %in% elec]
  elec_locs$RadianTheta <- pi / 180 * elec_locs$Theta
  elec_locs$x <- elec_locs$Radius * sin(elec_locs$RadianTheta)
  elec_locs$y <- elec_locs$Radius * cos(elec_locs$RadianTheta)
  data_diff_locs <- merge(elec_locs, data_diff, by = "Electrode")
  
  interprep <- data.table(x = data_diff_locs$x, y = data_diff_locs$y, z = data_diff_locs$EEG)
  
  rmax <- .75  
  grid_res <- 67 
  xo <- seq(min(-rmax, interprep$x), max(rmax, interprep$x), length = grid_res)
  yo <- seq(max(rmax, interprep$y), min(-rmax, interprep$y), length = grid_res)
  
  interpol <- data.table(v4_interp(interprep, xo, yo))
  interpol_m <- melt(interpol, id.vars = "x", variable.name = "y", value.name = "EEG")
  interpol_m$y <- as.numeric(as.character(interpol_m$y))
  
  interpol_m$incircle <- (interpol_m$x)^2 + (interpol_m$y)^2 < 0.7 ^ 2
  interpol_m <- interpol_m[interpol_m$incircle, ]
  interpol_m <- interpol_m[!is.na(interpol_m$EEG), ]
  
  amplim_plusmin <- c(-amplim, amplim) 
  my_spectrum <- colorRampPalette(c("#00007F", "blue", "#0080ff", "white",
                                    "white", "white", "#ff6969", "red", "#a90101"))
  
  head_shape <- circle_fun(c(0, 0), 1.022, npoints = 100)
  nose <- data.table(x = c(-0.075, 0, .075), y = c(.511, .591, .511))
  mask_ring <- circle_fun(diameter = 1.42)
  
  p <- ggplot(interpol_m, aes(x = x, y = y, fill = EEG))
  p <- p + geom_raster() + stat_contour(aes(z = EEG), binwidth = 0.5)
  p <- p + theme_topo()
  
  p <- p + labs(title = title, subtitle = subtitle)
  p <- p + theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 7.5, hjust = 0.5),
    plot.margin = margin(5, 5, 5, 5)
  )
  p <- p + geom_path(data = mask_ring, aes(x, y, z = NULL, fill = NULL), colour = "white", size = 6)
  p <- p + scale_fill_gradientn(colours = my_spectrum(10), limits = amplim_plusmin, guide = "colourbar", oob = scales::squish, name = label)
  p <- p + geom_point(data = data_diff_locs, aes(x, y), size = 1)
  p <- p + geom_path(data = head_shape, aes(x, y, z = NULL, fill = NULL), size = 1.5)
  p <- p + geom_path(data = nose, aes(x, y, z = NULL, fill = NULL), size = 1.5)
  p <- p + coord_equal()
  
  if (omit_legend) {
    if (save_legend) {
      lgnd <- get_legend(p)
      ggsave(paste0(file, "_topolegend.pdf"), lgnd, device = cairo_pdf, width = 1.5, height = 4)
    }
    p <- p + theme(legend.position = "none")
  }
  
  ggsave(paste0(file, "_", cond_man, "_", tw[1], "-", tw[2], ".pdf"), p, device = cairo_pdf, width = 4, height = 4.8)
}

compute_difference <- function(data, cond_man, cond_base, name) {
  data_diff <- data[Condition == cond_man, ]$EEG - data[Condition == cond_base, ]$EEG
  data.table(Timestamp = unique(data$Timestamp), Condition = factor(name), Electrode = unique(data$Electrode), EEG = data_diff)
}

circle_fun <- function(center = c(0, 0), diameter = 1, npoints = 100) {
  r <- diameter / 2
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.table(x = xx, y = yy))
}

v4_interp <- function(df, xo, yo, rmax = .75, gridRes = 67) {
  xo <- matrix(rep(xo, length(yo)), nrow = length(xo), ncol = length(yo))
  yo <- t(matrix(rep(yo, length(xo)), nrow = length(yo), ncol = length(xo)))
  xy <- df$x + df$y * sqrt(as.complex(-1))
  d <- matrix(rep(xy, length(xy)), nrow = length(xy), ncol = length(xy))
  d <- abs(d - t(d))
  diag(d) <- 1
  g <- (d^2) * (log(d) - 1) 
  diag(g) <- 0
  weights <- qr.solve(g, df$z)
  xy <- t(xy)
  outmat <- matrix(nrow = gridRes,ncol = gridRes)
  for (i in 1:gridRes) {
    for (j in 1:gridRes) {
      test4 <- abs((xo[i, j] + sqrt(as.complex(-1)) * yo[i, j]) - xy)
      g <- (test4^2) * (log(test4) - 1)
      outmat[i, j] <- g %*% weights
    }
  }
  outDf <- data.table(x = xo[, 1], outmat)
  names(outDf)[1:length(yo[1, ]) + 1] <- yo[1, ]
  outDf
}

theme_topo <- function(base_size = 12) {
  theme_bw(base_size = base_size) %+replace%
    theme(rect = element_blank(), line = element_blank(), axis.text = element_blank(), axis.title = element_blank())
}

items_and_means <- function(data, Predictor) {
  data_items <- data[, lapply(.SD, mean), by = list(Item, Condition), .SDcols = c(Predictor)]
  data_means <- data[, lapply(.SD, mean), by = list(Condition), .SDcols = c(Predictor)]
  data_means <- aggregate(as.formula(paste0(Predictor, "~ Condition")), data, FUN = mean)
  list(data_items, data_means)
}

plot_density <- function(data, data_means, ylab, xlab, predictor, leg_labs, leg_vals, ylimits, xbreaks) {
  p <- ggplot(data, aes_string(x = predictor, color = "Condition", fill = "Condition"))
  p <- p + geom_density(alpha = 0.4) + theme_minimal()
  p <- p + ylim(ylimits)
  p <- p + geom_vline(data = data_means, aes_string(xintercept = predictor, color = "Condition"), linetype = "dashed")
  p <- p + scale_color_manual(labels = leg_labs, values = leg_vals)
  p <- p + scale_fill_manual(labels = leg_labs, values = leg_vals)
  p <- p + scale_x_continuous(name = xlab, breaks = xbreaks)
  p <- p + labs(y = ylab)
  p <- p + theme(legend.position = "bottom")
  p
}

plot_rSPR <- function(
    data, file, yunit, title, ylims = NULL, modus = "Condition", leg_labs, leg_vals
) {
  if (modus == "t-value"){
    sig_dt <- data[, c("Region", "Spec", "sig")]
    sig_dt$posit <- rep(seq(ylims[1] + 2, ylims[1] + 4, length = length(unique(data$Spec))), length = nrow(sig_dt))
    sig_dt$sig <- factor(sig_dt$sig, levels = c(1, 0), labels = c("sign", "insign"))
  } else if (modus == "coefficients") { 
  } else {
    data$Spec <- data$Condition
  }
  
  p <- ggplot(data, aes(x = Region, y = logRT, color = Spec, group = Spec)) +
    geom_point(size = 2.5, shape = "cross") + geom_line(size = 0.5)
  p <- p + theme_minimal()
  p <- p + geom_errorbar(aes(ymin = logRT - logRT_CI, ymax = logRT + logRT_CI), width = .1, size = 0.3)
  
  if (modus == "coefficients") {
    p <- p + scale_color_manual(name = "Coefficients", labels = leg_labs, values = leg_vals)
  } else if (modus == "t-value") {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed")
    p <- p + scale_color_manual(name = "Z-value", labels = leg_labs, values = leg_vals)
    p <- p + geom_point(data = sig_dt, aes(x = Region, y = posit, shape = sig), size = 2.5)
    p <- p + scale_shape_manual(values = c(20, 32), name = "P-values", labels = c("Significant", "Nonsignificant"))
  } else { 
    p <- p + scale_color_manual(name = "Condition", labels = leg_labs, values = leg_vals)
  }
  
  if (is.vector(ylims) == TRUE) {
    p <- p + ylim(ylims[1], ylims[2])
  }
  
  p <- p + theme(plot.title = element_text(size = 8),
                 axis.text.x = element_text(size = 7),
                 legend.position = "bottom",
                 legend.text = element_text(size = 5),
                 legend.title = element_text(size = 4),
                 legend.box = "vertical",
                 legend.spacing.y = unit(-0.2, "cm"),
                 legend.margin = margin(0, 0, 0, 0),
                 legend.box.margin = margin(-10, -10, -10, -50))
  p <- p + labs(x = "Region", y = yunit, title = title)
  
  if (file != FALSE) {
    ggsave(file, p, device = cairo_pdf, width = 3, height = 3)
  } else {
    p
  }
}