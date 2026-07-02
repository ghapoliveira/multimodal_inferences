# benjamini-hochberg.r
# Applies Benjamini-Hochberg FDR correction to t/p-values within each time window
# and predictor; writes corrected values back plus a boolean "_sig" column per electrode.
# Timepoints outside the windows are zeroed.
#
# See requirements_R.txt
#
# This was adapted from Christoph Aurnhammer's code (https://github.com/caurnhammer/psyp23rerps).

library(data.table)

bh_apply_wide <- function(
        data,
        elec,
        alpha=0.05,
        # Adapted to our windows of interest
        tws = list(c(250, 350), c(300, 500), c(500, 1000))) {
        
        preds <- unique(data$Spec)
        keep_ts <- c()
        
        # Adapted to our electrode cap (values were hard-coded before)
        elec_idx <- which(colnames(data) %in% elec)
        num_elec <- length(elec_idx)
        stopifnot(length(elec_idx) == length(elec))
        
        for (tw in tws) {
                keep_ts <- c(keep_ts, seq(tw[1], tw[2]))
                for (p in preds) {
                        df <- data[Spec == p & Timestamp >= tw[1] &
                                               Timestamp <= tw[2], ..elec_idx]
                        uncorrected <- unlist(df)
                        corrected <- p.adjust(uncorrected, method = "fdr")
                        corrected_matrix <- matrix(corrected,
                                                   nrow = nrow(df), ncol = num_elec)

                        data[Spec == p & Timestamp >= tw[1] &
                                         Timestamp <= tw[2],
                                         (elec_idx)] <- data.table(corrected_matrix)
                                         
                        sig_names <- paste0(colnames(data)[elec_idx], "_sig")
                        data[Spec == p & Timestamp >= tw[1] &
                                         Timestamp <= tw[2],
                                         (sig_names)] <- data.table(corrected_matrix < alpha)
                }
        }
        sigcols <- grep("_sig", colnames(data))
        data[!(Timestamp %in% keep_ts), c(elec_idx, sigcols)] <- 0
        data
}