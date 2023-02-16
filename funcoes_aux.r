format_tab <- function(df, caption, ...) {
    tabela <- kable(
        df,
        caption = caption,
        booktabs = T,
        ...
    ) %>%
        kable_styling(
            latex_options = c("striped", "hold_position"),
            full_width = F
        )
    return(tabela)
}

cov_matrix_plot <- function(x) {
    return(model.matrix(~ 0 + ., data = x) %>%
        cor(use = "pairwise.complete.obs") %>%
        ggcorrplot(show.diag = F, type = "lower", lab = TRUE, lab_size = 2) +
        theme(axis.text.x = element_text(angle = 90)))
}