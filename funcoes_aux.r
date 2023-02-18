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

cor_matrix_plot <- function(x) {
    return(model.matrix(~ 0 + ., data = x) %>%
        cor(use = "pairwise.complete.obs") %>%
        ggcorrplot(show.diag = F, type = "lower", lab = T, lab_size = 7) +
        theme(axis.text.x = element_text(angle = 30, size = 22)) +
        theme(axis.text.y = element_text(size = 22)))
}

anova_reduzida <- function(anova_base) {
    p <- nrow(anova_base) + 1
    anova_custom <- anova_base[-(p + 1), 1:2] %>%
        as.matrix() %>%
        colSums() %>%
        rbind(anova_base[p - 1, 1:2]) %>%
        rename_all(~ c("gl", "SS")) %>%
        mutate("MS" = SS / gl)

    anova_custom <- cbind(anova_custom,
        "$F_c$" = c(anova_custom$MS[1] / anova_custom$MS[2], NA)
    )

    anova_custom <- cbind(anova_custom,
        "P(F > $F_c$)" = c(pf(anova_custom["$F_c$"][[1]][1], p - 1, anova_base$Df[p - 1], lower.tail = F), NA)
    )

    rownames(anova_custom) <- c("Regressão", "Resíduos")


    return(anova_custom)
}

dummy_reg <- function(df) {
    n <- nrow(df)
    to_dummy <- cbind(1:n, df$`Região geográfica`) %>%
        as.data.frame() %>%
        rename_all(~ c("id", "Região geográfica")) %>%
        mutate("Região geográfica" = factor(`Região geográfica`, levels = 1:4))

    dummy <- dummyVars(" ~ .", data = to_dummy)
    dummy_cols <- data.frame(predict(dummy, newdata = to_dummy))[, 2:4] %>%
        rename_all(~ c("Região1", "Região2", "Região3"))

    return(cbind(select(df, -`Região geográfica`), dummy_cols))
}