head(select(data_sem_regiao, -c(Médicos, População, Leitos, `Renda Total`))[, 1:5]) %>%
    format_tab("Algumas observações do banco de dados disponível.", digits = 2, "latex")

head(select(data_sem_regiao, -c(Médicos, População, Leitos, `Renda Total`))[, 6:9]) %>%
    format_tab("Algumas observações do banco de dados disponível.", digits = 2, "latex")

head(select(data_sem_regiao, c(Médicos, População, Leitos, `Renda Total`))) %>%
    format_tab("\\label{table:head}Algumas observações do banco de dados disponível.", digits = 2, "latex")

describe(data_sem_regiao) %>%
    select(-c(vars, n, trimmed, mad, range)) %>%
    rename_all(~ c(
        "Média", "DP", "Mediana",
        "Min", "Max", "Skew", "Kurtosis", "EP"
    )) %>%
    format_tab("\\label{table:desc}Medidas descritivas das variáveis mantidas durante o estudo.", "latex", digits = 0)

(ggplot(
    pivot_longer(data_sem_regiao, cols = -Médicos),
    aes(x = value, y = Médicos, ncol(data_sem_regiao) - 1)
) +
    geom_point() +
    facet_wrap(~name, scales = "free_x") +
    theme_bw() +
    ylab("Número de médicos") +
    xlab("Valor")) %>%
    ggsave(filename = "assets/dispersao_y.png", .)

(ggplot(
    pivot_longer(train_df, cols = -c(`Região geográfica`, Médicos)),
    aes(x = log(value), y = log(Médicos), color = `Região geográfica`)
) +
    geom_point() +
    facet_wrap(~name, scales = "free_x") +
    theme_bw() +
    ylab("Número de médicos") +
    xlab("Valor") +
    theme(legend.position = "bottom")) %>%
    ggsave(filename = "assets/dispersao_logxlogy.png", .)


cor(train_df[, lin], train_df$Médicos) %>%
    cbind(cor(log(train_df[, lin]), log(train_df$Médicos))) %>%
    as.data.frame() %>%
    rename_all(~ c("r_1", "r_2")) %>%
    mutate("$Z_c$" = (atanh(r_1) - atanh(r_2)) / sqrt(2 / (nrow(train_df) - 3))) %>%
    round(2) %>%
    mutate("P(Z > $Z_c$)" = format(pnorm(`$Z_c$`, lower.tail = F), digits = 3)) %>%
    format_tab("\\label{table:corteste}Teste de Fisher para correlação linear simples entre o número de médicos e algumas variáveis explicativas.", format = "latex")

ggsave(filename = "assets/cor_medicos.png", cor_matrix_plot(select(data_sem_regiao, c(lin, "Médicos"))))

anova_reduzida(anova(both_medicos)) %>%
    format_tab("\\label{table:anovamed}ANOVA para o MRL encontrado para o número de médicos da cidade", digits = 2, "latex")

summary(both_medicos)[[4]] %>%
    as.data.frame() %>%
    rename_all(~ c("Estimativa", "EP", "$T_c$", "$P(T > |T_c|)$")) %>%
    format_tab("\\label{table:ttestmed}Testes T performados para os coeficientes para o MRL encontrado", digits = 2, "latex")

(ggplot(data = NULL) +
    geom_point(aes(x = 1:(n / 2), y = both_medicos$residuals), size = 2) +
    xlab("Ordem") +
    ylab("Resíduo") +
    scale_x_continuous(breaks = NULL) +
    theme_bw()) %>%
    ggsave(filename = "assets/seq_plot_medicos.png", .)

shapiro.test(both_medicos$residuals)

png(filename = "assets/qqplot_medicos.png")
qqnorm(both_medicos$residuals)
qqline(both_medicos$residuals)
dev.off()

anova_reduzida(alr3::pureErrorAnova(both_medicos)) %>%
    format_tab("\\label{table:lackmed}Teste de falta de ajustamento para o MRL encontrado.", digits = 2, "latex")

# bptest(both_medicos)

summary(gvlma(both_medicos)) %>%
    as.data.frame() %>%
    format_tab("\\label{table:pressupostosmed}Testes para suposições sobre o MRL encontrado.", digits = 2, "latex")
