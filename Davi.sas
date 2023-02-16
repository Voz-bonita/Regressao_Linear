/*  **********************************************************************   */
/*  **********************************************************************   */
/*  **********************************************************************   */
/*                        COMEÇO DO PROGRAMA DO DAVI                         */
/*  **********************************************************************   */
/*  **********************************************************************   */
/*  **********************************************************************   */


/* ************************** CONFIGURANDO ******************************* */

OPTIONS LS=80 PS=60 NODATE;
libname reg '/home/u61679719/REGLIN';

/* *********************** IMPORTAÇÃO DE DADOS **************************** */
proc import
    datafile='/home/u61679719/REGLIN/dados_trabalho.xls'
         out=reg.trab dbms=xls replace;
    getnames=YES;
run;

proc contents data=reg.trab varnum;
run;

proc print data=reg.trab label;
var x7 x4 x8 x15;

LABEL x7='Número de Médicos Ativos'
      x4='População Total'
      x8='Número de Leitos Hospitalares'
      x15='Renda Total';
run;

/*   **********************    TRANSFORMAÇÕES   ***********************  */
data reg.trab1;
set reg.trab;
lnx4=log(x4);
lnx7=log(x7);
lnx8=log(x8);
lnx15=log(x15);
run;
/* ************************** ANALISE DESCRITIVA ****************************** */
proc univariate data=reg.trab1;
histogram  lnx7 lnx4 lnx8 lnx15;
run;

/*BOXPLOTS*/
proc sgplot data=reg.trab1;
  vbox lnx7;
run;
proc sgplot data=reg.trab1;
  vbox lnx4;
run;
proc sgplot data=reg.trab1;
  vbox lnx8;
run;
proc sgplot data=reg.trab1;
  vbox lnx15;
run;

/*SCATTER*/
proc sgplot data=reg.trab1;
  SCATTER x=lnx7 y=lnx4;
run;
proc sgplot data=reg.trab1;
  SCATTER x=lnx7  y=lnx8;
run;
proc sgplot data=reg.trab1;
  SCATTER x=lnx7  y=lnx15;
run;


/* Aqui podemos comentar a notavel correlação entre as variáveis  */
proc corr data=reg.trab;
     var x7 x4 x8 x15;
run;  



/*  **************************** MODELO DE REGRESSÃO *************************** */

/* CRIANDO OS DOIS DATASETS = CONSTRUÇÃO E VALIDAÇÃO*/

data reg.trab_constr reg.trab_valid;
    set reg.trab1;
    if rand('uniform') < 0.5 then output reg.trab_constr;
    else output reg.trab_valid;
run;

/* CRIAÇÃO DO MODELO VIA STEPWISE  */
proc reg data=reg.trab_constr;
    model lnx7=lnx4 lnx8 lnx15 / selection=stepwise;
run;

proc reg data=reg.trab_constr;
    model lnx7=lnx4 lnx8 lnx15 / clb;
run;

/* VALIDADE DOS COEFICIENTES */

proc reg data=reg.trab_valid;
model lnx7=lnx15 lnx8 lnx4 ;
run;

/* VALIDADE DE PREDIÇÃO */

data b;
set reg.trab_valid;
lnx7est=0.71673-0.76818*lnx4+0.69383*lnx8+1.22192*lnx15;
erropred=lnx7-lnx7est;
erropred2=erropred*erropred;
run;

proc print data=b;
var id lnx7 lnx4 lnx8 lnx15 lnx7est erropred erropred2;
run;

proc means data=b sum mean n var vardef=n;
var erropred2;
run;

/* ANALISANDO MULTICOLINEARIDADE */

proc reg data=reg.trab_constr;
    model lnx7=lnx4 lnx8 lnx15 / selection=stepwise vif;
run;

/*  CRITERIOS DE SELEÇÃO */

symbol v=dot h=1 c=blue;
proc reg data=reg.trab_constr plots=(criteria(label));
model lnx7=lnx4 lnx8 lnx15
/selection= rsquare sse adjrsq cp aic bic;
run;

/* MELHORANDO COM MAIS GRAFICOS */


proc reg data=reg.trab_constr
plots=(diagnostics(stats=all)CRITERIONPANEL
RStudentByLeverage(label)
CooksD(label) criteria rsquare cp
DFFITS(label) DFBETAS(label)
ObservedByPredicted(label));
id id;
model lnx7=lnx4 lnx8 lnx15/influence;
run;

/*  **********************************************************************   */
/*  **********************************************************************   */
/*  **********************************************************************   */
/*                        FIM DO PROGRAMA DO DAVI                            */
/*  **********************************************************************   */
/*  **********************************************************************   */
/*  **********************************************************************   */

--