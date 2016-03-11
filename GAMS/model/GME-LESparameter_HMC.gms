*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
$ontext
   Household model Chile, Maule region

   Name      :   household_GME_LESparameters.gms
   Purpose   :   Estimate gamma and beta parameters for LES
   Author    :   F Fernández
   Date      :   02.10.15
   Since     :   September 2015
   CalledBy  :   .......

   Notes     :

$offtext
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
$onmulti
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*                         INCLUDE SETS AND BASE DATA                           *
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
$include basedata\load_basedata_V2.gms
;

*   ---- definition of current activities in each household
map_has(h,a,s)= yes$X0(h,a,s);
map_hj(h,j) = yes$jcons(h,j) ;

*###############################################################################
*                    GENERAL MAXIMUM ENTROPY PROBLEM                           -
*###############################################################################
*---Variables declaration
Positive variables
prob_B           Probability for beta parameter (marginal budget share)
prob_G           Probability for gamma parameter (uncompresibble consumption)
prob_E           Probability for error
beta_v           beta parameter
gamma_v          gamma parameter


Free variables
ENTRPY           The entropy measure to be maximized
mhu_v            error term

;
*-------------------------------------------------------------------------------
EQUATIONS
eq_Entropy        The objective function - maximizing Entropy
eq_expfunct       linear expenditure system equation plus an error term
eq_beta           beta equation
eq_gamma          gamma equation
eq_mhu            mhu equation
eq_probB          Summing probabilities to 1 - beta
eq_probG          Summing probabilities to 1 - gamma
eq_probE          Summing probabilities to 1 - mhu
eq_betaAccnst     Accounting restriction for beta
eq_gammaAccnst    Accounting restriction for gamma
eq_mhuAccnst      Accounting restriction for mhu
eq_nonAgr         Accounting restriction for non-agricultural goods

;
*-------------------------------------------------------------------------------
**Objective function
eq_Entropy..      ENTRPY =E= -SUM((h,j,eb), prob_B(h,j,eb)*log(prob_B(h,j,eb)))
                      -SUM((h,j,eg), prob_G(h,j,eg)*log(prob_G(h,j,eg)))
                      -SUM((h,j,ee), prob_E(h,j,ee)*log(prob_E(h,j,ee)));


**Data consistency constraints
eq_expfunct(h,j)$(jcons(h,j) gt 0)..   jcons(h,j)*jprice(j) =e= beta_v(h,j)*[(Y_0(h)- sum(jj, gamma_v(h,j)*jprice(j)))]
                                  + gamma_v(h,j)*jprice(j) + mhu_v(h,j);


eq_beta(h,j)..       beta_v(h,j)  =e= sum(eb, prob_B(h,j,eb)*Z1(eb,j))    ;


eq_gamma(h,j)$(jcons(h,j) gt 0)..      gamma_v(h,j) =e= sum(eg, prob_G(h,j,eg)*Z2(eg,j))   ;


eq_mhu(h,j)..        mhu_v(h,j)   =e= sum(ee, prob_E(h,j,ee)*Z3(h,ee,j))   ;                            ;


** Adding-up or Normalization constraints
eq_probB(h,j).. SUM(eb, prob_B(h,j,eb)) =E= 1 ;
eq_probG(h,j).. SUM(eg, prob_G(h,j,eg)) =E= 1 ;
eq_probE(h,j).. SUM(ee, prob_E(h,j,ee)) =E= 1 ;

**Accounting restrictions

eq_betaAccnst(h)..  sum(j, beta_v(h,j)) =e= 1 ;
eq_gammaAccnst(h,j)$(jcons(h,j) gt 0).. gamma_v(h,j) =l= jcons(h,j)    ;
eq_mhuAccnst(h)..   sum(j, mhu_v(h,j)) =e= 0  ;

* Accounting restriction (market goods)
eq_nonAgr(h,m)..   consval(h,m) =e= Y_0(h) - sum(agds, gamma_v(h,agds)*jprice(agds));

*   SOLVING THE MODEL
*-------------------------------- Defining the model ------------------------------
MODEL household_GME_LES OPTIMIZATION
/eq_Entropy
 eq_expfunct
eq_beta
eq_gamma
eq_mhu
eq_probB
eq_probG
eq_probE
eq_betaAccnst
eq_gammaAccnst
eq_mhuAccnst
eq_nonAgr
/ ;
*--------------------- Initial values of variables and bounds ---------------------
prob_B.lo(h,j,eb) = 0.001 ;
prob_G.lo(h,j,eg) = 0.001;
prob_E.lo(h,j,ee)= 0.001;
beta_v.l(h,'nagr-g')=0.5;
*eq_nonAgr.l('H2',m)= 0.45;

*--------------------------------- Solving the model ------------------------------

OPTION NLP = CONOPT3 ;

$ontext
household_GME_LES.scaleopt=1;
*eq_expfunct.scale(h,j) = 1000 ;
beta_v.scale(h,j) = 100 ;
*eq_beta.scale(h,j)= 1000;
gamma_v.scale(h,j)= 1000 ;
*eq_gamma.scale(h,j)= 1000 ;
*eq_nonAgr.scale(h,m)= 1000;
$offtext

SOLVE household_GME_LES using NLP maximizing ENTRPY    ;
*----------------------------------------------------------------------------------

parameter lespar;

lespar(h,j,'beta')= beta_v.l(h,j);
lespar(h,j,'gamma')= gamma_v.l(h,j);

display lespar;

*   ---- create gdx file with LES parameter data

execute_unload 'basedata\lespar.gdx' lespar ;






