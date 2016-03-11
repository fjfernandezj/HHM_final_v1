*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
$ontext
   Householdmodel Maule Region Chile

   Name      :   household_coreModel_noRisk.gms
   Purpose   :   Core model definition
   Author    :   F Fernández
   Date      :   30.09.15
   Since     :   September 2015
   CalledBy  :

   Notes     :   This file includes
                 + definition of main model equations
                 + definition of core model
$offtext
$onmulti ;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*                         CORE MODEL DEFINITION                                *
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*---Variables declaration
Variables
U               Weigthed sum of representative farm households' expected income
R               Farm household expected income
Z               Agricultural expected income
x               Agricultural activity levels (ha)
prdq            Produced quantities of goods
sldq            Sold quantities of goods - rented-out tradable factors
bght            Bought quantities of goods- rented-in tradable factors
cnsq            consumed quantities of goods
cs              Self-consumed quantities of goods
prcg            Prices of goods and tradable factors faced by households
IM               Imported quantities of goods and tradable factors
EX               Exported quantities of goods and tradable factors
;

Positive Variables
x
prdq
sldq
bght
cnsq
cs
prcg
IM
EX
HLAB            Hired Labour              (men - working days)
FLAB            Familiy Labour use        (men - working days)
FOUT            Hiring out                (men - working days)
LABEARN         Labor income              (million $CLP)
IM_Lab          Imported quantities of labour
EX_Lab          Exported quantities of labour
IL              Irrigated land
FW              Net water quantity at the farm gate (th m3)
;

*---Equation declaration
Equation
eq_Obj                Objective function
eq_FarmHhinc          Farm household expected income
eq_AgrInc_LP          Agricultural Income with linear Costs
*eq_AgrInc_NLP         Agricultural Income with PMP Cost parameters
eq_tLAND              Land constraint
eq_TotLab             Labour constraint
eq_FamLab             Family labor balance    (days)
eq_LabIncAcc          Labor income accounting  (million CLP$)
eq_Qttyblnce          Quantity balance for goods at farm household level
eq_prdGds             Produced goods at farm household level
eq_prdGds2            Produced goods constraint
eq_prcbnd1            Price band 1
eq_prcbnd2            Price band 2
eq_slknss1            Complementary slackness condition 1
eq_slknss2            Complementary slackness condition 2
eq_buyorsell          Households buy or sell goods not both
eq_cshcnstrnt_LP      Cash constraint Linear
*eq_cshcnstrnt_NLP     Cash constraint PMP lambda parameter
eq_expfunct           Farm household expenditure function
eq_qttbalgood         Quantity balance of goods at aggregated level (commune)
eq_qttbaltrf          Quantity balance of tradable factors at aggregated level (commune)
eq_iLAND              Irrigable land constraint
eq_waterUse           Water accounting equation
eq_water              Water accounting

;

*---Equation definition
eq_Obj..                  U =e=sum((h), w(h)*R(h));

eq_FarmHhinc(h)..         R(h) =e= Z(h) + LABEARN(h)  - sum(j, bght(h,j)*prcg(h,j))+ exinc(h) ;

eq_AgrInc_LP(h)..         Z(h) =e= sum(j, [sldq(h,j)+ cs(h,j)]*prcg(h,j)) + sb(h)
                          - sum((a,s)$map_has(h,a,s), acst(h,a,s)*x(h,a,s))- LabWage*HLAB(h);

********************************************Original FSSIM Agricultural Income eq**********************************************************
*eq_AgrInc_nlp(h,c)..      Z(h,c) =e= sum(j, [sldq(h,c,j)+ cs(h,c,j)]*prcg(h,c,j)) + sb(h)
*                          - sum((a,s), acst(h,c,a,s)*x(h,c,a,s)) - sum((a,aa,s), [d(h,c,a,s) + 0.5*Q(a,aa)*x(h,c,a,s)]*x(h,c,a,s))
*                          - sum(tf, [bght(h,c,tf)+lambda(h,c,tf)]*prcg(h,c,tf)) ;
**************************************************************************************************************************************

*eq_AgrInc_nlp(h,c)..      Z(h,c) =e= sum((a,s)$map_hcas(h,c,a,s),[yld(h,c,a,s)*x(h,c,a,s)]*pprice(a)) + sb(h)
*                          - sum((a,s)$map_hcas(h,c,a,s), ALPHACST(h,c,a,s)*x(h,c,a,s)**BETACST(h,c,a,s)) ;

*eq_AgrInc_nlp(h)..        Z(h) =e= sum(j, [sldq(h,j)+ cs(h,j)]*prcg(h,j)) + sb(h)
*                          - sum((a,s)$map_has(h,a,s), ALPHACST(h,a,s)*x(h,a,s)**BETACST(h,a,s))- LabWage *HLAB(h) ;


eq_tLAND..             sum((h,a,s)$map_has(h,a,s), X(h,a,s)) =L= tland;

eq_iLAND..             sum((h,a)$map_has(h,a,'irr'), X(h,a,'irr')) =L= IL;

eq_TotLab(h)..             sum((a,s)$map_has(h,a,s), labr(a,s)* X(h,a,s))=l=  FLAB(h) + HLAB(h) ;

eq_FamLab(h)..            FLAB(h) + FOUT(h) =e= flab0(h) ;

eq_LabIncAcc(h)..         LABEARN(h) =e=  FOUT(h)* owage;

eq_waterUse(h)..          sum(a$map_has(h,a,'irr'), fir(h,a,'irr')*X(h,a,'irr')) =L= FW(h);

eq_water(h)..             FW(h) =E=  DW(h)*hd;

eq_Qttyblnce(h,j)..       prdq(h,j)+ bght(h,j) =e= sldq(h,j) + cnsq(h,j);

eq_prdGds(h,j)..          prdq(h,j) =e= sum((a,s)$map_has(h,a,s), yl(h,a,s,j)*x(h,a,s));


eq_prdGds2(h,j)..         sum((a,s)$map_has(h,a,s), yl(h,a,s,j)*x(h,a,s)) =e= sldq(h,j) + cs(h,j)    ;


eq_prcbnd1(h,j)..         prcg(h,j) =l= jprice(j)*tb(h,j);

eq_prcbnd2(h,j)..         jprice(j)*ts(h,j) =l= prcg(h,j);

eq_slknss1(h,j)..         sldq(h,j)*(prcg(h,j)-[jprice(j)*ts(h,j)]) =e= 0;

eq_slknss2(h,j)..         bght(h,j)*(prcg(h,j)-[jprice(j)*tb(h,j)]) =e= 0;

eq_buyorsell(h,j)..       sldq(h,j)*bght(h,j) =e= 0;

eq_cshcnstrnt_LP(h)..      sum(j, sldq(h,j)*prcg(h,j)) + sb(h)+ exinc(h) + LABEARN(h)
                         =g= sum(j, bght(h,j)*prcg(h,j)) +  sum((a,s)$map_has(h,a,s), acst(h,a,s)*x(h,a,s)) + LabWage*HLAB(h) ;


*eq_cshcnstrnt_NLP(h)..      sum(j, sldq(h,j)*prcg(h,j))+ sb(h) + exinc(h)
*                         =g= sum(j, bght(h,j)*prcg(h,j)) + sum((a,s)$map_has(h,a,s), ALPHACST(h,a,s)*x(h,a,s)**BETACST(h,a,s)) + LabWage *HLAB(h) ;

eq_expfunct(h,j)$map_hj(h,j)..      cnsq(h,j)*prcg(h,j) =e= beta(h,j)*[R(h)- sum(jj, gamma(h,jj)*prcg(h,jj))]+ gamma(h,j)*prcg(h,j);


eq_qttbalgood(j)..          sum(h, w(h)*sldq(h,j)) +IM(j) =e= sum(h, w(h)*b(h,j)) +EX(j);


eq_qttbaltrf..        sum(h, w(h)*FOUT(h)) + IM_Lab =e= sum(h, w(h)*HLAB(h)) + EX_Lab;

IL.up = 1.2*icland;

*---Model definition
Model  household_noRisk model for the Maule region Chile /
eq_Obj
eq_FarmHhinc
eq_tLAND
eq_TotLab
eq_FamLab
eq_LabIncAcc
eq_Qttyblnce
eq_prdGds
eq_prdGds2
eq_prcbnd1
eq_prcbnd2
eq_slknss1
eq_slknss2
eq_buyorsell
eq_cshcnstrnt_LP
eq_expfunct
eq_qttbalgood
eq_qttbaltrf
eq_iLAND
eq_waterUse
eq_water
/;

********************************Solve statement*****************************

*Solve household_noRisk using nlp maximizing U;
