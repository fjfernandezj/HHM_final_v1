*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
$ontext
   Householdmodel Maule Region Chile

   Name      :   household_core Model.gms +  household_pmpCalibration.gms
   Purpose   :   Core model definition
   Author    :   F Fern�ndez
   Date      :   30.09.15
   Since     :   September 2015
   CalledBy  :

   Notes     :   This file includes
                 + definition of main model equations
                 + definition of core supply model plus consumption equations
                 + Calibration module
                 + Testing different scenarios
$offtext
$onmulti ;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
$include basedata\load_basedata_V2.gms
;

*~~~~~~~~~~~~~~~~~~~~~~~~ BASEYEAR DATA    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*   ---- definition of current activities in each household
map_has(h,a,s)= yes$X0(h,a,s);
map_hj(h,j) = yes$jcons(h,j) ;
aj(a,j) = yes$sum((h,s), yl(h,a,s,j) ne 0);

*   ---- definition of base model parameters
*   ---- LES parameters
$gdxin basedata\lespar.gdx
$load  lespar
$gdxin

;

gamma(h,j)$map_hj(h,j)= lespar(h,j,'gamma');
beta(h,j)$map_hj(h,j)= lespar(h,j,'beta');


*~~~~~~~~~~~~~~~~~~~~~~~~ CALIBRATION PARAMETERS            ~~~~~~~~~~~~~~~~~~*
Parameter
   eps1       "epsilon (activity)"
   eps2       "epsilon (crop)"
   eps3       "epsilon (total crop area)"
   mu1        "dual values from calibration constraints (activity)"
   mu2        "dual values from calibration constraints (group)"
   mu3        "dual values from calibration constraints (total activity area)"
   cvpar      "cost function parameters"
   LambdaL    "land marginal value"

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*                         CORE MODEL DEFINITION                                *
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*---Variables declaration
Variables
U               Weigthed sum of representative farm households' expected income

;

Positive Variables
*--- SUPPLY MODULE
x
R               Farm household expected income
Z               Agricultural expected income
IL              Irrigated land
HLAB            Hired Labour              (men - working days)
FLAB            Familiy Labour use        (men - working days)
FOUT            Hiring out                (men - working days)
LABEARN         Labor income              (million $CLP)
IM_Lab          Imported quantities of labour
EX_Lab          Exported quantities of labour
FW              Net water quantity at the farm gate (th m3)


* --- Consumption Module
prdq
sldq
bght
cnsq
cs

;


*---Equation declaration
Equation
eq_Obj                Objective function
eq_FarmHhinc          Farm household expected income
*** --- SUPPLY MODULE
eq_AgrInc_LP          Agricultural Income with linear Costs
eq_AgrInc_NLP         Agricultural Income with PMP Cost parameters
eq_tLAND              Land constraint
eq_iLAND              Irrigable land constraint
eq_TotLab             Labour constraint
eq_TotLab             Labour constraint
eq_FamLab             Family labor balance    (working days)
eq_LabIncAcc          Labor income accounting  (million CLP$)
eq_qttbaltrf          Quantity balance of tradable factors at aggregated level
eq_waterUse           Water accounting equation
eq_water              Water accounting

*** --- CONSUMPTION MODULE
eq_Qttyblnce          Quantity balance for goods at farm household level
eq_prdGds             Produced goods at farm household level
eq_prdGds2            Produced goods constraint
eq_buyorsell          Households buy or sell goods not both
eq_cshcnstrnt_LP      Cash constraint Linear
eq_expfunct           Farm household expenditure function
;

*---Equation definition
*---Supply module
* --- Objective function
eq_Obj..                  U =e=sum(h, w(h)* R(h));

eq_FarmHhinc(h)..         R(h) =e= Z(h) + sum(ls, LABEARN(h,ls)) + exinc(h) ;

eq_AgrInc_LP(h)..         Z(h) =e= sum(j, [sldq(h,j)+ cs(h,j)]*jprice(j)) + sb(h)
                          - sum((a,s)$map_has(h,a,s), acst(h,a,s)*x(h,a,s)) - sum(j,bght(h,j)*jprice(j)) -sum(ls, LabWage*HLAB(h,ls));

eq_tLAND(h)..              sum((a,s)$map_has(h,a,s), X(h,a,s)) =L= thland(h) ;

eq_iLAND(h)..             sum(a$map_has(h,a,'irr'), X(h,a,'irr')) =L= IL(h);

eq_TotLab(h,ls)..            sum((a,s)$map_has(h,a,s), labreq(h,a,s)* SLR(a,s,ls)*X(h,a,s))=l=  FLAB(h,ls) + HLAB(h,ls) ;

eq_FamLab(h,ls)..            FLAB(h,ls) + FOUT(h,ls) =e= flab0(h) * SLA(ls) ;

eq_LabIncAcc(h,ls)..         LABEARN(h,ls) =e=  FOUT(h,ls)* owage;

eq_qttbaltrf(ls)..            sum(h, w(h)*FOUT(h,ls)) + IM_Lab(ls) =e= sum(h, w(h)*HLAB(h,ls)) + EX_Lab(ls);

eq_waterUse(h)..          sum(a$map_has(h,a,'irr'), fir(h,a,'irr')*X(h,a,'irr')) =L= FW(h);

eq_water(h)..             FW(h) =E=  DW(h)*hd;


eq_Qttyblnce(h,j)..       prdq(h,j)+ bght(h,j) =e= sldq(h,j) + cnsq(h,j);

eq_prdGds(h,j)..          prdq(h,j) =e= sum((a,s)$map_has(h,a,s), yl(h,a,s,j)*x(h,a,s));

eq_prdGds2(h,j)..         sum((a,s)$map_has(h,a,s), yl(h,a,s,j)*x(h,a,s)) =e= sldq(h,j) + cs(h,j)    ;

eq_buyorsell(h,j)..       sldq(h,j)*bght(h,j) =e= 0;

eq_cshcnstrnt_LP(h)..      sum(j, sldq(h,j)*jprice(j)) + sb(h)+ exinc(h) + sum(ls, LABEARN(h,ls))
                        =g= sum(j, bght(h,j)*jprice(j)) +  sum((a,s)$map_has(h,a,s), acst(h,a,s)*x(h,a,s)) + sum(ls, LabWage*HLAB(h,ls)) ;

eq_expfunct(h,j)$map_hj(h,j)..      cnsq(h,j)*jprice(j) =e= beta(h,j)*[R(h)- sum(jj, gamma(h,jj)*jprice(jj))]+ gamma(h,j)*jprice(j);




* consider only potential activities
X.fx(h,a,s)$(not map_has(h,a,s)) = 0;

* bounds on variables
X.up(h,a,s)$map_has(h,a,s) = tland;

FW.up(h)                   = w0(h)*hd;

IL.up(h) = 1.2*ihland(h);


*---Model definition
Model  basesupply model for the Maule region Chile /
eq_Obj
eq_FarmHhinc
eq_AgrInc_LP
eq_tLAND
eq_iLAND
eq_TotLab
eq_FamLab
eq_LabIncAcc
eq_qttbaltrf
eq_waterUse
eq_water
eq_Qttyblnce
eq_prdGds
eq_prdGds2
eq_buyorsell
eq_cshcnstrnt_LP
eq_expfunct
/;



solve basesupply using NLP maximizing U;

*   ---- results parameter
parameter REPORT0 'Activity report by household'
          REPORT1 'Income and Resources report by household'
          REPORT2 'Production and Consumption Report by household'
          REPORT3 'Overall Report'
          REPORT4 'Regional Supply and Demand'
          REPORT5 'Regional Crop Allocation'
          REPORT6 'Labour Market (season)'
;

REPORT0(h,a,s,'baseLP') = x.l(h,a,s);

REPORT1(h,'Income','Household','baseLP') = R.L(h);
REPORT1(h,'Income','Agricultural','baseLP') = Z.L(h);
REPORT1(h,'Land','Total','baseLP') = sum((a,s)$map_has(h,a,s), X.L(h,a,s));
REPORT1(h,'Land','Irrigated','baseLP') = sum((a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT1(h,'Labour','Hired','baseLP') = sum(ls, HLAB.L(h,ls));
REPORT1(h,'Labour','Family','baseLP') =  sum(ls,FLAB.L(h,ls));
REPORT1(h,'Labour','HireOut','baseLP') =  sum(ls,FOUT.L(h,ls));
REPORT1(h,'Labour','Total_Req','baseLP') = sum((a,s,ls)$map_has(h,a,s), labreq(h,a,s)* SLR(a,s,ls)*X.L(h,a,s));
REPORT1(h,'Labour','Marginal','baseLP') = sum(ls,eq_TotLab.M(h,ls));
REPORT1(h,'Water','Level','baseLP') =  sum(a, fir(h,a,'irr')*X.L(h,a,'irr'));
REPORT1(h,'Water','Marginal','baseLP') = eq_waterUse.M(h);

REPORT2(h,j,'Production','baseLP') = prdq.L(h,j);
REPORT2(h,j,'Sold','baseLP') = sldq.L(h,j);
REPORT2(h,j,'Bought','baseLP') = bght.L(h,j);
REPORT2(h,j,'Consumption','baseLP') = cnsq.L(h,j);
REPORT2(h,j,'Self_Cons','baseLP') = cs.L(h,j);

REPORT3('Weight_ExpIncome','Level','baseLP') = U.L;
REPORT3('Ttl_Land','','baseLP')= sum((h,a,s), X.L(h,a,s));
REPORT3('Irr_Land','Level','baseLP') = sum((h,a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT3('Imp_Lab','Level','baseLP') =   sum(ls, IM_Lab.L(ls));
REPORT3('Exp_Lab','Level','baseLP') =   sum(ls, EX_Lab.L(ls));

*~~~~~~~~~~~~~~~~~~~~~~~~ CALIBRATION CONSTRAINTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

*   ---- adding calibration constraints
eps1=0.00001;
eps2=0.0000001;
eps3=0.000001;

equation
   CALIB1    calibration constraints (activity)
;

CALIB1(h,a,s)..  x(h,a,s)$map_has(h,a,s) =l= x0(h,a,s)*(1+eps1);
*CALIB2(h,j)..    prdq(h,j)$map_hj(h,j)   =l= prd0(h,j)*(1+eps1);
*CALIB3(h)..      HLAB(h)                 =l= hlab0(h) *(1+eps3);

Model calibHousehold calibration model MB /
   basesupply
   CALIB1
*   calib2
*   CALIB3
/;

*   ---- solving model with calibration constraints
solve calibHousehold using NLP maximizing U;

REPORT0(h,a,s,'pmpCalib') = x.l(h,a,s);

REPORT1(h,'income','Household','pmpCalib') = R.L(h);
REPORT1(h,'income','Agricultural','pmpCalib') = Z.L(h);
REPORT1(h,'Land','Total','pmpCalib') = sum((a,s)$map_has(h,a,s), X.L(h,a,s));
REPORT1(h,'Land','Irrigated','pmpCalib') = sum((a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT1(h,'Labour','Hired','pmpCalib') = sum(ls, HLAB.L(h,ls));
REPORT1(h,'Labour','Family','pmpCalib') =  sum(ls,FLAB.L(h,ls));
REPORT1(h,'Labour','HireOut','pmpCalib') =  sum(ls,FOUT.L(h,ls));
REPORT1(h,'Labour','Total_Req','pmpCalib') = sum((a,s,ls)$map_has(h,a,s), labreq(h,a,s)* SLR(a,s,ls)*X.L(h,a,s));
REPORT1(h,'Labour','Marginal','pmpCalib') = sum(ls,eq_TotLab.M(h,ls));
REPORT1(h,'Water','Level','pmpCalib') =  sum(a, fir(h,a,'irr')*X.L(h,a,'irr'));
REPORT1(h,'Water','Marginal','pmpCalib') = eq_waterUse.M(h);


REPORT2(h,j,'Production','pmpCalib') = prdq.L(h,j);
REPORT2(h,j,'Sold','pmpCalib') = sldq.L(h,j);
REPORT2(h,j,'Bought','pmpCalib') = bght.L(h,j);
REPORT2(h,j,'Consumption','pmpCalib') = cnsq.L(h,j);
REPORT2(h,j,'Self_Cons','pmpCalib') = cs.L(h,j);


REPORT3('Weight_ExpIncome','Level','pmpCalib') = U.L;
REPORT3('Ttl_Land','','pmpCalib')= sum((h,a,s), X.L(h,a,s));
REPORT3('Irr_Land','Level','pmpCalib') = sum((h,a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT3('Imp_Lab','Level','pmpCalib') =   sum(ls, IM_Lab.L(ls));
REPORT3('Exp_Lab','Level','pmpCalib') =   sum(ls, EX_Lab.L(ls));

* Check parameters (Activities, Labour, water and land)
parameter chPMP, chHlab, chIrrland, chTtlLnd;

* --- check activities
chPMP(h,a,s,'X0')  = x0(h,a,s);
chPMP(h,a,s,'pmpCalib') = x.l(h,a,s);
chPMP(h,a,s,'diff') = chPMP(h,a,s,'X0') - chPMP(h,a,s,'pmpCalib');

* --- check Hired labour
*chHlab(h,'Hlab0')= hlab0(h);
*chHlab(h,'pmpHlab') = HLAB.l(h) ;
*chHlab(h,'diff') = chHlab(h,'Hlab0') - chHlab(h,'pmpHlab')   ;

* --- check Irrigated Land
chIrrland('IL0') = sum((h,a),X0(h,a,'irr')) ;
chIrrland('pmpIrrLand')  = sum((h,a),X.l(h,a,'irr')) ;
chIrrland ('diff')  = chIrrland('IL0') -  chIrrland('pmpIrrLand') ;

* --- check Total Land
chTtlLnd('TtlLand0') = sum((h,a,s), x0(h,a,s));
chTtlLnd('pmpLnd')   = sum((h,a,s), x.l(h,a,s));
chTtlLnd('diff')     =  chTtlLnd('TtlLand0') -  chTtlLnd('pmpLnd') ;


*~~~~~~~~~~~~~~~~~~~ PMP MODEL (cost approach + supply elasticities) ~~~~~~~~~*
*   ---- cost function parameters
* Average cost function  AC = alpha+0.5*beta*X
* Standard version alpha=c, beta=mu/x0
* Exogenous supply elasticities
* No marginal activities

Parameter ALPHACST  "marginal cost intercept"
          BETACST   "marginal cost slope"
          mu1       "dual values from calibration constraints"
          cpar      "cost function parameters"
;

mu1(h,a,s)$map_has(h,a,s)  = CALIB1.M(h,a,s)/w(h);

* --- Different approaches to derive the parameters of the variable cost functions
* ---Depending on the approach that the user want to use we can deactivate or not each option

*** --- PMP 1srt approach (The �average cost� PMP variant)
*ALPHACST(h,a,s)$map_has(h,a,s) = acst(h,a,s)- mu1(h,a,s);
*BETACST(h,a,s)$map_has(h,a,s)  = 2*mu1(h,a,s)/x0(h,a,s);

**** --- PMP approaches using exogenous information

* --- PMP 2nd approach (Helming et al. approach)
*BETACST(h,a,s)$map_has(h,a,s) = (1/selas(a))*((pprice(a))/x0(h,a,s)) ;
*ALPHACST(h,a,s)$map_has(h,a,s) = acst(h,a,s)+ mu1(h,a,s)- BETACST(h,a,s)*x0(h,a,s);

* --- PMP 3rd approach with elasticities
BETACST(h,a,s)$map_has(h,a,s) = (1/selas(a)) ;
ALPHACST(h,a,s)$map_has(h,a,s) = (1/(1+BETACST(h,a,s))) * (acst(h,a,s)+ mu1(h,a,s))*x0(h,a,s)**(-BETACST(h,a,s));

cpar(h,a,s,'alphacst')  = ALPHACST(h,a,s);
cpar(h,a,s,'betacst')   = BETACST(h,a,s);
cpar(h,a,s,'mu1')       = mu1(h,a,s) ;

*   ---- non linear PMP model
equation eq_AgrInc_nlp            "non linear Agricultural expected income function"
         eq_cshcnstrnt_NLP        "Cash constraint PMP  parameters"
;

*** --- Eqs without Implicit marginal costs of labour
*eq_AgrInc_nlp(h)..        Z(h) =e= sum(j, [sldq(h,j)+ cs(h,j)]*jprice(j)) + sb(h)
*                          - sum((a,s)$map_has(h,a,s), ALPHACST(h,a,s)*x(h,a,s)+ 0.5 * BETACST(h,a,s)*x(h,a,s)*x(h,a,s)) - (LabWage*HLAB(h));


*eq_cshcnstrnt_NLP(h)..      sum(j, sldq(h,j)*jprice(j))+ sb(h) + exinc(h) + LABEARN(h)
*                           =g= sum(j, bght(h,j)*jprice(j)) + sum((a,s)$map_has(h,a,s), ALPHACST(h,a,s)*x(h,a,s)+ 0.5 * BETACST(h,a,s)*x(h,a,s)*x(h,a,s)) + (LabWage*HLAB(h)) ;


* --- Related with 3rd approach to estimate parameters
eq_AgrInc_nlp(h)..        Z(h) =e= sum(j, [sldq(h,j)+ cs(h,j)]*jprice(j)) + sb(h)
                          - sum((a,s)$map_has(h,a,s), ALPHACST(h,a,s)*x(h,a,s)** BETACST(h,a,s)*x(h,a,s)) - sum(j,bght(h,j)*jprice(j)) - sum(ls, LabWage*HLAB(h,ls));


eq_cshcnstrnt_NLP(h)..      sum(j, sldq(h,j)*jprice(j))+ sb(h) + exinc(h) + sum(ls, LABEARN(h,ls))
                           =g= sum(j, bght(h,j)*jprice(j)) + sum((a,s)$map_has(h,a,s), ALPHACST(h,a,s)*x(h,a,s)** BETACST(h,a,s)*x(h,a,s)) + sum(ls, LabWage*HLAB(h,ls)) ;


model PMPMODEL modelo PMP /
   eq_Obj
eq_FarmHhinc
eq_AgrInc_nlp
eq_tLAND
eq_iLAND
eq_TotLab
eq_FamLab
eq_LabIncAcc
eq_qttbaltrf
eq_waterUse
eq_water
eq_Qttyblnce
eq_prdGds
eq_prdGds2
eq_buyorsell
eq_cshcnstrnt_NLP
eq_expfunct
/;


solve PMPMODEL using NLP maximizing U;

REPORT0(h,a,s,'pmpModel') = x.l(h,a,s);


REPORT1(h,'income','Household','pmpModel') = R.L(h);
REPORT1(h,'income','Agricultural','pmpModel') = Z.L(h);
REPORT1(h,'Land','Total','pmpModel') = sum((a,s)$map_has(h,a,s), X.L(h,a,s));
REPORT1(h,'Land','Irrigated','pmpModel') = sum((a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT1(h,'Labour','Hired','pmpModel') = sum(ls, HLAB.L(h,ls));
REPORT1(h,'Labour','Family','pmpModel') =  sum(ls,FLAB.L(h,ls));
REPORT1(h,'Labour','HireOut','pmpModel') =  sum(ls,FOUT.L(h,ls));
REPORT1(h,'Labour','Total_Req','pmpModel') = sum((a,s,ls)$map_has(h,a,s), labreq(h,a,s)* SLR(a,s,ls)*X.L(h,a,s)) ;
REPORT1(h,'Labour','Marginal','pmpModel') = sum(ls,eq_TotLab.M(h,ls));
REPORT1(h,'Water','Level','pmpModel') =  sum(a, fir(h,a,'irr')*X.L(h,a,'irr'));
REPORT1(h,'Water','Marginal','pmpModel') = eq_waterUse.M(h);


REPORT2(h,j,'Production','pmpModel') = prdq.L(h,j);
REPORT2(h,j,'Sold','pmpModel') = sldq.L(h,j);
REPORT2(h,j,'Bought','pmpModel') = bght.L(h,j);
REPORT2(h,j,'Consumption','pmpModel') = cnsq.L(h,j);
REPORT2(h,j,'Self_Cons','pmpModel') = cs.L(h,j);


REPORT3('Weight_ExpIncome','Level','pmpModel') = U.L;
REPORT3('Ttl_Land','','pmpModel')= sum((h,a,s), X.L(h,a,s));
REPORT3('Irr_Land','Level','pmpModel') = sum((h,a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT3('Labour','Total_Req','pmpModel') = sum((h,a,s,ls)$map_has(h,a,s), labreq(h,a,s)* SLR(a,s,ls)*X.L(h,a,s))   ;
REPORT3('Imp_Lab','Level','pmpModel') =   sum(ls, IM_Lab.L(ls));
REPORT3('Exp_Lab','Level','pmpModel') =   sum(ls, EX_Lab.L(ls));

* Supply and Demand
REPORT4('REGIONAL',j,'Supply','pmpModel') = sum(h, prdq.L(h,j));
REPORT4('REGIONAL',j,'Consumption','pmpModel') = sum(h, cnsq.L(h,j));
REPORT4('REGIONAL',j,'Self_Cons','pmpModel') = sum(h,cs.L(h,j));
REPORT4('REGIONAL',j,'Revenue/ha','pmpModel') = sum(h, [sldq.l(h,j)+ cs.l(h,j)]*jprice(j)) ;

* Crop pattern at regional level
REPORT5('Total',a,'ha','pmpModel')= sum((h,s), X.L(h,a,s));

* Labour market(Season)
REPORT6('Labour','Imported',ls,'pmpModel') =  IM_Lab.L(ls);
REPORT6('Labour','Exported',ls,'pmpModel') =  EX_Lab.L(ls);

********************************************************************************
*                       Scenario construction
*******************************************************************************
*~~~~~~~~~~~~~~~~~~~~~ Scenario 1: Yields ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*   ---- Scenario 1.1: Yield changes at local level (-10% irrigated crops; -30% rainfed crops)
parameter yield_ori;

yield_ori(h,a,s,j)=yl(h,a,s,j);

yl(h,a,'dry',j)= yield_ori(h,a,'dry',j)*(0.7);
yl(h,a,'irr',j)= yield_ori(h,a,'irr',j)*(0.9);


solve PMPMODEL using NLP maximizing U;

REPORT0(h,a,s,'YdChg') = x.l(h,a,s);

REPORT1(h,'income','Household','YdChg') = R.L(h);
REPORT1(h,'income','Agricultural','YdChg') = Z.L(h);
REPORT1(h,'Land','Total','YdChg') = sum((a,s)$map_has(h,a,s), X.L(h,a,s));
REPORT1(h,'Land','Irrigated','YdChg') = sum((a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT1(h,'Labour','Hired','YdChg') = sum(ls, HLAB.L(h,ls));
REPORT1(h,'Labour','Family','YdChg') =  sum(ls,FLAB.L(h,ls));
REPORT1(h,'Labour','HireOut','YdChg') =  sum(ls,FOUT.L(h,ls));
REPORT1(h,'Labour','Total_Req','YdChg') = sum((a,s,ls)$map_has(h,a,s), labreq(h,a,s)* SLR(a,s,ls)*X.L(h,a,s));
REPORT1(h,'Labour','Marginal','YdChg') = sum(ls,eq_TotLab.M(h,ls));
REPORT1(h,'Water','Level','YdChg') =  sum(a, fir(h,a,'irr')*X.L(h,a,'irr'));
REPORT1(h,'Water','Marginal','YdChg') = eq_waterUse.M(h);
REPORT1(h,'FW','Level','YdChg') = DW(h)*hd;

REPORT2(h,j,'Production','YdChg') = prdq.L(h,j);
REPORT2(h,j,'Sold','YdChg') = sldq.L(h,j);
REPORT2(h,j,'Bought','YdChg') = bght.L(h,j);
REPORT2(h,j,'Consumption','YdChg') = cnsq.L(h,j);
REPORT2(h,j,'Self_Cons','YdChg') = cs.L(h,j);

REPORT3('Weight_ExpIncome','Level','YdChg') = U.L;
REPORT3('Ttl_Land','','YdChg')= sum((h,a,s), X.L(h,a,s));
REPORT3('Irr_Land','Level','YdChg') = sum((h,a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT3('Labour','Total_Req','YdChg') = sum((h,a,s,ls)$map_has(h,a,s), labreq(h,a,s)* SLR(a,s,ls)*X.L(h,a,s))   ;
REPORT3('Imp_Lab','Level','YdChg') =   sum(ls, IM_Lab.L(ls));
REPORT3('Exp_Lab','Level','YdChg') =   sum(ls, EX_Lab.L(ls));

* Supply and Demand
REPORT4('REGIONAL',j,'Supply','YdChg') = sum(h, prdq.L(h,j));
REPORT4('REGIONAL',j,'Consumption','YdChg') = sum(h, cnsq.L(h,j));
REPORT4('REGIONAL',j,'Self_Cons','YdChg') = sum(h,cs.L(h,j));
REPORT4('REGIONAL',j,'Revenue/ha','YdChg') = sum(h, [sldq.l(h,j)+ cs.l(h,j)]*jprice(j)) ;

REPORT5('Total',a,'ha','YdChg')= sum((h,s), X.L(h,a,s));

REPORT6('Labour','Imported',ls,'YdChg') =  IM_Lab.L(ls);
REPORT6('Labour','Exported',ls,'YdChg') =  EX_Lab.L(ls);

*~~~~~~~~~~~~~~~~~ YIELD CHANGE + WATER VARIABILITY SIMULATION ~~~~~~~~~~~~~~~~*
*   ---- water variability simulations without improvement on efficiency
parameter DW_ori
          hd_ori      ;

DW_ori(h)=DW(h);
hd_ori = hd;

DW(h) = [DW_ori(h)*(1-0.3)];

solve PMPMODEL using NLP maximizing U;

REPORT0(h,a,s,'Yd_lessW') = x.l(h,a,s);

REPORT1(h,'income','Household','Yd_lessW') = R.L(h);
REPORT1(h,'income','Agricultural','Yd_lessW') = Z.L(h);
REPORT1(h,'Land','Total','Yd_lessW') = sum((a,s)$map_has(h,a,s), X.L(h,a,s));
REPORT1(h,'Land','Irrigated','Yd_lessW') = sum((a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT1(h,'Labour','Hired','Yd_lessW') = sum(ls, HLAB.L(h,ls));
REPORT1(h,'Labour','Family','Yd_lessW') =  sum(ls,FLAB.L(h,ls));
REPORT1(h,'Labour','HireOut','Yd_lessW') =  sum(ls,FOUT.L(h,ls));
REPORT1(h,'Labour','Total_Req','Yd_lessW') = sum((a,s,ls)$map_has(h,a,s), labreq(h,a,s)* SLR(a,s,ls)*X.L(h,a,s)) ;
REPORT1(h,'Labour','Marginal','Yd_lessW') = sum(ls,eq_TotLab.M(h,ls));
REPORT1(h,'Water','Level','Yd_lessW') =  sum(a, fir(h,a,'irr')*X.L(h,a,'irr'));
REPORT1(h,'Water','Marginal','Yd_lessW') = eq_waterUse.M(h);
REPORT1(h,'FW','Level','Yd_lessW') = DW(h)*hd;

REPORT2(h,j,'Production','Yd_lessW') = prdq.L(h,j);
REPORT2(h,j,'Sold','Yd_lessW') = sldq.L(h,j);
REPORT2(h,j,'Bought','Yd_lessW') = bght.L(h,j);
REPORT2(h,j,'Consumption','Yd_lessW') = cnsq.L(h,j);
REPORT2(h,j,'Self_Cons','Yd_lessW') = cs.L(h,j);


REPORT3('Weight_ExpIncome','Level','Yd_lessW') = U.L;
REPORT3('Ttl_Land','','Yd_lessW')= sum((h,a,s), X.L(h,a,s));
REPORT3('Irr_Land','Level','Yd_lessW') = sum((h,a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT3('Labour','Total_Req','Yd_lessW') = sum((h,a,s,ls)$map_has(h,a,s), labreq(h,a,s)* SLR(a,s,ls)*X.L(h,a,s))   ;
REPORT3('Imp_Lab','Level','Yd_lessW') =   sum(ls, IM_Lab.L(ls));
REPORT3('Exp_Lab','Level','Yd_lessW') =   sum(ls, EX_Lab.L(ls));

* Supply and Demand
REPORT4('REGIONAL',j,'Supply','Yd_lessW') = sum(h, prdq.L(h,j));
REPORT4('REGIONAL',j,'Consumption','Yd_lessW') = sum(h, cnsq.L(h,j));
REPORT4('REGIONAL',j,'Self_Cons','Yd_lessW') = sum(h,cs.L(h,j));
REPORT4('REGIONAL',j,'Revenue/ha','Yd_lessW') = sum(h, [sldq.l(h,j)+ cs.l(h,j)]*jprice(j)) ;

REPORT5('Total',a,'ha','Yd_lessW')= sum((h,s), X.L(h,a,s));

REPORT6('Labour','Imported',ls,'Yd_lessW') =  IM_Lab.L(ls);
REPORT6('Labour','Exported',ls,'Yd_lessW') =  EX_Lab.L(ls);

*   ---- water variability simulations with 20% improvement on efficiency

hd = hd_ori * 1.2;

solve PMPMODEL using NLP maximizing U;

REPORT0(h,a,s,'Yd_IrrEff') = x.l(h,a,s);

REPORT1(h,'income','Household','Yd_IrrEff') = R.L(h);
REPORT1(h,'income','Agricultural','Yd_IrrEff') = Z.L(h);
REPORT1(h,'Land','Total','Yd_IrrEff') = sum((a,s)$map_has(h,a,s), X.L(h,a,s));
REPORT1(h,'Land','Irrigated','Yd_IrrEff') = sum((a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT1(h,'Labour','Hired','Yd_IrrEff') = sum(ls, HLAB.L(h,ls));
REPORT1(h,'Labour','Family','Yd_IrrEff') =  sum(ls,FLAB.L(h,ls));
REPORT1(h,'Labour','HireOut','Yd_IrrEff') =  sum(ls,FOUT.L(h,ls));
REPORT1(h,'Labour','Total_Req','Yd_IrrEff') = sum((a,s,ls)$map_has(h,a,s), labreq(h,a,s)* SLR(a,s,ls)*X.L(h,a,s));
REPORT1(h,'Labour','Marginal','Yd_IrrEff') = sum(ls,eq_TotLab.M(h,ls));
REPORT1(h,'Water','Level','Yd_IrrEff') =  sum(a, fir(h,a,'irr')*X.L(h,a,'irr'));
REPORT1(h,'Water','Marginal','Yd_IrrEff') = eq_waterUse.M(h);
REPORT1(h,'FW','Level','Yd_IrrEff') = DW(h)*hd;

REPORT2(h,j,'Production','Yd_IrrEff') = prdq.L(h,j);
REPORT2(h,j,'Sold','Yd_IrrEff') = sldq.L(h,j);
REPORT2(h,j,'Bought','Yd_IrrEff') = bght.L(h,j);
REPORT2(h,j,'Consumption','Yd_IrrEff') = cnsq.L(h,j);
REPORT2(h,j,'Self_Cons','Yd_IrrEff') = cs.L(h,j);


REPORT3('Weight_ExpIncome','Level','Yd_IrrEff') = U.L;
REPORT3('Ttl_Land','','Yd_IrrEff')= sum((h,a,s), X.L(h,a,s));
REPORT3('Irr_Land','Level','Yd_IrrEff') = sum((h,a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT3('Labour','Total_Req','Yd_IrrEff') = sum((h,a,s,ls)$map_has(h,a,s), labreq(h,a,s)* SLR(a,s,ls)*X.L(h,a,s))   ;
REPORT3('Imp_Lab','Level','Yd_IrrEff') =   sum(ls, IM_Lab.L(ls));
REPORT3('Exp_Lab','Level','Yd_IrrEff') =   sum(ls, EX_Lab.L(ls));

* Supply and Demand
REPORT4('REGIONAL',j,'Supply','Yd_IrrEff') = sum(h, prdq.L(h,j));
REPORT4('REGIONAL',j,'Consumption','Yd_IrrEff') = sum(h, cnsq.L(h,j));
REPORT4('REGIONAL',j,'Self_Cons','Yd_IrrEff') = sum(h,cs.L(h,j));
REPORT4('REGIONAL',j,'Revenue/ha','Yd_IrrEff') = sum(h, [sldq.l(h,j)+ cs.l(h,j)]*jprice(j)) ;

REPORT5('Total',a,'ha','Yd_IrrEff')= sum((h,s), X.L(h,a,s));

REPORT6('Labour','Imported',ls,'Yd_IrrEff') =  IM_Lab.L(ls);
REPORT6('Labour','Exported',ls,'Yd_IrrEff') =  EX_Lab.L(ls);

Option  REPORT0:1
        REPORT1:3
        REPORT2:1
        REPORT3:1
;
Display chPMP, chIrrland, chTtlLnd, REPORT0, REPORT1, REPORT2, REPORT3, REPORT4, REPORT5, REPORT6;
$exit
*   ---- create gdx file with model data

execute_unload '..\results\HouseholdChile_Results.gdx' REPORT0 REPORT1 REPORT2 REPORT3 REPORT4 REPORT5   ;
execute 'gdxxrw.exe ..\results\HouseholdChile_Results.gdx o=..\results\HouseholdChile_Results.xlsx par=REPORT0 rng=REPORT0!A1' ;
execute 'gdxxrw.exe ..\results\HouseholdChile_Results.gdx o=..\results\HouseholdChile_Results.xlsx par=REPORT1 rng=REPORT1!A1' ;
execute 'gdxxrw.exe ..\results\HouseholdChile_Results.gdx o=..\results\HouseholdChile_Results.xlsx par=REPORT2 rng=REPORT2!A1' ;
execute 'gdxxrw.exe ..\results\HouseholdChile_Results.gdx o=..\results\HouseholdChile_Results.xlsx par=REPORT3 rng=REPORT3!A1' ;
execute 'gdxxrw.exe ..\results\HouseholdChile_Results.gdx o=..\results\HouseholdChile_Results.xlsx par=REPORT4 rng=REPORT4!A1' ;
execute 'gdxxrw.exe ..\results\HouseholdChile_Results.gdx o=..\results\HouseholdChile_Results.xlsx par=REPORT5 rng=REPORT5!A1' ;
