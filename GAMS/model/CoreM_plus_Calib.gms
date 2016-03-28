*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
$ontext
   Householdmodel Maule Region Chile

   Name      :   household_core Model.gms +  household_pmpCalibration.gms
   Purpose   :   Core model definition
   Author    :   F Fernández
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
*eq_RscConst           Resource constraints at farm household level (land-labour-water-capital)
eq_tLAND              Land constraint
eq_iLAND              Irrigable land constraint
eq_TotLab             Labour constraint
eq_TotLab             Labour constraint
eq_FamLab             Family labor balance    (days)
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
eq_Obj..                  U =e=sum(h, w(h)*R(h));

eq_FarmHhinc(h)..         R(h) =e= Z(h) + LABEARN(h) + exinc(h) ;

eq_AgrInc_LP(h)..         Z(h) =e= sum(j, [sldq(h,j)+ cs(h,j)]*jprice(j)) + sb(h)
                          - sum((a,s)$map_has(h,a,s), acst(h,a,s)*x(h,a,s)) - (LabWage*HLAB(h));

eq_tLAND..                sum((h,a,s)$map_has(h,a,s), X(h,a,s)) =L= tland;

eq_iLAND..                sum((h,a)$map_has(h,a,'irr'), X(h,a,'irr')) =L= IL;

eq_TotLab(h)..            sum((a,s)$map_has(h,a,s), labreq(h,a,s)* X(h,a,s))=l=  FLAB(h) + HLAB(h) ;

eq_FamLab(h)..            FLAB(h) + FOUT(h) =e= flab0(h) ;

eq_LabIncAcc(h)..         LABEARN(h) =e=  FOUT(h)* owage;

eq_qttbaltrf..        sum(h, w(h)*FOUT(h)) + IM_Lab =e= sum(h, w(h)*HLAB(h)) + EX_Lab;

eq_waterUse(h)..          sum(a$map_has(h,a,'irr'), fir(h,a,'irr')*X(h,a,'irr')) =L= FW(h);

eq_water(h)..             FW(h) =E=  DW(h)*hd;


eq_Qttyblnce(h,j)..       prdq(h,j)+ bght(h,j) =e= sldq(h,j) + cnsq(h,j);

eq_prdGds(h,j)..          prdq(h,j) =e= sum((a,s)$map_has(h,a,s), yl(h,a,s,j)*x(h,a,s));

eq_prdGds2(h,j)..         sum((a,s)$map_has(h,a,s), yl(h,a,s,j)*x(h,a,s)) =e= sldq(h,j) + cs(h,j)    ;

eq_buyorsell(h,j)..       sldq(h,j)*bght(h,j) =e= 0;

eq_cshcnstrnt_LP(h)..      sum(j, sldq(h,j)*jprice(j)) + sb(h)+ exinc(h) + LABEARN(h)
                        =g= sum(j, bght(h,j)*jprice(j)) +  sum((a,s)$map_has(h,a,s), acst(h,a,s)*x(h,a,s)) + (LabWage*HLAB(h)) ;

eq_expfunct(h,j)$map_hj(h,j)..      cnsq(h,j)*jprice(j) =e= beta(h,j)*[R(h)- sum(jj, gamma(h,jj)*jprice(jj))]+ gamma(h,j)*jprice(j);


* consider only potential activities
X.fx(h,a,s)$(not map_has(h,a,s)) = 0;

* bounds on variables
X.up(h,a,s)$map_has(h,a,s) = tland;
*FOUT.up(h) = 0;

IL.up = 1.2*icland;

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
* Consumption eqs
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

;

REPORT0(h,a,s,'baseLP') = x.l(h,a,s);

REPORT1(h,'Income','Household','baseLP') = R.L(h);
REPORT1(h,'Income','Agricultural','baseLP') = Z.L(h);
REPORT1(h,'Land','Total','baseLP') = sum((a,s)$map_has(h,a,s), X.L(h,a,s));
REPORT1(h,'Land','Irrigated','baseLP') = sum((a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT1(h,'Labour','Hired','baseLP') = HLAB.L(h);
REPORT1(h,'Labour','Family','baseLP') =  FLAB.L(h);
REPORT1(h,'Labour','HireOut','baseLP') =  FOUT.L(h);
REPORT1(h,'Labour','Total_Req','baseLP') = sum((a,s), labr(a,s)*X.L(h,a,s));
REPORT1(h,'Labour','Marginal','baseLP') = eq_TotLab.M(h);
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
REPORT1(h,'Labour','Hired','pmpCalib') = HLAB.L(h);
REPORT1(h,'Labour','Family','pmpCalib') =  FLAB.L(h);
REPORT1(h,'Labour','HireOut','pmpCalib') =  FOUT.L(h);
REPORT1(h,'Labour','Total_Req','pmpCalib') = sum((a,s), labr(a,s)*X.L(h,a,s));
REPORT1(h,'Labour','Marginal','pmpCalib') = eq_TotLab.M(h);
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

* Check parameters (Activities, Labour, water and land)
parameter chPMP, chIrrland, chTtlLnd;

* --- check activities
chPMP(h,a,s,'X0')  = x0(h,a,s);
chPMP(h,a,s,'pmpCalib') = x.l(h,a,s);
chPMP(h,a,s,'diff') = chPMP(h,a,s,'X0') - chPMP(h,a,s,'pmpCalib');


* --- check activities
chIrrland('IL0') = sum((h,a),X0(h,a,'irr')) ;
chIrrland('pmpIrrLand')  = sum((h,a),X.l(h,a,'irr')) ;
chIrrland ('diff')  = chIrrland('IL0') -  chIrrland('pmpIrrLand') ;

* --- check activities
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


mu1(h,a,s)$map_has(h,a,s)  = CALIB1.M(h,a,s);

ALPHACST(h,a,s)$map_has(h,a,s) = acst(h,a,s)- mu1(h,a,s);
BETACST(h,a,s)$map_has(h,a,s)  = 2*mu1(h,a,s)/x0(h,a,s);

* PMP 2nd approach with elasticities
*BETACST(h,a,s)$map_has(h,a,s) = (1/selas(a))*((pprice(a))/x0(h,a,s)) ;
*ALPHACST(h,a,s)$map_has(h,a,s) = acst(h,a,s)+ mu1(h,a,s)- BETACST(h,a,s)*x0(h,a,s);

cpar(h,a,s,'alphacst') = ALPHACST(h,a,s);
cpar(h,a,s,'betacst')   = BETACST(h,a,s);
cpar(h,a,s,'mu1')    = mu1(h,a,s) ;

*   ---- non linear PMP model
equation eq_AgrInc_nlp            "non linear Agricultural expected income function"
         eq_cshcnstrnt_NLP        "Cash constraint PMP  parameters"
;

*** --- Eqs without Implicit marginal costs of labour
eq_AgrInc_nlp(h)..        Z(h) =e= sum(j, [sldq(h,j)+ cs(h,j)]*jprice(j)) + sb(h)
                          - sum((a,s)$map_has(h,a,s), ALPHACST(h,a,s)*x(h,a,s)+ 0.5 * BETACST(h,a,s)*x(h,a,s)*x(h,a,s)) - (LabWage*HLAB(h));


eq_cshcnstrnt_NLP(h)..      sum(j, sldq(h,j)*jprice(j))+ sb(h) + exinc(h) + LABEARN(h)
                           =g= sum(j, bght(h,j)*jprice(j)) + sum((a,s)$map_has(h,a,s), ALPHACST(h,a,s)*x(h,a,s)+ 0.5 * BETACST(h,a,s)*x(h,a,s)*x(h,a,s)) + (LabWage*HLAB(h)) ;


model PMPMODEL modelo PMP /
   basesupply
   eq_AgrInc_nlp
   eq_cshcnstrnt_NLP
/;

solve PMPMODEL using NLP maximizing U;

REPORT0(h,a,s,'pmpModel') = x.l(h,a,s);
*REPORT0(h,a,'irr','pmpModel') = sum((a,'irr'), X.L(h,a,'irr'));

REPORT1(h,'income','Household','pmpModel') = R.L(h);
REPORT1(h,'income','Agricultural','pmpModel') = Z.L(h);
REPORT1(h,'Land','Total','pmpModel') = sum((a,s)$map_has(h,a,s), X.L(h,a,s));
REPORT1(h,'Land','Irrigated','pmpModel') = sum((a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT1(h,'Labour','Hired','pmpModel') = HLAB.L(h);
REPORT1(h,'Labour','Family','pmpModel') =  FLAB.L(h);
REPORT1(h,'Labour','HireOut','pmpModel') =  FOUT.L(h);
REPORT1(h,'Labour','Total_Req','pmpModel') = sum((a,s), labr(a,s)*X.L(h,a,s));
REPORT1(h,'Labour','Marginal','pmpModel') = eq_TotLab.M(h);
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

*~~~~~~~~~~~~~~~~~~~~~ WATER VARIABILITY SIMULATION  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*   ---- water variability simulations
set SIM simulations  /SIM1*SIM5/
parameter DW_ori; DW_ori(h)=DW(h)*hd;

loop(sim,
DW(h) = [DW_ori(h)*(1-0.1*ord(sim))]/hd;
solve PMPMODEL using NLP maximizing U;

REPORT0(h,a,s,sim) = x.l(h,a,s);
*REPORT0(h,a,'irr',sim) = sum((a,'irr'), X.L(h,a,'irr'));

REPORT1(h,'income','Household',sim) = R.L(h);
REPORT1(h,'income','Agricultural',sim) = Z.L(h);
REPORT1(h,'Land','Total',sim) = sum((a,s)$map_has(h,a,s), X.L(h,a,s));
REPORT1(h,'Land','Irrigated',sim) = sum((a)$map_has(h,a,'irr'), X.L(h,a,'irr'));
REPORT1(h,'Labour','Hired',sim) = HLAB.L(h);
REPORT1(h,'Labour','Family',sim) =  FLAB.L(h);
REPORT1(h,'Labour','HireOut',sim) =  FOUT.L(h);
REPORT1(h,'Labour','Total_Req',sim) = sum((a,s), labr(a,s)*X.L(h,a,s));
REPORT1(h,'Labour','Marginal',sim) = eq_TotLab.M(h);
REPORT1(h,'Water','Level',sim) =  sum(a, fir(h,a,'irr')*X.L(h,a,'irr'));
REPORT1(h,'Water','Marginal',sim) = eq_waterUse.M(h);
REPORT1(h,'DW','CHANGE',sim) = DW(h)*hd;

REPORT2(h,j,'Production',sim) = prdq.L(h,j);
REPORT2(h,j,'Sold',sim) = sldq.L(h,j);
REPORT2(h,j,'Bought',sim) = bght.L(h,j);
REPORT2(h,j,'Consumption',sim) = cnsq.L(h,j);
REPORT2(h,j,'Self_Cons',sim) = cs.L(h,j);


REPORT3('Weight_ExpIncome','Level',sim) = U.L;
REPORT3('Ttl_Land','',sim)= sum((h,a,s), X.L(h,a,s));
REPORT3('Irr_Land','Level',sim) = sum((h,a)$map_has(h,a,'irr'), X.L(h,a,'irr'));

);


Option  REPORT0:1
        REPORT1:1
        REPORT2:1
        REPORT3:1
;
Display chPMP, chIrrland, chTtlLnd, REPORT0, REPORT1, REPORT2, REPORT3, owage ;

*   ---- create gdx file with model data

execute_unload '..\results\HouseholdChile_Results.gdx' REPORT0 REPORT1 REPORT2 REPORT3   ;
execute 'gdxxrw.exe ..\results\HouseholdChile_Results.gdx o=..\results\HouseholdChile_Results.xlsx par=REPORT0 rng=REPORT0!A1' ;
execute 'gdxxrw.exe ..\results\HouseholdChile_Results.gdx o=..\results\HouseholdChile_Results.xlsx par=REPORT1 rng=REPORT1!A1' ;
execute 'gdxxrw.exe ..\results\HouseholdChile_Results.gdx o=..\results\HouseholdChile_Results.xlsx par=REPORT2 rng=REPORT2!A1' ;
execute 'gdxxrw.exe ..\results\HouseholdChile_Results.gdx o=..\results\HouseholdChile_Results.xlsx par=REPORT3 rng=REPORT3!A1' ;
