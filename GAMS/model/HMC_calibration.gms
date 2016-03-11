*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
$ontext
   Householdmodel Chile

   Name      :   household_pmpCalibration.gms
   Purpose   :   model Household Chile (PMP)
   Author    :   F Fernández
   Date      :   18/11/2015
   Since     :   September 2015
   CalledBy  :

   Notes     :

$offtext
$onmulti;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*                         INCLUDE SETS AND BASE DATA                           *
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

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*                         PMP CALIBRATION -
*           SOLVE Household MODEL WITH CALIBRATION CONSTRAINTS                                            *
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

*~~~~~~~~~~~~~~~~~~~~~~~~ BASE LP MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

$include HMC_coreModel.gms
;

* consider only potential activities
X.fx(h,a,s)$(not map_has(h,a,s)) = 0;

* bounds on variables
X.up(h,a,s)$map_has(h,a,s) = tland;
*FOUT.up(h) = 0;
*prdq.up(h,c,j) = sum((a,s),yl(h,c,a,s,j)*x0(h,c,a,s));
*cs.up(h,c,j)= jcons_com(h,c,j);
*cnsq.up(h,c,j)= sum((a,s),yl(h,c,a,s,j)*x0(h,c,a,s));
*HLAB.up(h,c) = sum((a,s), labrnt(h,c,a,s)) ;
*FLAB.fx(h) = avFamLab(h)   ;
*HLAB.l(h) = avHrdLab(h) ;

model basemodel modelo lineal base /
   household_noRisk
   eq_AgrInc_LP
*   eq_cshcnstrnt_LP
/;

solve basemodel using NLP maximizing U;

*   ---- results parameter
parameter REPORT0 'Activity report by household'
          REPORT1 'Income and Resources report by household'
          REPORT2 'Production and Consumption Report by household'
          REPORT3 'Overall Report'

;

REPORT0(h,a,s,'baseLP') = X.L(h,a,s);

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
REPORT3('Ttl_Land','','baseLP')= tland;
REPORT3('Irr_Land','Level','baseLP') = IL.L;



*~~~~~~~~~~~~~~~~~~~~~~~~ CALIBRATION CONSTRAINTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

*   ---- adding calibration constraints
eps1=0.000001;
eps2=0.0000001;
eps3=0.000001;

equation
   calib1    calibration constraints (activity)
   calib2    calibration constraint  (production)
   calib3    calibration constraint  (labour)
;

CALIB1(h,a,s)..  x(h,a,s)$map_has(h,a,s) =l= x0(h,a,s)*(1+eps1);
*CALIB2(h,j)..    prdq(h,j)$map_hj(h,j)   =l= prd0(h,j)*(1+eps1);
*CALIB3(h)..      HLAB(h)                 =l= hlab0(h) *(1+eps3);

Model calibHousehold calibration model MB /
   household_noRisk
   eq_AgrInc_LP
   CALIB1
*   calib2
*   calib3
/;

*   ---- solving model with calibration constraints
solve calibHousehold using NLP maximizing U;

REPORT0(h,a,s,'pmpCalib') = X.L(h,a,s);

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
REPORT3('Ttl_Land','','pmpCalib')= tland;
REPORT3('Irr_Land','Level','pmpCalib') = IL.L;


parameter chPMP, cpar, lpar;

chPMP(h,a,s,'X0')  = x0(h,a,s);
chPMP(h,a,s,'pmpCalib') = x.l(h,a,s);
chPMP(h,a,s,'diff') = chPMP(h,a,s,'X0') - chPMP(h,a,s,'pmpCalib');


*~~~~~~~~~~~~~~~~~~~ PMP MODEL (cost approach + supply elasticities) ~~~~~~~~~*
*   ---- cost function parameters
* Average cost function  AC = alpha+0.5*beta*X
* Standard version alpha=c, beta=mu/x0
* Exogenous supply elasticities
* No marginal activities

Parameter ALPHACST  "marginal cost intercept"
          BETACST   "marginal cost slope"
          GAMMACOST "Implicit marginal costs of labour"
          mu1       "dual values from calibration constraints"
          mu2       "dual values from calibration constraints (production)"
          mu3       "dual values from calibration constraints (labour)"
          cpar      "cost function parameters"
;


mu1(h,a,s)$map_has(h,a,s)  = CALIB1.M(h,a,s);
*mu2(h,j)$map_hj(h,j)   = CALIB2.M(h,j);
*mu3(h)                 = CALIB3.M(h);

BETACST(h,a,s)$map_has(h,a,s) = (1/selas(a)) ;
ALPHACST(h,a,s)$map_has(h,a,s)= (1/(1+BETACST(h,a,s)))*(acst(h,a,s)+mu1(h,a,s))*x0(h,a,s)**(-BETACST(h,a,s));
*GAMMACOST(h)                  = mu3(h);

cpar(h,a,s,'alpha')$map_has(h,a,s)  = ALPHACST(h,a,s);
cpar(h,a,s,'beta')$map_has(h,a,s)   = BETACST(h,a,s);
cpar(h,a,s,'mu1')$map_has(h,a,s)    = mu1(h,a,s) ;
*cpar(h,*,*,'mu3')$map_has(h,a,s)    = mu3(h,a,s) ;
*

*   ---- non linear PMP model
equation eq_AgrInc_nlp            "non linear Agricultural expected income function"
         eq_cshcnstrnt_NLP        "Cash constraint PMP lambda parameter"

;

* only current activities are taken into account
*** --- Eqs Taking into account Implicit marginal costs of labour
*eq_AgrInc_nlp(h)..        Z(h) =e= sum(j, [sldq(h,j)+ cs(h,j)]*prcg(h,j)) + sb(h)
*                          - sum((a,s)$map_has(h,a,s), ALPHACST(h,a,s)*x(h,a,s)+ 0.5 * BETACST(h,a,s)*x(h,a,s)*x(h,a,s))- (LabWage*(HLAB(h) + GAMMACOST(h))) ;

*eq_cshcnstrnt_NLP(h)..      sum(j, sldq(h,j)*prcg(h,j))+ sb(h) + exinc(h)
*                         =g= sum(j, bght(h,j)*prcg(h,j)) + sum((a,s)$map_has(h,a,s), ALPHACST(h,a,s)*x(h,a,s)+ 0.5 * BETACST(h,a,s)*x(h,a,s)*x(h,a,s)) + (LabWage*(HLAB(h) + GAMMACOST(h))) ;


*** --- Eqs without Implicit marginal costs of labour
eq_AgrInc_nlp(h)..        Z(h) =e= sum(j, [sldq(h,j)+ cs(h,j)]*prcg(h,j)) + sb(h)
                          - sum((a,s)$map_has(h,a,s), ALPHACST(h,a,s)*x(h,a,s)+ 0.5 * BETACST(h,a,s)*x(h,a,s)*x(h,a,s))- (LabWage*HLAB(h)) ;

eq_cshcnstrnt_NLP(h)..      sum(j, sldq(h,j)*prcg(h,j))+ sb(h) + exinc(h)
                           =g= sum(j, bght(h,j)*prcg(h,j)) + sum((a,s)$map_has(h,a,s), ALPHACST(h,a,s)*x(h,a,s)+ 0.5 * BETACST(h,a,s)*x(h,a,s)*x(h,a,s)) + (LabWage*HLAB(h)) ;



model PMPMODEL modelo PMP /
   household_noRisk
   eq_AgrInc_nlp
   eq_cshcnstrnt_NLP
/;

* valores iniciales
X.l(h,a,s)=X0(h,a,s);

solve PMPMODEL using NLP maximizing U;

REPORT0(h,a,s,'pmpModel') = X.L(h,a,s);
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
REPORT3('Ttl_Land','','pmpModel')= tland;
REPORT3('Irr_Land','Level','pmpModel') = IL.L;


Display cpar, chPMP, REPORT0, REPORT1, REPORT2, REPORT3 ;
$exit


execute_unload 'basedata\cpar.gdx' cpar;
display chPMP, cpar, REPORT0, REPORT1, REPORT2, REPORT3;


