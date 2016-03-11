*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
$ontext
   Household Model Chile, Maule region

   Name      :   parshouseholdChile.gms
   Purpose   :   Core model parameters
   Author    :   F Fernández
   Date      :   30.09.15
   Since     :   September 2015
   CalledBy  :   run1_calPMP

   Notes     :   Declaration of core model parameters

$offtext
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*                         PARAMETERS DECLARATION                               *
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

* ---- DEFINING PARAMETERS FOR GME - LES function

Parameter
*--- BETA-LES Parameter
ielas        income elasticity for agricultural food products
jcons_com    Goods consumption (ton) per commune
jcons        Goods consumed (ton) per household
jprice       Consumer prices (millions of $CLP per ton)
aprice
consval      Consumption value million $CLP
exinc        Exogenous off-farm incomes for households (millions of $CLP)
sb           Subsidies (millions of $CLP)
Y_0_com      Initial Farm household full income per commune (millions of $CLP)
Y_0          Initial Farm household full Income (millions of $CLP)
bdgtshr      budget share by household-commune and good
avs          average budget share per good per commune
za           pre-support points for beta
Z1           Values of support points for beta

*--- GAMMA-LES Parameter
avg_hougamma average Gamma parameter by household -  average uncompressible consumption
avg_comgamma average Gamma parameter by commune -  average uncompressible consumption
zb           pre-support points for gamma
Z2           Values of support points for gamma

*--- MHU-LES Parameter (Error term)
Kst          Number of supports for mhu LES
avgc         average consumption of good by commune
std_les      standar deviation of consumption and expenditure in good j
Z3           Values of support points for mhu


**--- Core Model Data
tland        Total land availability                                   (ha)
thland       Total land of household f per commune                     (ha)
icland       irrigable land (ha) commune                               (ha)
w            Farm household weight within the commune                  (%)
labrnt       Hired labour                                           (working days)
labfam       Family labour                                          (working days)
Am           Input coefficients (input use of factor f in activity a)
B            Initial resource endowment
yl           economic output coefficient (yield of activiti a)
tb           Multiplicative transaction costs of goods (buyer)
ts           Multiplicative transaction costs of goods (seller)
acst_com     Accounting costs per household per commune
acst         Accounting costs per household
grmrg0       Gross margin observed per household per activity
avgLab       Average labour per activity and system
avFamLab     average family labour available per household and commune
LabWage      Hired labour wage (million $CLP per day)
owage        Hire-out  wage rate (millions $CLP per day)
labr         Average Labour requiremet per activity per system
avHrdLab     Average hired labour per household


*   ---- base year data commune------
   yld          "crop yield (tons/h)"

   x0           "crop area (2011) in ha household-commune level"

   w0           "water delivery (2011) in m3 commune level"

   labreq       "Total labour requirements (working days) "

   pprice       "Producer prices (millions $CLP/ton) (2011)"

   selas        "supply elasticity"

   totLab       "Total labour per commune (working days)"

   DW           'Gross Water Delivered'

   prd0         'Production observed(2011) (ton) per household'

   hlab0        'Hired labour observed in 2011 per household'

   flab0        'observed family labour per household'

* model data for baseline
   p_householdData(*,*,*,*,*)   'crop management data household level'
   p_houGdsData(*,*,*,*,*)      'Goods and Factors household data'
   p_consumptionData(*,*)       'Consumption data'
   p_supplyData(*,*)            'Supply data'


*   ---- LES function parameters
   beta        "marginal budget share"
   gamma       "minimum subsistence"
   lespar      "LES parameters"

* crop water requirements and irrigation efficiency
   nir          "net irrigation requirements"
   fir_com      "farm-gate (field) irrigation requirements per commune"
   fir          "farm-gate (field) irrigation requirements"
   gir          "gros irrigation requirements"
   Hd           "conveyance and distribution efficiency of the water network"
   Ha           "water application efficiency"


*   ---- cost function parameters commune
   BETACST     "implicit cost function's Beta parameter"
   ALPHACST    "implicit cost function's Alpha parameter"
   GAMMACST    "implicit cost function's Gamma parameter"
   cpar        "calibration parameters"
   lpar        "labour parameter"
   chPMP       "PMP check parameter - After calibration constraint"
  ;





