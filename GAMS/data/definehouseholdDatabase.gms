*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
$ontext
   Basin Agricultural Model

   Name      :   defineHouseholdDatabase
   Purpose   :   define model database
   Author    :   F Fernández
   Date      :   29.09.15
   Since     :   September 2015
   CalledBy  :

   Notes     :   Import excel data into gdx
                 Build model database

$offtext
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
$onmulti
* Mode database to generate GDX files with base data
* Mode datacheck to check market balances and common data

$setglobal database on
$setglobal datacheck off

*-------------------------------------------------------------------------------
*
*   Common sets and parameters
*
*-------------------------------------------------------------------------------
set
   hou                'households'
   com                'communes'
   act                'activities'
   sys                'production system'
   jf                 'good and factors'
   fct                'factors'
   gds                'goods'
   var                'variables'             /land, yield, price, hrdlab, famlab, hrdLabPr, famLabPr, selas, TotalCosts, vcost, vcost2, InputCosts, HrdLabCost, FamilyLabCost,MachCost    /
   eb                 'ME supports beta'       /e1*e11/
   eg                 'ME supports gamma'      /e1*e5/

;

parameter
   p_householdData   'crop management data Farm level'
   p_houGdsData      'Goods and Factors  Data'
   p_consumptionData 'Consumer prices ($CLP/ton) and Demand elasticities data'
   p_supplyData      'Producer prices ($CLP/ton) and supply elasticities data'


;

$if %database%==on $goto database
$if %datacheck%==on $goto datacheck

$label database
*-------------------------------------------------------------------------------
*
*   Import raw data (from XLS to GDX)
*
*-------------------------------------------------------------------------------
*   ---- auxiliary parameters

parameter
   t_householdData     'Household activities data'
   t_houGdsData        'Household goods and factors data'
   t_selasticities     'supply elasticities'
   t_cnsPrcs           'Consumer prices of agricultural goods'
   t_delas             'demand elasticities'
   t_cir               'Crop Irrigation Requirements mm/h (by AGrimied)'
   za(eb,gds)          'pre-support points for beta parameter of LES'
   zb(eg,gds)          'pre-support points for gamma parameter of LES'
    t_consumption       'Crops-Goods consumed (kg/capita/yr)'
;

*   ---- import sets (from xls to gdx)
$call "gdxxrw.exe ..\data\sets\household_DB_SETS.xlsx o=..\data\sets\sets.gdx se=2 index=indexSet!A3"

$gdxin ..\data\sets\sets.gdx
$load  hou com act sys jf fct gds
$gdxin


*   ---- import data (from xls to gdx)
$call "gdxxrw.exe ..\data\activities\FinalDB_0803.xlsx o=..\data\activities\FinalDB_0803.gdx se=2 index=indexData!A3"
$call "gdxxrw.exe ..\data\supportpoints\supportpoints.xlsx o=..\data\supportpoints\supportpoints.gdx se=2 index=indexData!A3"
$call "gdxxrw.exe ..\data\market\HousModel_supElast.xlsx o=..\data\market\HousModel_supElast.gdx se=2 index=index!A3"
$call "gdxxrw.exe ..\data\market\Consumer_prices.xlsx o=..\data\market\Consumer_prices.gdx se=2 index=index!A3"
$call "gdxxrw.exe ..\data\irrigation\CIR.xlsx o=..\data\irrigation\CIR.gdx se=2 index=indexDat!A3"
$call "gdxxrw.exe ..\data\consumption\Consumption_Data.xlsx o=..\data\consumption\consumption.gdx se=2 index=indexDat!A3"

$gdxin ..\data\activities\FinalDB_0803.gdx
$load  t_householdData t_houGdsData
$gdxin

$gdxin ..\data\supportpoints\supportpoints.gdx
$load    za zb
$gdxin

$gdxin ..\data\market\HousModel_supElast.gdx
$load  t_selasticities
$gdxin

$gdxin ..\data\market\Consumer_prices.gdx
$load  t_cnsPrcs t_delas
$gdxin

$gdxin ..\data\irrigation\CIR.gdx
$load  t_cir
$gdxin

$gdxin ..\data\consumption\consumption.gdx
$load  t_consumption
$gdxin
*-------------------------------------------------------------------------------
*
*   Define model database
*
*-------------------------------------------------------------------------------
*---- total production in  (ton)
p_householdData(hou,com,act,sys,'area') = t_householdData(hou,com,act,sys,'Area');

*---Yield (ton/ha)
*- Raw Yield data does not used the same unit of measurement (e.g. kg, units and qq)
* -- Crops with yield in qqm/ha
p_householdData(hou,com,'mze',sys,'yld')$(p_householdData(hou,com,'mze',sys,'area'))= t_householdData(hou,com,'mze',sys,'Yield')*(1/10);
p_householdData(hou,com,'wht',sys,'yld')$(p_householdData(hou,com,'wht',sys,'area'))= t_householdData(hou,com,'wht',sys,'Yield')*(1/10);
p_householdData(hou,com,'mzes',sys,'yld')$(p_householdData(hou,com,'mzes',sys,'area'))= t_householdData(hou,com,'mzes',sys,'Yield')*(1/10);
p_householdData(hou,com,'snf',sys,'yld')$(p_householdData(hou,com,'snf',sys,'area'))= t_householdData(hou,com,'snf',sys,'Yield')*(1/10);
p_householdData(hou,com,'soy',sys,'yld')$(p_householdData(hou,com,'soy',sys,'area'))= t_householdData(hou,com,'soy',sys,'Yield')*(1/10);
p_householdData(hou,com,'tob',sys,'yld')$(p_householdData(hou,com,'tob',sys,'area'))= t_householdData(hou,com,'tob',sys,'Yield')*(1/10);
p_householdData(hou,com,'cmb',sys,'yld')$(p_householdData(hou,com,'cmb',sys,'area'))= t_householdData(hou,com,'cmb',sys,'Yield')*(1/10);
p_householdData(hou,com,'pot',sys,'yld')$(p_householdData(hou,com,'pot',sys,'area'))= t_householdData(hou,com,'pot',sys,'Yield')*(1/10);
p_householdData(hou,com,'chk',sys,'yld')$(p_householdData(hou,com,'chk',sys,'area'))= t_householdData(hou,com,'chk',sys,'Yield')*(1/10);
p_householdData(hou,com,'ric',sys,'yld')$(p_householdData(hou,com,'ric',sys,'area'))= t_householdData(hou,com,'ric',sys,'Yield')*(1/10);
p_householdData(hou,com,'gbn',sys,'yld')$(p_householdData(hou,com,'gbn',sys,'area'))= t_householdData(hou,com,'gbn',sys,'Yield')*(1/10);
p_householdData(hou,com,'oat',sys,'yld')$(p_householdData(hou,com,'oat',sys,'area'))= t_householdData(hou,com,'oat',sys,'Yield')*(1/10);
p_householdData(hou,com,'pea',sys,'yld')$(p_householdData(hou,com,'pea',sys,'area'))= t_householdData(hou,com,'pea',sys,'Yield')*(1/10);
p_householdData(hou,com,'len',sys,'yld')$(p_householdData(hou,com,'len',sys,'area'))= t_householdData(hou,com,'len',sys,'Yield')*(1/10);

* -- Crops with yield in kg/ha
p_householdData(hou,com,'cbg',sys,'yld')$(p_householdData(hou,com,'cbg',sys,'area'))= t_householdData(hou,com,'cbg',sys,'Yield')*(1/1000);
p_householdData(hou,com,'tom',sys,'yld')$(p_householdData(hou,com,'tom',sys,'area'))= t_householdData(hou,com,'tom',sys,'Yield')*(1/1000);
p_householdData(hou,com,'cucs',sys,'yld')$(p_householdData(hou,com,'cucs',sys,'area'))= t_householdData(hou,com,'cucs',sys,'Yield')*(1/1000);
p_householdData(hou,com,'mels',sys,'yld')$(p_householdData(hou,com,'mels',sys,'area'))= t_householdData(hou,com,'mels',sys,'Yield')*(1/1000);
p_householdData(hou,com,'cbgs',sys,'yld')$(p_householdData(hou,com,'cbgs',sys,'area'))= t_householdData(hou,com,'cbgs',sys,'Yield')*(1/1000);
p_householdData(hou,com,'wtms',sys,'yld')$(p_householdData(hou,com,'wtms',sys,'area'))= t_householdData(hou,com,'wtms',sys,'Yield')*(1/1000);
p_householdData(hou,com,'sgb',sys,'yld')$(p_householdData(hou,com,'sgb',sys,'area'))= t_householdData(hou,com,'sgb',sys,'Yield')*(1/1000);

* -- Crops with yield in un/ha
*We assume the following coefficients to transform for 1 unit to kg
* Melon is assumed that produce 24000 units per ha (ODEPA, 2013) : Ficha Costo Melon. Also is knowing that at national level its yield is between 15 and 30 ton/ha (Corfo, nodo Hortícola, Universidad de Chile)
* Water melon           produce  7500 un/ha (ODEPA, 2013) Also is knowing that at national level its yield is between 16 and 60 ton/ha (Corfo, nodo Hortícola, Universidad de Chile)
* Squash                          2500 un/ha (Recabarren 2011; ODEPA, 2013) and 20 ton/ha (Urriola 2011, Tesis)
p_householdData(hou,com,'mel',sys,'yld')$(p_householdData(hou,com,'mel',sys,'area'))= t_householdData(hou,com,'mel',sys,'Yield')*(0.94/1000);
p_householdData(hou,com,'wtm',sys,'yld')$(p_householdData(hou,com,'wtm',sys,'area'))= t_householdData(hou,com,'wtm',sys,'Yield')*(5.06/1000);
p_householdData(hou,com,'oni',sys,'yld')$(p_householdData(hou,com,'oni',sys,'area'))= t_householdData(hou,com,'oni',sys,'Yield')*(0.3/1000);
p_householdData(hou,com,'sqh',sys,'yld')$(p_householdData(hou,com,'sqh',sys,'area'))= t_householdData(hou,com,'sqh',sys,'Yield')*(6.25/1000);

p_householdData(hou,com,act,sys,'prd')  =
p_householdData(hou,com,act,sys,'yld')*p_householdData(hou,com,act,sys,'area');

p_householdData(hou,com,act,'tot','area') = p_householdData(hou,com,act,'irr','area') + p_householdData(hou,com,act,'dry','area');

p_householdData(hou,com,act,'tot','prd')= p_householdData(hou,com,act,'irr','prd')+ p_householdData(hou,com,act,'dry','prd');


p_householdData(hou,com,act,'tot','yld')$(p_householdData(hou,com,act,'tot','area'))=
p_householdData(hou,com,act,'tot','prd')/p_householdData(hou,com,act,'tot','area');


*-----------------Cost per hectare ($CLP/h) (CLP$ 2011)-----------------
*------------------Even if the household doesnt grown the crop---------
p_householdData(hou,com,act,'irr','InputCosts')= t_householdData(hou,com,act,'irr','InputCosts');
p_householdData(hou,com,act,'dry','InputCosts')= t_householdData(hou,com,act,'dry','InputCosts');

p_householdData(hou,com,act,'irr','HrdLabCost')= t_householdData(hou,com,act,'irr','HrdLabCost');
p_householdData(hou,com,act,'dry','HrdLabCost')= t_householdData(hou,com,act,'dry','HrdLabCost');

p_householdData(hou,com,act,'irr','FamilyLabCost')= t_householdData(hou,com,act,'irr','FamilyLabCost');
p_householdData(hou,com,act,'dry','FamilyLabCost')= t_householdData(hou,com,act,'dry','FamilyLabCost');

p_householdData(hou,com,act,'irr','MachCost')= t_householdData(hou,com,act,'irr','MachCost');
p_householdData(hou,com,act,'dry','MachCost')= t_householdData(hou,com,act,'dry','MachCost');

* --- check vcost
p_householdData(hou,com,act,'irr','vcost2')= p_householdData(hou,com,act,'irr','InputCosts')+ p_householdData(hou,com,act,'irr','HrdLabCost')+ p_householdData(hou,com,act,'irr','FamilyLabCost')+p_householdData(hou,com,act,'irr','MachCost');
p_householdData(hou,com,act,'dry','vcost2')= p_householdData(hou,com,act,'dry','InputCosts')+ p_householdData(hou,com,act,'dry','HrdLabCost')+ p_householdData(hou,com,act,'dry','FamilyLabCost')+p_householdData(hou,com,act,'dry','MachCost');

p_householdData(hou,com,act,'irr','vcost')= t_householdData(hou,com,act,'irr','TotalCosts');
p_householdData(hou,com,act,'dry','vcost')= t_householdData(hou,com,act,'dry','TotalCosts');

parameter t_rawprices 'producer price observed in the survey'  ;
*---- Producer Prices ($CLP/ton)
*- Raw Price data does not used the same unit of measurement (e.g. $CLP/kg, $CLP/units and $CLP/qqm)

* -- Crops with Price in $CLP/qqm
t_rawprices(hou,com,'mze',sys,'prc')$(p_householdData(hou,com,'mze',sys,'area'))= t_householdData(hou,com,'mze',sys,'CropPrice')*(10);
t_rawprices(hou,com,'wht',sys,'prc')$(p_householdData(hou,com,'wht',sys,'area'))= t_householdData(hou,com,'wht',sys,'CropPrice')*(10);
t_rawprices(hou,com,'mzes',sys,'prc')$(p_householdData(hou,com,'mzes',sys,'area'))= t_householdData(hou,com,'mzes',sys,'CropPrice')*(10);
t_rawprices(hou,com,'snf',sys,'prc')$(p_householdData(hou,com,'snf',sys,'area'))= t_householdData(hou,com,'snf',sys,'CropPrice')*(10);
t_rawprices(hou,com,'soy',sys,'prc')$(p_householdData(hou,com,'soy',sys,'area'))= t_householdData(hou,com,'soy',sys,'CropPrice')*(10);
t_rawprices(hou,com,'tob',sys,'prc')$(p_householdData(hou,com,'tob',sys,'area'))= t_householdData(hou,com,'tob',sys,'CropPrice')*(10);
t_rawprices(hou,com,'cmb',sys,'prc')$(p_householdData(hou,com,'cmb',sys,'area'))= t_householdData(hou,com,'cmb',sys,'CropPrice')*(10);
t_rawprices(hou,com,'pot',sys,'prc')$(p_householdData(hou,com,'pot',sys,'area'))= t_householdData(hou,com,'pot',sys,'CropPrice')*(10);
t_rawprices(hou,com,'chk',sys,'prc')$(p_householdData(hou,com,'chk',sys,'area'))= t_householdData(hou,com,'chk',sys,'CropPrice')*(10);
t_rawprices(hou,com,'ric',sys,'prc')$(p_householdData(hou,com,'ric',sys,'area'))= t_householdData(hou,com,'ric',sys,'CropPrice')*(10);
t_rawprices(hou,com,'gbn',sys,'prc')$(p_householdData(hou,com,'gbn',sys,'area'))= t_householdData(hou,com,'gbn',sys,'CropPrice')*(10);
t_rawprices(hou,com,'oat',sys,'prc')$(p_householdData(hou,com,'oat',sys,'area'))= t_householdData(hou,com,'oat',sys,'CropPrice')*(10);
t_rawprices(hou,com,'pea',sys,'prc')$(p_householdData(hou,com,'pea',sys,'area'))= t_householdData(hou,com,'pea',sys,'CropPrice')*(10);
t_rawprices(hou,com,'len',sys,'prc')$(p_householdData(hou,com,'len',sys,'area'))= t_householdData(hou,com,'len',sys,'CropPrice')*(10);

* -- Crops with price in $CLP/kg
t_rawprices(hou,com,'cbg',sys,'prc')$(p_householdData(hou,com,'cbg',sys,'area'))= t_householdData(hou,com,'cbg',sys,'CropPrice')*(1000);
t_rawprices(hou,com,'tom',sys,'prc')$(p_householdData(hou,com,'tom',sys,'area'))= t_householdData(hou,com,'tom',sys,'CropPrice')*(1000);
t_rawprices(hou,com,'cucs',sys,'prc')$(p_householdData(hou,com,'cucs',sys,'area'))= t_householdData(hou,com,'cucs',sys,'CropPrice')*(1000);
t_rawprices(hou,com,'mels',sys,'prc')$(p_householdData(hou,com,'mels',sys,'area'))= t_householdData(hou,com,'mels',sys,'CropPrice')*(1000);
t_rawprices(hou,com,'cbgs',sys,'prc')$(p_householdData(hou,com,'cbgs',sys,'area'))= t_householdData(hou,com,'cbgs',sys,'CropPrice')*(1000);
t_rawprices(hou,com,'wtms',sys,'prc')$(p_householdData(hou,com,'wtms',sys,'area'))= t_householdData(hou,com,'wtms',sys,'CropPrice')*(1000);
t_rawprices(hou,com,'sgb',sys,'prc')$(p_householdData(hou,com,'sgb',sys,'area'))= t_householdData(hou,com,'sgb',sys,'CropPrice')*(1000);

* -- Crops with price in $CLP/un
t_rawprices(hou,com,'mel',sys,'prc')$(p_householdData(hou,com,'mel',sys,'area'))= t_householdData(hou,com,'mel',sys,'CropPrice')*(2500) ;
t_rawprices(hou,com,'wtm',sys,'prc')$(p_householdData(hou,com,'wtm',sys,'area'))= t_householdData(hou,com,'wtm',sys,'CropPrice')*(1430) ;
t_rawprices(hou,com,'oni',sys,'prc')$(p_householdData(hou,com,'oni',sys,'area'))= t_householdData(hou,com,'oni',sys,'CropPrice')*(3333);
t_rawprices(hou,com,'sqh',sys,'prc')$(p_householdData(hou,com,'sqh',sys,'area'))= t_householdData(hou,com,'sqh',sys,'CropPrice')*(833);

*--------------labor demand: workers/ha----------
p_householdData(hou,com, act,sys,'hrd_lab')= t_householdData(hou,com,act,sys,'HiredLabor');
p_householdData(hou,com,act,sys,'fam_lab') = t_householdData(hou,com,act,sys,'FamilyLab');
p_householdData(hou,com,act,sys,'tot_lab') = p_householdData(hou,com, act,sys,'hrd_lab') + p_householdData(hou,com,act,sys,'fam_lab');

*--------------Hired labor average cost-----------------------------------------
p_householdData(hou,com, act,sys,'HLab_Price') = t_householdData(hou,com,act,sys,'PriceHrdLab');
p_householdData(hou,com, act,sys,'FLab_Price') = t_householdData(hou,com,act,sys,'PriceFamLab');

p_supplyData(act,'prd_prc')$(sum((hou,com,sys), 1$t_rawprices(hou,com,act,sys,'prc')) gt 0) = sum((hou,com,sys), t_rawprices(hou,com,act,sys,'prc'))/sum((hou,com,sys), 1$t_rawprices(hou,com,act,sys,'prc'));
p_supplyData(act,'selast') =  t_selasticities(act,'elas');

*-----------------Revenue ($CLP/h) ($CLP 2011)------------------
p_householdData(hou,com,act,'irr','srev')$p_householdData(hou,com,act,'irr','yld')= p_supplyData(act,'prd_prc')*p_householdData(hou,com,act,'irr','yld');
p_householdData(hou,com,act,'dry','srev')$p_householdData(hou,com,act,'dry','yld')= p_supplyData(act,'prd_prc')*p_householdData(hou,com,act,'dry','yld');


*----------------Gross Margin ($CLP/h) ($CLP 2011)----------------
*------Household---
p_householdData(hou,com,act,sys,'gmar')$p_householdData(hou,com,act,sys,'yld')
                                                                         = p_householdData(hou,com,act,sys,'srev') - p_householdData(hou,com,act,sys,'vcost')    ;


*----------------Crop Irrigation requirements at the Base Line(th m3/h/yr)---------
*--------Original data in mm/m2/yr
*----For those crops without CIR information the associated figure is:
*tomato and onions = maize
*melon = 1.2 * CIR_maize
*water melon = 1.3 * CIR_maize

*------Comune level----
p_householdData(hou,com,act,'irr','cir')= (1/100)*t_cir(com,act,'BL');
p_householdData(hou,com,'chk','irr','cir')=p_householdData(hou,com,'cmb','irr','cir');
p_householdData(hou,com,'sqh','irr','cir')=p_householdData(hou,com,'mel','irr','cir')*0.8;



*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*                        Goods and Factors Data
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*---- Quantity of goods produced (ton)
*---- total production in  (ton)
p_houGdsData(hou,com,gds,sys,'area') = t_houGdsData(hou,com,gds,sys,'Area');

*---Yield (ton/ha)
*- Raw Yield data does not used the same unit of measurement (e.g. kg, units and qq)
* -- Crops with yield in qqm/ha
p_houGdsData(hou,com,'mze',sys,'yld')$(p_houGdsData(hou,com,'mze',sys,'area'))= t_houGdsData(hou,com,'mze',sys,'Yield')*(1/10);
p_houGdsData(hou,com,'wht',sys,'yld')$(p_houGdsData(hou,com,'wht',sys,'area'))= t_houGdsData(hou,com,'wht',sys,'Yield')*(1/10);
p_houGdsData(hou,com,'mzes',sys,'yld')$(p_houGdsData(hou,com,'mzes',sys,'area'))= t_houGdsData(hou,com,'mzes',sys,'Yield')*(1/10);
p_houGdsData(hou,com,'snf',sys,'yld')$(p_houGdsData(hou,com,'snf',sys,'area'))= t_houGdsData(hou,com,'snf',sys,'Yield')*(1/10);
p_houGdsData(hou,com,'soy',sys,'yld')$(p_houGdsData(hou,com,'soy',sys,'area'))= t_houGdsData(hou,com,'soy',sys,'Yield')*(1/10);
p_houGdsData(hou,com,'tob',sys,'yld')$(p_houGdsData(hou,com,'tob',sys,'area'))= t_houGdsData(hou,com,'tob',sys,'Yield')*(1/10);
p_houGdsData(hou,com,'cmb',sys,'yld')$(p_houGdsData(hou,com,'cmb',sys,'area'))= t_houGdsData(hou,com,'cmb',sys,'Yield')*(1/10);
p_houGdsData(hou,com,'pot',sys,'yld')$(p_houGdsData(hou,com,'pot',sys,'area'))= t_houGdsData(hou,com,'pot',sys,'Yield')*(1/10);
p_houGdsData(hou,com,'chk',sys,'yld')$(p_houGdsData(hou,com,'chk',sys,'area'))= t_houGdsData(hou,com,'chk',sys,'Yield')*(1/10);
p_houGdsData(hou,com,'ric',sys,'yld')$(p_houGdsData(hou,com,'ric',sys,'area'))= t_houGdsData(hou,com,'ric',sys,'Yield')*(1/10);
p_houGdsData(hou,com,'gbn',sys,'yld')$(p_houGdsData(hou,com,'gbn',sys,'area'))= t_houGdsData(hou,com,'gbn',sys,'Yield')*(1/10);
p_houGdsData(hou,com,'oat',sys,'yld')$(p_houGdsData(hou,com,'oat',sys,'area'))= t_houGdsData(hou,com,'oat',sys,'Yield')*(1/10);
p_houGdsData(hou,com,'pea',sys,'yld')$(p_houGdsData(hou,com,'pea',sys,'area'))= t_houGdsData(hou,com,'pea',sys,'Yield')*(1/10);
p_houGdsData(hou,com,'len',sys,'yld')$(p_houGdsData(hou,com,'len',sys,'area'))= t_houGdsData(hou,com,'len',sys,'Yield')*(1/10);

* -- Crops with yield in kg/ha
p_houGdsData(hou,com,'cbg',sys,'yld')$(p_houGdsData(hou,com,'cbg',sys,'area'))= t_houGdsData(hou,com,'cbg',sys,'Yield')*(1/1000);
p_houGdsData(hou,com,'tom',sys,'yld')$(p_houGdsData(hou,com,'tom',sys,'area'))= t_houGdsData(hou,com,'tom',sys,'Yield')*(1/1000);
p_houGdsData(hou,com,'cucs',sys,'yld')$(p_houGdsData(hou,com,'cucs',sys,'area'))= t_houGdsData(hou,com,'cucs',sys,'Yield')*(1/1000);
p_houGdsData(hou,com,'mels',sys,'yld')$(p_houGdsData(hou,com,'mels',sys,'area'))= t_houGdsData(hou,com,'mels',sys,'Yield')*(1/1000);
p_houGdsData(hou,com,'cbgs',sys,'yld')$(p_houGdsData(hou,com,'cbgs',sys,'area'))= t_houGdsData(hou,com,'cbgs',sys,'Yield')*(1/1000);
p_houGdsData(hou,com,'wtms',sys,'yld')$(p_houGdsData(hou,com,'wtms',sys,'area'))= t_houGdsData(hou,com,'wtms',sys,'Yield')*(1/1000);
p_houGdsData(hou,com,'sgb',sys,'yld')$(p_houGdsData(hou,com,'sgb',sys,'area'))= t_houGdsData(hou,com,'sgb',sys,'Yield')*(1/1000);

* -- Crops with yield in un/ha
*We assume the following coefficients to transform for 1 unit to kg
p_houGdsData(hou,com,'mel',sys,'yld')$(p_houGdsData(hou,com,'mel',sys,'area'))= t_houGdsData(hou,com,'mel',sys,'Yield')*(0.4/1000);
p_houGdsData(hou,com,'wtm',sys,'yld')$(p_houGdsData(hou,com,'wtm',sys,'area'))= t_houGdsData(hou,com,'wtm',sys,'Yield')*(0.7/1000);
p_houGdsData(hou,com,'oni',sys,'yld')$(p_houGdsData(hou,com,'oni',sys,'area'))= t_houGdsData(hou,com,'oni',sys,'Yield')*(0.3/1000);
p_houGdsData(hou,com,'sqh',sys,'yld')$(p_houGdsData(hou,com,'sqh',sys,'area'))= t_houGdsData(hou,com,'sqh',sys,'Yield')*(1.2/1000);

p_houGdsData(hou,com,gds,sys,'prd')  =
p_houGdsData(hou,com,gds,sys,'yld')*p_houGdsData(hou,com,gds,sys,'area');

p_houGdsData(hou,com,gds,'tot','area') = p_houGdsData(hou,com,gds,'irr','area') + p_houGdsData(hou,com,gds,'dry','area');

p_houGdsData(hou,com,gds,'tot','prd')= p_houGdsData(hou,com,gds,'irr','prd')+ p_houGdsData(hou,com,gds,'dry','prd');


p_houGdsData(hou,com,gds,'tot','yld')$(p_houGdsData(hou,com,gds,'tot','area'))=
p_houGdsData(hou,com,gds,'tot','prd')/p_houGdsData(hou,com,gds,'tot','area');



*---- Consumption of goods (ton)
*- Raw Consumption data does not used the same unit of measurement (e.g. kg, units and qq)
* -- Crops with Consumption in qqm
p_houGdsData(hou,com,'mze',sys,'cons')$(p_houGdsData(hou,com,'mze',sys,'area'))= t_consumption('mze','cons') *(1/10);
p_houGdsData(hou,com,'wht',sys,'cons')$(p_houGdsData(hou,com,'wht',sys,'area'))= t_consumption('wht','cons')*(1/10);
p_houGdsData(hou,com,'mzes',sys,'cons')$(p_houGdsData(hou,com,'mzes',sys,'area'))= t_consumption('mzes','cons')*(1/10);
p_houGdsData(hou,com,'snf',sys,'cons')$(p_houGdsData(hou,com,'snf',sys,'area'))= t_consumption('snf','cons')*(1/10);
p_houGdsData(hou,com,'soy',sys,'cons')$(p_houGdsData(hou,com,'soy',sys,'area'))= t_consumption('soy','cons')*(1/10);
p_houGdsData(hou,com,'tob',sys,'cons')$(p_houGdsData(hou,com,'tob',sys,'area'))= t_consumption('tob','cons')*(1/10);
p_houGdsData(hou,com,'cmb',sys,'cons')$(p_houGdsData(hou,com,'cmb',sys,'area'))= t_consumption('cmb','cons')*(1/10);
p_houGdsData(hou,com,'pot',sys,'cons')$(p_houGdsData(hou,com,'pot',sys,'area'))= t_consumption('pot','cons')*(1/10);
p_houGdsData(hou,com,'chk',sys,'cons')$(p_houGdsData(hou,com,'chk',sys,'area'))= t_consumption('chk','cons')*(1/10);
p_houGdsData(hou,com,'ric',sys,'cons')$(p_houGdsData(hou,com,'ric',sys,'area'))= t_consumption('ric','cons')*(1/10);
p_houGdsData(hou,com,'gbn',sys,'cons')$(p_houGdsData(hou,com,'gbn',sys,'area'))= t_consumption('gbn','cons')*(1/10);
p_houGdsData(hou,com,'oat',sys,'cons')$(p_houGdsData(hou,com,'oat',sys,'area'))= t_consumption('oat','cons')*(1/10);
p_houGdsData(hou,com,'pea',sys,'cons')$(p_houGdsData(hou,com,'pea',sys,'area'))= t_consumption('pea','cons')*(1/10);
p_houGdsData(hou,com,'len',sys,'cons')$(p_houGdsData(hou,com,'len',sys,'area'))= t_consumption('len','cons')*(1/10);

* -- Crops with Consumption in kg
p_houGdsData(hou,com,'cbg',sys,'cons')$(p_houGdsData(hou,com,'cbg',sys,'area'))= t_consumption('cbg','cons')*(1/1000);
p_houGdsData(hou,com,'tom',sys,'cons')$(p_houGdsData(hou,com,'tom',sys,'area'))= t_consumption('tom','cons')*(1/1000);
p_houGdsData(hou,com,'cucs',sys,'cons')$(p_houGdsData(hou,com,'cucs',sys,'area'))= t_consumption('cucs','cons')*(1/1000);
p_houGdsData(hou,com,'mels',sys,'cons')$(p_houGdsData(hou,com,'mels',sys,'area'))= t_consumption('mels','cons')*(1/1000);
p_houGdsData(hou,com,'cbgs',sys,'cons')$(p_houGdsData(hou,com,'cbgs',sys,'area'))= t_consumption('cbgs','cons')*(1/1000);
p_houGdsData(hou,com,'wtms',sys,'cons')$(p_houGdsData(hou,com,'wtms',sys,'area'))= t_consumption('wtms','cons')*(1/1000);
p_houGdsData(hou,com,'sgb',sys,'cons')$(p_houGdsData(hou,com,'sgb',sys,'area'))= t_consumption('sgb','cons')*(1/1000);

* -- Crops with Consumption in un/ha
*We assume the following coefficients to transform for 1 unit to kg
p_houGdsData(hou,com,'mel',sys,'cons')$(p_houGdsData(hou,com,'mel',sys,'area'))= t_consumption('mel','cons')*(0.4/1000);
p_houGdsData(hou,com,'wtm',sys,'cons')$(p_houGdsData(hou,com,'wtm',sys,'area'))= t_consumption('wtm','cons')*(0.7/1000);
p_houGdsData(hou,com,'oni',sys,'cons')$(p_houGdsData(hou,com,'oni',sys,'area'))= t_consumption('oni','cons')*(0.3/1000);
p_houGdsData(hou,com,'sqh',sys,'cons')$(p_houGdsData(hou,com,'sqh',sys,'area'))= t_consumption('sqh','cons')*(1.2/1000);

**--- Consumption (ton)
*p_houGdsData(hou,com,gds,sys,'percapCons') = t_consumption(gds,'cons');
*p_houGdsData(hou,com,gds,sys,'hhldCons') = [p_houGdsData(hou,com,gds,sys,'percapCons')*p_houGdsData(hou,com,gds,sys,'hhsize')]/1000;


parameter t_rawConsumerprices 'Consumer prices ODEPA year 2011'  ;
*---- Consumer Prices ($CLP/ton)
*- Raw Price data does not used the same unit of measurement (e.g. $CLP/kg, $CLP/units and $CLP/qqm)

* -- Agricultural goods with consumer price in $CLP/kg
t_rawConsumerprices('ric','prc')= t_cnsPrcs('ric','price')*(1000);
t_rawConsumerprices('pot','prc')= t_cnsPrcs('pot','price')*(1000);
t_rawConsumerprices('cmb','prc')= t_cnsPrcs('cmb','price')*(1000);
t_rawConsumerprices('gbn','prc')= t_cnsPrcs('gbn','price')*(1000);
t_rawConsumerprices('tom','prc')= t_cnsPrcs('tom','price')*(1000);
t_rawConsumerprices('sqh','prc')= t_cnsPrcs('sqh','price')*(1000);
t_rawConsumerprices('chk','prc')= t_cnsPrcs('chk','price')*(1000);
t_rawConsumerprices('len','prc')= t_cnsPrcs('len','price')*(1000);
t_rawConsumerprices('oat','prc')= t_cnsPrcs('oat','price')*(1000);
t_rawConsumerprices('wht','prc')= t_cnsPrcs('wht','price')*(1000);
t_rawConsumerprices('oni','prc')= t_cnsPrcs('oni','price')*(1000);
t_rawConsumerprices('mze','prc')= t_cnsPrcs('mze','price')*(1000);

* -- Agricultural goods with consumer price in $CLP/unidad
*For melon we assume that 1 unit weigth 1.5 kg
t_rawConsumerprices('mel','prc')= t_cnsPrcs('mel','price')*(1/1.5)*(1/0.001);

*For water melon we assume that 1 unit weigth 4 kg
t_rawConsumerprices('wtm','prc')= t_cnsPrcs('wtm','price')*(1/4)*(1/0.001);

*For cucumber and cabages we assume weigths of 0.3 kg and 0.5 kg
t_rawConsumerprices('cuc','prc')= t_cnsPrcs('cuc','price')*(300);
t_rawConsumerprices('cbg','prc')= t_cnsPrcs('cbg','price')*(500);

p_consumptionData(gds,'prc')= t_rawConsumerprices(gds,'prc');

p_consumptionData(gds,'delast')= t_delas(gds,'delas');



*   ---- create gdx file with model data

execute_unload '..\results\HouseholdChile_db_0803.gdx' p_householdData za zb p_houGdsData p_consumptionData p_supplyData  ;
execute 'gdxxrw.exe ..\results\HouseholdChile_db_0803.gdx o=..\results\HouseholdChile_db_0803.xlsx par=p_householdData rng=householdData!A1' ;
execute 'gdxxrw.exe ..\results\HouseholdChile_db_0803.gdx o=..\results\HouseholdChile_db_0803.xlsx par=p_houGdsData rng=houGdsData!A1' ;
execute 'gdxxrw.exe ..\results\HouseholdChile_db_0803.gdx o=..\results\HouseholdChile_db_0803.xlsx par=za rng=za!A1' ;
execute 'gdxxrw.exe ..\results\HouseholdChile_db_0803.gdx o=..\results\HouseholdChile_db_0803.xlsx par=zb rng=zb!A1' ;
execute 'gdxxrw.exe ..\results\HouseholdChile_db_0803.gdx o=..\results\HouseholdChile_db_0803.xlsx par=p_consumptionData rng=p_consumptionData!A1' ;
execute 'gdxxrw.exe ..\results\HouseholdChile_db_0803.gdx o=..\results\HouseholdChile_db_0803.xlsx par=p_supplyData rng=p_supplyData!A1' ;


option DECIMALS=1;
display p_householdData, p_houGdsData, p_consumptionData, p_supplyData;
$exit



