Libname Linier "Y:\";

Proc import datafile="Z:\Assignments\Practice (Non Graded) Assignment\Topic 9 - Linear Regression Models\Cars_Retail_Price.csv"
out= Linier.first
DBMS= csv replace;
GETNAMES= YES;
run;

proc contents data= Linier.first;
run;

data car;
set Linier.first;

if Make in ("Buick") then buick_dummy=1;
else buick_dummy=0;

if Make in ("Cadil") then cadil_dummy=1;
else cadil_dummy=0;

if Make in ("Chevr") then chevr_dummy=1;
else chevr_dummy=0;

if Make= "Ponti" then ponti_dummy=1;
else ponti_dummy=0;

if Make= "SAAB" then saab_dummy=1;
else saab_dummy=0;

if Make= "Satur" then satur_dummy=1;
else satur_dummy=0;

if Model= "Century" then century_dummy=1;
else century_dummy=0;

if Model= "Lacrossc" then lacrossc_dummy=1;
else lacrosse_dummy=0;

if Model= "Lesabre" then lesabre_dummy=1;
else lesabre_dummy=0;

if Model= "Park Avn" then park_dummy=1;
else park_dummy=0;

if Model= "CST-V" then cstv_dummy=1;
else cstv_dummy=0;

if Model="CTS" then cts_dummy=1;
else cts_dummy=0;

if Model= "Deville" then deville_dummy=1;
else deville_dummy=0;

if Model= "STS-V6" then sts_dummy=1;
else sts_dummy=0;

if Model= "XLR-V8" then XLR_dummy=1;
else XLR_dummy=0;

if Model="AVEO" then aveo_dummy=1;
else aveo_dummy=0;

if Model= "Cavalier" then cavalier_dummy=1;
else cavalier_dummy=0;

if Model="Classic" then Classic_dummy=1;
else Classic_dummy=0;

if Model="Cobalt" then Cobalt_dummy=1;
else Cobalt_dummy=0;

if Model="Corvette" then Corvette_dummy=1;
else Corvette_dummy=0;

if Model= "Impala" then Impala_dummy=1;
else Impala_dummy=0;

if Model="Malibu" then Malibu_dummy=1;
else Malibu_dummy=0;

if Model="Monte Car" then Monte_dummy=1;
else Monte_dummy=0;

if Model="Bonnevill" then Bonnevill_dummy=1;
else Bonnevill_dummy=0;

if Mosdel="G6" then G6_dummy=1;
else G6_dummy=0;

if Model="Grand Am" then Grand_dummy=1;
else Grand_dummy=0;

if Model="Grand Pri" then Grand_dummy=1;
else Grand_dummy=0;

if Model="GTO" then GTO_dummy=1;
else GTO_dummy=0;

if Model="Sunfire" then Sunfire_dummy=1;
else Sunfire_dummy=0;

if Model="Vibe" then vibe_dummy=1;
else vibe_dummy=0;

if Model="9_3" then nine_three_dummy=1;
else nine_three_dummy=0;

if Model="9_3 HO" then nine_ho_dummy=1;
else nine_ho_dummy=0;

if Model="9_5" then nine_five_dummy=1;
nine_five_dummy=0;

if Model="9_5 HO" then nine_five_ho=1;
else nine_five_ho=0;

if Model="9-2X AWD" then nine_x_dummy=1;
else nine_x_dummy=0;

if Model="Ion" then ion_dummy=1;
else ion_dummy=0;

if Model="L Series" then L_series_dummy=1;
else L_series_dummy=0;

if Type="Sedan" then sedan_dummy=1;
else sedan_dummy=0;

if Type="Conve" then conve_dummy=1;
else conve_dummy=0;

if Type="Hatch" then hatch_dummy=1;
else hatch_dummy=0;

if Type="Coupe" then coupe=1;
else coupe=0;

if Type="Wagon" then wagon_dummy=1;
else wagon_dummy=0;

if cruise=1 AND Leather=1 AND Sound=1 then Prem_dummy=1;
else Prem_dummy=0;

run;

proc means data=car;
class Make Type;
var Price;
run;

Proc means data=car;
class Cruise Sound Leather;
var Price;
run;

Proc plot data=car;
plot Price*Mileage;
run;

data car1;
set car;
if Price>52000 then Delete;
run;


Proc plot data=car1;
plot Price*Mileage;
run;

proc Plot data=car1;
Plot Price*Make;
run;

Proc reg data=car;
Model Price= Mileage buick_dummy  cadil_dummy chevr_dummy  ponti_dummy saab_dummy
sedan_dummy coupe hatch_dummy wagon_dummy   Cylinder liter  Cruise Sound Leather;
run;

Proc reg data=car;
Model Price= Mileage buick_dummy  cadil_dummy  saab_dummy
Cruise Sound Leather prem_dummy sedan_dummy coupe hatch_dummy wagon_dummy   Cylinder liter;
output out= resid_cars p = pred r = resid;
run;

data resid_cars1;
set resid_cars;
obs+1;
run;

proc plot data=resid_cars;
Plot pred*resid;
run; 

Proc reg data=car;
Model Price= Mileage buick_dummy  cadil_dummy  saab_dummy
Cruise Sound Leather prem_dummy sedan_dummy coupe hatch_dummy wagon_dummy   Cylinder /vif;
output out= resid_cars p = pred r = resid;
run;

goption ftext= 'Arial' htext=1.5 gunit=pct ctext=green;

symbol1 v=dot i=join c=blue h=0.1;
symbol2 v=dot i=join c=red h=0.1;

proc gplot data=resid_cars1;
plot (pred price) * obs/overlay vaxis=axis1 haxis=axis2  caxis=blue;
run;