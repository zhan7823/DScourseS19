create table insurance (policyID, statecode, county, eq_site_limit, hu_site_limit, fl_site_limit, fr_site_limit, tiv_2011, tiv_2012, eq_site_deductible, hu_site_deductible, fl_site_deductible, fr_site_deductible, point_latit_latitude, point_longitude, line, construction, point_granularity);
.mode csv
.import FL_insurance_sample.csv insurance
select * from insurance limit 10;
select county from insurance;
select avg(tiv_2012 , tiv_2011) from insurance;
select construction 'value', count (construction) 'frequency' from insurance group by construction;

