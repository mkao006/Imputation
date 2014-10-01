
/* file uploaded on 10dec2012*/
proc import datafile = "T:\Amanda\production\nicola_ws_request_12yrsR.xlsx"
out = capex.prod_basefile_10dec2012
dbms = excel replace;
getnames=yes;
run;

data Capex.geog (keep = area ele item num_45-num_50 symb_45-symb_50);
set capex.prod_basefile_10dec2012   ;
where ele in (31 51); 
if area in (351 298 357 76 245 246 247 43 ) then delete;
run;
/* storing the base file in a permanent lib*/
data Temp.geog_BASEFILE;
set Capex.geog; run;



data Temp.geog_clean_F_T;
set Temp.geog_BASEFILE;

 array num num_45-num_50;
 array sym symb_45-symb_50;
 array sub sub_45-sub_50;

do i = 1 to 6;
num{i} = round(num{i});
end;

do i = 1 to 6; 
sub{i} = 1;
end;

do i = 2 to 6;

if num{i} = num{i-1} then do; 
if sym{i} = "F" then sub{i} = "";
end; end;

do i = 2 to 6;
if sym{i} = "E" then sub{i} = "";
end; 

do i = 2 to 6;

if sym {i} = "T" then do; 
sym {i} = "I" ;
num{i} = "";
end; end;


RUN;




data Temp.geog_FINAL_basefile (drop = sub_45-sub_50 i);
set Temp.geog_clean_F_T;

 array num num_45-num_50;
 array sym symb_45-symb_50;
 array sub sub_45-sub_50;

 do i = 2 to 6;

if sub{i} = "" then do;
num{i} = "";
sym{i} = "I"; end; end; run;

/* form groups*/

data Temp.geog1 ;
set Temp.geog_FINAL_basefile ;

length comm_grp  $ 15;
length sub_reg $ 15;
length reg $ 15;

if item in ( 15	27	44	75	56	71	79	83	89	101	92	94	97	103	108) then comm_grp = "Cereals";
if item in (176	187	191	195	201	181	197	210	205	203	211 )then comm_grp = "Pulses";
if item in (125	116	122	137	135	136	149)then comm_grp = "roots_tub";
if item in (221	223	217	225	222	220	224	216	226	234)then comm_grp = "treenuts";
if item in (236	242	260	249	277 305 310	256	263	265	267	270	275	280	289	292	296	299	328	329	333	336	339)
then comm_grp = "oilcrops";
if item in (388	402	403	406	426	407	393	358	372	397	417	414	423	420	366	367	399	449	401	373	394	446	430	
378	567	568	463)then comm_grp = "veg";
if item in (486	489	577	569	574	572	571	603	490	495	507	497	512	560	600	515	521	523	526	530	531	534	536	
544	547	552	554	558	550	549	592	587	542	541	591	619 461)then comm_grp = "fruit";
if item in (656	661	667	671	674)then comm_grp = "stim_beve";
if item in (687	689	692	693	698	702	711	720	723)then comm_grp = "spices";
if item in (826)then comm_grp = "tobacco";
if item in (459 461 677)then comm_grp = "othr_crops";
if item in (837 836)then comm_grp = "rubber";
if item in (748 754)then comm_grp = "leaves";
if item in (767 780 788 800 809 821 782	987	773	1185 789)then comm_grp = "fibres";
if item in (867	947	1035	977	1017 1127	1097 1108	1111	1163	1166)then comm_grp = "othr_meat";
if item in (1058 1069 1073 1080 1141)then comm_grp = "poultry_meat";
if item in (882	951	982	1020	1130)then comm_grp = "milk";
if item in (1062 1091)then comm_grp = "eggs";
/*if item in (886 901)then comm_grp = "lvstk_prds";*/
/*if item in (162 163 1182)then comm_grp = "sugar";*/
/*if item in (36 60	237	244	257	258	252	261	264	266	268	271	276	281	290	293	297	331	334	337	340	1242 
278 307 313)then comm_grp = "veg_oils";*/
/*if item in (564	51	66	82	86)then comm_grp = "alch_bev";*/


if  area in ( 29 45 72 178 238 62 114 129 130 137 270 144 182 184 196 
201 226 215 251 181)then sub_reg = "E_Africa";

if  area in ( 7	32	37	39	46	250	61	74	193)then sub_reg = "C_Africa";

if  area in ( 4	59	124	143	206	222	205	13	103	105	112	118	121	299	221	179	194	212	225	
249)then sub_reg = "NE_NAfr";

if  area in ( 20	122	147	202	209)then sub_reg = "S_Africa";

if  area in ( 53	233	35	107	75	81	90	175	123	133	136	158	159	187	195	197	217)then sub_reg = "W_Africa";

if  area in ( 17 33	85	190	231)then sub_reg = "N_America";

if  area in ( 258	8	22	12	14	239	36	49	55	56	86	87	93	109	135	142	151	177	188	189	191	
220	224	240)then sub_reg = "Carib";

if  area in ( 9	19	21	40	44	58	65	69	91	169	170	207	234	236	23	48	60	89	95	138	157	166
)then sub_reg = "L_America";

if  area in ( 108	113	208	213	235	52)then sub_reg = "C_Asia";

if  area in ( 96	128	41	214	116	110	141	117)then sub_reg = "E_Asia";

if  area in ( 2	16	18	100	102	132	149	165	38 )then sub_reg = "S_Asia";

if  area in ( 26	115	101	120	131	28	171	200	216	176	237)then sub_reg = "SE_Asia";

if  area in ( 57	27	167	51	97	173	146	183	185	199	230	228	1	73)then sub_reg = "E_Europe";

if  area in ( 259	54	63	64	67	99	104	264	119	126	162	260	210	229)then sub_reg = "N_Europe";

if  area in ( 3	6	80	98	82	84	94	106	134	273	174	192	272	186	198	203	154	248	
223	50)then sub_reg = "S_Europe";

if  area in ( 11	255	15	68	79	256	140	150	211)then sub_reg = "W_Europe";

if  area in ( 10	156	161	66	153	168	25	155	88	83	127	145	148	163	164	180	5	
47	70	160	172	244	218	219	227	243)then sub_reg = "Oceania";



if area in ( 29	45	72	178	238	62	114	129	130	137	270	144	182	184	196	201	226	215	251	181	
7	32	37	39	46	250	61	74	193	20	122	147	202	209	53	233	35	107	75	81	90	175	123	133	136	158	
159	187	195	197	217 ) then reg = "SSA";

if area in ( 23 48	60	89	95	138	157	166	9	19	21	40	44	58	65	69	91	169	170	207	234	
236 258	8 22	12	14	239	36	49	55	56	86	87	93	109	135	142	151	177	188	189	191	220	224	240) 
then reg = "LAC";

if area in ( 108	113	208	213	235	52	96	128	41	214	116	110	141	117	2	16	18	100	102	132	149	
165	38	26	115	101	120	131	28	171	200	216	176	237) then reg = "Asia";

if area in (57	27	167	51	97	173	146	183	185	199	230	228	1	73	259	54	63	64	67	99	104	264	
119	126	162	260	210	229	3	6	80	98	82	84	94	106	134	273	174	192	272	186	198	203	154	248	223	50	11	
255	15	68	79	125	256	140	150	211 ) then reg = "Europe";



run;

/* delete blank comm_grp rows*/

data Temp.geog2 ;
set Temp.geog1 ; 
if comm_grp = "" then delete; run;


/* ELEMENT _31_*/

data Temp.geog3_31_wtavg;
		 set Temp.geog2 (where = ( ele = 31));
		
		 array num num_45-num_50;
		 array sym symb_45-symb_50;
	/* Remove to-be-excluded countries i.e. countries with different starting and ending years, - russia, belarus, etc
do i = 1 to 2;
if area in (127	145	163	180) then sym {i} = "M"; end; 

do i = 1 to 3;
if area in (1 52 57	80	98	63	73	108	113	119	126	146	185	186	198	208	154	213	230	235) then sym {i} = "M"; end;

do i = 1 to 4;
if area in (167	178	238	199) then sym {i} = "M"; end;

do i = 1 to 11;
if area in (255 256) then sym {i} = "M"; end;*/

/*do i = 1 to 6;
if area in (272 273) then sym {i} = "M"; end;*/

do i = 1 to 6;
if area in (164) then sym {i} = "M"; end;

do i = 1 to 6;
if area in (228 248) then sym {i} = "M"; end;

do i = 1 to 6;
if area in (51 62) then sym {i} = "M"; end;

do i = 1 to 6;
if area in (15) then sym {i} = "M"; end;

do i = 1 to 6;
if area in (186) then sym {i} = "M"; end;

 do i = 1 to 6;

		If sym {i} = "M" then num{i} = "";
		
END; run;





data Temp.geog_31_decm1;
set Temp.geog3_31_wtavg; run;

proc sort data = Temp.geog_31_decm1; by area comm_grp ; run;

/* this file is created mainly for the sum function, to avoid summing the non-required symbol values*/


data Temp.geog_31_decm2;
set Temp.geog_31_decm1;

	 array num num_45-num_50;
		 array sym symb_45-symb_50;
		 array num_a num_a_45-num_a_50;
		 array num_b num_b_45-num_b_50;

		do i = 2 to 6;
	
if sym{i} notin ( "E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4") and sym{i-1} notin ( "E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4") then do;
num_a{i} = num{i};
num_b{i-1} = num{i-1} ;
 end;end;

		 run;

/* chnage the SORT according to the combination*/

proc sort data = Temp.geog_31_decm2; by area comm_grp ; run;

/* continue with combination SORT*/

proc means data=Temp.geog_31_decm2 nway noprint; /* nway helps to output the dataset*/
by area comm_grp  ;
var num_a_45-num_a_50  num_b_45-num_b_50 ;
output out=Temp.geog_31_decm3 
sum=summ_a_45-summ_a_50  summ_b_45-summ_b_50;
run;

/* Validation 1*/



data Temp.geog_31_decm4 ;
set Temp.geog_31_decm3  ;


		array summ_a summ_a_45-summ_a_50  ;
		array summ_b summ_b_45-summ_b_50;
		 array ch ch_45-ch_50;
	
do i = 2 to 6;

ch{i} = summ_a{i}/summ_b{i-1}; 
end;


run; 

 data Temp.geog_31_decm5;
set Temp.geog_31_decm1 (keep = area item ele num_45-num_50 symb_45-symb_50 comm_grp sub_reg reg); run;

proc sort data = Temp.geog_31_decm5; by area comm_grp  ; run;

data Temp.geog_31_decm6;
merge Temp.geog_31_decm5 Temp.geog_31_decm4 ;
by area comm_grp ; run;

data Temp.geog_31_decm7;

format area 6. item 6. ele 6. 
num_45 8.1 symb_45 $ 5.
num_46 8.1 symb_46 $ 5.
num_47 8.1 symb_47 $ 5.
num_48 8.1 symb_48 $ 5.
num_49 8.1 symb_49 $ 5.
num_50 8.1 symb_50 $ 5.
comm_grp $ 15. sub_reg $ 15. reg $ 15.;
/*summ_1989-summ_2009 12.*/
/*ch_sum_1989-ch_sum_2009 12. ;*/

set  Temp.geog_31_decm6; 

array sym symb_45-symb_50;
array num num_45-num_50;
array ch ch_45-ch_50;

do i = 2 to 6;	
		
if sym{i} in ("I") and num{i-1} < 11 then num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
         End; 


do i = 2 to 6;
if ch{i} = 1 then ch{i} = ""; 
if ch{i} > 1.4 then ch{i} = "";
if ch{i} < 0.6 then ch{i} = "";
end;


do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = "" then do; 
		 num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
end;end;

 do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = 0 then do; 
num{i} = "";
end;end;

do i = 2 to 6;

		if num {i} ne '' and sym{i} in ("I") then do ;
sym{i} = "I1"; end;

		 end; run;


	/* Validation 2 - to solve the issue of repeated numbers*/

data Temp.geog_31_decm8; 
set Temp.geog_31_decm7; 
array sym symb_45-symb_50;
array num num_45-num_50;
array diff diff_45-diff_50;
array decim d_45-d_50;

do i = 2 to 6;
diff{i} = num {i} - num{i-1} ; diff{i} = round(diff{i}) ;  end; 


do i = 2 to 6;
if diff{i} = 0 then decim{i} = 1 ; 
end;

do i = 2 to 6;
if decim{i} = 1 and sym{i} = "I1" then do;
num{i} = "" ; 
sym{i} = "I"; end; 


if decim{i} = 1 and sym{i-1} = "I1" then DO;
num{i-1} = "" ;
sym{i-1} = "I"; 
end;
end; run;


data Temp.geog_31_decm8_count;
set Temp.geog_31_decm8;
id = _N_;  
array num num_45-num_50;
array sym symb_45-symb_50;
array memfound mem_45-mem_50; 
do i=2 to 6;
 if sym{i} = "I1" and num{i} ne "" then do; memfound{i}+1;
 total =Sum(of mem_46-mem_50);
drop i ;  end; end;
run;

/*
proc export data = Temp.geog_31_decm8_count
outfile = "C:\Documents and Settings\khaira\My Documents\CAPEX\SAS programs\SAS results\31.csv"
dbms = csv replace; run;*/




/*-----------------------*/


data Temp.geog_it_subr_31_decm1;
set Temp.geog3_31_wtavg; run;

proc sort data = Temp.geog_it_subr_31_decm1; by item sub_reg ; run;

/* this file is created mainly for the sum function, to avoid summing the non-required symbol values*/


data Temp.geog_it_subr_31_decm2;
set Temp.geog_it_subr_31_decm1;

	 array num num_45-num_50;
		 array sym symb_45-symb_50;
		 array num_a num_a_45-num_a_50;
		 array num_b num_b_45-num_b_50;

		 do i = 2 to 6;
	
if sym{i} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4") and sym{i-1} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4") then do;
num_a{i} = num{i};
num_b{i-1} = num{i-1} ;
 end;end;

		 run;

/* chnage the SORT according to the combination*/

proc sort data = Temp.geog_it_subr_31_decm2; by item sub_reg ; run;

/* continue with combination SORT*/

proc means data=Temp.geog_it_subr_31_decm2 nway noprint; /* nway helps to output the dataset*/
by item sub_reg  ;
var num_a_45-num_a_50  num_b_45-num_b_50 ;
output out=Temp.geog_it_subr_31_decm3 
sum=summ_a_45-summ_a_50  summ_b_45-summ_b_50;
run;

/* Validation 1*/



data Temp.geog_it_subr_31_decm4 ;
set Temp.geog_it_subr_31_decm3  ;


		array summ_a summ_a_45-summ_a_50 ;
		array summ_b summ_b_45-summ_b_50;
		 array ch ch_45-ch_50;
	
do i = 2 to 6;

ch{i} = summ_a{i}/summ_b{i-1}; 
end;


run; 

 data Temp.geog_it_subr_31_decm5;
set Temp.geog_31_decm8 (keep = area item ele num_45-num_50 symb_45-symb_50 comm_grp sub_reg reg); run;

proc sort data = Temp.geog_it_subr_31_decm5; by item sub_reg  ; run;

data Temp.geog_it_subr_31_decm6;
merge Temp.geog_it_subr_31_decm5 Temp.geog_it_subr_31_decm4 ;
by item sub_reg ; run;

data Temp.geog_it_subr_31_decm7;

format area 6. item 6. ele 6. 
num_45 8.1 symb_45 $ 5.
num_46 8.1 symb_46 $ 5.
num_47 8.1 symb_47 $ 5.
num_48 8.1 symb_48 $ 5.
num_49 8.1 symb_49 $ 5.
num_50 8.1 symb_50 $ 5.
comm_grp $ 15. sub_reg $ 15. reg $ 15.;
/*summ_1989-summ_2009 12.*/
/*ch_sum_1989-ch_sum_2009 12. ;*/

set  Temp.geog_it_subr_31_decm6; 

array sym symb_45-symb_50;
array num num_45-num_50;
array ch ch_45-ch_50;
do i = 2 to 6;	
		
if sym{i} in ("I") and num{i-1} < 11 then num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
         End; 


do i = 2 to 6;
if ch{i} = 1 then ch{i} = ""; 
if ch{i} > 1.4 then ch{i} = "";
if ch{i} < 0.6 then ch{i} = "";
end;


do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = "" then do; 
		 num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
end;end;

 do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = 0 then do; 
num{i} = "";
end;end;
do i = 2 to 6;

		if num {i} ne '' and sym{i} in ("I") then do ;
sym{i} = "I2"; end;

		 end; 


run;



	/* Validation 2 - to solve the issue of repeated numbers*/

data Temp.geog_it_subr_31_decm8; set Temp.geog_it_subr_31_decm7; 
array sym symb_45-symb_50;
array num num_45-num_50;
array diff diff_45-diff_50;
array decim d_45-d_50;

do i = 2 to 6;
diff{i} = num {i} - num{i-1} ; diff{i} = round(diff{i}) ;  end; 


do i = 2 to 6;
if diff{i} = 0 then decim{i} = 1 ; 
end;

do i = 2 to 6;
if decim{i} = 1 and sym{i} = "I2" then do;
num{i} = "" ; 
sym{i} = "I"; end; 



if decim{i} = 1 and sym{i-1} = "I2" then DO;
num{i-1} = "" ;
sym{i-1} = "I"; 
end;
end; run;



data Temp.geog_it_subr_31_decm8_count;
set Temp.geog_it_subr_31_decm8;
id = _N_;  
array num num_45-num_50;
array sym symb_45-symb_50;
array memfound mem_45-mem_50; 
do i=2 to 6;
 if sym{i} = "I2" and num{i} ne "" then do; memfound{i}+1;
 total =Sum(of mem_46-mem_50);
drop i ;  end; end;
run;








/*-----------------------*/

data Temp.geog_itg_subr_31_decm1;
set Temp.geog3_31_wtavg; run;

proc sort data = Temp.geog_itg_subr_31_decm1; by comm_grp sub_reg ; run;

/* this file is created mainly for the sum function, to avoid summing the non-required symbol values*/


data Temp.geog_itg_subr_31_decm2;
set Temp.geog_itg_subr_31_decm1;

	 array num num_45-num_50;
		 array sym symb_45-symb_50;
		 array num_a num_a_45-num_a_50;
		 array num_b num_b_45-num_b_50;

		 do i = 2 to 6;
	
if sym{i} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4") and sym{i-1} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4") then do;
num_a{i} = num{i};
num_b{i-1} = num{i-1} ;
 end;end;

		 run;

/* chnage the SORT according to the combination*/

proc sort data = Temp.geog_itg_subr_31_decm2; by comm_grp sub_reg ; run;

/* continue with combination SORT*/

proc means data=Temp.geog_itg_subr_31_decm2 nway noprint; /* nway helps to output the dataset*/
by comm_grp sub_reg  ;
var num_a_45-num_a_50  num_b_45-num_b_50 ;
output out=Temp.geog_itg_subr_31_decm3 
sum=summ_a_45-summ_a_50  summ_b_45-summ_b_50;
run;

/* Validation 1*/



data Temp.geog_itg_subr_31_decm4 ;
set Temp.geog_itg_subr_31_decm3  ;


		array summ_a summ_a_45-summ_a_50 ;
		array summ_b summ_b_45-summ_b_50;
		 array ch ch_45-ch_50;
	
do i = 2 to 6;

ch{i} = summ_a{i}/summ_b{i-1}; 
end;

run; 

 data Temp.geog_itg_subr_31_decm5;
set Temp.geog_it_subr_31_decm8 (keep = area item ele num_45-num_50 symb_45-symb_50 comm_grp sub_reg reg); run;

proc sort data = Temp.geog_itg_subr_31_decm5; by comm_grp sub_reg  ; run;

data Temp.geog_itg_subr_31_decm6;
merge Temp.geog_itg_subr_31_decm5 Temp.geog_itg_subr_31_decm4 ;
by comm_grp sub_reg ; run;

data Temp.geog_itg_subr_31_decm7;

format area 6. item 6. ele 6. 
num_45 8.1 symb_45 $ 5.
num_46 8.1 symb_46 $ 5.
num_47 8.1 symb_47 $ 5.
num_48 8.1 symb_48 $ 5.
num_49 8.1 symb_49 $ 5.
num_50 8.1 symb_50 $ 5.
comm_grp $ 15. sub_reg $ 15. reg $ 15.;
/*summ_1989-summ_2009 12.*/
/*ch_sum_1989-ch_sum_2009 12. ;*/

set  Temp.geog_itg_subr_31_decm6; 

array sym symb_45-symb_50;
array num num_45-num_50;
array ch ch_45-ch_50;
do i = 2 to 6;	
		
if sym{i} in ("I") and num{i-1} < 11 then num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
         End; 


do i = 2 to 6;
if ch{i} = 1 then ch{i} = ""; 
if ch{i} > 1.4 then ch{i} = "";
if ch{i} < 0.6 then ch{i} = "";
end;


do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = "" then do; 
		 num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
end;end;

 do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = 0 then do; 
num{i} = "";
end;end;
do i = 2 to 6;

		if num {i} ne '' and sym{i} in ("I") then do ;
sym{i} = "I3"; end;

		 end; run;


	/* Validation 2 - to solve the issue of repeated numbers*/

data Temp.geog_itg_subr_31_decm8; set Temp.geog_itg_subr_31_decm7; 
array sym symb_45-symb_50;
array num num_45-num_50;
array diff diff_45-diff_50;
array decim d_45-d_50;

do i = 2 to 6;
diff{i} = num {i} - num{i-1} ; diff{i} = round(diff{i}) ;  end; 


do i = 2 to 6;
if diff{i} = 0 then decim{i} = 1 ; 
end;

do i = 2 to 6;
if decim{i} = 1 and sym{i} = "I3" then do;
num{i} = "" ; 
sym{i} = "I"; end; 



if decim{i} = 1 and sym{i-1} = "I3" then DO;
num{i-1} = "" ;
sym{i-1} = "I"; 
end;
end; run;


data Temp.geog_itg_subr_31_decm8_count;
set Temp.geog_itg_subr_31_decm8;
id = _N_;  
array num num_45-num_50;
array sym symb_45-symb_50;
array memfound mem_45-mem_50; 
do i=2 to 6;
 if sym{i} = "I3" and num{i} ne "" then do; memfound{i}+1;
 total =Sum(of mem_46-mem_50);
drop i ;  end; end;
run;




/*---------------*/


data Temp.geog_it_reg_31_decm1;
set Temp.geog3_31_wtavg; run;

proc sort data = Temp.geog_it_reg_31_decm1; by item reg ; run;

/* this file is created mainly for the sum function, to avoid summing the non-required symbol values*/


data Temp.geog_it_reg_31_decm2;
set Temp.geog_it_reg_31_decm1;

	 array num num_45-num_50;
		 array sym symb_45-symb_50;
		 array num_a num_a_45-num_a_50;
		 array num_b num_b_45-num_b_50;

		 do i = 2 to 6;
	
if sym{i} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4") and sym{i-1} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4") then do;
num_a{i} = num{i};
num_b{i-1} = num{i-1} ;
 end;end;
		 run;

/* chnage the SORT according to the combination*/

proc sort data = Temp.geog_it_reg_31_decm2; by item reg ; run;

/* continue with combination SORT*/

proc means data=Temp.geog_it_reg_31_decm2 nway noprint; /* nway helps to output the dataset*/
by item reg  ;
var num_a_45-num_a_50  num_b_45-num_b_50 ;
output out=Temp.geog_it_reg_31_decm3 
sum=summ_a_45-summ_a_50  summ_b_45-summ_b_50;
run;

/* Validation 1*/



data Temp.geog_it_reg_31_decm4 ;
set Temp.geog_it_reg_31_decm3  ;


		array summ_a summ_a_45-summ_a_50 ;
		array summ_b summ_b_45-summ_b_50;
		 array ch ch_45-ch_50;
	
do i = 2 to 6;

ch{i} = summ_a{i}/summ_b{i-1}; 
end;


run; 

 data Temp.geog_it_reg_31_decm5;
set Temp.geog_itg_subr_31_decm8 (keep = area item ele num_45-num_50 symb_45-symb_50 comm_grp sub_reg reg); run;

proc sort data = Temp.geog_it_reg_31_decm5; by item reg  ; run;

data Temp.geog_it_reg_31_decm6;
merge Temp.geog_it_reg_31_decm5 Temp.geog_it_reg_31_decm4 ;
by item reg ; run;

data Temp.geog_it_reg_31_decm7;

format area 6. item 6. ele 6. 
num_45 8.1 symb_45 $ 5.
num_46 8.1 symb_46 $ 5.
num_47 8.1 symb_47 $ 5.
num_48 8.1 symb_48 $ 5.
num_49 8.1 symb_49 $ 5.
num_50 8.1 symb_50 $ 5.
comm_grp $ 15. sub_reg $ 15. reg $ 15.;
/*summ_1989-summ_2009 12.*/
/*ch_sum_1989-ch_sum_2009 12. ;*/

set  Temp.geog_it_reg_31_decm6; 

array sym symb_45-symb_50;
array num num_45-num_50;
array ch ch_45-ch_50;

if reg ne "" then do;

do i = 2 to 6;	
		
if sym{i} in ("I") and num{i-1} < 11 then num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
         End; 


do i = 2 to 6;
if ch{i} = 1 then ch{i} = ""; 
if ch{i} > 1.4 then ch{i} = "";
if ch{i} < 0.6 then ch{i} = "";
end;


do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = "" then do; 
		 num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
end;end;

 do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = 0 then do; 
num{i} = "";
end;end;
do i = 2 to 6;

		if num {i} ne '' and sym{i} in ("I") then do ;
sym{i} = "I4"; end; end;

		 end; run;


	/* Validation 2 - to solve the issue of repeated numbers*/

data Temp.geog_it_reg_31_decm8; set Temp.geog_it_reg_31_decm7; 
array sym symb_45-symb_50;
array num num_45-num_50;
array diff diff_45-diff_50;
array decim d_45-d_50;

do i = 2 to 6;
diff{i} = num {i} - num{i-1} ; diff{i} = round(diff{i}) ;  end; 


do i = 2 to 6;
if diff{i} = 0 then decim{i} = 1 ; 
end;

do i = 2 to 6;
if decim{i} = 1 and sym{i} = "I4" then do;
num{i} = "" ; 
sym{i} = "I"; end; 



if decim{i} = 1 and sym{i-1} = "I4" then DO;
num{i-1} = "" ;
sym{i-1} = "I"; 
end;
end; run;


data Temp.geog_it_reg_31_decm8_count;
set Temp.geog_it_reg_31_decm8;
id = _N_;  
array num num_45-num_50;
array sym symb_45-symb_50;
array memfound mem_45-mem_50; 
do i=2 to 6;
 if sym{i} = "I4" and num{i} ne "" then do; memfound{i}+1;
 total =Sum(of mem_46-mem_50);
drop i ;  end; end;
run;






/*---------------*/


data Temp.geog_itg_reg_31_decm1;
set Temp.geog3_31_wtavg; run;

proc sort data = Temp.geog_itg_reg_31_decm1; by comm_grp reg ; run;

/* this file is created mainly for the sum function, to avoid summing the non-required symbol values*/


data Temp.geog_itg_reg_31_decm2;
set Temp.geog_itg_reg_31_decm1;

	 array num num_45-num_50;
		 array sym symb_45-symb_50;
		 array num_a num_a_45-num_a_50;
		 array num_b num_b_45-num_b_50;

		 do i = 2 to 6;
	
if sym{i} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4") and sym{i-1} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4") then do;
num_a{i} = num{i};
num_b{i-1} = num{i-1} ;
 end;end;

		 run;

/* chnage the SORT according to the combination*/

proc sort data = Temp.geog_itg_reg_31_decm2; by comm_grp reg ; run;

/* continue with combination SORT*/

proc means data=Temp.geog_itg_reg_31_decm2 nway noprint; /* nway helps to output the dataset*/
by comm_grp reg  ;
var num_a_45-num_a_50  num_b_45-num_b_50 ;
output out=Temp.geog_itg_reg_31_decm3 
sum=summ_a_45-summ_a_50  summ_b_45-summ_b_50;
run;

/* Validation 1*/



data Temp.geog_itg_reg_31_decm4 ;
set Temp.geog_itg_reg_31_decm3  ;


		array summ_a summ_a_45-summ_a_50 ;
		array summ_b summ_b_45-summ_b_50;
		 array ch ch_45-ch_50;
	
do i = 2 to 6;

ch{i} = summ_a{i}/summ_b{i-1}; 
end;


run; 

 data Temp.geog_itg_reg_31_decm5;
set Temp.geog_it_reg_31_decm8 (keep = area item ele num_45-num_50 symb_45-symb_50 comm_grp sub_reg reg); run;

proc sort data = Temp.geog_itg_reg_31_decm5; by comm_grp reg  ; run;

data Temp.geog_itg_reg_31_decm6;
merge Temp.geog_itg_reg_31_decm5 Temp.geog_itg_reg_31_decm4 ;
by comm_grp reg ; run;

data Temp.geog_itg_reg_31_decm7;

format area 6. item 6. ele 6. 
num_45 8.1 symb_45 $ 5.
num_46 8.1 symb_46 $ 5.
num_47 8.1 symb_47 $ 5.
num_48 8.1 symb_48 $ 5.
num_49 8.1 symb_49 $ 5.
num_50 8.1 symb_50 $ 5.
comm_grp $ 15. sub_reg $ 15. reg $ 15.;
/*summ_1989-summ_2009 12.*/
/*ch_sum_1989-ch_sum_2009 12. ;*/

set  Temp.geog_itg_reg_31_decm6; 

array sym symb_45-symb_50;
array num num_45-num_50;
array ch ch_45-ch_50;

if reg ne "" then do;

do i = 2 to 6;	
		
if sym{i} in ("I") and num{i-1} < 11 then num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
         End; 


do i = 2 to 6;
if ch{i} = 1 then ch{i} = ""; 
if ch{i} > 1.4 then ch{i} = "";
if ch{i} < 0.6 then ch{i} = "";
end;


do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = "" then do; 
		 num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
end;end;

 do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = 0 then do; 
num{i} = "";
end;end;
do i = 2 to 6;

		if num {i} ne '' and sym{i} in ("I") then do ;
sym{i} = "I5"; end; end;

		 end; run;


	/* Validation 2 - to solve the issue of repeated numbers*/

data Temp.geog_itg_reg_31_decm8; set Temp.geog_itg_reg_31_decm7; 
array sym symb_45-symb_50;
array num num_45-num_50;
array diff diff_45-diff_50;
array decim d_45-d_50;

do i = 2 to 6;
diff{i} = num {i} - num{i-1} ; diff{i} = round(diff{i}) ;  end; 


do i = 2 to 6;
if diff{i} = 0 then decim{i} = 1 ; 
end;

do i = 2 to 6;
if decim{i} = 1 and sym{i} = "I5" then do;
num{i} = "" ; 
sym{i} = "I"; end; 



if decim{i} = 1 and sym{i-1} = "I5" then DO;
num{i-1} = "" ;
sym{i-1} = "I"; 
end;
end; run;



data Temp.geog_itg_reg_31_decm8_count;
set Temp.geog_itg_reg_31_decm8;
id = _N_;  
array num num_45-num_50;
array sym symb_45-symb_50;
array memfound mem_45-mem_50; 
do i=2 to 6;
 if sym{i} = "I5" and num{i} ne "" then do; memfound{i}+1;
 total =Sum(of mem_46-mem_50);
drop i ;  end; end;
run;





data Temp.miss_31_;
set Temp.geog_itg_reg_31_decm8; 
id = _N_;  
array num num_45-num_50;
array sym symb_45-symb_50;
array memfound mem_45-mem_50; 
do i=2 to 6;
 if num{i} = "" and sym{i} = "I" then do; 
memfound{i}+1;
 total =Sum(of mem_46-mem_50);
drop i ;  end; end;
run;



proc export data = Temp.geog_itg_reg_31_decm8
outfile = "C:\Documents and Settings\khaira\My Documents\CAPEX\SAS programs\SAS results\Dec2012_31.csv"
dbms = csv replace; run;













/* REVERSE*/

data Temp_Rev.geog_31_decmA;
set Temp.geog3_31_wtavg; run;


data Temp_Rev.geog_31_decm1A ;

retain area item ele num_50 symb_50 num_49 symb_49 num_48 symb_48 num_47 symb_47 num_46 symb_46 num_45 symb_45 comm_grp sub_reg reg ; 

set Temp_Rev.geog_31_decmA; 

run;



proc sort data = Temp_Rev.geog_31_decm1A; by area comm_grp ; run;

/* this file is created mainly for the sum function, to avoid summing the non-required symbol values*/


data Temp_Rev.geog_31_decm2A;
set Temp_Rev.geog_31_decm1A;

	 array num num_50-num_45;
		 array sym symb_50-symb_45;
		 array num_a num_a_50-num_a_45;
		 array num_b num_b_50-num_b_45;

		do i = 2 to 6;
	
if sym{i} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4" "I5") 
and sym{i-1} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4" "I5") then do;
num_a{i} = num{i};
num_b{i-1} = num{i-1} ;
 end;end;

		 run;

/* chnage the SORT according to the combination*/

proc sort data = Temp_Rev.geog_31_decm2A; by area comm_grp ; run;

/* continue with combination SORT*/

proc means data=Temp_Rev.geog_31_decm2A nway noprint; /* nway helps to output the dataset*/
by area comm_grp  ;
var num_a_50-num_a_45  num_b_50-num_b_45 ;
output out=Temp_Rev.geog_31_decm3A 
sum=summ_a_50-summ_a_45  summ_b_50-summ_b_45;
run;

/* Validation 1*/



data Temp_Rev.geog_31_decm4A ;
set Temp_Rev.geog_31_decm3A  ;


		array summ_a summ_a_50-summ_a_45  ;
		array summ_b summ_b_50-summ_b_45;
		 array ch ch_50-ch_45;
	
do i = 2 to 6;

ch{i} = summ_a{i}/summ_b{i-1}; 
end;


run; 

 data Temp_Rev.geog_31_decm5A;

retain area item ele num_50 symb_50 num_49 symb_49 num_48 symb_48 num_47 symb_47 num_46 symb_46 num_45 symb_45 comm_grp sub_reg reg ; 

set Temp.geog_itg_reg_31_decm8 (keep = area item ele num_50-num_45 symb_50-symb_45 comm_grp sub_reg reg); 

run;

proc sort data = Temp_Rev.geog_31_decm5A; by area comm_grp  ; run;

data Temp_Rev.geog_31_decm6A;
merge Temp_Rev.geog_31_decm5A Temp_Rev.geog_31_decm4A ;
by area comm_grp ; run;


data Temp_Rev.geog_31_decm7A;

format area 6. item 6. ele 6. 
num_50 8.1 symb_50 $ 5.
num_49 8.1 symb_49 $ 5.
num_48 8.1 symb_48 $ 5.
num_47 8.1 symb_47 $ 5.
num_46 8.1 symb_46 $ 5.
num_45 8.1 symb_45 $ 5.

comm_grp $ 15. sub_reg $ 15. reg $ 15.;
/*summ_1989-summ_2009 12.*/
/*ch_sum_1989-ch_sum_2009 12. ;*/

set  Temp_Rev.geog_31_decm6A; 

array sym symb_50-symb_45;
array num num_50-num_45;
array ch ch_50-ch_45;

do i = 2 to 6;	
		
if sym{i} in ("I") and num{i-1} < 11 then num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
         End; 


do i = 2 to 6;
if ch{i} = 1 then ch{i} = ""; 
if ch{i} > 1.4 then ch{i} = "";
if ch{i} < 0.6 then ch{i} = "";
end;


do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = "" then do; 
		 num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
end;end;

 do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = 0 then do; 
num{i} = "";
end;end;

do i = 2 to 6;

		if num {i} ne '' and sym{i} in ("I") then do ;
sym{i} = "IA"; end;

		 end; run;


	/* Validation 2 - to solve the issue of repeated numbers*/

data Temp_Rev.geog_31_decm8A; 
set Temp_Rev.geog_31_decm7A; 
array sym symb_50-symb_45;
array num num_50-num_45;
array diff diff_50-diff_45;
array decim d_50-d_45;

do i = 2 to 6;
diff{i} = num {i} - num{i-1} ; diff{i} = round(diff{i}) ;  end; 


do i = 2 to 6;
if diff{i} = 0 then decim{i} = 1 ; 
end;

do i = 2 to 6;
if decim{i} = 1 and sym{i} = "IA" then do;
num{i} = "" ; 
sym{i} = "I"; end; 


if decim{i} = 1 and sym{i-1} = "IA" then DO;
num{i-1} = "" ;
sym{i-1} = "I"; 
end;
end; run;


data Temp_Rev.geog_31_decm8A_count;
set Temp_Rev.geog_31_decm8A;
id = _N_;  
array num num_50-num_45;
array sym symb_50-symb_45;
array memfound mem_50-mem_45; 
do i=1 to 6;
 if sym{i} = "IA" and num{i} ne "" then do; memfound{i}+1;
 total =Sum(of mem_50-mem_46);
drop i ;  end; end;
run;



/*-----------------------*/




data Temp_Rev.geog_it_subr_31_decm1B;
set Temp_Rev.geog_31_decm1A ; run;

proc sort data = Temp_Rev.geog_it_subr_31_decm1B; by item sub_reg ; run;

/* this file is created mainly for the sum function, to avoid summing the non-required symbol values*/


data Temp_Rev.geog_it_subr_31_decm2B;
set Temp_Rev.geog_it_subr_31_decm1B;

	 array num num_50-num_45;
		 array sym symb_50-symb_45;
		 array num_a num_a_50-num_a_45;
		 array num_b num_b_50-num_b_45;

		 do i = 2 to 6;
	
if sym{i} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4" "I5" "IA" "IB" "IC" "ID" "IE") 
and sym{i-1} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4" "I5" "IA" "IB" "IC" "ID" "IE") then do;
num_a{i} = num{i};
num_b{i-1} = num{i-1} ;
 end;end;

		 run;

/* chnage the SORT according to the combination*/

proc sort data = Temp_Rev.geog_it_subr_31_decm2B; by item sub_reg ; run;

/* continue with combination SORT*/

proc means data=Temp_Rev.geog_it_subr_31_decm2B nway noprint; /* nway helps to output the dataset*/
by item sub_reg  ;
var num_a_50-num_a_45  num_b_50-num_b_45 ;
output out=Temp_Rev.geog_it_subr_31_decm3B 
sum=summ_a_50-summ_a_45  summ_b_50-summ_b_45;
run;

/* Validation 1*/



data Temp_Rev.geog_it_subr_31_decm4B ;
set Temp_Rev.geog_it_subr_31_decm3B  ;


		array summ_a summ_a_50-summ_a_45  ;
		array summ_b summ_b_50-summ_b_45;
		 array ch ch_50-ch_45;
	
do i = 2 to 6;

ch{i} = summ_a{i}/summ_b{i-1}; 
end;


run; 

 data Temp_Rev.geog_it_subr_31_decm5B;

retain area item ele num_50 symb_50 num_49 symb_49 num_48 symb_48 num_47 symb_47 num_46 symb_46 num_45 symb_45  comm_grp sub_reg reg ; 

set Temp_Rev.geog_31_decm8A (keep = area item ele num_50-num_45 symb_50-symb_45 comm_grp sub_reg reg); run;

proc sort data = Temp_Rev.geog_it_subr_31_decm5B; by item sub_reg  ; run;

data Temp_Rev.geog_it_subr_31_decm6B;
merge Temp_Rev.geog_it_subr_31_decm5B Temp_Rev.geog_it_subr_31_decm4B ;
by item sub_reg ; run;

data Temp_Rev.geog_it_subr_31_decm7B;

format area 6. item 6. ele 6. 
num_50 8.1 symb_50 $ 5.
num_49 8.1 symb_49 $ 5.
num_48 8.1 symb_48 $ 5.
num_47 8.1 symb_47 $ 5.
num_46 8.1 symb_46 $ 5.
num_45 8.1 symb_45 $ 5.

comm_grp $ 15. sub_reg $ 15. reg $ 15.;
/*summ_1989-summ_2009 12.*/
/*ch_sum_1989-ch_sum_2009 12. ;*/

set  Temp_Rev.geog_it_subr_31_decm6B; 

array sym symb_50-symb_45;
array num num_50-num_45;
array ch ch_50-ch_45;
do i = 2 to 6;	
		
if sym{i} in ("I") and num{i-1} < 11 then num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
         End; 


do i = 2 to 6;
if ch{i} = 1 then ch{i} = ""; 
if ch{i} > 1.4 then ch{i} = "";
if ch{i} < 0.6 then ch{i} = "";
end;


do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = "" then do; 
		 num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
end;end;

 do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = 0 then do; 
num{i} = "";
end;end;
do i = 2 to 6;

		if num {i} ne '' and sym{i} in ("I") then do ;
sym{i} = "IB"; end;

		 end; 


run;



	/* Validation 2 - to solve the issue of repeated numbers*/

data Temp_Rev.geog_it_subr_31_decm8B; set Temp_Rev.geog_it_subr_31_decm7B; 
array sym symb_50-symb_45;
array num num_50-num_45;
array diff diff_50-diff_45;
array decim d_50-d_45;

do i = 2 to 6;
diff{i} = num {i} - num{i-1} ; diff{i} = round(diff{i}) ;  end; 


do i = 2 to 6;
if diff{i} = 0 then decim{i} = 1 ; 
end;

do i = 2 to 6;
if decim{i} = 1 and sym{i} = "IB" then do;
num{i} = "" ; 
sym{i} = "I"; end; 



if decim{i} = 1 and sym{i-1} = "IB" then DO;
num{i-1} = "" ;
sym{i-1} = "I"; 
end;
end; run;



data Temp_Rev.geog_it_subr_31_decm8B_count;
set Temp_Rev.geog_it_subr_31_decm8B;
id = _N_;  
array num num_50-num_45;
array sym symb_50-symb_45;
array memfound mem_50-mem_45; 
do i=1 to 6;
 if sym{i} = "IB" and num{i} ne "" then do; memfound{i}+1;
 total =Sum(of mem_50-mem_46);
drop i ;  end; end;
run;









/*-----------------------*/

data Temp_Rev.geog_itg_subr_31_decm1C;
set Temp_Rev.geog_31_decm1A; run;

proc sort data = Temp_Rev.geog_itg_subr_31_decm1C; by comm_grp sub_reg ; run;

/* this file is created mainly for the sum function, to avoid summing the non-required symbol values*/


data Temp_Rev.geog_itg_subr_31_decm2C;
set Temp_Rev.geog_itg_subr_31_decm1C;

	 array num num_50-num_45;
		 array sym symb_50-symb_45;
		 array num_a num_a_50-num_a_45;
		 array num_b num_b_50-num_b_45;

		 do i = 2 to 6;
	
if sym{i} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4" "I5" "IA" "IB" "IC" "ID" "IE") 
and sym{i-1} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4" "I5" "IA" "IB" "IC" "ID" "IE") then do;
num_a{i} = num{i};
num_b{i-1} = num{i-1} ;
 end;end;

		 run;

/* chnage the SORT according to the combination*/

proc sort data = Temp_Rev.geog_itg_subr_31_decm2C; by comm_grp sub_reg ; run;

/* continue with combination SORT*/

proc means data=Temp_Rev.geog_itg_subr_31_decm2C nway noprint; /* nway helps to output the dataset*/
by comm_grp sub_reg  ;
var num_a_50-num_a_45  num_b_50-num_b_45 ;
output out=Temp_Rev.geog_itg_subr_31_decm3C 
sum=summ_a_50-summ_a_45  summ_b_50-summ_b_45;
run;

/* Validation 1*/



data Temp_Rev.geog_itg_subr_31_decm4C;
set Temp_Rev.geog_itg_subr_31_decm3C  ;


		array summ_a summ_a_50-summ_a_45 ;
		array summ_b summ_b_50-summ_b_45;
		 array ch ch_50-ch_45;
	
do i = 2 to 6;

ch{i} = summ_a{i}/summ_b{i-1}; 
end;

run; 

 data Temp_Rev.geog_itg_subr_31_decm5C;

 retain area item ele num_50 symb_50 num_49 symb_49 num_48 symb_48 num_47 symb_47 num_46 symb_46 num_45 symb_45 comm_grp sub_reg reg ; 

set Temp_Rev.geog_it_subr_31_decm8B (keep = area item ele num_45-num_50 symb_45-symb_50 comm_grp sub_reg reg); run;

proc sort data = Temp_Rev.geog_itg_subr_31_decm5C; by comm_grp sub_reg  ; run;

data Temp_Rev.geog_itg_subr_31_decm6C;
merge Temp_Rev.geog_itg_subr_31_decm5C Temp_Rev.geog_itg_subr_31_decm4C ;
by comm_grp sub_reg ; run;

data Temp_Rev.geog_itg_subr_31_decm7C;

format area 6. item 6. ele 6. 
num_50 8.1 symb_50 $ 5.
num_48 8.1 symb_48 $ 5.
num_47 8.1 symb_47 $ 5.
num_46 8.1 symb_46 $ 5.
num_45 8.1 symb_45 $ 5.

comm_grp $ 15. sub_reg $ 15. reg $ 15.;

/*summ_1989-summ_2009 12.*/
/*ch_sum_1989-ch_sum_2009 12. ;*/

set  Temp_Rev.geog_itg_subr_31_decm6C; 

array sym symb_50-symb_45;
array num num_50-num_45;
array ch ch_50-ch_45;
do i = 2 to 6;	
		
if sym{i} in ("I") and num{i-1} < 11 then num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
         End; 


do i = 2 to 6;
if ch{i} = 1 then ch{i} = ""; 
if ch{i} > 1.4 then ch{i} = "";
if ch{i} < 0.6 then ch{i} = "";
end;


do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = "" then do; 
		 num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
end;end;

 do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = 0 then do; 
num{i} = "";
end;end;
do i = 2 to 6;

		if num {i} ne '' and sym{i} in ("I") then do ;
sym{i} = "IC"; end;

		 end; run;


	/* Validation 2 - to solve the issue of repeated numbers*/

data Temp_Rev.geog_itg_subr_31_decm8C; 
set Temp_Rev.geog_itg_subr_31_decm7C; 

array sym symb_50-symb_45;
array num num_50-num_45;
array diff diff_50-diff_45;
array decim d_50-d_45;

do i = 2 to 6;
diff{i} = num {i} - num{i-1} ; diff{i} = round(diff{i}) ;  end; 


do i = 2 to 6;
if diff{i} = 0 then decim{i} = 1 ; 
end;

do i = 2 to 6;
if decim{i} = 1 and sym{i} = "IC" then do;
num{i} = "" ; 
sym{i} = "I"; end; 



if decim{i} = 1 and sym{i-1} = "IC" then DO;
num{i-1} = "" ;
sym{i-1} = "I"; 
end;
end; run;


data Temp_Rev.geog_itg_subr_31_decm8C_count;
set Temp_Rev.geog_itg_subr_31_decm8C;
id = _N_;  
array num num_50-num_45;
array sym symb_50-symb_45;
array memfound mem_50-mem_45; 
do i=1 to 6;
 if sym{i} = "IC" and num{i} ne "" then do; memfound{i}+1;
 total =Sum(of mem_50-mem_46);
drop i ;  end; end;
run;




/*---------------*/


data Temp_Rev.geog_it_reg_31_decm1D;
set Temp_Rev.geog_31_decm1A; run;

proc sort data = Temp_Rev.geog_it_reg_31_decm1D; by item reg ; run;

/* this file is created mainly for the sum function, to avoid summing the non-required symbol values*/


data Temp_Rev.geog_it_reg_31_decm2D;
set Temp_Rev.geog_it_reg_31_decm1D;

	 array num num_50-num_45;
		 array sym symb_50-symb_45;
		 array num_a num_a_50-num_a_45;
		 array num_b num_b_50-num_b_45;

		 do i = 2 to 6;
	
if sym{i} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4" "I5" "IA" "IB" "IC" "ID" "IE")
and sym{i-1} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4" "I5" "IA" "IB" "IC" "ID" "IE") then do;
num_a{i} = num{i};
num_b{i-1} = num{i-1} ;
 end;end;
		 run;

/* chnage the SORT according to the combination*/

proc sort data = Temp_Rev.geog_it_reg_31_decm2D; by item reg ; run;

/* continue with combination SORT*/

proc means data=Temp_Rev.geog_it_reg_31_decm2D nway noprint; /* nway helps to output the dataset*/
by item reg  ;
var num_a_50-num_a_45  num_b_50-num_b_45 ;
output out=Temp_Rev.geog_it_reg_31_decm3D 
sum=summ_a_50-summ_a_45  summ_b_50-summ_b_45;
run;

/* Validation 1*/



data Temp_Rev.geog_it_reg_31_decm4D ;
set Temp_Rev.geog_it_reg_31_decm3D  ;


		array summ_a summ_a_50-summ_a_45 ;
		array summ_b summ_b_50-summ_b_45;
		 array ch ch_50-ch_45;
	
do i = 2 to 6;

ch{i} = summ_a{i}/summ_b{i-1}; 
end;


run; 

 data Temp_Rev.geog_it_reg_31_decm5D;

retain area item ele num_50 symb_50 num_49 symb_49 num_48 symb_48 num_47 symb_47 num_46 symb_46 num_45 symb_45  comm_grp sub_reg reg ; 

set Temp_Rev.geog_itg_subr_31_decm8C (keep = area item ele num_50-num_45 symb_50-symb_45 comm_grp sub_reg reg); run;

proc sort data = Temp_Rev.geog_it_reg_31_decm5D; by item reg  ; run;

data Temp_Rev.geog_it_reg_31_decm6D;
merge Temp_Rev.geog_it_reg_31_decm5D Temp_Rev.geog_it_reg_31_decm4D ;
by item reg ; run;

data Temp_Rev.geog_it_reg_31_decm7D;

format area 6. item 6. ele 6. 
num_50 8.1 symb_50 $ 5.
num_49 8.1 symb_49 $ 5.
num_48 8.1 symb_48 $ 5.
num_47 8.1 symb_47 $ 5.
num_46 8.1 symb_46 $ 5.
num_45 8.1 symb_45 $ 5.

comm_grp $ 15. sub_reg $ 15. reg $ 15.;
/*summ_1989-summ_2009 12.*/
/*ch_sum_1989-ch_sum_2009 12. ;*/

set  Temp_Rev.geog_it_reg_31_decm6D; 

array sym symb_50-symb_45;
array num num_50-num_45;
array ch ch_50-ch_45;

if reg ne "" then do;

do i = 2 to 6;	
		
if sym{i} in ("I") and num{i-1} < 11 then num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
         End; 


do i = 2 to 6;
if ch{i} = 1 then ch{i} = ""; 
if ch{i} > 1.4 then ch{i} = "";
if ch{i} < 0.6 then ch{i} = "";
end;


do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = "" then do; 
		 num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
end;end;

 do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = 0 then do; 
num{i} = "";
end;end;
do i = 2 to 6;

		if num {i} ne '' and sym{i} in ("I") then do ;
sym{i} = "ID"; end; end;

		 end; run;


	/* Validation 2 - to solve the issue of repeated numbers*/

data Temp_Rev.geog_it_reg_31_decm8D; set Temp_Rev.geog_it_reg_31_decm7D; 
array sym symb_50-symb_45;
array num num_50-num_45;
array diff diff_50-diff_45;
array decim d_50-d_45;

do i = 2 to 6;
diff{i} = num {i} - num{i-1} ; diff{i} = round(diff{i}) ;  end; 


do i = 2 to 6;
if diff{i} = 0 then decim{i} = 1 ; 
end;

do i = 2 to 6;
if decim{i} = 1 and sym{i} = "ID" then do;
num{i} = "" ; 
sym{i} = "I"; end; 



if decim{i} = 1 and sym{i-1} = "ID" then DO;
num{i-1} = "" ;
sym{i-1} = "I"; 
end;
end; run;


data Temp_Rev.geog_it_reg_31_decm8D_count;
set Temp_Rev.geog_it_reg_31_decm8D;
id = _N_;  
array num num_50-num_45;
array sym symb_50-symb_45;
array memfound mem_50-mem_45; 
do i=1 to 6;
 if sym{i} = "ID" and num{i} ne "" then do; memfound{i}+1;
 total =Sum(of mem_50-mem_46);
drop i ;  end; end;
run;






/*---------------*/


data Temp_Rev.geog_itg_reg_31_decm1E;
set Temp_Rev.geog_31_decm1A; run;

proc sort data = Temp_Rev.geog_itg_reg_31_decm1E; by comm_grp reg ; run;

/* this file is created mainly for the sum function, to avoid summing the non-required symbol values*/


data Temp_Rev.geog_itg_reg_31_decm2E;
set Temp_Rev.geog_itg_reg_31_decm1E;

	 array num num_50-num_45;
		 array sym symb_50-symb_45;
		 array num_a num_a_50-num_a_45;
		 array num_b num_b_50-num_b_45;

		 do i = 2 to 6;
	
if sym{i} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4" "I5" "IA" "IB" "IC" "ID" "IE")
and sym{i-1} notin ("E" "/" "F" "M" "P" "C" "T" "X" "I" "I1" "I2" "I3" "I4" "I5" "IA" "IB" "IC" "ID" "IE") then do;
num_a{i} = num{i};
num_b{i-1} = num{i-1} ;
 end;end;

		 run;

/* chnage the SORT according to the combination*/

proc sort data = Temp_Rev.geog_itg_reg_31_decm2E; by comm_grp reg ; run;

/* continue with combination SORT*/

proc means data=Temp_Rev.geog_itg_reg_31_decm2E nway noprint; /* nway helps to output the dataset*/
by comm_grp reg  ;
var num_a_50-num_a_45  num_b_50-num_b_45 ;
output out=Temp_Rev.geog_itg_reg_31_decm3E 
sum=summ_a_50-summ_a_45  summ_b_50-summ_b_45;
run;

/* Validation 1*/



data Temp_Rev.geog_itg_reg_31_decm4E ;
set Temp_Rev.geog_itg_reg_31_decm3E  ;


		array summ_a summ_a_50-summ_a_45 ;
		array summ_b summ_b_50-summ_b_45;
		 array ch ch_50-ch_45;
	
do i = 2 to 6;

ch{i} = summ_a{i}/summ_b{i-1}; 
end;


run; 

 data Temp_Rev.geog_itg_reg_31_decm5E;

retain area item ele num_50 symb_50 num_49 symb_49 num_48 symb_48 num_47 symb_47 num_46 symb_46 num_45 symb_45 comm_grp sub_reg reg ; 

set Temp_Rev.geog_it_reg_31_decm8D (keep = area item ele num_50-num_45 symb_50-symb_45 comm_grp sub_reg reg); run;

proc sort data = Temp_Rev.geog_itg_reg_31_decm5E; by comm_grp reg  ; run;

data Temp_Rev.geog_itg_reg_31_decm6E;
merge Temp_Rev.geog_itg_reg_31_decm5E Temp_Rev.geog_itg_reg_31_decm4E ;
by comm_grp reg ; run;

data Temp_Rev.geog_itg_reg_31_decm7E;

format area 6. item 6. ele 6. 
num_50 8.1 symb_50 $ 5.
num_49 8.1 symb_49 $ 5.
num_48 8.1 symb_48 $ 5.
num_47 8.1 symb_47 $ 5.
num_46 8.1 symb_46 $ 5.
num_45 8.1 symb_45 $ 5.

comm_grp $ 15. sub_reg $ 15. reg $ 15.;
/*summ_1989-summ_2009 12.*/
/*ch_sum_1989-ch_sum_2009 12. ;*/

set  Temp_Rev.geog_itg_reg_31_decm6E; 

array sym symb_50-symb_45;
array num num_50-num_45;
array ch ch_50-ch_45;

if reg ne "" then do;

do i = 2 to 6;	
		
if sym{i} in ("I") and num{i-1} < 11 then num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
         End; 


do i = 2 to 6;
if ch{i} = 1 then ch{i} = ""; 
if ch{i} > 1.4 then ch{i} = "";
if ch{i} < 0.6 then ch{i} = "";
end;


do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = "" then do; 
		 num{i} = num{i-1}* ch{i};
			num{i} = round(num{i});
end;end;

 do i = 2 to 6;
	
		 if sym{i} in ("I") and num{i} = 0 then do; 
num{i} = "";
end;end;
do i = 2 to 6;

		if num {i} ne '' and sym{i} in ("I") then do ;
sym{i} = "IE"; end; end;

		 end; run;


	/* Validation 2 - to solve the issue of repeated numbers*/

data Temp_Rev.geog_itg_reg_31_decm8E; set Temp_Rev.geog_itg_reg_31_decm7E; 
array sym symb_50-symb_45;
array num num_50-num_45;
array diff diff_50-diff_45;
array decim d_50-d_45;

do i = 2 to 6;
diff{i} = num {i} - num{i-1} ; diff{i} = round(diff{i}) ;  end; 


do i = 2 to 6;
if diff{i} = 0 then decim{i} = 1 ; 
end;

do i = 2 to 6;
if decim{i} = 1 and sym{i} = "IE" then do;
num{i} = "" ; 
sym{i} = "I"; end; 



if decim{i} = 1 and sym{i-1} = "IE" then DO;
num{i-1} = "" ;
sym{i-1} = "I"; 
end;
end; run;



data Temp_Rev.geog_itg_reg_31_decm8E_count;
set Temp_Rev.geog_itg_reg_31_decm8E;
id = _N_;  
array num num_50-num_45;
array sym symb_50-symb_45;
array memfound mem_50-mem_45; 
do i=1 to 6;
 if sym{i} = "IE" and num{i} ne "" then do; memfound{i}+1;
 total =Sum(of mem_50-mem_46);
drop i ;  end; end;
run;



data Temp_Rev.miss_31_;
set Temp_Rev.geog_itg_reg_31_decm8E; 
id = _N_;  
array num num_50-num_45;
array sym symb_50-symb_45;
array memfound mem_50-mem_45; 
do i=1 to 6;
 if num{i} = "" and sym{i} = "I" then do; 
memfound{i}+1;
 total =Sum(of mem_50-mem_46);
drop i ;  end; end;
run;


data Temp_Rev.geog_itg_reg_31_decm8E_Unreverse ;

retain area item ele num_45 symb_45 num_46 symb_46 num_47 symb_47 num_48 symb_48 num_49 symb_49 num_50 symb_50 ; 

set Temp_Rev.geog_itg_reg_31_decm8E; 

run;





data Temp_Rev.all_changed_to_E_31;
set Temp_Rev.geog_itg_reg_31_decm8E_Unreverse; 

array sym symb_46-symb_50;
array num num_46-num_50;

do i = 1 to 5;
if sym{i} in ( "I1" "I2" "I3" "I4" "I5" "IA" "IB" "IC" "ID" "IE") then sym{i} = "E";

end;
run;


data temp_rev.upload_31a (drop = num_45 symb_45 comm_grp sub_reg reg _type_ _freq_ summ_a_50-summ_a_45 
summ_b_50-summ_b_45 ch_50-ch_45 i diff_50-diff_45 d_50-d_45);
set Temp_Rev.all_changed_to_E_31 ; 

array sym symb_46-symb_50;
array num num_46-num_50;

do i = 1 to 5;

if sym {i} not in ("E") then do;
num{i} = "";
sym {i} = "";
end;end;

if num_46 = "" and num_47 = "" and num_48 = "" and num_49 = "" and num_50 = "" then delete;

run;

data temp_rev.upload_31;

rename area = area item = item ele = element num_48 = num_2009 symb_48 = symb_2009 num_49 = num_2010 symb_49 = symb_2010 num_50 = num_2011 symb_50 = symb_2011;

label area = area item = item ele = element num_48 = num_2009 symb_48 = symb_2009 num_49 = num_2010 symb_49 = symb_2010 num_50 = num_2011 symb_50 = symb_2011;

set temp_rev.upload_31a (drop = num_46 num_47 symb_46 symb_47);

run;




proc export data = temp_rev.upload_31
outfile = "T:\production_10122012\for_upload_ele31.csv"
dbms = csv replace; run;
