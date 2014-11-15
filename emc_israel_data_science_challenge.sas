********************************************************************************; 
* VARIOUS SAS ROUTINES FOR EMC ISRAEL CHALLENGE DATA:                          *;
* READ IN DATA FROM CSR FORMAT TO SAS COO FORMAT                               *;
* EXPAND SAS COO TO TRADITIONAL SAS SET                                        *;
* NAIVE BAYES CLASSIFIER                                                       *;
* MULTICLASS LOGARITHMIC LOSS                                                  *;
* PATRICK.HALL@SAS.COM                                                         *;
********************************************************************************;

*** IMPORT TRAINING DATA TO CREATE COO FORMAT SAS DATA SET *********************; 
*** FILL IN PATH; 
%let train_file= ; /* CSR FILE */
data values columns row_offsets; 						
	 infile "&train_file" recfm= f lrecl= 1 end= eof; 
	 length accum $32; 
	 retain accum ' ';                         *** TEXT VARIABLE FOR INPUT VALUES;
	 retain recnum 1;                          *** LINE OF INPUT FILE; 
	 input x $char1.;                          *** PLACEHOLDER FOR ALL INPUT VALUES; 
	 if x='0d'x then return;                   
	 delim= x in (',' '0a'x);  
	 if not delim then accum= trimn(accum)||x; *** IF IT’S NOT A DELIMITER IT'S A VALUE;  
	 if not delim and not eof then return;     *** IF IT'S NOT EOF OR A DELIMITER, CONTINUE; 
	 nvalues+1;                                *** INCREMENT NUMBER OF NON-ZERO VALUES; 
	 value= input(accum,best32.);              *** CONVERT ACCUM TO NUMERIC VALUE; 
	 accum= ' ';                               *** RESET TEXT VARIABLE FOR NEXT VALUE OF X; 
	 if nvalues<10 then put recnum= value=;    *** OUTPUT A SMALL SAMPLE OF VALUES FOR LOG; 
	 if recnum= 1 then do;                     *** SPECIAL CASE FOR FIRST ROW OF INPUT FILE; 
	    if nvalues= 1 then call symputx('nrows',value); 
	    if nvalues= 2 then call symputx('ncols',value); 
	 end;
	 else if recnum= 2 then output values;     *** SAS DATA SET FOR NON-ZERO VALUES; 
	 else if recnum= 3 then output columns;    *** SAS DATA SET FOR COLUMN INDEX; 
	 else if recnum= 4 then output row_offsets;*** SAS DATA SET FOR ROW POINTER; 
	 if x='0a'x or eof then do;                *** TRUE CARRIAGE RETURN OR EOF, PRINT TO LOG; 
	    put recnum= nvalues=; 
	    recnum+1;                              *** INCREMENT TO NEXT INPUT LINE; 
	    nvalues= 0;                            *** RESET NVALUES; 
	 end;
	 keep value;                               *** KEEP VALUES, NOT TEMP VARS; 
run;

*** CREATE A COO FORMAT TABLE; 
*** CONTAINS THE ROW NUMBER, COLUMN NUMBER AND VALUE;  
*** ALL INFORMATION NECESSARY TO BUILD FULL TRAINING MATRIX OR JUST SELECTED FEATURES; 
data final_coo(keep= rownum colnum value); 
    set row_offsets(firstobs= 2) end= eof;        *** 2ND OBS IN ROW_OFFSETS TELLS WHERE ...;   
    retain prev 0;                                *** TO STOP THE FIRST ROW IN FINAL; 
    retain min_colnum 1e50 max_colnum 0; 
    rownum+1;                                     *** INITIALIZE ROWNUM TO ONE;
    count= value-prev;                            *** INDEX FOR END OF ROW;
    prev = value;                                 *** INDEX FOR START OF ROW; 
    do i=1 to count; 
       set values;                                *** GET MATRIX VALUE;  
       set columns (rename= (value= colnum));     *** GET COLUMN NUMBER; 
       min_colnum= min(min_colnum, colnum); 
       max_colnum= max(max_colnum, colnum); 
       output; 
    end;
    if eof then put _n_= min_colnum= max_colnum= "nrows=&nrows. ncols=&ncols."; 
run;

*** EXPAND TO FULL (OR PARTIAL) TRAINING SET *********************************;

*** IMPORT TRAINING LABELS;
*** FILL IN PATH; 
%let label_file= ;
data target;	
	length hkey 8;	
	hkey= _n_;
	infile "&label_file" delimiter= ',' missover dsd lrecl= 32767 firstobs= 1;
	informat target best32. ;
	format target best12. ;
	input target;
	if _n_ <= 10 then put hkey= target=; 
run;

*** EXPAND SUMMARY SET INTO FULL TRAINING MATRIX;  
*** THIS WILL TAKE SOME TIME (LIKE MAYBE DAYS ... );
*** AND DISK SPACE (~800 GB ...); 
*** BUILD THE FIRST 1000 LINES AND DO SOME BENCHMARKING WITH DIFFERENT ...; 
*** BUFNO, BUFSIZE, CATCACHE, AND COMPRESS OPTIONS; 
*** DO NOT ATTEMPT TO VIEW THE FULL (~800 GB, ~600K COLUMNS) TABLE IN THE GUI!!; 

*** REMEMBER, IN THE PAPER THIS WAS JUST AN EXAMPLE ABOUT A BIG CHUNK OF DATA ...; 
*** TO AVOID HAVING TO EXPAND THAT ENTIRE BIG CHUNK OF DATA ...;  
*** YOU CAN USE THE COO SET TO FIND THE COLUMNS YOU LIKE BEST ...; 
*** SOMETHING LIKE ...; 
/*proc sort */
/*	data= final_coo */
/*	out= _200highestTokenCount*/
/*	sortsize= MAX; */
/*	by colnum; */
/*run;*/
/*data _200highestTokenCount (keep= colnum count); */
/*	set _200highestTokenCount (keep= colnum); */
/*	by colnum; */
/*	retain count 0;*/
/*	count+1;*/
/*	if last.colnum then do;*/
/*		output; */
/*		count= 0; 		*/
/*	end; 	*/
/*run; */
/*proc sort */
/*	data= _200highestTokenCount*/
/*	out= _200highestTokenCount */
/*	sortsize= MAX; */
/*	by descending count; */
/*run;*/
/*data _200highestTokenCount; */
/*	set _200highestTokenCount(obs= 200); */
/*run; */
/*proc sql noprint; */
/*	select colnum into :selected_feature_names separated by ' token'*/
/*	from _200highestTokenCount*/
/*	order by colnum;*/
/*	select colnum into :selected_feature_values separated by ', '*/
/*	from _200highestTokenCount*/
/*	order by colnum; */
/*quit; */
/*%let selected_feature_names= token&selected_feature_names;*/
/*%put &selected_feature_names;*/
/*%put &selected_feature_values; */
/*data emcIsrael200; */
/*	set final_coo; */
/*	by rownum; */
/*	array tokens {200} &selected_feature_names;  */
/*	array lookup {200} (&selected_feature_values); */
/*	retain tokens; */
/*	do i= 1 to 200;*/
/*			if lookup{i}= colnum then tokens{i}= value;   */
/*			if tokens{i}= . then tokens{i}= 0; */
/*	end;*/
/*	keep rownum &selected_feature_names;  */
/*	if last.rownum then do; */
/*		output; */
/*		if mod(rownum, 10000)= 0 then putlog 'NOTE: currently processing record ' rownum ' ...'; */
/*		do j= 1 to 200; */
/*			tokens{j}=.;*/
/*		end;*/
/*	end; */
/*run; */
*** YOU WILL NEED TO ADJUST &NCOLS AND THE VAR NAMES IN THE CODE BELOW ACCORDINGLY; 
*** YOU CAN ALSO TRY THE HPTMINE PROCEDURE ...;

data emcIsraelFull; 
	set final_coo; 
	by rownum; 
	array tokens {&ncols} token1-token&ncols;      *** CREATE FULL NUMBER OF COLUMNS;  
	retain tokens;				     
	do i= 1 to &ncols;                             *** POPULATE ARRAY WITH EXPANDED VALUES; 
   		if i= (colnum+1) then tokens{i}= value;*** COLNUM STARTS AT 0; 
   		if tokens{i}= . then tokens{i}= 0; 
	end;
	keep rownum token1-token&ncols;  
	if last.rownum then do;  
	   output;                                     *** OUTPUT ONE ROW FOR EACH SET OF ROWNUMS; 
	   if mod(rownum, 1000)= 0 then putlog 'NOTE: currently processing record ' rownum;  
	   do j = 1 to &ncols;                         *** REINITIALIZE ARRAY; 
	      tokens{j}= .;
	   end;
	end; 
run; 

*** MERGE LABELS WITH HASH; 
data emcIsraelFull;
	declare hash h();
	length hkey target 8;                      *** DEFINE HASH;
	h.defineKey("hkey");
	h.defineData("target");
	h.defineDone();
	do until(eof1);                            *** FILL WITH TARGET SET; 
	   set target end= eof1; 
	   rc1= h.add();	
	   if rc1 then do; 
	      putlog 'ERROR: Target not found for line ' _n_=;
	      abort;
	   end;
	end;
	do until(eof2);                            *** EXECUTE MERGE; 
	   set emcIsraelFull (rename= (rownum= hkey)) end= eof2; 
	   rc2= h.find();
	   if rc2 then do; 
	      putlog 'ERROR: Target not found for line ' _n_=;
	      abort;
	   end;
	   output; 
	end;
	keep hkey target token1-token&ncols;
run;

*** APPEND TRAINING EXAMPLES THAT ARE ALL ZEROS; 
data missing; 
	merge target(rename= (hkey= rownum) in= a) final_coo(in= b); 
	by rownum; 
	if a and ^b; 
	keep rownum target; 
run;
data missing; 
	set missing; 
	array tokens token1-token&ncols (&ncols*0);
	do i= 1 to dim(tokens); 
		if tokens{i}= . then abort;
	end; 
	drop i; 
run;
proc append base= emcIsraelFull data= missing (rename= (rownum= hkey)); run;

*** BUILD A NAIVE BAYES CLASSIFIER *******************************************;
*** FILL IN GRID INFO; 
proc hpbnet data= emcIsraelFull structure= naive maxparents= 2 
	indeptest= mi mialpha= 0.05; /* INCREASE THIS THRESHOLD IF YOU ARE RUNNING OUT OF MEMORY */
	target target;
	input token:;
	output pred= emcIsraelFullPred 
	       varlevel= emcIsraelFullVarLev
	       varselect= emcIsraelFullVarSel;
	performance commit= 10000 nodes=  host= "" install= "";	/* FILL IN GRID INFO */  
run;

*** OUTPUT MULTI-CLASS LOGARITHMIC LOSS TO LOG *******************************; 
data _null_; 
	set emcIsraelFullPred end= eof;  
	array posteriorProbs p_:; 
	retain logloss 0;
	logloss + log(posteriorProbs[(96-target)+1]); 
	if eof then do; 
		logloss= (-1*logloss)/_n_; 
		put logloss= ; 
	end; 
run;   
