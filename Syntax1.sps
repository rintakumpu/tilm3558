SET SEED=63555.
DATASET ACTIVATE DataSet1.
USE ALL.
do if $casenum=1.
compute #s_$_1=800.
compute #s_$_2=1318.
end if.
do if  #s_$_2 > 0.
compute filter_$=uniform(1)* #s_$_2 < #s_$_1.
compute #s_$_1=#s_$_1 - filter_$.
compute #s_$_2=#s_$_2 - 1.
else.
compute filter_$=0.
end if.
VARIABLE LABELS filter_$ '800 from the first 1318 cases (SAMPLE)'.
FORMATS filter_$ (f1.0).
FILTER  BY filter_$.
EXECUTE.
DATASET ACTIVATE DataSet1.

USE ALL.
COMPUTE filter_$=(filter_$=1).
VARIABLE LABELS filter_$ 'filter_$=1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

HILOGLINEAR q23(1 3) k23(1 2) d2(1 2) 
  /METHOD=BACKWARD
  /CRITERIA MAXSTEPS(10) P(.05) ITERATION(20) DELTA(.5)
  /PRINT=FREQ RESID
  /DESIGN.
