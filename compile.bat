@echo off


echo UPRISE.bas
fbc  -s gui -t 10000 -fpu sse  -arch 686 -O 3 -vec 1 UPRISE.bas



echo 1_UPRISE_short.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 1_UPRISE_short.bas



echo 1_UPRISE_view_SNew.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 1_UPRISE_view_SNew.bas

echo 1_UPRISE_view_SOld.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 1_UPRISE_view_SOld.bas

echo 1_UPRISE_view_COld.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 1_UPRISE_view_COld.bas



echo 2_UPRISE_processing_SNew.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 2_UPRISE_processing_SNew.bas

echo 2_UPRISE_processing_SOld.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 2_UPRISE_processing_SOld.bas

echo 2_UPRISE_processing_COld.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 2_UPRISE_processing_COld.bas

echo 2_UPRISE_processing_SNew_Minus.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 2_UPRISE_processing_SNew_Minus.bas


echo 3_UPRISE_estimate.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_estimate.bas

echo 3_UPRISE_estimate_wave.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_estimate_wave.bas

echo 3_UPRISE_estimate_COld.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_estimate_COld.bas

echo 3_UPRISE_estimate_FLIP_te_ti.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_estimate_FLIP_te_ti.bas

echo 3_UPRISE_estimate_FLIP_te.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_estimate_FLIP_te.bas


echo 3_UPRISE_velocity.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_velocity.bas




echo Deleting UPRISE folder...
rmdir /q /s UPRISE

echo Creating UPRISE folders...
mkdir UPRISE
mkdir UPRISE\in
mkdir UPRISE\out

copy UPRISE.exe UPRISE
copy 1_UPRISE_short.exe UPRISE
copy 1_UPRISE_view_SNew.exe UPRISE
copy 1_UPRISE_view_SOld.exe UPRISE
copy 1_UPRISE_view_COld.exe UPRISE
copy 2_UPRISE_processing_SNew.exe UPRISE
copy 2_UPRISE_processing_SOld.exe UPRISE
copy 2_UPRISE_processing_COld.exe UPRISE
copy 2_UPRISE_processing_SNew_Minus.exe UPRISE
copy 3_UPRISE_estimate.exe UPRISE
copy 3_UPRISE_estimate_wave.exe UPRISE
copy 3_UPRISE_estimate_COld.exe UPRISE
copy 3_UPRISE_estimate_FLIP_te_ti.exe UPRISE
copy 3_UPRISE_estimate_FLIP_te.exe UPRISE
copy 3_UPRISE_velocity.exe UPRISE

copy albom.dll UPRISE
copy rz291014.dat UPRISE
copy razr4v.dat UPRISE
copy filter.dat UPRISE
copy filter_COld.dat UPRISE
copy config.dat UPRISE
copy config_wave.dat UPRISE
copy config_velocity.dat UPRISE
copy config_short.dat UPRISE
copy config_COld.dat UPRISE

xcopy ambig UPRISE\ambig /e /i /h

del help\uprise.chm
hhc help\uprise.hhp
copy help\uprise.chm UPRISE

copy Report.xls UPRISE

pause
