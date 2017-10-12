@echo off

echo Cleaning...
call clean.bat

echo =================================================================
echo Compiling...

echo UPRISE.bas
fbc  -s gui -t 10000 -fpu sse  -arch 686 -O 3 -vec 1 UPRISE.bas
IF %errorlevel% equ 1 Goto :Error



echo 1_UPRISE_short.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 1_UPRISE_short.bas
IF %errorlevel% equ 1 Goto :Error



echo 1_UPRISE_view_SNew.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 1_UPRISE_view_SNew.bas
IF %errorlevel% equ 1 Goto :Error

echo 1_UPRISE_view_SOld.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 1_UPRISE_view_SOld.bas
IF %errorlevel% equ 1 Goto :Error

echo 1_UPRISE_view_COld.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 1_UPRISE_view_COld.bas
IF %errorlevel% equ 1 Goto :Error

echo 2_UPRISE_processing_SNew.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 2_UPRISE_processing_SNew.bas
IF %errorlevel% equ 1 Goto :Error

echo 2_UPRISE_processing_SOld.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 2_UPRISE_processing_SOld.bas
IF %errorlevel% equ 1 Goto :Error

echo 2_UPRISE_processing_COld.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 2_UPRISE_processing_COld.bas
IF %errorlevel% equ 1 Goto :Error

echo 2_UPRISE_processing_SNew_Minus.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 2_UPRISE_processing_SNew_Minus.bas
IF %errorlevel% equ 1 Goto :Error

echo 3_UPRISE_estimate.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_estimate.bas
IF %errorlevel% equ 1 Goto :Error

echo 3_UPRISE_estimate_wave.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_estimate_wave.bas
IF %errorlevel% equ 1 Goto :Error

echo 3_UPRISE_estimate_COld.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_estimate_COld.bas
IF %errorlevel% equ 1 Goto :Error

echo 3_UPRISE_estimate_FLIP_te_ti.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_estimate_FLIP_te_ti.bas
IF %errorlevel% equ 1 Goto :Error

echo 3_UPRISE_estimate_FLIP_te.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_estimate_FLIP_te.bas
IF %errorlevel% equ 1 Goto :Error

echo 3_UPRISE_estimate_FLIP_te_storm.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_estimate_FLIP_te_storm.bas
IF %errorlevel% equ 1 Goto :Error

echo 3_UPRISE_estimate_storm.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_estimate_storm.bas
IF %errorlevel% equ 1 Goto :Error

echo 3_UPRISE_estimate_newton.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_estimate_newton.bas
IF %errorlevel% equ 1 Goto :Error

echo 3_UPRISE_velocity.bas
fbc -s console -t 10000 -fpu sse  -arch 686  -O 3 -vec 1 3_UPRISE_velocity.bas
IF %errorlevel% equ 1 Goto :Error

echo =================================================================
echo Creating UPRISE folders...
mkdir UPRISE
mkdir UPRISE\in
mkdir UPRISE\out

echo =================================================================
echo Copying files...

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
copy 3_UPRISE_estimate_FLIP_te_storm.exe UPRISE
copy 3_UPRISE_estimate_storm.exe UPRISE
copy 3_UPRISE_estimate_newton.exe UPRISE
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
copy config_screen.dat UPRISE

xcopy ambig UPRISE\ambig /e /i /h

copy Report.xls UPRISE

echo =================================================================
echo Help file generating...
hhc help\uprise.hhp
move help\uprise.chm UPRISE\urpise.chm

echo =================================================================
echo =================================================================
echo SUCCESS.
pause
exit

:Error
echo ERROR.
pause
