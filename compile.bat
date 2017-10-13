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
echo Moving and copying files...

move UPRISE.exe UPRISE\UPRISE.exe 
move 1_UPRISE_short.exe UPRISE\1_UPRISE_short.exe
move 1_UPRISE_view_SNew.exe UPRISE\1_UPRISE_view_SNew.exe
move 1_UPRISE_view_SOld.exe UPRISE\1_UPRISE_view_SOld.exe
move 1_UPRISE_view_COld.exe UPRISE\1_UPRISE_view_COld.exe
move 2_UPRISE_processing_SNew.exe UPRISE\2_UPRISE_processing_SNew.exe
move 2_UPRISE_processing_SOld.exe UPRISE\2_UPRISE_processing_SOld.exe
move 2_UPRISE_processing_COld.exe UPRISE\2_UPRISE_processing_COld.exe
move 2_UPRISE_processing_SNew_Minus.exe UPRISE\2_UPRISE_processing_SNew_Minus.exe
move 3_UPRISE_estimate.exe UPRISE\3_UPRISE_estimate.exe
move 3_UPRISE_estimate_wave.exe UPRISE\3_UPRISE_estimate_wave.exe
move 3_UPRISE_estimate_COld.exe UPRISE\3_UPRISE_estimate_COld.exe
move 3_UPRISE_estimate_FLIP_te_ti.exe UPRISE\3_UPRISE_estimate_FLIP_te_ti.exe
move 3_UPRISE_estimate_FLIP_te.exe UPRISE\3_UPRISE_estimate_FLIP_te.exe
move 3_UPRISE_estimate_FLIP_te_storm.exe UPRISE\3_UPRISE_estimate_FLIP_te_storm.exe
move 3_UPRISE_estimate_storm.exe UPRISE\3_UPRISE_estimate_storm.exe
move 3_UPRISE_estimate_newton.exe UPRISE\3_UPRISE_estimate_newton.exe
move 3_UPRISE_velocity.exe UPRISE\3_UPRISE_velocity.exe

copy albom.dll UPRISE
copy rz291014.dat UPRISE
copy razr4v.dat UPRISE
copy filter.dat UPRISE
copy filter_COld.dat UPRISE

xcopy config UPRISE\config /e /i /h
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
