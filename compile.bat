@echo off

echo 1_UPRISE_view_SNew.bas
fbc -s console -t 10000 -fpu sse  -arch 686 1_UPRISE_view_SNew.bas

echo 1_UPRISE_view_SOld.bas
fbc -s console -t 10000 -fpu sse  -arch 686 1_UPRISE_view_SOld.bas

echo 2_UPRISE_processing_SNew.bas
fbc -s console -t 10000 -fpu sse  -arch 686 2_UPRISE_processing_SNew.bas

echo 2_UPRISE_processing_SOld.bas
fbc -s console -t 10000 -fpu sse  -arch 686 2_UPRISE_processing_SOld.bas

echo 3_UPRISE_estimate.bas
fbc -s console -t 10000 -fpu sse  -arch 686 3_UPRISE_estimate.bas

echo 3_UPRISE_estimate_wave.bas
fbc -s console -t 10000 -fpu sse  -arch 686 3_UPRISE_estimate_wave.bas

echo 3_UPRISE_velocity.bas
fbc -s console -t 10000 -fpu sse  -arch 686 3_UPRISE_velocity.bas

echo 3_UPRISE_short.bas
fbc -s console -t 10000 -fpu sse  -arch 686 3_UPRISE_short.bas

echo 3_UPRISE_velocity.bas
fbc -s console -t 10000 -fpu sse  -arch 686 3_UPRISE_velocity.bas

echo Deleting UPRISE folder...
rmdir /q /s UPRISE

echo Creating UPRISE folders...
mkdir UPRISE
mkdir UPRISE\in
mkdir UPRISE\out

copy 1_UPRISE_view_SNew.exe UPRISE
copy 1_UPRISE_view_SOld.exe UPRISE
copy 2_UPRISE_processing_SNew.exe UPRISE
copy 2_UPRISE_processing_SOld.exe UPRISE
copy 3_UPRISE_estimate.exe UPRISE
copy 3_UPRISE_estimate_wave.exe UPRISE
copy 3_UPRISE_short.exe UPRISE
copy 3_UPRISE_velocity.exe UPRISE

copy albom.dll UPRISE
copy rz240912.dat UPRISE
copy filter.dat UPRISE
copy config.dat UPRISE
copy config_wave.dat UPRISE

xcopy ambig UPRISE\ambig /e /i /h

pause
