@echo off
echo 1_UPRISE_short.bas
fbc -s console -t 10000 -fpu sse  -arch 686 1_UPRISE_short.bas
echo 1_UPRISE_view_SNew.bas
fbc -s console -t 10000 -fpu sse  -arch 686 1_UPRISE_view_SNew.bas
echo 1_UPRISE_view_SOld.bas
fbc -s console -t 10000 -fpu sse  -arch 686 1_UPRISE_view_SOld.bas
echo 1_UPRISE_view_COld.bas
fbc -s console -t 10000 -fpu sse  -arch 686 1_UPRISE_view_COld.bas
echo 2_UPRISE_integrate_SNew.bas
fbc -s console -t 10000 -fpu sse  -arch 686 2_UPRISE_integrate_SNew.bas
echo 2_UPRISE_integrate_SOld.bas
fbc -s console -t 10000 -fpu sse  -arch 686 2_UPRISE_integrate_SOld.bas
echo 2_UPRISE_integrate_COld.bas
fbc -s console -t 10000 -fpu sse  -arch 686 2_UPRISE_integrate_COld.bas
echo 3_UPRISE_approximate_polynomial.bas
fbc -s console -t 10000 -fpu sse  -arch 686 3_UPRISE_approximate_polynomial.bas
echo 3_UPRISE_approximate_trapezoidal.bas
fbc -s console -t 10000 -fpu sse  -arch 686 3_UPRISE_approximate_trapezoidal.bas
echo 3_UPRISE_approximate_COld.bas
fbc -s console -t 10000 -fpu sse  -arch 686 3_UPRISE_approximate_COld.bas
echo 4_UPRISE_estimate_grad.bas
fbc -s console -t 10000 -fpu sse  -arch 686 4_UPRISE_estimate_grad.bas
echo 4_UPRISE_estimate_grad_COld.bas
fbc -s console -t 10000 -fpu sse  -arch 686 4_UPRISE_estimate_grad_COld.bas
echo 4_UPRISE_velocity.bas
fbc -s console -t 10000 -fpu sse  -arch 686 4_UPRISE_velocity.bas
echo UPRISE.bas
fbc -s gui     -t 10000 -fpu sse  -arch 686 UPRISE.bas

rmdir /q  /s UPRISE 

mkdir UPRISE
mkdir UPRISE\in
mkdir UPRISE\out

copy 1_UPRISE_short.exe UPRISE
copy 1_UPRISE_view_SNew.exe UPRISE
copy 1_UPRISE_view_SOld.exe UPRISE
copy 1_UPRISE_view_COld.exe UPRISE
copy 2_UPRISE_integrate_SNew.exe UPRISE
copy 2_UPRISE_integrate_SOld.exe UPRISE
copy 2_UPRISE_integrate_COld.exe UPRISE
copy 3_UPRISE_approximate_polynomial.exe UPRISE
copy 3_UPRISE_approximate_trapezoidal.exe UPRISE
copy 3_UPRISE_approximate_COld.exe UPRISE
copy 4_UPRISE_estimate_grad.exe UPRISE
copy 4_UPRISE_estimate_grad_COld.exe UPRISE
copy 4_UPRISE_velocity.exe UPRISE
copy UPRISE.exe UPRISE

copy albom.dll UPRISE
copy rz240912.dat UPRISE
copy filter.dat UPRISE
copy filter_COld.dat UPRISE
copy config.dat UPRISE
copy config_COld.dat UPRISE
copy config_short.dat UPRISE
copy report.xls UPRISE

xcopy ambig UPRISE\ambig /e /i /h

pause
