@echo off
echo 01_UPRISE_view.bas
fbc -s console -t 10000 -fpu sse  -arch 686 01_UPRISE_view.bas
echo 02_UPRISE_spline.bas
fbc -s console -t 10000 -fpu sse  -arch 686 02_UPRISE_spline.bas
echo 03_UPRISE_integrate.bas
fbc -s console -t 10000 -fpu sse  -arch 686 03_UPRISE_integrate.bas
echo 04_UPRISE_approximate_polynomial.bas
fbc -s console -t 10000 -fpu sse  -arch 686 04_UPRISE_approximate_polynomial.bas
echo 04_UPRISE_approximate_trapezoidal.bas
fbc -s console -t 10000 -fpu sse  -arch 686 04_UPRISE_approximate_trapezoidal.bas
echo 05_UPRISE_estimate.bas
fbc -s console -t 10000 -fpu sse  -arch 686 05_UPRISE_estimate.bas
echo 05_UPRISE_velocity.bas
fbc -s console -t 10000 -fpu sse  -arch 686 05_UPRISE_velocity.bas
echo UPRISE.bas
fbc -s gui     -t 10000 -fpu sse  -arch 686 UPRISE.bas
pause
