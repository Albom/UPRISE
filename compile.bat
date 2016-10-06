@echo off
echo 01_UPRISE_view.bas
fbc -s console -t 10000 -fpu sse  -arch 686 01_UPRISE_view.bas
echo 02_UPRISE_integrate.bas
fbc -s console -t 10000 -fpu sse  -arch 686 02_UPRISE_integrate.bas
echo 03_UPRISE_approximate_polynomial_1.bas
fbc -s console -t 10000 -fpu sse  -arch 686 03_UPRISE_approximate_polynomial_1.bas
echo 03_UPRISE_approximate_trapezoidal_1.bas
fbc -s console -t 10000 -fpu sse  -arch 686 03_UPRISE_approximate_trapezoidal_1.bas
echo 04_UPRISE_estimate_grad.bas
fbc -s console -t 10000 -fpu sse  -arch 686 04_UPRISE_estimate_grad.bas
echo UPRISE.bas
fbc -s gui     -t 10000 -fpu sse  -arch 686 UPRISE.bas
pause
