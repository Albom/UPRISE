@echo off
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
echo 3_UPRISE_approximate_polynomial.bas
fbc -s console -t 10000 -fpu sse  -arch 686 3_UPRISE_approximate_polynomial.bas
echo 3_UPRISE_approximate_trapezoidal.bas
fbc -s console -t 10000 -fpu sse  -arch 686 3_UPRISE_approximate_trapezoidal.bas
echo 4_UPRISE_estimate_grad.bas
fbc -s console -t 10000 -fpu sse  -arch 686 4_UPRISE_estimate_grad.bas
echo 4_UPRISE_velocity.bas
fbc -s console -t 10000 -fpu sse  -arch 686 4_UPRISE_velocity.bas
echo UPRISE.bas
fbc -s gui     -t 10000 -fpu sse  -arch 686 UPRISE.bas
pause
