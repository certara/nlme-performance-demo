# nlme-performance-demo

Repository containing scripts presented at public conferences for NLME performance testing and benchmarking. The public benchmarks are limited to the finite difference implementation of NLME.

A NONMEM and NLME license are required. It is assumed the nmfe75.bat is in the system path.

A free 30 day NLME license is available, contact support\@certara.com

To execute the ODE-Michaelis-Menten benchmarks

clone repo into a folder
reboot the computer if desired for a consistent environment
Open R Studio
Navigate to the sub folder "ODE-Michaelis-Menten" in the installed folder
Execute the nlme benchmark (RunNLME.R)
reboot the computer if desired for a consistent environment
Navigate back to the sub folder "ODE-Michaelis-Menten" in the installed folder
Execute the NONMEM benchmark (RunNONMEM.R)
Navigate back to the sub folder "ODE-Michaelis-Menten" in the installed folder
Execute combine.R for plots and statistics
