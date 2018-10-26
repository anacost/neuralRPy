# neuralRPy
neural network example 


Two examples of neural network using data to predict net surface radiation.
- One notebook in R using neuralnet:
[R jupyter notebook](../master/neuralnet_sensitivity_met_data.ipynb)

- Other in python with tensorflow:
[python jupyter notebook](../master/tensorflowPy.ipynb)


Estimating net surface radiation by using ANN

[sensitivity_met_data.txt](../master/data/sensitivity_met_data.txt):
1. X - date and time: 2017-08-03 00:00:00 to 2017-09-08 23:30:00
2. Rl_downwell - Longwave incoming radiation in W/m^2
3. AT_mbar - Atmospheric pressure in mbar
4. Rs_downwell - Shortwave incoming radiation in W/m^2
5. rH - Relative humidity in %
6. T_b_1477 - Air temperature in degree Celsius at 10m height
7. D_g_1477 - Wind direction in degree
8. F_1_s_g_1477 - Wind speed in m/s
9. Rnet - Net surface radiation in W/m^2


The goal is to estimate Rnet (net surface radiation). 
