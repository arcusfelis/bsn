Motivation
==========

Measuring an efficiently of different variants of conflicts resolving in the hashing store.
Results (Count of steps).
all is miss and in together.

```erlang
1> bsn_measure:check_type(ext, 100, 0.5).   
[{0.1,{miss,1.0},{in,1.0},{all,1.0}},
 {0.25,{miss,1.7},{in,1.56},{all,1.636}},
 {0.5,{miss,2.328},{in,2.28},{all,2.306}},
 {0.75,{miss,2.408},{in,2.293},{all,2.351}},
 {0.9,{miss,3.063},{in,2.378},{all,2.73}},
 {1,{miss,3.056},{in,2.78},{all,2.91}}]

2> bsn_measure:check_type(int_linear, 100, 0.5).
[{0.1,{miss,2.0},{in,1.4},{all,1.75}},
 {0.25,{miss,12.867},{in,5.44},{all,9.491}},
 {0.5,{miss,31.606},{in,12.84},{all,20.301}},
 {0.75,{miss,32.829},{in,13.653},{all,22.91}},
 {0.9,{miss,37.168},{in,15.889},{all,26.816}},
 {1,{miss,100.0},{in,19.13},{all,58.74}}]

3> bsn_measure:check_type(int_quadric, 100, 0.5).
[{0.1,{miss,1.0},{in,1.0},{all,1.0}},
 {0.25,{miss,4.069},{in,2.12},{all,3.167}},
 {0.5,{miss,6.788},{in,3.44},{all,5.147}},
 {0.75,{miss,8.442},{in,4.093},{all,5.874}},
 {0.9,{miss,12.262},{in,4.678},{all,9.042}},
 {1,{miss,70.268},{in,5.357},{all,37.646}}]
```


