//version 1.04 in effect 9/1/06
//version 2.01 in effect 6/3/06
//version 2.02 in effect 11/9/07
#define RSAT_VERSION 2.02

//Example flags

//whether to verify the solution after solving the instance
#define VERIFY_SOLUTION 0
//whether to terminate execution after timeout limit
#define TIME_OUT 1
//whether to occasionally pick a decision variable randomly (see select_variable in solver.cpp).
#define USE_RANDOM_ORDER 0

#define ALLOW_PARTIAL_ORDER 1 //allow options that specify partial ordering among variables
