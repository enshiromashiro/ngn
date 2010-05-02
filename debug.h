#ifndef __INCLUDE_DEBUG__
#define __INCLUDE_DEBUG__


#ifdef DEBUG_MODE
#include <cstdio>

#define dbgPrint1(a) printf(a)
#define dbgPrint2(a, b) printf(a, b)
#define dbgPrint3(a, b, c) printf(a, b, c)
#define dbgPrint4(a, b, c, d) printf(a, b, c, d)

#else

#define dbgPrint1(a)  
#define dbgPrint2(a, b)  
#define dbgPrint3(a, b, c)  
#define dbgPrint4(a, b, c, d)  

#endif



#endif	//__INCLUDE_DEBUG__
