#ifndef __INCLUDE_NGN_OPTIONS__
#define __INCLUDE_NGN_OPTIONS__

#include <string>

//NGN名前空間始まり
_NGN_BEGIN


struct Options {
public:
	std::string targetfile;
	std::string outfile;
	std::string tmpfile;
	
	bool no_stdout;
	
	bool no_logfile;
	
	Options() {
		no_stdout = false;
		no_logfile = false;
	}
};


//NGN名前空間終わり
_NGN_END

#endif	//__INCLUDE_NGN_OPTIONS__
