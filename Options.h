#ifndef __INCLUDE_NGN_OPTIONS__
#define __INCLUDE_NGN_OPTIONS__

#include <string>

//NGN���O��Ԏn�܂�
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


//NGN���O��ԏI���
_NGN_END

#endif	//__INCLUDE_NGN_OPTIONS__
