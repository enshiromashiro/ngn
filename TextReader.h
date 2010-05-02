#ifndef __INCLUDE_NGN_TEXTREADER__
#define __INCLUDE_NGN_TEXTREADER__


#include <fstream>
#include <string>

#include "ngn.h"


//NGN名前空間始まり
_NGN_BEGIN

class TextReader {
public:
	
	std::string filename;
	
	void getline();
	
	TextReader(std::string& f);
	~TextReader();

private:
	
	std::ifstream infile;
};


//NGN名前空間終わり
_NGN_END


#endif	//__INCLUDE_NGN_TEXTREADER__
