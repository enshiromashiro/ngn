#ifndef __INCLUDE_NGN_TEXTREADER__
#define __INCLUDE_NGN_TEXTREADER__


#include <fstream>
#include <string>

#include "ngn.h"


//NGN���O��Ԏn�܂�
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


//NGN���O��ԏI���
_NGN_END


#endif	//__INCLUDE_NGN_TEXTREADER__
