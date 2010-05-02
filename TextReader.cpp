#include <iostream>
#include <fstream>
#include <string>

#include "ngn.h"
#include "debug.h"
#include "TextReader.h"

//NGN���O��Ԏn�܂�
_NGN_BEGIN


TextReader::TextReader(std::string& f) {
	dbgPrint1("[dbg]TextReader constructor\n");
	
	filename = f;
	infile.open(f.c_str());
	
	if (infile.fail()) {
		std::cerr << "cannot open file \"" << filename << "\"" << std::endl;
		return ;
	}
}

TextReader::~TextReader() {
	
}

void TextReader::getline() {
	std::string str;
	std::getline(infile, str);
	std::cout << str << std::endl;
}



//NGN���O��ԏI���
_NGN_END

