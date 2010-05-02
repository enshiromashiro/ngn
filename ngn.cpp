/************************************************/
/* ngn - Novel page GeNerator                   */
/* author: subaru                               */
/*                                              */
/* usage:                                       */
/*	ngn	[TARGET] [-o OUTFILE] [-t TMPFILE]      */
/*		[--template-marker TITLE TEXT AFTER]    */
/*		[--target-marker TITLE TEXT AFTER]      */
/*		[--no-stdout]                           */
/************************************************/

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <fstream>

#include "ngn.h"
#include "debug.h"
#include "Options.h"


//オプションが入ってるよー
ngn::Options* option;


void initialize() {
	option = new ngn::Options();
}

void finalize() {
	delete option;
}


bool argumentCheck(int argc, char* argv[]) {
	int count = 1;
	bool noerr = true;
	
	dbgPrint2("[dbg]argchk(). argc: %d\n", argc);
	
	if (argc <= 1) {
		std::cerr << "ngn: no input file" << std::endl;
		noerr = false;
		
	} else {
		while (count <= argc) {
			char* ch = &argv[count][0];
			if (*ch == '-') {
				switch (*(ch+1)) {
				case 'o':
					if (argc > count + 1) {
						option->outfile = std::string(argv[++count]);
					} else {
						std::cerr << "ngn: no argument of output file." << std::endl;
						noerr = false;
						count += 20;
					}
					break;
					
				case 't':
					if (argc > count + 1) {
						option->tmpfile = std::string(argv[++count]);
					} else {
						std::cerr << "ngn: no argument of template file." << std::endl;
						noerr = false;
						count += 20;
					}
					break;
				
				case '-':
					if (0 == strcmp(ch+2, "no-stdout")) {
						option->no_stdout = true;
						
					} else if (0 == strcmp(ch+2, "tmplate-marker")) {
						std::cerr << "tmppppppp" << std::endl;
						
					} else if (0 == strcmp(ch+2, "targget-marker")) {
						std::cerr << "targetttttt" << std::endl;
						
					} else if (0 == strcmp(ch+2, "help")) {
						std::cerr << ngn::usage << std::endl;
						
					} else {
						std::cerr << "ngn: unknown option" << std::endl;
						noerr = false;
						count += 20;
					}
					break;
				
				default:
					std::cerr << "ngn: unknown option" << std::endl;
					noerr = false;
					count += 20;
					break;
				}
				
			} else {
				option->targetfile = std::string(argv[count]);
			}
			
			count++;
		}
	}
	return noerr;
}


int main(int argc, char* argv[]) {

	std::cout << ngn::ngn << std::endl;
	
	//初期化処理
	initialize();
	
	//引数チェック
	if (false == argumentCheck(argc, argv)) {
		std::cerr << "ngn halted." << std::endl;
		exit(EXIT_FAILURE);
	}
	
	//マーカーを作るぜ
	
	//終了処理
	finalize();
	
	return EXIT_SUCCESS;
}




