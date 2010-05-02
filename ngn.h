#ifndef __INCLUDE_NGN_NGN__
#define __INCLUDE_NGN_NGN__


//デバッグモード
//#define DEBUG_MODE


//NGN名前空間始まり
#ifndef _NGN_BEGIN
#define _NGN_BEGIN namespace ngn {
#endif

//NGN名前空間終わり
#ifndef _NGN_END
#define _NGN_END }
#endif


//NGN名前空間始まり
_NGN_BEGIN


//デフォルトの値たち
#define NGN_INFILE "in.txt"

#define NGN_OUTFILE "out.html"
#define NGN_OUTDIR "out"

#define NGN_TEMPFILE "tmp.html"
#define NGN_TEMPDIR "./"

static char* ngn = "ngn: Novel page GeNerator";

static char* usage = "usage: ngn [TARGET] [-o OUTFILE] [-t TMPFILE]\n\
           [--template-marker TITLE TEXT AFTER]\n\
           [--target-marker TITLE TEXT AFTER]\n\
           [--no-stdout]\n";



//NGN名前空間終わり
_NGN_END

#endif	//__INCLUDE_NGN_NGN__
