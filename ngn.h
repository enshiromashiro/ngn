#ifndef __INCLUDE_NGN_NGN__
#define __INCLUDE_NGN_NGN__


//�f�o�b�O���[�h
//#define DEBUG_MODE


//NGN���O��Ԏn�܂�
#ifndef _NGN_BEGIN
#define _NGN_BEGIN namespace ngn {
#endif

//NGN���O��ԏI���
#ifndef _NGN_END
#define _NGN_END }
#endif


//NGN���O��Ԏn�܂�
_NGN_BEGIN


//�f�t�H���g�̒l����
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



//NGN���O��ԏI���
_NGN_END

#endif	//__INCLUDE_NGN_NGN__
