#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <stdarg.h>
#include <iconv.h>
#include <migemo.h>
#include <emacs-module.h>

int plugin_is_GPL_compatible;

/* from cmigemo charset.h */
enum {
    CHARSET_NONE = 0,
    CHARSET_CP932 = 1,
    CHARSET_EUCJP = 2,
    CHARSET_UTF8 = 3,
};
typedef int (*charset_proc_int2char)(unsigned int, unsigned char*);
#define CHARSET_PROC_INT2CHAR charset_proc_int2char

/* from cmigemo charset.c */
#define BUFLEN_DETECT 4096
static int cp932_int2char(unsigned int in, unsigned char* out)
{
    if (in >= 0x100)
    {
	if (out)
	{
	    out[0] = (unsigned char)((in >> 8) & 0xFF);
	    out[1] = (unsigned char)(in & 0xFF);
	}
	return 2;
    }
    else
	return 0;
}
static int eucjp_int2char(unsigned int in, unsigned char* out)
{
    /* CP932と内容は同じだが将来JISX0213に対応させるために分離しておく */
    if (in >= 0x100)
    {
	if (out)
	{
	    out[0] = (unsigned char)((in >> 8) & 0xFF);
	    out[1] = (unsigned char)(in & 0xFF);
	}
	return 2;
    }
    else
	return 0;
}
static int utf8_int2char(unsigned int in, unsigned char* out)
{
    if (in < 0x80)
	return 0;
    if (in < 0x800)
    {
	if (out)
	{
	    out[0] = 0xc0 + (in >> 6);
	    out[1] = 0x80 + ((in >> 0) & 0x3f);
	}
	return 2;
    }
    if (in < 0x10000)
    {
	if (out)
	{
	    out[0] = 0xe0 + (in >> 12);
	    out[1] = 0x80 + ((in >> 6) & 0x3f);
	    out[2] = 0x80 + ((in >> 0) & 0x3f);
	}
	return 3;
    }
    if (in < 0x200000)
    {
	if (out)
	{
	    out[0] = 0xf0 + (in >> 18);
	    out[1] = 0x80 + ((in >> 12) & 0x3f);
	    out[2] = 0x80 + ((in >> 6) & 0x3f);
	    out[3] = 0x80 + ((in >> 0) & 0x3f);
	}
	return 4;
    }
    if (in < 0x4000000)
    {
	if (out)
	{
	    out[0] = 0xf8 + (in >> 24);
	    out[1] = 0x80 + ((in >> 18) & 0x3f);
	    out[2] = 0x80 + ((in >> 12) & 0x3f);
	    out[3] = 0x80 + ((in >> 6) & 0x3f);
	    out[4] = 0x80 + ((in >> 0) & 0x3f);
	}
	return 5;
    }
    else
    {
	if (out)
	{
	    out[0] = 0xf8 + (in >> 30);
	    out[1] = 0x80 + ((in >> 24) & 0x3f);
	    out[2] = 0x80 + ((in >> 18) & 0x3f);
	    out[3] = 0x80 + ((in >> 12) & 0x3f);
	    out[4] = 0x80 + ((in >> 6) & 0x3f);
	    out[5] = 0x80 + ((in >> 0) & 0x3f);
	}
	return 6;
    }
}
static int
charset_detect_buf(const unsigned char* buf, int len)
{
    int sjis = 0, smode = 0;
    int euc = 0, emode = 0, eflag = 0;
    int utf8 = 0, umode = 0, ufailed = 0;
    int i;
    for (i = 0; i < len; ++i)
    {
	unsigned char c = buf[i];
	// SJISであるかのチェック
	if (smode)
	{
	    if ((0x40 <= c && c <= 0x7e) || (0x80 <= c && c <= 0xfc))
		++sjis;
	    smode = 0;
	}
	else if ((0x81 <= c && c <= 0x9f) || (0xe0 <= c && c <= 0xf0))
	    smode = 1;
	// EUCであるかのチェック
	eflag = 0xa1 <= c && c <= 0xfe;
	if (emode)
	{
	    if (eflag)
		++euc;
	    emode = 0;
	}
	else if (eflag)
	    emode = 1;
	// UTF8であるかのチェック
	if (!ufailed)
	{
	    if (umode < 1)
	    {
		if ((c & 0x80) != 0)
		{
		    if ((c & 0xe0) == 0xc0)
			umode = 1;
		    else if ((c & 0xf0) == 0xe0)
			umode = 2;
		    else if ((c & 0xf8) == 0xf0)
			umode = 3;
		    else if ((c & 0xfc) == 0xf8)
			umode = 4;
		    else if ((c & 0xfe) == 0xfc)
			umode = 5;
		    else
		    {
			ufailed = 1;
			--utf8;
		    }
		}
	    }
	    else
	    {
		if ((c & 0xc0) == 0x80)
		{
		    ++utf8;
		    --umode;
		}
		else
		{
		    --utf8;
		    umode = 0;
		    ufailed = 1;
		}
	    }
	    if (utf8 < 0)
		utf8 = 0;
	}
    }
    // 最終的に一番得点の高いエンコードを返す
    if (euc > sjis && euc > utf8)
	return CHARSET_EUCJP;
    else if (!ufailed && utf8 > euc && utf8 > sjis)
	return CHARSET_UTF8;
    else if (sjis > euc && sjis > utf8)
	return CHARSET_CP932;
    else
	return CHARSET_NONE;
}
static int
charset_detect_file(const char* path)
{
    int charset = CHARSET_NONE;
    FILE* fp;
    if ((fp = fopen(path, "rt")) != NULL)
    {
	unsigned char buf[BUFLEN_DETECT];
	size_t len = fread(buf, sizeof(buf[0]), sizeof(buf), fp);
	fclose(fp);
	if (len > 0 && len <= INT_MAX)
	    charset = charset_detect_buf(buf, (int)len);
    }
    return charset;
}
/**/

static migemo *pmigemo = NULL;

static int int2char(unsigned int in, unsigned char* out,
		    CHARSET_PROC_INT2CHAR proc)
{
  switch (in)  {
  case '.': case '*': case '+': case '?':
  case '[': case ']': case '^': case '$':
  case '\\':
    if (!out)
      return 0;
    out[0] = '\\';
    out[1] = in;
    return 2;
  default:
    break;
  }
  return (*proc)(in, out);
}

static int eucjp(unsigned int in, unsigned char* out)
{
  return int2char(in, out, eucjp_int2char);
}    

static int cp932(unsigned int in, unsigned char* out)
{
  return int2char(in, out, cp932_int2char);
}

static int utf8(unsigned int in, unsigned char* out)
{
  return int2char(in, out, utf8_int2char);
}

static int charset;

static MIGEMO_PROC_INT2CHAR getproc(int charset)
{
  
  switch (charset) {
  case CHARSET_CP932:
    return cp932;
    break;
  case CHARSET_EUCJP:
    return eucjp;
    break;
  case CHARSET_UTF8:
    return utf8;
    break;
  default:
    break;
  }
  return NULL;
}

extern int vasprintf(char **, const char *, va_list);

static void
error (emacs_env *env, const char *fmt, ...)
{
  va_list arg;
  va_start (arg, fmt);
  char *buf = NULL;

  if (vasprintf (&buf, fmt, arg) == -1)
    {
      va_end (arg);
      return;
    }
  emacs_value args[] = { env->make_string (env, buf, strlen (buf)) };
  free (buf);
  va_end (arg);
  env->funcall (env, env->intern (env, "error"), 1, args);
}

#define CHARSET_TO_CODE(charset)                                         \
  (((charset) == CHARSET_UTF8) ? "UTF-8"                                 \
                               : ((charset) == CHARSET_EUCJP) ? "EUC-JP" \
                                                              : "CP932")

#ifdef USE_LIBICONV
static bool
is_multibyte_string (emacs_env *env, emacs_value args[])
{
  return env->is_not_nil (env,
                          env->funcall (env,
			                env->intern (env, "multibyte-string-p"),
                                        1, args));
}

static char *
code_convert_string (char *str, ptrdiff_t size, int charset, bool encode)
{
  char *result;
  size_t inbytesleft, outbytesleft;
  char *inbuf, *outbuf;

  inbytesleft = size;
  outbytesleft = size * 6;
  if ((result = malloc (outbytesleft)) != NULL)
    {
      const char *tocode, *fromcode;
      iconv_t cd;

      if (encode)
        {
          tocode = CHARSET_TO_CODE (charset);
          fromcode = CHARSET_TO_CODE (CHARSET_UTF8);
        }
      else
        {
          tocode = CHARSET_TO_CODE (CHARSET_UTF8);
          fromcode = CHARSET_TO_CODE (charset);
        }
      if ((cd = iconv_open (tocode, fromcode)) == (iconv_t)-1)
	{
	  free (result);
	  return NULL;
	}

      inbuf = str;
      outbuf = result;
      if (iconv (cd, &inbuf, &inbytesleft, &outbuf, &outbytesleft) == -1)
	{
	  free (result);
	  return NULL;
	}
      *outbuf = '\0';
      iconv_close (cd);
    }

  return result;
}
#endif

static emacs_value
Fcmigemo_open (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  emacs_value lisp_str = args[0];
  ptrdiff_t size = 0;
  char *dictionary = NULL;
  MIGEMO_PROC_INT2CHAR int2char;  

  if (pmigemo)
    error (env, "cmigemo already opened.");

  env->copy_string_contents (env, lisp_str, dictionary, &size);
  dictionary = alloca (size);
  env->copy_string_contents (env, lisp_str, dictionary, &size);

  if (!(pmigemo = migemo_open (dictionary)) ||
      !migemo_is_enable (pmigemo)) {
    pmigemo = NULL;
    error (env, "could not open cmigemo.");
  }

  migemo_set_operator (pmigemo, MIGEMO_OPINDEX_OR, (unsigned char *) "\\|");
  migemo_set_operator (pmigemo, MIGEMO_OPINDEX_NEST_IN, (unsigned char *) "\\(");
  migemo_set_operator (pmigemo, MIGEMO_OPINDEX_NEST_OUT, (unsigned char *) "\\)");
  migemo_set_operator (pmigemo, MIGEMO_OPINDEX_NEWLINE, (unsigned char *) "\\s-*");

  charset = charset_detect_file (dictionary);

#ifndef USE_LIBICONV
  if (charset != CHARSET_UTF8)
    {
      migemo_close (pmigemo);
      pmigemo = NULL;
      error (env, "charset of migemo-dictionary is %s. this cmigemo-module can use UTF-8 dictionary only.", CHARSET_TO_CODE (charset));
    }
#endif

  if ((int2char = getproc (charset)) != NULL)
    migemo_setproc_int2char (pmigemo, int2char);

  return env->intern (env, "t");
}

static emacs_value
Fcmigemo_close (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  if (!pmigemo)
    error (env, "cmigemo was not opened.");
  migemo_close (pmigemo);
  pmigemo = NULL;
  return env->intern (env, "t");
}

static emacs_value
Fcmigemo_query (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  emacs_value word = args[0];
  char *temp = NULL, *ans;
  ptrdiff_t size = 0;
  emacs_value result = env->intern (env, "nil");

  if (!pmigemo)
    error (env, "cmigemo was not opened.");

  env->copy_string_contents (env, word, temp, &size);
  temp = malloc (size);
  env->copy_string_contents (env, word, temp, &size);

#ifdef USE_LIBICONV
  if (is_multibyte_string (env, args))
    {
      char *temp2;

      switch (charset)
        {
        default:
        case CHARSET_CP932:
        case CHARSET_EUCJP:
          temp2 = code_convert_string (temp, size, charset, true);
          if (!temp2)
            error (env, "cmigemo-module: code conversion (encode) failed.");
          else
            {
              free (temp);
              temp = temp2;
            }
          break;
        case CHARSET_UTF8:
          break;
        }
    }
#endif

  ans = (char *) migemo_query (pmigemo, (unsigned char *) temp);
  free (temp);

  if (ans)
#ifdef USE_LIBICONV
    {
      switch (charset)
        {
        default:
        case CHARSET_CP932:
        case CHARSET_EUCJP:
          temp = code_convert_string (ans, strlen (ans) + 1, charset, false);
          if (!temp)
            error (env, "cmigemo-module: code conversion (decode) failed.");
          else
            {
              result = env->make_string (env, temp, strlen (temp));
              free (temp);
            }
          break;
        case CHARSET_UTF8:
          result = env->make_string (env, ans, strlen (ans));
          break;
        }
    }
#else
    result = env->make_string (env, ans, strlen (ans));
#endif

  migemo_release (pmigemo, (unsigned char *) ans);

  return result;
}

static emacs_value
Fcmigemo_load (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  emacs_value lisp_str = args[0];
  ptrdiff_t size = 0;
  char *dictionary = NULL;
  MIGEMO_PROC_INT2CHAR int2char;  

  if (!pmigemo)
    error (env, "cmigemo was not opened");

  env->copy_string_contents (env, lisp_str, dictionary, &size);
  dictionary = alloca (size);
  env->copy_string_contents (env, lisp_str, dictionary, &size);

  if (migemo_load (pmigemo, MIGEMO_DICTID_MIGEMO,
		   dictionary) == MIGEMO_DICTID_INVALID)
    error (env, "migemo_load invalid dict %s", dictionary);

  /*
   * Migemo_load resets int2char proc,
   * then we set it again.
   */

  int2char = getproc(charset);
  migemo_setproc_int2char (pmigemo, int2char);

  return env->intern (env, "t");
}

/* Lisp utilities for easier readability (simple wrappers).  */

/* Provide FEATURE to Emacs.  */
static void
provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

/* Bind NAME to FUN.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qfset, 2, args);
}

/* Module init function.  */
int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);

#define DEFUN(lsym, csym, amin, amax, doc, data) \
  bind_function (env, lsym, \
		 env->make_function (env, amin, amax, csym, doc, data))

  DEFUN ("cmigemo-open", Fcmigemo_open, 1, 1, "open cmigemo with DICTIONARY.", NULL);
  DEFUN ("cmigemo-close", Fcmigemo_close, 0, 0, "close cmigemo.", NULL);
  DEFUN ("cmigemo-query", Fcmigemo_query, 1, 1, "query cmigemo about WORD.", NULL);
  DEFUN ("cmigemo-load", Fcmigemo_load, 1, 1, "load a sub DICTIONARY.", NULL);

#undef DEFUN

  provide (env, "cmigemo-module");
  return 0;
}
