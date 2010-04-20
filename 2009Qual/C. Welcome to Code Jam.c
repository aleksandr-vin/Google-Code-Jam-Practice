/*
 *  Google Code Jam -- Qualification Round 2009
 *  C. Welcome to Code Jam
 *  http://code.google.com/codejam/contest/dashboard?c=90101#s=p2
 *  Practicing for GCJ 2010, Aleksandr Vinokurov
 *
 *  This is a solution written in C language after being unable to
 *  conquer the C-large-practice.in.txt file with the Lisp program.
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>

unsigned int cps(const char * phrase,
		 unsigned int phrase_len,
		 const char * text,
		 unsigned int text_len)
{
  const char   fp     = phrase[0];
  const char   ft     = text[0];
  const char * rp     = &phrase[1];
  const char * rt     = &text[1];
  unsigned int rp_len = phrase_len - 1;
  unsigned int rt_len = text_len - 1;
  unsigned int spsc   = 0;

  if (phrase_len > text_len)
    return 0;
  else
    {
      if (fp != ft)
	return cps(phrase, phrase_len, rt, rt_len);
      else
	{
	  if (rp_len > 0)
	    spsc = cps(rp, rp_len, rt, rt_len);
	  else
	    spsc = 1;
	  if (spsc > 0)
	    {
	      spsc += /*(rt[0] == ft && rp[0] != fp) ?
		spsc : */ cps(phrase, phrase_len, rt, rt_len);
	    }
	  return spsc;
	}
    }
}

unsigned int count_phrase_subsequencies(const char * phrase,
					const char * text)
{
  if (phrase == NULL || text == NULL)
    return 0;
  return cps(phrase, strlen(phrase), text, strlen(text));
}

/* The first simple test case */
void testcase1(void)
{
  const char         test_phrase[] = "abc";
  const char         test_text[]   = "abbcc";
  const unsigned int test_cps      = 4;

  printf(";; Test case #1: %s\n",
	 (test_cps == count_phrase_subsequencies(test_phrase, test_text) ?
	  "ok." : "FAILED!"));
}

/**
 *  Count PHRASE subsequencies on DATA
 *
 *  PHRASE is a zero-terminated string
 *  DATA   is a series of CASES of zero-terminated strings
 *  RESULT is a buffer for CASES results
 */
unsigned int *
count_phrase_subsequencies_on_data(const char * phrase,
				   const char * data,
				   const unsigned int cases,
				   unsigned int result[]) /* a buffer for results */
{
  unsigned int i = 0;
  unsigned int itext_len = 0;
  char *       itext = (char *)data;

  for (i = 0; i < cases; i++)
    {
      result[i] = count_phrase_subsequencies(phrase, itext);
      itext    += 1 + strlen(itext);
    }
  return result;
}

unsigned int * cps_on_cases(const char * phrase,
			    const char * data,
			    const unsigned int cases,
			    unsigned int * result)
{
  const unsigned int newline_symb_len = strlen("\n");
  unsigned int i       = 0;
  char *       idata   = (char *)data;
  char         text[501];

  for(i = 0; i < 3; i++)
    {
      sscanf(idata, "%s", &text);
      idata      += newline_symb_len + strlen(text);
      result[i]   = count_phrase_subsequencies(phrase, text);
    }
  return result;
}

/* A bit harder test case */
void testcase2(void)
{
  const unsigned int newline_symb_len = strlen("\n");
  const char phrase[] = "abc";
  const char data[]   = "3\n\
abbcc\n\
aabbcc\n\
abbbbaabbbbbaaaaabbdesssa\n";
  const unsigned int  cps[3] = {4,8,0};
  unsigned int result_buf[3] = {0,0,0};
  unsigned int * result      = NULL;
  unsigned int i             = 0;
  unsigned int test_result   = 1;
  unsigned int cases = 0;
  char cases_str[100];
  char * idata = NULL;

  sscanf(data, "%s", &cases_str);
  cases = (unsigned int)strtoul(cases_str, NULL, 10);
  if (cases != 3)
    {
      printf("testcase #2 error: cases read are not equal to 3!\n");
      return;
    }
  idata = (char *)data + newline_symb_len + strlen(cases_str);
  result = cps_on_cases(phrase, idata, cases, result_buf);
  for(i = 0; i < 3; i++)
    test_result = test_result && (cps[i] == result[i]);
  printf(";; Test case #2: %s\n",
	 (test_result ? "ok." : "FAILED!"));
}

void run_on_file(const char *file)
{
  const unsigned int newline_symb_len = strlen("\n");
  const char phrase[] = "welcome to code jam";
  unsigned int cases  = 0;
  unsigned int i      = 0;
  char      text[501];
  char cases_str[100];
  FILE * stream;

  stream = fopen(file, "r");
  if (stream == NULL)
    {
      printf("testcase #3 error: can't open file %s: %s\n", file, strerror(errno));
      return;
    }

  fgets(cases_str, 100, stream);
  cases = (unsigned int)strtoul(cases_str, NULL, 10);

  for (i = 0; i < cases; i++)
    {
      fgets(text, 501, stream);
      text[strlen(text) - newline_symb_len] = '\0';
      /*printf(";; text: %s\n", text);*/
      printf("Case #%d: %04d\n", i + 1,
	     count_phrase_subsequencies(phrase, text));
    }
}

int main(int argc, char *argv[])
{
  if (argc == 1)
    {
      testcase1();
      testcase2();
      return 0;
    }

  /*printf("; Running on file %s\n", argv[1]);*/
  run_on_file(argv[1]);
}
