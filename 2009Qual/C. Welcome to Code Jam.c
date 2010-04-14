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
	      spsc += (rt[0] == ft && rp[0] != fp) ?
		spsc : cps(phrase, phrase_len, rt, rt_len);
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

  printf("Test case #1: %s\n",
	 (test_cps == count_phrase_subsequencies(test_phrase, test_text) ?
	  "ok." : "FAILED!"));
}

unsigned int *
count_phrase_subsequencies_on_data(const char * phrase,
				   const char * data,
				   const unsigned int cases,
				   unsigned int result[]) /* a buffer for results */
{
  return result;
}

/* A bit harder test case */
void testcase2(void)
{
  const char phrase[] = "abc";
  const char data[]   = "\
abbcc\0\
aabbcc\0\
abbbbaabbbbbaaaaabbdesssa\0";
  const unsigned int  cps[3] = {4,8,0};
  unsigned int result_buf[3] = {0,0,0};
  unsigned int *result       = NULL;
  unsigned int i             = 0;
  unsigned int test_result   = 1;

  result = count_phrase_subsequencies_on_data(phrase, data, 3, result_buf);
  for(i = 0; i < 3; i++)
    test_result = test_result && (cps[i] == result[i]);
  printf("Test case #2: %s\n",
	 (test_result ? "ok." : "FAILED!"));
}

int main(void)
{
  testcase1();
  testcase2();
}
