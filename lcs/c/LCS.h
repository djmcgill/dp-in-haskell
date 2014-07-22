#ifndef LCS_H
#define LCS_H



typedef struct my_index {
	int ix;
	int key;
	int ss_length;
	UT_hash_handle hh;
} my_index;

typedef struct substring_list {
	char* substring;
	struct substring_list* next;
} substring_list;

my_index** findAllCommonSubstrings (const char* const restrict,
                                    const char* const restrict);

substring_list* getLongestSubstrings (my_index**, const char* const);

int hash (int, int);

#endif