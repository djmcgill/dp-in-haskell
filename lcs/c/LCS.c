
#define NDEBUG

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "uthash-master/src/uthash.h"
#include "uthash-master/src/utlist.h"

#include "LCS.h"

int main (int argc, char* argv[]) {
	if (argc != 3) {
		printf("Usage: %s <string_1> <string_2>", argv[0]);
		exit(0);
	}
	my_index** substrings = findAllCommonSubstrings (argv[1], argv[2]);
	substring_list* lcs = getLongestSubstrings (substrings, argv[1]);

	// free the hash table
	my_index *current_index, *tmp;
  	HASH_ITER(hh, *substrings, current_index, tmp) {
    	free(current_index);
    }
    HASH_CLEAR(hh, *substrings);

    // do stuff with the list of longest common substrings.
    // In this case we just print them.
	printf ("The longest common substrings are:\n");
	substring_list* tmp_lcs;
	LL_FOREACH (lcs, tmp_lcs) {
		printf("\t%s\n", tmp_lcs->substring);
	}


	// free the list
	substring_list* tmp_lcs_2;
	LL_FOREACH_SAFE(lcs, tmp_lcs, tmp_lcs_2) {
		LL_DELETE (lcs, tmp_lcs);
		free(tmp_lcs);
	}

}

my_index** findAllCommonSubstrings (const char* const restrict s1,
                                    const char* const restrict s2) {

	my_index** substrings = malloc(sizeof(my_index*));
	*substrings = NULL;

	int i;
	int iN = strlen(s1);
	int j;
	int jN = strlen(s2);

	for (i = 0; i < iN; i++) {
		for (j = 0; j < jN; j++) {
			if (s1[i] == s2[j]) {
				// find and delete the previous value, default of 0
				int old_key = hash (i-1, j-1);
				my_index* previous;
				HASH_FIND_INT (*substrings, &old_key, previous);
				int previous_value = 0;
				if (previous != NULL) {
					previous_value = previous->ss_length;
					HASH_DEL(*substrings, previous);
				}

				free(previous);

				// create the new value to be inserted
				int new_key = hash (i, j);
				my_index* new_value = malloc (sizeof(my_index));
				*new_value = (my_index){i, new_key, previous_value + 1};

				// if there's a collision, we have a problem
				my_index* new_index;
				HASH_FIND_INT (*substrings, &new_key, new_index);
				if (new_index != NULL) {
					printf ("Error: string2 is too long. 16777618 characters (~16MB) is the maximum");
				} else {
					HASH_ADD_INT(*substrings, key, new_value);
				}
			}
		}
	}
	return substrings;
}

substring_list* getLongestSubstrings (my_index** substrings,
                                   const char* const s) {
	substring_list* longest_substrings = NULL;

	// loop through the whole hash, looking for the maximum length
	int max_length = 0;
	my_index *current_index, *tmp;
  	HASH_ITER(hh, *substrings, current_index, tmp) {
  		int current_length = current_index->ss_length;
  		if (current_length > max_length) {max_length = current_length;}
    }

	// any strings with the max length get added to the list
	HASH_ITER(hh, *substrings, current_index, tmp) {
  		int current_length = current_index->ss_length;
  		if (current_length == max_length) {
  			// add the substring to the list
  			substring_list* new_ss = malloc (sizeof (substring_list));

  			size_t length = current_index->ss_length;
  			size_t startIndex = current_index->ix - length + 1;
  			new_ss->substring = malloc (length+1 * sizeof (char));
  			strncpy (new_ss->substring, s + startIndex, length);

  			LL_PREPEND(longest_substrings, new_ss);
  		}
    }

	// clear the whole hashtable
	return longest_substrings;
}

int hash (int _ix1, int _ix2) {
	return _ix1*16777619 ^ _ix2; // FNV hash
}