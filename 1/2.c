// Build with -lcurl
#include <curl/curl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// number of elves to count (here the top 3 are counted)
#define MAX_COUNT 3

// Print message and exits
void panic(const char *msg) {
    printf("%s\n", msg);
    exit(EXIT_FAILURE);
}

// Read the content of a file into a string
char *file_to_string(const char *path, size_t *len) {
    FILE *file = fopen(path, "r");
    // Get size of file
    fseek(file, 0L, SEEK_END);
    long s = ftell(file) + 1;

    rewind(file);
    // Allocate buffer
    char *buf = malloc(s + 1);
    if (!buf) {
        panic("Couldn't allocate buffer for file.");
    }

    // Read content to buffer
    int rc = fread(buf, 1, s, file);
    if (!rc) {
        panic("Error when reading file.");
    }
    fclose(file);

    // Null terminate
    buf[s - 1] = '\0';

    if (len != NULL) {
        *len = s;
    }

    return buf;
}

struct Buffer {
    char  *ptr;
    size_t len;
};

// Call back used by libcurl, used to store the response.
size_t writefunc(void *ptr, size_t size, size_t nmemb, struct Buffer *s) {
    size_t new_len = s->len + size * nmemb;
    s->ptr         = realloc(s->ptr, new_len + 1);
    if (s->ptr == NULL) {
        panic("Realloc failed.");
    }
    memcpy(s->ptr + s->len, ptr, size * nmemb);
    s->ptr[new_len] = '\0';
    s->len          = new_len;

    return size * nmemb;
}

char *get_input() {
    char *cookie;
    {
        size_t l;
        // Path should probably be adapted
        char  *session = file_to_string("../session", &l);
        // remove last newline from session file
        session[l - 2] = '\0';

        cookie    = malloc(8 + l); // 8 is the length of "session="
        cookie[0] = '\0';

        strcat(cookie, "session=");
        strcat(cookie, session);
    }

    CURL *curl;
    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();

    if (!curl) {
        panic("Curl couldn't be initialized.");
    }

    struct Buffer buf = {.ptr = malloc(0), .len = 0};

    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writefunc);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &buf);
    curl_easy_setopt(curl, CURLOPT_COOKIE, cookie);
    curl_easy_setopt(curl, CURLOPT_URL, "https://adventofcode.com/2022/day/1/input");

    CURLcode res = curl_easy_perform(curl);
    if (res != CURLE_OK) {
        panic("Curl didn't return with OK.");
    }

    curl_easy_cleanup(curl);

    return buf.ptr;
}

int main() {
    char * input = get_input();

    char * input_start = input;

    // Array of the top MAX_COUNT elves (sorted)
    int max_elfs[MAX_COUNT] = {0};

    int current_elf = 0;
    int current_num = 0;
    // Process character by character
    do {
        char c = *input;
        if(c == '\n' && current_num == 0) { // New line with current_num = 0 (last char was also a new line) => new elf
            // This is some sort of insert sort I guess.

            // Index of the largest of maxs that is less than the current elf
            //
            // i.e for current_elf=65000 and max_elfs=[12000, 50000, 100000] => i=1, found=true
            int i;
            // If such an index was even found
            bool found;
            // Loop in reverse to go from biggest to smallest
            for(i = MAX_COUNT - 1; i >= 0; i--) {
                if(max_elfs[i] < current_elf) {
                    found = true;
                    break;
                }
            }

            if(found) {
                // Shift all the maxs and insert the current elf
                int val = current_elf;
                for(; i >= 0; i--) {
                    int prev = max_elfs[i];
                    max_elfs[i] = val;
                    val = prev;
                }
            }
            // Reset current elf
            current_elf = 0;
        } else if(c == '\n') { // new line in elf
            // Add current line to current_elf and reset current_num
            current_elf += current_num;
            current_num = 0;
        } else if(c >= '0' && c <= '9') {
            // Parse character as digit
            int digit = c - '0';
            current_num *= 10;
            current_num += digit;
        } else {
            printf("invalid character '%c' (%d) at index %ld\n", c, c, input - input_start);
        }
    } while(*(++input) != '\0');
    
    int sum = 0;
    for(int i = 0; i < MAX_COUNT; i++) {
        sum += max_elfs[i];
        printf("#%d %d\n", MAX_COUNT - i, max_elfs[i]);
    }

    printf("sum of the top %d max elves: %d\n", MAX_COUNT, sum);

    // Buffers are freed by OS
}
