// annotate with ms

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <stdarg.h>
#include <signal.h>

typedef unsigned long long MS;

MS millisecs(void)
{
  struct timeval tv;
  gettimeofday(&tv, 0);
  return tv.tv_sec * 1000000 + tv.tv_usec;
}

#define ARRAY(T) struct { T* ptr; size_t length, capacity; }

void dump_hex(size_t len, unsigned char *data) {
  size_t i;
  for (i = 0; i < len; ++i) {
    printf("%02x", data[i]);
  }
}

void set(size_t size_1, size_t size_2, char *ch1, char *ch2, void *dest, ...) {
  size_t i;
  unsigned int *uidp = (unsigned int *) dest;
  va_list vl;
  if (size_1 != size_2) {
    printf("Disparate set sizes: %i (%s) vs %i (%s)\n", size_1, ch1, size_2, ch2);
    *(int*) 0=0;
  }
  if (size_1 & 0x3) {
    printf("Element size must be divisible by four. \n");
    *(int*) 0=0;
  }
  
  va_start(vl, dest);
  for (i = 0; i < size_1 / 4; ++i) {
    *uidp++ = va_arg(vl, unsigned int);
  }
  va_end(vl);
}
#define SET(A, B) set(sizeof(A), sizeof(B), #A, #B, &A, B)
#define VSET(A, B) ({\
  if (sizeof(A) != sizeof(B)) *(int*) 0=0;\
  memcpy(&(A), &(B), sizeof(A));\
})

int equal(size_t size_1, size_t size_2, unsigned int *p1, unsigned int *p2) {
  size_t i = 0;
  if (size_1 != size_2) {
    printf("Disparate cmp sizes: %i vs %i\n", size_1, size_2);
    *(int*) 0=0;
  }
  if (size_1 & 0x3) {
    printf("Element size must be divisible by four. \n");
    *(int*) 0=0;
  }
  
  for (i = 0; i < size_1 / 4; ++i) {
    if (p1[i] != p2[i]) return 0;
  }
  return 1;
}
// #define EQUAL(A, B) equal(sizeof(A), sizeof(B), (A), (B))
#define EQUAL(A, B) equal(sizeof(A), sizeof(B), (unsigned int *) &(A), (unsigned int *) &(B))

typedef ARRAY(char) string;

string mkString(char* c) {
  string res;
  res.ptr = c;
  res.length = strlen(c);
  res.capacity = res.length;
  return res;
}

// for passing to %.*s
#define PRINTFORM(x) (x).length, (x).ptr

string slice(string s, size_t from, size_t to)
{
  s.length = to - from;
  s.ptr += from;
  if (from > 0)
    s.capacity = 0; /* can't reallocate a string beyond the start */
  return s;
}

#define ASSOC_ARRAY_ENTRY(K, T) struct {\
  K key; \
  T value; \
}

#define ASSOC_ARRAY(K, T) struct {\
  ARRAY(ASSOC_ARRAY_ENTRY(K, T)) field; \
}

#define ARRAY_TYPE(A) typeof(*(A).ptr)

#define ASSOC_KEYTYPE(A) typeof((A).field.ptr[0].key)
#define ASSOC_VALUETYPE(A) typeof((A).field.ptr[0].value)

#define ARRAY_RESIZE(A, N, I) do {\
  size_t array_resize_i;\
  if (newlen > (A).capacity)\
  {\
    (A).ptr = realloc((A).ptr, N * sizeof(ARRAY_TYPE(A)));\
    (A).capacity = N;\
  }\
  for (array_resize_i = (A).size; array_resize_i < N; ++array_resize_i)\
    (A).ptr[array_resize_i] = I;\
  (A).size = N;\
} while (false)

#define ARRAY_GEN(T, N, I) ({\
  size_t _array_gen_i;\
  ARRAY(T) _array_gen_res;\
  _array_gen_res.length = _array_gen_res.capacity = N;\
  _array_gen_res.ptr = malloc(_array_gen_res.length*sizeof(T));\
  for (_array_gen_i = 0; _array_gen_i < _array_gen_res.length; ++_array_gen_i)\
    SET(_array_gen_res.ptr[_array_gen_i], (I));\
  _array_gen_res;\
})

#define ASSOC_GEN(K, T, I) ({\
  ASSOC_ARRAY(K, T) _assoc_gen_res;\
  SET(_assoc_gen_res.field, ARRAY_GEN(typeof(_assoc_gen_res.field.ptr[0]), 1, I));\
  _assoc_gen_res;\
})

#define ASSOC_ISFREE(A, V, I) EQUAL((A).field.ptr[V].key, (I).key)

#define FILLRATE(A, I) ({\
  size_t unfilled = 0, fillrate_i;\
  for (fillrate_i = 0; fillrate_i < (A).field.length; ++fillrate_i)\
    if (ASSOC_ISFREE((A), fillrate_i, (I))) unfilled ++;\
  ((A).field.length - unfilled) * 100 / (A).field.length;\
})

#define ASSOC_ITERATE_ALL_FROM(A, I, F, LI, LK, LV, LB) ({\
  size_t _aif_i;\
  size_t LI = (F); \
  for (_aif_i = 0; _aif_i < (A).field.length; ++_aif_i)\
  {\
    ASSOC_KEYTYPE(A) LK;   VSET(LK, (A).field.ptr[LI].key);\
    ASSOC_VALUETYPE(A) LV; VSET(LV, (A).field.ptr[LI].value);\
    LB\
    if (++LI == (A).field.length)\
      LI = 0;\
  }\
})

#define ASSOC_ITERATE(A, I, LK, LV, LB) ({\
  size_t _assoc_iterate_i; \
  for (_assoc_iterate_i = 0; _assoc_iterate_i < (A).field.length; ++_assoc_iterate_i)\
  {\
    if (ASSOC_ISFREE((A), _assoc_iterate_i, (I))) continue;\
    ASSOC_KEYTYPE(A) LK;   VSET(LK, (A).field.ptr[_assoc_iterate_i].key);\
    ASSOC_VALUETYPE(A) LV; VSET(LV, (A).field.ptr[_assoc_iterate_i].value);\
    LB\
  }\
})

#define ASSOC_INSERT_PLAIN(A, I, E, HV, K, V) ({\
  __label__ assoc_done;\
  typeof(K) _aip_key;\
  SET(_aip_key, (K)); /* done here in case key is computationally expensive */\
  ASSOC_ITERATE_ALL_FROM(A, I, HV, _aip_cur_i, _aip_key_i, _aip_value_i,\
    ({\
      if (ASSOC_ISFREE((A), _aip_cur_i, (I)) || E((A).field.ptr[_aip_cur_i].key, _aip_key))\
      {\
        typeof((A).field.ptr[0]) tmpv;\
        VSET(tmpv.key, _aip_key);\
        SET(tmpv.value, (V));\
        VSET((A).field.ptr[_aip_cur_i], tmpv);\
        goto assoc_done;\
      }\
    });\
  );\
  printf("Couldn't insert: assoc array full at %i; %i. \n", (A).field.length, FILLRATE(A, I));\
  *(int*) 0 = 0;\
  assoc_done:;\
})

#define ASSOC_FREE(A) free(A.field.ptr)

#define ASSOC_INSERT(A, I, E, H, K, V) ({\
  typeof(A) _assoc_insert_res;\
  VSET(_assoc_insert_res, A);\
  typeof(V) _assoc_insert_value;\
  SET(_assoc_insert_value, (V)); /* done here in case V references A; so we can free A early */\
  if (FILLRATE(A, I) > 20) /* sparse AA; trade memory for speed */\
  {\
    SET(_assoc_insert_res.field, ARRAY_GEN(typeof((A).field.ptr[0]), (A).field.length * 2, (I)));\
    /* printf("Resize to %i\n", (A).field.length * 2);*/\
    ASSOC_ITERATE(A, (I), _assoc_insert_key, _assoc_insert_value, ({\
        ASSOC_INSERT_PLAIN(_assoc_insert_res, (I), E, H, _assoc_insert_key, _assoc_insert_value);\
      });\
    );\
    ASSOC_FREE(A);\
  }\
  ASSOC_INSERT_PLAIN(_assoc_insert_res, (I), E, H, K, _assoc_insert_value);\
  VSET(A, _assoc_insert_res);\
})

#define ASSOC_FIND(A, I, HV, E, K) ({\
  size_t _assoc_find_res = ~0U;\
  ASSOC_ITERATE_ALL_FROM(A, I, HV, _assoc_find_id, _assoc_find_key, _assoc_find_value,\
    ({\
      if (EQUAL(_assoc_find_key, (I).key)) break;\
      else if (E((K), _assoc_find_key)) { _assoc_find_res = _assoc_find_id; break; }\
    });\
  );\
  _assoc_find_res;\
})

#define ASSOC_GET(A, I, HV, E, K) ({\
  ASSOC_VALUETYPE(A) _assoc_get_res;\
  int _assoc_get_found = 0;\
  ASSOC_ITERATE_ALL_FROM(A, I, HV, _assoc_get_id, _assoc_get_key, _assoc_get_value,\
    ({\
      if (EQUAL(_assoc_get_key, (I).key)) break;\
      else if (E((K), _assoc_get_key)) {\
        _assoc_get_found = 1;\
        VSET(_assoc_get_res, _assoc_get_value);\
        break;\
      }\
    });\
  );\
  if (!_assoc_get_found) {\
    printf("Entry not found! \n");\
    *(int*) 0=0;\
  }\
  _assoc_get_res;\
})

#define ASSOC_UPDATE(A, I, E, H, K, NK, VN, VB) ({\
  unsigned int _assoc_update_hv = H(K) % (A).field.length;\
  size_t _assoc_update_pos = ASSOC_FIND(A, I, _assoc_update_hv, E, K);\
  if (_assoc_update_pos != ~0U) {\
    ASSOC_VALUETYPE(A) VN;\
    VSET(VN, (A).field.ptr[_assoc_update_pos].value);\
    SET((A).field.ptr[_assoc_update_pos].value, VB);\
  } else {\
    ASSOC_INSERT(A, I, E, _assoc_update_hv, NK, ({\
      ASSOC_VALUETYPE(A) VN;\
      VSET(VN, (I).value);\
      VB;\
    }));\
  }\
})

string readln(string buffer) {
  ssize_t bytesRead = getline(&buffer.ptr, &buffer.capacity, stdin);
  
  if (bytesRead == -1) { // FD closed
    buffer.ptr = 0;
    buffer.length = buffer.capacity = 0;
    return buffer;
  }
  
  return slice(buffer, 0, bytesRead - 1 /* remove trailing newline */);
}

void swap_strings(string* a, string* b)
{
  string temp = *a;
  *a = *b;
  *b = temp;
}

typedef struct {
  int num;
  float sum;
} LineCount;

unsigned int hash_string(string s)
{
  unsigned int res = 0, i;
  unsigned int *sip = (unsigned int *) s.ptr;
  for (i = 0; i < s.length / 4; ++i)
  {
    res ^= sip[i];
  }
  return res;
}

int equal_strings(string a, string b)
{
  int res;
  if (a.length != b.length) res = 0;
  else res = memcmp(a.ptr, b.ptr, a.length) == 0;
  // printf("strcmp: %.*s and %.*s => %i\n", PRINTFORM(a), PRINTFORM(b), res);
  return res;
}

string dup(string s) {
  string res = s;
  res.ptr = malloc(s.length);
  memcpy(res.ptr, s.ptr, s.length);
  return res;
}

int find(string s, char* match) {
  size_t i;
  size_t mlen = strlen(match);
  if (mlen > s.length) return -1;
  for (i = 0; i <= s.length - mlen; ++i) {
    if (memcmp(s.ptr + i, match, mlen) == 0) return i;
  }
  return -1;
}

string filter(string s) {
  int pos = find(s, "0x");
  while (pos != -1) {
    int npos;
    pos += 2;
    while (s.ptr[pos] >= '0' && s.ptr[pos] <= '9' || s.ptr[pos] >= 'a' && s.ptr[pos] <= 'f')
      s.ptr[pos++] = '.';
    npos = find(slice(s, pos, s.length), "0x");
    if (npos != -1) npos += pos;
    pos = npos;
  } while (pos != -1);
  size_t i, ns = 0;
  for (i = 0; i < s.length; ++i) {
    if (s.ptr[i] >= '0' && s.ptr[i] <= '9') s.ptr[i] = '_';
    /*if (s.ptr[i] >= '0' && s.ptr[i] <= '9') ns ++;
    else ns = 0;
    if (ns < 2) continue;
    if (ns == 2) {
      s.ptr[i-1] = s.ptr[i] = '_';
    } else s.ptr[i] = '_';*/
  }
  return s;
}

ASSOC_ARRAY(string, LineCount) counts;
ASSOC_ARRAY_ENTRY(string, LineCount) Init = {{0, 0, 0}, {0, 0.0f}};

void printCounts(void)
{
  int i = 0;
  ASSOC_ITERATE(counts, Init, key, value,
    ({
      printf("%f over %i: %.*s\n", value.sum, value.num, key.length, key.ptr);
      i++;
    });
  );
  printf("%i lines. \n", i);
}

int brk;
void handle_sigint(int sign_id) {
  brk = 1;
  close(0);
}

int main(int argc, const char** argv)
{
  brk = 0;
  signal(SIGINT, handle_sigint);
  MS last_time = millisecs(), start_time = last_time;
  string last_line = {0, 0, 0}, cur_line = {0, 0, 0};
  SET(counts, ASSOC_GEN(string, LineCount, Init));
  while (!brk)
  {
    MS cur_time;
    cur_line = filter(readln(cur_line));
    cur_time = millisecs();
    float delta = (cur_time - last_time) / 1000.0f, total = (cur_time - start_time) / 1000.0f;
    printf("%f\t%f\t: %.*s\n", delta, total, PRINTFORM(last_line));
    ASSOC_UPDATE(counts, Init, equal_strings, hash_string, last_line, dup(last_line), old_value, ({
      LineCount nv = old_value;
      nv.num ++;
      nv.sum += delta;
      nv;
    }));
    last_time = millisecs();
    if (!cur_line.ptr)
    {
      break;
    }
    swap_strings(&cur_line, &last_line);
  }
  printf("%.*s\n", PRINTFORM(last_line));
  printCounts();
  return 0;
}
