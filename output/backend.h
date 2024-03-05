#ifndef BACKEND_H
#define BACKEND_H

#include "stdint.h"
#include "stddef.h"
#include "stdlib.h"
#include "stdbool.h"
#include "stb_ds.h"

#ifdef _MSC_VER
	#define MSVC
	#define force_inline __forceinline
	#define __FUNCTION_SIG__ __FUNCSIG__
#elif __GNUC__
	#define GCC
	#define force_inline __attribute__((always_inline))
	#define __FUNCTION_SIG__ __PRETTY_FUNCTION__
#else
	#define force_inline inline
#endif

typedef int32_t s32;
typedef int64_t s64;

typedef uint32_t u32;
typedef uint64_t u64;


typedef float f32;
typedef double f64;

typedef ptrdiff_t isize;
typedef size_t usize;

typedef const char * String;

#define _internal_make_default_operators(T)\
static inline T T##_add(T l, T r) {\
    return l + r;\
}\
static inline T T##_sub(T l, T r) {\
    return T##_add(l, -r);\
}\
static inline T T##_mul(T l, T r) {\
    return l * r;\
}\
static inline T T##_div(T l, T r) {\
    return l/r;\
}\
\
static inline bool T##_le(T l, T r) {\
    return l < r;\
}\
\
static inline bool T##_ge(T l, T r) {\
    return l > r;\
}\
\
static inline bool T##_leq(T l, T r) {\
    return l <= r;\
}\
\
static inline bool T##_geq(T l, T r) {\
    return l >= r;\
}\
static inline bool T##_eq(T l, T r) {\
    return l == r;\
}\
static inline bool T##_neq(T l, T r) {\
    return l != r;\
}\


// T must be the unsigned version of T
#define _internal_make_default_operators_wrapping(T, UT)\
_Static_assert(sizeof(T) == sizeof(UT));\
static inline T T##_add(T l, T r) {\
    u64 c = (UT)l + (UT)r;\
    s64 val;\
    memcpy(&val, &c, sizeof(UT));\
    return val;\
}\
static inline T T##_sub(T l, T r) {\
    return T##_add(l, -r);\
}\
static inline T T##_mul(T l, T r) {\
    return l * r;\
}\
static inline T T##_div(T l, T r) {\
    return l/r;\
}\
\
static inline bool T##_le(T l, T r) {\
    return l < r;\
}\
\
static inline bool T##_ge(T l, T r) {\
    return l > r;\
}\
\
static inline bool T##_leq(T l, T r) {\
    return l <= r;\
}\
\
static inline bool T##_geq(T l, T r) {\
    return l >= r;\
}\
static inline bool T##_eq(T l, T r) {\
    return l == r;\
}\
static inline bool T##_neq(T l, T r) {\
    return l != r;\
}\
static inline T T##_mod(T l, T r) {\
    return l % r;\
}\

_internal_make_default_operators_wrapping(s64, u64);
_internal_make_default_operators(f64);

typedef enum _Base_Type {
    TYPE_BOOL,
    TYPE_S16,
    TYPE_S32,
    TYPE_S64,

    TYPE_F32,
    TYPE_F64,
    TYPE_POINTER,
    TYPE_STRUCT,    
    TYPE_ARRAY,
    TYPE_STRING,
} Base_Type;

typedef struct Type_Info Type_Info;


typedef struct _Type_Vars {
    const Type_Info **infos;
    const usize *offsets;
    isize count;

} Type_Vars;

struct Type_Info {
    const char *name;
    const Base_Type type;
    const Base_Type *inner;
    const Type_Vars vars;
};





extern const Type_Info Type_Info_S64;
extern const Type_Info Type_Info_F64;
extern const Type_Info Type_Info_Bool;
extern const Type_Info Type_Info_String;
extern const Type_Info Type_Info_Pointer;

// [..]
// static const Type_Info Type_Info_Array_Bool = {"[..]bool", TYPE_ARRAY, Type_Info_Bool, NULL};



void print(void *value, const Type_Info *info);


void _crash(const char *, int, const char *);
#define assert(COND, ...) if(!(COND)) {_crash(__FILE__, __LINE__, ##__VA_ARGS__); }



/* template<typename K, typename V> 
struct Map {
    _MapKV<K,V> *map = nullptr;
    Map() {
        hmdefault(map, -2);
    }
    void insert(const K &key, const V &val) {
        hmput(map, key, val);
    }   
    int remove(const K &key) {
        return hmdel(map, key);
    }
    isize len() {
        return hmlen(map);
    }
    _MapKV<K,V> *operator[](const K &key) {
        return hmget(map, key);
    }
    _MapKV<K,V> &idx(isize i) {
        return map[i];
    }
    void destroy() {
        hmfree(map);
    }
*/




#define array_init(a) a = NULL
#define array_add(a, v) arrpush(a,v)
#define array_push(a) arrpush(a,0)
#define array_len(a) arrlen(a)
#define array_cap(a) arrcap(a)
#define array_free(a) arrfree(a)
#define array_resize(a,l) arrsetlen(a,l)
#define array_reserve(a,l) arrsetcap(a,l)

// @todo
#define _internal_new(TYPE, COUNT) (TYPE *)malloc(COUNT * sizeof(TYPE))
#define _internal_delete(PTR) free(PTR)

#define new _internal_new
#define delete _internal_delete

#ifdef GCC

static inline isize _internal_bounds_check(isize i, isize s) {assert(0 <= i && i < s, "out of bounds"); return i;}
#define array_get(a,i) a[_internal_bounds_check(i, array_len(a))]
// gcc exclusive?
// #define array_get(a, i) ({__typeof__(i) _internal_index_##a = i; assert(0 <= _internal_index_##a && _internal_index_##a < array_len(a), "out of bounds"); a[_internal_index_##a];})
#else 

#endif // GCC
#endif // BACKEND_H