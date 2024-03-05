#include "backend.h"

// declarations

struct Object; typedef struct Object Object;
struct Entity; typedef struct Entity Entity;


void count_even_odd(s64 * a);
void proc_struct_arg(Object obj);
void array_test();
void pointer_pointer_proc(s64 * * p);
s64 fibonacci(s64 n);
void test_sort();
void proc_struct_arg_ptr(Object * obj);
void defer_early();
void _main();
bool pointer_proc(s64 * p);
s64 mul_value(s64 value);
s64 float_stuff(s64 a,f64 b);
void selection_sort(s64 * arr);
Object return_struct();
void empty_proc();
void empty_proc_implicit();
void matrix();
void pass_array(s64 * p);
void test_strings();
void struct_in_struct();


// definitions

struct Object {
s64 value;
f64 float_value;
String name;

};
struct Entity {
Object obj;
s64 index;
bool is_object;
Entity * ptr;

};


// type infos
const Type_Info Type_Info_Object;
const Type_Info Type_Info_Entity;

const Type_Info Type_Info_Object = { "Object", TYPE_STRUCT, NULL, {
            (const Type_Info *[]){ &Type_Info_S64, &Type_Info_F64, &Type_Info_String,  },
            (const usize []){ offsetof(Object, value), offsetof(Object, float_value), offsetof(Object, name),  },
            3
        }
};
const Type_Info Type_Info_Entity = { "Entity", TYPE_STRUCT, NULL, {
            (const Type_Info *[]){ &Type_Info_Object, &Type_Info_S64, &Type_Info_Bool, &Type_Info_Pointer,  },
            (const usize []){ offsetof(Entity, obj), offsetof(Entity, index), offsetof(Entity, is_object), offsetof(Entity, ptr),  },
            4
        }
};


void count_even_odd(s64 * a) {
s64 even = {0};
s64 odd = {0};
{
 for(isize i = 0; i < array_len(a); ++i) { 
 s64 * it = &array_get(a, i); {
if(s64_eq(s64_mod(*it, 2), 0)) {
even = s64_add(even, 1);
continue;
}
odd = s64_add(odd, 1);
}
 }
}
}
void proc_struct_arg(Object obj) {
obj.value = 2;
return ;
}
void array_test() {
s64 * arr = {0};
array_add(arr,4);
s64 f = array_get(arr,(s64_sub(1, 1)));
print(&f, &Type_Info_S64);
array_add(arr,13);
pass_array(arr);
s64 val = {0};
s64 * ptr = &val;
}
void pointer_pointer_proc(s64 * * p) {
return ;
}
s64 fibonacci(s64 n) {
if(s64_leq(n, 0)) {
return 0;
}
if(s64_eq(n, 1)) {
return 1;
}
return s64_add(fibonacci(s64_sub(n, 1)), fibonacci(s64_sub(n, 2)));
}
void test_sort() {
String line = "~~~~~~~~~~~~~~~test sort~~~~~~~~~~~~~~~~~~~";
print(&line, &Type_Info_String);
s64 * arr = {0};
{
 for(isize i = 0; i < 10; ++i) { {
array_add(arr,s64_sub(10, i));
} }
}
selection_sort(arr);
{
 for(isize _ = 0; _ < array_len(arr); ++_) { 
 s64 * it = &array_get(arr, _); {
s64 value = *it;
print(&value, &Type_Info_S64);
}
 }
}
line = "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
print(&line, &Type_Info_String);
}
void proc_struct_arg_ptr(Object * obj) {
obj->value = 2;
Object v = *obj;
print(&v, &Type_Info_Object);
return ;
}
void defer_early() {
;{
s64 a = 100001;
print(&a, &Type_Info_S64);
};
 return ;
; {
s64 a = 100001;
print(&a, &Type_Info_S64);
}}
void _main() {
s64 fibo = fibonacci(10);
print(&fibo, &Type_Info_S64);
test_sort();
struct_in_struct();
array_test();
Object obj = {0};
mul_value(2);
obj.float_value = 100;
proc_struct_arg_ptr(&obj);
s64 index = s64_mul(s64_mul(10, 3), (s64_add(5, 3)));
s64 just_type = {0};
s64 a = s64_sub(s64_add(s64_sub(5, 2), index), 4);
pointer_proc(&a);
f64 fval = 1;
a = s64_add(a, s64_mul(float_stuff(1,2.5), -1));
a = mul_value(s64_mul(a, 1));
if(s64_le(a, 2)) {
print(&a, &Type_Info_S64);
}
else if(s64_eq(a, 2)) {
print(&a, &Type_Info_S64);
}
else {
print(&a, &Type_Info_S64);
}
matrix();
; {
s64 a = 1000001;
print(&a, &Type_Info_S64);
}{
String s = "last line printed";
print(&s, &Type_Info_String);
}}
bool pointer_proc(s64 * p) {
*p = *p;
return true;
}
s64 mul_value(s64 value) {
return s64_mul(value, 2);
}
s64 float_stuff(s64 a,f64 b) {
f64 some_long_name = 1;
f64 s = 2;
s64 some_var = 2;
return some_var;
}
void selection_sort(s64 * arr) {
{
 for(isize i = 0; i < array_len(arr); ++i) { 
 s64 * it = &array_get(arr, i); {
s64 min = i;
{
 for(isize j = s64_add(i, 1); j < array_len(arr); ++j) { {
if(s64_le(array_get(arr,(j)), array_get(arr,(min)))) {
min = j;
}
} }
}
if(s64_neq(min, i)) {
s64 val = array_get(arr,(min));
array_get(arr,(min)) = array_get(arr,(i));
array_get(arr,(i)) = val;
}
}
 }
}
}
Object return_struct() {
Object obj = {0};
return obj;
}
void empty_proc() {
}
void empty_proc_implicit() {
}
void matrix() {
s64 * * mat = {0};
array_push(mat);
array_add(array_get(mat,(0)),23);
array_add(array_get(mat,(0)),46);
s64 val = array_get(array_get(mat,(0)),(1));
print(&val, &Type_Info_S64);
}
void pass_array(s64 * p) {
array_get(p,(0)) = 1;
s64 i = 0;
while(s64_le(i, 3)) {
array_add(p,i);
i = s64_add(i, 1);
}
{
 for(isize index = 0; index < array_len(p); ++index) { 
 s64 * it = &array_get(p, index); {
s64 val = s64_mul(array_get(p,(index)), *it);
print(&val, &Type_Info_S64);
}
 }
}
return ;
}
void test_strings() {
String name = "a name for my string";
print(&name, &Type_Info_String);
}
void struct_in_struct() {
Entity entity = {0};
entity.obj.value = 3;
entity.obj.float_value = 4.5;
entity.index = 2;
entity.is_object = true;
print(&entity, &Type_Info_Entity);
entity.obj.name = "a name";
entity.ptr = &entity;
print(&entity, &Type_Info_Entity);
}
