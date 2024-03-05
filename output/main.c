#include "backend.h"
#include "emitted.h"

#include "stdio.h"
// const Type_Info Type_Info_Object = {"Object", TYPE_STRUCT, NULL, 
//  {
//     (const Type_Info *[]){&Type_Info_S64, &Type_Info_F64}, 
//     (const usize []){offsetof(Object, value), offsetof(Object, float_value)},
//     2,
//  }
// };

void test_structures();

int main(int, char **) {
    // test_structures();
    printf("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    _main();
    printf("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\nfinished successfully \n");
    return 0;
}




void test_structures() {
    printf("\n~~~~~~~~~~~~~~~~~~TEST_STRUCTURES~~~~~~~~~~~~~~~~~~~\n");
    s64 _max = s64_add(INT64_MAX, 1);
    printf("%ld %d\n", _max, _max == INT64_MIN);    
    // usize i = 0?: 2;
    int _a = 1, _b = 2;
    int **arr = (int * []){ &_a, &_b};
    Object obj = {200, 1.5};
    print(&obj, &Type_Info_Object);
    printf("%d %d\n", *arr[0], *arr[1]);    
    
    typedef struct {
        s64 first;
        f64 second;
        s64 third;
    } Struct;
    Struct s = {0}; // doesn't work in C++
    printf("%ld %2.4f %ld\n", s.first, s.second, s.third);    
    Struct *a;
    array_init(a);
    array_add(a, ((Struct){0,0.5}));
    array_add(a, ((Struct){1,1.5}));
    array_add(a, ((Struct){2,2.5}));
    for(int i = 0; i < array_len(a); ++i) {
        printf("%ld\n", a[i].first);
    }
}