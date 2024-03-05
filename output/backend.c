#include "backend.h"
#include <stdio.h>
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"


const Type_Info Type_Info_S64 = {"s64", TYPE_S64, NULL, {0}};
const Type_Info Type_Info_F64 = {"f64", TYPE_F64, NULL, {0}};
const Type_Info Type_Info_Bool = {"bool", TYPE_BOOL, NULL, {0}};
const Type_Info Type_Info_Pointer = {"*", TYPE_POINTER, NULL, {0}};
const Type_Info Type_Info_String = {"string", TYPE_STRING, NULL, {0}};

void _print(const void *value, const Type_Info *info) {
    if(!info) {
        printf("no type info passed for printing\n");
        return;
    }
    switch(info->type) {
        case TYPE_S64: {
            printf("%ld", *(s64 *)value);
            break;
        }
        case TYPE_F64: {
            printf("%2.4f", *(f64 *)value);
            break;
        }
        case TYPE_STRING: {
            String s = *(String *)value;
            if(s == NULL) {
                printf("\"\"");
            } else {
                printf("\"%s\"", s);
            }
            break;
        }
        case TYPE_BOOL: {
            bool v = *(bool *)value;
            printf("%s", v? "true" : "false");
            break;
        }
        case TYPE_POINTER: {
            void *ptr = *(void **)value;
            if(ptr == NULL) {
                printf("nil");
            } else {
                printf("%p", ptr);
            }
            break;
        }
        case TYPE_STRUCT: {
            printf("%s {", info->name);
            for(isize i = 0; i < info->vars.count; ++i) {
                usize off = info->vars.offsets[i];
                const Type_Info *var_info = info->vars.infos[i];                
                _print((value + off), var_info);
                if(i < info->vars.count-1) {
                    printf(", ");
                }                
            }
            printf("}");
            break;
        }
        default: {
            // assert(false, "unkown type for printing");
            printf("unkown type for printing");
            break;
        }
    }    
}
// @todo
void print_n(void **values, const Type_Info **infos, isize count) {
    
}
void print(void *value, const Type_Info *info) {
    _print(value, info);
    printf("\n");
}



void _crash(const char *file_name, int line, const char *msg) {
	printf("\n-----------CRASH-----------");
	printf("\nerror: %s", msg);
	printf("\nfile:  %s", file_name);
	printf("\nline:  %d", line);
	printf("\n---------------------------\n");
    fflush(stdout);
 	abort();
}