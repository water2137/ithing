#ifndef CPU_OFFSETS_H
#define CPU_OFFSETS_H

#include "ithing.h"
#include <stddef.h>

#define CPU_OFF_STACK offsetof(cpu_t, stack)
#define CPU_OFF_STACK_PTR offsetof(cpu_t, stack_ptr)
#define CPU_OFF_HEAP offsetof(cpu_t, heap)

#endif
