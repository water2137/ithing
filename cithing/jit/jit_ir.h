/* jit_ir.h
 * SPDX-License-Identifier: Apache-2.0
 * Copyright 2025 water2137
 */

#ifndef X96_JIT_IR_H
#define X96_JIT_IR_H

#include "ir_defs.h"

void jit_allocate_registers(ir_function_t *func, int num_regs);

#endif /* X96_JIT_IR_H */
