/* jit_backend.h
 * SPDX-License-Identifier: Apache-2.0
 * Copyright 2025 water2137
 */

#ifndef X96_JIT_BACKEND_H
#define X96_JIT_BACKEND_H

#include "jit_ir.h"
#include <stddef.h>

/* Forward declaration */
struct jit_context_s;
typedef struct cpu_s cpu_t;

typedef struct jit_backend_s
{
	const char *name;

	/* Initialize the backend */
	bool (*init)(struct jit_context_s *ctx);

	/* Clean up resources */
	void (*destroy)(struct jit_context_s *ctx);

	/* Compile IR Function to Machine Code
	 * Returns pointer to entry point, or NULL on failure
	 */
	void *(*compile)(struct jit_context_s *ctx, ir_function_t *func);

	/* Patch a jump at 'jump_site' to target 'target_addr'
	 * Used for Lazy Block Chaining
	 */
	void (*patch_jump)(void *jump_site, void *target_addr);

} jit_backend_t;

typedef void (*jit_block_entry_t)(cpu_t *cpu);

/* Global Registry */
extern const jit_backend_t jit_backend_x86_64;

#endif /* X96_JIT_BACKEND_H */
