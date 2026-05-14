/* jit_context.h
 * SPDX-License-Identifier: Apache-2.0
 * Copyright 2025 water2137
 */

#ifndef X96_JIT_CONTEXT_H
#define X96_JIT_CONTEXT_H

#include "ithing.h"
#include "jit_backend.h"
#include <stddef.h>
#include <stdint.h>

#define JIT_CACHE_SIZE (32 * 1024 * 1024) /* 32 MiB */

typedef struct jit_block_s
{
	uintptr_t guest_addr;
	void *host_addr;
	struct jit_block_s *next; /* Hash bucket collision */
} jit_block_t;

typedef struct jit_context_s
{
	const jit_backend_t *backend;
	void *code_cache;
	size_t cache_used;
	jit_block_t *block_map[4096]; /* Simple hash map */
	uint32_t hotness[4096];		  /* Hotness counters */
	cpu_t *cpu;
} jit_context_t;

jit_context_t *jit_init(cpu_t *cpu);
void jit_shutdown(jit_context_t *jit);
void *jit_get_block(jit_context_t *jit, uintptr_t addr);
void jit_add_block(jit_context_t *jit, uintptr_t addr, void *host_addr);

ir_function_t *jit_translate_block(jit_context_t *jit, uintptr_t addr);

#endif /* X96_JIT_CONTEXT_H */
