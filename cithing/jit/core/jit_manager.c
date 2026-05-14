#include "ithing.h"
#include "jit/jit_context.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

jit_context_t *jit_init(cpu_t *cpu)
{
	jit_context_t *jit = (jit_context_t *)malloc(sizeof(jit_context_t));
	if (!jit)
	{
		return NULL;
	}
	memset(jit, 0, sizeof(jit_context_t));
	jit->cpu = cpu;

	/* Select backend (hardcoded for now) */
	jit->backend = &jit_backend_x86_64;

	/* Allocate Code Cache */
	jit->code_cache =
		mmap(NULL, JIT_CACHE_SIZE, PROT_READ | PROT_WRITE | PROT_EXEC,
			 MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (jit->code_cache == MAP_FAILED)
	{
		free(jit);
		return NULL;
	}

	if (jit->backend->init)
	{
		if (!jit->backend->init(jit))
		{
			munmap(jit->code_cache, JIT_CACHE_SIZE);
			free(jit);
			return NULL;
		}
	}

	return jit;
}

void jit_shutdown(jit_context_t *jit)
{
	if (!jit)
	{
		return;
	}
	if (jit->backend->destroy)
	{
		jit->backend->destroy(jit);
	}
	munmap(jit->code_cache, JIT_CACHE_SIZE);
	/* Free block map entries */
	int i;
	for (i = 0; i < 4096; i++)
	{
		jit_block_t *curr = jit->block_map[i];
		while (curr)
		{
			jit_block_t *next = curr->next;
			free(curr);
			curr = next;
		}
	}
	free(jit);
}

void *jit_get_block(jit_context_t *jit, uintptr_t addr)
{
	uint64_t hash = (addr ^ (addr >> 12)) & 0xFFF;
	jit_block_t *curr = jit->block_map[hash];
	while (curr)
	{
		if (curr->guest_addr == addr)
		{
			return curr->host_addr;
		}
		curr = curr->next;
	}
	return NULL;
}

void jit_add_block(jit_context_t *jit, uintptr_t addr, void *host_addr)
{
	uint64_t hash = (addr ^ (addr >> 12)) & 0xFFF;
	jit_block_t *block = (jit_block_t *)malloc(sizeof(jit_block_t));
	if (!block)
	{
		return;
	}
	block->guest_addr = addr;
	block->host_addr = host_addr;
	block->next = jit->block_map[hash];
	jit->block_map[hash] = block;
}
