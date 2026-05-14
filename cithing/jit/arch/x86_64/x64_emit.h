/* x64_emit.h
 * SPDX-License-Identifier: Apache-2.0
 * Copyright 2025 water2137
 */

#ifndef X96_X64_EMIT_H
#define X96_X64_EMIT_H

#include <stdint.h>

typedef struct
{
	uint8_t *ptr;
} x64_buf_t;

static inline void x64_emit8(x64_buf_t *b, uint8_t val)
{
	*b->ptr++ = val;
}

static inline void x64_emit32(x64_buf_t *b, uint32_t val)
{
	*(uint32_t *)b->ptr = val;
	b->ptr += 4;
}

static inline void x64_emit64(x64_buf_t *b, uint64_t val)
{
	*(uint64_t *)b->ptr = val;
	b->ptr += 8;
}

/* REX prefix: 0x40 | W R X B */
#define REX_W (1 << 3)
#define REX_R (1 << 2)
#define REX_X (1 << 1)
#define REX_B (1 << 0)

static inline void x64_rex(x64_buf_t *b, uint8_t flags)
{
	x64_emit8(b, 0x40 | flags);
}

/* ModRM: mod(2) | reg(3) | rm(3) */
static inline void x64_modrm(x64_buf_t *b, uint8_t mod, uint8_t reg, uint8_t rm)
{
	x64_emit8(b, (mod << 6) | ((reg & 7) << 3) | (rm & 7));
}

/* Common Instructions */
static inline void x64_mov_reg_imm64(x64_buf_t *b, uint8_t reg, uint64_t imm)
{
	x64_rex(b, REX_W | ((reg >> 3) & 1));
	x64_emit8(b, 0xB8 | (reg & 7));
	x64_emit64(b, imm);
}

static inline void x64_ret(x64_buf_t *b)
{
	x64_emit8(b, 0xC3);
}

#endif /* X96_X64_EMIT_H */
