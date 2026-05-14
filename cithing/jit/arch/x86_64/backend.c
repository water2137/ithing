/* backend.c
 * SPDX-License-Identifier: Apache-2.0
 * Copyright 2025 water2137
 */

#include "cpu_offsets.h"
#include "jit/jit_backend.h"
#include "jit/jit_context.h"
#include "x64_emit.h"
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>

/*
 * x86_64 Register Mapping for JIT:
 * 0: rax
 * 1: rcx
 * 2: rdx
 * 3: rsi
 * 4: rdi
 * 5: r8
 * 6: r9
 * 7: r10
 * 8: r11
 * 9: r12 (callee-save)
 * 10: r13 (callee-save)
 * 11: r14 (callee-save)
 * 12: r15 (callee-save)
 *
 * Fixed:
 * rbx: cpu_t* (callee-save)
 * rbp: frame pointer (callee-save)
 * rsp: stack pointer
 */

static const uint8_t reg_to_x64[] = {0,	 1,	 2,	 6,	 7,	 8, 9,
									 10, 11, 12, 13, 14, 15};

static bool x86_64_init(struct jit_context_s *ctx)
{
	(void)ctx;
	return true;
}

static void x86_64_destroy(struct jit_context_s *ctx)
{
	(void)ctx;
}

static void x64_emit_rex(x64_buf_t *b, bool w, int reg, int rm)
{
	uint8_t f = 0;
	if (w)
	{
		f |= REX_W;
	}
	if (reg >= 8)
	{
		f |= REX_R;
	}
	if (rm >= 8)
	{
		f |= REX_B;
	}
	if (f || reg >= 8 || rm >= 8)
	{
		x64_rex(b, f);
	}
}

static void x64_get_vreg(x64_buf_t *b, ir_function_t *func, int vreg,
						 int phys_dst_idx)
{
	int phys = func->vreg_map[vreg];
	int dst_x64 = reg_to_x64[phys_dst_idx];
	if (phys != -1)
	{
		int src_x64 = reg_to_x64[phys];
		if (src_x64 == dst_x64)
		{
			return;
		}
		x64_emit_rex(b, true, dst_x64, src_x64);
		x64_emit8(b, 0x89);
		x64_modrm(b, 3, src_x64, dst_x64);
	}
	else
	{
		/* mov dst, [rbp - disp] */
		x64_emit_rex(b, true, dst_x64, 5); /* rbp=5 */
		x64_emit8(b, 0x8B);
		x64_modrm(b, 1, dst_x64, 5);
		x64_emit8(b, (uint8_t)(-(int)((vreg + 1) * 8)));
	}
}

static void x64_put_vreg(x64_buf_t *b, ir_function_t *func, int vreg,
						 int phys_src_idx)
{
	int phys = func->vreg_map[vreg];
	int src_x64 = reg_to_x64[phys_src_idx];
	if (phys != -1)
	{
		int dst_x64 = reg_to_x64[phys];
		if (src_x64 == dst_x64)
		{
			return;
		}
		x64_emit_rex(b, true, src_x64, dst_x64);
		x64_emit8(b, 0x89);
		x64_modrm(b, 3, src_x64, dst_x64);
	}
	else
	{
		/* mov [rbp - disp], src */
		x64_emit_rex(b, true, src_x64, 5);
		x64_emit8(b, 0x89);
		x64_modrm(b, 1, src_x64, 5);
		x64_emit8(b, (uint8_t)(-(int)((vreg + 1) * 8)));
	}
}

extern Value jit_helper_load_var(cpu_t *cpu, int id, Env_t *env);
extern Value jit_helper_make_lam(cpu_t *cpu, int lambda_idx, Env_t *env);
extern Value jit_helper_apply(cpu_t *cpu, Value f, Value a);

static void x64_emit_host_call(x64_buf_t *b, ir_function_t *func,
							   ir_instr_t *instr, void *helper)
{
	for (int i = 0; i < 13; i++)
	{
		int reg = reg_to_x64[i];
		if (reg >= 8)
		{
			x64_emit8(b, 0x41);
			x64_emit8(b, 0x50 + (reg - 8));
		}
		else
		{
			x64_emit8(b, 0x50 + reg);
		}
	}

	x64_rex(b, REX_W);
	x64_emit8(b, 0x89);
	x64_emit8(b, 0xDF);

	if (instr->src1 != IR_REG_INVALID)
	{
		int phys = func->vreg_map[instr->src1];
		if (phys != -1)
		{
			int offset = (12 - phys) * 8;
			x64_emit_rex(b, true, 6, 4);
			x64_emit8(b, 0x8B);
			x64_emit8(b, 0x74);
			x64_emit8(b, 0x24);
			x64_emit8(b, offset);
		}
		else
		{
			int disp = (instr->src1 + 1) * 8;
			x64_emit_rex(b, true, 6, 5);
			x64_emit8(b, 0x8B);
			x64_modrm(b, 1, 6, 5);
			x64_emit8(b, -disp);
		}
	}

	if (instr->src2 != IR_REG_INVALID)
	{
		int phys = func->vreg_map[instr->src2];
		if (phys != -1)
		{
			int offset = (12 - phys) * 8;
			x64_emit_rex(b, true, 2, 4);
			x64_emit8(b, 0x8B);
			x64_emit8(b, 0x54);
			x64_emit8(b, 0x24);
			x64_emit8(b, offset);
		}
		else
		{
			int disp = (instr->src2 + 1) * 8;
			x64_emit_rex(b, true, 2, 5);
			x64_emit8(b, 0x8B);
			x64_modrm(b, 1, 2, 5);
			x64_emit8(b, -disp);
		}
	}

	x64_mov_reg_imm64(b, 0, (uint64_t)helper);
	x64_emit8(b, 0xFF);
	x64_emit8(b, 0xD0);

	if (instr->dst != IR_REG_INVALID)
	{
		int dst_idx = func->vreg_map[instr->dst];
		if (dst_idx != -1)
		{
			int offset = (12 - dst_idx) * 8;
			x64_emit_rex(b, true, 0, 4);
			x64_emit8(b, 0x89);
			x64_emit8(b, 0x44);
			x64_emit8(b, 0x24);
			x64_emit8(b, offset);
		}
		else
		{
			int disp = (instr->dst + 1) * 8;
			x64_emit_rex(b, true, 0, 5);
			x64_emit8(b, 0x89);
			x64_modrm(b, 1, 0, 5);
			x64_emit8(b, -disp);
		}
	}

	for (int i = 12; i >= 0; i--)
	{
		int reg = reg_to_x64[i];
		if (reg >= 8)
		{
			x64_emit8(b, 0x41);
			x64_emit8(b, 0x58 + (reg - 8));
		}
		else
		{
			x64_emit8(b, 0x58 + reg);
		}
	}
}

static void *x86_64_compile(struct jit_context_s *ctx, ir_function_t *func)
{
	jit_context_t *jit = (jit_context_t *)ctx;
	uint8_t *code_start = (uint8_t *)jit->code_cache + jit->cache_used;
	x64_buf_t b = {code_start};
	ir_block_t *block = func->blocks;
	int stack_size;

	jit_allocate_registers(func, 13);

	/* Prologue */
	x64_emit8(&b, 0x55); /* push rbp */
	x64_rex(&b, REX_W);
	x64_emit8(&b, 0x89);
	x64_emit8(&b, 0xE5); /* mov rbp, rsp */

	/* Save callee-saved registers */
	x64_emit8(&b, 0x53); /* push rbx */
	x64_emit8(&b, 0x41);
	x64_emit8(&b, 0x54); /* push r12 */
	x64_emit8(&b, 0x41);
	x64_emit8(&b, 0x55); /* push r13 */
	x64_emit8(&b, 0x41);
	x64_emit8(&b, 0x56); /* push r14 */
	x64_emit8(&b, 0x41);
	x64_emit8(&b, 0x57); /* push r15 */

	/* Load cpu_t* into rbx (first argument is rdi) */
	x64_rex(&b, REX_W);
	x64_emit8(&b, 0x89);
	x64_emit8(&b, 0xFB); /* mov rbx, rdi */

	stack_size = (func->vreg_count + 1) * 8;
	stack_size = (stack_size + 15) & ~15;
	x64_rex(&b, REX_W);
	x64_emit8(&b, 0x81);
	x64_emit8(&b, 0xEC);
	x64_emit32(&b, stack_size);

	while (block)
	{
		ir_instr_t *instr = block->head;
		while (instr)
		{
			switch (instr->op)
			{
			case IR_GET_ARG:
			{
				int arg_idx = instr->src1;
				int src_x64 = (arg_idx == 1) ? 6 : 2; /* 1=rsi, 2=rdx */
				int dst_idx = func->vreg_map[instr->dst];
				if (dst_idx != -1)
				{
					int dst_x64 = reg_to_x64[dst_idx];
					if (dst_x64 != src_x64)
					{
						x64_emit_rex(&b, true, dst_x64, src_x64);
						x64_emit8(&b, 0x89);
						x64_modrm(&b, 3, src_x64, dst_x64);
					}
				}
				else
				{
					int disp = (instr->dst + 1) * 8;
					x64_emit_rex(&b, true, src_x64, 5);
					x64_emit8(&b, 0x89);
					x64_modrm(&b, 1, src_x64, 5);
					x64_emit8(&b, -disp);
				}
				break;
			}
			case IR_OP_LOAD_VAR:
				x64_emit_host_call(&b, func, instr,
								   (void *)jit_helper_load_var);
				break;
			case IR_OP_MAKE_LAM:
				x64_emit_host_call(&b, func, instr,
								   (void *)jit_helper_make_lam);
				break;
			case IR_OP_APPLY:
				x64_emit_host_call(&b, func, instr, (void *)jit_helper_apply);
				break;
			case IR_MOV_IMM:
			{
				int dst_idx = (func->vreg_map[instr->dst] != -1)
								  ? func->vreg_map[instr->dst]
								  : 0;
				int dst_x64 = reg_to_x64[dst_idx];
				x64_mov_reg_imm64(&b, dst_x64, (uint64_t)instr->imm);
				if (func->vreg_map[instr->dst] == -1)
				{
					x64_put_vreg(&b, func, instr->dst, 0);
				}
				break;
			}
			case IR_ADD:
			{
				int dst_idx = (func->vreg_map[instr->dst] != -1)
								  ? func->vreg_map[instr->dst]
								  : 0;
				int dst_x64 = reg_to_x64[dst_idx];
				x64_get_vreg(&b, func, instr->src1, dst_idx);
				x64_get_vreg(&b, func, instr->src2, 1); /* use rcx as scratch */
				x64_emit_rex(&b, true, dst_x64, reg_to_x64[1]);
				x64_emit8(&b, 0x01);
				x64_modrm(&b, 3, reg_to_x64[1], dst_x64);
				if (func->vreg_map[instr->dst] == -1)
				{
					x64_put_vreg(&b, func, instr->dst, dst_idx);
				}
				break;
			}
			case IR_RET:
			{
				if (instr->src1 != IR_REG_INVALID)
				{
					x64_get_vreg(&b, func, instr->src1, 0);
				}
				/* Epilogue */
				x64_rex(&b, REX_W);
				x64_emit8(&b, 0x81);
				x64_emit8(&b, 0xC4);
				x64_emit32(&b, stack_size);

				x64_emit8(&b, 0x41);
				x64_emit8(&b, 0x5F); /* pop r15 */
				x64_emit8(&b, 0x41);
				x64_emit8(&b, 0x5E); /* pop r14 */
				x64_emit8(&b, 0x41);
				x64_emit8(&b, 0x5D); /* pop r13 */
				x64_emit8(&b, 0x41);
				x64_emit8(&b, 0x5C); /* pop r12 */
				x64_emit8(&b, 0x5B); /* pop rbx */
				x64_emit8(&b, 0x5D); /* pop rbp */
				x64_ret(&b);
				break;
			}
			default:
				break;
			}

			instr = instr->next;
		}
		block = block->next;
	}

	jit->cache_used += (b.ptr - code_start);
	return code_start;
}

static void x86_64_patch_jump(void *jump_site, void *target_addr)
{
	(void)jump_site;
	(void)target_addr;
}

const jit_backend_t jit_backend_x86_64 = {"x86_64", x86_64_init, x86_64_destroy,
										  x86_64_compile, x86_64_patch_jump};
