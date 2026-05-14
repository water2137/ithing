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

/* x86_64 Register Numbers */
#define X64_RAX 0
#define X64_RCX 1
#define X64_RDX 2
#define X64_RBX 3
#define X64_RSP 4
#define X64_RBP 5
#define X64_RSI 6
#define X64_RDI 7
#define X64_R8 8
#define X64_R9 9
#define X64_R10 10
#define X64_R11 11
#define X64_R12 12
#define X64_R13 13
#define X64_R14 14
#define X64_R15 15

/*
 * x86_64 Register Mapping for JIT:
 * rcx is RESERVED as scratch.
 * rbx: cpu_t* (fixed, callee-save)
 * rbp: frame pointer (fixed, callee-save)
 * rsp: stack pointer
 */
#define NUM_PHYS_REGS 12
static const uint8_t reg_to_x64[NUM_PHYS_REGS] = {
	X64_RAX, X64_RDX, X64_RSI, X64_RDI, X64_R8,	 X64_R9,
	X64_R10, X64_R11, X64_R12, X64_R13, X64_R14, X64_R15};

static int x86_64_init(struct jit_context_s *ctx)
{
	(void)ctx;
	return 1;
}

static void x86_64_destroy(struct jit_context_s *ctx)
{
	(void)ctx;
}

static void x64_emit_rex(x64_buf_t *b, int w, int reg, int rm)
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

static void x64_get_vreg_to_x64(x64_buf_t *b, ir_function_t *func, int vreg,
								int dst_x64)
{
	int phys = func->vreg_map[vreg];
	if (phys != -1)
	{
		int src_x64 = reg_to_x64[phys];
		if (src_x64 == dst_x64)
		{
			return;
		}
		x64_emit_rex(b, 1, src_x64, dst_x64);
		x64_emit8(b, 0x89);
		x64_modrm(b, 3, src_x64, dst_x64);
	}
	else
	{
		/* mov dst, [rbp - disp] */
		x64_emit_rex(b, 1, dst_x64, X64_RBP);
		x64_emit8(b, 0x8B);
		x64_modrm(b, 1, dst_x64, X64_RBP);
		x64_emit8(b, (uint8_t)(-(int)((vreg + 1) * 8)));
	}
}

static void x64_put_vreg_from_x64(x64_buf_t *b, ir_function_t *func, int vreg,
								  int src_x64)
{
	int phys = func->vreg_map[vreg];
	if (phys != -1)
	{
		int dst_x64 = reg_to_x64[phys];
		if (src_x64 == dst_x64)
		{
			return;
		}
		x64_emit_rex(b, 1, src_x64, dst_x64);
		x64_emit8(b, 0x89);
		x64_modrm(b, 3, src_x64, dst_x64);
	}
	else
	{
		/* mov [rbp - disp], src */
		x64_emit_rex(b, 1, src_x64, X64_RBP);
		x64_emit8(b, 0x89);
		x64_modrm(b, 1, src_x64, X64_RBP);
		x64_emit8(b, (uint8_t)(-(int)((vreg + 1) * 8)));
	}
}

extern Value force(cpu_t *cpu, Value v);
extern Value jit_helper_make_lam(cpu_t *cpu, int lambda_idx, Env_t *env);
extern Value jit_helper_apply(cpu_t *cpu, Value f, Value a);

static void x64_emit_host_call(x64_buf_t *b, ir_function_t *func,
							   ir_instr_t *instr, void *helper)
{
	/* Save managed registers */
	for (int i = 0; i < NUM_PHYS_REGS; i++)
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

	/* Argument 1: cpu (rdi) from rbx */
	x64_rex(b, REX_W);
	x64_emit8(b, 0x89);
	x64_emit8(b, 0xDF); /* mov rdi, rbx */

	/* Argument 2: rsi */
	if (instr->src1 != IR_REG_INVALID)
	{
		int phys = func->vreg_map[instr->src1];
		if (phys != -1)
		{
			int offset = (NUM_PHYS_REGS - 1 - phys) * 8;
			x64_emit_rex(b, 1, X64_RSI, X64_RSP);
			x64_emit8(b, 0x8B);
			x64_emit8(b, 0x74);
			x64_emit8(b, 0x24);
			x64_emit8(b, offset);
		}
		else
		{
			int disp = (instr->src1 + 1) * 8;
			x64_emit_rex(b, 1, X64_RSI, X64_RBP);
			x64_emit8(b, 0x8B);
			x64_modrm(b, 1, X64_RSI, X64_RBP);
			x64_emit8(b, -disp);
		}
	}

	/* Argument 3: rdx */
	if (instr->src2 != IR_REG_INVALID)
	{
		int phys = func->vreg_map[instr->src2];
		if (phys != -1)
		{
			int offset = (NUM_PHYS_REGS - 1 - phys) * 8;
			x64_emit_rex(b, 1, X64_RDX, X64_RSP);
			x64_emit8(b, 0x8B);
			x64_emit8(b, 0x54);
			x64_emit8(b, 0x24);
			x64_emit8(b, offset);
		}
		else
		{
			int disp = (instr->src2 + 1) * 8;
			x64_emit_rex(b, 1, X64_RDX, X64_RBP);
			x64_emit8(b, 0x8B);
			x64_modrm(b, 1, X64_RDX, X64_RBP);
			x64_emit8(b, -disp);
		}
	}

	x64_mov_reg_imm64(b, X64_RAX, (uint64_t)helper);
	x64_emit8(b, 0xFF);
	x64_emit8(b, 0xD0); /* call rax */

	/* Store result to dst if needed */
	if (instr->dst != IR_REG_INVALID)
	{
		int dst_idx = func->vreg_map[instr->dst];
		if (dst_idx != -1)
		{
			int offset = (NUM_PHYS_REGS - 1 - dst_idx) * 8;
			x64_emit_rex(b, 1, X64_RAX, X64_RSP);
			x64_emit8(b, 0x89);
			x64_emit8(b, 0x44);
			x64_emit8(b, 0x24);
			x64_emit8(b, offset);
		}
		else
		{
			int disp = (instr->dst + 1) * 8;
			x64_emit_rex(b, 1, X64_RAX, X64_RBP);
			x64_emit8(b, 0x89);
			x64_modrm(b, 1, X64_RAX, X64_RBP);
			x64_emit8(b, -disp);
		}
	}

	/* Restore managed registers */
	for (int i = NUM_PHYS_REGS - 1; i >= 0; i--)
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

	jit_allocate_registers(func, NUM_PHYS_REGS);

	/* Prologue */
	x64_emit8(&b, 0x55); /* push rbp */
	x64_emit_rex(&b, 1, X64_RSP, X64_RBP);
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
	x64_emit_rex(&b, 1, X64_RDI, X64_RBX);
	x64_emit8(&b, 0x89);
	x64_emit8(&b, 0xFB); /* mov rbx, rdi */

	stack_size = (func->vreg_count + 1) * 8;
	stack_size = (stack_size + 15) & ~15;
	x64_emit_rex(&b, 1, 0, X64_RSP);
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
				int src_x64 = (arg_idx == 1) ? X64_RSI : X64_RDX;
				int dst_idx = func->vreg_map[instr->dst];
				if (dst_idx != -1)
				{
					int dst_x64 = reg_to_x64[dst_idx];
					if (dst_x64 != src_x64)
					{
						x64_emit_rex(&b, 1, src_x64, dst_x64);
						x64_emit8(&b, 0x89);
						x64_modrm(&b, 3, src_x64, dst_x64);
					}
				}
				else
				{
					x64_put_vreg_from_x64(&b, func, instr->dst, src_x64);
				}
				break;
			}
			case IR_OP_LOAD_LOCAL:
			{
				int depth = (int)instr->imm;
				int dst_idx = (func->vreg_map[instr->dst] != -1)
								  ? func->vreg_map[instr->dst]
								  : 0;
				int dst_x64 = reg_to_x64[dst_idx];

				x64_get_vreg_to_x64(&b, func, instr->src1, dst_x64);

				while (depth--)
				{
					/* mov dst, [dst + 8] (next) */
					x64_emit_rex(&b, 1, dst_x64, dst_x64);
					x64_emit8(&b, 0x8B);
					x64_modrm(&b, 1, dst_x64, dst_x64);
					x64_emit8(&b, 8);
				}
				/* mov dst, [dst + 0] (val) */
				x64_emit_rex(&b, 1, dst_x64, dst_x64);
				x64_emit8(&b, 0x8B);
				x64_modrm(&b, 0, dst_x64, dst_x64);

				if (func->vreg_map[instr->dst] == -1)
				{
					x64_put_vreg_from_x64(&b, func, instr->dst, dst_x64);
				}
				break;
			}
			case IR_OP_LOAD_GLOBAL:
			{
				int gid = (int)instr->imm;
				int dst_idx = (func->vreg_map[instr->dst] != -1)
								  ? func->vreg_map[instr->dst]
								  : 0;
				int dst_x64 = reg_to_x64[dst_idx];

				/* Load from global_vals into dst_x64 */
				x64_emit_rex(&b, 1, X64_RBX, dst_x64);
				x64_emit8(&b, 0x8B);
				x64_modrm(&b, 1, dst_x64, X64_RBX);
				x64_emit8(&b, CPU_OFFSET_GLOBAL_VALS);

				x64_emit_rex(&b, 1, dst_x64, dst_x64);
				x64_emit8(&b, 0x8B);
				if (gid * 8 < 128)
				{
					x64_modrm(&b, 1, dst_x64, dst_x64);
					x64_emit8(&b, gid * 8);
				}
				else
				{
					x64_modrm(&b, 2, dst_x64, dst_x64);
					x64_emit32(&b, gid * 8);
				}

				/* Fast Path: Check if TAG_THUNK (3) using RCX as scratch */
				x64_emit_rex(&b, 1, dst_x64, X64_RCX);
				x64_emit8(&b, 0x89);
				x64_modrm(&b, 3, dst_x64, X64_RCX); /* mov rcx, dst */

				x64_emit_rex(&b, 1, 0, X64_RCX);
				x64_emit8(&b, 0x83);
				x64_emit8(&b, 0xE1);
				x64_emit8(&b, 0x03); /* and rcx, 3 */

				x64_emit_rex(&b, 1, 0, X64_RCX);
				x64_emit8(&b, 0x83);
				x64_emit8(&b, 0xF9);
				x64_emit8(&b, 0x03); /* cmp rcx, 3 */
				x64_emit8(&b, 0x75); /* jne .not_thunk */
				uint8_t *patch_not_thunk = b.ptr++;

				/* It IS a thunk. Check if already evaluated. */
				x64_emit_rex(&b, 1, dst_x64, X64_RCX);
				x64_emit8(&b, 0x89);
				x64_modrm(&b, 3, dst_x64, X64_RCX); /* mov rcx, dst */

				x64_emit_rex(&b, 1, 0, X64_RCX);
				x64_emit8(&b, 0x83);
				x64_emit8(&b, 0xE1);
				x64_emit8(&b, 0xF8); /* and rcx, ~7 */

				x64_emit_rex(&b, 1, X64_RCX, X64_RCX);
				x64_emit8(&b, 0x8B);
				x64_modrm(&b, 0, X64_RCX,
						  X64_RCX); /* mov rcx, [rcx] (t->result) */

				x64_emit_rex(&b, 1, X64_RCX, X64_RCX);
				x64_emit8(&b, 0x85);
				x64_modrm(&b, 3, X64_RCX, X64_RCX); /* test rcx, rcx */
				x64_emit8(&b, 0x74);				/* je .slow_path */
				uint8_t *patch_need_force = b.ptr++;

				/* Thunk already evaluated: mov dst, rcx */
				x64_emit_rex(&b, 1, X64_RCX, dst_x64);
				x64_emit8(&b, 0x89);
				x64_modrm(&b, 3, X64_RCX, dst_x64);
				x64_emit8(&b, 0xEB); /* jmp .done */
				uint8_t *patch_fast_done = b.ptr++;

				/* Slow Path: call force(cpu, dst) */
				*patch_need_force = (uint8_t)(b.ptr - (patch_need_force + 1));
				instr->src1 = instr->dst;
				x64_emit_host_call(&b, func, instr, (void *)force);
				instr->src1 = IR_REG_INVALID;
				x64_emit8(&b, 0xEB); /* jmp .done */
				uint8_t *patch_slow_done = b.ptr++;

				*patch_not_thunk = (uint8_t)(b.ptr - (patch_not_thunk + 1));
				*patch_fast_done = (uint8_t)(b.ptr - (patch_fast_done + 1));
				*patch_slow_done = (uint8_t)(b.ptr - (patch_slow_done + 1));

				if (func->vreg_map[instr->dst] == -1)
				{
					x64_put_vreg_from_x64(&b, func, instr->dst, dst_x64);
				}
				break;
			}
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
					x64_put_vreg_from_x64(&b, func, instr->dst, dst_x64);
				}
				break;
			}
			case IR_ADD:
			{
				int dst_idx = (func->vreg_map[instr->dst] != -1)
								  ? func->vreg_map[instr->dst]
								  : 0;
				int dst_x64 = reg_to_x64[dst_idx];
				x64_get_vreg_to_x64(&b, func, instr->src1, dst_x64);
				x64_get_vreg_to_x64(&b, func, instr->src2,
									X64_RCX); /* use rcx as scratch */
				x64_emit_rex(&b, 1, X64_RCX, dst_x64);
				x64_emit8(&b, 0x01);
				x64_modrm(&b, 3, X64_RCX, dst_x64);
				if (func->vreg_map[instr->dst] == -1)
				{
					x64_put_vreg_from_x64(&b, func, instr->dst, dst_x64);
				}
				break;
			}
			case IR_RET:
			{
				if (instr->src1 != IR_REG_INVALID)
				{
					x64_get_vreg_to_x64(&b, func, instr->src1, X64_RAX);
				}
				/* Epilogue */
				x64_emit_rex(&b, 1, 0, X64_RSP);
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
