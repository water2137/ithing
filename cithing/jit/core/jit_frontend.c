#include "ithing.h"
#include "jit/ir_defs.h"
#include "jit/jit_context.h"
#include <stdio.h>
#include <stdlib.h>

/* Host helpers for JITed code to call */
Value jit_helper_load_var(cpu_t *cpu, int id, Env_t *env);
Value jit_helper_make_lam(cpu_t *cpu, int lambda_idx, Env_t *env);
Value jit_helper_apply(cpu_t *cpu, Value f, Value a);

ir_function_t *jit_translate_block(jit_context_t *jit, uintptr_t lambda_idx)
{
	ir_function_t *func = ir_alloc_function();
	ir_block_t *block = ir_add_block(func);
	LambdaBody_t *lb = &jit->cpu->current_module->lambdas[lambda_idx];

	/*
	 * JITed Function Signature:
	 * Value func(cpu_t *cpu, Value arg, Env_t *captured_env)
	 *
	 * Mapping to IR regs (Virtual):
	 * v0: cpu (rdi)
	 * v1: arg (rsi)
	 * v2: captured_env (rdx)
	 */

	ir_reg_t r_env = func->vreg_count++;
	ir_emit(block, IR_GET_ARG, r_env, 2, 0);

	ir_reg_t stack[64];
	int sp = 0;

	for (int pc = 0; pc < lb->count; pc++)
	{
		Instruction_t inst = lb->instructions[pc];
		switch (inst.op)
		{
		case OP_LOAD_VAR:
		{
			ir_reg_t res = func->vreg_count++;
			ir_emit_imm(block, IR_MOV_IMM, func->vreg_count, inst.arg);
			ir_emit(block, IR_OP_LOAD_VAR, res, func->vreg_count, r_env);
			func->vreg_count++;
			stack[sp++] = res;
			break;
		}
		case OP_MAKE_LAM:
		{
			ir_reg_t res = func->vreg_count++;
			ir_emit_imm(block, IR_MOV_IMM, func->vreg_count, inst.arg);
			ir_emit(block, IR_OP_MAKE_LAM, res, func->vreg_count, r_env);
			func->vreg_count++;
			stack[sp++] = res;
			break;
		}
		case OP_CALL:
		{
			sp -= 2;
			ir_reg_t res = func->vreg_count++;
			ir_emit(block, IR_OP_APPLY, res, stack[sp + 1], stack[sp]);
			stack[sp++] = res;
			break;
		}
		case OP_RET:
		{
			ir_reg_t res = stack[--sp];
			ir_emit(block, IR_RET, IR_REG_INVALID, res, 0);
			break;
		}
		}
	}

	return func;
}
