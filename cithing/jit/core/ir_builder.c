#include "jit/ir_defs.h"
#include <stdlib.h>
#include <string.h>

ir_function_t *ir_alloc_function(void)
{
	ir_function_t *func = calloc(1, sizeof(ir_function_t));
	return func;
}

void ir_free_function(ir_function_t *func)
{
	if (!func)
	{
		return;
	}
	ir_block_t *block = func->blocks;
	while (block)
	{
		ir_instr_t *instr = block->head;
		while (instr)
		{
			ir_instr_t *next = instr->next;
			if (instr->asm_str)
			{
				free(instr->asm_str);
			}
			free(instr);
			instr = next;
		}
		ir_block_t *next_block = block->next;
		free(block);
		block = next_block;
	}
	if (func->name)
	{
		free(func->name);
	}
	if (func->vreg_map)
	{
		free(func->vreg_map);
	}
	free(func);
}

ir_block_t *ir_add_block(ir_function_t *func)
{
	ir_block_t *block = calloc(1, sizeof(ir_block_t));
	block->id = func->block_count++;

	if (!func->blocks)
	{
		func->blocks = block;
	}
	else
	{
		ir_block_t *curr = func->blocks;
		while (curr->next)
		{
			curr = curr->next;
		}
		curr->next = block;
	}
	return block;
}

ir_instr_t *ir_emit(ir_block_t *block, ir_opcode_t op, ir_reg_t dst,
					ir_reg_t src1, ir_reg_t src2)
{
	ir_instr_t *instr = calloc(1, sizeof(ir_instr_t));
	instr->op = op;
	instr->dst = dst;
	instr->src1 = src1;
	instr->src2 = src2;

	if (!block->head)
	{
		block->head = instr;
		block->tail = instr;
	}
	else
	{
		block->tail->next = instr;
		instr->prev = block->tail;
		block->tail = instr;
	}
	return instr;
}

ir_instr_t *ir_emit_imm(ir_block_t *block, ir_opcode_t op, ir_reg_t dst,
						__uint128_t imm)
{
	ir_instr_t *instr = ir_emit(block, op, dst, IR_REG_INVALID, IR_REG_INVALID);
	instr->imm = imm;
	return instr;
}
