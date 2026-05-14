/* ir_defs.h
 * SPDX-License-Identifier: Apache-2.0
 * Copyright 2025 water2137
 */

#ifndef X96_IR_DEFS_H
#define X96_IR_DEFS_H

#include <stdint.h>

/* Virtual Registers */
typedef uint32_t ir_reg_t;
#define IR_REG_INVALID ((ir_reg_t) - 1)

/* IR Opcodes */
typedef enum
{
	/* ALU */
	IR_ADD,
	IR_SUB,
	IR_MUL,
	IR_AND,
	IR_OR,
	IR_XOR,
	IR_SHL,
	IR_SHR,
	IR_SAR,
	IR_CMP,
	IR_EQ,
	IR_NE,
	IR_LT,
	IR_LE,
	IR_GT,
	IR_GE,
	IR_ULT,
	IR_ULE,
	IR_UGT,
	IR_UGE,
	IR_DIV,	 /* Signed Division */
	IR_UDIV, /* Unsigned Division */
	IR_MOD,	 /* Remainder */

	/* Floating Point ALU */
	IR_FADD,
	IR_FSUB,
	IR_FMUL,
	IR_FDIV,
	IR_FCMP, /* General Compare */
	IR_FEQ,
	IR_FNE,
	IR_FLT,
	IR_FLE,
	IR_FGT,
	IR_FGE,

	/* Data Movement */
	IR_MOV,		 /* Register Copy */
	IR_FMOV_IMM, /* Load Float Immediate */

	/* Memory */
	IR_LOAD,  /* Load from RAM */
	IR_STORE, /* Store to RAM */
	IR_LOAD_GLOBAL,
	IR_STORE_GLOBAL,
	IR_LEA,		/* Load effective address */
	IR_GET_GPR, /* Read Arch GPR */
	IR_SET_GPR, /* Write Arch GPR */

	/* Control Flow */
	IR_JUMP,	/* Unconditional Jump */
	IR_BRANCH,	/* Conditional Branch */
	IR_RET,		/* Return */
	IR_EXIT_TB, /* Exit Translation Block */

	/* Lambda Specific */
	IR_GET_ARG,
	IR_OP_LOAD_LOCAL,
	IR_OP_LOAD_GLOBAL,
	IR_OP_MAKE_LAM,
	IR_OP_APPLY,
	IR_OP_MAKE_THUNK,

	/* Constants */
	IR_MOV_IMM, /* Load Immediate */

	/* System */
	IR_CALL_HOST,  /* Call Host Function */
	IR_INLINE_ASM, /* Inline Assembly Container */

	/* Compiler Specific */
	IR_CALL,	  /* Function Call */
	IR_VLA_ALLOC, /* Stack Allocation */

	/* Conversions */
	IR_SEXT,  /* Sign Extend */
	IR_ZEXT,  /* Zero Extend */
	IR_TRUNC, /* Truncate */
	IR_I2F,	  /* Int to Float */
	IR_F2I,	  /* Float to Int */
	IR_F2F,	  /* Float to Float */
} ir_opcode_t;

/* IR Instruction */
typedef struct ir_instr_s
{
	ir_opcode_t op;
	ir_reg_t dst;
	ir_reg_t src1;
	ir_reg_t src2;
	uint64_t imm;  /* For immediate variants */
	char *asm_str; /* For inline asm */
	struct ir_instr_s *next;
	struct ir_instr_s *prev;
} ir_instr_t;

/* Basic Block */
typedef struct ir_block_s
{
	int id;
	ir_instr_t *head;
	ir_instr_t *tail;
	struct ir_block_s *next;
} ir_block_t;

/* Function / Translation Unit */
typedef struct ir_function_s
{
	char *name;
	ir_block_t *blocks;
	int block_count;
	int vreg_count;
	int *vreg_map; /* Maps vreg -> phys_reg */
} ir_function_t;

/* IR Builder API */
ir_function_t *ir_alloc_function(void);
void ir_free_function(ir_function_t *func);
ir_block_t *ir_add_block(ir_function_t *func);
ir_instr_t *ir_emit(ir_block_t *block, ir_opcode_t op, ir_reg_t dst,
					ir_reg_t src1, ir_reg_t src2);
ir_instr_t *ir_emit_imm(ir_block_t *block, ir_opcode_t op, ir_reg_t dst,
						uint64_t imm);

#endif /* X96_IR_DEFS_H */
