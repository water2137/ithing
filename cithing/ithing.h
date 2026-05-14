#ifndef ITHING_H
#define ITHING_H

#include "tagging.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef uintptr_t Value;

#define TAG_VAR 0
#define TAG_APP 1
#define TAG_LAM 2
#define TAG_THUNK 3

typedef enum
{
	OP_LOAD_VAR,
	OP_MAKE_LAM,
	OP_CALL,
	OP_RET
} opcode_t;

typedef struct
{
	uint8_t op;
	int32_t arg;
} Instruction_t;

typedef struct
{
	Instruction_t *instructions;
	int count;
	int param_id;
} LambdaBody_t;

typedef struct
{
	LambdaBody_t *lambdas;
	int lambda_count;
} Module_t;

typedef struct env_s
{
	int id;
	Value val;
	struct env_s *next;
} Env_t;

typedef struct
{
	int lambda_idx;
	Module_t *module;
	Env_t *env;
	void *compiled_code;
} VLam_t;

typedef struct
{
	Value f;
	Value a;
} VApp_t;

typedef enum
{
	EXPR_VAR,
	EXPR_LAM,
	EXPR_APP
} expr_type_t;
typedef struct expr_s
{
	expr_type_t type;
	union
	{
		int var_idx;
		struct
		{
			int param_id;
			struct expr_s *body;
		} lam;
		struct
		{
			struct expr_s *f;
			struct expr_s *a;
		} app;
	};
	void *bytecode;
} Expr_t;

typedef struct
{
	Value result;
	Expr_t *expr;
	Env_t *env;
} VThunk_t;

typedef struct cpu_s
{
	Value *stack;
	int stack_top;
	void *jit_context;
	Module_t *current_module;
	Env_t *global_env;
} cpu_t;

#define STACK_PUSH(cpu, v) (cpu->stack[cpu->stack_top++] = (v))
#define STACK_POP(cpu) (cpu->stack[--cpu->stack_top])

/* FFI Exports */
cpu_t *c_init();
Expr_t *mk_var(int idx);
Expr_t *mk_lam(int param_id, Expr_t *body);
Expr_t *mk_app(Expr_t *f, Expr_t *a);

void c_register_global(cpu_t *cpu, int id, Expr_t *expr);
Value c_eval(cpu_t *cpu, Expr_t *expr);

uintptr_t c_get_tag(Value v);
int c_get_var_idx(Value v);
Value c_get_app_f(Value v);
Value c_get_app_a(Value v);
int c_get_lam_idx(Value v);

char *c_decode(cpu_t *cpu, Value v);
char *c_quote(cpu_t *cpu, Value v);

/* Internal API needed by JIT */
Value vm_exec(cpu_t *cpu, int lambda_idx, Env_t *env);
Value force(cpu_t *cpu, Value v);
Value apply(cpu_t *cpu, Value f, Value a);

#endif
