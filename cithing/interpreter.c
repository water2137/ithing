#include "ithing.h"
#include "jit/jit_context.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Expr_t *mk_var(int idx)
{
	Expr_t *e = malloc(sizeof(Expr_t));
	e->type = EXPR_VAR;
	e->var_idx = idx;
	e->bytecode = NULL;
	return e;
}

Expr_t *mk_lam(int param_id, Expr_t *body)
{
	Expr_t *e = malloc(sizeof(Expr_t));
	e->type = EXPR_LAM;
	e->lam.param_id = param_id;
	e->lam.body = body;
	e->bytecode = NULL;
	return e;
}

Expr_t *mk_app(Expr_t *f, Expr_t *a)
{
	Expr_t *e = malloc(sizeof(Expr_t));
	e->type = EXPR_APP;
	e->app.f = f;
	e->app.a = a;
	e->bytecode = NULL;
	return e;
}

int register_lambda(Module_t *mod, Expr_t *lam_expr)
{
	int idx = mod->lambda_count++;
	mod->lambdas =
		realloc(mod->lambdas, sizeof(LambdaBody_t) * mod->lambda_count);
	mod->lambdas[idx].param_id = lam_expr->lam.param_id;
	mod->lambdas[idx].instructions = malloc(sizeof(Instruction_t) * 1024);
	mod->lambdas[idx].count = 0;

	void emit(Expr_t * e)
	{
		switch (e->type)
		{
		case EXPR_VAR:
			mod->lambdas[idx].instructions[mod->lambdas[idx].count++] =
				(Instruction_t){OP_LOAD_VAR, e->var_idx};
			break;
		case EXPR_APP:
			emit(e->app.a);
			emit(e->app.f);
			mod->lambdas[idx].instructions[mod->lambdas[idx].count++] =
				(Instruction_t){OP_CALL, 0};
			break;
		case EXPR_LAM:
		{
			int inner_idx = register_lambda(mod, e);
			mod->lambdas[idx].instructions[mod->lambdas[idx].count++] =
				(Instruction_t){OP_MAKE_LAM, inner_idx};
			break;
		}
		}
	}
	emit(lam_expr->lam.body);
	mod->lambdas[idx].instructions[mod->lambdas[idx].count++] =
		(Instruction_t){OP_RET, 0};
#ifdef DEBUG
	printf("compiled lambda %d with %d instructions\n", idx,
		   mod->lambdas[idx].count);
#endif
	return idx;
}

cpu_t *c_init()
{
	cpu_t *cpu = calloc(1, sizeof(cpu_t));
	cpu->jit_context = jit_init(cpu);
	cpu->stack = malloc(sizeof(Value) * 8192);
	cpu->stack_top = 0;
	cpu->current_module = calloc(1, sizeof(Module_t));
	cpu->global_env = NULL;
	return cpu;
}

void c_register_global(cpu_t *cpu, int id, Expr_t *expr)
{
	VThunk_t *thunk = malloc(sizeof(VThunk_t));
	thunk->result = 0;
	thunk->expr = expr;
	thunk->env = cpu->global_env;

	Env_t *new_node = malloc(sizeof(Env_t));
	new_node->id = id;
	new_node->val = (Value)TAG_PTR(thunk, TAG_THUNK);
	new_node->next = cpu->global_env;
	cpu->global_env = new_node;
}

Value c_eval(cpu_t *cpu, Expr_t *expr)
{
	int idx = register_lambda(
		cpu->current_module,
		&(Expr_t){.type = EXPR_LAM, .lam = {.param_id = -1, .body = expr}});
	return vm_exec(cpu, idx, cpu->global_env);
}

uintptr_t c_get_tag(Value v)
{
	return GET_TAG(v);
}
int c_get_var_idx(Value v)
{
	return (int)(uintptr_t)UNTAG_PTR(v);
}
Value c_get_app_f(Value v)
{
	return ((VApp_t *)UNTAG_PTR(v))->f;
}
Value c_get_app_a(Value v)
{
	return ((VApp_t *)UNTAG_PTR(v))->a;
}
int c_get_lam_idx(Value v)
{
	return ((VLam_t *)UNTAG_PTR(v))->lambda_idx;
}

Value jit_helper_load_var(cpu_t *cpu, int id, Env_t *env)
{
	Env_t *curr = env;
	while (curr)
	{
		if (curr->id == id)
		{
			return force(cpu, curr->val);
		}
		curr = curr->next;
	}
	curr = cpu->global_env;
	while (curr)
	{
		if (curr->id == id)
		{
			return force(cpu, curr->val);
		}
		curr = curr->next;
	}
	return (Value)TAG_PTR(((uintptr_t)id) << 3, TAG_VAR);
}

Value jit_helper_make_lam(cpu_t *cpu, int lambda_idx, Env_t *env)
{
	VLam_t *vlam = malloc(sizeof(VLam_t));
	vlam->lambda_idx = lambda_idx;
	vlam->module = cpu->current_module;
	vlam->env = env;
	vlam->compiled_code = jit_get_block(cpu->jit_context, lambda_idx);

	if (!vlam->compiled_code)
	{
		jit_context_t *jit = cpu->jit_context;
		if (++jit->hotness[lambda_idx % 4096] > 50)
		{
			ir_function_t *func = jit_translate_block(jit, lambda_idx);
			vlam->compiled_code = jit->backend->compile(jit, func);
			jit_add_block(jit, lambda_idx, vlam->compiled_code);
			ir_free_function(func);
		}
	}
	return (Value)TAG_PTR(vlam, TAG_LAM);
}

Value jit_helper_apply(cpu_t *cpu, Value f, Value a)
{
	return apply(cpu, f, a);
}

Value vm_exec(cpu_t *cpu, int lambda_idx, Env_t *env)
{
	Instruction_t *instructions =
		cpu->current_module->lambdas[lambda_idx].instructions;
	int count = cpu->current_module->lambdas[lambda_idx].count;
	int pc = 0;
#ifdef DEBUG
	printf("vm_exec lambda %d, instructions: %d\n", lambda_idx, count);
#endif
	while (pc < count)
	{
		Instruction_t inst = instructions[pc++];
#ifdef DEBUG
		printf("  executing op %d with arg %d\n", inst.op, inst.arg);
#endif
		switch (inst.op)
		{
		case OP_LOAD_VAR:
			STACK_PUSH(cpu, jit_helper_load_var(cpu, inst.arg, env));
			break;
		case OP_MAKE_LAM:
			STACK_PUSH(cpu, jit_helper_make_lam(cpu, inst.arg, env));
			break;
		case OP_CALL:
		{
			Value f = STACK_POP(cpu);
			Value a = STACK_POP(cpu);
			STACK_PUSH(cpu, apply(cpu, f, a));
			break;
		}
		case OP_RET:
			return STACK_POP(cpu);
		}
	}
	return 0;
}

Value apply(cpu_t *cpu, Value f, Value a)
{
	f = force(cpu, f);
	if (GET_TAG(f) == TAG_LAM)
	{
		VLam_t *vlam = (VLam_t *)UNTAG_PTR(f);
		Env_t *new_env = malloc(sizeof(Env_t));
		new_env->id = vlam->module->lambdas[vlam->lambda_idx].param_id;
		new_env->val = a;
		new_env->next = vlam->env;
		if (vlam->compiled_code)
		{
			typedef Value (*jit_fn)(cpu_t *, Value, Env_t *);
			return ((jit_fn)vlam->compiled_code)(cpu, a, new_env);
		}
		return vm_exec(cpu, vlam->lambda_idx, new_env);
	}
	VApp_t *vapp = malloc(sizeof(VApp_t));
	vapp->f = f;
	vapp->a = a;
	return (Value)TAG_PTR(vapp, TAG_APP);
}

typedef struct
{
	char *buf;
	size_t size;
	size_t cap;
} Buffer_t;

static void buf_printf(Buffer_t *b, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	int needed = vsnprintf(NULL, 0, fmt, args);
	va_end(args);

	if (b->size + needed >= b->cap)
	{
		b->cap = (b->cap + needed) * 2;
		b->buf = realloc(b->buf, b->cap);
	}
	va_start(args, fmt);
	vsnprintf(b->buf + b->size, b->cap - b->size, fmt, args);
	va_end(args);
	b->size += needed;
}

static void c_quote_to_buf(cpu_t *cpu, int l, Value v, Buffer_t *b)
{
	v = force(cpu, v);
	uintptr_t tag = GET_TAG(v);
	void *ptr = UNTAG_PTR(v);

	switch (tag)
	{
	case TAG_VAR:
	{
		int idx = (int)((intptr_t)ptr >> 3);
		if (idx < 0)
		{
			buf_printf(b, "x%d", -idx - 1);
		}
		else
		{
			buf_printf(b, "v%d", idx);
		}
		break;
	}
	case TAG_APP:
	{
		VApp_t *app = (VApp_t *)ptr;
		buf_printf(b, "(");
		c_quote_to_buf(cpu, l, app->f, b);
		buf_printf(b, " ");
		c_quote_to_buf(cpu, l, app->a, b);
		buf_printf(b, ")");
		break;
	}
	case TAG_LAM:
	{
		int i = -l - 1;
		Value var = (Value)TAG_PTR(((uintptr_t)i) << 3, TAG_VAR);
		Value res = apply(cpu, v, var);
		buf_printf(b, "(\\x%d -> ", l);
		c_quote_to_buf(cpu, l + 1, res, b);
		buf_printf(b, ")");
		break;
	}
	case TAG_THUNK:
		buf_printf(b, "<thunk>");
		break;
	default:
		buf_printf(b, "<unknown_tag:%lu val:%p>", (unsigned long)tag, ptr);
		break;
	}
}

static int c_as_church_numeral(cpu_t *cpu, Value v)
{
	v = force(cpu, v);
	if (GET_TAG(v) != TAG_LAM)
	{
		return -1;
	}
	Value f_tok = (Value)TAG_PTR(((uintptr_t)-1000) << 3, TAG_VAR);
	Value x_tok = (Value)TAG_PTR(((uintptr_t)-1001) << 3, TAG_VAR);
	Value res1 = apply(cpu, v, f_tok);
	res1 = force(cpu, res1);
	if (GET_TAG(res1) != TAG_LAM)
	{
		return -1;
	}
	Value res2 = apply(cpu, res1, x_tok);
	int count = 0;
	while (1)
	{
		res2 = force(cpu, res2);
		if (GET_TAG(res2) == TAG_VAR &&
			(int)((intptr_t)UNTAG_PTR(res2) >> 3) == -1001)
		{
			return count;
		}
		if (GET_TAG(res2) == TAG_APP)
		{
			VApp_t *app = (VApp_t *)UNTAG_PTR(res2);
			Value f = force(cpu, app->f);
			if (GET_TAG(f) == TAG_VAR &&
				(int)((intptr_t)UNTAG_PTR(f) >> 3) == -1000)
			{
				count++;
				res2 = app->a;
				continue;
			}
		}
		return -1;
	}
}

char *c_quote(cpu_t *cpu, Value v)
{
	Buffer_t b = {.buf = malloc(1024), .size = 0, .cap = 1024};
	c_quote_to_buf(cpu, 0, v, &b);
	return b.buf;
}

char *c_decode(cpu_t *cpu, Value v)
{
	int n = c_as_church_numeral(cpu, v);
	if (n != -1)
	{
		char *r = malloc(32);
		sprintf(r, "%d", n);
		return r;
	}
	Buffer_t b = {.buf = malloc(1024), .size = 0, .cap = 1024};
	c_quote_to_buf(cpu, 0, v, &b);
	return b.buf;
}

Value force(cpu_t *cpu, Value v)
{
	if (GET_TAG(v) != TAG_THUNK)
	{
		return v;
	}
	VThunk_t *t = (VThunk_t *)UNTAG_PTR(v);
	if (t->result)
	{
		return t->result;
	}
	t->result = c_eval(cpu, t->expr);
	return t->result;
}
