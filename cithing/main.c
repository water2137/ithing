#include "ithing.h"
#include "jit/jit_context.h"
#include <stdio.h>
#include <stdlib.h>

/* Forward declaration from interpreter.c */
void run_interpreter(cpu_t *cpu, Expr_t *program);

Expr_t *mk_var(int idx)
{
	Expr_t *e = malloc(sizeof(Expr_t));
	e->type = EXPR_VAR;
	e->var_idx = idx;
	return e;
}

Expr_t *mk_lam(int param_id, Expr_t *body)
{
	Expr_t *e = malloc(sizeof(Expr_t));
	e->type = EXPR_LAM;
	e->lam.param_id = param_id;
	e->lam.body = body;
	return e;
}

Expr_t *mk_app(Expr_t *f, Expr_t *a)
{
	Expr_t *e = malloc(sizeof(Expr_t));
	e->type = EXPR_APP;
	e->app.f = f;
	e->app.a = a;
	return e;
}

int main()
{
	cpu_t *cpu = calloc(1, sizeof(cpu_t));
	cpu->jit_context = jit_init(cpu);

	/* Create a simple program: (\x -> x) (\y -> y) */
	/* id = \x -> x */
	Expr_t *id_lam = mk_lam(1, mk_var(1));
	/* prog = id id */
	Expr_t *prog = mk_app(id_lam, id_lam);

	printf("cithing: starting...\n");
	run_interpreter(cpu, prog);

	jit_shutdown(cpu->jit_context);
	free(cpu);
	return 0;
}
