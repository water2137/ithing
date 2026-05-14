#include "ithing.h"
#include <stdio.h>
#include <stdlib.h>

void print_value(Value v)
{
	uintptr_t tag = GET_TAG(v);
	void *ptr = UNTAG_PTR(v);

	switch (tag)
	{
	case TAG_VAR:
		printf("v%d", (int)(uintptr_t)ptr);
		break;
	case TAG_APP:
	{
		VApp_t *app = (VApp_t *)ptr;
		printf("(");
		print_value(app->f);
		printf(" ");
		print_value(app->a);
		printf(")");
		break;
	}
	case TAG_LAM:
	{
		VLam_t *lam = (VLam_t *)ptr;
		printf("(\\x%d -> ...)", lam->lambda_idx);
		break;
	}
	case TAG_THUNK:
		printf("<thunk>");
		break;
	default:
		printf("<unknown:%lu>", (unsigned long)tag);
	}
}
