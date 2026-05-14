#include "jit/jit_ir.h"
#include <limits.h>
#include <stdlib.h>
#include <string.h>

typedef struct
{
	int vreg;
	int start;
	int end;
	int phys_reg_idx;
} live_interval_t;

static int compare_intervals(const void *a, const void *b)
{
	const live_interval_t *ia = (const live_interval_t *)a;
	const live_interval_t *ib = (const live_interval_t *)b;
	return ia->start - ib->start;
}

static void compute_liveness(ir_function_t *func, live_interval_t *intervals)
{
	int i;
	int inst_id = 0;
	ir_block_t *block = func->blocks;

	for (i = 0; i < func->vreg_count; i++)
	{
		intervals[i].vreg = i;
		intervals[i].start = INT_MAX;
		intervals[i].end = -1;
		intervals[i].phys_reg_idx = -1;
	}

	while (block)
	{
		ir_instr_t *instr = block->head;
		while (instr)
		{
			inst_id += 2;

			if (instr->src1 != IR_REG_INVALID &&
				instr->src1 < (unsigned)func->vreg_count)
			{
				if (intervals[instr->src1].start > inst_id)
				{
					intervals[instr->src1].start = inst_id;
				}
				if (intervals[instr->src1].end < inst_id)
				{
					intervals[instr->src1].end = inst_id;
				}
			}
			if (instr->src2 != IR_REG_INVALID &&
				instr->src2 < (unsigned)func->vreg_count)
			{
				if (intervals[instr->src2].start > inst_id)
				{
					intervals[instr->src2].start = inst_id;
				}
				if (intervals[instr->src2].end < inst_id)
				{
					intervals[instr->src2].end = inst_id;
				}
			}
			if (instr->dst != IR_REG_INVALID &&
				instr->dst < (unsigned)func->vreg_count)
			{
				if (intervals[instr->dst].start > inst_id)
				{
					intervals[instr->dst].start = inst_id;
				}
				if (intervals[instr->dst].end < inst_id)
				{
					intervals[instr->dst].end = inst_id;
				}
			}
			instr = instr->next;
		}
		block = block->next;
	}
}

void jit_allocate_registers(ir_function_t *func, int num_regs)
{
	live_interval_t *intervals;
	live_interval_t **sorted_ptrs;
	live_interval_t **active;
	int *hints;
	int i, j;
	int active_count = 0;
	int count = 0;
	ir_block_t *block;

	if (func->vreg_count == 0 || num_regs <= 0)
	{
		return;
	}

	func->vreg_map = malloc(func->vreg_count * sizeof(int));
	intervals = malloc(func->vreg_count * sizeof(live_interval_t));
	sorted_ptrs = malloc(func->vreg_count * sizeof(live_interval_t *));
	active = malloc(num_regs * sizeof(live_interval_t *));
	hints = malloc(func->vreg_count * sizeof(int));

	if (!func->vreg_map || !intervals || !sorted_ptrs || !active || !hints)
	{
		if (func->vreg_map)
		{
			free(func->vreg_map);
			func->vreg_map = NULL;
		}
		if (intervals)
		{
			free(intervals);
		}
		if (sorted_ptrs)
		{
			free(sorted_ptrs);
		}
		if (active)
		{
			free(active);
		}
		if (hints)
		{
			free(hints);
		}
		return;
	}

	for (i = 0; i < func->vreg_count; i++)
	{
		hints[i] = -1;
	}

	/* Scan instructions to generate register hints */
	block = func->blocks;
	while (block)
	{
		ir_instr_t *instr = block->head;
		while (instr)
		{
			if (instr->dst != IR_REG_INVALID && instr->src1 != IR_REG_INVALID)
			{
				if (instr->dst < (unsigned)func->vreg_count)
				{
					hints[instr->dst] = instr->src1;
				}
			}
			instr = instr->next;
		}
		block = block->next;
	}

	compute_liveness(func, intervals);

	for (i = 0; i < func->vreg_count; i++)
	{
		func->vreg_map[i] = -1;
		if (intervals[i].start != INT_MAX)
		{
			sorted_ptrs[count++] = &intervals[i];
		}
	}

	qsort(sorted_ptrs, count, sizeof(live_interval_t *), compare_intervals);

	for (i = 0; i < num_regs; i++)
	{
		active[i] = NULL;
	}

	for (i = 0; i < count; i++)
	{
		live_interval_t *current = sorted_ptrs[i];

		/* Expire old intervals from active set */
		for (j = 0; j < num_regs; j++)
		{
			if (active[j] && active[j]->end < current->start)
			{
				active[j] = NULL;
				active_count--;
			}
		}

		if (active_count < num_regs)
		{
			int assigned_reg = -1;
			int hint_vreg = hints[current->vreg];

			if (hint_vreg != -1 && hint_vreg < func->vreg_count)
			{
				int hint_phys = func->vreg_map[hint_vreg];
				if (hint_phys != -1 && active[hint_phys] == NULL)
				{
					assigned_reg = hint_phys;
				}
			}

			if (assigned_reg == -1)
			{
				for (j = 0; j < num_regs; j++)
				{
					if (active[j] == NULL)
					{
						assigned_reg = j;
						break;
					}
				}
			}

			if (assigned_reg != -1)
			{
				active[assigned_reg] = current;
				current->phys_reg_idx = assigned_reg;
				func->vreg_map[current->vreg] = assigned_reg;
				active_count++;
			}
		}
		else
		{
			/* Spill the interval that ends last */
			int spill_idx = -1;
			int max_end = current->end;

			for (j = 0; j < num_regs; j++)
			{
				if (active[j] && active[j]->end > max_end)
				{
					max_end = active[j]->end;
					spill_idx = j;
				}
			}

			if (spill_idx != -1)
			{
				live_interval_t *spilled = active[spill_idx];
				func->vreg_map[spilled->vreg] = -1;
				spilled->phys_reg_idx = -1;

				active[spill_idx] = current;
				current->phys_reg_idx = spill_idx;
				func->vreg_map[current->vreg] = spill_idx;
			}
			else
			{
				func->vreg_map[current->vreg] = -1;
			}
		}
	}

	free(active);
	free(sorted_ptrs);
	free(intervals);
	free(hints);
}
