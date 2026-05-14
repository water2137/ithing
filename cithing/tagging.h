#ifndef TAGGING_H
#define TAGGING_H
#include <assert.h>
#include <stdalign.h>
#include <stddef.h>
#include <stdint.h>

#if UINTPTR_MAX == 0xFFFFFFFFFFFFFFFFULL
#define TAG_BITS 3
#elif UINTPTR_MAX == 0xFFFFFFFF
#define TAG_BITS 2
#else
#error "unsupported bitness"
#endif

#define TAG_MASK ((1ULL << TAG_BITS) - 1)
#define UNTAG_MASK (~(uintptr_t)TAG_MASK)

_Static_assert(
	((uintptr_t)1 << TAG_BITS) <= alignof(max_align_t),
	"alignment mismatch: TAG_BITS is too wide for this architecture");

/* Do not use TAG_PTR raw */
#define TAG_PTR(ptr, tag) ((void *)(((uintptr_t)(ptr)) | ((tag) & TAG_MASK)))

#define SAFE_TAG_PTR(ptr, tag)                                                 \
	(assert(((uintptr_t)(tag) & ~TAG_MASK) == 0), TAG_PTR(ptr, tag))

#define STATIC_CHECK_TAG(tag)                                                  \
	_Static_assert((tag) <= TAG_MASK,                                          \
				   "tag value is too large for allocated bits")

#define UNTAG_PTR(ptr) ((void *)(((uintptr_t)(ptr)) & UNTAG_MASK))

#define GET_TAG(ptr) (((uintptr_t)(ptr)) & TAG_MASK)
#endif
