/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1992-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, ECRC.
 * 
 * END LICENSE BLOCK */
/*---------------------------------------------------------------------
 * IDENTIFICATION	memory.h
 *
 * AUTHOR		Joachim Schimpf
 *
 * DESCRIPTION		see alloc.c shared_mem.c private_mem.c lock.s
 *
 * USAGE:		include this file, link with libshm.a
 *---------------------------------------------------------------------*/

#ifndef ECLIPSE_MEMMAN_H
#define ECLIPSE_MEMMAN_H

#ifdef _WIN32
#ifndef DLLEXP
#define DLLEXP __declspec(dllexport)
#endif
#else
#define DLLEXP
#endif

/*---------------------------------------------------------------------
 * Size-dependent values
 *---------------------------------------------------------------------*/

#if (SIZEOF_INT == 4)
typedef unsigned int	bits32;			/* exactly 32 bit */
#else
#error "No code for dealing with sizeof(int) != 4"
#endif

#ifndef ECLIPSE_TYPEDEF_WORD
#if (SIZEOF_CHAR_P == SIZEOF_INT)
typedef int		word;			/* pointer-sized */
typedef unsigned int	uword;
#elif (SIZEOF_CHAR_P == SIZEOF_LONG)
typedef long		word;			/* pointer-sized */
typedef unsigned long	uword;
#elif (defined(HAVE_LONG_LONG) || defined(__GNUC__)) && \
   (SIZEOF_CHAR_P == __SIZEOF_LONG_LONG__)
typedef long long 		word;		/* pointer-sized */
typedef unsigned long long 	uword;
#elif (defined(HAVE___INT64) && SIZEOF_CHAR_P == 8)
typedef __int64          word;
typedef unsigned __int64 uword;
#else
#error "No code for dealing with word size > long long/__int64!"
#endif
#endif


#if (SIZEOF_CHAR_P == 8)
/* Maximal representable address divided by bits per byte. */
/* For -taso we address only the 32-bit memory */
#define MAX_ADDRESS_BYTE	0x20000000
#else
#define MAX_ADDRESS_BYTE	0x20000000
#endif


/* the unit of allocation */

typedef union
{
    struct
    {
	word a1, a2;	/* seems resonable not to allocate smaller */
    }		p;
    double	d;	/* may be same or less than 2 pointer sizes */
} unit_type;

/*---------------------------------------------------------------------
 * Logical page manager
 *---------------------------------------------------------------------*/

#define BYTES_PER_UNIT		sizeof(unit_type)
#define BYTES_PER_PAGE		4096	/* logical page size =< physical */
#define UNITS_PER_PAGE		(BYTES_PER_PAGE/BYTES_PER_UNIT)
#define WORDS_PER_PAGE		(BYTES_PER_PAGE/sizeof(bits32))
#define BITMAP_BLOCKSIZE	BYTES_PER_PAGE
#if (SIZEOF_CHAR_P == 4)
/* Maximal representable address divided by bits per byte. */
#define MAX_ADDRESS_BYTE	0x20000000
#define BITMAP_BLOCKS		(MAX_ADDRESS_BYTE/BYTES_PER_PAGE/BITMAP_BLOCKSIZE)
#define USE_BITMAPS
#ifndef SIGN_BIT
#define SIGN_BIT		((uword) 0x80000000L)
#endif
#else
#define BITMAP_BLOCKS		1
#undef USE_BITMAPS
#endif
#define PAGE_LISTS		32
#define MIN_OS_PAGE_REQUEST	8	/* min pages to get from OS */

struct cluster {
	struct cluster	*next;
	void		*addr;
	word		size;
	word		dummy;
};

struct page_log {
    void *addr;
    word npages;
};

struct page_admin {
	word		allocated;		/* # pages gotten from OS */
	word		freed;			/* # pages in free list */
	void		*min_addr, *max_addr;
	struct page_log	*log_page;		/* log of more'd pages */
	word		log_idx;
	struct cluster	*free[PAGE_LISTS];	/* free[i]: i-page-clusters */
						/* free[0]: larger clusters */
	bits32		*map[BITMAP_BLOCKS];	/* bitmap of pages (1 = free) */
};


/*---------------------------------------------------------------------
 * Block manager
 *---------------------------------------------------------------------*/

#define HEAP_STAT_ALLOCATED	0
#define HEAP_STAT_USED		1

#define LARGEST_SMALL_BLOCK	7	/* units */
#define SMALLEST_POWER_BLOCK	8	/* units */
#define SMALLEST_PAGE_BLOCK	(BYTES_PER_PAGE/BYTES_PER_UNIT)	/* units */
#define LARGEST_POWER_BLOCK	(SMALLEST_PAGE_BLOCK/2)
#define POWER_FIRST_INDEX	6
#define POWERS			32

struct heap {
	void	 *small_blocks[LARGEST_SMALL_BLOCK+1];
	void	 *powers[POWERS];

	void	*alloc_ptr;
	word	alloc_free;		/* in heap_units */

    /* statistics */

	word	small_allocated[LARGEST_SMALL_BLOCK+1];
	word	powers_allocated[POWERS];
	word	requested,		/* in bytes */
		used,			/* small/power only (in heap_units) */
		allocs,
		small_block_pages,
		power_pages;
};


/*---------------------------------------------------------------------
 * Allocation with headers
 *---------------------------------------------------------------------*/

typedef union mem_header
{
	struct
	{
		struct heap	*magic;
		word	size;
	}	s;
	double	dummy;	       /* force alignment of blocks */
} HEADER;


/*---------------------------------------------------------------------
 * Interrupt disabling (obsolete)
 *---------------------------------------------------------------------*/

#define InterruptsDisabled	0
#define Disable_Int()
#define Enable_Int()
#define InterruptsPending	0
#define Set_Interrupts_Pending()
#define Clr_Interrupts_Pending()


/*---------------------------------------------------------------------
 * Spin Locks
 *---------------------------------------------------------------------*/

#if (defined(_PA_RISC1_0) || defined(_PA_RISC1_1))
typedef int a_mutex_t[4];
#else
typedef int a_mutex_t;
#endif

/*---------------------------------------------------------------------
 * Heap descriptor, lowest level
 *---------------------------------------------------------------------*/

/* The private memory part */

struct heap_descriptor {
	struct shm_desc	*shared_header;	/* NULL for private memory */
	struct page_admin *pages;
	struct heap	*heap;
	int		map_fd;
	void*		(*more)(word,int,struct heap_descriptor*);
	int		(*less)(void*,word,struct heap_descriptor*);
	void		(*panic)(const char*, const char*);
	int		debug_level;
};

/* The shared memory part (only if really shared) */

struct shm_desc {
	void *application_header;	/* must be the first word! */
	char *start;			/* own address */
	char *brk;			/* end of allocated space */
	char *lim;			/* end of the mapped region */
	char *stop;			/* end of the reserved address space */
	int incr;			/* mapping increment in bytes */
	int processes;			/* number of attached processes */
	char *mapfile;			/* file it is mapped to */
	a_mutex_t lock;			/* memory management lock */
	struct heap heap;		/* block manager structure */
	struct page_admin pages;	/* page manager structure */
	char mapfile_buf[1024];		/* string buffer for mapfile name */
};


/*---------------------------------------------------------------------
 * Simplified private heap interface
 *---------------------------------------------------------------------*/

extern struct heap_descriptor private_heap;


/*---------------------------------------------------------------------
 * Function prototypes
 *---------------------------------------------------------------------*/

void		pagemanager_init(struct heap_descriptor *);
void		pagemanager_fini(struct heap_descriptor *);
void *		alloc_page(struct heap_descriptor *);
void *		alloc_pagewise(struct heap_descriptor *, word, word *);
void		free_pages(struct heap_descriptor *, void *, word);

void		irq_lock_init(void (*irq_fct)(void));
void		a_mutex_init(a_mutex_t *);
void		a_mutex_lock(volatile a_mutex_t *);
void		a_mutex_unlock(a_mutex_t *);
void		a_mutex_destroy(a_mutex_t *);

void		alloc_init(struct heap_descriptor *);
void		alloc_debug_level(struct heap_descriptor *, int);
void *		alloc_size(struct heap_descriptor *, word);
void		free_size(struct heap_descriptor *, void *, word);
void *		realloc_size(struct heap_descriptor *, void *, word, word);
void *		h_alloc(struct heap_descriptor *, word);
void		h_free(struct heap_descriptor *, void *);
void *		h_realloc(struct heap_descriptor *, void *, word);
int		address_in_heap(struct heap_descriptor *, void *);
int		alloc_statistics(struct heap_descriptor *, int);

void *		hp_alloc_size(word size);
void		hp_free_size(void *, word size);
void *		hp_realloc_size(void *, word, word);
/* export these for possible use in eplex */
DLLEXP void *	hp_alloc(word size);
DLLEXP void	hp_free(void *);
DLLEXP void *	hp_resize(void *, word);
int		hp_statistics(int what);

char		*shared_mem_base(void);
char		*shared_mem_init(int create_flag,
			char* mapfile, char* start,
			word size, word increment,
			void (*panic_fct)(const char*, const char*),
			struct heap_descriptor *hd);
void		shared_mem_release(struct heap_descriptor *hd);
int		shared_mem_save(struct heap_descriptor *hd, int fd);
int		shared_mem_restore(struct heap_descriptor *hd, int fd);
void		private_mem_init(void (*panic_fct)(const char*, const char*));
char *		private_mem_init_desc(void (*panic_fct)(const char*, const char*),
			struct heap_descriptor *hd);
void		private_mem_fini();
void		private_mem_fini_desc(struct heap_descriptor *hd);


#endif /* ECLIPSE_MEMMAN_H */
