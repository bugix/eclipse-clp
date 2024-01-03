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
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * SEPIA C SOURCE MODULE
 *
 * VERSION	$Id: dict.c,v 1.23 2017/09/01 16:08:57 jschimpf Exp $
 */

/*
 * IDENTIFICATION	dict.c
 *
 * AUTHOR:		Joachim Schimpf
 *
 * DESCRIPTION		SEPIA dictionary and related routines
 *
 * CONTENTS:
 *
 *	dict_init()
 *
 *		initialise the dictionary data structures and enter
 *		some predefined functors.
 *
 *	dident	enter_dict_n(char *name, int namelength, int arity)
 *
 *		Returns the DID for the functor with given name and arity.
 *		If it is not yet in the dictionary, it is entered. The name
 *		is specified with the length, so it can contain NUL bytes.
 *
 *	dident	enter_dict(char *name, int arity)
 *
 *		Same as enter_dict_n(), but takes a NUL-terminated C string
 *
 *	dident	in_dict(char *name, int arity)
 *
 *		Same as enter_dict(), but makes the entry a permanent one, ie.
 *		it will never be garbage collected. It is safe to store such
 *		DIDs in places that the garbage collector does not know about.
 *
 *	dident	ec_did(char *name, int arity)
 *
 *		Same as in_dict(), for naming like other user functions.
 *
 *	dident	add_dict(dident olddid, int newarity)
 *
 *		Converts a given DID into one for the same name but different
 *		arity. If such an entry does not yet exist, it is created.
 *
 *	dident	check_did_n(char *name, int namelength, int arity)
 *
 *		Returns the DID for the functor with given name and arity.
 *		If it is not yet in the dictionary, D_UNKNOWN is returned.
 *
 *	dident	check_did(dident olddid, int newarity)
 *
 *		Converts a given DID into one for the same name but different
 *		arity. If such an entry does not exist, D_UNKNOWN is returned.
 *
 *	pword  *enter_string_n(char *name, int length, int stability)
 *
 *		Create an atom with the given stability and returns a pointer
 *		to the corresponding string in the heap. This string exists
 *		only as long as a functor with this name exists. That means,
 *		if the string pointer is stored in a place where it is not
 *		known to the garbage collector, the stability has to be
 *		sufficiently high.
 *
 *	dident	bitfield_did(int bitfield)
 *
 *		convert a 20-bit bitfield representation of a DID (as used in
 *		the variable names) to a standard 32-bit DID.
 *
 *	int	next_functor(int *index, dident *did)
 *
 *		support function for traversing the dictionary, see below.
 *
 *	gc_dictionary(arity)
 *
 *		Dictionary garbage collector.
 *
 */


#include	"config.h"
#include	"sepia.h"
#include	"types.h"
#include	"embed.h"
#include	"error.h"
#include	"mem.h"
#include	"ec_io.h"
#include	"dict.h"
#include	"emu_export.h"
#include	"property.h"
#include	"os_support.h"


/* Make sure all calls to this are outside dictionary-locked regions! */
#define LogPrintf(s,...) { \
	if (EclGblFlags & GC_VERBOSE) \
	    ec_printff(log_output_, s, __VA_ARGS__); \
    }


static dident	_in_dict_opt(char *name, int length, unsigned int hval, int arity, int options);
static void	_std_did_init(void);
static void	_constant_table_init(int);
static void	_finish_gc();



/*-----------------------------------------------------------------------------

The basic data structure for the dictionary is the struct dict_item.
A dictionary identifier (DID) is simply the address of such a dict_item.
A dict_item contains:

	- arity
	- pointer to an ECLiPSe string buffer representing the name
	- procedure chain
	- property chain
	- collision chain
	- flags

dict_items are allocated in blocks of DICT_ITEM_BLOCK_SIZE (1024) elements.
The addresses of these blocks are kept in a directory array of size
DICT_DIRECTORY_SIZE (512). The maximum number of dictionary entries is thus
DICT_DIRECTORY_SIZE * DICT_ITEM_BLOCK_SIZE (524288).
This scheme is necessary to have a short 19-bit identifier (9 bits directory index,
10 bits block index) for DIDs, which is used to store variable names in the tag.
This index is also used for iterating through the dictionary.
For all other purposes, a DID is stored directly as its dict_item's address.

For finding DIDs when their name is given, there is a hash table of size
DICT_HASH_TABLE_SIZE. The hash value is computed from the name only, not
from the arity. Thus all functors with the same name hash onto the same
slot of the hash table (together with other functors whose name happens to
give the same hash value). All colliding entries are kept in a circular
chain, built using the 'next' field of the dict_items. The dict_item that
is referenced from the hash table is marked with the 'head' bit.

The circular collision chain is also used to find a functor that has the
same name but different arity as a given one (e.g. in functor/3), so no
new hashing is needed in this case, see function add_dict().

The strings holding the functor names are allocated separately from the
dict_items. All functors with the same name (but different arities) share
the same string. Note that, due to the current handling of strings in Sepia,
these strings are not only referenced by dict_items, but may also be pointed
to by TSTRG pwords from elsewhere. The dictionary strings look like standard
Sepia strings, but their tag is TBUFFER|IN_DICT|<ref_counter>.
The reference counter counts the number of references from dict_items only,
and is used to free the string when the last functor with this name disappears.
To make sure that referenced strings are not collected, the marking routine
marks the corresponding atom whenever a persistent string is encountered.

-----------------------------------------------------------------------------*/

#define DICT_DIRECTORY_SIZE	1024
#define DICT_ITEM_BLOCK_SIZE	1024
#define DICT_MAX_ENTRIES	(DICT_DIRECTORY_SIZE*DICT_ITEM_BLOCK_SIZE)

#define DidBlock(i) ((i) >> 10)
#define DidOffset(i) ((i) & 0x3ff)
#define MakeBitField(block, offs) ((block)<<10|(offs))

/* values for the options for _in_dict_opt() */
#define IN_DICT_CHECK	0
#define IN_DICT_ENTER	1

#define Inc_Ref_Ctr(tag)	{ (tag) += 0x100; }
#define DecRefCtr(tag)		((tag) -= 0x100, (tag) & 0x0fffff00)


/* DICT_HASH_TABLE_SIZE must be a power of 2 (we use masking) */
#define DICT_HASH_TABLE_SIZE	8192

#if 1
#define Combine_(hash, c)	\
	hash += (hash<<3) + (c);

#define Finish_(hash)
#else
/* Jenkins one-at-a-time hash */
#define Combine_(hash, c)	\
	hash += (c);		\
	hash += (hash << 10);	\
	hash ^= (hash >> 6);

#define Finish_(hash)		\
	hash += (hash << 3);	\
	hash ^= (hash >> 11);	\
	hash += (hash << 15);
#endif

/* compute hash value and length of a NULL-terminated string */
#define Hash(id, hash, length) {					\
	char *str = (id);						\
        for (length = hash = 0; *str; str++, length++) {		\
	    Combine_(hash, *(unsigned char *)str);			\
	}								\
	Finish_(hash);							\
        hash %= dict->hash_table_size;					\
}

/* compute hash value of a string of given length */
#define Hashl(id, hash, n) {						\
	char *str = (id);						\
	int length = (n);						\
        for (hash = 0; length > 0; str++, --length) {			\
	    Combine_(hash, *(unsigned char *)str);			\
	}								\
	Finish_(hash);							\
        hash %= dict->hash_table_size;					\
}


/*
 * Compare 2 strings of length length.
 * length is decremented and is 0 if the strings were equal
 */
#define Compare_N_Chars(length, s1, s2) {				\
	char *aux1 = (s1), *aux2 = (s2);			\
	while (length) {						\
	    if (*aux1++ != *aux2++)					\
		break;							\
	    --length;							\
	}								\
}

#define DidInUse(d)	(DidString(d))

/* We use a few anonymous DIDs to hold:
 *   - the per-type properties (macros and portrays)
 *   - an unnamed dummy module
 */
#define ANONYMOUS_MODULE	(NTYPES+1)
#define NANONYMOUS		(ANONYMOUS_MODULE+1)

/*
 * TYPEDEFS and GLOBAL VARIABLES
 */

#define dict ((struct dictionary*)shared_data->dictionary)

struct dictionary {
	ec_mutex_t lock;	/* lock for hash table */
	int	current_season;	/* 0/1, changes on every garbage collection */
	int	dgc_step_count;	/* >0 if dict gc under way */
	int	dir_index;	/* next free directory slot */
	dident	free_item_list;	/* chain of free dict_items */
	int	items_free;	/* number of elements in this chain */
	int	table_usage;	/* number of hash slots in use */
	int	collisions;	/* number of hash collisions */
	int	gc_countdown;	/* remaining allocations before triggering gc */
	int	gc_interval;	/* allocations between triggering gc */
	int	gc_number;	/* number of garbage collections so far (statistics only) */
	word	gc_time;	/* and the time they took */
	unsigned long total_collected;/* and the number of collected entries */
	int	string_used;
	int	string_free;

	int	hash_table_size;
	dident	*hash_table;
	struct dict_item anonymous_did[NANONYMOUS];/* to hold anon. properties */
	dident	directory[DICT_DIRECTORY_SIZE];	/* table of dict_item blocks */
};


void
dict_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	int i;
	shared_data->dictionary = hg_alloc_size(sizeof(struct dictionary));
	dict->hash_table_size = DICT_HASH_TABLE_SIZE;
	dict->hash_table = hg_alloc_size(DICT_HASH_TABLE_SIZE*sizeof(dident));
	for (i=0; i< DICT_HASH_TABLE_SIZE; i++)
	    dict->hash_table[i] = D_UNKNOWN;
	for (i=0; i< DICT_DIRECTORY_SIZE; i++)
	    dict->directory[i] = D_UNKNOWN;
	for (i=0; i < NANONYMOUS; i++)
	{
	    dict->anonymous_did[i].string = 0;
	    dict->anonymous_did[i].properties = 0;
	    dict->anonymous_did[i].macro = 0;
	}
	dict->dir_index = 0;
	dict->free_item_list = D_UNKNOWN;
	dict->items_free = 0;
	dict->string_used = 0;
	dict->string_free = 0;
	dict->table_usage = 0;
	dict->collisions = 0;
	dict->gc_interval = DICT_ITEM_BLOCK_SIZE/16*15;
	/* Set countdown to 0 to disable collections during the boot phase.
	 * First collection is triggered manually, then gc_interval is used.
	 */
	dict->gc_countdown = 0;
	dict->gc_number = 0;
	dict->gc_time = 0;
	dict->total_collected = 0;
	dict->dgc_step_count = 0;
	dict->current_season = 0;
	mt_mutex_init(&dict->lock);
    }
    if (flags & INIT_PRIVATE)
    {
	int i;

	_std_did_init();

	/* Tag descriptor array (more settings in bip_emu_init()) */
	for (i=0; i <= NTYPES; i++)
	{
	    tag_desc[i].super =
	    tag_desc[i].tag.kernel = (word) i;
	    tag_desc[i].order = 0;
	    tag_desc[i].type_name =
	    tag_desc[i].tag_name = D_UNKNOWN;
	}

	tag_desc[TLIST].tag_name = in_dict("list", 0);
	tag_desc[TCOMP].tag_name = in_dict("structure", 0);
	tag_desc[TSTRG].tag_name = d_.string0;
	tag_desc[TBIG].tag_name = in_dict("bignum", 0);
	tag_desc[TDBL].tag_name = d_.double0;
	tag_desc[TRAT].tag_name = d_.rational0;
	tag_desc[TSUSP].tag_name = d_.goal;
	tag_desc[THANDLE].tag_name = in_dict("handle", 0);
	tag_desc[TNIL].tag_name = d_.nil;
	tag_desc[TINT].tag_name = d_.integer0;
	tag_desc[TDICT].tag_name = d_.atom0;
	tag_desc[TPTR].tag_name = d_.meta0;

	tag_desc[TLIST].super = TCOMP;
	tag_desc[TCOMP].type_name = d_.compound0;
	tag_desc[TSTRG].type_name = d_.string0;
	tag_desc[TBIG].super = TINT;
	tag_desc[TINT].type_name = d_.integer0;
	tag_desc[TDBL].type_name = d_.float0;
	tag_desc[TRAT].type_name = d_.rational0;
	tag_desc[TSUSP].type_name = d_.goal;
	tag_desc[THANDLE].type_name = in_dict("handle", 0);
	tag_desc[TNIL].super = TDICT;
	tag_desc[TDICT].type_name = d_.atom0;
	tag_desc[TPTR].type_name = d_.meta0;
    }

    _constant_table_init(flags);
}


/*
 * Return dict_item for the specified type/tag.
 * It is used to attach properties to types, in particular macros.
 */

dident
transf_did(word t)
{
    int i = tag_desc[TagTypeC(t)].super;
    return (0<=i && i<=NTYPES) ? &dict->anonymous_did[i] : D_UNKNOWN;
}


/*
 * String allocation for dictionary.
 * These strings are write-once, read-only, except for dictionary gc.
 */

#define StringSize(length) (BufferSizePwords(length+1) * sizeof(pword))

static pword *
alloc_string(int length)
{
    pword *ptr;
    ptr = (pword *) hg_alloc_size(StringSize(length));
    return ptr;
}

static void
free_string(pword *ptr)
{
    hg_free_size(ptr, StringSize(ptr->val.nint));
}



/*
 * return a new dict_item
 * This must be called under dict->lock
 *
 * Initializes all fields except .next
 * Free dict_items are in the free list and can be recognised also
 * by having a NULL string field.
 */

static dident
_alloc_dict_item(pword *dict_string, int arity)
{
    dident dip;

    dip = dict->free_item_list;
    if (!dip)				/* free list empty, allocate a new block */
    {
	int i;
	if (dict->dir_index == DICT_DIRECTORY_SIZE) {
	    mt_mutex_unlock(&dict->lock);
	    ec_panic("dictionary overflow", "atom/functor creation");
	}
	dip =
	dict->free_item_list =
	dict->directory[dict->dir_index] =
	    (dident) hg_alloc_size(sizeof(struct dict_item) * DICT_ITEM_BLOCK_SIZE);
	for (i = 0; i < DICT_ITEM_BLOCK_SIZE; ++i)
	{
	    dip[i].bitfield = MakeBitField(dict->dir_index, i);
	    dip[i].string = (pword *) 0;
	    dip[i].arity = UNUSED_DID_ARITY;
	    dip[i].next = &dip[i+1];
	}
	dip[i-1].next = D_UNKNOWN;
	dict->dir_index++;
	dict->items_free += DICT_ITEM_BLOCK_SIZE;
    }

    dip->string = dict_string;		/* initialize the dict_item */
    Inc_Ref_Ctr(dict_string->tag.kernel);
    dip->arity = arity;
    dip->procedure = 0;
    dip->properties = 0;
    dip->macro = 0;
    dip->season = dict->current_season;
    dip->module = 0;
    dip->isop = 0;
    dip->dict_flags = 0;

    dict->free_item_list = dip->next; /* unlink it from the free list */
    dict->items_free--;

    if (--dict->gc_countdown == 0)
	ec_signal_dict_gc();		/* trigger garbage collection */

    return dip;
}


dident
in_dict(char *name, int arity)
{
    unsigned int hval;
    int len;
    dident dip;
    Hash(name, hval, len);
    dip = _in_dict_opt(name, len, hval, arity, IN_DICT_ENTER);
    Set_Did_Stability(dip, DICT_PERMANENT);
    return dip;
}

dident Winapi
ec_did(const char *name, const int arity)
{
    unsigned int hval;
    int len;
    dident dip;
    Hash((char *)name, hval, len);
    dip = _in_dict_opt((char *) name, len, hval, arity, IN_DICT_ENTER);
    Set_Did_Stability(dip, DICT_PERMANENT);
    return dip;
}

dident
enter_dict(char *name, int arity)
{
    unsigned int hval;
    int len;
    Hash(name, hval, len);
    return _in_dict_opt(name, len, hval, arity, IN_DICT_ENTER);
}

dident
enter_dict_n(char *name, word len, int arity)
{
    unsigned int hval;
    Hashl(name, hval, len);
    return _in_dict_opt(name, (int) len, hval, arity, IN_DICT_ENTER);
}

dident
check_did_n(char *name, word len, int arity)
{
    unsigned int hval;
    Hashl(name, hval, len);
    return _in_dict_opt(name, (int) len, hval, arity, IN_DICT_CHECK);
}

pword *
enter_string_n(char *name, word len, int stability)
{
    unsigned int hval;
    dident dip;
    Hashl(name, hval, len);
    dip = _in_dict_opt(name, (int) len, hval, 0, IN_DICT_ENTER);
    Set_Did_Stability(dip, stability);
    return DidString(dip);
}

dident
bitfield_did(word bf)
{
    return (dident) (dict->directory[DidBlock(bf)] + DidOffset(bf));
}


/*
 * _in_dict_opt(name, length, hval, arity, options)
 *	options are IN_DICT_CHECK or IN_DICT_ENTER
 *
 * We guarantee that functors with the same name always share their name string!
 */

static dident
_in_dict_opt(char *name,	/* might not be NUL-terminated! */
	int length,
	unsigned int hval,
	int arity,
	int options)
{
    int locked = 0;
    dident dip;
    dident start;
    pword *dict_string;

    mt_mutex_lock(&dict->lock);
    start = dict->hash_table[hval];
    dict_string = (pword *) 0;
    if (start)
    {
	dip = start;
	do
	{
	    if (!dict_string)
	    {
		if (DidLength(dip) == length)
		{
		    word cmp = length;
		    Compare_N_Chars(cmp, name, DidName(dip));
		    if (!cmp)		/* name found */
		    {
			if (DidArity(dip) == arity)
			    goto _dict_unlock_return_;
			else
			    dict_string = DidString(dip);
		    }
		}
	    }
	    else if (DidString(dip) == dict_string && DidArity(dip) == arity)
		goto _dict_unlock_return_;
	    dip = dip->next;
	} while (dip != start);
    }
    if (options == IN_DICT_CHECK) {
	mt_mutex_unlock(&dict->lock);
	return D_UNKNOWN;
    }

    if (!dict_string)	/* a functor with a new name */
    {
	dict->string_used += length+1;
	dict_string = alloc_string(length);
	Set_Buffer_Size(dict_string, length+1);
	dict_string->tag.kernel = TBUFFER|IN_DICT;
	Copy_Bytes((char *)(dict_string+1), name, length);
	((char *)(dict_string+1))[length] = 0;
	if (start)
	    dict->collisions++;
    }
    dip = _alloc_dict_item(dict_string, arity);
    if (start)
    {
	dip->next = start->next;
	start->next = dip;
    }
    else	/* the first entry in this hash slot */
    {
	dip->next = dip;
	Set_Did_Head(dip);
	dict->hash_table[hval] = dip;
	dict->table_usage++;
    }

_dict_unlock_return_:
    if (dip->season != dict->current_season)
	dip->season = dict->current_season;
    mt_mutex_unlock(&dict->lock);
    return dip;
}


dident
add_dict(dident old_did, int new_arity)
{
    dident dip;

    mt_mutex_lock(&dict->lock);
    dip = old_did;
    do {
	if (DidArity(dip) == new_arity && DidString(dip) == DidString(old_did))
	    goto _dict_unlock_return_;
	dip = dip->next;
    } while (dip != old_did);

    /* not found, make a new entry */
    dip = _alloc_dict_item(DidString(old_did), new_arity);
    dip->next = old_did->next;
    old_did->next = dip;
_dict_unlock_return_:
    if (dip->season != dict->current_season)
	dip->season = dict->current_season;
    mt_mutex_unlock(&dict->lock);
    return dip;
}

dident
check_did(dident old_did, int new_arity)
{
    dident dip;

    mt_mutex_lock(&dict->lock);
    dip = old_did;
    do {
	if (DidArity(dip) == new_arity && DidString(dip) == DidString(old_did)) {
	    if (dip->season != dict->current_season)
		dip->season = dict->current_season;
	    mt_mutex_unlock(&dict->lock);
	    return dip;
	}
	dip = dip->next;
    } while (dip != old_did);
    mt_mutex_unlock(&dict->lock);
    return D_UNKNOWN;
}


/*
 * int next_functor()
 *
 * A support function to scan the dictionary. It is used to implement
 * current_functor/1 and the like.
 * The update semantics of this function is unclear (i.e. if a new
 * functor is entered between successive calls of next_functor(),
 * it will be returned or not, depending on where it is inserted).
 * Note also that dictionary GCs might happen between successive calls
 * to this function, which has similar consequences.
 * However, the function is at least robust and will not crash.
 *
 * To be used like:
 *
 *	int	idx = 0;
 *	dident	did;
 *
 *	while (next_functor(&idx, &did, 0))
 *	{
 *		<use did>
 *	}
 */

int
next_functor(			/* returns 0 when dictionary exhausted	*/
    	int *pidx,		/* in/out: current dict index		*/
	dident *pdid,		/* output: valid did			*/
	int weak_access		/* do not consider lookup as a "use"	*/
	)
{
    dident dip;
    int idx = *pidx;

    mt_mutex_lock(&dict->lock);
    while (DidBlock(idx) < dict->dir_index)
    {
	dip = dict->directory[DidBlock(idx)];
	if (dip)
	{
	    dip += DidOffset(idx);
	    do
	    {
		idx++;
		if (DidInUse(dip))
		{
		    if (!weak_access) {
			/* if the result is going to be returned to Prolog,
			 * it must be updated, because we may be during a GC
			 * and the engine marking may have happened already.
			 */
			if (dip->season != dict->current_season)
			    dip->season = dict->current_season;
		    }
		    *pdid = dip;
		    *pidx = idx;
		    mt_mutex_unlock(&dict->lock);
		    return 1;
		}
		dip++;
	    } while (DidOffset(idx));
	}
	else
	    idx = (DidBlock(idx) + 1) * DICT_ITEM_BLOCK_SIZE;
    }
    mt_mutex_unlock(&dict->lock);
    return 0;
}


/*--------------------------------------------------------------
 * Dictionary garbage collection
 *--------------------------------------------------------------*/

/*
 * _tidy_dictionary()
 */

#define Useful(d)	(  (d)->season == dict->current_season \
 			|| DidStability(d) > DICT_VOLATILE \
			|| (d)->procedure || (d)->properties)


/**
 * Resize dict's hash table (grow or shrink).
 * To be called under lock.
 */
static void
_resize_hash_table(void)
{
    unsigned i;
    dident *new_hash_table, *old_hash_table;

    /* compute new table size */
    unsigned num_hashed_entries = dict->table_usage+dict->collisions;
    unsigned old_table_size = dict->hash_table_size;
    unsigned new_table_size = DICT_HASH_TABLE_SIZE;
    while (new_table_size < 2*num_hashed_entries && new_table_size < DICT_MAX_ENTRIES)
    	new_table_size <<= 1;
    if (new_table_size == old_table_size)
    	return;

    LogPrintf("DICTIONARY table resize %d->%d\n", old_table_size, new_table_size);

    /* init new table */
    new_hash_table = hg_alloc_size(new_table_size*sizeof(dident));
    for (i = 0; i < new_table_size; i++)
    	new_hash_table[i] = NULL;
    old_hash_table = dict->hash_table;
    dict->hash_table = new_hash_table;
    dict->hash_table_size = new_table_size;	/* needed for Hashl macro! */
    dict->table_usage = 0;
    dict->collisions = 0;

    /* transfer entries from old_hash_table[] */
    for (i = 0; i < old_table_size; i++)
    {
	while(old_hash_table[i])
	{
	    unsigned hval;
	    dident dip, *rem_tail;
	    dident new_chain, *new_tail;
	    pword *dict_string;

	    rem_tail = &old_hash_table[i];
	    new_chain = *rem_tail;
	    *rem_tail = NULL;
	    new_tail = &new_chain->next;
	    dict_string = DidString(new_chain);

	    /* extract all items with same name string as new_chain */
	    for(dip=new_chain->next; dip!=new_chain; dip=dip->next)
	    {
	        if (DidString(dip) == dict_string) {
		    *new_tail = dip;
		    new_tail = &dip->next;
		} else {
		    *rem_tail = dip;
		    rem_tail = &dip->next;
		}
	    }
	    *rem_tail = old_hash_table[i];	/* (re-)close the loop */

	    /* insert new_chain..new_tail into new table */
	    Hashl(DidName(new_chain), hval, DidLength(new_chain));

	    if (dict->hash_table[hval]) {
		Clr_Did_Head(new_chain);	/* might be set from before */
		*new_tail = dict->hash_table[hval]->next; /* insert after head */
		dict->hash_table[hval]->next = new_chain;
		++dict->collisions;
	    } else {
		Set_Did_Head(new_chain);	/* might already be set */
		*new_tail = new_chain;		/* close the loop */
		dict->hash_table[hval] = new_chain; /* insert in new slot */
		++dict->table_usage;
	    }
	}
    }

    hg_free_size(old_hash_table, old_table_size*sizeof(dident));

#if 0
    unsigned i;
    dident *new_hash_table;

    /* compute new table size */
    unsigned num_hashed_entries = dict->table_usage+dict->collisions;
    unsigned old_table_size = dict->hash_table_size;
    unsigned new_table_size = DICT_HASH_TABLE_SIZE;
    while (new_table_size < 2*num_hashed_entries && new_table_size < DICT_MAX_ENTRIES)
    	new_table_size <<= 1;
    if (new_table_size == old_table_size)
    	return;

    LogPrintf("DICTIONARY table resize %d->%d\n", old_table_size, new_table_size);

    /* init new table */
    new_hash_table = hg_alloc_size(new_table_size*sizeof(dident));
    for (i = 0; i < new_table_size; i++)
    	new_hash_table[i] = NULL;
    dict->hash_table_size = new_table_size;	/* needed for Hashl macro! */
    dict->table_usage = 0;
    dict->collisions = 0;

    /* transfer entries */
    for (i = 0; i < old_table_size; i++)
    {
	while(dict->hash_table[i])
	{
	    unsigned hval;
	    dident dip, *rem_tail;
	    dident new_chain, *new_tail;
	    pword *dict_string;

	    rem_tail = &dict->hash_table[i];
	    new_chain = *rem_tail;
	    *rem_tail = NULL;
	    new_tail = &new_chain->next;
	    dict_string = DidString(new_chain);

	    /* extract all items with same name string as new_chain */
	    for(dip=new_chain->next; dip!=new_chain; dip=dip->next)
	    {
	        if (DidString(dip) == dict_string) {
		    *new_tail = dip;
		    new_tail = &dip->next;
		} else {
		    *rem_tail = dip;
		    rem_tail = &dip->next;
		}
	    }
	    *rem_tail = dict->hash_table[i];	/* (re-)close the loop */

	    /* insert new_chain..new_tail into new table */
	    Hashl(DidName(new_chain), hval, DidLength(new_chain));

	    if (new_hash_table[hval]) {
		Clr_Did_Head(new_chain);	/* might be set from before */
		*new_tail = new_hash_table[hval]->next; /* insert after head */
		new_hash_table[hval]->next = new_chain;
		++dict->collisions;
	    } else {
		Set_Did_Head(new_chain);	/* might already be set */
		*new_tail = new_chain;		/* close the loop */
		new_hash_table[hval] = new_chain; /* insert in new slot */
		++dict->table_usage;
	    }
	}
    }

    /* replace the table */
    hg_free_size(dict->hash_table, old_table_size*sizeof(dident));
    dict->hash_table = new_hash_table;
#endif
}


#if 0

/*
 * The free list is built such that the oldest dids are reused first in order
 * to quickly fill did blocks again, so that they are more or less read-only
 * afterwards.
 * Another advantage of this scheme is that we can easily detect when a block
 * becomes completely unused, and then free the whole block.
 */

static void
_tidy_dictionary0(void)
{
    int block, idx;
    dident dip, aux;
    dident *free_list_tail = &dict->free_item_list;

    *free_list_tail = D_UNKNOWN;
    for (block = 0; block < dict->dir_index; block++)
    {
	dip = dict->directory[block];
	for (idx = 0; idx < DICT_ITEM_BLOCK_SIZE; idx++, dip++)
	{
	    if (DidInUse(dip) && Useful(dip))
	    {
		continue;
	    }
	    else if (DidInUse(dip))		/* a garbage did, unlink it */
	    {
		/* Tidy the collision chain in which dip occurs. This assumes that
		 * all DIDs with this name are in the same chain!
		 */
		dident prev = dip;
		int head_removed = 0;

		do
		{
		    aux = prev->next;
		    if (Useful(aux))		/* no garbage, skip it */
		    {
			prev = aux;
			continue;
		    }
		    else			/* remove aux */
		    {
			pword *str = DidString(aux);
			prev->next = aux->next;
			aux->next = D_UNKNOWN;
			dict->items_free++;
			if (DecRefCtr(str->tag.kernel) == 0)
			{
			    dict->string_used -= str->val.nint + 1;
			    free_string(str);
			    /*
			    p_fprintf(current_err_, "%s/%d (with string)\n",
							DidName(aux), DidArity(aux));
			    */
			}
			/*
			else
			{
			    p_fprintf(current_err_, "%s/%d\n",
							DidName(aux), DidArity(aux));
			}
			*/
			aux->string = (pword *) 0;
			aux->arity = -1;
			if (DidIsHead(aux))
			    head_removed = 1;
		    }
		} while (aux != dip);

		if (head_removed)
		{
		    char *dummy1;
		    int dummy2;
		    unsigned int hval;
		    Hash(DidName(dip), hval, dummy2, dummy1);
		    if (prev != dip)
		    {
			Set_Did_Head(prev);
			dict->hash_table[hval] = prev;
		    }
		    else	/* we removed all chain elements */
		    {
			dict->hash_table[hval] = D_UNKNOWN;
			dict->table_usage--;
		    }
		}
	    } /* else: an already unlinked garbage did */
	    *free_list_tail = dip;		/* add it to the free list */
	    free_list_tail = &dip->next;
	}
    }
    *free_list_tail = D_UNKNOWN;
    Succeed_;
}

#endif /* 0 */

/*
 * alternatively, scan through the hash table
 * To be called under lock.
 */

static void
_tidy_dictionary(void)
{
    int idx;
    dident dip;
    dident prev;

    for (idx = 0; idx < dict->hash_table_size; idx++)
    {
	prev = dict->hash_table[idx];
	if (prev)
	{
	    do
	    {
		dip = prev->next;
		if (Useful(dip))
		{
		    prev = dip;
		}
		else		/* a garbage did, unlink it */
		{
		    pword *str = DidString(dip);
		    ec_atomic_store(&prev->next, dip->next);
		    /*
		    p_fprintf(current_err_, "\n%s/%d", DidName(dip), DidArity(dip));
		    */
		    if (DecRefCtr(str->tag.kernel) == 0)
		    {
			dict->collisions--;
			dict->string_used -= str->val.nint + 1;
			free_string(str);
			/*
			p_fprintf(current_err_, " (with string)");
			*/
		    }
		    /* add it to the free list */
#ifdef DEBUG_DICT
		    dip->arity = (word) dip->string;
		    dip->string = (pword *) 0;
#else
		    dip->arity = UNUSED_DID_ARITY;
		    dip->string = (pword *) 0;
		    dip->next = dict->free_item_list;
		    dict->free_item_list = dip;
		    dict->items_free++;
#endif
		    if (DidIsHead(dip))	/* removing the chain header */
		    {
			if (prev != dip)
			{
			    Set_Did_Head(prev);
			    dict->hash_table[idx] = prev;
			}
			else	/* we removed all chain elements */
			{
			    dict->hash_table[idx] = D_UNKNOWN;
			    dict->collisions++;	/* was not a collision */
			    dict->table_usage--;
			}
		    }
		}
	    } while (!(DidIsHead(dip)));
	}
    }

    /* If hash table is still quite full, resize it.  We could also shrink. */
    if (dict->table_usage > dict->hash_table_size/2)
        _resize_hash_table();
}


/**
 * Mark a dictionary item
 */
void
ec_mark_did(dident d)
{
    d->season = dict->current_season;
}


/**
 * Mark a pointer that *may* refer to a dict_item
 */
void
ec_mark_did_conservative(void *p)
{
    int block;

    for(block=0; block < dict->dir_index; ++block)
    {
	dident dip = dict->directory[block];
	if ((char*)dip <= (char*)p  &&  (char*)p <= (char*)(dip+DICT_ITEM_BLOCK_SIZE-1))
	{
	    /* p points into this block - is it correctly aligned? */
	    if (((char*)p - (char*)dip) % sizeof(dident) == 0  &&  DidInUse(dip))
		dip->season = dict->current_season;
	    return;
	}
    }
}


/**
 * Mark a pointer that *may* refer to a dictionary string.
 * This is terribly expensive, but we don't know whether the pointer
 * is a valid pointer at all, and whether it can be dereferenced.
 */
void
ec_mark_string_conservative(void *p)
{
    int block;
    for(block=0; block < dict->dir_index; ++block) {
	int i;
	for(i=0; i < DICT_ITEM_BLOCK_SIZE; ++i) {
	    dident dip = &dict->directory[block][i];
	    if (DidInUse(dip)  &&  (p == dip->string)) {
		dip->season = dict->current_season;
		return;
	    }
	}
    }
}


/**
 * Change the dictionary "season" and trigger garbage collection,
 * if not already running.
 *
 * Meaning of dgc_step_count:
 *	>=2	engine marking phase (dgc_step_count-2 engines pending)
 *	1	final gc phase (global marking + tidying)
 *	0	gc finished/no gc running
 *
 * If triggered by the countdown, the initial call will be made from
 * the signal engine.  But it may also be called explicitly from any
 * engine via garbage_collect_dictionary/0.
 */

int
p_gc_dictionary(ec_eng_t *unused)
{
    ec_eng_t *eng;
    int engine_markings_required = 0;

    mt_mutex_lock(&dict->lock);
    if (dict->dgc_step_count > 0) {
	/* If dictionary GC is currently under way, just succeed */
	mt_mutex_unlock(&dict->lock);
	Succeed_;
    }
    /* Incremening dgc_step_count from 0 is under lock.  For other
     * operations on dgc_step_count ec_atomic_add() is sufficient. */
    dict->dgc_step_count = 2;

    /* Change season.  The lock is needed to make sure that no slow
     * dict-enter operations are in progress with the old season tag.
     * After unlocking, any new dictionary entries will tagged with
     * the new season value.  Old entries require update via marking.
     */
    dict->current_season ^= 1;
    dict->gc_number++;
    /* In case of manual triggering, pretend we triggered via countdown */
    if (dict->gc_countdown > 0)
	dict->gc_countdown = 0;
    mt_mutex_unlock(&dict->lock);

    LogPrintf("DICTIONARY GC #%d start (season=%d)\n", dict->gc_number, dict->current_season);

    /* Send marking request to every engine (including aux/sig/timer engine) */
    mt_mutex_lock(&EngineListLock);	/* protect engine list */
    eng = eng_chain_header;
    do {
	++engine_markings_required;
	assert(!eng->needs_dgc_marking);
	eng->needs_dgc_marking = 1;
	eng = eng->next;
    } while(eng != eng_chain_header);
    ec_atomic_add(&dict->dgc_step_count, engine_markings_required);
    do {
	/*
	 * Counter will decrement atomically as soon as request is handled.
	 * This can happen either directly inside ecl_request, or much
	 * later when the engine handles the request via an event.
	 */
	if (ecl_request(eng, DICT_GC_REQUEST) == PEXITED) {
	    /* no request to handle */
	    ec_atomic_add(&dict->dgc_step_count, -1);
	}
	eng = eng->next;
    } while(eng != eng_chain_header);
    mt_mutex_unlock(&EngineListLock);

    /* If all engine markings are already done, finish gc now.
     * Otherwise, leave it to the engine marker that finishes last.
     */
    assert(dict->dgc_step_count >= 2);
    if (ec_atomic_add(&dict->dgc_step_count, -1) == 1)
	_finish_gc();

    Succeed_;
}


/**
 * Mark dict entries accessible from the given engine (and its parents).
 * We do have exclusive access to these engines, there should not be
 * any races to access an engine for marking.  The parent_engine links
 * form disjoint linear chains, starting with an active engine, and
 * ending in an engine invoked from the host program or via resume_async.
 * However, there will be redundant invocations of this function for
 * parent engines that have already been marked via their descendants.
 * This is detected by looking at needs_dgc_marking.
 * If we detect that we have just marked the last engine that needed
 * marking for the current collection, we continue into _finish_gc()
 * to do global marking and tidying.
 */
void
ecl_mark_engine(ec_eng_t *ec_eng, word arity)
{
    ec_eng_t *eng = ec_eng;

    LogPrintf("DICTIONARY GC #%d marking engine %x\n", dict->gc_number, ec_eng);

    do {
	if (eng->needs_dgc_marking) {
	    int dgc_step;
	    eng->needs_dgc_marking = 0;
	    mark_dids_from_stacks(eng, arity);
	    dgc_step = ec_atomic_add(&dict->dgc_step_count, -1);
	    assert(dgc_step >= 1);
	    if (dgc_step == 1)
		return _finish_gc();
	}
    } while ((eng = eng->parent_engine));
}


/**
 * This is invoked after all engines have done their dictionary marking.
 * We mark from all the global items, then tidy the dictionary, and finish.
 */
static void
_finish_gc()
{
    int i = 0;
    int new_countdown;
    int gc_number = dict->gc_number;
    long usage_before, garbage;
    dident d;

    /* We just marked the last engine, now proceed to global marking */

    LogPrintf("DICTIONARY GC #%d global marking\n", gc_number);

    while (next_functor(&i, &d, 1))	/* mark from all the properties */
    {
	if (DidProc(d))
	    ec_mark_dids_from_procs(DidProc(d));
	if (DidProperties(d))
	    mark_dids_from_properties(d);
    }

    for (i=0; i < NANONYMOUS; i++)	/* mark from the anonymous DIDs */
    {
	d = &dict->anonymous_did[i];
	if (DidProperties(d))
	    mark_dids_from_properties(d);
    }

    mark_dids_from_streams();		/* mark from the stream descriptors */

    LogPrintf("DICTIONARY GC #%d cleanup\n", gc_number);

    /* Under lock, tidy up and reinitialize counts */
    mt_mutex_lock(&dict->lock);
    usage_before = dict->dir_index * DICT_ITEM_BLOCK_SIZE - dict->items_free;
    _tidy_dictionary();
    garbage = usage_before -
		  (dict->dir_index * DICT_ITEM_BLOCK_SIZE - dict->items_free);
    dict->total_collected += garbage;

    /* Re-init countdown.  Subtract half the entries that were made since this
     * gc started, assuming that (on average) so many came too late for this
     * marking round.  If next collection is already due, trigger it below.
     */
    assert(dict->gc_countdown <= 0);
    new_countdown = dict->gc_interval - (-dict->gc_countdown/2);
    if (new_countdown > 0)
	dict->gc_countdown = new_countdown;
    else
	dict->gc_countdown = 0;		/* re-trigger below */

    ec_atomic_add(&dict->dgc_step_count, -1);
    assert(dict->dgc_step_count == 0);
    mt_mutex_unlock(&dict->lock);

    LogPrintf("DICTIONARY GC #%d done (%d-%d, %.1f%%, total=%ld)\n", gc_number,
    		usage_before, garbage, (100.0*garbage)/usage_before, dict->total_collected);

    /* After unlocking, if new interval-triggered gc is already due, do it */
    if (new_countdown <= 0 && dict->gc_interval > 0)
	ec_signal_dict_gc();
}


/*--------------------------------------------------------------
 * Statistics and debugging
 *--------------------------------------------------------------*/

/*ARGSUSED*/
int
p_dict_param(value vwhat, type twhat, value vval, type tval, ec_eng_t *ec_eng)
{
    pword result;

    result.tag.kernel = TINT;
    switch(vwhat.nint)
    {
    case 0:	/* # entries */
	result.val.nint = dict->dir_index * DICT_ITEM_BLOCK_SIZE -
				dict->items_free;
	break;
    case 1:	/* # free items */
	result.val.nint = dict->items_free;
	break;
    case 2:	/* hash table size */
	result.val.nint = dict->hash_table_size;
	break;
    case 3:	/* hash table usage */
	result.val.nint = dict->table_usage;
	break;
    case 4:	/* collisions */
	result.val.nint = dict->collisions;
	break;
    case 5:	/* gc_number */
	result.val.nint = dict->gc_number;
	break;
    case 6:	/* gc time */
	Fail_;
	Return_Unify_Float(vval, tval, dict->gc_time/clock_hz);
    case 7:	/* set or get the gc interval */
	if (IsInteger(tval))
	{
	    dict->gc_interval = vval.nint;
	    if (dict->gc_countdown > 0)		/* only update if not expired */
		dict->gc_countdown = vval.nint;
	}
	result.val.nint = dict->gc_interval;
	break;
    case 8:	/* countdown */
	result.val.nint = dict->gc_countdown;
	break;
    case 9:	/* remaining steps in current GC (0 = no GC running) */
	result.val.nint = dict->dgc_step_count;
	break;
    case 10:	/* total entries collected */
	result.val.nint = dict->total_collected;
	break;
    default:
	Fail_;
    }
    Return_Unify_Pw(vval, tval, result.val, result.tag);
}


/*
 * auxiliary functions for debugging
 */

#ifdef PRINTAM
#ifndef lint

int
pr_functors(value v, type t)
{
    dident dip;
    int index, len;

    Check_Integer(t);
    for (index = 0; index < dict->hash_table_size; index++)
    {
	dip = dict->hash_table[index];
	if (dip)
	{
	    len = 0;
	    do {
		len++;
		dip = dip->next;
	    } while (!(DidIsHead(dip)));
	    if (dip != dict->hash_table[index])
		p_fprintf(current_output_,"BAD COLLISION LIST\n");
	    if (len >= v.nint)
	    {
		p_fprintf(current_output_,"[%d]", index);
		do {
		    p_fprintf(current_output_," %s/%d",
				DidName(dip), DidArity(dip));
		    dip = dip->next;
		} while (!(DidIsHead(dip)));
		p_fprintf(current_output_,"\n");
	    }
	}
    }
    Succeed_;
}

int
pr_dict(void)
{
    p_fprintf(current_output_, "blocks allocated = %d\n", dict->dir_index);
    p_fprintf(current_output_, "items used = %d\n",
		dict->dir_index*DICT_ITEM_BLOCK_SIZE-dict->items_free);
    p_fprintf(current_output_, "items free = %d\n", dict->items_free);
    p_fprintf(current_output_, "string_used = %d\n", dict->string_used);
    p_fprintf(current_output_, "table_usage = %d/%d\n",
				dict->table_usage, dict->hash_table_size);
    p_fprintf(current_output_, "table_usage ratio = %.1f%%\n",
				100.0*dict->table_usage/dict->hash_table_size);
    p_fprintf(current_output_, "collisions = %d\n", dict->collisions);
    p_fprintf(current_output_, "collision ratio = %.1f%%\n",
				100.0*dict->collisions/dict->table_usage);
    p_fprintf(current_output_, "gc countdown = %d\n", dict->gc_countdown);
    p_fprintf(current_output_, "gc number = %d\n", dict->gc_number);
    p_fprintf(current_output_, "gc time = %.3f\n",
				(float)dict->gc_time/clock_hz);
    p_fprintf(current_output_, "season = %d\n", dict->current_season);
    p_fprintf(current_output_, "pending gc steps = %d\n", dict->dgc_step_count);
    ec_flush(current_output_);
    Succeed_;
}


/*
 * 	help debugging: print a dictionary entry
*/
static dident
_pdict(dident entry)
{
    pri	*proc;

    p_fprintf(current_err_, "%s/", DidName(entry));
    p_fprintf(current_err_, "%d", DidArity(entry));
    p_fprintf(current_err_, "\n length=%d ", DidLength(entry));
    p_fprintf(current_err_, "module=%d ", DidModule(entry));
    p_fprintf(current_err_, "macro=%d ", DidMacro(entry));
    p_fprintf(current_err_, "attainable=%d ", entry->season==dict->current_season);
    p_fprintf(current_err_, "stability=%d ", DidStability(entry));
    p_fprintf(current_err_, "head=%d ", DidIsHead(entry)?1:0);
    p_fprintf(current_err_, "isop=%d", DidIsOp(entry));
    p_fprintf(current_err_, "\n next=%x ", DidNext(entry));
    p_fprintf(current_err_, "properties=%x ", DidProperties(entry));
    proc = entry->procedure;
    p_fprintf(current_err_, "\n proc=0x%x", proc);
    if (proc) {
	p_fprintf(current_err_, "(code=0x%x", PriCode(proc));
	p_fprintf(current_err_, " next=0x%x", PriNext(proc));
	p_fprintf(current_err_, " module=%d", PriModule(proc));
	p_fprintf(current_err_, " flags=0x%x", PriFlags(proc));
	p_fprintf(current_err_, " did=0x%x)", PriDid(proc));
    }
    (void) ec_newline(current_err_);
    return entry;
}

#endif /* lint */
#endif /* PRINTAM */


static void
_std_did_init(void)
{
	/* The first did entered is the empty name. This is used for
	 * unknown variable names. It has a zero bitfield representation.
	 */
	d_.empty = 	in_dict("", 0);

	d_.semi0 = 	in_dict(";", 0);
	d_.naf = 	in_dict("\\+", 1);
	d_.not1 = 	in_dict("not", 1);
	d_.fail_if = 	in_dict("fail_if", 1);
	d_.once =	in_dict("once", 1);
	d_.not_unify = 	in_dict("\\=", 2);
	d_.diff_reg =   in_dict("~=",2);
	d_.not_identical = 	in_dict("\\==", 2);
	d_.not_equal =	in_dict("=\\=", 2);

	d_.eocl = 	in_dict( ".", 0);
	d_.eof = 	in_dict( "end_of_file", 0);
	d_.list = 	in_dict( ".", 2);
	d_.rulech0 = 	in_dict(":-",0);
	d_.rulech1 = 	in_dict( ":-", 1);
	d_.rulech2 = 	in_dict( ":-", 2);
	d_.goalch = 	in_dict( "?-", 1);
	d_.grammar = 	in_dict("-->", 2);
	d_.nil = 	in_dict( "[]", 0);
	d_.fail = 	in_dict("fail",0);
	d_.false0 = 	in_dict("false",0);
	d_.nilcurbr = 	in_dict( "{}", 0);
	d_.nilcurbr1 = 	in_dict( "{}", 1);
	d_.cond = 	in_dict( "->", 2);
	d_.cut = 	in_dict( "!", 0);
	d_.syscut = 	in_dict( "syscut", 0);
	d_.cut_to = 	in_dict( "cut_to", 1);
        d_.arg =	in_dict("arg", 3);
        d_.subscript =	in_dict("subscript", 2);
	d_.comma = 	in_dict( ",", 2);
	d_.semicolon = 	in_dict( ";", 2);
	d_.colon =	in_dict(":", 2);
	d_.bar = 	in_dict( "|", 2);
	d_.uref = 	in_dict( "_", 0);
      	d_.univ = 	in_dict("=..", 2);
		/* arithmetic */
	d_.plus1 = 	in_dict("+", 1);
	d_.plus = 	in_dict("+", 2);
	d_.minus1 = 	in_dict("-", 1);
	d_.minus = 	in_dict("-", 2);
	d_.times = 	in_dict("*", 2);
	d_.inf = 	in_dict("<", 2);
	d_.sup = 	in_dict(">", 2);
	d_.supq = 	in_dict(">=", 2);
	d_.infq = 	in_dict("=<", 2);
	d_.inf0 = 	in_dict("<", 0);
	d_.sup0 = 	in_dict(">", 0);
	d_.supq0 = 	in_dict(">=", 0);
	d_.infq0 = 	in_dict("=<", 0);
	d_.quotient = 	in_dict("/",2);
	d_.div = 	in_dict("//", 2);
	d_.modulo = 	in_dict("mod", 2);
	d_.equal = 	in_dict("=:=", 2);
	d_.is = 	in_dict("is",2);	
	d_.rshift = 	in_dict(">>", 2);
	d_.lshift = 	in_dict("<<", 2);
	d_.and2 = 	in_dict("/\\",2);
	d_.or2 = 	in_dict("\\/", 2);
	d_.power = 	in_dict("^", 2);
	d_.bitnot = 	in_dict("\\", 1);
	d_.min =	in_dict("min",2);
	d_.minint =	in_dict("minint",1);
	d_.max =	in_dict("max",2);
	d_.maxint =	in_dict("maxint",1);
	d_.abs =	in_dict("abs",1);
	d_.xor2 =	in_dict("xor",2);
	d_.pi =		in_dict("pi",0);
	d_.e =		in_dict("e",0);	
	d_.sin =	in_dict("sin",1);
	d_.cos =	in_dict("cos",1);
	d_.tan =	in_dict("tan",1);
	d_.asin =	in_dict("asin",1);
	d_.acos =	in_dict("acos",1);
	d_.atan =	in_dict("atan",1);
	d_.exp =	in_dict("exp",1);
	d_.sqrt =	in_dict("sqrt",1);
	d_.ln =		in_dict("ln",1);
	d_.fix =	in_dict("fix",1);
	d_.float1 =	in_dict("float",1);
	d_.breal1 =	in_dict("breal",1);
	d_.breal_from_bounds = in_dict("breal_from_bounds",1);
	d_.breal_min = in_dict("breal_min",1);
	d_.breal_max = in_dict("breal_max",1);
	d_.round =	in_dict("round",1);
	d_.floor1 =	in_dict("floor",1);
	d_.rational1 =	in_dict("rational",1);
	d_.numerator1 =	in_dict("numerator",1);
	d_.denominator1 = in_dict("denominator",1);

		/* term comparison */
	d_.unify = 	in_dict("=", 2);
	d_.identical = 	in_dict("==", 2);
	d_.less = 	in_dict("@<", 2);
	d_.lessq = 	in_dict("@=<", 2);
	d_.greater = 	in_dict("@>", 2);
	d_.greaterq = 	in_dict("@>=", 2);

	d_.reset = 	in_dict("reset",0);
	d_.block =	in_dict("block", 0);
	d_.exit_block = in_dict("exit_block",1);
	d_.call = 	in_dict("call", 1);
	d_.call_body = 	in_dict("call_", 2);
	d_.break0 =	in_dict("break", 0);
	d_.error = 	in_dict("error",2);
	d_.syserror = 	in_dict("syserror", 4);
	d_.user = 	in_dict("user", 0);
	d_.true0 = 	in_dict("true", 0);
	d_.default0 = 	in_dict("default", 0);
	d_.read = 	in_dict("read",0);
	d_.write = 	in_dict("write",0);
	d_.update =	in_dict("update",0);
	d_.append =	in_dict("append", 0);
	d_.string =	in_dict("string", 1);
	d_.dummy_call =	in_dict("dummy_call",0);
	d_.no_err_handler =	in_dict("no_err_handler",2);
	d_.error_handler =	in_dict("error_handler",2);
	d_.throw1	= in_dict("throw",1);

	d_.hang =	in_dict("hang",0);
	d_.nohang =	in_dict("nohang",0);

	/* stream names */
	d_.stdin0 =		in_dict("stdin", 0);
	d_.stdout0 =		in_dict("stdout", 0);
	d_.stderr0 =		in_dict("stderr", 0);
	d_.input = 		in_dict("input",0);
	d_.output = 		in_dict("output",0);
	d_.err = 		in_dict("error",0);
	d_.warning_output = 	in_dict("warning_output",0);
	d_.log_output = 	in_dict("log_output",0);
	d_.user_input = 	in_dict("user_input",0);
	d_.user_output = 	in_dict("user_output",0);
	d_.user_error = 	in_dict("user_error",0);
	d_.null = 		in_dict("null", 0);

	d_.flush = 		in_dict("flush",0);
	d_.emulate = 		in_dict("Emulate",0);
	d_.abort = 		in_dict("abort",0);
	d_.var = 		in_dict("var", 1);
	d_.nonground = 		in_dict("nonground", 1);
	d_.ground = 		in_dict("ground", 1);
	d_.on =			in_dict("on", 0);
	d_.off =		in_dict("off", 0);
	d_.prolog =		in_dict("prolog", 0);
	d_.system =		in_dict("system", 0);
	d_.built_in =		in_dict("built_in", 0);

		/* assert */
	d_.clause =	in_dict("clause", 3);

	d_.halt = 	in_dict("halt",0);

		/* declarations */
	d_.dynamic = 	in_dict("dynamic",1);
	d_.abolish = 	in_dict("abolish",1);
	d_.mode = 	in_dict("mode",1);
	d_.local = 	in_dict("local",1);
	d_.global = 	in_dict("global",1);
	d_.export1 = 	in_dict("export",1);
	d_.import = 	in_dict("import",1);
	d_.from = 	in_dict("from",2);
	d_.module1 = 	in_dict("module", 1);
	d_.module_directive = 	in_dict("module_directive", 4);

		/* module names */
	d_.dummy_module =	&dict->anonymous_did[ANONYMOUS_MODULE];
	d_.default_module =	in_dict(ec_options.default_module, 0);
	d_.eclipse_home =	in_dict(ec_eclipse_home, 0);
	d_.kernel_sepia =	in_dict("sepia_kernel", 0);

	       /* operators */
	d_.local0	= in_dict("local", 0);
	d_.global0	= in_dict("global", 0);

		/* debugger */
	d_.skip = 		in_dict("skip", 0);
	d_.spy = 		in_dict("spy", 0);
	d_.leash = 		in_dict("leash", 0);
	d_.ellipsis =		in_dict("...",0);

		/* modes */
	d_.plus0 =		in_dict("+", 0);
	d_.plusplus =		in_dict("++", 0);
	d_.minus0 =		in_dict("-", 0);
	d_.question =		in_dict("?", 0);

        d_.unify0 = in_dict("=", 0);
        d_.stop = in_dict("stop", 0);
        d_.print = in_dict("print", 0);
        d_.notrace = in_dict("notrace", 0);
        d_.trace = in_dict("trace", 0);
        d_.trace_frame = in_dict("tf", TF_ARITY);
        d_.debug = in_dict("debug", 0);
        d_.nodebug = in_dict("nodebug", 0);
        d_.global_trail_overflow = in_dict("global_trail_overflow", 0);
        d_.local_control_overflow = in_dict("local_control_overflow", 0);

	d_.at2 = in_dict("@", 2);
	d_.lock = in_dict("lock", 1);
	d_.localb = in_dict("local_", 2);
	d_.globalb = in_dict("global_", 2);
	d_.exportb = in_dict("export_", 2);
	d_.import_fromb = in_dict("import_from_", 3);
	d_.woken = in_dict("woken", WL_ARITY);
	d_.define_global_macro3 = in_dict("define_global_macro",3);
	d_.define_local_macro3 = in_dict("define_local_macro",3);
	d_.erase_macro1 = in_dict("erase_macro",1);
	d_.trans_term = in_dict("trans_term",2);

        d_.var0 = in_dict("var", 0);
        d_.atom0 = in_dict("atom", 0);
        d_.string0 = in_dict("string", 0);
	d_.float0 = in_dict("float",0);
        d_.integer0 = in_dict("integer", 0);
        d_.double0 = in_dict("double", 0);
        d_.rational0 = in_dict("rational", 0);
        d_.real0 = in_dict("real", 0);
	d_.byte = in_dict("byte", 0);
        d_.compound0 = in_dict("compound", 0);
	d_.universally_quantified = in_dict("universally_quantified", 0);
	d_.suspend_attr = in_dict("suspend", 3);
	d_.constrained = in_dict("constrained", 0);
	d_.meta0 = in_dict("meta", 0);
	d_.free = in_dict("free",0);

	/* properties and handle types */
	d_.bag = in_dict("bag", 0);
	d_.event = in_dict("event", 0);
	d_.record = in_dict("record", 0);
	d_.dbref = in_dict("dbref", 0);
	d_.shelf = in_dict("shelf", 0);
	d_.store = in_dict("store", 0);
	d_.stream = in_dict("stream", 0);

	/* macros */
	d_.top_only = in_dict("top_only", 0);
	d_.protect_arg = in_dict("protect_arg", 0);
	d_.clause0 = in_dict("clause", 0);
	d_.goal = in_dict("goal", 0);
	d_.term = in_dict("term", 0);

	d_.with2 =		in_dict("with", 2);
	d_.with_attributes2 =	in_dict("with attributes", 2);
	d_.apply2 =		in_dict("apply", 2);

	d_.some = in_dict("some", 0);
	d_.all = in_dict("all", 0);
	d_.one = in_dict("one", 0);

	/* built-ins */
	d_.system_debug = in_dict("system_debug", 0);
	d_.external = in_dict("external", 0);
	d_.softcut = in_dict("*->", 2);
	d_.functor = in_dict("functor", 3);
	d_.integer = in_dict("integer", 1);
        d_.double1 = in_dict("double", 1);
	d_.atom = in_dict("atom", 1);
	d_.atomic = in_dict("atomic", 1);
	d_.nonvar = in_dict("nonvar", 1);
	d_.meta = in_dict("meta", 1);
	d_.number = in_dict("number", 1);
	d_.real = in_dict("real", 1);
	d_.breal = in_dict("breal", 1);
	d_.compound = in_dict("compound", 1);
	d_.free1 = in_dict("free", 1);
	d_.bignum = in_dict("bignum", 1);
	d_.is_event = in_dict("is_event", 1);
	d_.is_handle = in_dict("is_handle", 1);
	d_.is_list = in_dict("is_list", 1);
	d_.is_suspension = in_dict("is_suspension", 1);
	d_.pragma = in_dict("pragma", 1);
	d_.make_suspension = in_dict("make_suspension", 3);
	d_.wake = in_dict("wake", 0);

	/* suspension attributes */
	d_.state = in_dict("state", 0);
	d_.priority = in_dict("priority", 0);
	d_.invoc = in_dict("invoc", 0);
	d_.module0 = in_dict("module", 0);
}



/*--------------------------------------------------------------------
 * Constant table for storing non-simple ground constants
 * other than strings and atoms.
 * Entries are made
 *	- for constants occurring in code
 *	- explicitly by calling canonical_copy/2
 * There is currently no garbage collection on this table.
 * Terms in this table are made persistent, which means that pointers
 * to these terms (and their subterms) can always be shared and never
 * need to be copied again. This is indicated by the PERSISTENT bit
 * being set in pointers (in)to these persistent heap term.
 * Also, DIDs within these terms are marked as permanent,
 * so the dictionary gc does not need to scan this table.
 *--------------------------------------------------------------------*/

#define CONSTANT_TABLE_MIN_SIZE		256
#define CONSTANT_TABLE_MAX_SIZE		1048576
#define CONSTANT_TABLE_EXPAND_FACTOR	2

#define ec_const_table ((struct constant_table *)shared_data->constant_table)

typedef struct constant_entry {			/* one table entry */
    	pword			value;
    	uword			hash;
    	struct constant_entry	*next;
} t_constant_entry;

struct constant_table {			/* the whole table */
	uword			size;
	uword			nentries;
	uword			nreuse;
	t_constant_entry	**htable;
};


/*
 * Initialise the table
 */

static void
_constant_table_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	uword i;
	shared_data->constant_table = hg_alloc_size(sizeof(struct constant_table));
	ec_const_table->size = CONSTANT_TABLE_MIN_SIZE;
	ec_const_table->nentries = 0;
	ec_const_table->nreuse = 0;
	ec_const_table->htable = (t_constant_entry **)
		hg_alloc_size(ec_const_table->size * sizeof(t_constant_entry *));
	for (i=0; i< ec_const_table->size; i++)
	    ec_const_table->htable[i] = NULL;
    }
}


/*
 * Grow the table
 */

static void
_constant_table_expand(struct constant_table *table)
{
    uword new_size = table->size * CONSTANT_TABLE_EXPAND_FACTOR;
    t_constant_entry **new_htable;
    uword i;

    /* make and initialize a larger table */
    new_htable = (t_constant_entry **)
	    hg_alloc_size(new_size * sizeof(t_constant_entry *));
    for (i = 0; i < new_size; ++i)
    {
	new_htable[i] = NULL;
    }

    /* redistribute the entries from the old table */
    for (i = 0; i < table->size; ++i)
    {
	t_constant_entry *elem;
	for(elem = table->htable[i]; elem; )
	{
	    t_constant_entry **new_slot = &new_htable[elem->hash % new_size];
	    t_constant_entry *next_elem = elem->next;
	    elem->next = *new_slot;
	    *new_slot = elem;
	    elem = next_elem;
	}
    }

    /* free the old table */
    hg_free_size(table->htable, table->size * sizeof(t_constant_entry *));

    /* assign the new one */
    table->htable = new_htable;
    table->size = new_size;
}


/*
 * Lookup/Enter
 *
 * PSUCCEED:		*presult contains the tabled copy of (v,t)
 *			    or (v,t) itself in case of simple terms
 * PFAIL:		the term cannot be tabled in a meaningful way,
 *			    e.g. because it is a bounded real
 *			    (v,t) itself is returned as result anyway
 * INSTANTIATION_FAULT:	the term was nonground
 * other:		a serious problem occurred
 */

int
ec_constant_table_enter(ec_eng_t *ec_eng, value v, type t, pword *presult)
{
    int res = PSUCCEED;		/* initialise for ec_term_hash() */
    t_constant_entry *pelem;
    t_constant_entry **pslot;
    uword hash;

    /* no point tabling simple (single-pword) things */
    if (IsSimple(t))
    {
	presult->val.all = v.all;
	presult->tag.all = t.all;
	if (IsAtom(t)) {
	    Set_Did_Stability(v.did, DICT_PERMANENT);
	}
	return PSUCCEED;
    }

    /*
     * Bounded reals cannot be shared (when nonzero width)
     * because they must not arithmetically compare equal!
     */
    if (IsInterval(t) &&  (IvlLwb(v.ptr) < IvlUpb(v.ptr)))
    {
	presult->val.all = v.all;
	presult->tag.all = t.all;
	return PFAIL;
    }

    /* compute hash value */
    hash = ec_term_hash(v, t, MAX_U_WORD, &res);
    if (res != PSUCCEED)
    {
	return res;
    }

    /* lookup the entry */
    pslot = &ec_const_table->htable[hash % ec_const_table->size];
    for(pelem = *pslot; pelem; pslot = &pelem->next, pelem = *pslot)
    {
	if (pelem->hash == hash
	 && ec_compare_terms(v, t, pelem->value.val, pelem->value.tag) == 0
	 )
	{
	    break;
	}
    }

    if (!pelem)
    {
	/* insert new entry */
	pelem = (t_constant_entry *) hg_alloc_size(sizeof(t_constant_entry));
	if ((res = create_heapterm(ec_eng, &pelem->value, v, t)) != PSUCCEED)
	{
	    hg_free_size(pelem, sizeof(t_constant_entry));
	    return res;
	}

	/*
	 * Mark it as a persistent heap term.
	 * This will also make any DIDs within the term permanent,
	 * so dictionary gc does not need to mark persistent terms.
	 */
	make_heapterm_persistent(&pelem->value);

	pelem->hash = hash;
	pelem->next = *pslot;
	*pslot = pelem;
	++ec_const_table->nentries;

	/* expand table if too full */
	if (ec_const_table->nentries > ec_const_table->size
	 && ec_const_table->size < CONSTANT_TABLE_MAX_SIZE)
	{
	    _constant_table_expand(ec_const_table);
	}

    }
    else
    {
	++ec_const_table->nreuse;
    }

    *presult = pelem->value;
    return PSUCCEED;
}


#ifdef PRINTAM
int
pr_constant_table(void)
{
    uword entry_count = 0;
    uword max_chain = 0;
    uword used_slots = 0;
    uword i;

    for(i = 0; i < ec_const_table->size; ++i)
    {
	uword chain_length = 0;
	t_constant_entry *pelem = ec_const_table->htable[i];
	if (pelem)
	    ++used_slots;
	for(; pelem; pelem = pelem->next)
	{
	    writeq_term(pelem->value.val.all, pelem->value.tag.all);
	    ++chain_length;
	}
	entry_count += chain_length;
	if (chain_length > max_chain)
	    max_chain = chain_length;
    }

    p_fprintf(current_output_, "GROUND CONSTANT TABLE\n");
    p_fprintf(current_output_, "size      = %d\n", ec_const_table->size);
    p_fprintf(current_output_, "entries   = %d\n", ec_const_table->nentries);
    p_fprintf(current_output_, "reuse     = %d\n", ec_const_table->nreuse);
    p_fprintf(current_output_, "max chain = %d\n", max_chain);
    p_fprintf(current_output_, "avg chain = %f\n", ((double) entry_count)/used_slots);
    if (entry_count != ec_const_table->nentries)
	p_fprintf(current_output_, "!!! Deviating entry count %d\n", entry_count);
    Succeed_;
}
#endif
