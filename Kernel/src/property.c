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
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: property.c,v 1.14 2017/09/01 03:05:10 jschimpf Exp $
 *
 * IDENTIFICATION:	property.c
 *
 * DESCRIPTION:		property list handling
 *
 * CONTENTS:
 *			get_property_ref()	requires lock
 *			swap_[global_]property()
 *			set_global_property()
 *			get_global_property()
 *			get_visible_property()
 *			get_visible_property_handle()
 *			erase_global_property()
 *			erase_property()
 *			erase_module_props()
 *			mark_dids_from_properties()
 *
 * AUTHOR:		bruno, joachim
 *
 * This version implements the following semantics of property lists:
 * -	There is no difference between module independent properties
 *	and module dependent global properties.
 *	Therefore the same routines can be used for both.
 * -	Independent/global properties can be created, accessed, modified
 *	and erased from everywhere. We always work on the visible property
 *	(except when a local is created it may hide a global one).
 * -	When a module is erased, its local properties are erased as well.
 */


#include "config.h"
#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "error.h"
#include "mem.h"
#include "dict.h"
#include "module.h"
#include "property.h"
#include "emu_export.h"
#include "os_support.h"


static void	free_prop_value(int, pword*);

extern void	mark_dids_from_array(pword *prop_value),
		mark_dids_from_pwords(pword *from, register pword *to),
		mark_dids_from_heapterm(pword *root);



static inline void
_rem_from_module_entry(property *m, module_item *pm)
{
    register property *p, **prev;
    prev = &(pm->properties);
    p = *prev;
    while (p != m)
    {
	if (!p) return;	/* should not happen, but ... */
	prev = &p->next_prop;
	p = *prev;
    }
    *prev = p->next_prop;
}


static inline module_item *
_module_property(dident functor)
{
    property	*p;
    for (p = DidProperties(functor); p; p = p->next_prop)
    {
	if (p->name == MODULE_PROP)
	    return (module_item*) p->property_value.val.ptr;
    }
    assert(0);
    return NULL;
}


/*
 * Get a pointer to a property value (a pword)
 *
 * 'which' is one of
 *	VISIBLE_PROP	look up the visible property, error if none exists
 *	LOCAL_PROP	access local property, create if none
 *	GLOBAL_PROP	access global property, create if none
 *
 * 'module'/mod_tag	the module (for {VISIBLE|LOCAL}_PROP), or d_.nil
 *			for GLOBAL_PROP ([] cannot be a module name)
 *
 * The return value is a pointer to the property value of the new descriptor.
 * If a descriptor already exists, it is returned (EXISTING_PROP).
 * Otherwise a new one with appropriate scope is created (NEW_PROP).
 * A new local definition hides an existing global one (HIDING_PROP).
 *
 * A global descriptor is always created, even when only local properties
 * exist. It is the one in the property chain. If no global property
 * exists, its module field contains D_UNKNOWN, otherwise it holds [].
 * The global descriptor is the head of a circular list of local properties.
 * The property_value field of any descriptor is initialised with a TEND tag.
 *
 * Returns a pointer into the property descriptor and must thus be called under
 * PropertyLock, which can be released when the pointer is no longer used.
 *
 * Return values:
 *	LOCKED		module access error (only for {VISIBLE|LOCAL}_PROP)
 *	PERROR		no such property (only for VISIBLE_PROP)
 * >0:
 *	LOCAL_PROP	existing local
 *	GLOBAL_PROP	existing global
 *	LOCAL_PROP|NEW_PROP	new local descriptor
 *	GLOBAL_PROP|NEW_PROP	new global descriptor
 *	LOCAL_PROP|GLOBAL_PROP|NEW_PROP	new local hiding existing global
 *
 * Expects:
 *	PropertyLock
 */

int
get_property_ref(dident functor, int property_name,
	dident module, type mod_tag, int which, pword **ppw)
{
    property	*p, *head;
    property	**prev;
    module_item	*pm;


    prev = &DidProperties(functor);
    p = *prev;
    while (p && p->name != property_name)	/* find the right property list */
    {
	prev = &p->next_prop;
	p = *prev;
    }
    if (!p)
    {
	if (which & VISIBLE_PROP)
	    return PERROR;
	/* make a new property list header (=global entry) */
	p = (property *) hg_alloc_size(sizeof(property));
	p->name = property_name;
	p->next_prop = NULL;
	p->next_mod = p;
	p->module = D_UNKNOWN;
	p->property_value.tag.kernel = TEND;
	*prev = p;
    }

    if (which == GLOBAL_PROP)
    {
    	if (p->module != D_UNKNOWN) {
	    *ppw = &p->property_value;
	    return GLOBAL_PROP;
	}
	p->module = module;
	*ppw = &p->property_value;
	return NEW_PROP|GLOBAL_PROP;
    }

    if (module != D_UNKNOWN && UnauthorizedAccess(module, mod_tag))
	return LOCKED;

    /* VISIBLE_PROP or LOCAL_PROP */
    head = p;	
    for(p = head->next_mod; p != head; p = p->next_mod)
    {
	if (p->module == module) {
	    *ppw = &p->property_value;
	    return LOCAL_PROP;
	}
    }

    if (which == VISIBLE_PROP)
    {
	if (head->module == D_UNKNOWN)
	    return PERROR;
	*ppw = &head->property_value;
	return GLOBAL_PROP;	/* return the global */
    }

    /* insert a new local descriptor at the beginning	*/
    p = (property *) hg_alloc_size(sizeof(property));
    p->name = property_name;
    p->module = module;
    p->property_value.tag.kernel = TEND;
    p->next_mod = head->next_mod;
    head->next_mod = p;
    
    pm = _module_property(module);
    p->next_prop = pm->properties;
    pm->properties = p;

    *ppw = &p->property_value;
    return NEW_PROP|LOCAL_PROP|(head->module==D_UNKNOWN? 0: GLOBAL_PROP);
}
	

/*
 * Create or modify a (global) property value.
 * This swaps the property value and does not need to be called locked.
 * The caller should free the old value if necessary.
 *
 * 'which' is
 *	LOCAL_PROP	access local property
 *	GLOBAL_PROP	access global property
 * Returns
 *	LOCKED		module access error (only for LOCAL_PROP)
 * >0:
 *	LOCAL_PROP	existing local (old value returned)
 *	GLOBAL_PROP	existing global (old value returned)
 *	LOCAL_PROP|NEW_PROP	new local descriptor (no old value)
 *	GLOBAL_PROP|NEW_PROP	new global descriptor (no old value)
 *	LOCAL_PROP|GLOBAL_PROP|NEW_PROP	new local hiding existing global (no old value)
 * Acquires:
 *	PropertyLock
 */

int
swap_property(dident functor, int property_name, dident module, type mod_tag,
		int which, pword *new, pword *old)
{
    int res;
    pword *pw;

    mt_mutex_lock(&PropertyLock);
    res = get_property_ref(functor, property_name, module, mod_tag, which, &pw);
    if (res >= 0) {
	if (!(res & NEW_PROP))
	    *old = *pw;
	*pw = *new;
    }
    mt_mutex_unlock(&PropertyLock);
    return res;
}

int
swap_global_property(dident functor, int property_name, pword *new, pword *old)
{
    return swap_property(functor, property_name, d_.nil, tdict, GLOBAL_PROP, new, old);
}



/*
 * Quick routine to create and/or set a module-independent property.
 * The init pword is simply copied into the property, so the caller
 * must make sure this is safe.  The old value is overwritten and lost.
 * Does not return a pointer into the property, therefore no lock
 * necessary around call.
 * Returns:
 *	GLOBAL_PROP		existing global
 *	GLOBAL_PROP|NEW_PROP	new global descriptor
 * SEE ALSO:
 *	swap_property()
 */

int
set_global_property(dident functor, int property_name, pword *init)
{
    pword *prop;
    mt_mutex_lock(&PropertyLock);
    int res = get_property_ref(functor, property_name,
			    d_.nil, tdict, GLOBAL_PROP, &prop);
    assert((res & ~NEW_PROP) == GLOBAL_PROP);
    *prop = *init;
    mt_mutex_unlock(&PropertyLock);
    return res;
}


/*
 * Quick routine to get a module-independent property.
 * Does not return a pointer into the property, therefore no lock
 * necessary around call.
 * Can only be used if the property value is simple, i.e. not affected
 * by someone else modifying/erasing the property afterwards.
 * Returns: PSUCCEED or PFAIL
 * SEE ALSO:
 *	swap_property()
 */
int
get_global_property(dident functor, int property_name, pword *result)
{
    property	*p;

    mt_mutex_lock(&PropertyLock);
    for (p = DidProperties(functor); p; p = p->next_prop)
    {
	if (p->name == property_name)
	{
	    *result = p->property_value;
	    mt_mutex_unlock(&PropertyLock);
	    return PSUCCEED;
	}
    }
    mt_mutex_unlock(&PropertyLock);
    return PFAIL;
}

pword
global_property(dident functor, int property_name)
{
    property	*p;
    pword result;

    mt_mutex_lock(&PropertyLock);
    for (p = DidProperties(functor); p; p = p->next_prop)
    {
	if (p->name == property_name)
	{
	    result = p->property_value;
	    mt_mutex_unlock(&PropertyLock);
	    return result;
	}
    }
    mt_mutex_unlock(&PropertyLock);
    result.tag.kernel = TEND;
    return result;
}


/*
 * Get the property pword.  Do not use this for TPTR properties, as they
 * must be reference-counted, use get_visible_property_handle() instead!
 * Does not return a pointer into the property, therefore no lock
 * necessary around call.
 * Returns: LOCAL_PROP, GLOBAL_PROP (>=0) or PERROR, LOCKED
 */

int
get_visible_property(dident functor, int property_name, dident module, type mod_tag, pword *result)
{
    int res;
    pword *pw;

    mt_mutex_lock(&PropertyLock);
    res = get_property_ref(functor, property_name, module, mod_tag, VISIBLE_PROP, &pw);
    if (res >= 0) {
	assert(res==LOCAL_PROP||res==GLOBAL_PROP);
	*result = *pw;
    }
    mt_mutex_unlock(&PropertyLock);
    return res;
}

pword
visible_property(dident functor, int property_name, dident module, type mod_tag, int *res)
{
    pword pw;
    pw.tag.kernel = TEND;
    *res = get_visible_property(functor, property_name, module, mod_tag, &pw);
    return pw;
}


/*
 * Acquire a copy (counted reference) of a heap-stored external object.
 * This assumes that the property is a TPTR of the given class!
 * No lock necessary around call.
 */

int
get_visible_property_handle(dident functor, int property_name,
	dident module, type mod_tag, const t_ext_type *class, t_ext_ptr *result)
{
    int res;
    pword *pw;

    mt_mutex_lock(&PropertyLock);
    res = get_property_ref(functor, property_name, module, mod_tag, VISIBLE_PROP, &pw);
    if (res >= 0)
    {
	assert(res==LOCAL_PROP||res==GLOBAL_PROP);
	assert(pw->tag.kernel == TPTR);
	*result = class->copy((t_ext_ptr) pw->val.ptr);
    }
    mt_mutex_unlock(&PropertyLock);
    return res;
}



/*
 * erase a module independent or the global property
 */

int
erase_global_property(dident functor, int property_name)
{
	return erase_property(functor, property_name,
				      D_UNKNOWN, tdict, GLOBAL_PROP);
}


/*
 * erase a property
 * flag is one of {VISIBLE_PROP, GLOBAL_PROP, LOCAL_PROP}.
 * This function can return a valid Prolog error code.
 * A successful erase may return PSUCCEED or PFAIL (the latter
 * if the property has been completely removed for functor
 * i.e the global and all locals)
 * TODO: Call under lock, because caller may need to update
 * dident flags synchronously.
 */

int
erase_property(dident functor, int property_name, dident module, type mod_tag, int which)
{
	register property	*p, **prev_p;
	int			res;

	if (which != GLOBAL_PROP && UnauthorizedAccess(module, mod_tag))
	{
	    return LOCKED;
	}

	mt_mutex_lock(&PropertyLock);
	/* get pointer to property list from atom */
	prev_p = &(DidProperties(functor));
	p = *prev_p;

	/* scan property list until an entry for property is found or end */
	while (p)
	{
	    if (p->name == property_name)
	    {
		if (which != GLOBAL_PROP)
		{
		    register property	 *m, **prev_m;

		    prev_m = &(p->next_mod);
		    m = *prev_m;

		    while (m != p)	/* scan module list */
		    {
			if (m->module == module)
			{			/* erase the local	*/
			    module_item *pm;

			    pm = _module_property(module);
			    *prev_m = m->next_mod;
			    _rem_from_module_entry(m, pm);
			    free_prop_value(property_name, &m->property_value);
			    hg_free_size(m, sizeof(property));

			    if (p->next_mod == p && p->module == D_UNKNOWN)
			    {	/* all erased, remove head descriptor	*/
				*prev_p = p->next_prop;
				hg_free_size(p, sizeof(property));
                              /* this is not an error, it is a message
                                 to notify that the property is erased
                                 completely */
                              res = PFAIL;
			      goto _unlock_return_;
			    }
			    res = PSUCCEED;
			    goto _unlock_return_;
			}
			prev_m = &(m->next_mod);
			m = *prev_m;
		    }
		}
		if (which != LOCAL_PROP  &&  p->module != D_UNKNOWN)
		{				/* erase the global	*/
		    free_prop_value(property_name, &p->property_value);
		    if (p->next_mod == p)
		    {		/* no locals: remove global descriptor	*/
			*prev_p = p->next_prop;
			hg_free_size(p, sizeof(property));
                      /* this is not an error, it is a message to notify
                         that the property is erased completely       */
			res = PFAIL;
			goto _unlock_return_;
		    }
		    else
			p->module = D_UNKNOWN;	/* just mark it unused	*/
		    res = PSUCCEED;
		    goto _unlock_return_;
		}
		res = PERROR;
		goto _unlock_return_;		/* should give a warning */
	    }
	    prev_p = &(p->next_prop);
	    p = *prev_p;
	}
	res = PERROR;
_unlock_return_:
	mt_mutex_unlock(&PropertyLock);
        return(res);
}


/*
 * this is to be called from erase_module
 * prop_list is a list of module dependent (local) property descriptors
 * linked with the next_prop field
 */

void
erase_module_props(module_item *module_prop)
{
    property *prop_list;

    mt_mutex_lock(&PropertyLock);
    prop_list = module_prop->properties;
    module_prop->properties = NULL;
    while(prop_list)
    {
	property *p = prop_list->next_mod;

	while (p->next_mod != prop_list)
	    p = p->next_mod;
	p->next_mod = prop_list->next_mod;

	p = prop_list;
	prop_list = prop_list->next_prop;
	free_prop_value((int) p->name, &p->property_value);
	hg_free_size(p, sizeof(property));
    }
    mt_mutex_unlock(&PropertyLock);
}


/*
 * free all space associated to the property value
 */

static void
free_prop_value(int prop_name, pword *prop_value)
{
    switch(prop_name)
    {
    case GLOBVAR_PROP:
	free_heapterm(prop_value);
	break;

    case ARRAY_PROP:
	ec_free_array(prop_value);
	break;

    case GLOBREF_PROP:
	/* value is in constant table */
	break;

    case IDB_PROP:
    {
	heap_rec_header_tid.free((t_ext_ptr)prop_value->val.wptr);
	break;
    }

    case HTABLE_PROP:
    {
	heap_htable_tid.free((t_ext_ptr)prop_value->val.wptr);
	break;
    }

    case SHELF_PROP:
    {
	heap_array_tid.free((t_ext_ptr)prop_value->val.wptr);
	break;
    }

    case MODULE_PROP:
    case TRANS_PROP:
    case WRITE_TRANS_PROP:
    case GOAL_TRANS_PROP:
    case WRITE_GOAL_TRANS_PROP:
    case CLAUSE_TRANS_PROP:
    case WRITE_CLAUSE_TRANS_PROP:
	hg_free(prop_value->val.ptr);
	break;

    case EVENT_PROP:
    case STREAM_PROP:
    case PREFIX_PROP:
    case INFIX_PROP:
    case POSTFIX_PROP:
    case SYSCALL_PROP:
	break;

    default:
	p_fprintf(current_err_, "Unknown property type %d in free_prop_value()\n", prop_name);
	ec_flush(current_err_);
	break;
    }
}


/*
 * Support function for the dictionary garbage collector.
 * Mark all DIDs that occur in the given property list
 * (ie. treat all the properties a single functor).
 */

void
mark_dids_from_properties(dident functor)
{
    property *prop_list;

    mt_mutex_lock(&PropertyLock);
    prop_list = functor->properties;
    for (; prop_list; prop_list = prop_list->next_prop)
    {
	register property *p = prop_list;
	do
	{
	    if (p->module != D_UNKNOWN)
	    {
		switch (p->name)
		{
		case ARRAY_PROP:
		    mark_dids_from_array(&p->property_value);
		    break;

		case GLOBVAR_PROP:
		    mark_dids_from_heapterm(&p->property_value);
		    break;

		case GLOBREF_PROP:
		    /* value is in constant table */
		    break;

		case HTABLE_PROP:
		    {
			heap_htable_tid.mark_dids((t_ext_ptr)p->property_value.val.wptr);
		    }
		    break;

		case SHELF_PROP:
		    {
			heap_array_tid.mark_dids((t_ext_ptr)p->property_value.val.wptr);
		    }
		    break;

		case IDB_PROP:
		    {
			heap_rec_header_tid.mark_dids((t_ext_ptr)p->property_value.val.wptr);
		    }
		    break;

		case TRANS_PROP:
		case WRITE_TRANS_PROP:
		case GOAL_TRANS_PROP:
		case WRITE_GOAL_TRANS_PROP:
		case CLAUSE_TRANS_PROP:
		case WRITE_CLAUSE_TRANS_PROP:
		    {
			macro_desc *md = (macro_desc *) p->property_value.val.ptr;
			Mark_Did(md->trans_function);
			Mark_Did(md->module);
		    }
		    break;

		case MODULE_PROP:
		    {
			module_item *m = (module_item *) p->property_value.val.ptr;
			register didlist *scan;
			for (scan = m->imports; scan; scan = scan->next)
			{
			    Mark_Did(scan->name);
			}
		    }
		    break;

		case STREAM_PROP:	/* just an integer */
		    break;

		case PREFIX_PROP:	/* did */
		case INFIX_PROP:	/* did */
		case POSTFIX_PROP:	/* did */
		case SYSCALL_PROP:	/* did or integer */
		case EVENT_PROP:	/* pri */
		    mark_dids_from_pwords(&p->property_value, &p->property_value + 1);
		    break;

		default:
		    p_fprintf(current_err_, "Unknown property type %d in mark_dids_from_properties()\n", p->name);
		    ec_flush(current_err_);
		    break;
		}
	    }
	    p = p->next_mod;
	} while (p != prop_list);
    }
    mt_mutex_unlock(&PropertyLock);
}


#if 0
/*
 * These two functions are used to implement the macros in external.h
 * Deprecated: they return an unprotected pointer into property!
 */

pword *
get_array_header(dident adid)
{
    pword *pw;
    int res = get_global_property(adid,
		DidArity(adid) > 0 ? ARRAY_PROP : GLOBVAR_PROP, &pw);
    return res<0 ? NULL : pw;
}

pword *
get_visible_array_header(dident adid, value vm, type tm, int *res)
{
    pword *pw;
    int res = get_visible_property(adid,
		    DidArity(adid) > 0 ? ARRAY_PROP : GLOBVAR_PROP,
		    vm.did, tm, VISIBLE_PROP, &pw);
    return res<0 ? NULL : pw;
}

#endif
