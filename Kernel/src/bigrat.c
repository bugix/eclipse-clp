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
 * Copyright (C) 1993-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, ECRC and IC-Parc
 * 
 * END LICENSE BLOCK */

/*
 * IDENTIFICATION	bigrat.c
 * 
 * VERSION		$Id: bigrat.c,v 1.14 2017/09/07 00:11:45 jschimpf Exp $
 *
 * AUTHOR		Joachim Schimpf
 *
 * DESCRIPTION		bignum and rational arithmetic
 *			Interface to GNUmp library (version 2 or 3)
 *
 *	All interfacing via struct tag_desc{}
 *	Keep this file free of built-ins !!!
 *
 *	Has to be linked with libgmp.{a,so,lib}
 */

#include "config.h"
#include "sepia.h"
#include "types.h"
#include "error.h"
#include "embed.h"
#include "mem.h"
#include "dict.h"
#include "emu_export.h"
#include "rounding_control.h"


#ifndef HAVE_LIBGMP

void
bigrat_init(void)
{
}

extern int
ecl_double_to_int_or_bignum(ec_eng_t *ec_eng, double f, pword *pres)
{
	if (MIN_S_WORD_DBL <= (f) && (f) < MAX_S_WORD_1_DBL)
	{
	    pres->val.nint = (word) (f);
	    pres->tag.kernel = TINT;
	}
	else
	{
	    Bip_Error(ARITH_EXCEPTION);
	}

	Succeed_;
}

extern int
ec_array_to_big(ec_eng_t *ec_eng, const void *p, int count, int order, int size, int endian, unsigned nails, pword *result)
{
    Bip_Error(ARITH_EXCEPTION);
}

extern int
ec_big_to_chunks(ec_eng_t *ec_eng, pword *pw1, uword chunksize, pword *result)
{
    Bip_Error(ARITH_EXCEPTION);
}

#ifdef HAVE_LONG_LONG

pword Winapi
ecl_long_long(ec_eng_t *ec_eng, const long long int l)
{
    pword pw;
    Make_Integer(&pw, 0);
    return pw;
}

int Winapi
ec_get_long_long(const pword w, long long int *l)
{
    Bip_Error(RANGE_ERROR);
}

#endif


#else

#include	"gmp.h"

/* gmp.h may not set this correctly, as it needs to be configured */
#undef GMP_LIMB_BITS
#define	GMP_LIMB_BITS (SIZEOF_WORD*8)
#undef GMP_NAIL_BITS
#define	GMP_NAIL_BITS 0


#ifdef USING_MPIR
/* MP_INT and MP_RAT are from gmp 1, and gmp provides the
   following compatibility macros, but MPIR does not.
   This is a qick work-around, a proper fix would be to update
   our code to use the more current GMP defs 
*/
typedef __mpz_struct MP_INT;
typedef __mpq_struct MP_RAT;
#endif


#if defined(HAVE_LONG_LONG) || defined(__GNUC__)
typedef long long long_long;	
#else
#ifdef HAVE___INT64
typedef __int64 long_long;
#endif
#endif


/*
#define DEBUG_RAT_ALLOC
*/


/*
 * Conversions between ECLiPSe representation and gmp representation 
 *
 *
 *			|---------------|	MP_INT:
 *			|		|	-----------------
 *			|   array of	|	| size & sign	|
 *			|    limbs	|	| alloc		|
 *			|		| <------ d		|
 * |--------------|	|---------------|	-----------------
 * |	     TBIG |	|BIGSIGN TBUFFER|
 * |	   ----------->	| bufsize	|
 * |--------------|	|---------------|
 *  pword		 pwords on global
 *
 * When we make an MP_INT/MP_RAT from a TBIG/TRAT we share the limb
 * array on the global stack. Note that such a MP_INT/MP_RAT may not
 * be assigned to, otherwise the gmp functions try to free the space
 * to the heap. The sign must be copied from the TBUFFER header.
 * The macros/functions that convert MP_INT/MP_RAT to Prolog numbers
 * always clear (free) the MP_INT/MP_RATs, so they must not be
 * accessed afterwards.
 */

#define RatNegative(pw)	((pw)->val.ptr->tag.kernel & BIGSIGN)
#define BigZero(pw)	(BufferSize(pw) == sizeof(mp_limb_t) && \
				((mp_limb_t *) BufferStart(pw))[0] == 0)
#define RatZero(pw)	BigZero(Numer(pw)->val.ptr)
#define BigPosMin(pw)	(BufferSize(pw) == sizeof(mp_limb_t) && !BigNegative(pw) \
				&& ((mp_limb_t *) BufferStart(pw))[0] == MIN_S_WORD)
#define BigOne(pw)	(BufferSize(pw) == sizeof(mp_limb_t) && \
				((mp_limb_t *) BufferStart(pw))[0] == 1)
#define RatIntegral(pw)	BigOne(Denom(pw)->val.ptr)

#define Duplicate_Buffer(old, new) { /*allow old==new*/	\
	pword *_to, *_from = old;			\
	int _i = BufferPwords(_from);			\
	(new) = _to = TG;				\
	TG += _i;					\
	Check_Gc;					\
	do { *_to++ = *_from++; } while (--_i);		\
	}

#define Negate_Big(pbig)				\
	(pbig)->tag.kernel ^= BIGSIGN;

#define Make_Big(pw, pbig)				\
        (pw)->val.ptr = (pbig);				\
	(pw)->tag.kernel = TBIG;
        
#define Make_Rat(pw, prat)				\
	(pw)->val.ptr = (prat);				\
	(pw)->tag.kernel = TRAT;

#define Push_Rat_Frame() Push_List_Frame()
#define Numer(prat) (prat)
#define Denom(prat) ((prat)+1)

#define Big_To_Mpi(pbig, mpi) {				\
	int _limbs = BufferSize(pbig)/sizeof(mp_limb_t);	\
	(mpi)->_mp_d = (mp_limb_t *) BufferStart(pbig);	\
	(mpi)->_mp_alloc = _limbs;				\
	(mpi)->_mp_size = 					\
	    (_limbs == 1 && ((mp_limb_t *) BufferStart(pbig))[0] == 0) ? 0 : \
	    BigNegative(pbig) ? -_limbs : _limbs;	\
	}

/* Create a properly normalized TINT or TBIG pword from the MP_INT mpi */
/* Clears the MP_INT! */
#define Pw_From_Mpi(pw, mpi)	_pw_from_mpi(ec_eng, pw, mpi)

/* produces a buffer holding the bignum (know not to be zero) */
/* Clears the MP_INT! */
#define Push_Big_Mpi_Nonzero(mpi) {			\
	int _size = (MpiNegative(mpi) ? -(mpi)->_mp_size : (mpi)->_mp_size);	\
	pword *_pbig = TG;				\
	mp_limb_t *_from, *_to;				\
	Push_Buffer(_size * sizeof(mp_limb_t));		\
	if (MpiNegative(mpi)) {				\
	    (_pbig)->tag.kernel |= BIGSIGN;		\
	}						\
	_from = (mpi)->_mp_d;				\
	_to = (mp_limb_t *) BufferStart(_pbig);		\
	do { *_to++ = *_from++; } while (--_size);	\
	mpz_clear(mpi);					\
	}

/* produces a buffer holding the bignum (may be zero) */
/* Clears the MP_INT! */
#define Push_Big_Mpi(mpi) 				\
	if (MpiZero(mpi)) {				\
	    pword *_pbig = TG;				\
	    Push_Buffer(sizeof(mp_limb_t));		\
	    ((mp_limb_t *) BufferStart(_pbig))[0] = 0;	\
	    mpz_clear(mpi);				\
	} else {					\
	    Push_Big_Mpi_Nonzero(mpi);			\
	}

/* produces a bignum buffer holding small integer n */
#define Push_Big_Int(neg, n) {				\
	pword *_pbig = TG;				\
	Push_Buffer(sizeof(mp_limb_t));			\
	if (neg) {					\
	    (_pbig)->tag.kernel |= BIGSIGN;		\
	    ((mp_limb_t *) BufferStart(_pbig))[0] = -(n);	\
	} else {					\
	    ((mp_limb_t *) BufferStart(_pbig))[0] = (n);	\
	}}

#define Push_Big_PosInt(n) {				\
	pword *_pbig = TG;				\
	Push_Buffer(sizeof(mp_limb_t));			\
	((mp_limb_t *) BufferStart(_pbig))[0] = (n);	\
	}

/* Produces a bignum buffer holding 64 bit integer n.
 * For 64 bit architectures an mp_limb_t is 64 bit so 
 * Push_Big_Int/Push_Big_PosInt suffices. 
 */
#if GMP_LIMB_BITS == 32	

#define Push_Big_Int64(neg, n) {			\
	pword *_pbig = TG;				\
	Push_Buffer(2 * sizeof(mp_limb_t));		\
	if (neg) {					\
	    (_pbig)->tag.kernel |= BIGSIGN;		\
	    ((mp_limb_t *) BufferStart(_pbig))[0] = ((-n) & 0xFFFFFFFF); \
	    ((mp_limb_t *) BufferStart(_pbig))[1] = (((-n) >> 32) & 0xFFFFFFFF); \
	} else {					\
	    ((mp_limb_t *) BufferStart(_pbig))[0] = (n & 0xFFFFFFFF); \
	    ((mp_limb_t *) BufferStart(_pbig))[1] = ((n >> 32) & 0xFFFFFFFF); \
	}}

#define Push_Big_PosInt64(n) {				\
	pword *_pbig = TG;				\
	Push_Buffer( 2 * sizeof(mp_limb_t));		\
	((mp_limb_t *) BufferStart(_pbig))[0] = (n & 0xFFFFFFFF);	\
	((mp_limb_t *) BufferStart(_pbig))[1] = ((n >> 32) & 0xFFFFFFFF); \
	}
#elif GMP_LIMB_BITS == 64

#define Push_Big_Int64(neg, n) Push_Big_Int(neg, n)
#define Push_Big_PosInt64(neg, n) Push_Big_PosInt(neg, n)

#else

PROBLEM: No code to cope with MP_LIMB size for this platform!

#endif

#define Push_Big_Copy(old) {				\
	pword *_from = old;				\
	pword *_to = TG;				\
	int _i = BufferPwords(_from);			\
	TG += _i;					\
	Check_Gc;					\
	do { *_to++ = *_from++; } while (--_i);		\
	}

#define Normalize_Big(pw, pbig) _pw_from_big(ec_eng, pw, pbig)

#define Rat_To_Mpr(prat, mpr) {				\
	pword *_pw = (prat)[0].val.ptr;			\
	Big_To_Mpi(_pw, mpq_numref(mpr));			\
	_pw = (prat)[1].val.ptr;			\
	Big_To_Mpi(_pw, mpq_denref(mpr));			\
	}

/* Clears the MP_RAT! */
#define Push_Rat_Mpr(mpr) {				\
	pword *_pw = TG;				\
	Push_List_Frame();				\
	Make_Big(_pw, TG);				\
	Push_Big_Mpi(mpq_numref(mpr));			\
	Make_Big(_pw+1, TG);				\
	Push_Big_Mpi(mpq_denref(mpr));			\
	}

/* Create a TRAT pword from the MP_RAT mpr */
/* Clears the MP_RAT! */
#define Pw_From_Mpr(pw, mpr) {				\
	Make_Rat(pw, TG);				\
	Push_Rat_Mpr(mpr);				\
	}


/*--------------------------------------------------------------------------
 * Stuff that should be in the gmp library but isn't
 * It relies on the internal gmp representation
 *--------------------------------------------------------------------------*/

#define MpiNegative(x) ((x)->_mp_size < 0)
#define MpiZero(x) ((x)->_mp_size == 0)

#define ABS(x) (x >= 0 ? x : -x)
#ifndef HUGE_VAL
#define HUGE_VAL (limb_value[1]*limb_value[31])
#endif

/* A 1024 bit bignum is the largest representable as a double.
 * That corresponds to 32 32-bit limbs or 16 64-bit limbs.
 * mpn_to_double looks at the upper 3 limbs (2 if 64-bit limbs),
 * which is enough for 52-bit double precision, and multiplies
 * them with their corresponding limb value.
 */
#define LIMB_VALUE_TABLE_SIZE 32
static double limb_value[LIMB_VALUE_TABLE_SIZE] = {
1.0,				/* 2^(0*32) = 2^(0*64) */
4294967296.0,			/* 2^(1*32) */
1.8446744073709552e+19,		/* 2^(2*32) = 2^(1*64) */
7.9228162514264338e+28,		/* 2^(3*32) */
3.4028236692093846e+38,
1.4615016373309029e+48,
6.2771017353866808e+57,
2.695994666715064e+67,
1.157920892373162e+77,
4.9732323640978664e+86,
2.13598703592091e+96,
9.173994463960286e+105,
3.9402006196394479e+115,
1.6923032801030364e+125,
7.2683872429560689e+134,
3.1217485503159922e+144,
1.3407807929942597e+154,
5.7586096570152914e+163,
2.4733040147310453e+173,
1.0622759856335342e+183,
4.5624406176221952e+192,
1.959553324262937e+202,
8.4162174424773976e+211,
3.6147378671465184e+221,
1.5525180923007089e+231,
6.6680144328798543e+240,
2.8638903918474961e+250,
1.2300315572313621e+260,
5.2829453113566525e+269,
2.269007733883336e+279,
9.7453140114e+288,		/* 2^(30*32) = 2(15*64) */
4.1855804968213567e+298		/* 2^(31*32) */
};

static double
mpn_to_double(mp_ptr d, mp_size_t size)
{
    double res = 0.0;
    int i = ABS(size);
#if GMP_LIMB_BITS == 32
    switch (i)
    {
    case 3:
	res =  limb_value[2] * d[2];
	/* fall through */
    case 2:
	res += limb_value[1] * d[1];
	/* fall through */
    case 1:
	res += (double) d[0];
	/* fall through */
    case 0:
	break;
    default:
	if (i-3 < LIMB_VALUE_TABLE_SIZE)
	    res =   ( (double)        d[i-3]
		    + limb_value[1] * d[i-2]
		    + limb_value[2] * d[i-1]
		    ) * limb_value[i-3];
	else
	    res = HUGE_VAL;
	break;
    }
#else
    switch (i)
    {
    case 2:
	res += limb_value[2] * d[1];
	/* fall through */
    case 1:
	res += (double) d[0];
	/* fall through */
    case 0:
	break;
    default:
	if (2*(i-2) < LIMB_VALUE_TABLE_SIZE)
	    res =   ( (double)        d[i-2]
		    + limb_value[2] * d[i-1]
		    ) * limb_value[2*(i-2)];
	else
	    res = HUGE_VAL;
	break;
    }
#endif
    return size < 0 ? -res : res;
}

#if __GNU_MP_VERSION < 3
static double
mpz_to_double(MP_INT *z)
{
    return mpn_to_double(z->_mp_d, (mp_size_t) z->_mp_size);
}

static int
mpz_getbit(MP_INT *z, uword bit)
{


    mp_size_t size = z->_mp_size;
    mp_size_t offs = bit / GMP_LIMB_BITS;

    if (size > 0)
	return offs >= size ? 0 : (z->_mp_d[offs] >> (bit%GMP_LIMB_BITS)) & 1;
/*
    else if (size < 0)
    {
	Simulate two's complement arithmetic. Not implemented.
    }
*/
    else
	return 0;
}

static void
mpz_swap(MP_INT *x, MP_INT *y)
{
    MP_INT z;
    z = *x;
    *x= *y;
    *y= z;
}
#else
#define mpz_to_double(z) mpz_get_d(z)
#define mpz_getbit(z,i) mpz_tstbit(z,i)
#endif

/*
 * Divide two bignums giving a double float result. The naive solution
 *	return mpz_to_double(num) / mpz_to_double(den);
 * suffers from floating point overflows when the numbers are huge
 * and is inefficient because it looks at unnecessarily many digits.
 *
 * IEEE double precision is 53 bits mantissa and 12 bits signed exponent.
 * So the largest integer representable with doubles is 1024 bits wide,
 * of which the first 53 are ones, i.e. it lies between 2^1023 and 2^1024.
 */

static double
mpz_fdiv(MP_INT *num, MP_INT *den)
{
    long ne, de;
    double nf, df;
    nf = mpz_get_d_2exp(&ne, num);      /* scale to 0.5..1.0 */
    df = mpz_get_d_2exp(&de, den);      /* scale to 0.5..1.0 */
    return ldexp(nf/df, ne-de);         /* divide and scale back */
}


#define HAVE_MPQ_GET_D (__GNU_MP_VERSION >= 3)
#define MPQ_GET_D_ROUNDS_TOWARDS_ZERO (__GNU_MP_VERSION > 4 || (__GNU_MP_VERSION == 4 && __GNU_MP_VERSION_MINOR >= 2))

#if HAVE_MPQ_GET_D && !MPQ_GET_D_ROUNDS_TOWARDS_ZERO
#define mpq_to_double(q) mpq_get_d(q)
#else
static double
mpq_to_double(MP_RAT *q)
{
    return mpz_fdiv(mpq_numref(q), mpq_denref(q));
}
#endif


/*
 * Try to compute a "nice" rational from a float, using continued fractions.
 * Stop when the rational converts back into the original float exactly.
 * If the process doesn't converge (due to numeric problems), or produces
 * a result longer than the fast bitwise conversion from the float mantissa,
 * then fall back to the bitwise conversion.
 */

static void
mpq_set_double(MP_RAT *q, double f)     /* float -> nice rational */
{
    double fabs = (f < 0.0) ? -f : f;	/* get rid of the sign */
    double x = fabs;
    MP_RAT b, c;
    MP_INT *pna = mpq_numref(q);        /* use output q for a directly */
    MP_INT *pda = mpq_denref(q);
    MP_INT *pnb = mpq_numref(&b);
    MP_INT *pdb = mpq_denref(&b);
    MP_INT *pnc = mpq_numref(&c);
    MP_INT *pdc = mpq_denref(&c);
    MP_INT big_xi;

    int half_exp;                       /* predict bitwise conversion size */
    double fr = frexp(fabs, &half_exp);
    int bitwise_denominator_size = DBL_MANT_DIG-(half_exp-1);
    if (bitwise_denominator_size < 1)
        bitwise_denominator_size = 1;

    mpz_set_ui(pna, 1L);                /* a = q = 1/0 */
    mpz_set_ui(pda, 0L);
    mpq_init(&b);                       /* b = 0/1 */
    mpq_init(&c);                       /* auxiliary */
    mpz_init(&big_xi);                  /* auxiliary */

    while ((mpz_fdiv(pna, pda)) != fabs)
    {
        /* infinite x indicates failure to converge */
        if (!isfinite(x))
            goto _bitwise_conversion_;

	double xi = floor(x);
	double xf = x - xi;

        /* compute a = a*xi + b for both numerator and denominator */
	mpq_swap(q, &b);
	if (x < (double)LONG_MAX)
	{
	    unsigned long int_xi = (unsigned long) xi;
	    mpz_mul_ui(pnc, pnb, int_xi);
	    mpz_mul_ui(pdc, pdb, int_xi);
	}
	else
	{
	    mpz_set_d(&big_xi, xi);
	    mpz_mul(pnc, pnb, &big_xi);
	    mpz_mul(pdc, pdb, &big_xi);
	}
	mpz_add(pna, pna, pnc);
	mpz_add(pda, pda, pdc);

        /* if it gets too long, fall back to bitwise conversion */
        if (mpz_sizeinbase(pda, 2) > bitwise_denominator_size)
           goto _bitwise_conversion_;

	x = 1.0/xf;
    }

    if (f < 0.0)
	mpq_neg(q, q);
    mpq_canonicalize(q);                /* normally not be necessary */
    goto _cleanup_;

_bitwise_conversion_:
    mpq_set_d(q, f);                    /* bitwise conversion */

_cleanup_:
    mpz_clear(&big_xi);
    mpq_clear(&c);
    mpq_clear(&b);
}


/*--------------------------------------------------------------------------
 * memory allocation
 *--------------------------------------------------------------------------*/

#ifdef DEBUG_RAT_ALLOC
static void *
_rat_alloc(int size)
{
    void *ptr = hg_alloc_size(size);
    p_fprintf(current_err_, "alloc %d at 0x%x\n", size, ptr);
    ec_flush(current_err_);
    return ptr;
}

static void *
_rat_realloc(void *ptr, int oldsize, int newsize)
{
    void *newptr = hg_realloc_size(ptr, oldsize, newsize);
    p_fprintf(current_err_, "reall %d at 0x%x to %d at 0x%x\n", oldsize, ptr,
		newsize, newptr);
    ec_flush(current_err_);
    return newptr;
}

static void
_rat_free(void *ptr, int size)
{
    p_fprintf(current_err_, "free  %d at 0x%x\n", size, ptr);
    ec_flush(current_err_);
    hg_free_size(ptr, size);
}
#endif /* DEBUG_RAT_ALLOC */

static void
_pw_from_mpi(ec_eng_t *ec_eng, pword *pw, MP_INT *mpi)
{
    if (mpi->_mp_size == 1) {
	if (mpi->_mp_d[0] < MIN_S_WORD) {	
	    pw->tag.kernel = TINT;
	    pw->val.nint = mpi->_mp_d[0];
	    mpz_clear(mpi);
	    return;
	}
    } else if (mpi->_mp_size == -1) {
	if (mpi->_mp_d[0] <= MIN_S_WORD) {
	    pw->tag.kernel = TINT;
	    pw->val.nint = - (word) mpi->_mp_d[0];
	    mpz_clear(mpi);
	    return;
	}
    } else if (mpi->_mp_size == 0) {
	pw->tag.kernel = TINT;
	pw->val.nint = 0;
	mpz_clear(mpi);
	return;
    }
    Make_Big(pw, TG);
    Push_Big_Mpi_Nonzero(mpi);
}

static void
_pw_from_big(ec_eng_t *ec_eng, pword *pw, pword *pbig)
{
    /* BigZero not handled specially here! */
    if (BufferSize(pbig) == sizeof(mp_limb_t))
    {
	mp_limb_t i = ((mp_limb_t *) BufferStart(pbig))[0];
	if (BigNegative(pbig)) {
	    if (i <= (mp_limb_t) MIN_S_WORD) {
		pw->tag.kernel = TINT;
		pw->val.nint = - (word)i;
		return;
	    }
	} else {
	    if (i < (mp_limb_t) MIN_S_WORD) {
		pw->tag.kernel = TINT;
		pw->val.nint = i;
		return;
	    }
	}
    }
    Make_Big(pw,pbig);
}


/*--------------------------------------------------------------------------
 * methods for the TBIG type
 *--------------------------------------------------------------------------*/

/*ARGSUSED*/
static int
_write_big(int quoted, stream_id stream, value vbig, type tbig)
{
    int		len1;
    MP_INT	a;

    Big_To_Mpi(vbig.ptr, &a);
    len1 = mpz_sizeinbase(&a, 10) + (mpz_sgn(&a) < 0 ? 1 : 0) + 1;
    {
	New_Array(char, s, len1);
	(void) mpz_get_str(s, 10, &a);
	(void) ec_outfs(stream, s);	/* length is not precise! */
	Delete_Array(char, s, len1);
	Succeed_;
    }
}

/*ARGSUSED*/
static int
_big_string_size(value vb, type tb, int quoted_or_base)	/* the result is not exact, may be greater */
{
    MP_INT	a;
    Big_To_Mpi(vb.ptr, &a);
    return mpz_sizeinbase(&a, quoted_or_base < 2 ? 10 : quoted_or_base)
    	+ (mpz_sgn(&a) < 0 ? 1 : 0);
}

/*ARGSUSED*/
static int
_big_to_string(value vb, type tb, char *buf, int quoted_or_base)
{
    char	*s = buf;
    MP_INT	a;

    Big_To_Mpi(vb.ptr, &a);
    (void) mpz_get_str(s, quoted_or_base < 2 ? 10 : quoted_or_base, &a);
    while (*s) s++;
    return s - buf;
}

/*ARGSUSED*/
static int
_compare_big(value v1, value v2)
{
    MP_INT	a, b;

    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    return mpz_cmp(&a, &b);
}

/*ARGSUSED*/
static int
_arith_compare_big(value v1, value v2, int *res)
{
    MP_INT	a, b;

    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    *res = mpz_cmp(&a, &b);
    Succeed_;
}

/*ARGSUSED*/
static int
_equal_big(pword *pw1, pword *pw2)
{
    MP_INT	a, b;

    Big_To_Mpi(pw1, &a);
    Big_To_Mpi(pw2, &b);
    return (mpz_cmp(&a, &b) == 0);
}

/*ARGSUSED*/
static int
_copy_size_big(value v1, type t1)
{
    return BufferPwords(v1.ptr) * sizeof(pword);
}


/*ARGSUSED*/
static pword *
_copy_heap_big(value v1, type t1, pword *top, pword *dest)
{
    int arity;
    pword *pw;

    dest->val.ptr = top;
    dest->tag.kernel = Tag(t1.kernel);
    arity = BufferPwords(v1.ptr);
    pw = v1.ptr;
    do                          /* copy arity pwords */
	*top++ = *pw++;
    while(--arity > 0);
    return(top);
}


static int
_big_from_string(ec_eng_t *ec_eng, char *s, pword *result, int base)
{
    MP_INT a;
    if (mpz_init_set_str(&a, s, base))
	{ Bip_Error(BAD_FORMAT_STRING) }
    Pw_From_Mpi(result, &a);
    Succeed_;
}


extern int
ec_array_to_big(ec_eng_t *ec_eng, const void *p, int count, int order, int size, int endian, unsigned nails, pword *result)
{
#ifndef _WIN32
    MP_INT z;
    mpz_init(&z);
    mpz_import(&z, count, order, size, endian, nails, (const void *) p);
    Pw_From_Mpi(result, &z);
#else
    /*
     * while we a use using GMP 3, a replacement for:
     * mpz_import(&z, count, 1, sizeof(int), 0, 0, (const void *) p);
     * Most significant word comes first in p.
     */
#ifdef WORDS_BIGENDIAN
#error "ec_array_to_big() only implemented for little endian"
#endif

    int i, significant_chunks, chunks_allocated;
    int *src = (int*)p;
    int *dest;

    if (size != sizeof(int))
	return UNIMPLEMENTED;

    Make_Big(result, TG);
    dest = (int *) BufferStart(TG);

    significant_chunks = count;
    while (significant_chunks > 1  &&  src[count-significant_chunks] == 0)
    	--significant_chunks;

    chunks_allocated = RoundTo(significant_chunks, sizeof(mp_limb_t)/sizeof(int));
    Push_Buffer(chunks_allocated*sizeof(int));
    for(i=0; i<significant_chunks; i++)
	dest[i] = src[count-i-1];
    for(; i<chunks_allocated; i++)
	dest[i] = 0;
#endif
    return PSUCCEED;
}


/*
 * Break a bignum into integers of chunksize bits
 */

extern int
ec_big_to_chunks(ec_eng_t *ec_eng, pword *pw1, uword chunksize, pword *result)
{
    unsigned long pos = 0;
    unsigned long offset = 0;
    MP_INT z;

    if (chunksize > SIZEOF_WORD*8)
    {
	Bip_Error(RANGE_ERROR)
    }
    if (BigNegative(pw1))
    {
    	Bip_Error(UNIMPLEMENTED);
    }
    Big_To_Mpi(pw1, &z);
    while (pos < ULONG_MAX)
    {
	pword *pw = TG;
	uword chunk = 0;
	for(;;)
	{
	    unsigned long lpos;
	    pos = mpz_scan1(&z, pos);
	    lpos = pos-offset;
	    if (lpos >= chunksize)
		break;
	    chunk |= 1<<lpos;
	    ++pos;
	}
	Make_List(result, pw);
	Push_List_Frame();
	Make_Integer(pw, chunk);
	result = pw+1;
	offset += chunksize;
    }
    Make_Nil(result);
    Succeed_;
}


#ifdef HAVE_LONG_LONG
#ifndef SIZEOF_LONG_LONG
#ifdef __SIZEOF_LONG_LONG__
#define SIZEOF_LONG_LONG __SIZEOF_LONG_LONG__
#else
#define SIZEOF_LONG_LONG 8
#endif
#endif

pword Winapi
ecl_long_long(ec_eng_t *ec_eng, const long long int l)
{
    pword w;
    if (l >= MIN_S_WORD && l <= MAX_S_WORD) {
        Make_Integer(&w, l);
    }
    else {
        Make_Big(&w, TG);
        Push_Big_Int64(l < 0, l);
    }
    return w;
}


int Winapi
ec_get_long_long(const pword w, long long int *l)
{
    const pword *pw = &w;
    Dereference_(pw);

    if (IsInteger(pw->tag))
    {
#if SIZEOF_WORD > SIZEOF_LONG_LONG
	/* range error if val.nint is too large for long long */
	if (pw->val.nint > LLONG_MAX || pw->val.nint < LLONG_MIN)
	    return RANGE_ERROR;
#endif
	*l = pw->val.nint;
    }
    else if (IsBignum(pw->tag))
    {
	int _limbs = BufferSize(pw)/sizeof(mp_limb_t);
	switch(_limbs) {
	    case 1:
		*l = ((mp_limb_t *) BufferStart(pw))[0];
		if (BigNegative(pw)) {
		    *l = -*l;
		}
		break;
	    case 2:
		*l = ((mp_limb_t *) BufferStart(pw))[1];
		if (BigNegative(pw)) {
		    *l = -((*l << 32) | (((mp_limb_t *) BufferStart(pw))[0]));
		}
		else {
		    *l = ((*l << 32) | ((mp_limb_t *) BufferStart(pw))[0]);
		}
		break;
	    default:
		return RANGE_ERROR;
	}
    }
    else if (IsRef(pw->tag))
	return INSTANTIATION_FAULT;
    else
	return TYPE_ERROR;
    return PSUCCEED;
}
#endif


/*--------------------------------------------------------------------------
 * methods for the TRAT type
 *--------------------------------------------------------------------------*/

/*ARGSUSED*/
static int
_write_rat(int quoted, stream_id stream, value vrat, type trat)
{
    int res = _write_big(quoted, stream, vrat.ptr[0].val, vrat.ptr[0].tag);
    if (res != PSUCCEED) return res;
    (void) ec_outfc(stream, '_');
#ifdef ALT_RAT_SYNTAX
    (void) ec_outfc(stream, '/');
#endif
    return _write_big(quoted, stream, vrat.ptr[1].val, vrat.ptr[1].tag);
}

/*ARGSUSED*/
static int
_rat_string_size(value vr, type tr, int quoted)
{
    MP_INT	bign;
    int		len;

    Big_To_Mpi(vr.ptr[0].val.ptr, &bign);
    len = mpz_sizeinbase(&bign, 10) + (mpz_sgn(&bign) < 0 ? 1 : 0);
    Big_To_Mpi(vr.ptr[1].val.ptr, &bign);
    len += mpz_sizeinbase(&bign, 10);
#ifdef ALT_RAT_SYNTAX
    return len + 2;		/* for the "_/" */
#else
    return len + 1;		/* for the "_" */
#endif
}

/*ARGSUSED*/
static int
_rat_to_string(value vr, type tr, char *buf, int quoted)
{
    MP_INT	bign;
    char	*s = buf;

    Big_To_Mpi(vr.ptr[0].val.ptr, &bign);
    (void) mpz_get_str(s, 10, &bign);
    while (*s) s++;
    *s++ = '_';
#ifdef ALT_RAT_SYNTAX
    *s++ = '/';
#endif
    Big_To_Mpi(vr.ptr[1].val.ptr, &bign);
    (void) mpz_get_str(s, 10, &bign);
    while (*s) s++;
    return s - buf;
}

/*ARGSUSED*/
static int
_compare_rat(value v1, value v2)
{
    MP_RAT	a, b;

    Rat_To_Mpr(v1.ptr, &a);
    Rat_To_Mpr(v2.ptr, &b);
    return mpq_cmp(&a, &b);
}

/*ARGSUSED*/
static int
_arith_compare_rat(value v1, value v2, int *res)
{
    MP_RAT	a, b;

    Rat_To_Mpr(v1.ptr, &a);
    Rat_To_Mpr(v2.ptr, &b);
    *res = mpq_cmp(&a, &b);
    Succeed_;
}

/*ARGSUSED*/
static int
_equal_rat(pword *pw1, pword *pw2)
{
    MP_RAT	a, b;

    Rat_To_Mpr(pw1, &a);
    Rat_To_Mpr(pw2, &b);
    return mpq_equal(&a, &b);
}

/*ARGSUSED*/
static int
_copy_size_rat(value v1, type t1)
{
    return _copy_size_big(v1.ptr[0].val, v1.ptr[0].tag)
	 + _copy_size_big(v1.ptr[1].val, v1.ptr[1].tag)  
	 + 2 * sizeof(pword);
}

/*ARGSUSED*/
static pword *
_copy_heap_rat(value v1, type t1, pword *top, pword *dest)
{
    dest->tag.kernel = Tag(t1.kernel);
    dest->val.ptr = top;
    dest = top;
    top += 2;
    top = _copy_heap_big(v1.ptr[0].val, v1.ptr[0].tag, top, dest);
    dest++;
    top = _copy_heap_big(v1.ptr[1].val, v1.ptr[1].tag, top, dest);
    return top;
}

static int
_rat_from_string(ec_eng_t *ec_eng,
	char *s,	/* points to valid rational representation */
	pword *result,
	int base)
{
    MP_RAT a;
    char *s1;

    mpq_init(&a);
    for (s1=s; *s1 != '_'; s1++)
	;
    *s1 = '\0';		/* not quite clean ... */
    if (mpz_set_str(mpq_numref(&a), s, base)) {
	*s1++ = '_';
	mpq_clear(&a);
	Bip_Error(BAD_FORMAT_STRING)
    }
    *s1++ = '_';
#ifdef ALT_RAT_SYNTAX
    if (*s1 == '/') ++s1;	/* allow 3_4 or 3_/4 */
#endif
    if (mpz_set_str(mpq_denref(&a), s1, base) || MpiZero(mpq_denref(&a))) {
	mpq_clear(&a);
	Bip_Error(BAD_FORMAT_STRING)
    }
    mpq_canonicalize(&a);
    Pw_From_Mpr(result, &a);
    Succeed_;
}


/*--------------------------------------------------------------------------
 * type coercion functions
 *--------------------------------------------------------------------------*/

/*ARGSUSED*/
static int
_unimp_err(void)
{
    return UNIMPLEMENTED;
}

static int
_int_big(ec_eng_t *ec_eng, value in, value *out)	/* CAUTION: we allow out == &in */
{
    /* this is the only place where we create unnormalized bignums */
    out->ptr = TG;
    Push_Big_Int(in.nint < 0, in.nint);
    Succeed_;
}

static int
_int_rat(ec_eng_t *ec_eng, value in, value *out)	/* CAUTION: we allow out == &in */
{
    pword *pw = TG;
    Push_Rat_Frame();
    Make_Big(Numer(pw), TG);
    Push_Big_Int(in.nint < 0, in.nint);
    Make_Big(Denom(pw), TG);
    Push_Big_PosInt(1);
    out->ptr = pw;
    Succeed_;
}

static int
_int_nicerat(ec_eng_t *ec_eng, value in, pword *pres)
{
    pres->tag.kernel = TRAT;
    return _int_rat(ec_eng, in, &pres->val);
}

static int
_big_rat(ec_eng_t *ec_eng, value in, value *out)	/* CAUTION: we allow out == &in */
{
    pword *pw = TG;
    Push_Rat_Frame();
    Make_Big(Numer(pw), in.ptr);
    Make_Big(Denom(pw), TG);
    Push_Big_PosInt(1);
    out->ptr = pw;
    Succeed_;
}

static int
_big_nicerat(ec_eng_t *ec_eng, value in, pword *pres)
{
    pres->tag.kernel = TRAT;
    return _big_rat(ec_eng, in, &pres->val);
}

static int
_big_dbl(ec_eng_t *ec_eng, value in, value *out)	/* CAUTION: we allow out == &in */
{
    MP_INT a;
    Big_To_Mpi(in.ptr, &a);
    Make_Double_Val(*out, mpz_to_double(&a));
    Succeed_;
}

static int
_rat_dbl(ec_eng_t *ec_eng, value in, value *out)	/* CAUTION: we allow out == &in */
{
    MP_RAT a;
    Rat_To_Mpr(in.ptr, &a);
    Make_Double_Val(*out,  mpq_to_double(&a));
    Succeed_;
}

static int
_rat_ivl(ec_eng_t *ec_eng, value in, value *out)	/* CAUTION: we allow out == &in */
{
    pword *pw;
    value dval;
    MP_RAT a;
    Rat_To_Mpr(in.ptr, &a);

/* Disabled, because it leads to machine-dependent test results */
/* #if HAVE_MPQ_GET_D && MPQ_GET_D_ROUNDS_TOWARDS_ZERO */
#if 0
    Make_Double_Val(dval, mpq_get_d(&a));
    if (Dbl(dval) < 0.0) {
        Push_Interval(pw, down(Dbl(dval)), Dbl(dval));
    } else {
        Push_Interval(pw, Dbl(dval), up(Dbl(dval)));
    }
#else
    /* mpq_get_d() rounds to nearest, or we are using our own mpq_to_double() */
    Make_Double_Val(dval, mpq_to_double(&a));
    Push_Interval(pw, down(Dbl(dval)), up(Dbl(dval)));
#endif

    out->ptr = pw;
    Succeed_;
}

/*ARGSUSED*/
static int
_dbl_rat(ec_eng_t *ec_eng, value in, value *out)	/* CAUTION: we allow out == &in */
{
    MP_RAT c;
    if (!isfinite(Dbl(in)))
	{ Bip_Error(ARITH_EXCEPTION); }
    mpq_init(&c);
    mpq_set_d(&c, Dbl(in));
    out->ptr = TG;
    Push_Rat_Mpr(&c);
    Succeed_;
}

static int
_dbl_nicerat(ec_eng_t *ec_eng, value in, pword *pres)
{
    MP_RAT c;
    if (!isfinite(Dbl(in)))
	{ Bip_Error(ARITH_EXCEPTION); }
    mpq_init(&c);
    mpq_set_double(&c, Dbl(in));
    Make_Rat(pres, TG);
    Push_Rat_Mpr(&c);
    Succeed_;
}


/*--------------------------------------------------------------------------
 * Bignum operations
 *
 * Integers that fit in a word are normally represented as TINT, so TBIGs
 * are really big (> 2147483647 or < -2147483648).
 * However, for operations with two input arguments, one of them may be an
 * unnormalized TBIG due to type coercion.
 * Results always have to be normalized.
 *--------------------------------------------------------------------------*/

static int
_big_nop(ec_eng_t *ec_eng, value v1, pword *pres)
{
    Make_Big(pres, v1.ptr);
    Succeed_;
}

static int
_big_add(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_INT a,b,c;
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    mpz_add(&c, &a, &b);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_next(ec_eng_t *ec_eng, value v1, pword *pres)
{
    MP_INT a,c;
    if (BigNegative(v1.ptr)) {
	Fail_;
    }
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    mpz_add_ui(&c, &a, 1L);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_prev(ec_eng_t *ec_eng, value v1, pword *pres)
{
    MP_INT a,c;
    if (BigNegative(v1.ptr) /*|| BigZero(v1.ptr)*/) {
	Fail_;
    }
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    mpz_sub_ui(&c, &a, 1L);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_sub(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_INT a,b,c;
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    mpz_sub(&c, &a, &b);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_mul(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_INT a,b,c;
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    mpz_mul(&c, &a, &b);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_idiv(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_INT a,b,c;
    if (BigZero(v2.ptr))
	{ Bip_Error(ARITH_EXCEPTION); }
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    mpz_tdiv_q(&c, &a, &b);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_rem(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_INT a,b,c;
    if (BigZero(v2.ptr))
	{ Bip_Error(ARITH_EXCEPTION); }
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    mpz_tdiv_r(&c, &a, &b);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_floordiv(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_INT a,b,c;
    if (BigZero(v2.ptr))
	{ Bip_Error(ARITH_EXCEPTION); }
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    mpz_fdiv_q(&c, &a, &b);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_floorrem(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_INT a,b,c;
    if (BigZero(v2.ptr)) {
	Normalize_Big(pres, v1.ptr);
	Succeed_;
    }
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    mpz_fdiv_r(&c, &a, &b);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_pow(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_INT a,c;
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    if (v2.nint < 0)	/* big x neg_int -> rat */
    {
	mpz_pow_ui(&c, &a, (uword) (-v2.nint));
	Make_Rat(pres, TG);
	Push_Rat_Frame();
	Make_Big(Numer(pres->val.ptr), TG);
	Push_Big_PosInt(1);
	Make_Big(Denom(pres->val.ptr), TG);
	Push_Big_Mpi(&c);
    }
    else		/* big x int -> big */
    {
	mpz_pow_ui(&c, &a, (uword) v2.nint);
	Pw_From_Mpi(pres, &c);
    }
    Succeed_;
}

static int
_big_min(ec_eng_t *ec_eng,
	value v1,	/* CAUTION: One of the inputs may be unnormalized */
	value v2,
	pword *pres)
{
    MP_INT a,b;
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    pres->val.ptr = mpz_cmp(&a, &b) < 0 ? v1.ptr : v2.ptr;
    Normalize_Big(pres, pres->val.ptr);
    Succeed_;
}

static int
_big_max(ec_eng_t *ec_eng,
	value v1,	/* CAUTION: One of the inputs may be unnormalized */
	value v2,
	pword *pres)
{
    MP_INT a,b;
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    pres->val.ptr = mpz_cmp(&a, &b) > 0 ? v1.ptr : v2.ptr;
    Normalize_Big(pres, pres->val.ptr);
    Succeed_;
}

static int
_big_neg(ec_eng_t *ec_eng,
	value v1,	/* can't be zero */
	pword *pres)
{
    pword *pw;
    if (BigPosMin(v1.ptr)) {
	Make_Integer(pres, MIN_S_WORD);
    } else {
	Duplicate_Buffer(v1.ptr, pw);
	Negate_Big(pw);
	Make_Big(pres, pw);
    }
    Succeed_;
}

static int
_big_abs(ec_eng_t *ec_eng, value v1, pword *pres)
{
    if (BigNegative(v1.ptr))
    {
	return _big_neg(ec_eng, v1, pres);
    }
    Make_Big(pres, v1.ptr);
    Succeed_;
}

static int
_big_sgn(ec_eng_t *ec_eng, value v1,	/* can't be zero */
	pword *pres)
{
    Make_Integer(pres, BigNegative(v1.ptr) ? -1: 1);
    Succeed_;
}

/* By convention, vsign is an integer 1, 0 or -1 */
static int
_big_copysign(ec_eng_t *ec_eng, value v, value vsign, pword *pres)
{
    if (BigNegative(v.ptr) ? (vsign.nint >= 0) : (vsign.nint < 0))
	return _big_neg(ec_eng, v, pres);
    Make_Big(pres, v.ptr);
    Succeed_;
}

static int
_big_nexttoward(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    int res;
    MP_INT a, b, c;

    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    res = mpz_cmp(&a, &b);
    if (res == 0) {
	Make_Big(pres, v1.ptr);
    } else {
	mpz_init(&c);
	if (res < 0)
	    mpz_add_ui(&c, &a, 1L);
	else
	    mpz_sub_ui(&c, &a, 1L);
	Pw_From_Mpi(pres, &c);
    }
    Succeed_;
}

static int
_big_and(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_INT a,b,c;
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    mpz_and(&c, &a, &b);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_or(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_INT a,b,c;
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    mpz_ior(&c, &a, &b);
    Pw_From_Mpi(pres, &c);	/* might be small negative number */
    Succeed_;
}

static int
_big_xor(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
#if __GNU_MP_VERSION < 3
    Bip_Error(UNIMPLEMENTED);
#else
    MP_INT a,b,c;
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    mpz_xor(&c, &a, &b);
    Pw_From_Mpi(pres, &c);
    Succeed_;
#endif
}

static int
_big_com(ec_eng_t *ec_eng, value v1, pword *pres)
{
    MP_INT a,c;
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    mpz_com(&c, &a);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_shl(ec_eng_t *ec_eng, value v1, value v2, pword *pres)	/* big x int -> big */
{
    MP_INT a,c;
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    if (v2.nint >= 0)
	mpz_mul_2exp(&c, &a, (uword) v2.nint);
    else
#if __GNU_MP_VERSION < 3
	mpz_div_2exp(&c, &a, (uword) -v2.nint);
#else
        mpz_fdiv_q_2exp(&c, &a, (uword) -v2.nint);
#endif
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_shr(ec_eng_t *ec_eng, value v1, value v2, pword *pres)	/* big x int -> big */
{
    MP_INT a,c;
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    if (v2.nint >= 0)
#if __GNU_MP_VERSION < 3
	mpz_div_2exp(&c, &a, (uword) v2.nint);
#else
        mpz_fdiv_q_2exp(&c, &a, (uword) v2.nint);
#endif
    else
	mpz_mul_2exp(&c, &a, (uword) -v2.nint);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_setbit(ec_eng_t *ec_eng, value v1, value v2, pword *pres)	/* big x int -> big */
{
    MP_INT a,c;
    Big_To_Mpi(v1.ptr, &a);
    mpz_init_set(&c, &a);
    mpz_setbit(&c, (uword) v2.nint);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_clrbit(ec_eng_t *ec_eng, value v1, value v2, pword *pres)	/* big x int -> big */
{
    MP_INT a,c;
    Big_To_Mpi(v1.ptr, &a);
    mpz_init_set(&c, &a);
    mpz_clrbit(&c, (uword) v2.nint);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_getbit(ec_eng_t *ec_eng, value v1, value v2, pword *pres)	/* big x int -> int */
{
    MP_INT a;
    if (BigNegative(v1.ptr))
	{ Bip_Error(UNIMPLEMENTED); }
    Big_To_Mpi(v1.ptr, &a);
    Make_Integer(pres, mpz_getbit(&a, v2.nint));
    Succeed_;
}

static int
_big_gcd(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_INT a,b,c;
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    mpz_gcd(&c, &a, &b);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_gcd_ext(ec_eng_t *ec_eng, value v1, value v2, pword *ps, pword *pt, pword *pg)
{
  MP_INT a,b,g,s,t;
  mpz_init(&g);
  mpz_init(&s);
  mpz_init(&t);
  Big_To_Mpi(v1.ptr, &a);
  Big_To_Mpi(v2.ptr, &b);
  mpz_gcdext(&g, &s, &t, &a, &b);

  Pw_From_Mpi(ps, &s);    
  Pw_From_Mpi(pt, &t);
  Pw_From_Mpi(pg, &g);
  
  Succeed_;
}

  
static int
_big_lcm(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_INT a,b,c;
    mpz_init(&c);
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    mpz_lcm(&c, &a, &b);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_big_powm(ec_eng_t *ec_eng, value vbase, value vexp, value vmod, pword *pres)
{
    MP_INT a,b,c,d;
    if (BigNegative(vexp.ptr)) { Bip_Error(UNIMPLEMENTED); }
    Big_To_Mpi(vbase.ptr, &a);
    Big_To_Mpi(vexp.ptr, &b);
    Big_To_Mpi(vmod.ptr, &c);
    mpz_init(&d);
    mpz_powm(&d, &a, &b, &c);
    Pw_From_Mpi(pres, &d);
    Succeed_;
}

static int
_big_atan2(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_INT a,b;
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    Make_Double(pres, Atan2(mpz_to_double(&a), mpz_to_double(&b)));
    Succeed_;
}


/*--------------------------------------------------------------------------
 * Miscellaneous operations
 *--------------------------------------------------------------------------*/

static int
_int_neg(ec_eng_t *ec_eng, value v1, pword *pres)
{
    if (v1.nint == MIN_S_WORD)
    {
	Make_Big(pres, TG);
	Push_Big_PosInt(MIN_S_WORD);
    }
    else
    {
    	Make_Integer(pres, -v1.nint);
    }
    Succeed_;
}

/*
 * We want to provide a double -> TINT or TBIG function with two slightly
 * different interfaces: one suitable for the arith_op table's ARITH_FIX
 * operator, and one suitable for external code to call which doesn't
 * require the double to be in TDBL packaging.
 */

#define Dbl_Fix(f, pres) {						\
	if (MIN_S_WORD_DBL <= (f) && (f) < MAX_S_WORD_1_DBL)		\
	{								\
	    pres->val.nint = (word) (f);				\
	    pres->tag.kernel = TINT;					\
	}								\
	else if (!isfinite(f))                                          \
	{								\
	    Bip_Error(ARITH_EXCEPTION);					\
	}								\
	else                    /* convert to a bignum */		\
	{								\
	    MP_INT c;							\
	    mpz_init(&c);						\
	    mpz_set_d(&c, f);				 		\
	    Make_Big(pres, TG);						\
	    Push_Big_Mpi_Nonzero(&c);			 		\
	}								\
    }

static int
_dbl_fix(ec_eng_t *ec_eng, value v1, pword *pres)
{
    double f = Dbl(v1);
    Dbl_Fix(f, pres);
    Succeed_;
}

static int
_dbl_int2(ec_eng_t *ec_eng, value v1, pword *pres)
{
    double ipart;
    double fpart = modf(Dbl(v1), &ipart);

    if (fpart != 0.0)
    {
	Bip_Error(ARITH_EXCEPTION);
    }
    else if (MIN_S_WORD_DBL <= (ipart) && (ipart) < MAX_S_WORD_1_DBL)
    {
	pres->val.nint = (word) ipart;
	pres->tag.kernel = TINT;
    }
    else if (!isfinite(ipart))
    {
	Bip_Error(ARITH_EXCEPTION);
    }
    else
    {
	MP_INT c;
	mpz_init(&c);
	mpz_set_d(&c, ipart);
	Make_Big(pres, TG);
	Push_Big_Mpi_Nonzero(&c);
    }
    Succeed_;
}

extern int
ecl_double_to_int_or_bignum(ec_eng_t *ec_eng, double f, pword *pres)
{
    Dbl_Fix(f, pres);
    Succeed_;
}

/*--------------------------------------------------------------------------
 * Rational operations
 *--------------------------------------------------------------------------*/

static int
_rat_nop(ec_eng_t *ec_eng, value v1, pword *pres)
{
    Make_Rat(pres, v1.ptr);
    Succeed_;
}

static int
_rat_neg(ec_eng_t *ec_eng, value v1, pword *pres)
{
    if (RatZero(v1.ptr))
    {
	Make_Rat(pres, v1.ptr);
    }
    else
    {
	pword *pw = TG;
	Push_Rat_Frame();
	Make_Big(Numer(pw), TG);
	Push_Big_Copy(Numer(v1.ptr)->val.ptr);
	Negate_Big(Numer(pw)->val.ptr);
	Make_Big(Denom(pw), Denom(v1.ptr)->val.ptr);
	Make_Rat(pres, pw);
    }
    Succeed_;
}

static int
_rat_abs(ec_eng_t *ec_eng, value v1, pword *pres)
{
    if (RatNegative(v1.ptr))
    {
	return _rat_neg(ec_eng, v1, pres);
    }
    Make_Rat(pres, v1.ptr);
    Succeed_;
}

static int
_rat_sgn(ec_eng_t *ec_eng, value v1, pword *pres)
{
    Make_Integer(pres, RatNegative(v1.ptr) ? -1: RatZero(v1.ptr)? 0: 1);
    Succeed_;
}

/* By convention, vsign is an integer 1, 0 or -1 */
static int
_rat_copysign(ec_eng_t *ec_eng, value v, value vsign, pword *pres)
{
    if (RatNegative(v.ptr) ? (vsign.nint >= 0) : (vsign.nint < 0))
	return _rat_neg(ec_eng, v, pres);
    Make_Rat(pres, v.ptr);
    Succeed_;
}

static int
_rat_add(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_RAT a,b,c;
    mpq_init(&c);
    Rat_To_Mpr(v1.ptr, &a);
    Rat_To_Mpr(v2.ptr, &b);
    mpq_add(&c, &a, &b);
    Pw_From_Mpr(pres, &c);
    Succeed_;
}

static int
_rat_sub(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_RAT a,b,c;
    mpq_init(&c);
    Rat_To_Mpr(v1.ptr, &a);
    Rat_To_Mpr(v2.ptr, &b);
    mpq_sub(&c, &a, &b);
    Pw_From_Mpr(pres, &c);
    Succeed_;
}

static int
_rat_mul(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_RAT a,b,c;
    mpq_init(&c);
    Rat_To_Mpr(v1.ptr, &a);
    Rat_To_Mpr(v2.ptr, &b);
    mpq_mul(&c, &a, &b);
    Pw_From_Mpr(pres, &c);
    Succeed_;
}

/* only used if PREFER_RATIONALS */
static int
_int_div(ec_eng_t *ec_eng, value v1, value v2, pword *pres)	/* int x int -> rat */
{
    MP_RAT c;
    if (v2.nint == 0)
	{ Bip_Error(ARITH_EXCEPTION); }
    mpz_init_set_si(mpq_numref(&c), v1.nint);
    mpz_init_set_si(mpq_denref(&c), v2.nint);
    mpq_canonicalize(&c);
    Make_Rat(pres, TG);
    Push_Rat_Mpr(&c);
    Succeed_;
}


static int
_big_div(ec_eng_t *ec_eng, value v1, value v2, pword *pres)	/* big x big -> rat */
{
    MP_INT a,b;
    Big_To_Mpi(v1.ptr, &a);
    Big_To_Mpi(v2.ptr, &b);
    if (EclGblFlags & PREFER_RATIONALS)
    {
        MP_RAT c;
        if (MpiZero(&b))
            { Bip_Error(ARITH_EXCEPTION); }
        mpz_init_set(mpq_numref(&c), &a);
        mpz_init_set(mpq_denref(&c), &b);
        mpq_canonicalize(&c);
	Pw_From_Mpr(pres, &c);
    }
    else
    {
	double d = mpz_fdiv(&a, &b);
	Make_Checked_Float(pres, d);
    }
    Succeed_;
}

static int
_rat_div(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_RAT a,b,c;
    if (RatZero(v2.ptr))
	{ Bip_Error(ARITH_EXCEPTION); }
    mpq_init(&c);
    Rat_To_Mpr(v1.ptr, &a);
    Rat_To_Mpr(v2.ptr, &b);
    mpq_div(&c, &a, &b);
    Pw_From_Mpr(pres, &c);
    Succeed_;
}

static int
_rat_pow(ec_eng_t *ec_eng, value v1, value v2, pword *pres)		/* rat x int -> rat */
{
    MP_RAT c;
    mpq_init(&c);
    if (v2.nint == 0)
    {
	mpq_set_ui(&c, 1L, 1L);
    }
    else
    {
	word exp = v2.nint < 0 ? -v2.nint : v2.nint;
	MP_RAT a,b;
	Rat_To_Mpr(v1.ptr, &b);
	mpq_init(&a);
	mpq_set(&a, &b);	/* need to copy because we assign to it */
	if (exp % 2)
	    mpq_set(&c, &a);
	else
	    mpq_set_ui(&c, 1L, 1L);
	while ((exp /= 2) != 0)
	{
	    mpq_mul(&a, &a, &a);
	    if (exp % 2)
		mpq_mul(&c, &c, &a);
	}
	mpq_clear(&a);
	if (v2.nint < 0)
	    mpq_inv(&c, &c);
    }
    Pw_From_Mpr(pres, &c);
    Succeed_;
}

static int
_rat_min(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_RAT a,b;
    Rat_To_Mpr(v1.ptr, &a);
    Rat_To_Mpr(v2.ptr, &b);
    Make_Rat(pres, mpq_cmp(&a, &b) < 0 ? v1.ptr : v2.ptr);
    Succeed_;
}

static int
_rat_max(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_RAT a,b;
    Rat_To_Mpr(v1.ptr, &a);
    Rat_To_Mpr(v2.ptr, &b);
    Make_Rat(pres, mpq_cmp(&a, &b) > 0 ? v1.ptr : v2.ptr);
    Succeed_;
}

static int
_rat_fix(ec_eng_t *ec_eng, value v1, pword *pres)
{
    MP_INT a,b,c;
    Big_To_Mpi(Numer(v1.ptr)->val.ptr, &a);
    Big_To_Mpi(Denom(v1.ptr)->val.ptr, &b);
    mpz_init(&c);
    mpz_tdiv_q(&c, &a, &b);
    Pw_From_Mpi(pres, &c);
    Succeed_;
}

static int
_rat_int2(ec_eng_t *ec_eng, value v1, pword *pres)
{
    if (!RatIntegral(v1.ptr))
    {
	Bip_Error(ARITH_EXCEPTION);
    }
    Normalize_Big(pres, Numer(v1.ptr)->val.ptr);
    Succeed_;
}

static int
_rat_f_c(ec_eng_t *ec_eng, value v1, pword *pres, void (*div_function) (MP_INT*, const MP_INT*, const MP_INT*))
{
    MP_INT a,b,c;
    Big_To_Mpi(Denom(v1.ptr)->val.ptr, &b);
    if (mpz_cmp_si(&b, 1L) == 0)
    {
	Make_Rat(pres, v1.ptr);		/* it's already integer */
    }
    else
    {
	Big_To_Mpi(Numer(v1.ptr)->val.ptr, &a);
	mpz_init(&c);
	(*div_function)(&c, &a, &b);
	Make_Rat(pres, TG);
	Push_Rat_Frame();
	Make_Big(Numer(pres->val.ptr), TG);
	Push_Big_Mpi(&c);
	Make_Big(Denom(pres->val.ptr), TG);
	Push_Big_PosInt(1);
    }
    Succeed_;
}

static int
_rat_floor(ec_eng_t *ec_eng, value v1, pword *pres)
{
    return _rat_f_c(ec_eng, v1, pres, mpz_fdiv_q);
}

static int
_rat_ceil(ec_eng_t *ec_eng, value v1, pword *pres)
{
    return _rat_f_c(ec_eng, v1, pres, mpz_cdiv_q);
}

static int
_rat_truncate(ec_eng_t *ec_eng, value v1, pword *pres)
{
    return _rat_f_c(ec_eng, v1, pres, mpz_tdiv_q);
}

static int
_rat_num(ec_eng_t *ec_eng, value v1, pword *pres)
{
    Normalize_Big(pres, Numer(v1.ptr)->val.ptr);
    Succeed_;
}

static int
_rat_den(ec_eng_t *ec_eng, value v1, pword *pres)
{
    Normalize_Big(pres, Denom(v1.ptr)->val.ptr);
    Succeed_;
}

static int
_rat_atan2(ec_eng_t *ec_eng, value v1, value v2, pword *pres)
{
    MP_RAT a,b;
    Rat_To_Mpr(v1.ptr, &a);
    Rat_To_Mpr(v2.ptr, &b);
    Make_Double(pres, Atan2(mpq_to_double(&a), mpq_to_double(&b)));
    Succeed_;
}


/*--------------------------------------------------------------------------
 * Initialize bignums and rationals
 *--------------------------------------------------------------------------*/

void
bigrat_init(void)
{
#ifdef DEBUG_RAT_ALLOC
    mp_set_memory_functions(_rat_alloc, _rat_realloc, _rat_free);
#else
    mp_set_memory_functions((void*(*)(size_t)) hg_alloc_size,
		(void*(*)(void*, size_t, size_t)) hg_realloc_size,
		(void(*)(void*, size_t)) hg_free_size);
#endif

    tag_desc[TINT].coerce_to[TBIG] = _int_big;		/* 32 bit integers */
    tag_desc[TINT].coerce_to[TRAT] = _int_rat;
    tag_desc[TINT].arith_op[ARITH_DIV] = _int_div;	/* may yield rat */
    tag_desc[TINT].arith_op[ARITH_CHGSIGN] =
    tag_desc[TINT].arith_op[ARITH_NEG] = _int_neg;	/* may yield big */
    tag_desc[TINT].arith_op[ARITH_NICERAT] = _int_nicerat;

    tag_desc[TDBL].coerce_to[TRAT] = _dbl_rat;		/* double */
    tag_desc[TDBL].arith_op[ARITH_FIX] = _dbl_fix;
    tag_desc[TDBL].arith_op[ARITH_INT] = _dbl_int2;
    tag_desc[TDBL].arith_op[ARITH_NICERAT] = _dbl_nicerat;

    tag_desc[TBIG].from_string = _big_from_string;	/* bignums */
    tag_desc[TBIG].write = _write_big;
    tag_desc[TBIG].compare = _compare_big;
    tag_desc[TBIG].arith_compare = _arith_compare_big;
    tag_desc[TBIG].equal = _equal_big;
    tag_desc[TBIG].copy_size = _copy_size_big;
    tag_desc[TBIG].copy_to_heap = _copy_heap_big;
    tag_desc[TBIG].string_size = _big_string_size;
    tag_desc[TBIG].to_string = _big_to_string;
    tag_desc[TBIG].coerce_to[TRAT] = _big_rat;
    tag_desc[TBIG].coerce_to[TDBL] = _big_dbl;
    tag_desc[TBIG].arith_op[ARITH_PLUS] = _big_nop;
    tag_desc[TBIG].arith_op[ARITH_FLOAT] = _big_nop;	/* handled by coercion */
    tag_desc[TBIG].arith_op[ARITH_ROUND] = _big_nop;
    tag_desc[TBIG].arith_op[ARITH_FLOOR] = _big_nop;
    tag_desc[TBIG].arith_op[ARITH_CEIL] = _big_nop;
    tag_desc[TBIG].arith_op[ARITH_TRUNCATE] = _big_nop;
    tag_desc[TBIG].arith_op[ARITH_FIX] = _big_nop;
    tag_desc[TBIG].arith_op[ARITH_INT] = _big_nop;
    tag_desc[TBIG].arith_op[ARITH_MIN] = _big_min;
    tag_desc[TBIG].arith_op[ARITH_MAX] = _big_max;
    tag_desc[TBIG].arith_op[ARITH_ABS] = _big_abs;
    tag_desc[TBIG].arith_op[ARITH_CHGSIGN] =
    tag_desc[TBIG].arith_op[ARITH_NEG] = _big_neg;
    tag_desc[TBIG].arith_op[ARITH_ADD] = _big_add;
    tag_desc[TBIG].arith_op[ARITH_SUB] = _big_sub;
    tag_desc[TBIG].arith_op[ARITH_MUL] = _big_mul;
    tag_desc[TBIG].arith_op[ARITH_DIV] = _big_div;
    tag_desc[TBIG].arith_op[ARITH_IDIV] = _big_idiv;
    tag_desc[TBIG].arith_op[ARITH_MOD] = _big_rem;
    tag_desc[TBIG].arith_op[ARITH_POW] = _big_pow;
    tag_desc[TBIG].arith_op[ARITH_FLOORDIV] = _big_floordiv;
    tag_desc[TBIG].arith_op[ARITH_FLOORREM] = _big_floorrem;
    tag_desc[TBIG].arith_op[ARITH_AND] = _big_and;
    tag_desc[TBIG].arith_op[ARITH_OR] = _big_or;
    tag_desc[TBIG].arith_op[ARITH_COM] = _big_com;
    tag_desc[TBIG].arith_op[ARITH_XOR] = _big_xor;
    tag_desc[TBIG].arith_op[ARITH_SHL] = _big_shl;
    tag_desc[TBIG].arith_op[ARITH_SHR] = _big_shr;
    tag_desc[TBIG].arith_op[ARITH_SGN] = _big_sgn;
    tag_desc[TBIG].arith_op[ARITH_SETBIT] = _big_setbit;
    tag_desc[TBIG].arith_op[ARITH_GETBIT] = _big_getbit;
    tag_desc[TBIG].arith_op[ARITH_CLRBIT] = _big_clrbit;
    tag_desc[TBIG].arith_op[ARITH_NICERAT] = _big_nicerat;
    tag_desc[TBIG].arith_op[ARITH_GCD] = _big_gcd;
    tag_desc[TBIG].arith_op[ARITH_GCD_EXT] = _big_gcd_ext;
    tag_desc[TBIG].arith_op[ARITH_LCM] = _big_lcm;
    tag_desc[TBIG].arith_op[ARITH_POWM] = _big_powm;
    tag_desc[TBIG].arith_op[ARITH_NEXT] = _big_next;
    tag_desc[TBIG].arith_op[ARITH_PREV] = _big_prev;
    tag_desc[TBIG].arith_op[ARITH_ATAN2] = _big_atan2;
    tag_desc[TBIG].arith_op[ARITH_COPYSIGN] = _big_copysign;
    tag_desc[TBIG].arith_op[ARITH_NEXTTOWARD] = _big_nexttoward;

    tag_desc[TRAT].from_string = _rat_from_string;	/* rationals */
    tag_desc[TRAT].write = _write_rat;
    tag_desc[TRAT].compare = _compare_rat;
    tag_desc[TRAT].arith_compare = _arith_compare_rat;
    tag_desc[TRAT].equal = _equal_rat;
    tag_desc[TRAT].copy_size = _copy_size_rat;
    tag_desc[TRAT].copy_to_heap = _copy_heap_rat;
    tag_desc[TRAT].string_size = _rat_string_size;
    tag_desc[TRAT].to_string = _rat_to_string;
    tag_desc[TRAT].coerce_to[TDBL] = _rat_dbl;
    tag_desc[TRAT].arith_op[ARITH_PLUS] = _rat_nop;
    tag_desc[TRAT].arith_op[ARITH_FLOAT] = _rat_nop;	/* handled by coercion */
    tag_desc[TRAT].arith_op[ARITH_NICERAT] = _rat_nop;
    tag_desc[TRAT].coerce_to[TIVL] = _rat_ivl;
    tag_desc[TRAT].arith_op[ARITH_CHGSIGN] =
    tag_desc[TRAT].arith_op[ARITH_NEG] = _rat_neg;
    tag_desc[TRAT].arith_op[ARITH_ABS] = _rat_abs;
    tag_desc[TRAT].arith_op[ARITH_MIN] = _rat_min;
    tag_desc[TRAT].arith_op[ARITH_MAX] = _rat_max;
    tag_desc[TRAT].arith_op[ARITH_ADD] = _rat_add;
    tag_desc[TRAT].arith_op[ARITH_SUB] = _rat_sub;
    tag_desc[TRAT].arith_op[ARITH_MUL] = _rat_mul;
    tag_desc[TRAT].arith_op[ARITH_DIV] = _rat_div;
    tag_desc[TRAT].arith_op[ARITH_POW] = _rat_pow;
    tag_desc[TRAT].arith_op[ARITH_FLOOR] = _rat_floor;
    tag_desc[TRAT].arith_op[ARITH_CEIL] = _rat_ceil;
    tag_desc[TRAT].arith_op[ARITH_TRUNCATE] = _rat_truncate;
    tag_desc[TRAT].arith_op[ARITH_ROUND] = _unimp_err;
    tag_desc[TRAT].arith_op[ARITH_FIX] = _rat_fix;
    tag_desc[TRAT].arith_op[ARITH_INT] = _rat_int2;
    tag_desc[TRAT].arith_op[ARITH_NUM] = _rat_num;
    tag_desc[TRAT].arith_op[ARITH_DEN] = _rat_den;
    tag_desc[TRAT].arith_op[ARITH_SGN] = _rat_sgn;
    tag_desc[TRAT].arith_op[ARITH_ATAN2] = _rat_atan2;
    tag_desc[TRAT].arith_op[ARITH_COPYSIGN] = _rat_copysign;
}

#endif
