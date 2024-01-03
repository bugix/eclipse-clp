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
 * The Initial Developer of the Original Code is  Coninfer Ltd.
 * Portions created by the Initial Developer are
 * Copyright (C) 2020 Coninfer Ltd
 * 
 * Contributor(s):      Joachim Schimpf, Coninfer Ltd
 * 
 * END LICENSE BLOCK */


/*----------------------------------------------------------------------
 * Built-in predicates related to random number generation
 *----------------------------------------------------------------------*/

#include "config.h"

#ifdef STDC_HEADERS
#include <stdlib.h>
#else
# ifdef HAVE_RANDOM
#  if (SIZEOF_LONG == 8)
    extern int	random();
#  else
    extern long	random();
#  endif
# endif
#endif


#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "error.h"
#include "mem.h"
#include "dict.h"
#include "emu_export.h"
#include "os_support.h"


/*
 * Static variables
 */

static dident	d_system,               /* random generator modes */
                d_lcg,
                d_xs,
                d_sm;


/*----------------------------------------------------------------------
 * Several random generators with small state
 *----------------------------------------------------------------------*/

/*
 * SplitMix64, from Sebastiano Vigna's web site http://vigna.di.unimi.it/
 * 64 bit state, period 2^64
 * The big advantage of this is that it has a 64bit state that can
 * be seeded to anything, including zero.  It can be used directly,
 * or to compute seeds for another generator.
 */

static uint64_t
_splitmix64(uint64_t *pstate) {
    uint64_t z = (*pstate += 0x9e3779b97f4a7c15);
    z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9;
    z = (z ^ (z >> 27)) * 0x94d049bb133111eb;
    return z ^ (z >> 31);
}

static inline int32_t
_sm_irandom(uint64_t *pstate)
{
    return _splitmix64(pstate) >> 33;    /* retain 31 upper bits only */
}

static inline double
_sm_frandom(uint64_t *pstate)
{
    uint64_t r;
    do r = (_splitmix64(pstate) >> 11);  /* get 53 random bits, suppress zero */
    while(!r);
    return ldexp((double) r, -53);      /* divide by 2^53 */
}

static void
_sm_seed(uint64_t *pstate, uword n)
{
   *pstate = (uint64_t) n;
}


/*
 * xorshift64* by George Marsaglia
 * https://en.wikipedia.org/wiki/Xorshift
 * 64 bit state, period 2^64-1
 * Seeded using splitmix64().
 */

static inline uint64_t
_xorshift64s(uint64_t *state)
{
    uint64_t x = *state;  /* The state must be seeded with a nonzero value. */
    x ^= x >> 12; // a
    x ^= x << 25; // b
    x ^= x >> 27; // c
    *state = x;
    return x * 0x2545F4914F6CDD1Dul;
}

static inline int32_t
_xs_irandom(uint64_t *pstate)
{
    return _xorshift64s(pstate) >> 33;   /* retain upper 31 bits only  */
}

static inline double
_xs_frandom(uint64_t *pstate)
{
    uint64_t r;
    do r = (_xorshift64s(pstate) >> 11); /* get 53 random bits, suppress zero */
    while(!r);
    return ldexp((double) r, -53);      /* divide by 2^53 */
}

static void
_xs_seed(uint64_t *pstate, uword n)
{
   uint64_t aux_state = n;
   while(!(*pstate = _splitmix64(&aux_state)))
        /* zero, try again */ ;
}


/*
 * Compatibility: we have used this one for frandom/1 since the 1990s.
 *
 * Random generator taken from random2.c
 * by John Burton, available from the net. Part of original comment:
 *
 * PMMMLCG - Prime Modulus M Multiplicative Linear Congruential Generator   *
 *  Modified version of the Random number generator proposed by             *
 *  Park & Miller in "Random Number Generators: Good Ones Are Hard to Find" *
 *  CACM October 1988, Vol 31, No. 10                                       *
 *   - Modifications proposed by Park to provide better statistical         *
 *     properties (i.e. more "random" - less correlation between sets of    *
 *     generated numbers                                                    *
 *   - generator is of the form                                             *
 *         x = ( x * A) % M                                                 *
 *   - Choice of A & M can radically modify the properties of the generator *
 *     the current values were chosen after followup work to the original   *
 *     paper mentioned above.                                               *
 *   - The generator has a period of 2^31 - 1 with numbers generated in the *
 *     range of 0 < x < M                                                   *
 *   - The generator can run on any machine with a 32-bit integer, without  *
 *     overflow.                                                            *
 */

#define RND_A	48271
#define RND_M	2147483647
#define RND_Q	(RND_M / RND_A)
#define RND_R	(RND_M % RND_A)

static void
_pmmmlcg_seed(uint64_t *prand_state, uword n)
{
    int32_t *pstate = (int32_t*) prand_state;
    int32_t seed0 = ((uint32_t) n) % RND_M;
    *pstate = (seed0==0) ? 1 : seed0;	/* seed must be in range 1..2147483646 */
}

static inline int32_t
_pmmmlcg(int32_t *pstate)
{
    int32_t state = *pstate;
    int32_t lo,hi,test;

    hi = state / RND_Q;
    lo = state % RND_Q;
    test = RND_A * lo - RND_R * hi;
    *pstate = state = (test > 0) ? (test) : (test + RND_M);
    return state;
}

static inline int32_t
_pmmmlcg_irandom(uint64_t *prand_state)
{
    return _pmmmlcg((int32_t*)prand_state);
}

static inline double
_pmmmlcg_frandom(uint64_t *prand_state)
{
    static double temp = 1.0 / (double)RND_M;
    return (double)_pmmmlcg((int32_t*)prand_state) * temp;
}


/*
 * Use a system-supplied generator, i.e. either random() or rand().
 * This is process-wide, not per engine!
 */

static void
_rnd_seed(uint64_t *prand_state, uword n)
{
#ifdef HAVE_RANDOM
    srandom((unsigned) n);
#else
    srand((unsigned) n);
#endif
}

static inline int32_t
_rnd_irandom(uint64_t *prand_state)
{
    int32_t n;
#ifdef HAVE_RANDOM
    /* use long random(), which should return 0..2147483647 */
    n = random();
#else
    /* use int rand(), which should return at least 0..32767 */
    n = (rand() << 15) ^ rand();	/* make at least 30 bits */
    n &= 0x7fffffff;                    /* make sure it's max 31 bits */
#endif
    return n;
}

static inline double
_rnd_frandom(uint64_t *prand_state)
{
    long n;
#ifdef HAVE_RANDOM
    /* use long random(), which should return 0..2147483647 */
    n = random() & 0x7fffffff;          /* make sure it's 31 bits */
    return ldexp((double) n, -31);      /* divide by 2^31 */
#else
    /* use int rand(), which should return at least 0..32767 */
    n = (rand() << 15) ^ rand();	/* make at least 30 bits */
    n &= 0x3fffffff;                    /* make sure it's only 30 bits */
    return ldexp((double) n, -30);      /* divide by 2^30 */
#endif
}


/*----------------------------------------------------------------------
 * The built-in predicates and C API
 * These predicates all depend on the ENG_RND_MODE setting in VM_FLAGS
 *----------------------------------------------------------------------*/

/*
 * This is called on new engine creation, when not inheriting the state.
 */
void
ec_random_init(ec_eng_t *ec_eng)
{
    uword rand_init = ec_unix_time() * getpid();
    switch (VM_FLAGS & ENG_RND_MASK)
    {
    case ENG_RND_SYS: break;    /* global state, not touched here */
    case ENG_RND_LCG: _pmmmlcg_seed(&ec_eng->frand_state, rand_init); break;
    case ENG_RND_XS:  _xs_seed(&ec_eng->frand_state, rand_init); break;
    case ENG_RND_SM:  _sm_seed(&ec_eng->frand_state, rand_init); break;
    }
}


/*
 * frandom(-RandomFloat)
 * Return normal float 0<RandomFloat<1
 */
static int
p_frandom(value v, type t, ec_eng_t *ec_eng)
{
    double f;
    switch (VM_FLAGS & ENG_RND_MASK)
    {
    case ENG_RND_SYS: f = _rnd_frandom(&ec_eng->frand_state); break;
    case ENG_RND_LCG: f = _pmmmlcg_frandom(&ec_eng->frand_state); break;
    case ENG_RND_XS:  f = _xs_frandom(&ec_eng->frand_state); break;
    case ENG_RND_SM:  f = _sm_frandom(&ec_eng->frand_state); break;
    }
    Return_Unify_Float(v, t, f); /* may expand f several times! */
}


/*
 * random(-Random)
 * Binds its argument to a random 31-bit or 30-bit integer.
 */
static int
p_random(value v, type t, ec_eng_t *ec_eng)
{
    int32_t n;
    switch (VM_FLAGS & ENG_RND_MASK)
    {
    case ENG_RND_SYS: n = _rnd_irandom(&ec_eng->frand_state); break;
    case ENG_RND_LCG: n = _pmmmlcg_irandom(&ec_eng->frand_state); break;
    case ENG_RND_XS:  n = _xs_irandom(&ec_eng->frand_state); break;
    case ENG_RND_SM:  n = _sm_irandom(&ec_eng->frand_state); break;
    }
    Return_Unify_Integer(v,t,n); /* may expand n several times! */
}


/*
 * random(+Limit, -Random)
 * Return a random integer between 0 and Limit-1.
 */
static int
p_random2(value vmax, type tmax, value v, type t, ec_eng_t *ec_eng)
{
    int32_t n;
    Check_Integer(tmax);
    if (vmax.nint <= 0  ||  INT32_MAX < vmax.nint-1) {
        Bip_Error(RANGE_ERROR)
    }
    switch (VM_FLAGS & ENG_RND_MASK)
    {
    case ENG_RND_SYS: n = _rnd_irandom(&ec_eng->frand_state); break;
    case ENG_RND_LCG: n = _pmmmlcg_irandom(&ec_eng->frand_state); break;
    case ENG_RND_XS:  n = _xs_irandom(&ec_eng->frand_state); break;
    case ENG_RND_SM:  n = _sm_irandom(&ec_eng->frand_state); break;
    }
    n = n % vmax.nint;
    Return_Unify_Integer(v,t,n); /* may expand n several times! */
}


/*
 * seed(+Seed)
 * The argument must be an int.
 * Seeds the RNG currently selected for the calling engine.
 */
static int
p_seed(value v, type t, ec_eng_t *ec_eng)
{
    Check_Integer(t);
    switch (VM_FLAGS & ENG_RND_MASK)
    {
    case ENG_RND_SYS: _rnd_seed(&ec_eng->frand_state, (uword)v.nint); break;
    case ENG_RND_LCG: _pmmmlcg_seed(&ec_eng->frand_state, (uword)v.nint); break;
    case ENG_RND_XS:  _xs_seed(&ec_eng->frand_state, (uword)v.nint); break;
    case ENG_RND_SM:  _sm_seed(&ec_eng->frand_state, (uword)v.nint); break;
    }
    Succeed_;
}


/*
 * random_mode(?Mode)
 * Internal predicate (used by set_flag/2 and /get_flag/2)
 * to switch between the supported random generators.
 */
static int
p_random_mode(value v, type t, ec_eng_t *ec_eng)
{
#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

    if (IsRef(t)) {
        dident d;
        switch (VM_FLAGS & ENG_RND_MASK)
        {
        case ENG_RND_SYS: d = d_system; break;
        case ENG_RND_LCG: d = d_lcg; break;
        case ENG_RND_XS:  d = d_xs; break;
        case ENG_RND_SM:  d = d_sm; break;
        }
        Return_Unify_Atom(v, t, d);

    } else {
        Check_Atom(t);
        if (v.did == d_system) {
            VM_FLAGS = (VM_FLAGS & ~ENG_RND_MASK) | ENG_RND_SYS;
        } else if (v.did == d_lcg) {
            VM_FLAGS = (VM_FLAGS & ~ENG_RND_MASK) | ENG_RND_LCG;
        } else if (v.did == d_xs) {
            VM_FLAGS = (VM_FLAGS & ~ENG_RND_MASK) | ENG_RND_XS;
        } else if (v.did == d_sm) {
            VM_FLAGS = (VM_FLAGS & ~ENG_RND_MASK) | ENG_RND_SM;
        } else {
            Bip_Error(RANGE_ERROR);
        }
        ec_random_init(ec_eng);
        Succeed_;
    }
#undef Bip_Error
#define Bip_Error(N) Bip_Error_Return(N)
}


/*----------------------------------------------------------------------
 * Initialisation
 *----------------------------------------------------------------------*/

void
bip_random_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("random",1), 	p_random, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("random",2), 	p_random2, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("frandom",1), 	p_frandom, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("seed",1),	p_seed,	B_SAFE);
	(void) built_in(in_dict("random_mode",1), p_random_mode, B_UNSAFE|U_SIMPLE);
    }
    if (flags & INIT_PRIVATE)
    {
        d_system = in_dict("system", 0);
        d_lcg = in_dict("lcg", 0);
        d_xs = in_dict("xs", 0);
        d_sm = in_dict("sm", 0);
    }
    if (flags & INIT_PROCESS)
    {
	/* initialize process-wide random generator */
        _rnd_seed(NULL, (uword) ec_unix_time() * getpid());
    }
}

/* Add all new code in front of the initialization function! */