/*
 * Inline code for activating an XPRESS-MP OEM licence.
 *
 * The following C variables are defined at this point:
 *   int n;		has the value of LicNum coming from eplex_lic_info.ecl
 *   int err;		for returning an error code
 *   char slicmsg[256]; for returning a licensing message
 */

#if 0	/* change to 1 if using OEM licensing code */

    XPRSbeginlicensing(NULL);
    err = XPRSlicense(&n, slicmsg);
    if (err != 8)
    {
	/*** INSERT YOUR MAGIC FORMULA HERE ***/
	n = ...magic(n)...;

	err = XPRSlicense(&n, slicmsg);	/* second call */
    }
    err = XPRSinit(licloc);
    XPRSendlicensing();

#else	/* normal initialization (using a licence server) */

    err = XPRSinit(licloc);

#endif

