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
 * VERSION	$Id: io.c,v 1.28 2017/09/01 16:02:22 jschimpf Exp $
 */

/*
 * IDENTIFICATION               io.c
 *
 * DESCRIPTION			Primitives for the I/O.
 *
 * CONTENTS:
 *
 * AUTHOR       VERSION  DATE   REASON
 * Pierre Dufresne
 * Micha Meier			Complete change - output buffering, handling
 *				the terminal, prompts, system streams,
 *				portability, the 'user' stream, raw mode,
 *				interrupt fixes
 *
 * ----------------------------------------------------------------------
 *
 * General:
 *
 *   I/O Buffers:
 *    -	Apart from null stream, all streams have a buffer
 *    -	string and queue streams have lists of buffers and switch between them
 *    -	buffers have LOOKAHEAD extra bytes in front (for unget/1 and lexer)
 *    -	buffers have 1 extra byte after (for EOB_MARK, used in lexer)
 *
 *   StreamBuf(nst)	points to a buffer
 *   StreamPtr(nst)	points into that buffer (read[/write] position)
 *   StreamSize(nst)	is that buffer's size
 *   StreamCnt(nst)	is the number of used bytes in that buffer
 *
 *   StreamWBuf(nst)	is used for queue streams only because they need to
 *			maintain separate read and write positions.
 *   StreamOffset 	offset of the current buffer from beginning of stream
 *			(except for queue streams)
 *
 * ----------------------------------------------------------------------
 *
 * File/Tty streams:
 *
 *   They have a single buffer, used as follows:
 *
 *   Read streams (or RDWR, not MWRITE):
 *
 *	 already_read not_yet_read dont_care
 *	 +------------+------------+---------+
 *	buf          ptr        buf+cnt   buf+size
 *
 *
 *
 *   Write streams (or RDWR, not MREAD):
 *
 *	 not_yet_flushed dont_care
 *	 +---------------+-------------------+
 *	buf             ptr               buf+size
 *
 *   After seeking back:
 *
 *	 not_yet_flushed not_flushed
 *	 +---------------+-----------+-------+
 *	buf             ptr       buf+cnt buf+size
 *
 *
 *
 *   Updated stream (RDWR, MREAD, MWRITE):
 *
 *	 not_yet_flushed not_flushed
 *	 +---------------+-----------+-------+
 *	buf             ptr       buf+cnt buf+size
 *
 *	Ptr is used both for reading and writing.
 *	When writing, ptr may go beyond buf+cnt.
 *
 *
 *  - MWRITE means the buffer is dirty (was written into and not yet flushed)
 *    MWRITE implies SWRITE or SRDWR.
 *  - Offset is the offset of the beginning of the buffer from the
 *    beginning of the file.
 *  - MREAD means the buffer was read. This is only relevant for RDWR streams
 *    and basically means that the fd is not positioned at offset.
 *    MREAD implies SREAD or SRDWR.
 *  - MEOF means that end_of_file was already read once, and the next attempt
 *    should raise an error.
 *
 * ----------------------------------------------------------------------
 *
 * String streams:
 *
 *   String streams have a doubly linked list of buffers, all the same size,
 *   which together hold the contents of the string stream. read/write/seek
 *   can switch back and forth through this buffer list.
 *   One of the buffers is always active (pointed to by StreamBuf) and used
 *   similar to the single buffer in the case of true files.
 *
 *	 xxxxxxxxxxxxxxxxxxxxxxxx dont_care
 *	 +---------------+--------+----------+
 *	buf             ptr    buf+cnt    buf+size
 *
 *	Ptr is used both for reading and writing.
 *	Buf+cnt indicates the end of the used buffer and is always >= ptr.
 *	If the buffer is the last in the chain, Buf+cnt is the end of "file".
 *
 *  MREAD	always set, indicating the buffer content is valid
 *  MWRITE	unused
 *  StreamBuf	current buffer
 *  StreamPtr	points into current buffer (read/write position)
 *  StreamCnt	every buffer in the list has its own cnt field, which is
 *		copied to StreamCnt when the buffer is/becomes current.
 *  StreamWBuf	unused
 *
 * ----------------------------------------------------------------------
 *
 * Queue streams:
 *
 *   Queue streams have a doubly linked circular list of buffers, all the
 *   same size. Read and write position are separate. The read buffer is
 *   similar to the single buffer in the case of true files:
 *
 *	 dont_care    not_yet_read dont_care
 *	 +------------+------------+---------+
 *	buf          ptr        buf+cnt   buf+size
 *
 *	Ptr is used for reading only, writing is done at buf+cnt.
 *
 *   The write buffer is pointed to by StreamWBuf. The write position in
 *   this buffer is StreamWBuf + BufHeader(StreamWBuf)->cnt.
 *   StreamBuf and StreamWBuf are either identical (in which case the read
 *   position is always below the write position), or StreamWBuf is just
 *   before StreamBuf in the circular list. The write pointer is not allowed
 *   to enter the read buffer from the left, instead, a fresh buffer gets
 *   inserted between StreamWBuf and StreamBuf when needed during writing.
 *   Buffers that become empty during reading are eagerly unlinked and freed.
 *
 *  MREAD	always set, indicating the buffer content is valid
 *  MWRITE	queue was written into, but not yet flushed
 *  StreamBuf	current (read) buffer
 *  StreamCnt	every buffer in the list has its own cnt field, which is copied
 *		to StreamCnt when the buffer is/becomes current (read) buffer.
 *  StreamWBuf	current write buffer
 *  StreamOffset is the sum of counts of full buffers between read and
 *		write buffer (to speed up size computation).
 *
 * ----------------------------------------------------------------------
 */


/*
 * INCLUDES:
 */
#include "config.h"
#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "mem.h"
#include "error.h"
#include "dict.h"
#include "lex.h"
#include "ec_io.h"
#include "emu_export.h"
#include "property.h"
#include "module.h"
#include "os_support.h"

#include <stdio.h>	/* for sprintf, readline */
#include <errno.h>	/* for EMFILE */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#else
#ifndef _WIN32
extern long		lseek();
#endif
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef _WIN32

#define Termio	int

#else

#if defined(HAVE_TCGETATTR)
#define TERMIO_POSIX_STYLE
#include <termios.h>
#define Termio		struct termios
#endif

#if defined(TERMIO_SYS_V_STYLE)
#include <termio.h>
#define Termio		struct termio
#define GetTermAttr	TCGETA
#define SetTermAttr	TCSETA
#else
# if defined(HAVE_PUSHBACK)
# include <termio.h>
# endif
#endif

#if !defined(Termio)
#define TERMIO_BSD_STYLE
#include <sgtty.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#define Termio		struct sgttyb
#define GetTermAttr	TIOCGETP
#define SetTermAttr	TIOCSETN
#endif

#if defined(SIGIO_SETSIG)
#include <stropts.h>
#endif

#if defined(SIGIO_FIOASYNC)
#include <sys/ioctl.h>
#endif

#endif


/*
 * DEFINES:
 */

#define STREAM_MIN	32
#define STREAM_INC	16


/*
 * Set the stream number as a property of the atom's did. The counter for
 * the previous stream, if any, is decremented, and the new one is incremented.
 */
#define Set_New_Stream(did, nst) {				\
	int res;						\
	pword old, new;						\
	new.tag.kernel = TPTR;					\
	new.val.wptr = (uword*) stream_tid.copy(nst);		\
	res = swap_global_property(did, STREAM_PROP, &new, &old);	\
	assert(res >= 0);					\
	if (!(res & NEW_PROP)) /* free old stream, if any */	\
	    stream_tid.free((stream_id) old.val.wptr);		\
    }

#define Check_Stream_Owner(nst)	\
	if (StreamUnit(nst) != NO_UNIT && nst->fd_pid && nst->fd_pid != own_pid) \
	{ Bip_Error(PERROR); }

#define LocalStreams() (StreamDescriptors == stream_ids_)


/*
 * Simple linear congruential random sequence generator used for scrambling
 * streams. The constants are taken from B.Schneier, Applied Cryptography
 */
#define NextRand(r)	(((r)*4096 + 150889) % 714025)


/*
 * Macros for linked buffer chains (string streams and queues)
 * The LOOKAHEAD bytes are being updated only when going forward one buffer,
 * because only then the previous buffer may just have changed.
 */

#define StreamBufHeader(nst)	((linked_io_buffer_t*) StreamBuf(nst) - 1)
#define StreamWBufHeader(nst)	((linked_io_buffer_t*) StreamWBuf(nst) - 1)
#define BufHeader(buf)		((linked_io_buffer_t*) (buf) - 1)

#define Advance_Buffer(nst) \
	StreamBuf(nst) = StreamBufHeader(nst)->next; \
	Copy_Bytes(StreamBuf(nst)-LOOKAHEAD, StreamPtr(nst)-LOOKAHEAD, LOOKAHEAD);  \
	StreamCnt(nst) = StreamBufHeader(nst)->cnt; \
	StreamPtr(nst) = StreamBuf(nst);

#define Retreat_Buffer(nst) \
	StreamBuf(nst) = StreamBufHeader(nst)->prev; \
	StreamCnt(nst) = StreamBufHeader(nst)->cnt; \
	StreamPtr(nst) = StreamBuf(nst);

#define New_Buffer(nst, buf) { \
	linked_io_buffer_t *buf_header = (linked_io_buffer_t *) hg_alloc(sizeof(linked_io_buffer_t) + StreamSize(nst) + 1); \
	buf_header->next = buf_header->prev = 0; \
	buf_header->cnt = 0; \
	buf = (unsigned char *) (buf_header + 1); \
	buf[0] = EOB_MARK; \
}

#define Append_New_Buffer(nst) { \
	unsigned char *buf; \
	New_Buffer(nst, buf); \
	BufHeader(buf)->prev = StreamBuf(nst); \
	StreamBufHeader(nst)->next = buf; \
}

#define Free_Prev_Buffer(nst) { \
	unsigned char *empty_buf = BufHeader(StreamBuf(nst))->prev;  \
	BufHeader(BufHeader(empty_buf)->prev)->next = StreamBuf(nst);  \
	BufHeader(StreamBuf(nst))->prev = BufHeader(empty_buf)->prev;  \
	hg_free(BufHeader(empty_buf));  \
}


/*
 * EXTERNAL VARIABLE DEFINITIONS:
 */

/*
 * Stream  descriptors
 */
stream_id	stream_ids_[STREAM_MIN];
stream_desc	stream_desc_structs_[STREAM_MIN];


/* SICStus-like read hook */
int (*E_read_hook)() = NULL;

/*
 * TYPES
 */

typedef struct linked_io_buffer {
    unsigned char *		prev;
    unsigned char *		next;
    uword			cnt;
    /* char			_lookahead[LOOKAHEAD];  but aligned: */
    word			_lookahead[(LOOKAHEAD-1)/sizeof(word) + 1];
} linked_io_buffer_t;


io_channel_t
	ec_file, ec_tty, ec_pipe, ec_socket, ec_queue_stream,
	ec_string_stream, ec_null_stream;


/*
 * FUNCTION DEFINITIONS:
 */

static void	_free_stream(stream_id nst),
		_init_fd_stream(stream_id nst, int unit, int mode, dident name, dident prompt, stream_id prompt_stream, int size);

#if defined(HAVE_READLINE)
static void	_resize_stream_buffer(stream_id nst, long newsize);
#endif

static int	_isafifo(int fd),
		_local_io_close(stream_id nst),
		_local_io_flush_out(stream_id nst),
		_local_fill_buffer(stream_id nst),
		_queue_fill_buffer(stream_id nst),
		_string_fill_buffer(stream_id nst),
		_local_tty_in(stream_id nst);



/*
 * FUNCTION NAME:	io_init()
 *
 * PARAMETERS:		NONE
 *
 * DESCRIPTION:		Initializes the system streams.
 *
 * This depends on the setting of ec_options.io_option:
 *
 * SHARED_IO	stream descriptors are in shared memory and actual I/O 
 *		 is done by the owner process of the file descriptor (default).
 * OWN_IO	have streams in private memory so that every worker
 *		 can have its private streams. This is mainly for
 *		 debugging the parallel system.
 * MEMORY_IO	Do not connect to stdin/stdout/stderr, but open all standard
 *		 Eclipse streams as in-memory queues. This is for embedding.
 */

int
io_init(int flags)
{
    int		i;

    if (flags & INIT_SHARED)
    {
	/* 
	 * Make a shared array of stream descriptors
	 */
	StreamDescriptors = (stream_desc **)
		hg_alloc_size(STREAM_MIN * sizeof(stream_desc *));
	for (i = 0; i < STREAM_MIN; i++)
	{
	    StreamId(i) = (stream_desc *) hg_alloc_size(sizeof(stream_desc));
	    StreamMode(StreamId(i)) = SCLOSED;
	    StreamNref(StreamId(i)) = 0;
	    StreamNr(StreamId(i)) = i;
	}
	NbStreams = STREAM_MIN;
	NbStreamsFree = STREAM_MIN - 4;
	stream_tid.copy(StreamId(0));
	stream_tid.copy(StreamId(1));
	stream_tid.copy(StreamId(2));
	stream_tid.copy(StreamId(3));
    }
    if (flags & INIT_PRIVATE)
    {
	/* 
	 * Initialize some private data
	 */
	current_input_ = user_input_ = StreamId(0);
	current_output_ = warning_output_ = log_output_ = user_output_ = StreamId(1);
	current_err_ = user_err_ = StreamId(2);
	null_ = StreamId(3);
    }
    if (flags & INIT_SHARED)
    {
	Set_New_Stream(d_.stdin0, StreamId(0));
	Set_New_Stream(d_.stdout0, StreamId(1));
	Set_New_Stream(d_.stderr0, StreamId(2));
	Set_New_Stream(d_.input, current_input_);
	Set_New_Stream(d_.output, current_output_);
	Set_New_Stream(d_.err, current_err_);
	Set_New_Stream(d_.warning_output, warning_output_);
	Set_New_Stream(d_.log_output, log_output_);
	Set_New_Stream(d_.null, null_);
	Set_New_Stream(d_.user_input, user_input_);
	Set_New_Stream(d_.user_output, user_output_);
	Set_New_Stream(d_.user_error, user_err_);
    }

    /*
     * Create the standard Eclipse I/O streams
     */
    if (flags & INIT_SHARED)
    {
	/* 
	 * Make the system streams (init or reinit)
	 */
	init_stream(null_,  NO_UNIT, SRDWR | SNULL | SDONTCLOSE, d_.null,
	    NO_STREAM, 0);

	if (ec_options.io_option == MEMORY_IO)
	{
	    init_stream(current_input_, NO_UNIT, SREAD|SQUEUE|SDONTCLOSE|SYIELD,
	    	D_UNKNOWN, NO_STREAM, 0);
	    init_stream(current_output_, NO_UNIT, SWRITE|SQUEUE|SDONTCLOSE|SFLUSHEOL|SYIELD,
		D_UNKNOWN, NO_STREAM, 0);
	    init_stream(current_err_, NO_UNIT, SWRITE|SQUEUE|SDONTCLOSE|SFLUSHEOL|SYIELD,
		D_UNKNOWN, NO_STREAM, 0);

	}
	else	/* connect to fd 0,1,2 */
	{
	    _init_fd_stream(current_input_,  0, SREAD|SDONTCLOSE,  d_.user,
	    	in_dict(" ",0), current_output_, 0);
	    _init_fd_stream(current_output_, 1, SWRITE|SDONTCLOSE, d_.user,
	    	NO_PROMPT, NO_STREAM, 0);
	    _init_fd_stream(current_err_,    2, SWRITE|SDONTCLOSE, d_.err,
	    	NO_PROMPT, NO_STREAM, 0);
	}
    }
    /* else we are attaching to shared memory: do nothing */

    return PSUCCEED;
}


/*
 * FUNCTION NAME:	init_stream
 *
 * PARAMETERS:	
 * 			nst -	Sepia stream number
 *			unit -	OS file descriptor, NO_UNIT if a special one
 *			mode -	Sepia i/o mode
 *			name -	if a file, then its name, otherwise an acronym
 *			paired_stream - the paired read-stream for sockets
 *			size -	the size of the buffer or 0 if default
 *				If buf != 0 then size must be its size
 *
 * DESCRIPTION:		Initialize a new stream. Fill the data into
 *			the stream structure, allocate buffer(s)
 *			if necessary.
 *			The stream is not shared, no locking necessary.
 */
void
init_stream(stream_id nst, int unit, int mode, dident name, stream_id paired_stream, int size)
{
    unsigned char *buf;

    assert(!IsOpened(nst) || (mode & SSLAVE)); 
    assert(paired_stream == NO_STREAM || (mode&STYPE) == SSOCKET);

    StreamUnit(nst) = unit;
    if (LocalStreams())
    {
	nst->fd_pid = 0;
    }
    else
    {
	/* stderr and stdout is handled in every process locally */
	/* stdin and others are done on the fd owner */
	nst->fd_pid = (unit == 1 || unit == 2) ? 0 : own_pid;
    }
    my_io_aport(&nst->aport);
    if (!(mode & SSLAVE))
    {
	mt_mutex_init_recursive(&nst->lock);
    }
    if (paired_stream)
    {
	StreamPairedStream(nst) = paired_stream;
	/* link the slave back to the master */
	assert(!(mode & SSLAVE) && (StreamMode(paired_stream) & SSLAVE));
	StreamPairedStream(paired_stream) = nst;
    } else {
	StreamPairedStream(nst) = nst;	/* self */
    }
    StreamLine(nst) = 1;
    StreamEncoding(nst) = SENC_DEFAULT;
    StreamPromptStream(nst) = NO_STREAM;
    StreamPrompt(nst) = NO_PROMPT;
    StreamName(nst) = name;
    StreamPath(nst) = D_UNKNOWN;
    StreamCnt(nst) = 0;
    StreamOffset(nst) = 0;
    Make_Nil(&StreamEvent(nst));
    StreamEventEngine(nst) = NULL;
    StreamRand(nst) = 0;
    StreamLastWritten(nst) = -1;
    StreamOutputMode(nst) = DEFAULT_OUTPUT_MODE;
    StreamPrintDepth(nst) = 0;
    nst->signal_thread = 0;
    nst->cond = 0;
    switch (mode & STYPE)
    {
	case SSTRING:	nst->methods = &ec_string_stream;	break;
	case SPIPE:	nst->methods = &ec_pipe;		break;
	case SQUEUE:	nst->methods = &ec_queue_stream;	break;
	case SNULL:	nst->methods = &ec_null_stream;		break;
	case STTY:	nst->methods = &ec_tty;			break;
	case SSOCKET:	nst->methods = &ec_socket;		break;
	default:	nst->methods = &ec_file;		break;
    }
    StreamMode(nst) = mode|StreamMethods(nst).mode_defaults;

    /* Don't initialise StreamNref(nst) because it is a property of
     * the descriptor, not of the stream proper. When the descriptor
     * has just been obtained via find_free_stream(), it is initialised anyway.
     * Otherwise the stream is only re-initialised, e.g. after restore. */

    if (size == 0)
    {
	if (unit == NO_UNIT)
	{
	    size = StreamMethods(nst).buf_size_hint;
	}
	else
	{
#if defined(HAVE_ST_BLKSIZE)
	    struct stat st;

	    if(fstat(unit, &st) < 0 || st.st_blksize == 0)
		/* if size not available, take default */
		size = StreamMethods(nst).buf_size_hint;
	    else
		size = st.st_blksize;	/* else take the given size */
#else
	    size = StreamMethods(nst).buf_size_hint;
#endif
	}
    }

    StreamSize(nst) = size;
    if (size == 0)
    {
	buf = 0;			/* no buffer, e.g. null stream */
    }
    else
    {
	/* allocate and initialise the I/O buffer (or buffer list) */
	New_Buffer(nst, buf);
	/* for queues, make a cyclic buffer list */
	if (IsQueueStream(nst))
	    BufHeader(buf)->next = BufHeader(buf)->prev = buf;
    }
    StreamBuf(nst) = StreamWBuf(nst) = buf;
    if(IsReadStream(nst))
    {
	StreamLexAux(nst) = (unsigned char *) hg_alloc(BUFSIZE);
	StreamLexSize(nst) = BUFSIZE;
    }
    else
    {
	StreamLexAux(nst) = NO_BUF;
	StreamLexSize(nst) = 0;
    }
    StreamPtr(nst) = buf;
}


/* same as above, but the stream type is derived from the fd (unit) */

static void
_init_fd_stream(stream_id nst, int unit, int mode, dident name, dident prompt, stream_id prompt_stream, int size)
{
    if (isatty(unit)) {
    	mode |= STTY;
    } else {
	if (errno == EBADF) {
	    mode |= SNULL;
	    unit = NO_UNIT;
	} else if (_isafifo(unit)) {
	    mode |= SPIPE;
	} else {
	    mode |= SFILE;
	}
	prompt = NO_PROMPT;
	prompt_stream = NO_STREAM;
	size = 0;
    }
    init_stream(nst, unit, mode, name, NO_STREAM, size);
    StreamPrompt(nst) = prompt;
    StreamPromptStream(nst) = prompt_stream;
}


/*
 * Find and return an unused stream descriptor, create if necessary.
 * This is the only way to find free stream descriptors.
 * The returned stream has a ref count of 2 and is unshared
 * [ref cont 1 is reserved, and only used when closing in _lose_stream()].
 * Modifications of the global array, and the increment of the
 * reference counter from 0 are protected by SharedDataLock.
 * After calling this, invoke Trigger_Gc_If_Out_Of_Streams
 * to free up stream handles if necessary.
 */

stream_id
find_free_stream(void)
{
    int		i;
    stream_id	nst;

    mt_mutex_lock(&SharedDataLock);	/* for global stream array */
    for(i = 0; i < NbStreams; i++)
    {
	nst = StreamId(i);
	if(StreamNref(nst) <= 0)
	{
	    assert(StreamNref(nst) == 0);
	    ec_atomic_add(&NbStreamsFree, -1);
	    StreamNref(nst) = 2;
	    mt_mutex_unlock(&SharedDataLock);
	    return(nst);
	}
    }

    if (LocalStreams())
    {
	mt_mutex_unlock(&SharedDataLock);
	return 0;		/* can't extend the static array */
    }

    StreamDescriptors = (stream_desc **) hg_realloc_size(
		StreamDescriptors,
		NbStreams * sizeof(stream_desc *),
		(NbStreams + STREAM_INC) * sizeof(stream_desc *));
    for (; i < NbStreams + STREAM_INC; i++)
    {
	StreamId(i) = (stream_desc *)  hg_alloc_size(sizeof(stream_desc));
	StreamMode(StreamId(i)) = SCLOSED;
	StreamNref(StreamId(i)) = 0;
	StreamNr(StreamId(i)) = i;
    }

    nst = StreamId(NbStreams);
    NbStreams += STREAM_INC;
    ec_atomic_add(&NbStreamsFree, STREAM_INC-1);
    StreamNref(nst) = 2;
    mt_mutex_unlock(&SharedDataLock);
    return nst;
}

/*
 * A slave is a stream descriptor that is paired with a master stream
 * descriptor, and can only be accessed via StreamPairedStream().  It
 * is not in the global stream array, and some fields are invalid, e.g.
 * - stream number
 * - reference count
 * - mutex
 * It is recognizable from the SSLAVE flag.
 */
stream_id
alloc_slave_stream(void)
{
    stream_id nst = (stream_desc *) hg_alloc_size(sizeof(stream_desc));
    StreamMode(nst) = SCLOSED|SSLAVE;
    StreamNref(nst) = 0;
    StreamNr(nst) = -1;
    return nst;
}


/*
 * Support function for dictionary garbage collector
 * Mark the DIDs in the stream descriptors
 */

void
mark_dids_from_streams(void)
{
    int i;
    for(i = 0; i < NbStreams; i++)
	stream_tid.mark_dids(StreamId(i));
}


stream_id
ec_open_file(char *name, int mode, int *err)
{
    int		i = 0;
    int		fd;
    int		smode;
    char	buf[MAX_PATH_LEN];
    stream_id	nst;
    io_channel_t *io_type;

    /* translate the user mode to the corresponding system mode smode */
    switch (mode)
    {
    case SREAD:			smode = O_RDONLY;			break;
    case SWRITE:		smode = O_WRONLY | O_CREAT | O_TRUNC;	break;
    case SRDWR:			smode = O_RDWR | O_CREAT;		break;
    case SAPPEND:
    case (SAPPEND|SWRITE):	smode = O_APPEND | O_CREAT | O_WRONLY;	break;
    case (SAPPEND|SRDWR):	smode = O_APPEND | O_CREAT | O_RDWR;	break;
    default:			*err = RANGE_ERROR; return NO_STREAM;
    }

    /* try to open the file (don't use absolute path if possible) */
    (void) expand_filename(name, buf, EXPAND_STANDARD);
    if ((fd = ec_open(buf, smode, 0666)) < 0) {
	*err = SYS_ERROR_ERRNO;
	return NO_STREAM;
    }
    io_type = isatty(fd) ? &ec_tty : &ec_file;

    /* make the stream descriptor */
    nst = find_free_stream();
    init_stream(nst, fd, mode|io_type->io_type, enter_dict(name, 0), NO_STREAM, 0);
    (void) expand_filename(name, buf, EXPAND_ABSOLUTE);
    StreamPath(nst) = enter_dict(buf, 0);
    return(nst);
}


/*
 * Auxiliary function for ec_close_stream()
 * Called with stream locked.
 */
static void
_free_stream(stream_id nst)
{
    if (!IsOpened(nst))
        return;

    if (StreamBuf(nst) != NO_BUF)
    {
	if (IsQueueStream(nst))
	{
	    /* free a circular list of buffers */
	    unsigned char *first = StreamBuf(nst);
	    unsigned char *next = StreamBuf(nst);
	    do
	    {
		linked_io_buffer_t *this = BufHeader(next);
		next = this->next;
		hg_free(this);
	    } while (next != first);
	}
	else
	{
	    /* StreamBuf(nst) is either a single buffer, or a member
	     * of a doubly linked list (in case of string stream) */
	    unsigned char *prev = StreamBufHeader(nst)->prev;
	    unsigned char *next = StreamBuf(nst);
	    while (prev)
	    {
		linked_io_buffer_t *this = BufHeader(prev);
		prev = this->prev;
		hg_free(this);
	    }
	    while (next)
	    {
		linked_io_buffer_t *this = BufHeader(next);
		next = this->next;
		hg_free(this);
	    }
	}
	StreamBuf(nst) = NO_BUF;
    }

    if (!IsNil(StreamEvent(nst).tag)) {
	if (IsTag(StreamEvent(nst).tag.kernel, TPTR)) {
	    /* If the stream handle is embedded in the event goal, freeing the
	     * goal term may lead to recursive closing of the stream. To avoid
	     * this, we temporarily increment the reference count */
	    if (StreamMode(nst) & SSLAVE)
		ec_atomic_add(&nst->paired_stream->nref, 1);
	    else
		ec_atomic_add(&nst->nref, 1);
	    heap_event_tid.free(StreamEvent(nst).val.wptr);
	    if (!(StreamMode(nst) & SSLAVE))
		ec_atomic_add(&nst->paired_stream->nref, -1);
	    else
		ec_atomic_add(&nst->nref, -1);
	}
	engine_tid.free(StreamEventEngine(nst));
	StreamEventEngine(nst) = NULL;
	Make_Nil(&StreamEvent(nst));
    }

    if (StreamMode(StreamPairedStream(nst)) & SSLAVE)
    {
	_free_stream(StreamPairedStream(nst));
	StreamPairedStream(nst) = NULL;
    }
    else if (StreamLexAux(nst) != NO_BUF)
    {
	hg_free(StreamLexAux(nst));
	StreamLexAux(nst) = NO_BUF;
    }

    if (StreamMode(nst) & SSLAVE)
	hg_free_size(nst, sizeof(stream_desc));
    else 
	StreamMode(nst) = SCLOSED;
}

/*
 * Close a stream
 * This does not change the reference count.
 * Call with stream already locked (or reference count is 0, i.e. not shared).
 * Options:
 *	CLOSE_FORCE: free the stream even if there were errors
 *	CLOSE_LOST: we are closing because the stream was lost (fail/gc)
*/

int
ec_close_stream(stream_id nst, int options)
{
    int res = PSUCCEED;

    if(!IsOpened(nst))
	return STREAM_SPEC;

    assert(!(StreamMode(nst) & SSLAVE));

    if (IsWriteStream(nst) && !IsQueueStream(nst))
    {
        int err = ec_flush(nst);
        if ((err != PSUCCEED) && !(options & CLOSE_FORCE))
            return err;
	res = err;
    }
    /* Don't close the stdin, stdout and stderr file descriptors (SDONTCLOSE)
     * because this very likely hangs the system. Moreover, we would then
     * need a way to reopen them, e.g. after restoring a saved state.
     */
    if (StreamMode(nst) & SDONTCLOSE)
        return res;

    if (!(IsNullStream(nst) || IsStringStream(nst) || IsQueueStream(nst)))
    {
        int err = RemoteStream(nst) ? io_rpc(nst, IO_CLOSE) : _local_io_close(nst);
        if ((err != PSUCCEED) && !(options & CLOSE_FORCE))
            return err;
	res = (res==PSUCCEED ? err : res);
    }

    if(IsFileStream(nst) &&
	( StreamMode(nst) & SDELETECLOSED ||
	  StreamMode(nst) & SDELETELOST && options & CLOSE_LOST))
    {
	if (ec_unlink(DidName(StreamPath(nst))) < 0)
	{
	    if (!(options & CLOSE_FORCE)) return SYS_ERROR_ERRNO;
	    res = (res==PSUCCEED ? SYS_ERROR_ERRNO : res);
	}
    }
    _free_stream(nst);
    return res;
}


/* Auxiliary function which has to be executed on the process
 * that owns the file descriptor (possibly via rpc).
 */

#define THREAD_TIMEOUT 3000	/* milliseconds */

static int
_local_io_close(stream_id nst)
{
    int err;

    Check_Stream_Owner(nst);

    if (IsSocket(nst))
    {
	if (SocketUnix(nst) != D_UNKNOWN)
	    (void) ec_unlink(DidName(SocketUnix(nst)));

	(void) ec_stream_reset_sigio(nst);
	if (SocketInputStream(nst))
	    (void) ec_stream_reset_sigio(SocketInputStream(nst));
    }
    ec_teardown_stream_sigio_thread(nst, 1);
    if (StreamPairedStream(nst))
	ec_teardown_stream_sigio_thread(StreamPairedStream(nst), 1);

#if defined(HAVE_READLINE)
    if (IsReadlineStream(nst)) {
	if (fclose(StreamFILE(nst)) < 0)
	{
	    return(SYS_ERROR_ERRNO);
	}
    }
#endif

    return StreamMethods(nst).close(StreamUnit(nst));
}


void
reset_ttys_and_buffers(void)
{
    int i;

    for (i = 0; i < NbStreams; i++)
    {
	stream_id nst = StreamId(i);
	Lock_Stream(nst);
	(void) ec_flush(nst);
	Unlock_Stream(nst);
    }
}

void
flush_and_close_io(int own_streams_only)
{
    int         i;

    for (i = 0; i < NbStreams; i++)
    {
	stream_id nst = StreamId(i);
	/*
	ec_printff(current_err_, "Stream %d: nref=%d, %s, %s\n", i, nst->nref,
		(!IsOpened(nst))?"closed":mutex_was_unlocked(&nst->lock)?"open":"open+locked",
		nst->name? DidName(nst->name) : "");
	*/
	if (!IsOpened(nst))
	    continue;
	Lock_Stream(nst);
	if (!own_streams_only || !RemoteStream(nst))
	{
            (void) ec_close_stream(nst, CLOSE_FORCE|CLOSE_LOST);
	}
	Unlock_Stream(nst);
    }
}

/**************** INPUT ****************/

/*
 * FUNCTION NAME:	fill_buffer()
 *
 * PARAMETERS:		nst - stream identifier
 *
 * DESCRIPTION:
 *
 * Get a new buffer from the specified stream.
 * This function is called from lex_an() only when EOB_MARK is encountered,
 * and from ec_getch() if moreover Ptr - Buf >= Cnt
 * StreamPtr(nst) points to the EOB_MARK.
 */
int
fill_buffer(register stream_id nst)
{
    register unsigned char	*ptr;
    register unsigned char	*start;
    int i;

    if (StreamPtr(nst) - StreamBuf(nst) < StreamCnt(nst))
	return PSUCCEED;	/* shouldn't happen */

    /* Refilling after having returned EOF is only allowed for SoftEofStreams */
    if ((StreamMode(nst) & MEOF) && !IsSoftEofStream(nst))
	return PEOF;

    if (IsStringStream(nst))
    {
	return _string_fill_buffer(nst);
    }
    else if (IsQueueStream(nst))
    {
	return _queue_fill_buffer(nst);
    }

    /* Copy the last LOOKAHEAD characters in the buffer before its beginning
     * to enable the pushback in the lexer.
     */
    ptr = StreamPtr(nst);
    start = StreamBuf(nst);
    for (i = LOOKAHEAD; i > 0; --i)
    	*--start = *--ptr;

    /* print the prompt if necessary */
    if (StreamMode(nst) & DONT_PROMPT)
	StreamMode(nst) ^= DONT_PROMPT;	/* but print further prompts */
    else if(StreamPromptStream(nst) != NO_STREAM)
    {
	(void) ec_outf(	StreamPromptStream(nst),
		DidName(StreamPrompt(nst)),
		DidLength(StreamPrompt(nst)));
	(void) StreamMethods(nst).flush(StreamPromptStream(nst));
    }

    /* If the buffer has been modified, flush it before reading a new one. */
    (void) StreamMethods(nst).flush(nst);

    return RemoteStream(nst) ? io_rpc(nst, IO_FILL): _local_fill_buffer(nst);
}


/* Auxiliary function which has to be executed on the process
 * that owns the file descriptor (possibly via rpc).
 */
static int
_local_fill_buffer(stream_id nst)
{
    int		count;

    Check_Stream_Owner(nst);
    /* initialise the buffer properly, cause the read may be aborted */
    StreamOffset(nst) += StreamCnt(nst);
    StreamCnt(nst) = 0;
    StreamPtr(nst) = StreamBuf(nst);
    *StreamPtr(nst) = EOB_MARK;

    if (E_read_hook != NULL)
	while (!(*E_read_hook)(StreamUnit(nst)));

#if defined(HAVE_READLINE)
    if (IsReadlineStream(nst)) {
	char		*line;
	char		*expansion;
	int		res;
	extern FILE	*rl_instream;
	extern char	*readline();
	extern void	add_history();
	extern int	history_expand();

	rl_instream = StreamFILE(nst);
	line = readline("");
	if (line == (char *) 0)
	    count = 0;
	else {
	    res = history_expand(line, &expansion);
	    if (res) {
		(void) ec_outfs(StreamPromptStream(nst), expansion);
		(void) ec_outfc(StreamPromptStream(nst), '\n');
		(void) ec_flush(StreamPromptStream(nst));
	    }
	    switch (res)
	    {
	    case 2:	/* :p - print only */
		/* add_history() is called in history_expand() */
		/* fall into */
	    case -1:	/* error in expansion */
		count = 1;
		(void) free(expansion);
		*line = '\0';
		break;

	    case 1:	/* expansion done */
		(void) free(line);
		line = expansion;
		/* fall into */
	    case 0:	/* no expansion */
		count = strlen(line) + 1;
	    }
	    if (count > StreamSize(nst)) {
		_resize_stream_buffer(nst, (long) count);
	    }
	    (void) strcpy((char *) StreamBuf(nst), line);
	    StreamBuf(nst)[count - 1] = '\n';
	    StreamBuf(nst)[count] = EOB_MARK;
	    if (count > 1)
		add_history(line);
	    (void) free(line);
	}
	StreamBuf(nst)[count] = EOB_MARK;
    } else
#endif
    {
	int err;
	int wanted = StreamSize(nst);
	count = StreamMethods(nst).read(StreamUnit(nst), (char *) StreamBuf(nst), wanted, &err);
	/* Caution: Windows may store a ^Z in the buffer while returning 0... */
	StreamBuf(nst)[count] = EOB_MARK;
	if (count <= 0)
	{
	    StreamMode(nst) &= ~MREAD;
	    return count == 0 ? PEOF : err;
	}
	else if (nst->signal_thread && StreamNeedsThread(nst))
	{
	    ec_reenable_sigio(nst, wanted, count);
	}
    }
    StreamMode(nst) = (StreamMode(nst) & ~MEOF) | MREAD;
    StreamCnt(nst) = count;

    if (StreamMode(nst) & SSCRAMBLE)
    {
	/*
	 * descramble the data we have just read
	 */
	int i;
	uint32_t key = StreamRand(nst);
	for (i=0; i < count; ++i)
	{
	    uint8_t plain;
	    key = NextRand(key);
	    StreamBuf(nst)[i] = plain = StreamBuf(nst)[i] ^ (key % 0xff);
	    key += plain;
	}
	StreamRand(nst) = key;
    }

    return(PSUCCEED);
}


static int
_queue_fill_buffer(stream_id nst)
{
    if (StreamWBuf(nst) != StreamBuf(nst))
    {
	Advance_Buffer(nst)
	Free_Prev_Buffer(nst)
	if (StreamBuf(nst) != StreamWBuf(nst))
	    StreamOffset(nst) -= StreamCnt(nst);
	return PSUCCEED;
    }
    else
    {
	StreamMode(nst) &= ~MWRITE;	/* nothing more to read */
	return PEOF;
    }
}


static int
_string_fill_buffer(stream_id nst)
{
    if (StreamBufHeader(nst)->next)
    {
	StreamOffset(nst) += StreamCnt(nst);
	Advance_Buffer(nst)
	return PSUCCEED;
    }
    else
    {
	return PEOF;
    }
}


/*ARGSUSED*/
int
set_readline(stream_id nst)
{
#if defined(HAVE_READLINE)
    FILE	*f;
    extern char *rl_basic_word_break_characters;
    extern char *rl_completer_quote_characters;
    extern char *rl_readline_name;
    extern void	using_history(), stifle_history();

    f = fdopen(StreamUnit(nst), "r");
    if (f == (FILE *) 0) {
	return SYS_ERROR_ERRNO;
    }
    nst->stdfile = (void *) f;
    StreamMode(nst) |= READLINE;
    using_history();
    stifle_history(100);
    /* we include all special and solo characters as word breakers,
       and '.' so that filenames are quoted */
    rl_basic_word_break_characters = " \t\n\"\\';.|&{}()[],!";
    rl_completer_quote_characters = "'\"";
    rl_readline_name = "eclipse";
#endif
    return PSUCCEED;
}

/*
 * FUNCTION NAME:	ec_getch(nst) 
 *
 * PARAMETERS:		nst -	input stream number
 *
 * DESCRIPTION:		Reads one character from the specified input
 * 			stream. If the buffer is empty, it calls fill_buffer
 *			to get a new one.
 *			MUST BE CALLED WITH STREAM LOCKED !!!
 */
int
ec_getch(stream_id nst)
{
    int			res;

    if (nst == null_)
	return PEOF;
    if(!(IsReadStream(nst)))
	return(STREAM_MODE);
    if (StreamPtr(nst) - StreamBuf(nst) >= StreamCnt(nst))
    {
	if (fill_buffer(nst) != PSUCCEED)
	{
	    if (StreamMode(nst) & MEOF)
		return IsSoftEofStream(nst) ? PEOF : READ_PAST_EOF;
	    else
		StreamMode(nst) |= MEOF;
	    return PEOF;
	}
    }
    res = *StreamPtr(nst)++;
    if (res == '\n')
	StreamLine(nst)++;
    return(res);
}


int
ec_ungetch(stream_id nst)
{
    int		res;

    if (nst == null_)
	return PSUCCEED;
    if(!(IsReadStream(nst)))
	return(STREAM_MODE);

    if (StreamMode(nst) & MEOF)
    {
	/* if we had just read eof, simply forget this fact */
	StreamMode(nst) &= ~MEOF;
    }
    else
    {
	/*
	 * Ideally, we would allow exactly LOOKAHEAD bytes of unget
	 * (independent of current buffer alignment), except at the beginning
	 * of the stream, or after a seek.
	 * To implement this precisely is very difficult, because:
	 * - after seek, the buffer contents may or may not be valid (MREAD),
	 *   the lookahead bytes may be valid even if the buffer is invalid
	 * - we don't know the "beginning" of queues
	 * - we don't know how many consecutive ungets we had already
	 * We therefore just say that you can unget up to LOOKAHEAD bytes
	 * of what you have read immediately before, otherwise the result
	 * is undefined (you will read random data after an illegal unget).
	 */
	if (StreamPtr(nst) > StreamBuf(nst) - LOOKAHEAD)
	{
	    /* actually go back in the buffer (or the lookahead pre-buffer) */
	    --StreamPtr(nst);
	    if (*StreamPtr(nst) == '\n')
		StreamLine(nst)--;
	}
    }
    Succeed_;
}


/*
 * read the next n characters from the specified stream and return
 * a pointer to this string. This is an unterminated string!
 * *res is set to the number of available bytes (may be less than n).
 * If the string is completely in the stream input buffer, we return
 * a pointer into this buffer, otherwise a compact copy is constructed
 * in the lex_aux buffer and its address is returned.
 * If there was a problem reading, NULL is returned and *res is set
 * to the error code.
 */

char *
ec_getstring(stream_id nst,
	word n,		/* number of bytes requested */
	word *res)	/* number of bytes read, or error code */
{
    register unsigned char	*pbuf, *paux;
    word			avail, nleft;
    word			lex_aux_size;
    int				err;

    if (StreamPtr(nst) - StreamBuf(nst) >= StreamCnt(nst))
    {
	if (fill_buffer(nst) != PSUCCEED)
	{
	    if (StreamMode(nst) & MEOF) {
		*res = IsSoftEofStream(nst) ? PEOF : READ_PAST_EOF;
	    } else {
		StreamMode(nst) |= MEOF;
		*res = PEOF;
	    }
	    return NULL;
	}
    }
    pbuf = StreamPtr(nst);
    avail = StreamBuf(nst) + StreamCnt(nst) - pbuf;

    if (avail >= n)	/* all requested bytes are in the buffer */
    {
	StreamPtr(nst) = pbuf + n;
	*res = n;
	return (char *) pbuf;
    }

    /* we have to make a copy */

    lex_aux_size = StreamLexSize(nst);
    if (n > lex_aux_size)	/* grow the lex_aux buffer, if necessary */
    {
	while (lex_aux_size < n)
		lex_aux_size *= 2;
	hg_free(StreamLexAux(nst));
	StreamLexAux(nst) = (unsigned char *) hg_alloc((int)lex_aux_size);
	StreamLexSize(nst) = lex_aux_size;
    }

    /* now copy the string into the lex_aux buffer */

    paux = StreamLexAux(nst);
    nleft = n;
    while (avail < nleft)
    {
	nleft -= avail;
	while (avail--)
	    *paux++ = *pbuf++;
	StreamPtr(nst) = pbuf;	/* needed for fill_buffer() */
	err = fill_buffer(nst);
	switch(err)
	{
	case PSUCCEED:
	    break;
	case PEOF:
	    *res = n - nleft;
	    return (char *) StreamLexAux(nst);
	default:
	    *res = (word) err;
	    return NULL;
	}
	pbuf = StreamPtr(nst);
	avail = StreamBuf(nst) + StreamCnt(nst) - pbuf;
    }
    while (nleft--)
	*paux++ = *pbuf++;
    StreamPtr(nst) = pbuf;
    *res = n;
    return (char *) StreamLexAux(nst);
}


static int
_queue_read(stream_id nst, char *s, int count)
{
    int rem_count = count;
    int inbuf = StreamBuf(nst) + StreamCnt(nst) - StreamPtr(nst);
    unsigned char *ptr;

    while (rem_count > inbuf)
    {
	/* copy the whole rest of this buffer */
	for(ptr = StreamPtr(nst); inbuf > 0; --inbuf,--rem_count)
	{
	    *s++ = *ptr++;
	}
	StreamPtr(nst) = ptr;

	/* get next buffer if available */
	if (_queue_fill_buffer(nst) == PEOF)
	    return count-rem_count;	/* less than requested */

	inbuf = StreamBuf(nst) + StreamCnt(nst) - StreamPtr(nst);
    }

    /* copy the remaining wanted chars from the last buffer */
    for(ptr = StreamPtr(nst); rem_count > 0; --inbuf,--rem_count)
    {
	*s++ = *ptr++;
    }
    StreamPtr(nst) = ptr;
    if (inbuf == 0)
    {
	StreamMode(nst) &= ~MWRITE;	/* nothing more to read */
    }
    return count;
}


static int
_queue_content(stream_id nst, char *s)
{
    int count = 0;
    unsigned char *buf = StreamBuf(nst);
    unsigned char *ptr = StreamPtr(nst);

    for(;;)
    {
	count += BufHeader(buf)->cnt - (ptr - buf);

	while(ptr < buf + BufHeader(buf)->cnt)
	    *s++ = *ptr++;

	if (buf == StreamWBuf(nst))
	    break;

	ptr = buf = BufHeader(buf)->next;
    }
    return count;
}


static int
_queue_size(stream_id nst)
{
    if (StreamBuf(nst) == StreamWBuf(nst))
	return StreamOffset(nst) +
	    BufHeader(StreamWBuf(nst))->cnt - (StreamPtr(nst)-StreamBuf(nst));
    else
	return StreamOffset(nst) +
	    (StreamSize(nst)-(StreamPtr(nst)-StreamBuf(nst))) + BufHeader(StreamWBuf(nst))->cnt;
}

static int
_string_size(stream_id nst)
{
    unsigned char *buf = StreamBuf(nst);
    int count = 0;

    while (BufHeader(buf)->prev)
    	buf = BufHeader(buf)->prev;

    do
    {
	count += BufHeader(buf)->cnt;
	buf = BufHeader(buf)->next;
    } while(buf);

    return count;
}


static int
_string_content(stream_id nst, char *s)
{
    unsigned char *buf = StreamBuf(nst);
    unsigned char *ptr;
    int count = 0;

    while (BufHeader(buf)->prev)
    	buf = BufHeader(buf)->prev;

    do
    {
	count += BufHeader(buf)->cnt;
	for (ptr = buf; ptr < buf + BufHeader(buf)->cnt; )
	    *s++ = *ptr++;
	buf = BufHeader(buf)->next;
    } while(buf);

    return count;
}


int Winapi
ec_queue_read(int qid, char *s, int count)
{
    stream_id nst = StreamId(qid);
    if (!IsOpened(nst))
	return STREAM_SPEC;
    if (!IsQueueStream(nst) || !IsWriteStream(nst))
	return STREAM_MODE;
    Lock_Stream(nst);
    int res = _queue_read(nst, s, count);
    Unlock_Stream(nst);
    return res;
}

int Winapi
ec_queue_avail(int qid)
{
    stream_id nst = StreamId(qid);
    if (!IsOpened(nst))
	return STREAM_SPEC;
    if (!IsQueueStream(nst) || !IsWriteStream(nst))
	return STREAM_MODE;
    Lock_Stream(nst);
    int res = _queue_size(nst);
    Unlock_Stream(nst);
    return res;
}


/**************** OUTPUT ****************/

static int
_queue_write(stream_id nst, char *s, int count)
{
    unsigned char *wptr, *wbuf, *new_buf;
    word i, bfree;

    if (count == 0)
	return PSUCCEED;
    	
    /* raise event if writing to empty queue */
    if (StreamBuf(nst) == StreamWBuf(nst) && StreamPtr(nst) == StreamBuf(nst) + StreamCnt(nst))
    {
	StreamMode(nst) &= ~MEOF;
	if (!IsNil(StreamEvent(nst).tag))
	{
	    int res = ecl_post_event(StreamEventEngine(nst), StreamEvent(nst));
	    Return_If_Error(res);
	}
    }

    /* fill buffers to their end, and insert new ones if needed */
    wbuf = StreamWBuf(nst);
    wptr = wbuf + BufHeader(wbuf)->cnt;
    bfree = StreamSize(nst) - BufHeader(wbuf)->cnt;
    while (count > bfree)
    {
	while (bfree-- > 0)
	{
	    *wptr++ = *s++;
	    --count;
	}
	*wptr = EOB_MARK;
	BufHeader(wbuf)->cnt = wptr - wbuf;
	if (StreamBuf(nst) == wbuf)
	    StreamCnt(nst) = BufHeader(wbuf)->cnt;
	else
	    StreamOffset(nst) += BufHeader(wbuf)->cnt;

	if (BufHeader(wbuf)->next != StreamBuf(nst))
	{
	    ec_printff(current_err_, "Inconsistent buffer list in queue\n");
	}
	/* insert a new empty write buffer */
	New_Buffer(nst, new_buf);
	BufHeader(new_buf)->prev = wbuf;
	BufHeader(new_buf)->next = BufHeader(wbuf)->next;
	BufHeader(BufHeader(wbuf)->next)->prev = new_buf;
	BufHeader(wbuf)->next = new_buf;
	wptr = wbuf = new_buf;
	bfree = StreamSize(nst);
    }

    /* Fill last buffer partially (count <= bfree) */
    for(i = BufHeader(wbuf)->cnt; i < BufHeader(wbuf)->cnt + count; ++i)
    	wbuf[i] = *s++;
    BufHeader(wbuf)->cnt += count;
    if (StreamBuf(nst) == wbuf)
	StreamCnt(nst) = BufHeader(wbuf)->cnt;

    wbuf[BufHeader(wbuf)->cnt] = EOB_MARK;
    StreamWBuf(nst) = wbuf;
    StreamMode(nst) |= MWRITE;	/* there is something to read */
    return PSUCCEED;
}

static int
_string_write(stream_id nst, char *s, int count)
{
    /* remaining space in the current buffer */
    int bfree = StreamSize(nst) - (StreamPtr(nst) - StreamBuf(nst));

    /* fill buffers to their end, and append new ones */
    while (count > bfree)
    {
	while (bfree-- > 0)
	{
	    *(StreamPtr(nst))++ = *s++;
	    --count;
	}
	if (StreamPtr(nst) > StreamBuf(nst) + StreamCnt(nst))
	{
	    StreamCnt(nst) = StreamBufHeader(nst)->cnt = StreamPtr(nst) - StreamBuf(nst);
	    *StreamPtr(nst) = EOB_MARK;
	}
	if (!StreamBufHeader(nst)->next)
	{
	    Append_New_Buffer(nst);
	}
	StreamOffset(nst) += StreamCnt(nst);
	Advance_Buffer(nst);
	bfree = StreamSize(nst) - (StreamPtr(nst) - StreamBuf(nst));
    }

    /* Fill last buffer partially (count <= bfree) */
    while (count-- > 0)
    {
    	*(StreamPtr(nst))++ = *s++;
    }
    if (StreamPtr(nst) > StreamBuf(nst) + StreamCnt(nst))
    {
	StreamCnt(nst) = StreamBufHeader(nst)->cnt = StreamPtr(nst) - StreamBuf(nst);
	*StreamPtr(nst) = EOB_MARK;
    }
    Succeed_;
}

static int
_buffer_write(stream_id nst, char *s, int count)
{
    int bfree = StreamSize(nst) - (StreamPtr(nst) - StreamBuf(nst));
    int res = PSUCCEED;

    if (bfree < 0 || bfree > StreamSize(nst))
    {
	/*
	 * Ptr points outside the buffer, we *must* be reading
	 * the lex_aux buffer, i.e. the buffer is MREAD.
	 */
	StreamPtr(nst) = StreamBuf(nst);
	bfree = StreamSize(nst);
    }
    while (count > 0)
    {
	int size = count < bfree ? count : bfree;
	count -= size;
	bfree -= size;
	while (size--)
	    *(StreamPtr(nst))++ = *s++;
	StreamMode(nst) |= MWRITE;
	if (count <= 0)
	    break;
	else if ((res = StreamMethods(nst).flush(nst)) != PSUCCEED)
	    break;
	/* for string streams StreamPtr(nst) != StreamBuf(nst) ! */
	bfree = StreamSize(nst) - (StreamPtr(nst) - StreamBuf(nst));
    }
    /*
     * If we are writing into a r/w stream whose buffer was not
     * read, we must mark that there is nothing to read for lex_an()
     */
    if (!(StreamMode(nst) & MREAD))
	*StreamPtr(nst) = EOB_MARK;
    return res;
}

static int
_tty_write(stream_id nst, char *s, int count)
{
    if (IsTty(nst) && StreamMode(nst) & MREAD)
    {
	/*
	 * The tty is open in update mode and there is something
	 * in the buffer. We flush it, maybe discarding the rest.
	 */
	StreamPtr(nst) = StreamBuf(nst);
	StreamCnt(nst) = 0;
	StreamMode(nst) &= ~MREAD;
    }
    return _buffer_write(nst, s, count);
}

/*ARGSUSED*/
static int
_null_write(stream_id nst, char *s, int count)
{
    Succeed_;
}

int Winapi
ec_queue_write(int qid, char *s, int count)
{
    int res;
    stream_id nst = StreamId(qid);
    if (!IsOpened(nst))
	return STREAM_SPEC;
    if (!IsQueueStream(nst) || !IsReadStream(nst))
	return STREAM_MODE;
    Lock_Stream(nst);
    res = _queue_write(nst, s, count);
    Unlock_Stream(nst);
    return res < 0 ? res : count;
}


/*
 * FUNCTION NAME:	ec_outf(nst, s, count)
 *
 * PARAMETERS:		nst -	output stream number
 *			s -	address of the area to output
 *			count -	number of bytes to output
 *
 * DESCRIPTION:		This is the basic output primitive that
 *			writes the specified number of bytes into the
 *			output buffer. The check for a full buffer
 *			is done at the beginning, i.e. after the output
 *			the buffer may be full.
 */
int
ec_outf(stream_id nst /*locked!*/, const char *s, int count)
{
    if (!IsWriteStream(nst))
	return STREAM_MODE;
    if (count <= 0)
	return PSUCCEED;
    StreamLastWritten(nst) = s[count-1];
    return StreamMethods(nst).outf(nst, (char *) s, count);
}


/*
 * FUNCTION NAME:	ec_outfc(nst, c)
 *
 * PARAMETERS:		nst -	output stream number
 *			c - character to output
 *
 * DESCRIPTION:		Write one character into the output buffer.
 */
int
ec_outfc(stream_id nst /*locked!*/, int c)
{
    char ch = c;
    if (!IsWriteStream(nst))
	return STREAM_MODE;
    StreamLastWritten(nst) = c;
    return StreamMethods(nst).outf(nst, &ch, 1);
}


/*
 * FUNCTION NAME:	ec_outfw(nst, w)
 *
 * PARAMETERS:		nst -	output stream number
 *			w - word to output
 *
 * DESCRIPTION:		Write one longword into the output buffer.
 *			StreamPtr is assumed to be longword-aligned!
 *			This is only used internally for profile_stream.
 *			Stream must be a file.
 */
int
ec_outfw(stream_id nst /*locked!*/, word w)
{
    int			res = PSUCCEED;
    register char	*p;
    register char	*s;
    int			i;

    if (StreamPtr(nst) == StreamBuf(nst) + StreamSize(nst))
	res = StreamMethods(nst).flush(nst);
    p = (char *) StreamPtr(nst);
    s = (char *) &w;
    for (i = 0; i < sizeof(word); i++)
	*p++ = *s++;
    StreamPtr(nst) = (unsigned char *) p;
    StreamMode(nst) |= MWRITE;
    return res;
}

int
ec_newline(stream_id nst /*locked!*/)
{
    int		res;

    if (StreamMode(nst) & SEOLCR)
	res = ec_outf(nst, "\r\n", 2);
    else
	res = ec_outfc(nst, '\n');
    if (res != PSUCCEED)
	return res;
    if (StreamMode(nst) & SFLUSHEOL)
	return StreamMethods(nst).flush(nst);
    return PSUCCEED;
}

int
ec_outfs(stream_id nst /*locked!*/, const char *s)
{
    return ec_outf(nst, s, strlen(s)); /* could be more efficient */
}

int
ec_flush(stream_id nst /*locked!*/)
{
    if (!IsWriteStream(nst))
	return(STREAM_MODE);
    return StreamMethods(nst).flush(nst);
}


#if defined(HAVE_READLINE)
/*
 * Increase the size of a (string) stream buffer
 * Make sure that newsize >= StreamSize(nst) !
 * This function may modify StreamBuf, StreamPtr, StreamSize
 */
static void
_resize_stream_buffer(stream_id nst, word newsize)
{
    register word ptr_off = StreamPtr(nst) - StreamBuf(nst);

    StreamBuf(nst) = (unsigned char *) (
			(linked_io_buffer_t*) hg_resize(
				StreamBufHeader(nst),
				(int)(sizeof(linked_io_buffer_t) + newsize + 1))
			+ 1);
    StreamBuf(nst)[newsize] = EOB_MARK;
    StreamSize(nst) = newsize;
    StreamPtr(nst) = StreamBuf(nst) + ptr_off;
}
#endif


/*
 * FUNCTION NAME:	..._flush(nst) 
 *
 * PARAMETERS:		nst -	stream number
 *
 * DESCRIPTION:		Flushes the buffer which is supposed to
 *			contain some written data. If the buffer was originally
 *			read in, it has to seek back and write the whole
 *			buffer.
 */

static int
_dummy_flush(stream_id nst)
{
    return PSUCCEED;
}

static int
_queue_flush(stream_id nst)
{
    if (StreamMode(nst) & MWRITE) {
        if (nst->cond)
            (void) ec_cond_signal(nst->cond, 1);
        if (StreamMode(nst) & SYIELD)
            return YIELD_ON_FLUSH_REQ;
    }
    return PSUCCEED;
}

static int
_buffer_flush(stream_id nst)
{
    if (!(StreamMode(nst) & MWRITE))
        return PSUCCEED;
    return RemoteStream(nst) ? io_rpc(nst, IO_FLUSH) : _local_io_flush_out(nst);
}

/* Auxiliary function which has to be executed on the process
 * that owns the file descriptor (possibly via rpc).
 */
static int
_local_io_flush_out(stream_id nst)
{
    int		n;

    Check_Stream_Owner(nst);
    /* save the last character */
    *(StreamBuf(nst) - 1) = *(StreamPtr(nst) - 1);
    if (StreamMode(nst) & MREAD && !IsTty(nst))
    {
	if (lseek(StreamUnit(nst), (long) - StreamCnt(nst), LSEEK_INCR) == -1)
	{
	    return SYS_ERROR_ERRNO;	/* from lseek() */
	}
	if (StreamPtr(nst) < StreamBuf(nst) + StreamCnt(nst))
	    StreamPtr(nst) = StreamBuf(nst) + StreamCnt(nst);
	StreamMode(nst) &= ~MREAD;
    }
    if (StreamMode(nst) & SSCRAMBLE)
    {
	/*
	 * scramble the data before writing it out
	 */
	int bufsize = StreamSize(nst);
	New_Array(char, scrambled_buf, bufsize);
	int count = StreamPtr(nst) - StreamBuf(nst);
	uint32_t key = StreamRand(nst);
	int i;
	for (i=0; i < count; ++i)
	{
	    uint8_t plain = StreamBuf(nst)[i];
	    key = NextRand(key);
	    scrambled_buf[i] = plain ^ (key % 0xff);
	    key += plain;
	}
	StreamRand(nst) = key;
	n = StreamMethods(nst).write(StreamUnit(nst), scrambled_buf, count);
	Delete_Array(char, scrambled_buf, bufsize);
    }
    else
    {
	n = StreamMethods(nst).write(StreamUnit(nst),
			(char *) StreamBuf(nst),
			StreamPtr(nst) - StreamBuf(nst));
    }
    if (n == PSUCCEED)
    {
	StreamOffset(nst) += StreamPtr(nst) - StreamBuf(nst);
	StreamPtr(nst) = StreamBuf(nst);
	StreamMode(nst) &= ~MWRITE;
	if (IsReadStream(nst))
	{
	    /* Mark the input buffer empty. */
	    StreamCnt(nst) = 0;
	    *StreamBuf(nst) = EOB_MARK;
	}
    }
    return(n);	/* maybe SYS_ERROR_OS */
}


/*
 * FUNCTION NAME:	ec_seek_stream(nst, pos, whence) 
 *
 * PARAMETERS:		nst -	stream number
 *
 * DESCRIPTION:		Seek on a stream.
 */

int
ec_seek_stream(stream_id nst, long int pos, int whence)
{
    /* no seek on scrambled files: synchronisation gets lost */
    /* no seek on append files: always at eof */
    if(!IsOpened(nst) || (StreamMode(nst) & (SSCRAMBLE|SAPPEND)))
    {
	return STREAM_MODE;
    }
    return StreamMethods(nst).seek(nst, pos, whence);
}

/*ARGSUSED*/
static int
_dummy_seek(stream_id nst, long int pos, int whence)
{
    return PSUCCEED;
}

/*ARGSUSED*/
static int
_illegal_seek(stream_id nst, long int pos, int whence)
{
    return STREAM_MODE;
}

static int
_string_seek(stream_id nst, long int pos, int whence)
{
    int i;
    if (pos < 0)
	return RANGE_ERROR;

    if (whence == LSEEK_END)			/* seek to current end */
    {
	/* skip to the last buffer */
	while(StreamBufHeader(nst)->next)
	{
	    StreamOffset(nst) += StreamCnt(nst);
	    Advance_Buffer(nst)
	}
	StreamPtr(nst) = StreamBuf(nst) + StreamCnt(nst);
    }
    else if (pos <= StreamOffset(nst) + StreamCnt(nst)) /* seek backwards */
    {
	while(pos < StreamOffset(nst))
	{
	    Retreat_Buffer(nst)
	    StreamOffset(nst) -= StreamCnt(nst);
	}
	StreamPtr(nst) = StreamBuf(nst) + (pos - StreamOffset(nst));
    }
    else					/* seek forward */
    {
	/* skip forward over existing buffers */
	while(pos >= StreamOffset(nst) + StreamSize(nst) && StreamBufHeader(nst)->next)
	{
	    StreamOffset(nst) += StreamCnt(nst);
	    Advance_Buffer(nst)
	}

	if (pos < StreamOffset(nst) + StreamCnt(nst))
	{
	    /* seek target is before the current end */
	    StreamPtr(nst) = StreamBuf(nst) + (pos - StreamOffset(nst));
	}
	else if (pos < StreamOffset(nst) + StreamSize(nst))
	{
	    /* stream gets extended within existing buffer */
	    if (!IsWriteStream(nst))
		return RANGE_ERROR;
	    for (i = StreamCnt(nst); i < pos - StreamOffset(nst); ++i)
		StreamBuf(nst)[i] = 0;
	    StreamCnt(nst) = BufHeader(StreamBuf(nst))->cnt = pos - StreamOffset(nst);
	    StreamBuf(nst)[StreamCnt(nst)] = EOB_MARK;
	    StreamPtr(nst) = StreamBuf(nst) + StreamCnt(nst);
	}
	else
	{
	    /* the stream must be extended with new buffers */
	    if (!IsWriteStream(nst))
		return RANGE_ERROR;

	    /* zero-fill the rest of the last buffer so far */
	    for (i = StreamCnt(nst); i < StreamSize(nst); ++i)
		StreamBuf(nst)[i] = 0;
	    StreamCnt(nst) = BufHeader(StreamBuf(nst))->cnt = StreamSize(nst);
	    StreamBuf(nst)[StreamCnt(nst)] = EOB_MARK;

	    /* append and zero-fill fresh buffers as necessary */
	    while(pos >= StreamOffset(nst) + StreamSize(nst))
	    {
		Append_New_Buffer(nst)
		StreamOffset(nst) += StreamCnt(nst);
		Advance_Buffer(nst)
		if (pos < StreamOffset(nst) + StreamSize(nst))
		    break;
		for (i = 0; i < StreamSize(nst); ++i)
		    StreamBuf(nst)[i] = 0;
		StreamCnt(nst) = BufHeader(StreamBuf(nst))->cnt = StreamSize(nst);
		StreamBuf(nst)[StreamCnt(nst)] = EOB_MARK;
	    }

	    /* zero-fill new last buffer as far as necessary */
	    for (i = StreamCnt(nst); i < pos - StreamOffset(nst); ++i)
		StreamBuf(nst)[i] = 0;
	    StreamCnt(nst) = BufHeader(StreamBuf(nst))->cnt = pos - StreamOffset(nst);
	    StreamBuf(nst)[StreamCnt(nst)] = EOB_MARK;

	    StreamPtr(nst) = StreamBuf(nst) + StreamCnt(nst);
	}
    }
    StreamMode(nst) &= ~MEOF;
    return PSUCCEED;
}

static int
_file_seek(stream_id nst, long int pos, int whence)
{
    int		res;
    struct_stat	buf;
    long	max = -1;
    long	at;

    if (!IsWriteStream(nst) || whence == LSEEK_END)	/* we need the length */
    {
	if (fstat(StreamUnit(nst), &buf) == 0)
	    max = buf.st_size;
	else
	{
	    Bip_Error(SYS_ERROR_ERRNO)
	}
    }
    if (whence == LSEEK_END)
    {
	pos = max;
    }
    else if (pos < 0 || max > 0 && pos > max)
    {
	Bip_Error(RANGE_ERROR)
    }

    if (IsSocket(nst))
	nst = SocketInputStream(nst);
    at = StreamOffset(nst);
    /*
     * If the pointer stays inside an input buffer, we do not have
     * to seek in the real file.
     */
    if (StreamMode(nst) & MREAD && pos >= at && pos <= at + StreamCnt(nst))
    {
	StreamPtr(nst) = StreamBuf(nst) + pos - at;
    }
    else
    {
	if (!IsOpened(nst))
	{
	    Bip_Error(STREAM_MODE);
	}
	if (IsWriteStream(nst))
	    ec_flush(nst);
	at = lseek(StreamUnit(nst), pos, LSEEK_SET);
	StreamMode(nst) &= ~(MREAD | MWRITE);
	StreamPtr(nst) = StreamBuf(nst);
	*StreamPtr(nst) = EOB_MARK;
	StreamCnt(nst) = 0;
	StreamOffset(nst) = pos;
	if (at != pos)
	{
	    Bip_Error(SYS_ERROR_ERRNO);
	}
    }
    StreamMode(nst) &= ~MEOF;
    Succeed_;
}


/* 
 * Allow seeking back to the beginning of current buffer.  We use this
 * to get left error context for error messages on non-seekable devices.
 */

static int
_buffer_seek(stream_id nst, long int pos, int whence)
{
    int		res;
    struct_stat	buf;
    long	max = -1;
    long	buf_offset;

    if (IsSocket(nst))
	nst = SocketInputStream(nst);

    buf_offset = StreamOffset(nst);
    if (whence == LSEEK_END ||
	pos < buf_offset ||
	pos > buf_offset + (StreamPtr(nst)-StreamBuf(nst)))
    {
	Bip_Error(RANGE_ERROR)
    }
    StreamPtr(nst) = StreamBuf(nst) + pos - buf_offset;
    StreamMode(nst) &= ~MEOF;
    Succeed_;
}


/*
 * at (position in stream)
 */

int
ec_stream_at(stream_id nst, long int *pos)
{
    return StreamMethods(nst).at(nst, pos);
}

static int
_buffer_at(stream_id nst, long int *pos)
{
    *pos = StreamOffset(nst) + (StreamPtr(nst) - StreamBuf(nst));
    Succeed_;
}

static int
_file_at(stream_id nst, long int *pos)
{
    if (StreamMode(nst) & SAPPEND)
    {
	struct_stat buf;
	if (fstat(StreamUnit(nst), &buf) == 0)
	    *pos = buf.st_size;
	else
	{
	    Bip_Error(SYS_ERROR_ERRNO)
	}
    }
    else
    {
	*pos = StreamOffset(nst) + (StreamPtr(nst) - StreamBuf(nst));
    }
    Succeed_;
}

static int
_socket_at(stream_id nst, long int *pos)
{
    nst = SocketInputStream(nst);
    *pos = StreamOffset(nst) + (StreamPtr(nst) - StreamBuf(nst));
    Succeed_;
}

static int
_dummy_at(stream_id nst, long int *pos)
{
    *pos = 0;
    Succeed_;
}

/*
 * We consider queues as files whose beginning gets cut off on reading.
 * I.e. read-queues are always at 0, write-queues are always at the
 * position corresponding to the number of bytes currently in the queue.
 * (This can be used e.g. to find out how many bytes a write/2 produced)
 */

static int
_queue_at(stream_id nst, long int *pos)
{
    if (IsWriteStream(nst))	/* write and read/write queues */
	*pos = _queue_size(nst);
    else
	*pos = 0;
    Succeed_;
}



/*
 * nonempty - we have readable input in the buffer
 */

static int
_string_nonempty(stream_id nst)
{
    return StreamPtr(nst) - StreamBuf(nst) < StreamCnt(nst)
    	|| StreamBufHeader(nst)->next;
}

static int
_buffer_nonempty(stream_id nst)
{
    return StreamPtr(nst) - StreamBuf(nst) < StreamCnt(nst);
}


/*
 * at_eof
 */

static int
_always_at_eof(stream_id nst)
{
    Succeed_;
}

static int
_tty_at_eof(stream_id nst)
{
    Succeed_If(!_buffer_nonempty(nst));
}

static int
_queue_at_eof(stream_id nst)
{
    Succeed_If(_queue_size(nst) == 0);
}

static int
_string_at_eof(stream_id nst)
{
    Succeed_If(!_string_nonempty(nst));
}

static int
_buffer_at_eof(stream_id nst)
{
    struct_stat buf;
    long	offset;

    if (StreamMode(nst) & SAPPEND)
    {
	Succeed_;
    }

#if defined(HAVE_FSTAT)
    if(fstat(StreamUnit(nst), &buf) < 0)
#else
    if(ec_stat(DidName(StreamPath(nst)), &buf) < 0)
#endif
    {
	Bip_Error(SYS_ERROR_ERRNO)
    }
    offset = StreamPtr(nst) - StreamBuf(nst);
#ifndef _WIN32
    /* check the fd directly, our own stream flags are not reliable: */
    if (S_ISSOCK(buf.st_mode) || S_ISFIFO(buf.st_mode))
    {
	Succeed_If((buf.st_size == 0 && offset == StreamCnt(nst)) ||
		StreamMode(nst) & MEOF);
    }
#endif
    Succeed_If(StreamOffset(nst) + offset == (unsigned) buf.st_size ||
	StreamMode(nst) & MEOF);
}



/*
 * truncate a stream at the current (write) position
 * only makes sense on file and string streams
 */

static int
_string_truncate(stream_id nst)
{
    unsigned char *next = StreamBufHeader(nst)->next;
    if (next)
    {
	do {
	    linked_io_buffer_t *this = BufHeader(next);
	    next = this->next;
	    hg_free(this);
	} while (next);
	StreamBufHeader(nst)->next = NULL;
    }
    StreamCnt(nst) = StreamBufHeader(nst)->cnt = StreamPtr(nst) - StreamBuf(nst);
    *StreamPtr(nst) = EOB_MARK;
    Succeed_;
}


static int
_file_truncate(stream_id nst)
{
#ifdef _WIN32
    /* On windows, we need to flush in order to set
     * the real file pointer to the truncate position
     */
    int res = _buffer_flush(nst);
    Return_If_Error(res);
    if (ec_truncate(StreamUnit(nst)))
    {
	return SYS_ERROR_OS;
    }
#else
    if (ftruncate(StreamUnit(nst),
    	(off_t) StreamOffset(nst) + (StreamPtr(nst) - StreamBuf(nst))))
    {
	return SYS_ERROR_ERRNO;
    }
#endif
    StreamCnt(nst) = StreamPtr(nst) - StreamBuf(nst);
    *StreamPtr(nst) = EOB_MARK;
    Succeed_;
}



/*
 * Output to a Prolog stream in the form of printf().
 */

#include <stdarg.h>

static int
_ec_printf(stream_id nst, int flush, const char *fmt, va_list args)
{
    char	ibuf[BUFSIZE];
    int		res;

#ifdef HAVE_VSNPRINTF
#ifdef _WIN32
    res = _vsnprintf(ibuf, BUFSIZE, fmt, args);
#else
    res = vsnprintf(ibuf, BUFSIZE, fmt, args);
#endif
#else
    res = vsprintf(ibuf, fmt, args);
#endif
    va_end(args);
    if (res < 0 || res >= BUFSIZE)
	res = BUFSIZE;			/* truncate */
    Lock_Stream(nst);
    res = ec_outf(nst, ibuf, res);
    if (flush)
	res = (res==PSUCCEED) ? ec_flush(nst) : res;
    Unlock_Stream(nst);
    return res;
}

/**
 * Printf with atomic final flush
 */
int
ec_printff(stream_id nst, const char *fmt, ...)
{
    va_list	args;
    va_start(args, fmt);
    return _ec_printf(nst, 1, fmt, args);
}

int
p_fprintf(stream_id nst, const char *fmt, ...)
{
    va_list	args;
    va_start(args, fmt);
    return _ec_printf(nst, 0, fmt, args);
}

/**
 * Print a message, optionally adding a newline.
 * Flush if the stream has the SFLUSHEOL option set (even if no newline).
 */
int
ec_print_msg(stream_id nst, const char *msg, int newline)
{
    int res;
    Lock_Stream(nst);
    res = ec_outfs(nst, msg);
    if (res == PSUCCEED) {
        if (newline)
            res = ec_newline(nst);
        else if (StreamMode(nst) & SFLUSHEOL)
            res = ec_flush(nst);
    }
    Unlock_Stream(nst);
    return res;
}



/**************** RAW TTY PRIMITIVES ****************/

#ifndef _WIN32

/*
 * FUNCTION NAME:	_set_raw_tty(fd, min, time)
 *
 * PARAMETERS:		fd	- file descriptor
 *
 * DESCRIPTION:		Uses an ioctl(2) system call to set the specified
 *			file descriptor which must be a terminal into raw mode.
 *			
 */


/*ARGSUSED*/
static int
_set_raw_tty(int fd, int min, int time, struct termios *tbuf)
{
#ifndef _WIN32
    Termio	rawbuf;

    if (isatty(fd))
    {
#ifdef TERMIO_POSIX_STYLE
	if (tcgetattr(fd, tbuf) == -1)
#else
	if (ioctl(fd, GetTermAttr, tbuf) == -1)
#endif
	{
	    return SYS_ERROR_ERRNO;
	}
	rawbuf = *tbuf;

#if defined(TERMIO_POSIX_STYLE) || defined(TERMIO_SYS_V_STYLE)
	rawbuf.c_iflag = tbuf->c_iflag & ~(INLCR | ICRNL);
	rawbuf.c_oflag = tbuf->c_oflag & ~OPOST;
	rawbuf.c_lflag = tbuf->c_lflag &
			    ~(ICANON | ECHO | ECHOE | ECHOK | ECHONL);
	rawbuf.c_cc[VMIN] = min;
	rawbuf.c_cc[VTIME] = time;
#endif
#ifdef TERMIO_BSD_STYLE
	rawbuf.sg_flags = (tbuf->sg_flags | CBREAK) & ~ECHO;
#endif

#ifdef TERMIO_POSIX_STYLE
	if (tcsetattr(fd, TCSANOW, &rawbuf) == -1)
#else
	if (ioctl(fd, SetTermAttr, &rawbuf) == -1)
#endif
	{
	    return SYS_ERROR_ERRNO;
	}
    }
#endif
    return PSUCCEED;
}


/*
 * FUNCTION NAME:	_unset_raw_tty(fd)
 *
 * PARAMETERS:		fd	- file descriptor
 *
 * DESCRIPTION:		Uses an ioctl(2) system call to set the specified
 *			file descriptor which must be a terminal back
 *			into normal mode. Since a global variable is used
 *			to store the normal mode, this function is not
 *			safe for interrupts, but only if you use
 *			raw i/o for two terminals with different setting,
 *			which is rather rare.
 */

static int
_unset_raw_tty(int fd, struct termios *tbuf)
{
#ifndef _WIN32
    if (isatty(fd))
    {
#ifdef TERMIO_POSIX_STYLE
	if (tcsetattr(fd, TCSANOW, tbuf) == -1)
#else
	if (ioctl(fd, SetTermAttr, tbuf) == -1)
#endif
	{
	    return SYS_ERROR_ERRNO;
	}
    }
#endif
    return PSUCCEED;
}

#endif

/*
 * FUNCTION NAME:	ec_tty_in(nst)
 *
 * PARAMETERS:		nst -	input stream
 *
 * DESCRIPTION:
 *
 * Raw input from a stream. Since we switch between cooked and raw
 * mode to execute this function, all unread data will be discarded.
 * This is a bug in the whole concept.
 */
int
ec_tty_in(stream_id nst)
{
    if (!IsReadStream(nst))
	return(STREAM_MODE);
    if (!IsTty(nst))
    {
	/* suppress prompting during raw input */
	int saved_flag = StreamMode(nst) & DONT_PROMPT;
	int res;
	StreamMode(nst) |= DONT_PROMPT;
	res = ec_getch(nst);
	StreamMode(nst) = (StreamMode(nst) & ~DONT_PROMPT) | saved_flag;
	return res;
    }
    return RemoteStream(nst) ? io_rpc(nst, IO_TTYIN) : _local_tty_in(nst);
}

/* Auxiliary function which has to be executed on the process
 * that owns the file descriptor (possibly via rpc).
 * Returns character read (>=0) or error (<0)
 */
static int
_local_tty_in(stream_id nst)
{
#ifdef _WIN32
    return ec_getch_raw(StreamUnit(nst));
#else
    int			n;
    char		c;
    int			res;
    Termio		tbuf;
#if defined(TERMIO_POSIX_STYLE) || defined(TERMIO_SYS_V_STYLE)
    Termio		rawbuf;
    char		buf[10];
#endif

    Check_Stream_Owner(nst);
    /* The loop is to allow signals being handled during the read */
    /* We handle only after resetting raw mode, in case the handler longjmps */
    for (;;)
    {
	errno = 0;
	if ((res = _set_raw_tty(StreamUnit(nst), 1, 0, &tbuf)) != PSUCCEED)
	{
	    return res;
	}
	if (E_read_hook != NULL)
	{
	    while (!(*E_read_hook)(StreamUnit(nst)))
	    {}
	}
#if defined(TERMIO_POSIX_STYLE) || defined(TERMIO_SYS_V_STYLE)
	n = read(StreamUnit(nst), &c, 1);
	if (n == 1 && c == 27)	/* escape read */
	{
	    (void) _set_raw_tty(StreamUnit(nst), 0, 2, &rawbuf);
	    n = read(StreamUnit(nst), &buf[0], 2);
	    if (n == 2 && buf[0] == '[')
		c = buf[1];
	    else
		n = 1;
	}
#endif
#ifdef TERMIO_BSD_STYLE
	n = 1 << StreamUnit(nst);
	n = select(StreamUnit(nst) + 1,(fd_set *) &n,
		(fd_set *) 0,(fd_set *) 0, (struct timeval *) 0);
	if (n == 1)
	    n = read(StreamUnit(nst), &c, 1);
#endif
	if (errno != EINTR)
	    break;
	res = _unset_raw_tty(StreamUnit(nst), &tbuf);
	if (res != PSUCCEED)
	    return res;
    }
    res = _unset_raw_tty(StreamUnit(nst), &tbuf);
    if (res != PSUCCEED)
	return res;
    if(n < 1)
	return SYS_ERROR_ERRNO;
    return (int) c;
#endif
}

int
ec_tty_outs(stream_id nst, char *s, int n)
{
    if(!(IsWriteStream(nst)))
	return(STREAM_MODE);
    if(!(IsTty(nst)))
	return(ec_outf(nst,s,n));

    {
#ifdef _WIN32
	for(; n>0; --n)
	    (void) ec_putch_raw(*s++);
	return PSUCCEED;
#else
	int res;
	Termio	tbuf;

	if ((res = _set_raw_tty(StreamUnit(nst), 1, 0, &tbuf)) != PSUCCEED)
	{
	    return res;
	}
	res = StreamMethods(nst).write(StreamUnit(nst), s, n);
	(void) _unset_raw_tty(StreamUnit(nst), &tbuf);
	return(res);
#endif
    }
}

int
ec_tty_out(stream_id nst, int c)
{
    if(!(IsWriteStream(nst)))
	return(STREAM_MODE);
    if(!(IsTty(nst)))
	return(ec_outfc(nst,c));

    {
#ifdef _WIN32
	(void) ec_putch_raw(c);
	return PSUCCEED;
#else
	char s = c;
	return ec_tty_outs(nst, &s, 1);
#endif
    }
}


/*
 * A general routine which changes the setting of a symbolic stream.
 * It checks for the system-defined streams and updates them correspondingly.
 * The first argument must be the DID of the symbolic name.
 * The new stream must already be copied/reference-counted!
 * Returns PSUCCEED or error.
 */
int
set_stream(dident name, stream_id neww)
{
    int unclosable = 0;

    if(!(IsOpened(neww)))
	return(STREAM_SPEC);

    /* Check the mode of the new channel for special streams */

    if(name == d_.user || name == d_.null)
	return(SYSTEM_STREAM);
    else if(name == d_.input)
    {
	if(!IsReadStream(neww))
	    return(STREAM_MODE);
	current_input_ = neww;
    }
    else if(name == d_.output)
    {
	if(!IsWriteStream(neww))
	    return(STREAM_MODE);
	current_output_ = neww;
    }
    else if(name == d_.err)
    {
	if(!IsWriteStream(neww))
	    return(STREAM_MODE);
	current_err_ = neww;
    }
    else if(name == d_.warning_output)
    {
	if(!IsWriteStream(neww))
	    return(STREAM_MODE);
	warning_output_ = neww;
    }
    else if(name == d_.log_output)
    {
	if(!IsWriteStream(neww))
	    return(STREAM_MODE);
	log_output_ = neww;
    }
    else if(name == d_.user_input)
    {
	if(!IsReadStream(neww))
	    return(STREAM_MODE);
	user_input_ = neww;
    }
    else if(name == d_.user_output)
    {
	if(!IsWriteStream(neww))
	    return(STREAM_MODE);
	user_output_ = neww;
    }
    else if(name == d_.user_error)
    {
	if(!IsWriteStream(neww))
	    return(STREAM_MODE);
	user_err_ = neww;
    }
    /*
     * stdin,stdout,stderr should not be changeable, but there are cases
     * where we want to do it once, e.g. when starting a remote server.
     * Undocumented: allow it when user_xxx has been set beforehand.
     */
    else if(name == d_.stdin0)
    {
	if(neww != user_input_)
	    return(SYSTEM_STREAM);
	if(!IsReadStream(neww))
	    return(STREAM_MODE);
	unclosable = 1;
    }
    else if(name == d_.stdout0 || name == d_.stderr0)
    {
	if(neww != (name==d_.stdout0? user_output_ : user_err_))
	    return(SYSTEM_STREAM);
	if(!IsWriteStream(neww))
	    return(STREAM_MODE);
	unclosable = 1;
    }

    /* And now change the property setting */
    {
	int res;
	pword old, new;
	stream_id old_stream;
	new.tag.kernel = TPTR;
	new.val.wptr = (uword*) neww;	/* assumed already copied */
	res = swap_global_property(name, STREAM_PROP, &new, &old);
	if (res < 0)
	    return res;
	old_stream = (res & NEW_PROP) ? NO_STREAM : (stream_id) old.val.wptr;
	if (unclosable) {
	    /* make previous target stream closable (unless stdin/out/err) */
	    if (old_stream  &&  !(0<=old_stream->unit && old_stream->unit<=2))
	    	StreamMode(old_stream) &= ~SDONTCLOSE;
	    /* make new target stream unclosable */
	    StreamMode(neww) |= SDONTCLOSE;
	}
	if (old_stream)	/* free old stream, if any */
	    stream_tid.free(old_stream);
	return PSUCCEED;
    }
}

#if defined(HAVE_PUSHBACK) && !defined(HAVE_READLINE)
pushback_char(int fd, char *p)
{
    (void) ioctl(fd, TIOCSTI, p);
}
#endif



int
ec_stream_set_sigio(stream_id nst)
{
    if (!(StreamMode(nst) & SSIGIO))
    {
#ifdef USE_REAL_SIGIO
	if (!(IsSocket(nst) || IsPipe(nst)))
	    return UNIMPLEMENTED;
	int res = set_sigio(StreamUnit(nst));
#else
	int res = ec_setup_stream_sigio_thread(nst);
#endif
	Return_If_Error(res);
	StreamMode(nst) |= SSIGIO;
    }
    return PSUCCEED;
}


int
ec_stream_reset_sigio(stream_id nst)
{
    if (StreamMode(nst) & SSIGIO)
    {
	StreamMode(nst) &= ~SSIGIO;
#ifdef USE_REAL_SIGIO
	int res = reset_sigio(StreamUnit(nst));
#else
	int res = ec_teardown_stream_sigio_thread(nst, 0);
#endif
	Return_If_Error(res);
    }
    return PSUCCEED;
}


int
set_sigio(int fd)
{
#ifdef SIGIO_FASYNC
    int		i;
    if (fcntl(fd, F_SETOWN, getpid()) == -1 ||
	(i = fcntl(fd, F_GETFL, 0)) == -1)
    {
	return SYS_ERROR_ERRNO;
    }
    /* FASYNC enables signaling the pgrp when data ready */
    if (fcntl(fd, F_SETFL, i | FASYNC) == -1) {
	return SYS_ERROR_ERRNO;
    }
#endif
#ifdef SIGIO_SETSIG
    /* see manual streamio(7) */
    if (ioctl(fd, I_SETSIG, S_RDNORM|S_RDBAND|S_HIPRI|S_BANDURG) == -1) {
	return SYS_ERROR_ERRNO;
    }
#endif
#ifdef SIGIO_FIOASYNC
    int		on = 1;
    int		pid = (int) getpid();

    /* set the process receiving SIGIO/SIGURG signals to us */
    if (ioctl(fd, SIOCSPGRP, &pid) == -1)
    {
	return SYS_ERROR_ERRNO;
    }
    /* allow receipt of asynchronous I/O signals */
    if (ioctl(fd, FIOASYNC, &on) == -1)
    {
	return SYS_ERROR_ERRNO;
    }
#endif
    return PSUCCEED;
}

int
reset_sigio(int fd)
{
#ifdef SIGIO_FASYNC
    int		i;

    if ((i = fcntl(fd, F_GETFL, 0)) == -1)
    {
	return SYS_ERROR_ERRNO;
    }

    if (fcntl(fd, F_SETFL, i & ~FASYNC) == -1) {
	return SYS_ERROR_ERRNO;
    }
#endif
#ifdef SIGIO_SETSIG
    if (ioctl(fd, I_SETSIG, 0) == -1) {
	return SYS_ERROR_ERRNO;
    }
#endif
#ifdef SIGIO_FIOASYNC
    int		off = 0;

    /* allow receipt of asynchronous I/O signals */
    if (ioctl(fd, FIOASYNC, &off) == -1)
    {
	return SYS_ERROR_ERRNO;
    }
#endif
    return PSUCCEED;
}

static int
_isafifo(int fd)
{
#if !defined(_WIN32) || defined(S_IFIFO)
    struct_stat st;

    return
	fd != NO_UNIT &&
	fstat(fd, &st) != -1 &&
	( (st.st_mode & S_IFMT) == S_IFIFO
#if defined(SOCKETS) && defined(S_IFSOCK)
	|| (st.st_mode & S_IFMT) == S_IFSOCK
#endif
	);
#else
    return 0;
#endif
}


/*
 * Dispatch function for all I/O related RPCs
 */
int
do_io_action(stream_id nst, int action)
{
    switch (action)
    {
    case IO_FLUSH:
	return _local_io_flush_out(nst);
    case IO_FILL:
	return _local_fill_buffer(nst);
    case IO_CLOSE:
	return _local_io_close(nst);
    case IO_TTYIN:
	return _local_tty_in(nst);
    case IO_BIND:
    case IO_CONNECT:
    case IO_LISTEN:
    case IO_ACCEPT:
	return STREAM_MODE;	/* not yet possible */
    }
    return MPS_ERROR;
}


/*
 * New I/O OS layer
 */

static int
_dummy_close(int fd)
{
    return PSUCCEED;
}

static int
_dummy_io(int fd, char *buf, int n)
{
    return PSUCCEED;
}

static int
_dummy_read(int fd, char *buf, int n, int *err)
{
    *err = UNIMPLEMENTED;
    return 0;
}

static int
_dummy_size(stream_id nst)
{
    return 0;
}

static int
_dummy_content(stream_id nst, char *buf)
{
    return 0;
}


static int
_close_fd(int fd)
{
    if (close(fd) < 0)
	return SYS_ERROR_ERRNO;
    return PSUCCEED;
}

/*
 * The basic output primitive which uses the write(2) system call to output
 * the buffer or raw data. If it has been interrupted by an interrupt,
 * the system call is restarted.
 */
static int
_write_fd(int fd, char *buf, int n)
{
    int		cnt = 0;

    for (;;)
    {
	cnt = write(fd, buf, n);
	if (cnt == n)
	    return PSUCCEED;
	else if (cnt < 0 )
	{
#ifdef EINTR
	    if (errno == EINTR)
		continue;	/* an interrupted call, try again */
#endif
	    return SYS_ERROR_ERRNO;	/* from write() */
	}
	else
	{
	    n -= cnt;
	    buf += cnt;
	}
    }
}

static int
_read_fd(int fd, char *buf, int n, int *err)
{
    int count;

    for (;;)
    {
	count = read(fd, buf, n);
	if (count < 0)
	{
#ifdef EINTR
	    if (errno == EINTR)
		continue;	/* an interrupted call, try again */
#endif
	    *err = SYS_ERROR_ERRNO;
	}
	return count;
    }
}

io_channel_t	ec_file = {
    SFILE,		/*io_type*/
#ifdef _WIN32
    SREPOSITION|SCOMPRESS|SEOLCR,	/*mode_defaults*/
#else
    SREPOSITION|SCOMPRESS|SSELECTABLE,	/*mode_defaults*/
#endif
    BUFSIZE,		/*buf_size_hint*/
    _close_fd,		/*close*/
    _dummy_io,		/*ready*/
    _read_fd,		/*read*/
    _write_fd,		/*write*/
    _file_at,		/*at*/
    _buffer_at_eof,	/*at_eof*/
    _buffer_nonempty,	/*buffer_nonempty*/
    _file_truncate,	/*truncate*/
    _file_seek,		/*seek*/
    _buffer_flush,	/*flush*/
    _dummy_size,	/*size*/
    _dummy_content,	/*content*/
    _buffer_write	/*outf*/
};

io_channel_t	ec_pipe = {
    SPIPE,		/*io_type*/
#ifdef _WIN32
    SEOF_RESET|SCOMPRESS|SEOLCR,		/*mode_defaults*/
#else
    SEOF_RESET|SCOMPRESS|SSELECTABLE,	/*mode_defaults*/
#endif
    BUFSIZE,		/*buf_size_hint*/
    _close_fd,		/*close*/
    _dummy_io,		/*ready*/
    _read_fd,		/*read*/
    _write_fd,		/*write*/
    _buffer_at,		/*at*/
    _buffer_at_eof,	/*at_eof*/
    _buffer_nonempty,	/*buffer_nonempty*/
    _dummy_io,		/*truncate*/
    _buffer_seek,	/*seek*/
    _buffer_flush,	/*flush*/
    _dummy_size,	/*size*/
    _dummy_content,	/*content*/
    _buffer_write	/*outf*/
};

io_channel_t	ec_tty = {
    STTY,		/*io_type*/
#ifdef _WIN32
    SEOF_RESET|SFLUSHEOL|SEOLCR,		/*mode_defaults*/
#else
    SEOF_RESET|SFLUSHEOL|SSELECTABLE,	/*mode_defaults*/
#endif
    TTY_BUF_SIZE,	/*buf_size_hint*/
    _close_fd,		/*close*/
    _dummy_io,		/*ready*/
    _read_fd,		/*read*/
    _write_fd,		/*write*/
    _buffer_at,		/*at*/
    _buffer_nonempty,	/*buffer_nonempty*/
    _tty_at_eof,	/*at_eof*/
    _dummy_io,		/*truncate*/
    _buffer_seek,	/*seek*/
    _buffer_flush,	/*flush*/
    _dummy_size,	/*size*/
    _dummy_content,	/*content*/
    _tty_write		/*outf*/
};

io_channel_t	ec_null_stream = {
    SNULL,		/*io_type*/
    SEOF_RESET|SREPOSITION|SSELECTABLE,	/*mode_defaults*/
    0,			/*buf_size_hint*/
    _dummy_close,	/*close*/
    _dummy_io,		/*ready*/
    _dummy_read,	/*read*/
    _dummy_io,		/*write*/
    _dummy_at,		/*at*/
    _always_at_eof,	/*at_eof*/
    _dummy_size,	/*buffer_nonempty*/
    _dummy_io,		/*truncate*/
    _dummy_seek,	/*seek*/
    _dummy_flush,	/*flush*/
    _dummy_size,	/*size*/
    _dummy_content,	/*content*/
    _null_write		/*outf*/
};

extern int ec_write_socket(int, char *, int);
extern int ec_read_socket(int, char *, int, int*);
extern int ec_close_socket(int);

io_channel_t	ec_socket = {
    SSOCKET,		/*io_type*/
#ifdef _WIN32
    SEOF_RESET|SEOLCR|SCOMPRESS|SSELECTABLE,	/*mode_defaults*/
#else
    SEOF_RESET|SCOMPRESS|SSELECTABLE,		/*mode_defaults*/
#endif
    BUFSIZE,		/*buf_size_hint*/
    ec_close_socket,	/*close*/
    _dummy_io,		/*ready*/
    ec_read_socket,	/*read*/
    ec_write_socket,	/*write*/
    _socket_at,		/*at*/
    _buffer_at_eof,	/*at_eof*/
    _buffer_nonempty,	/*buffer_nonempty*/
    _dummy_io,		/*truncate*/
    _buffer_seek,	/*seek*/
    _buffer_flush,	/*flush*/
    _dummy_size,	/*size*/
    _dummy_content,	/*content*/
    _buffer_write	/*outf*/
};

io_channel_t	ec_string_stream = {
    SSTRING,		/*io_type*/
    MREAD|SREPOSITION|SSELECTABLE,	/*mode_defaults*/
    1024,		/*buf_size_hint*/
    _dummy_close,	/*close*/
    _dummy_io,		/*ready*/
    _dummy_read,	/*read*/
    _dummy_io,		/*write*/
    _buffer_at,		/*at*/
    _string_at_eof,	/*at_eof*/
    _string_nonempty,	/*buffer_nonempty*/
    _string_truncate,	/*truncate*/
    _string_seek,	/*seek*/
    _dummy_flush,	/*flush*/
    _string_size,	/*size*/
    _string_content,	/*content*/
    _string_write	/*outf*/
};

io_channel_t	ec_queue_stream = {
    SQUEUE,		/*io_type*/
    SEOF_RESET|MREAD|SSELECTABLE,	/*mode_defaults*/
    1024,		/*buf_size_hint*/
    _dummy_close,	/*close*/
    _dummy_io,		/*ready*/
    _dummy_read,	/*read*/
    _dummy_io,		/*write*/
    _queue_at,		/*at*/
    _queue_at_eof,	/*at_eof*/
    _queue_size,	/*buffer_nonempty*/
    _dummy_io,		/*truncate*/
    _illegal_seek,	/*seek*/
    _queue_flush,	/*flush*/
    _queue_size,	/*size*/
    _queue_content,	/*content*/
    _queue_write	/*outf*/
};

