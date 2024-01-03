#
# $Id: Makefile,v 1.1 2017/10/25 10:03:23 jschimpf Exp $
#

MANDIR = ../../man

#DIRMODE = 2755
#FILEMODE = 644
DIRMODE = u=rwx,g=srx,o=rx
FILEMODE = u=rw,go=r

install:
	@mkdir -p $(MANDIR)/manl
	@chmod $(DIRMODE) $(MANDIR)
	@chmod $(DIRMODE) $(MANDIR)/manl
	cp -pr *.l $(MANDIR)/manl
	@chmod $(FILEMODE) $(MANDIR)/manl/*

clean:

