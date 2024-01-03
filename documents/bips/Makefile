#
# $Id: Makefile,v 1.1 2017/10/25 10:03:20 jschimpf Exp $
#

DOCDIR = ../../doc

#DIRMODE = 2755
#FILEMODE = 644
DIRMODE = u=rwx,g=srx,o=rx
FILEMODE = u=rw,go=r

install:
	@mkdir -p $(DOCDIR)/bips/kernel
	@chmod $(DIRMODE) $(DOCDIR)
	@chmod $(DIRMODE) $(DOCDIR)/bips
	@chmod $(DIRMODE) $(DOCDIR)/bips/kernel
	cp -pr kernel/*.eci $(DOCDIR)/bips/kernel
	@chmod $(FILEMODE) $(DOCDIR)/bips/kernel/*.eci

clean:

