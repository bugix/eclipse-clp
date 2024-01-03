#
# $Id: Makefile,v 1.2 2017/10/25 21:32:07 jschimpf Exp $
#

DOCDIR = ../doc

SUBDIRS = bips userman embedding libman tutorial visualisation applications man

#DIRMODE = 2755
#FILEMODE = 644
DIRMODE = u=rwx,g=srx,o=rx
FILEMODE = u=rw,go=r

.PHONY: $(SUBDIRS)


install: $(SUBDIRS) $(DOCDIR)/index.html

$(SUBDIRS):
	$(MAKE) -C $@ install


$(DOCDIR)/index.html:	index.html
	@mkdir -p $(@D)
	@chmod $(DIRMODE) $(@D)
	cp -p index.html $(@D)
	@chmod $(FILEMODE) $@

clean:
	@for dir in $(SUBDIRS); do \
	    $(MAKE) -C $$dir clean; \
	done

