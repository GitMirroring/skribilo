EXTRA_DIST     = skribe.el.in skribilo.el
lisp_DATA      = skribe.el skribilo.el

CLEANFILES     = skribe.el

include $(top_srcdir)/substitute.am

.in:
	$(AM_V_SUBSTITUTE)$(substitute) "$^" > "$@.tmp" &&	\
	mv "$@.tmp" "$@"
