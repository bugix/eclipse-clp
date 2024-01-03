:- module(fd_mdd).

:- lib(fd).	% for #::
get_domain_size(X, Val) :- dvar_size(X, Val).

:- comment(summary, "Extensional constraints over FD variables").
:- include(generic_mdd).
