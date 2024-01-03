%
% This file tells lib(eplex) what optimizer version to expect
% on a particular machine. Add lines of the form:
%
%	licence(Hostname, Solver, Version, LicStr, LicNum).
% E.g.
%	licence('breeze',  xpress, '1326icp', default, 0). % OEM XPRESS-MP Version 13.26
%	licence('cow.ic.ac.uk',  cplex, '80', '', 0).	% CPLEX Version 8.0
%
% Hostname must match the result of get_flag(hostname,H),
% converted to an atom. On some machines, this is just a name,
% on others it is the complete internet domain name.
%
% Solver is one of: cplex, xpress, osi, gurobi.
%
% Version is a solver-specific atom, usually the concatenation
% of the major and minor version numbers.  For osi, it indicates
% the actual COIN solver used.
%
% The meaning of LicStr and LicNum depends on the optimizer:
%
% CPLEX:
%	LicStr:	environment settings for runtime licences, e.g.
%		'CPLEXLICENSE=/usr/local/cplexlic.ptr', otherwise ''
%	LicNum:	serial number for runtime licences
%
% XPRESS-MP:
%	LicStr:	atom default if OEM version used.
%               Otherwise: directory where the licence (.pwd) files are located
%			   (overrides value of XPRESS environment variable)
%	LicNum:	unused
%
% COIN:
%	LicStr:	unused, set to ''
%	LicNum:	unused
%
% GUROBI:
%	LicStr:	unused, set to ''
%	LicNum:	unused
%
% The order of entries below is important:  If a machine has multiple
% optimizers (or optimizer versions) installed, and no solver is
% explicitly requested (i.e. lib(eplex) is called rather than, for
% instance, lib(eplex_cplex)), then the first matching will get loaded.
% Hostname and Version may be left uninstantiated.
%

% Insert any host-specific lines here
%licence('hostname.where.com', cplex, '110', '', 0).

% By default, use COIN/OR OSI solvers
licence(_, osi, clpcbc, '', 0).
licence(_, osi, symclp, '', 0).
licence(_, osi, glpk, '', 0).

% Defaults for other solvers
licence(_AnyHost, glpk,   _AnyVersion, '', 0).
licence(_AnyHost, gurobi, _AnyVersion, '', 0).
licence(_AnyHost, cplex,  _AnyVersion, '', 0).
licence(_AnyHost, xpress, _AnyVersion, default, 0).
