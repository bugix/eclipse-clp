% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipseclp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe/GLPK Interface
% The Initial Developer of the Original Code is Joachim Schimpf
% Portions created by the Initial Developer are
% Copyright (C) 2019 Joachim Schimpf
% 
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK

% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	eplex_glpk.ecl
%
% Description:	ECLiPSe/GLPK interface
%
% Authors:	J.Schimpf, Coninfer Ltd
% ----------------------------------------------------------------------

:- module(eplex_glpk, [], [empty_language]).

:- comment(categories, ["Constraints","Interfacing"]).
:- comment(summary, "Load lib(eplex) with the GLPK solver").
:- comment(author, "Joachim Schimpf, Coninfer Ltd").
:- comment(copyright, "Joachim Schimpf").
:- comment(date, "May 2019").

% eplex setup checks for existence of module eplex_glpk!
:- local initialization(ensure_loaded(eplex)).
:- export initialization(import(eplex)).

