:-module(graphviz_license).

:-pragma(nodebug).
:-pragma(system).

% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK

:-export get_license/1.

:-local initialization(print_license).

:-comment(get_license/1, [
        summary:"Returns the license description for this module",
        amode:get_license(+),
        amode:get_license(-),
        args:[license:"The license string" ],
        desc:"Returns the license description for this module"]).

get_license(
"Graphviz/Grappa are provided under the Eclipse Public License - v 1.0.
See http://www.graphviz.org/License.php
or  http://www.eclipse.org/legal/epl-v10.html").


print_license:-
        get_license(License),
        writeln(log_output, License),
        flush(log_output).
