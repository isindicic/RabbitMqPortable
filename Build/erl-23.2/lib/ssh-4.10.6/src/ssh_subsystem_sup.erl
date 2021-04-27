%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%
%%----------------------------------------------------------------------
%% Purpose: The ssh subsystem supervisor 
%%----------------------------------------------------------------------

-module(ssh_subsystem_sup).

-behaviour(supervisor).

-include("ssh.hrl").

-export([start_link/5,
	 connection_supervisor/1,
	 channel_supervisor/1,
         tcpip_fwd_supervisor/1,
         start_channel/8
	]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Role, Address, Port, Profile, Options) ->
    supervisor:start_link(?MODULE, [Role, Address, Port, Profile, Options]).

connection_supervisor(SupPid) ->
    Children = supervisor:which_children(SupPid),
    ssh_connection_sup(Children).

channel_supervisor(SupPid) when is_pid(SupPid) ->    
    Children = supervisor:which_children(SupPid),
    ssh_channel_sup(Children).

tcpip_fwd_supervisor(SupPid) when is_pid(SupPid) ->    
    Children = supervisor:which_children(SupPid),
    tcpip_fwd_sup(Children).

start_channel(Role, SupPid, ConnRef, Callback, Id, Args, Exec, Opts) ->
    ChannelSup = channel_supervisor(SupPid),
    ssh_channel_sup:start_child(Role, ChannelSup, ConnRef, Callback, Id, Args, Exec, Opts).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([Role, Address, Port, Profile, Options]) ->
    SupFlags = #{strategy  => one_for_all,
                 intensity =>    0,
                 period    => 3600
                },
    ChildSpecs = child_specs(Role, Address, Port, Profile, Options),
    {ok, {SupFlags,ChildSpecs}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_specs(Role, Address, Port, Profile, Options) ->
    [ssh_channel_child_spec(Role, Address, Port, Profile, Options), 
     ssh_connection_child_spec(Role, Address, Port, Profile, Options),
     ssh_tcpip_forward_acceptor_child_spec()].
  
ssh_connection_child_spec(Role, Address, Port, _Profile, Options) ->
    #{id       => id(Role, ssh_connection_sup, Address, Port),
      start    => {ssh_connection_sup, start_link, [Options]},
      restart  => temporary,
      type     => supervisor
     }.

ssh_channel_child_spec(Role, Address, Port, _Profile, Options) ->
    #{id       => id(Role, ssh_channel_sup, Address, Port),
      start    => {ssh_channel_sup, start_link, [Options]},
      restart  => temporary,
      type     => supervisor
     }.

ssh_tcpip_forward_acceptor_child_spec() ->
    #{id       => make_ref(),
      start    => {ssh_tcpip_forward_acceptor_sup, start_link, []},
      restart  => temporary,
      type     => supervisor
     }.


id(Role, Sup, Address, Port) ->
    {Role, Sup, Address, Port}.

ssh_connection_sup([{_, Child, _, [ssh_connection_sup]} | _]) ->
    Child;
ssh_connection_sup([_ | Rest]) ->
    ssh_connection_sup(Rest).

ssh_channel_sup([{_, Child, _, [ssh_channel_sup]} | _]) ->
    Child;
ssh_channel_sup([_ | Rest]) ->
    ssh_channel_sup(Rest).

tcpip_fwd_sup([{_, Child, _, [ssh_tcpip_forward_acceptor_sup]} | _]) ->
    Child;
tcpip_fwd_sup([_ | Rest]) ->
    tcpip_fwd_sup(Rest).

